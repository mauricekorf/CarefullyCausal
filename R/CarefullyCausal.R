#' CarefullyCausal
#'
#' performs outcome regression, IPTW, S-standardization, T-standardization and TMLE. \bold{test}.
#' It provides the estimand, estimates, discusses the assumptions and provides diagnostics including interpretations.
#' It can be used when having a fixed-exposure, outcome is either dichotomous or continuous and when
#' exposure is dichotomous, multi-value or continuous.
#'
#' @param formula Specify your formula as in (Y ~ X + W)
#' @param data input your data set
#' @param family Can be "gaussian" or "binomial" and relates to the outcome of interest (dependent variable)
#' @param exposure Specify your exposure variable name (e.g. "treatment")
#' @param pvalue TRUE or FALSE, by default is FALSE and does not show the p-value
#' @param interaction Specify the interaction you want to include betwen the exposure and covariate. This is an argument for S-standardization.
#' @param boot1 The number of bootstrap iterations to get the standard error for S-and-T-standardization
#' @param boot2 The number of bootstrap iterations to get the p-value for S-and-T-standardization
#' @param standardization TRUE or FALSE, to show the S-and-T-standardization. Computationally more intensive due to bootstrap.
#' @param bins numeric, relevant when exposure is continuous. The number of bins your exposure will be categorized into.
#' @param confidence Can be "norm" or "bca", which correspond to a normal confidence interval or bias-adjusted and corrected confidence interval.
#' @param result_type Can be "log" (default), "rr" (risk ratio) or "or" (odd ratio)
#' @param outcome_SL_library Specify what estimators to use within Superlearner to estimate the outcome (g-estimation step within TMLE)
#' @param ps_SL_library Specify what estimators to use within Superlearner to estimate the propensity scores within TMLE
#' @param ps_method Can be "SL" (default) or "cbps" if ps_tmle is NULL.
#' @param ps_formula Specify the formula to estimate the propensity scores within TMLE. If empty, is uses same formula that is specified under argument formula with dependent variable being exposure.
#' @param ps_tmle It is NULL by default, user can input own estimated propensity scores. It is an n x 1 vector.
#' @param ip_weights_iptw user-specified IP weights that are used in IPTW.
#'
#' @return Prints the estimand, estimates, assumptions and diagnostics
#' @export
#' @import CBPS
#' @import survey
#' @import stats
#' @import cobalt
#' @import boot
#' @import ggplot2
#' @import stats
#' @import ggridges
#' @import SuperLearner
#'
#' @examples
CarefullyCausal <- function(formula,data,family="gaussian", exposure, pvalue=FALSE, interaction=NULL,boot1=100,boot2=100,standardization=TRUE,bins=10,confidence="norm",result_type="log",
                            outcome_SL_library=c("SL.glm"),ps_SL_library=NULL,ps_method="SL", ps_formula=NULL, ps_tmle=NULL,
                            ip_weights_iptw=NULL)
{



  #when user does not/forgets to specify result_type when outcome is binary, then by default change it to log such that
  #output is shown in log-odds and not in probabilities. For consistency and to avoid misinterpretations
  if (family=="binomial" & result_type!="log"){
    result_type=="log"
  }


  #we create different if statements which seem unnecessary,but we want that the user can explicitly specify
  #the result_type of interest. For the RR we need to fit a quasipoisson, for OR we fit logistic regression but
  #later on we need to exponentiate coefficients which makes it different from "normal" logistic regression within
  #the functions.
  if(family=="binomial" & result_type=="rr"){
    object <- glm(formula=formula,data=data,family=quasipoisson())
  } else if (family=="binomial" & result_type=="log"){ #run GLM
    object <- glm(formula=formula,data=data,family=family)
  } else if (family=="binomial" & result_type=="or"){ #run GLM
    object <- glm(formula=formula,data=data,family=family)
  } else{
    object <- glm(formula=formula,data=data,family=family)
  }


  glm_output <- object
  input_data <- object$data
  outcome <- object$terms[[2]] #get dependent variable
  covariates <- gsub(",\\s+$","",                       #remove last line comma ", " $ represents end of line and \\s+ -> one or more spaces
                     gsub("\\s+,","",                        #remove one or more spaces that occur before comma
                          gsub("^,\\s","",                        #remove first comma if treatment is the first variable
                               gsub(paste0(exposure),"",              #remove treatment variable
                                    gsub("\\s*\\+",",",                     #remove all + symbols and extra space before
                                         gsub("^\\s*","",                        #remove first space
                                              gsub("^\\+,","",toString(object$terms[[3]])))))))) #we need regex, to account for all variations of input order
  # these are saved in output object
  if (covariates!=""){
    g_df = input_data[,as.vector(all.vars(glm_output$terms[[3]]))]
    covariates_ps = gsub(",","+",covariates)
    propensity_scores_cbps  <- CBPS(as.formula(paste0(exposure,"~",covariates_ps)),data = g_df, ATT = 0, method = "exact")$fitted.values #get ATE (ATT=0)
  }

  #bootstrap iteration (boot1) must be at least size of data in order to compute BCA CI (needed to estimate acceleration parameter)
  #if (standardization==TRUE & confidence=="bca"){
  #boot1 = max(boot1,nrow(input_data))
  #}

  if (length(levels(factor(input_data[,exposure])))>4){ # if more than 4 levels including control, then its continuous treatment

    exchangeability_expl = paste("Conditional exchangeability implies the absence of any confounding or selection bias after adjusting for: ",covariates, ".",
                                 "In other words, the exposed group is a perfect representation what would have happened",
                                 "to the unexposed group had they been exposed (and vice versa). The covariate balancing table and corresponding balance plots",
                                 "in the output can be consulted ($Assumptions$exchangeability$covariate_balance) where better balance between the two groups",
                                 "will indicate less residual bias due to the variables adjusted for. Any confounding due to variables not adjusted for will remain.",
                                 "Since the exposure is continuous, the balance between covariates is measured in terms of (linear) correlation between each covariate and the exposure",
                                 "following the approach of Zhu,Coffman and Ghosh (2015) and Austin (2019).After weighting, the weighted correlation between any covariate and exposure",
                                 "should be as low as possible, preferably zero or close to 0 as it indicates independence between the covariate and exposure.",
                                 "Zhu,Coffman and Ghosh (2015) recommend 0.1 as threshold to indicate balance. However, it should be noted that linear correlation is used",
                                 "and thus that a low correlation may simply indicate the absence of a linear relation, but there might be non-linear relations.",
                                 "To validate this to a certain degree, for each covariate the polynomial terms up to degree 3 are also evaluated. However, interactions may also",
                                 "be of importance in terms of inducing non-linear relations, but since interactions are highly context-specific and considering all possible",
                                 "interactions for each covariate clutters the output and thus the readability, they are not considered yet.",
                                 "Besides checking the covariate balance of the current adjusted covariates, an important reflective question to be thought-over is:",
                                 "given the current adjusted for covariates, are there any other important (unmeasured) covariates that affect both treatment assignment and outcome of interest",
                                 "that should be adjusted for without inducing collider or selection bias?",
                                 "Next to this conditional setting, we can also have a setting when no covariates are adjusted for and we assume marginal exchangeability.",
                                 "Marginal exchangeability implies the absence of any confounding or selection bias when not adjusting for anything (unconditionally).",
                                 "This is obtained in an ideal randomized experiment design")
    positivity_expl = paste("Positivity requires that there are both exposed and unexposed individuals within every strata of:",
                            paste0(covariates,"."), "More formally, positivity is satisfied if for every combination of confounders:",paste0("'",covariates,"'"),
                            "the probability of receiving exposure (a+1, small interval as it is continuous)",paste0("'",exposure,"'"),"is",
                            paste0("0<Pr(",exposure,")<1."),"The conditional probability of receiving exposure",paste0("'",exposure,"'"),
                            "when conditioning on covariates",paste0("'",covariates,"'"),"is referred to as propensity score (PS). Hence, the propensity score ranges table",
                            "shows the minimum and maximum value of the estimated conditional probabilities and should not equal 0 or 1,as that would",
                            "violate positivity since there could be a deterministic assignment of exposure.",
                            "The ridge.plot() function or navigating to the output at $Assumptions$Positivity$plot can be consulted to judge whether positivity",
                            "seems plausible. This plot is a so-called ridge plot and shows the conditional density distribution of the propensity score for each bin.",
                            "As the exposure is continuous, the exposure variable is categorized/binned into 10 bins, by default, but can be changed.",
                            "In this manner,the distribution of the propensity scores corresponding to different exposure doses can be evaluated to some extent,",
                            "even though the exposure is continuous.")
    Consistency_expl = paste("Consistency is twofold, first, consistency implies that the exposure",paste0("'",exposure,"'"),
                             "must be sufficiently well-defined so that any variation within the definition of the exposure would not",
                             "result in a different outcome. Secondly, this well-defined exposure must occur in the data since there",
                             "will otherwise be a mismatch between the defined exposure version and the treatment version present in the data.",
                             "For example, suppose you are interested in the effect of BMI reduction (body mass index) on mortality. There are many different",
                             "ways how one can reduce BMI: exercise, diet,smoke, gastric bypass and so on. However, each of these treatment versions can have",
                             "a different effect on mortality, even though they all reduce BMI. For example, say the focus is now on exercising where one is specifically interested in the effect of",
                             "walking twice a week for 1 hour (treatment), but there is only data on people who run. Suppose now that is is expected that running leads to very different results,",
                             "such that we cannot assume treatment variation irrelevance. Then this would mean that there is a mismatch in the defined exposure and the one observed in the data,",
                             "such that when using the data it actually answers a different causal question than the investigator is interested in")
    no_measurement_error_expl = paste("Measurement errors and thus bias are not just limited to observational studies but can occur in any study design and",
                                      "it can occur in the exposure, outcome or confounders.")
    well_specified_model_expl = paste("It is assumed that all models are well-specified, such that the respective model includes all",
                                      "relevant non-linearities and/or statistical interactions. Hence, it should be evaluated whether including for example squared terms or",
                                      "other transformations are necessary.")


    summary_result <- summaryglm.cc(object)
    summary_result2 <- summarycausal.cc(summary_result, object2=glm_output,pvalue=pvalue,exposure=exposure,outcome=outcome,covariates=covariates,interaction=interaction,
                                        standardization=standardization,family = family,result_type = result_type,ip_weights_iptw = ip_weights_iptw)


    #actual output list, showing each object when saving
    output = list( summary_result <- summary_result,
                   summary_result2 <- summary_result2,
                   input_arguments <- list(outcome=outcome,
                                           covariates=covariates,
                                           exposure=exposure,
                                           data=input_data,
                                           family=family,
                                           result_type=result_type,
                                           confidence_interval=confidence),
                   interpretation <- paste("This study evaluated the effect of",exposure,"on",paste0(outcome,","),
                                           "while adjusting for covariates:",  if (covariates!=""){paste0(covariates,".")}else{"No covariates were controlled for."},
                                           "The exposure is continuous and thus the effects should be interpreted in terms of a 1 unit increase in the exposure.",
                                           "When deploying outcome regression the average treatment effec (ATE) is estimated to be:",paste0(paste(round(summary_result2$Estimate[[1]]),3),"."),
                                           "When using IPTW the respective estimated ATE is:",paste0(paste(round(summary_result2$Estimate[[2]]),3),"."),
                                           if (family=="binomial" & result_type=="log"){ paste("It should be noted that the effects are in terms of log-odds.")}
                                           else if (family=="binomial" & result_type=="or"){paste("It should be noted that the effects are in terms of odds ratio.")}
                                           else if (family=="binomial" & result_type=="rr"){paste("It should be noted that the effects are in terms of risk ratio.")}
                                           else {paste("It should be noted that the effects are in terms of the dependent variable.")},
                                           paste("However, in order to be able to interpret these effects as causal, the investigator evaluated and assumes that all assumptions seem plausible, specifically the",
                                                 "five key assumptions: (conditional) exchangeability, positivity, consistency, no measurement error and well-specified model.",
                                                 "First, the investigator assumes that (conditional) exchangeability holds", if(covariates!=""){paste("such that the set of covariates",covariates)}else{"such that not adjusting for any covariate"},
                                                 "is sufficient to completely eliminate all confounding and selection bias. Unmeasured confounding will always be a potential threat, but the investigator",
                                                 "assumes this to be negligible. The second assumption, positivity, implies that the investigator evaluated and confirmed that within every stratum of",
                                                 "variables adjusted for, there are both exposed and unexposed (non-deterministic assignment). Thirdly, the investigator",
                                                 "argues that the exposure variable",exposure,"is sufficiently well-defined such that any variation within the definition",
                                                 "of the exposure would not result in a different outcome. Fourthly, the investigator assumes that no substantial measurement errors",
                                                 "are present such that no substantial measurement bias is induced. Lastly, the investigator evaluated and assumes that all models are",
                                                 "well-specified, such that all relevant non-linearities and/or statistical interactions are taken into account. Given that these above mentioned",
                                                 "assumptions seem plausible, the investigator also evaluated whether the different models yielded (very) different results and if so",
                                                 "it was further investigated as to why these differences appeared.")),
                   estimand_interpretation <- paste("The estimand shows the average causal effect in the population of interest given the different exposure regimes.",
                                                    "More specifically, it shows the effect of receiving one more unit of",paste0(exposure,"."),
                                                    "When adjusting for a set of covariates",paste0("(",covariates,")"), "the estimand is displayed in either a conditional or marginal way.",
                                                    "Particularly, defining the estimand depends on the approach/model used. For example, outcome regression",
                                                    "has a conditional estimand while IPTW has a marginal estimand."),
                   assumptions <- list(exchangeability=list(explanation=exchangeability_expl,
                                                            covariate_balance=covariate_table(data = input_data,covariates = covariates,exposure = exposure),
                                                            balance_plots=gps_plot_intern(object = glm_output,covariates = covariates,exposure = exposure)),
                                       positivity=list(explanation=positivity_expl,
                                                       plot=ridge_plot_intern(object=glm_output,covariates=covariates,exposure=exposure,bins = bins)),
                                       consistency=Consistency_expl,
                                       no_measurement_error=no_measurement_error_expl,
                                       well_specified_model=well_specified_model_expl),
                   Propensity_Scores_CBPS <- list(Propensity_Scores_CBPS = propensity_scores_cbps),
                   Reference_readings <- list(Overview_assumptions="Hernán MA, Robins JM (2020). Causal Inference: What If. Boca Raton: Chapman & Hall/CRC",
                                              Consistency_assumption="Hernán, MA. (2012). Beyond exchangeability: the other conditions for causal inference in medical research. Statistical Methods in Medical Research, 21(1), 3-5.",
                                              IPTW="Hernán MA, Robins JM (2020). Causal Inference: What If (chapter 2 & 16)"))

    names(output) = c("Output_GLM", "Causal_estimates","Input_arguments","Interpretation","Estimand_interpretation","Assumptions","Propensity Scores","reference readings")
    class(output) <- c(output$class, c("cccont"))
    return(output)

    printcont.cccont(output)




  }
  else{ #if exposure <= 4 levels including control then it is discrete

    #define assumptions text, shown in saved output
    estimand = character()
    exchangeability_expl = paste("Conditional exchangeability implies the absence of any confounding or selection bias after adjusting for: ",covariates, ".",
                                 "In other words, the exposed group is a perfect representation what would have happened",
                                 "to the unexposed group had they been exposed (and vice versa). The covariate balancing table and corresponding balance plots",
                                 "in the output can be consulted ($Assumptions$exchangeability$covariate_balance) where better balance between the two groups",
                                 "will indicate less residual bias due to the variables adjusted for. Any confounding due to variables not adjusted for will remain.",
                                 "Besides checking the covariate balance of the current adjusted covariates, an important reflective question to be thought-over is:",
                                 "given the current adjusted for covariates, are there any other important (unmeasured) covariates that affect both treatment assignment and outcome of interest",
                                 "that should be adjusted for without inducing collider or selection bias?",
                                 "Next to this conditional setting, we can also have a setting when no covariates are adjusted for and we assume marginal exchangeability.",
                                 "Marginal exchangeability implies the absence of any confounding or selection bias when not adjusting for anything (unconditionally).",
                                 "This is obtained in an ideal randomized experiment design")
    positivity_expl = paste("Positivity requires that there are both exposed and unexposed individuals within every strata of:",
                            paste0(covariates,"."), "More formally, positivity is satisfied if for every combination of confounders:",paste0("'",covariates,"'"),
                            "the probability of receiving exposure",paste0("'",exposure,"'"),"is",
                            paste0("0<Pr(",exposure,")<1."),"The conditional probability of receiving exposure",paste0("'",exposure,"'"),
                            "when conditioning on covariates",paste0("'",covariates,"'"),"is referred to as propensity score (PS). Hence, the propensity score ranges table",
                            "shows the minimum and maximum value of the estimated conditional probabilities and should not equal 0 or 1,as that would",
                            "violate positivity since there could be a deterministic assignment of exposure. In addition, the ps.plot() function can be used",
                            "to generate a propensity score plot and can be used to evaluate the complete distribution of the estimated PS.",
                            "This PS plot can be used to look for ranges of the PS where there is no overlap between exposed and unexposed, in terms of the propensity scores.",
                            "The ranges table should be used in conjuction with the PS plot.")
    Consistency_expl = paste("Consistency is twofold, first, consistency implies that the exposure",paste0("'",exposure,"'"),
                             "must be sufficiently well-defined so that any variation within the definition of the exposure would not",
                             "result in a different outcome. Secondly, this well-defined exposure must occur in the data since there",
                             "will otherwise be a mismatch between the defined exposure version and the treatment version present in the data.",
                             "For example, suppose you are interested in the effect of BMI reduction (body mass index) on mortality. There are many different",
                             "ways how one can reduce BMI: exercise, diet,smoke, gastric bypass and so on. However, each of these treatment versions can have",
                             "a different effect on mortality, even though they all reduce BMI. For example, say the focus is now on exercising where one is specifically interested in the effect of",
                             "walking twice a week for 1 hour (treatment), but there is only data on people who run. Suppose now that is is expected that running leads to very different results,",
                             "such that we cannot assume treatment variation irrelevance. Then this would mean that there is a mismatch in the defined exposure and the one observed in the data,",
                             "such that when using the data it actually answers a different causal question than the investigator is interested in")
    no_measurement_error_expl = paste("Measurement errors and thus induced bias are not just limited to observational studies but can occur in any study design and ",
                                      "it can occur in the exposure, outcome or confounders.")
    well_specified_model_expl = paste("It is assumed that all models are well-specified, such that the respective model includes all",
                                      "relevant non-linearities and/or statistical interactions. Hence, it should be evaluated whether including for example squared terms or",
                                      "other transformations are necessary.")


    #These variables are created for the "interpretation" section
    lvls = levels(input_data[,exposure]) #get exposure level names
    contrasts= length(lvls)-1 #get number of contrasts (= #exposure levels minus reference level)

    summary_result <- summaryglm.cc(object) #actual analyses are run
    summary_result2 <- summarycausal.cc(summary_result, object2=glm_output,pvalue=pvalue,exposure=exposure,outcome=outcome,covariates=covariates,interaction=interaction,boot1=boot1,boot2=boot2,
                                        standardization = standardization, confidence=confidence, family = family,result_type = result_type,
                                        outcome_SL_library=outcome_SL_library,ps_SL_library=ps_SL_library,ps_method=ps_method,
                                        ps_formula=ps_formula, ps_tmle=ps_tmle,ip_weights_iptw=ip_weights_iptw)
    estimates_int = split(summary_result2$Estimate,ceiling(seq_along(summary_result2$Estimate) / contrasts))#split estimates per model
    estimates_int[] <- lapply(estimates_int,round,3) #round the estimates to 3 decimals



    #actual output list, showing each object when saving
    output = list( summary_result <- summary_result,
                   summary_result2 <- summary_result2,
                   input_arguments <- list(outcome=outcome,
                                           covariates=covariates,
                                           exposure=exposure,
                                           data=input_data,
                                           family=family,
                                           result_type=result_type,
                                           confidence_interval=confidence,
                                           S_T_standardization=standardization),
                   interpretation <- paste("This study evaluated the effect of",exposure,"on",paste0(outcome,","),
                                           "while adjusting for covariates:",  if (covariates!=""){paste0(covariates,".")}else{"No covariates were controlled for."},
                                           "Effects were estimated using reference level: ",paste0(lvls[1],","), "implying all effect estimates (contrasts)",
                                           "should be interpreted with respect to this exposure level.",
                                           "Specifically, the contrasts considered are: ",paste(lvls[-1],collapse =","),"with respect to",paste0(lvls[1],"."),
                                           "When deploying outcome regression the average treatment effect (ATE) is, respectively, estimated to be:",paste0(paste(estimates_int[[1]],collapse = ","),"."),
                                           "When using IPTW the respective estimated ATE are:",paste0(paste(estimates_int[[2]],collapse = ","),"."),
                                           if(standardization==TRUE){
                                             paste("Moreover, S-standardization respectively estimated:", paste(estimates_int[[3]],collapse = ","),
                                                   ",whereas T-standardization estimated the ATE to be:",paste0(paste(estimates_int[[4]],collapse = ","),"."))
                                           },
                                           if (family=="binomial" & result_type=="log"){ paste("It should be noted that the effects are in terms of log-odds.")}
                                           else if (family=="binomial" & result_type=="or"){paste("It should be noted that the effects are in terms of odds ratio.")}
                                           else if (family=="binomial" & result_type=="rr"){paste("It should be noted that the effects are in terms of risk ratio.")}
                                           else {paste("It should be noted that the effects are in terms of the dependent variable.")},
                                           paste("However, in order to be able to interpret these effects as causal, the investigator evaluated and assumes that all assumptions seem plausible, specifically the",
                                                 "five key assumptions: (conditional) exchangeability, positivity, consistency, no measurement error and well-specified model.",
                                                 "First, the investigator assumes that (conditional) exchangeability holds", if(covariates!=""){paste("such that the set of covariates",covariates)}else{"such that not adjusting for any covariate"},
                                                 "is sufficient to completely eliminate all confounding and selection bias. Unmeasured confounding will always be a potential threat, but the investigator",
                                                 "assumes this to be negligible. The second assumption, positivity, implies that the investigator evaluated and confirmed that within every stratum of",
                                                 "variables adjusted for, there are both exposed and unexposed (non-deterministic assignment). Thirdly, the investigator",
                                                 "argues that the exposure variable",exposure,"is sufficiently well-defined such that any variation within the definition",
                                                 "of the exposure would not result in a different outcome. Fourthly, the investigator assumes that no substantial measurement errors",
                                                 "are present such that no substantial measurement bias is induced. Lastly, the investigator evaluated and assumes that all models are",
                                                 "well-specified, such that all relevant non-linearities and/or statistical interactions are taken into account. Given that these above mentioned",
                                                 "assumptions seem plausible, the investigator also evaluated whether the different models yielded (very) different results and if so",
                                                 "it was further investigated as to why these differences appeared.")),
                   estimand_interpretation <- paste("The estimand shows the average causal effect in the population of interest given the different exposure regimes.",
                                                    "More specifically, the effect of receiving",paste0(paste0(for(i in 2:length(levels(factor(input_data[,exposure])))){
                                                      estimand[i-1] = paste(paste("exposure level",levels(factor(input_data[,exposure]))[i]),"with respect to the reference exposure level",levels(factor(input_data[,exposure]))[1],sep = " ")
                                                    },paste(estimand, collapse = " or ")),"."),
                                                    "When adjusting for a set of covariates",paste0("(",covariates,")"),"the estimand is displayed in either a conditional or marginal way.",
                                                    "Particularly, defining the estimand depends on the approach/model used. For example, outcome regression",
                                                    "has a conditional estimand while IPTW has a marginal estimand. The T-standardization approach also has a marginal estimand",
                                                    "while for the S-standardization it depends on whether interactions between treatment and covariates are taken into account.",
                                                    "When no interactions are considered, then it is the same as outcome regression and thus has a conditional estimand, however",
                                                    "when all interactions are considered then it is like T-standardization and thus has a marginal estimand."),
                   assumptions <- list(exchangeability=list(explanation=exchangeability_expl,
                                                            covariate_balance=covariate_table(data = input_data,covariates = covariates,exposure = exposure),
                                                            balance_plots=covb_plot_intern(object = glm_output,covariates = covariates,exposure = exposure)),
                                       positivity=list(explanation=positivity_expl,
                                                       plots=ps_plot_intern(object = glm_output,outcome = outcome,covariates = covariates,exposure = exposure),
                                                       ps_table=ps.table.intern(glm_output=summary_result,data=input_data,outcome=outcome,covariates=covariates,exposure=exposure)),
                                       consistency=Consistency_expl,
                                       no_measurement_error=no_measurement_error_expl,
                                       well_specified_model=well_specified_model_expl),
                   Propensity_Scores_CBPS <- list(Propensity_Scores_CBPS = propensity_scores_cbps),
                   Reference_readings <- list(Overview_assumptions="Hernán MA, Robins JM (2020). Causal Inference: What If. Boca Raton: Chapman & Hall/CRC",
                                              Consistency_assumption="Hernán, MA. (2012). Beyond exchangeability: the other conditions for causal inference in medical research. Statistical Methods in Medical Research, 21(1), 3-5.",
                                              TMLE="Van der Laan & Rose (2013). Targeted learning: Causal inference for observational and experimental learning",
                                              IPTW="Hernán MA, Robins JM (2020). Causal Inference: What If (chapter 2 & 16)",
                                              S_T_standardization="Künzel, S. R., Sekhon, J. S., Bickel, P. J., & Yu, B. (2019). Metalearners for estimating heterogeneous treatment effects using machine learning. Proceedings of the national academy of sciences, 116(10), 4"))
    names(output) = c("Output_GLM", "Causal_estimates","Input_arguments","Interpretation","Estimand_interpretation","Assumptions","Propensity Scores","reference readings")
    class(output) <- c(output$class, c("ccdisc"))
    return(output)

    print.ccdisc(output)


  } #end of first else statement (), so when treatment is discrete (<=4 levels)
} #end of function




