###This R file contains multiple functions that are used internally###


#FUNCTION [1]
#Covariate balance table using COBALT package. Distinction between continuous and discrete exposures.
#continuous: correlations (linear) are shown and test for polynomial up to degree 3
covariate_table <- function(data,exposure,covariates){

  if (covariates!=""){
    covariates_ps = gsub(",","+",covariates) #replace , by +
    ps_output  <- CBPS(as.formula(paste0(exposure,"~",covariates_ps)),data = data, ATT = 0, method = "exact") #use CBPS package, get ATE, exact method


    if (length(levels(factor(data[,exposure])))>4){
      bal_table = bal.tab(ps_output,
                          stats = c("c","k"),
                          un=TRUE,
                          poly=3,
                          thresholds = c(cor=0.1))
      return(bal_table)

    } else{
      bal_table = bal.tab(ps_output,
                          disp = "mean",
                          stats = c("mean.diffs"),
                          un=TRUE,
                          pairwise = TRUE,
                          which.treat = .all)
      return(bal_table)}
  } else{
    "No covariates provided"
  }
}



#FUNCTION [2]
#output print function when exposure is continuous
print.cccont <- function(h){
  outcome = as.character(h[[3]][[1]])
  covariates = as.character(h[[3]][[2]])
  exposure = as.character(h[[3]][[3]])
  df = h[[3]][[4]]
  family= as.character(h[[3]][[5]])
  result_type = as.character(h[[3]][[6]])
  min_estimate = as.character(round(min(h[[2]][[1]])),4)
  max_estimate = as.character(round(max(h[[2]][[1]])),4)

  if (covariates=="") {
    cat("\nEstimand: \n")
    cat(cat(cat(cat(paste0("E[",outcome,"^",exposure,"=a+1","]"))," -")," "),cat(paste0("E[",outcome,"^",exposure,"=a","]", "\n")))
  }
  else {
    cat("\nEstimands: \n")
    cat("Conditional \n")
    cat(cat(cat(cat(cat(cat(cat(cat(cat(cat("E["),outcome, sep = ""),exposure,sep = "^"),"=a+1",sep = ""),"|",sep=""),covariates,sep = ""),"]",sep = ""),"-", sep = "  ")," "),cat(cat(cat(cat(cat(cat(cat("E["),outcome, sep = ""),exposure,sep = "^"),"=a",sep = ""),"|",sep=""),covariates,sep = ""),"]","\n",sep = ""))
    cat("\nMarginal \n")
    cat(cat(cat(cat(paste0("E[",outcome,"^",exposure,"=a+1","]"))," -")," "),cat(paste0("E[",outcome,"^",exposure,"=a","]", "\n")))
    cat("*Please see output at $Estimand_interpretation for details \n \n")

  }
  cat("\nTreatment effect: \n")
  h[2]<- lapply(h[2],round,3)
  print(h[[2]])
  if(family=="binomial" & result_type=="rr") {
    cat("\nInterpretation is in terms of risk ratio (rr) \n")
    cat("note: For outcome regression and IPTW, quasipoisson is deployed where only the estimate and CI are transformed into risk ratio. \n")

  } else if (family=="binomial" & result_type=="or"){
    cat("\nInterpretation is in terms of odds ratio (or) \n")
    cat("note: For outcome regression and IPTW, logistic regression is deployed where only the estimate and CI are transformed into odds ratio. \n")

  } else if (family=="binomial" & result_type=="log"){
    cat("\nInterpretation is in terms of log odds \n")
  }

  cat(cat(cat(cat("\nPlease evaluate whether the difference beteen the lowest estimate:"),min_estimate, sep = " "),"and highest:"),max_estimate,"is of substance, \n")
  cat("given the nature of the data. If so, evaluate the different modelling assumptions.")
  cat("\n")
  cat("\n")


  cat("\nTo interpret these effects as causal, the following key assumptions must be satisfied: \n")

  if (covariates=="") {
    cat("\n[1] Marginal exchangeability: implies that,unconditionally,no confounding nor selection bias is present \n")
    cat("this is obtained in an ideal randomized experiment design \n")
  } else {
    cat(cat(cat("\n[1] Conditional exchangeability: implies that adjusting for "), "\"", covariates,"\"",sep = ""),"is enough to completely eliminate \n")
    cat("all confounding and selection bias. See the covariate balance table ($Assumptions$exchangeability$covariate_balance) \n")
    cat("in the saved output and the corresponding explanations ($Assumptions$exchangeability$explanation). \n")
  }

  if (covariates==""){
    cat("\n[2] Positivity: is satisfied when,unconditionally, both exposed and unexposed individuals are observed \n")
    cat("as it is an unconditional setting,exposure should be assigned randomly with 0<Pr<1 \n")
  } else {
    cat("\n[2] Positivity: is satisfied when both exposed and unexposed (a+1 and a respectively) individuals are observed",
        "within every stratum of variables adjusted for (",covariates,").","A more in-depth explanation can be found at $Assumptions$positivity \n")
  }

  cat(cat(cat("\n[3] Consistency: implies that exposure"),paste0("'",exposure,"'")),"must be sufficiently well-defined so that any variation within \n")
  cat("the definition of the exposure would not result in a different outcome. See $Assumption$consistency \n")
  cat("for a more in-depth explanation and examples. \n")

  cat("\n[4] No measurement error: assumes that all variables were measured without substantial error, such that\n")
  cat("no substantial measurement bias is present. However, if the presence of substantial measurement bias is plausible, \n")
  cat("then the estimated effects should be carefully reconsidered as being causal effects. See $Assumptions$no_measurement_error")
  cat("for a further discussion \n")

  cat("\n[5] Well-specified models: assumes that any models used are well-specified meaning that they include all\n")
  cat("relevant non-linearities and/or statistical interactions\n")

}



#FUNCTION [3]
#output print function when exposure is discrete
print.ccdisc <- function(h){
  object = h[[3]][[4]]
  outcome = as.character(h[[3]][[1]])
  covariates = as.character(h[[3]][[2]])
  exposure = as.character(h[[3]][[3]])
  df = h[[3]][[4]]
  family= as.character(h[[3]][[5]])
  result_type = as.character(h[[3]][[6]])
  std = as.character(h[[3]][[8]])
  min_estimate = as.character(round(min(h[[2]][[1]]),4))
  max_estimate = as.character(round(max(h[[2]][[1]]),4))


  if (covariates=="") {
    exposure_lvls = levels(factor(object[,exposure]))[-1]
    exposure_ref = levels(factor(object[,exposure]))[1]

    cat("\nEstimand: \n")
    for (i in 1:length(exposure_lvls)){
      cat(cat(cat(cat(paste0("E[",outcome,"^",exposure,"=",exposure_lvls[i],"]"))," -")," "),cat(paste0("E[",outcome,"^",exposure,"=",exposure_ref,"]", "\n")))
    }
  } else {
    exposure_lvls = levels(factor(object[,exposure]))[-1]
    exposure_ref = levels(factor(object[,exposure]))[1]

    cat("\nEstimand: \n")
    cat("Conditional \n")
    for (i in 1:length(exposure_lvls)){
      cat(cat(cat(cat(cat(cat(cat(cat(cat(cat("E["),outcome, sep = ""),exposure,sep = "^"),"=",exposure_lvls[i],sep = ""),"|",sep=""),covariates,sep = ""),"]",sep = ""),"-", sep = "  ")," "),cat(cat(cat(cat(cat(cat(cat("E["),outcome, sep = ""),exposure,sep = "^"),"=",exposure_ref,sep = ""),"|",sep=""),covariates,sep = ""),"]","\n",sep = ""))
    }
    cat("\nMarginal \n")
    for (i in 1:length(exposure_lvls)){
      cat(cat(cat(cat(paste0("E[",outcome,"^",exposure,"=",exposure_lvls[i],"]"))," -")," "),cat(paste0("E[",outcome,"^",exposure,"=",exposure_ref,"]", "\n")))
    }
    cat("*Please see output at $Estimand_interpretation for details \n \n")
  }
  cat("\nTreatment effect: \n")
  h[2]<- lapply(h[2],round,3)
  print(h[[2]])
  cat("\nReference exposure level:",levels(factor(object[,exposure]))[1],"\n")
  if(family=="binomial" & result_type=="rr") {
    cat("Interpretation is in terms of risk ratio (rr) \n")
    cat("note: For outcome regression and IPTW, quasipoisson is deployed where only the estimate and CI are transformed into risk ratio. \n")
    cat("For TMLE the SE is in log(rr) and the CI are transformed back in risk ratio scale.")

    if(std=="TRUE"){#s/t-standardization are estimated, so show text below
      cat("\nFor S-and-T-standardization bootstrapping is deployed such that the estimate,p-and-s-value & bias adjusted CI \n")
      cat("are expressed as risk ratio.")}

  } else if (family=="binomial" & result_type=="or"){
    cat("Interpretation is in terms of odds ratio (or) \n")
    cat("note: For outcome regression and IPTW, logistic regression is deployed where only the estimate and CI are transformed into odds ratio. ")
    if(std=="TRUE"){#s/t-standardization are estimated, so show text below
      cat("For S-and-T-standardization bootstrapping is deployed such that the estimate & bias adjusted CI \n")
      cat("are expressed as odds ratio.")}

  } else if (family=="binomial" & result_type=="log"){
    cat("Interpretation is in terms of log odds \n")
    if(std=="TRUE"){#s/t-standardization are estimated, so show text below
      cat("note: for S-and-T-standardization bootstrapping is deployed and shows the bias adjusted CI")}
  }
  cat("\n")

  cat(cat(cat(cat("\nPlease evaluate whether the difference beteen the lowest estimate:"),min_estimate, sep = " "),"and highest:"),max_estimate,"is of substance, \n")
  cat("given the nature of the data. If so, evaluate the different modelling assumptions.")
  cat("\n")
  cat("\n")


  cat("\nTo interpret these effects as causal, the following key assumptions must be satisfied: \n")

  if (covariates=="") {
    cat("\n[1] Marginal exchangeability: implies that,unconditionally,no confounding nor selection bias is present \n")
    cat("this is obtained in an ideal randomized experiment design \n")
  } else {
    cat(cat(cat("\n[1] Conditional exchangeability: implies that adjusting for "), "\"", covariates,"\"",sep = ""),"is enough to completely eliminate \n")
    cat("all confounding and selection bias. See the covariate balance table ($Assumptions$exchangeability$covariate_balance) \n")
    cat("in the saved output and the corresponding explanations ($Assumptions$exchangeability$explanation). \n")
  }


  if (covariates==""){
    cat("\n[2] Positivity: is satisfied when,unconditionally, both exposed and unexposed individuals are observed \n")
    cat("as it is an unconditional setting,exposure should be assigned randomly with 0<Pr<1 \n")
  } else {
    cat("\n[2] Positivity: is satisfied when both exposed and unexposed individuals are observed",
        "within every stratum of variables adjusted for (",covariates,").","This can be evaluated using the propensity plots saved in the output at $Assumptions$positivity$plots",
        "(or identically use the ps.plot() function), the table below ($Assumptions$positivity$ps_table) ")
    cat("and the corresponding explanation found at $Assumptions$positivity$explanation. Note: PS=propensity score \n \n")
    ps.table(h)
  }
  cat(cat(cat("\n[3] Consistency: implies that exposure"),paste0("'",exposure,"'")),"must be sufficiently well-defined so that any variation within \n")
  cat("the definition of the exposure would not result in a different outcome. See $Assumption$consistency \n")
  cat("for a more in-depth explanation and examples. \n")

  cat("\n[4] No measurement error: assumes that all variables were measured without substantial error, such that\n")
  cat("no substantial measurement bias is present. However, if the presence of substantial measurement bias is plausible, \n")
  cat("then the estimated effects should be carefully reconsidered as being causal effects. See $Assumptions$no_measurement_error \n")
  cat("for a further discussion \n")

  cat("\n[5] Well-specified models: assumes that any models used are well-specified meaning that they include all\n")
  cat("relevant non-linearities and/or statistical interactions\n")

}#end of function



#FUNCTION [4]
#Propensity Score table
ps.table.intern <- function(glm_output,data,outcome,covariates,exposure){


  if (covariates!=""){
    df = data[,as.vector(all.vars(glm_output$terms[[3]]))]
    covariates_ps = gsub(",","+",covariates)
    no_cols_before = ncol(df)
    ps_output  <- CBPS(as.formula(paste0(exposure,"~",covariates_ps)),data = df, ATT = 0, method = "exact") #get ATE (ATT=0), exact method
    df <- as.data.frame(cbind(df,ps_output$fitted.values))
    no_cols_after = ncol(df)


    #Compute the ranges per treatment level within each treatment regime
    iter=0
    index_vector=no_cols_after-no_cols_before
    list_ps = list()
    for (i in (no_cols_before+1):no_cols_after){
      iter <- iter+1
      if (iter<=max(index_vector)){
        list_ps[[iter]] = by(df[,i],eval(parse(text = paste0("df$",exposure))),range)
      }
    }

    #rounding to 4 decimals (within the nested list)
    for (w in 1:length(list_ps)) {

      for (z in 1:length(list_ps[[w]])){
        list_ps[[w]][[z]] <-round(list_ps[[w]][[z]],4)
      }
    }

    # Turn list into DF: show actual table of PS ranges. Change row/col names according to factor labels
    ps_ranges = as.data.frame(do.call(cbind, list_ps))
    rownames(ps_ranges) = paste("observed exposure:",rownames(ps_ranges)," ",sep = " ")
    if (length(levels(factor(df[,exposure])))==2) { #for binary we only show one (PS for non-reference level)
      colnames(ps_ranges) = paste("PS range for",levels(factor(eval(parse(text = paste0("df$",exposure)))))[2],sep = " ")
    } else{ #for non-binary treatment levels, we show all
      colnames(ps_ranges) = paste("PS range for",levels(factor(eval(parse(text = paste0("df$",exposure))))),sep = " ")
    }
    ps_ranges
  } else{
    "No covariates provided"
  }
}



#FUNCTION [5]
#function that generates the estimates table (including SE, CI, p-value, s-value)
#two cases: pvalue shown and pvalue not shown.
#it estimates and merges: GLM, IPTW, S-standardization, T-standardization and TMLE
#some duplicated code (combersome), but is due to older version (to be improved)
summarycausal.cc <-function (x,object2=NULL,pvalue=NULL,covariates=NULL,outcome=NULL,exposure=NULL,interaction=NULL,digits = max(3L, getOption("digits") - 3L),
                             symbolic.cor = x$symbolic.cor,
                             boot1=NULL,
                             boot2=NULL,
                             standardization=NULL,
                             confidence=NULL,
                             family=NULL,
                             result_type=NULL,
                             outcome_SL_library=NULL,
                             ps_SL_library=NULL,
                             ps_method=NULL,
                             ps_formula=NULL,
                             ps_tmle=NULL,
                             ip_weights_iptw=NULL,
                             signif.stars = getOption("show.signif.stars"), ...)
{

  if (pvalue==FALSE){ # The default is that we do not show the p-value, but rather the s-value. (optional to show p-value)
    if(length(x$aliased) == 0L) {
      cat("\nNo Coefficients\n")
    } else {
      ## df component added in 1.8.0
      ## partial matching problem here.
      df <- if ("df" %in% names(x)) x[["df"]] else NULL
      if (!is.null(df) && (nsingular <- df[3L] - df[1L]))
        cat("\nCoefficients: (", nsingular,
            " not defined because of singularities)\n", sep = "")
      coefs <- x$coefficients
      if(!is.null(aliased <- x$aliased) && any(aliased)) {
        cn <- names(aliased)
        coefs <- matrix(NA, length(aliased), 4L,
                        dimnames=list(cn, colnames(coefs)))
        coefs[!aliased, ] <- x$coefficients
      }
      coef_df <- as.data.frame(coefs) #transform into df, also allowed for printCoefmat
      rownames(coef_df) = paste(rownames(coef_df),"outcome regression",sep = " ")


      #IPTW: Add output rows from IPTW to GLM output DF
      if (covariates!=""){

        if (is.null(ip_weights_iptw)){ #we estimate IP weights using CBPS
          iptw_data = object2$data
          df = object2$data
          str_vector = all.vars(object2$terms[[3]])
          df <- df[,str_vector]
          covariates_ps = covariates
          covariates_ps = gsub(",","+",covariates_ps) #replace , by +
          no_cols_before = ncol(df)
          ps_output  <- CBPS(as.formula(paste0(exposure,"~",covariates_ps)),data = df, ATT = 0, method = "exact") #use CBPS package, get ATE, exact method
          df <- as.data.frame(cbind(df,ps_output$fitted.values))

          iptw_data$weights = ps_output$weights
          design.ps <- svydesign(ids=~1, weights=~weights, data=iptw_data)
          iptw_output <- suppressWarnings(svyglm(object2$formula, data=iptw_data, design=design.ps, family = object2$family))
        } else { #when the user inputs own IP weights (argument: ip_weights_iptw)

          iptw_data = object2$data
          iptw_data = cbind(iptw_data,ip_weights_iptw)
          design.ps <- svydesign(ids=~1, weights=~ip_weights_iptw, data=iptw_data)
          iptw_output <- suppressWarnings(svyglm(object2$formula, data=iptw_data, design=design.ps, family = object2$family))
        }

        iptw_df <- as.data.frame(summary(iptw_output)$coefficients)
        exposure_names <- row.names(iptw_df) #retrieve all variable names (row IDs)
        index <- grep(paste0(gsub("\"","",deparse(exposure)),".*"),exposure_names) #match all variables with partial string in exposure argument (to select all treatment categories)
        iptw_df <- iptw_df[index,] #only show the treatment variables
        rownames(iptw_df) = paste(rownames(iptw_df),"IPTW",sep = " ")
        coef_df = rbind(coef_df,setNames(iptw_df, names(coef_df)))
      }

      #continue with regression output
      coef_df$s_value = -log2(coef_df[,4]) #Transform p-value into s-value -log2(). add as new column
      coef_df$CI_upper = coef_df[,1]-(1.96*coef_df[,2]) #compute 95% CI (critical=1.96) lower bound
      coef_df$CI_lower = coef_df[,1]+(1.96*coef_df[,2]) #compute 95% CI upper bound
      names(coef_df)[5:7] <- c("S-value", "95%.CI.lower", "95%.CI.upper")
      exposure_names <- row.names(coef_df) #retrieve all variable names (row IDs)
      index <- grep(paste0(gsub("\"","",deparse(exposure)),".*"),exposure_names) #match all variables with partial string in exposure argument (to select all treatment categories)
      new_exposure_names <- exposure_names[index] #retrieve the string names corresponding to the index
      coef_df <- coef_df[c(new_exposure_names),c(1,2,5:7)] #only show the treatment variables (note: reference category)
      if(family=="binomial" & result_type=="rr"){#exponentiate coefs & CI to change into relative risk ratio (output from quasipoisson regression)
        coef_df[,1] <- exp(coef_df[,1])
        coef_df[,4] <- exp(coef_df[,4])
        coef_df[,5] <- exp(coef_df[,5])
      } else if (family=="binomial" & result_type=="or"){#exponentiate coefs & CI, to change log-odds into OR (output from logistic regression)
        coef_df[,1] <- exp(coef_df[,1])
        coef_df[,4] <- exp(coef_df[,4])
        coef_df[,5] <- exp(coef_df[,5])
      }


      #ADD s_learner to output table if discrete exposure <=4, not relevant for continuous exposure
      if(length(levels(factor(object2$data[,exposure])))<=4 & standardization==TRUE){
        t <- t_learner(object2=object2,exposure=exposure,outcome=outcome,covariates=covariates,interaction=interaction,b_iter=boot1,b_iter2=boot2,confidence=confidence,family=family,result_type=result_type)
        s <- s_learner(object2=object2,exposure=exposure,outcome=outcome,covariates=covariates,interaction=interaction,b_iter=boot1,b_iter2=boot2,confidence=confidence,family=family,result_type=result_type)
        coef_df <- rbind(coef_df,setNames(s[,-3],names(coef_df)),setNames(t[,-3],names(coef_df)))}

      #ADD TMLE
      if(length(levels(factor(object2$data[,exposure])))<=2){
        tmle_c <- tmle_cc(object2 = object2, input_data = object2$data,exposure = exposure,outcome=outcome,covariates=covariates,family = family,
                          outcome_SL_library = outcome_SL_library,ps_SL_library = ps_SL_library,ps_method = ps_method, ps_formula = ps_formula,result_type = result_type, ps_tmle=ps_tmle)
        coef_df <- rbind(coef_df,setNames(tmle_c[,-3],names(coef_df)))}



    }
  }
  else { #p-value is TRUE (not NULL)
    if(length(x$aliased) == 0L) {
      cat("\nNo Coefficients\n")
    } else {
      ## df component added in 1.8.0
      ## partial matching problem here.
      df <- if ("df" %in% names(x)) x[["df"]] else NULL
      if (!is.null(df) && (nsingular <- df[3L] - df[1L]))
        cat("\nCoefficients: (", nsingular,
            " not defined because of singularities)\n", sep = "")
      coefs <- x$coefficients
      if(!is.null(aliased <- x$aliased) && any(aliased)) {
        cn <- names(aliased)
        coefs <- matrix(NA, length(aliased), 4L,
                        dimnames=list(cn, colnames(coefs)))
        coefs[!aliased, ] <- x$coefficients
      }
      coef_df <- as.data.frame(coefs) #transform into df, also allowed for printCoefmat
      rownames(coef_df) = paste(rownames(coef_df),"outcome regression",sep = " ")


      #IPTW: Add output rows from IPTW to GLM output DF
      if (covariates!=""){

        if (is.null(ip_weights_iptw)){
          iptw_data = object2$data
          df = object2$data
          str_vector = all.vars(object2$terms[[3]])
          df <- df[,str_vector]
          covariates_ps = covariates
          covariates_ps = gsub(",","+",covariates_ps) #replace , by +
          no_cols_before = ncol(df)
          ps_output  <- CBPS(as.formula(paste0(exposure,"~",covariates_ps)),data = df, ATT = 0, method = "exact") #use CBPS package, get ATE, exact method
          df <- as.data.frame(cbind(df,ps_output$fitted.values))

          iptw_data$weights = ps_output$weights
          design.ps <- svydesign(ids=~1, weights=~weights, data=iptw_data)
          iptw_output <- suppressWarnings(svyglm(object2$formula, data=iptw_data, design=design.ps, family = object2$family)) #supress warning that we use weights (non-integer)
        }else { #when the user inputs own IP weights (argument: ip_weights_iptw)

          iptw_data = object2$data
          iptw_data = cbind(iptw_data,ip_weights_iptw)
          design.ps <- svydesign(ids=~1, weights=~ip_weights_iptw, data=iptw_data)
          iptw_output <- suppressWarnings(svyglm(object2$formula, data=iptw_data, design=design.ps, family = object2$family))
        }

        iptw_df <- as.data.frame(summary(iptw_output)$coefficients)
        exposure_names <- row.names(iptw_df) #retrieve all variable names (row IDs)
        index <- grep(paste0(gsub("\"","",deparse(exposure)),".*"),exposure_names) #match all variables with partial string in exposure argument (to select all treatment categories)
        iptw_df <- iptw_df[index,] #only show the treatment variables (note
        rownames(iptw_df) = paste(rownames(iptw_df),"IPTW",sep = " ")
        coef_df = rbind(coef_df,setNames(iptw_df, names(coef_df)))

      }


      coef_df$s_value = -log2(coef_df[,4]) #Transform p-value into s-value -log2(). add as new column
      coef_df$CI_upper = coef_df[,1]-(1.96*coef_df[,2]) #compute 95% CI (critical=1.96) lower bound
      coef_df$CI_lower = coef_df[,1]+(1.96*coef_df[,2]) #compute 95% CI upper bound
      names(coef_df)[4:7] <- c("P-value","S-value", "95%.CI.lower", "95%.CI.upper")
      exposure_names <- row.names(coef_df)
      index <- grep(paste0(gsub("\"","",deparse(exposure)),".*"),exposure_names)
      new_exposure_names <- exposure_names[index]
      coef_df <- coef_df[c(new_exposure_names),c(1,2,4,5:7)]
      if(family=="binomial" & result_type=="rr"){#change into relative risk ratio (from quasipoisson)
        coef_df[,1] <- exp(coef_df[,1])
        coef_df[,5] <- exp(coef_df[,5])
        coef_df[,6] <- exp(coef_df[,6])
      } else if (family=="binomial" & result_type=="or"){#change log-odds into OR
        coef_df[,1] <- exp(coef_df[,1])
        coef_df[,5] <- exp(coef_df[,5])
        coef_df[,6] <- exp(coef_df[,6])
      }

      #ADD s_learner to output table if discrete exposure <=4, not relevant for continuous exposure
      if(length(levels(factor(object2$data[,exposure])))<=4 & standardization==TRUE){
        t <- t_learner(object2=object2,exposure=exposure,outcome=outcome,covariates=covariates,interaction=interaction,b_iter=boot1,b_iter2=boot2,confidence=confidence,family=family,result_type=result_type)
        s <- s_learner(object2=object2,exposure=exposure,outcome=outcome,covariates=covariates,interaction=interaction,b_iter=boot1,b_iter2=boot2,confidence=confidence,family=family,result_type=result_type)
        coef_df <- rbind(coef_df,setNames(s,names(coef_df)),setNames(t,names(coef_df)))}


      #ADD TMLE
      if(length(levels(factor(object2$data[,exposure])))<=2){
        tmle_c <- tmle_cc(object2 = object2, input_data = object2$data,exposure = exposure,outcome=outcome,covariates=covariates,family = family,
                          outcome_SL_library = outcome_SL_library,ps_SL_library = ps_SL_library,ps_method = ps_method, ps_formula = ps_formula,result_type = result_type, ps_tmle=ps_tmle)
        coef_df <- rbind(coef_df,setNames(tmle_c,names(coef_df)))}

    }

  }


  correl <- x$correlation
  if(!is.null(correl)) {
    # looks most sensible not to give NAs for undefined coefficients
    #         if(!is.null(aliased) && any(aliased)) {
    #             nc <- length(aliased)
    #             correl <- matrix(NA, nc, nc, dimnames = list(cn, cn))
    #             correl[!aliased, !aliased] <- x$correl
    #         }
    p <- NCOL(correl)
    if(p > 1) {
      cat("\nCorrelation of Coefficients:\n")
      if(is.logical(symbolic.cor) && symbolic.cor) {# NULL < 1.7.0 objects
        print(symnum(correl, abbr.colnames = NULL))
      } else {
        correl <- format(round(correl, 2L), nsmall = 2L,
                         digits = digits)
        correl[!lower.tri(correl)] <- ""
        print(correl[-1, -p, drop=FALSE], quote = FALSE)
      }
    }
  }
  coef_df
}





#FUNCTION [6]
#function from GLM, to obtain GLM regression output and format
summaryglm.cc <- function(object, dispersion = NULL,
                          correlation = FALSE, symbolic.cor = FALSE, ...)
{
  est.disp <- FALSE
  df.r <- object$df.residual
  if(is.null(dispersion))	# calculate dispersion if needed
    dispersion <-
    if(object$family$family %in% c("poisson", "binomial"))  1
  else if(df.r > 0) {
    est.disp <- TRUE
    if(any(object$weights==0))
      warning("observations with zero weight not used for calculating dispersion")
    sum((object$weights*object$residuals^2)[object$weights > 0])/ df.r
  } else {
    est.disp <- TRUE
    NaN
  }

  ## calculate scaled and unscaled covariance matrix

  aliased <- is.na(coef(object))  # used in print method
  p <- object$rank
  if (p > 0) {
    p1 <- 1L:p
    Qr <- qr(object)
    ## WATCHIT! doesn't this rely on pivoting not permuting 1L:p? -- that's quaranteed
    coef.p <- object$coefficients[Qr$pivot[p1]]
    covmat.unscaled <- chol2inv(Qr$qr[p1,p1,drop=FALSE])
    dimnames(covmat.unscaled) <- list(names(coef.p),names(coef.p))
    covmat <- dispersion*covmat.unscaled
    var.cf <- diag(covmat)

    ## calculate coef table

    s.err <- sqrt(var.cf)
    tvalue <- coef.p/s.err

    dn <- c("Estimate", "Std. Error")
    if(!est.disp) { # known dispersion
      pvalue <- 2*pnorm(-abs(tvalue))
      coef.table <- cbind(coef.p, s.err, tvalue, pvalue)
      dimnames(coef.table) <- list(names(coef.p),
                                   c(dn, "z value","Pr(>|z|)"))
    } else if(df.r > 0) {
      pvalue <- 2*pt(-abs(tvalue), df.r)
      coef.table <- cbind(coef.p, s.err, tvalue, pvalue)
      dimnames(coef.table) <- list(names(coef.p),
                                   c(dn, "t value","Pr(>|t|)"))
    } else { # df.r == 0
      coef.table <- cbind(coef.p, NaN, NaN, NaN)
      dimnames(coef.table) <- list(names(coef.p),
                                   c(dn, "t value","Pr(>|t|)"))
    }
    df.f <- NCOL(Qr$qr)
  } else {
    coef.table <- matrix(, 0L, 4L)
    dimnames(coef.table) <-
      list(NULL, c("Estimate", "Std. Error", "t value", "Pr(>|t|)"))
    covmat.unscaled <- covmat <- matrix(, 0L, 0L)
    df.f <- length(aliased)
  }
  ## return answer

  ## these need not all exist, e.g. na.action.
  keep <- match(c("call","terms","family","deviance", "aic",
                  "contrasts", "df.residual","null.deviance","df.null",
                  "iter", "na.action"), names(object), 0L)
  ans <- c(object[keep],
           list(deviance.resid = residuals(object, type = "deviance"),
                coefficients = coef.table,
                aliased = aliased,
                dispersion = dispersion,
                df = c(object$rank, df.r, df.f),
                cov.unscaled = covmat.unscaled,
                cov.scaled = covmat))

  if(correlation && p > 0) {
    dd <- sqrt(diag(covmat.unscaled))
    ans$correlation <-
      covmat.unscaled/outer(dd,dd)
    ans$symbolic.cor <- symbolic.cor
  }
  class(ans) <- "summary.glm2"
  ans
}



# FUNCTION [7]
#The S-standardization function, only applicable for discrete variables (V3 s-learner is used, with additional adjustments since then)
#fit one model on all data, change exposure variable to one level, predict, take difference/ratio/odds ratio
#p-value is not useful when using Odds ratio, not for outcome regression/IPTW
s_learner <- function(object2,exposure,outcome,covariates,interaction=NULL,b_iter,b_iter2,confidence,
                      result_type,family){

  # Test if interaction terms are input, otherwise use original formula from GLM
  if (is.null(interaction)){
    formula_std <- object2$formula
  } else{
    covariates_std <- paste(gsub(",","+",covariates),paste(interaction, collapse = "+" ), sep ="+" )
    formula_std <- as.formula(paste0(outcome,"~",covariates_std))
  }


  # [1] fit S-learner model
  s_learner <- glm(formula_std, data=object2$data, family = family)

  # [2] create x number of copies of the data, for each exposure level
  data_list = list()
  for (i in 1:length(levels(object2$data[,exposure]))){
    name <- paste0("data_", i)
    data_list[[name]] = object2$data
  }

  # [3+4] change for each data copy the treatment level into one level, respectively. Make predictions on these copies
  lvls = levels(object2$data[,exposure])
  for(i in 1:length(data_list)){
    levels(data_list[[i]][,exposure])[] <- lvls[i]
    data_list[[i]]$predictcc <- predict(s_learner,data_list[[i]],type="response")
    if (family=="binomial" & result_type=="log"){#when outcome is binary, transform probabilities into log-odds. Output should be in log-odds
      data_list[[i]]$predictcc <- qlogis(data_list[[i]]$predictcc)
    }
  }


  # [5] compute ate for all contrasts w.r.t reference level.
  ate_list=list()
  for(i in 2:length(data_list)){
    name <- paste0("ATE",lvls[i] ,"_",lvls[1])
    if(family=="binomial" & result_type=="rr"){
      ate <- mean(data_list[[i]]$predictcc) / mean(data_list[[1]]$predictcc) #relative risk: X/Y
    } else if (family=="binomial" & result_type=="or"){
      ate <- (mean(data_list[[i]]$predictcc)/(1-mean(data_list[[i]]$predictcc))) / (mean(data_list[[1]]$predictcc)/(1-mean(data_list[[1]]$predictcc))) #Odds-ratio: (x/1-x) / (y/1-y), being probabilities
    } else {
      ate <- mean(data_list[[i]]$predictcc) - mean(data_list[[1]]$predictcc)} #risk difference, when binomial & log, difference in log-odds
    ate_list[[name]] = ate
  }

  # Create final output dataframe
  output_df <- as.data.frame(do.call(rbind, ate_list))
  colnames(output_df) <- "estimate"


  #obtain Confidence interval, SE and pvalue
  #Note: we take ln() when "rr" or "or", as they have asymmetrical CI. After computing CI, SE we transform back to original scale exp()
  #we obtain boot1 number of estimates for statistic of interest, obtain SE & CI based on these iterations.
  #boot2 is only for number of iterations for computing the p-value for each boot1 iteration
  sp_learner <- function(data,indices,formula,family,b_iter2){
    d <- data[indices,]
    #d <- object2$data[sample(1:nrow(object2$data), nrow(object2$data),replace = TRUE), ]
    s_learner <- glm(formula, data=d,family = family)

    data_list = list()
    for (i in 1:length(levels(data[,exposure]))){
      name <- paste0("data_", i)
      data_list[[name]] = d
    }
    # change for each data copy the treatment level into one level, respectively.
    # make predictions for each copy with the fitted GLM on whole dataset
    lvls = levels(data[,exposure])
    for(i in 1:length(data_list)){
      levels(data_list[[i]][,exposure])[] <- lvls[i]
      data_list[[i]]$predictcc <- predict(s_learner,data_list[[i]],type="response")
      if (family=="binomial" & result_type=="log"){#when outcome is binary, transform probabilities into log-odds
        data_list[[i]]$predictcc <- qlogis(data_list[[i]]$predictcc)
      }
    }
    # compute ATE for all exposure levels w.r.t the ref level which is level 1
    ate_list=list()
    for(i in 2:length(data_list)){
      name <- paste0("ATE",lvls[i] ,"_",lvls[1])
      if(family=="binomial" & result_type=="rr"){
        ate <- log(mean(data_list[[i]]$predictcc) / mean(data_list[[1]]$predictcc))
      } else if (family=="binomial" & result_type=="or"){
        ate <- log((mean(data_list[[i]]$predictcc)/(1-mean(data_list[[i]]$predictcc))) / (mean(data_list[[1]]$predictcc)/(1-mean(data_list[[1]]$predictcc))))
      } else {
        ate <- mean(data_list[[i]]$predictcc) - mean(data_list[[1]]$predictcc)}
      ate_list[[name]] = ate
    }



    # get predictions into one df (useful for next procedures)
    pred_exp = ncol(data_list[[1]])
    predictions_df = data.frame(matrix(NA,    # Create empty data frame
                                       nrow = nrow(data_list[[1]]),
                                       ncol = length(data_list)-1))
    for (i in 1:length(data_list)){
      predictions_df[,i] <- data_list[[i]][,pred_exp]
    }

    # obtain observed t-statistic
    t_observed=list()
    for(i in 2:ncol(predictions_df)){
      name <- paste0("t_obs",lvls[i] ,"_",lvls[1])
      t_obs <- (mean(predictions_df[,i]) - mean(predictions_df[,1]))/ sqrt(var(predictions_df[,i])/nrow(predictions_df) + var(predictions_df[,1])/nrow(predictions_df))
      t_observed[[name]] = t_obs
    }

    #[8] shift data distribution (generate null distribution, equal means)
    null_df <- predictions_df
    group_mean <- colMeans(null_df)
    overall_mean <- mean(group_mean)
    for (i in 1:ncol(null_df)){
      null_df[,i] <- null_df[,i] - group_mean[[i]] + overall_mean
    }

    # [9-10] Bootstrap null-data, to get distribution of t-statistics under H0 being true
    boot_t = data.frame(matrix(NA,
                               nrow = b_iter2,
                               ncol = ncol(null_df)-1))
    k=1
    for (i in 1:b_iter2){
      n_datalist = list()#take bootstrap sample columnwise (per prediction column)
      for (i in 1:ncol(null_df)){
        name <- paste0("bdata_", i)
        b_data <- data.frame(null_df[sample(1:nrow(data.frame(null_df[,i])), nrow(data.frame(null_df[,i])),replace = TRUE),i ])
        n_datalist[[name]] = b_data
      }

      fd = data.frame(n_datalist)
      colnames(fd) <- lvls

      tstatistic_list = list()
      for (i in 2:ncol(fd)){
        name = paste0("tstatistic",lvls[i] ,"_",lvls[1])
        t = (mean(fd[,i]) - mean(fd[,1])) / sqrt(var(fd[,i])/nrow(fd) + var(fd[,1])/nrow(fd))
        tstatistic_list[[name]] = t
        #t2 = t.test(d[,2],d[,1])
      }
      boot_t[k,] <- unlist(tstatistic_list)
      k=k+1
    }

    # [10] Pvalue
    p_values=list()
    for (i in 1:length(t_observed)){
      name <- paste0("pvalue",lvls[i+1] ,"_",lvls[1])
      pvalue <- sum(abs(boot_t[,i]) >= abs(t_observed[[i]]))/b_iter2
      p_values[[name]] = pvalue
    }
    return(c(unlist(ate_list),unlist(p_values)))
  }
  results <- boot(data=object2$data,statistic = sp_learner,R=b_iter,formula=formula_std, family=family,b_iter2=b_iter2)

  if(confidence=="bca"){
    #When user want bca confidence interval:
    ci <- lapply(1:length(ate_list), function (p) boot.ci(results, type = "bca", index = p))


    # add CI and SE to output table
    df_std <- data.frame(lower_95=1:length(ate_list),
                         upper_95=1:length(ate_list))
    for (i in 1:length(ci)){
      df_std[i,] <- ci[[i]]$bca[4:5]
    }
  }else if(confidence=="norm") {
    #When user want bca confidence interval:
    ci <- lapply(1:length(ate_list), function (p) boot.ci(results, type = "norm", index = p))


    # add CI and SE to output table
    df_std <- data.frame(lower_95=1:length(ate_list),
                         upper_95=1:length(ate_list))
    for (i in 1:length(ci)){
      df_std[i,] <- ci[[i]]$norm[2:3]
    }
  }
  df_std$standard_error <- unlist(lapply(1:length(ate_list),function(m) sd(results$t[,m]))) #add standard error to this df
  if(family=="binomial" & result_type=="rr"){df_std[,1:2] <-exp(df_std[,1:2]) }#transform CI back to rr scale (from log)
  else if (family=="binomial" & result_type=="or"){df_std[,1:2] <-exp(df_std[,1:2])} #transform CI back to or scale (from log)
  df_std$pvalue <- unlist(lapply((length(ate_list)+1):dim(results$t)[2],function(m) mean(results$t[,m]))) #we want last x columns (for pvalues)

  output_df = cbind(output_df,df_std)
  output_df$row_name <- "name"
  for (i in 1:nrow(output_df)){
    output_df[i,6] <- paste(paste0(exposure,lvls[i+1]),"S-standardization",sep = " ")
  }
  output_df$s_value = -log2(output_df[,5]) #Transform p-value into s-value -log2(). add as new column

  final_output <- output_df[,c(1,4,5,7,2,3)]
  rownames(final_output) <- output_df[,6]
  names(output_df)[1:6] <- c("Estimate","Std.Error","P-value","S-value", "95%.CI.lower", "95%.CI.upper")



  return(final_output)
}


# FUNCTION [8]
#function to check covariate balance using COBALT package when exposure is continuous.
#this is an internal function, the table output is saved in the output
gps_plot_intern <- function(object,covariates,exposure){


  df = object$data[,as.vector(all.vars(object$terms[[3]]))]
  covariates_ps = gsub(",","+",covariates)
  ps_output  <- CBPS(as.formula(paste0(exposure,"~",covariates_ps)),data = df, ATT = 0, method = "exact") #get ATE (ATT=0), exact method


  # get plots
  # remove interactions
  # add name per plot in list
  # add plots of discrete setting to output
  plots = list()
  sep = strsplit(covariates, split = ", ")
  sep = gsub(".*\\*.*","",sep[[1]]) # remove all interactions, otherwise throws error (no term called x*y to balance)
  sep = sep[nzchar(sep)] #remove empty "", .=all characters, *=zero or more, \\*=asterisk symbol, .*=any character zero or more

  plots[["Covariate_balance"]] <- love.plot(ps_output, stats = c("c"),
                                            thresholds = c(cor = .1),
                                            abs = TRUE, wrap = 20,
                                            limits = list(ks = c(0, .5)),
                                            var.order = "unadjusted", line = TRUE)

  #for (i in 1:length(sep[[1]])){
  #  plots[[sep[[1]][[i]]]] = bal.plot(ps_output,sep[[1]][[i]], which = "both")
  #}
  for (i in 1:length(sep)){
    plots[[sep[i]]] <- bal.plot(ps_output,sep[i], which = "both")
  }

  return(plots)
}



# FUNCTION [9]
#Same as 'gps_plot_intern' but for categorical treatment. We obtain all covariate balance plots
covb_plot_intern <- function(object,covariates,exposure){


  df = object$data[,as.vector(all.vars(object$terms[[3]]))]
  covariates_ps = gsub(",","+",covariates)
  ps_output  <- CBPS(as.formula(paste0(exposure,"~",covariates_ps)),data = df, ATT = 0, method = "exact") #get ATE (ATT=0), exact method


  # get plots
  # remove interactions
  # add name per plot in list
  # add plots of discrete setting to output
  plots = list()
  sep = strsplit(covariates, split = ", ")
  sep = gsub(".*\\*.*","",sep[[1]]) #remove interaction terms, cant plot these, we dont balance them
  sep = sep[nzchar(sep)] #remove empty string from vector


  plots[["Covariate_balance_std"]] <- love.plot(ps_output, thresholds = c(m = .1), binary = "std")
  plots[["Covariate_balance_abs"]] <- love.plot(ps_output,
                                                binary ="std",
                                                drop.distance = TRUE,
                                                var.order = "unadjusted",
                                                abs = TRUE,
                                                line = TRUE,
                                                thresholds = c(m = .1))

  for (i in 1:length(sep)){
    plots[[sep[i]]] <- bal.plot(ps_output,sep[i], which = "both")
  }
  return(plots)
}



#FUNCTION [10]
# same as ps_plot() but designed to save all plots in the saved output (store the plots)
ps_plot_intern <- function(object,outcome,covariates,exposure){

  plot_list = list()


  df = object$data[,as.vector(all.vars(object$terms[[3]]))]
  covariates_ps = gsub(",","+",covariates)
  no_cols_before = ncol(df)
  ps_output  <- CBPS(as.formula(paste0(exposure,"~",covariates_ps)),data = df, ATT = 0, method = "exact") #get ATE (ATT=0), exact method
  df <- as.data.frame(cbind(df,ps_output$fitted.values))
  no_cols_after = ncol(df)


  # Find window size for the plot
  # get densities for each corresponding: only max value of y.
  list1 = list()
  for (i in (no_cols_before+1):no_cols_after){
    name <- paste0("density", i)
    list1[[length(list1)+1]] = assign(name, by(df[,i],eval(parse(text = paste0("df$",exposure))),density))

  }

  #First get appropriate window sizes for the plots
  if (length(levels(factor(df[,exposure])))==2) { #for binary, get second factor level (=non-reference one)
    plot_titles = levels(factor(df[,exposure]))[2]
  } else{
    plot_titles = levels(factor(df[,exposure])) #for non-binary, it does not matter
  }

  #new for loop
  for (j in 1:length(list1)){

    title=paste0("Density plot of propensity score for exposure: ","\"",plot_titles[j],"\"")
    global_max=0
    for (i in 1:length(list1[[j]])){

      y_max=max(list1[[j]][[i]]$y)

      if (y_max > global_max){
        global_max <- y_max
      }
    }

    xmin_vector <- c()
    xmax_vector <- c()
    for (i in 1:length(list1[[j]])){

      if (any(list1[[j]][[i]]$x>0)==TRUE){ #we want to select the minimum but only >0 (bound of PS is [0,1])
        xmin_vector = c(xmin_vector,min(list1[[j]][[i]]$x[list1[[j]][[i]]$x>0])) #select within the list of x-values

      } else{
        xmin_vector = c(xmin_vector,0)
      }

      if (any(list1[[j]][[i]]$x<1)==TRUE){ #select maximum but only <1 (bound of PS is [0,1])
        xmax_vector = c(xmax_vector,max(list1[[j]][[i]]$x[list1[[j]][[i]]$x<1]))
      } else{
        xmax_vector = c(xmax_vector,1)
      }

    }
    x_min = min(xmin_vector)
    x_max = max(xmax_vector)


    plotcolors <- c("red","blue","orange","black") #color palette, now 4 colors since we accept up to 4 treatments
    for (k in 1:length(list1[[j]])){

      if (k==1){
        plot(list1[[j]][[k]], ylim=c(0,global_max),xlim=c(x_min,x_max),col=plotcolors[k],
             main = title,
             cex.main=0.9,
             xlab = "Propensity Score")
      }
      else{
        lines(list1[[j]][[k]],col=plotcolors[k])
      }
    }
    legend("topright",cex=0.5,legend = levels(factor(eval(parse(text = paste0("df$",exposure))))),col = plotcolors[1:length(list1[[j]])], lty = 1, bty = "n")
    plot_list[[j]] <- recordPlot()
    dev.off(dev.list()["RStudioGD"])

  }
  return(plot_list)
}#end of function



#FUNCTION [11]
#function to obtain "conditional density plots of the propensity score", when exposure is continuous.
#bins argument: bins=number of categories to form (discretization)
#it is used an internal function: plots are saved in output
ridge_plot_intern <- function(object,covariates,exposure,bins){


  df = object$data[,as.vector(all.vars(object$terms[[3]]))]
  covariates_ps = gsub(",","+",covariates)
  outcome <- as.character(object$terms[[2]]) #get dependent variable
  ps_output  <- CBPS(as.formula(paste0(exposure,"~",covariates_ps)),data = df, ATT = 0, method = "exact") #get ATE (ATT=0), exact method
  df <- as.data.frame(cbind(df,ps_output$fitted.values))

  exp_index = grep(exposure,colnames(df))


  #bin the exposure variable
  df$bins = as.factor((cut(df[,exp_index],breaks = bins)))


  # save plot in "plot
  plot <- ggplot(df, aes(x = df$`ps_output$fitted.values`, y = df$bins, group = df$bins, fill=df$bins)) +
    geom_density_ridges(alpha=0.6, scale=3) +
    labs(title = paste("Exposure",exposure),
         x="Propensity Score", y=paste("Discretization in",bins,"bins")) +
    theme_ridges() +
    theme(legend.position = "none",
          plot.title = element_text(hjust = 0.5),
          axis.title.y = element_text(hjust = 0.5),
          axis.title.x = element_text(hjust = 0.5))

  return(plot)
}



#FUNCTION [12]
#The T-standardization function, only applicable for discrete variables
#We split the data based on exposure variable, fit two models,predict all data with each model, take difference/ratio/odds
t_learner <- function(object2,exposure,outcome,covariates,interaction=NULL,b_iter,b_iter2,confidence,
                      result_type,family){

  # [1 + 2] Split the data and fit the models (note we don't need seperate interactions, already accounted for)
  dfc = object2$data
  index_exp <- match(exposure, names(dfc))
  index_y <- match(as.character(outcome), names(dfc))
  lvls = levels(dfc[,exposure])

  model_list = list()
  cov_no_exp = unlist(strsplit(covariates,", "))
  new_formula = reformulate(cov_no_exp, response=outcome) #exclude exposure from formula
  for (i in 1:length(lvls)){
    name <- paste0("model_", i)
    data = dfc[dfc[,index_exp]==lvls[i],]
    model_list[[name]] = glm(new_formula,data=data, family = object2$family)
  }
  # [3] Predict original data
  predictions_list = list() #save predictions
  for (i in 1:length(model_list)){
    name <- paste0("predictions_", i)
    predictions_list[[name]] = predict(model_list[[i]],dfc, type = "response") #exclude treatment variable, adds no value. automatically dropped
    if (family=="binomial" & result_type=="log"){#when outcome is binary, transform probabilities into log-odds
      predictions_list[[i]] <- qlogis(predictions_list[[i]])
    }

  }
  # [4] Obtain mean estimates (ATE)
  ate_list=list()
  for(i in 2:length(predictions_list)){
    name <- paste0("ATE",lvls[i] ,"_",lvls[1])
    if(family=="binomial" & result_type=="rr"){
      ate <- mean(predictions_list[[i]]) / mean(predictions_list[[1]])
    } else if (family=="binomial" & result_type=="or"){
      ate <- (mean(predictions_list[[i]])/(1-mean(predictions_list[[i]]))) / (mean(predictions_list[[1]])/(1-mean(predictions_list[[1]])))
    } else {
      ate <- mean(predictions_list[[i]]) - mean(predictions_list[[1]])}
    ate_list[[name]] = ate
  }


  # Create final output dataframe
  output_df <- as.data.frame(do.call(rbind, ate_list))
  colnames(output_df) <- "estimate"


  # [5-10]
  #obtain Confidence interval, SE and pvalue
  tp_learner <- function(data,indices,formula,family,b_iter2){
    #d <- object2$data[sample(1:nrow(object2$data), nrow(object2$data),replace = TRUE), ]
    d <- data[indices,]
    i_exp<- match(exposure, names(d))
    i_y <- match(as.character(outcome), names(d))
    lvls = levels(d[,exposure])

    #fit models
    model_list = list()
    cov_no_exp = unlist(strsplit(covariates,", "))
    new_formula = reformulate(cov_no_exp, response=outcome) #exclude exposure from formula
    for (i in 1:length(lvls)){
      name <- paste0("model_", i)
      data = d[d[,i_exp]==lvls[i],]
      model_list[[name]] = glm(new_formula,data=data, family = family)
    }
    #get predictions
    predictions_list = list() #save predictions
    for (i in 1:length(model_list)){
      name <- paste0("predictions_", i)
      predictions_list[[name]] = predict(model_list[[i]],d, type = "response") #exclude treatment variable, adds no value
      if (family=="binomial" & result_type=="log"){#when outcome is binary, transform probabilities into log-odds
        predictions_list[[i]] <- qlogis(predictions_list[[i]])
      }
    }
    #get ATE
    ate_list=list()
    for(i in 2:length(predictions_list)){
      name <- paste0("ATE",lvls[i] ,"_",lvls[1])
      if(family=="binomial" & result_type=="rr"){
        ate <- log(mean(predictions_list[[i]]) / mean(predictions_list[[1]]))
      } else if (family=="binomial" & result_type=="or"){
        ate <- log((mean(predictions_list[[i]])/(1-mean(predictions_list[[i]]))) / (mean(predictions_list[[1]])/(1-mean(predictions_list[[1]]))))
      } else {
        ate <- mean(predictions_list[[i]]) - mean(predictions_list[[1]])}
      ate_list[[name]] = ate
    }


    # get predictions into one df (useful for next procedures)
    predictions_df = data.frame(predictions_list)

    # obtain observed t-statistic
    t_observed=list()
    for(i in 2:ncol(predictions_df)){
      name <- paste0("t_obs",lvls[i] ,"_",lvls[1])
      t_obs <- (mean(predictions_df[,i]) - mean(predictions_df[,1]))/ sqrt(var(predictions_df[,i])/nrow(predictions_df) + var(predictions_df[,1])/nrow(predictions_df))
      t_observed[[name]] = t_obs
    }

    # shift data distribution (generate null distribution, equal means)
    null_df <- predictions_df
    group_mean <- colMeans(null_df)
    overall_mean <- mean(group_mean)
    for (i in 1:ncol(null_df)){
      null_df[,i] <- null_df[,i] - group_mean[[i]] + overall_mean
    }

    # Bootstrap null-data, to get distribution of t-statistics under H0 being true
    boot_t = data.frame(matrix(NA,
                               nrow = b_iter2,
                               ncol = ncol(null_df)-1))
    k=1
    for (i in 1:b_iter2){
      n_datalist = list()#take bootstrap sample columnwise (per prediction column)
      for (i in 1:ncol(null_df)){
        name <- paste0("bdata_", i)
        b_data <- data.frame(null_df[sample(1:nrow(data.frame(null_df[,i])), nrow(data.frame(null_df[,i])),replace = TRUE),i ])
        n_datalist[[name]] = b_data
      }

      fd = data.frame(n_datalist)
      colnames(fd) <- lvls

      tstatistic_list = list()
      for (i in 2:ncol(fd)){
        name = paste0("tstatistic",lvls[i] ,"_",lvls[1])
        t = (mean(fd[,i]) - mean(fd[,1])) / sqrt(var(fd[,i])/nrow(fd) + var(fd[,1])/nrow(fd))
        tstatistic_list[[name]] = t
        #t2 = t.test(d[,2],d[,1])
      }
      boot_t[k,] <- unlist(tstatistic_list)
      k=k+1
    }

    # [10] Pvalue
    p_values=list()
    for (i in 1:length(t_observed)){
      name <- paste0("pvalue",lvls[i+1] ,"_",lvls[1])
      pvalue <- sum(abs(boot_t[,i]) >= abs(t_observed[[i]]))/b_iter2
      p_values[[name]] = pvalue
    }
    return(c(unlist(ate_list),unlist(p_values)))
  }
  results <- boot(data=object2$data,statistic = tp_learner,R=b_iter,formula=formula_std, family=family,b_iter2=b_iter2)

  #User specifies what CI is desired, BCA needs boot1 iterations at least as sample size (takes longer w.r.t norm CI)
  if(confidence=="bca"){
    ci <- lapply(1:length(ate_list), function (p) boot.ci(results, type = "bca", index = p))


    # add CI and SE to output table
    df_std <- data.frame(lower_95=1:length(ate_list),
                         upper_95=1:length(ate_list))
    for (i in 1:length(ci)){
      df_std[i,] <- ci[[i]]$bca[4:5]
    }
  }else if(confidence=="norm") {
    #When user want bca confidence interval:
    ci <- lapply(1:length(ate_list), function (p) boot.ci(results, type = "norm", index = p))


    # add CI and SE to output table
    df_std <- data.frame(lower_95=1:length(ate_list),
                         upper_95=1:length(ate_list))
    for (i in 1:length(ci)){
      df_std[i,] <- ci[[i]]$norm[2:3]
    }
  }
  df_std$standard_error <- unlist(lapply(1:length(ate_list),function(m) sd(results$t[,m]))) #add standard error to this df
  if(family=="binomial" & result_type=="rr"){df_std[,1:2] <-exp(df_std[,1:2]) }#transform back to rr scale (from log)
  else if (family=="binomial" & result_type=="or"){df_std[,1:2] <-exp(df_std[,1:2])} #transform back to or scale (from log)
  df_std$pvalue <- unlist(lapply((length(ate_list)+1):dim(results$t)[2],function(m) mean(results$t[,m]))) #we want last x columns (for pvalues)

  output_df = cbind(output_df,df_std)
  output_df$row_name <- "name"
  for (i in 1:nrow(output_df)){
    output_df[i,6] <- paste(paste0(exposure,lvls[i+1]),"T-standardization",sep = " ")
  }
  output_df$s_value = -log2(output_df[,5]) #Transform p-value into s-value -log2(). add as new column

  final_output <- output_df[,c(1,4,5,7,2,3)]
  rownames(final_output) <- output_df[,6]
  names(output_df)[1:6] <- c("Estimate","Std.Error","P-value","S-value", "95%.CI.lower", "95%.CI.upper")


  return(final_output)
}



#FUNCTION [13]
#only for binary exposure
tmle_cc <- function(object2,input_data,exposure,outcome,covariates,family, outcome_SL_library,
                    ps_SL_library,ps_method, ps_formula, result_type,ps_tmle){


  # specify ML models to be used for outcome modeling (Q) and propensity score estimation (g)
  # by default: same SL library is used for Q and g when not specified for g
  if(is.null(ps_SL_library)){
    ps_SL_library = outcome_SL_library
  }


  # STAGE 1
  # [1] Fit outcome model (Q), generate 3 different predictions:
  #  (1.1) Predictions given observed treatment
  #  (1.2) Predictions given treatment=1 for everybody
  #  (1.3) Predictions given treatment=0 for everybody

  Y <- input_data[ ,as.character(outcome)] #get Y variable as vector
  if (length(levels(factor(Y)))>4){
    min.Y = min(Y)
    max.Y = max(Y)
    Y = (Y-min.Y) / (max.Y - min.Y)
  }
  str_vector = all.vars(object2$terms[[3]]) #only select exposure+input covariates
  W_A <- input_data[,str_vector]
  Q <- SuperLearner(Y = Y, # outcome variable
                    X = W_A, # W_A is the matrix with exposure + covariates
                    family=family, # binomial or gaussian depending on outcome Y
                    method = "method.CC_nloglik", # following hanna A frank et al. (2023), for both continuous/binary outcome
                    SL.library = outcome_SL_library) # specify SuperLearner library




  # (1.1) predictions (probabilities) for actual received treatment
  Q_A <- as.vector(predict(Q, type = "response")$pred)

  # (1.2) predictions (probabilities) for everyone exposed (A=1), non reference category so position 2
  # ensure that we have 2 levels but just one value, otherwise error with one level factor
  W_A1 <- W_A
  lvls <- levels(W_A1[,exposure])
  W_A1[,exposure] <- factor(lvls[2], levels = lvls)
  Q_1 <- as.vector(predict(Q, newdata = W_A1, type = "response")$pred) # predict, we have one actual value but need to have 2 factor levels (error)

  # (1.3) predictions (probabilities) for everyone unexposed (A=0)
  W_A0 <- W_A
  W_A0[,exposure] <- factor(lvls[1], levels = lvls)
  Q_0 <- as.vector(predict(Q, newdata = W_A0)$pred) # predict, we have one actual value but need to have 2 factor levels (error)


  # Put all predictions, outcome Y and exposure A into a new df to keep an overview
  df_tmle <- data.frame(Y=input_data[,as.character(outcome)],
                        A=input_data[,exposure],
                        Q_1,Q_0,Q_A)


  # STAGE 1 ATE (same as G-computation, e.g. S-standardization)
  ate_gcomp <- mean(qlogis(df_tmle$Q_1) - qlogis(df_tmle$Q_0)) #show in log-odds
  ate_gcomp_prob <- mean(df_tmle$Q_1 - df_tmle$Q_0) #show in prob





  # STAGE 2

  # We again use SL, as CBPS or other package cannot be used with 1 exposure level which is what we require
  # It is similar to stage 1 but applied to PS modelling and predicting
  # [2] Fit PS model (g) and compute 3 different IP weights:
  #  (2.1) estimate IP weight corresponding to observed treatment
  #  (2.2) estimate IP weight corresponding to everbody got A=1
  #  (2.3) estimate IP weight corresponding to everbody got A=0

  # Split exposure & covariates and fit PS model
  A <- as.numeric(as.character(input_data[,exposure]))
  W <- as.data.frame(W_A[,-grep(exposure, colnames(W_A))]) # matrix of predictors (adjustment set), remove exposure

  if (is.null(ps_tmle)){

    if (ps_method=="SL"){
      g <- SuperLearner(Y = A, # outcome is the A (treatment) vector, numeric
                        X = W, # W is a matrix/df of predictors
                        family=binomial(), # treatment is a binomial outcome by default
                        method = "method.CC_nloglik",
                        SL.library=ps_SL_library) # SL library for PS model

      # Estimate the PS using model g
      # Note: the non-reference category gets predicted! so H_1 corresponds to lvls[2]
      gW <- as.vector(predict(g)$pred) #Pr(A=1|W)/propensity score
      H1W <- ifelse(df_tmle$A==lvls[2], 1/gW,0)
      H0W <- ifelse(df_tmle$A==lvls[1],1/(1-gW),0)



    } else if (ps_method=="cbps"){

      if (is.null(ps_formula)){ #use same formula as for outcome model
        covariates_ps = gsub(",","+",covariates)
        g <- CBPS(as.formula(paste0(exposure,"~",covariates_ps)),data = input_data, ATT = 0, method = "exact",standardize = FALSE)
      } else{
        g <- CBPS(formula = ps_formula,data = input_data, ATT = 0, method = "exact", standardize = FALSE)
      }

      # Estimate the PS using model g
      # Note: the non-reference category gets predicted! so H_1 corresponds to lvls[2]
      gW <- g$fitted.values #propensity scores
      H1W <- ifelse(df_tmle$A==lvls[2], 1/gW,0)
      H0W <- ifelse(df_tmle$A==lvls[1],1/(1-gW),0)
    }
  } else { #use input PS (argument g)

    gW <- ps_tmle #propensity scores
    H1W <- ifelse(df_tmle$A==lvls[2], 1/gW,0)
    H0W <- ifelse(df_tmle$A==lvls[1],1/(1-gW),0)

  }



  # [3] fluctuation parameter estimation
  # NOTE: check order of coefficient
  glm_fit <- glm(Y ~ -1 + H0W + H1W + offset(qlogis(Q_A)), family=binomial) #binomial is not due to Y but is best fit for this
  epsilon <- coef(glm_fit)


  # update expected outcome of all observations given A=1 & A=0, in terms of probabilities
  Q_1_update_pr <- plogis(qlogis(Q_1) + epsilon[2]/gW)
  Q_0_update_pr <- plogis(qlogis(Q_0) + epsilon[1]/(1-gW))
  Q_A_update_pr <- plogis(qlogis(Q_A) + (epsilon[1] * H0W + epsilon[2] * H1W))


  # ATE
  # check: can we do log-odds like how we would compute it in terms of probabilities? Y-logit(y) does it make sense?
  if (result_type=="log" & family=="binomial"){
    mu1_logodds=mean(qlogis(Q_1_update_pr))
    mu0_logodds=mean(qlogis(Q_0_update_pr))
    tmle_ate=mu1_logodds-mu0_logodds

    mu1=mean(Q_1_update_pr)
    mu0=mean(Q_0_update_pr)
    #wd=log((mu1/(1-mu1))/(mu0/(1-mu0)))

  } else if (result_type=="rr" & family=="binomial"){
    mu1=mean(Q_1_update_pr)
    mu0=mean(Q_0_update_pr)
    tmle_ate <- mu1/mu0

  } else if (result_type=="or" & family=="binomial"){
    mu1=mean(Q_1_update_pr)
    mu0=mean(Q_0_update_pr)
    tmle_ate <- (mu1 * (1 - mu0)) / ((1 - mu1) * mu0) #marginal odds ratio
  }

  # ATE when exposure is continuous
  if (family=="gaussian"){
    tmle_ate_scaled = Q_1_update_pr-Q_0_update_pr
    tmle_ate = (max.Y-min.Y)*mean(tmle_ate_scaled, na.rm=T)
  }



  # [4] SE, Confidence Interval & p-value


  # Compute efficient influence curves (IEC)
  # CHECK EIC, now we get Log(odds ratio), not same to CI of
  # log-odds
  if (result_type=="log" & family=="binomial"){
    EIC <- 1 * (1/(mu1 * (1 - mu1)) * (1 * H1W * 1 * (Y - Q_A_update_pr) + Q_1_update_pr) - 1/(mu0 * (1 - mu0)) * (1 * H0W * 1 * (Y - Q_A_update_pr) + Q_0_update_pr))

    # Compute SE, CI and P-value based on EIC
    n <- nrow(input_data)
    tmle_var <- var(EIC)/n
    tmle_se <- sqrt(tmle_var)
    conf_low <- tmle_ate- 1.96*tmle_se
    conf_high <- tmle_ate + 1.96*tmle_se
    pval <- round(2 * pnorm(-abs(tmle_ate / tmle_se)),4)
  } else if (result_type=="rr" & family=="binomial"){
    # rr
    # EIC equation is from tmle() source code. It is the EIC for log(RR) -> calcParameters() function in tmle
    logRR <- log(tmle_ate)
    EIC <- (1/mu1 * ((H1W) *  (Y - Q_A_update_pr) + Q_1_update_pr - mu1) -  1/mu0 * (H0W * (Y - Q_A_update_pr) + Q_0_update_pr - mu0))

    # Compute SE, CI and P-value based on EIC
    n <- nrow(input_data)
    tmle_var <- var(EIC)/n #in log
    tmle_se <- sqrt(tmle_var) #in log
    conf_low <- exp(logRR- 1.96*tmle_se)
    conf_high <- exp(logRR+ 1.96*tmle_se)
    pval <- round(2 * pnorm(-abs(logRR/sqrt(tmle_se))),4)
  } else if (result_type=="or" & family=="binomial"){
    #mor
    # This one describes the Delta-method approach: has some criticism (eg. negative bound symmetric etc.)
    #NOTE: we split the EIC equation into A=1 and A=0 setting, which we call D1 and D0 respectively.
    # when we do D1-D0, we get exactly the EIC we would compute when not splitting into A=1 and A=0
    #D1 <- H1W*(input_data[,as.character(outcome)] - Q_1_update_pr) + Q_1_update_pr - mean(Q_1_update_pr)
    #D0 <- H0W*(input_data[,as.character(outcome)] - Q_0_update_pr) + Q_0_update_pr - mean(Q_0_update_pr)
    #EIC <- (1 - mu0) / mu0 / (1 - mu1)^2 * D1 - mu1 / (1 - mu1) / mu0^2 * D0

    # This is the approach programmed by tmle package (source), we just log-transform like usual in Epi
    EIC <- 1 * (1/(mu1 * (1 - mu1)) * (1 * H1W * 1 * (Y - Q_A_update_pr) + Q_1_update_pr) - 1/(mu0 * (1 - mu0)) * (1 * H0W * 1 * (Y - Q_A_update_pr) + Q_0_update_pr))


    # Compute SE, CI and P-value based on EIC
    n <- nrow(input_data)
    tmle_var <- var(EIC)/n
    tmle_se <- sqrt(tmle_var)
    conf_low <- exp(log(tmle_ate)- 1.96*tmle_se)
    conf_high <- exp(log(tmle_ate) + 1.96*tmle_se)
    pval <- round(2 * pnorm(-abs(log(tmle_ate) / tmle_se)),4)
  } else if (family=="gaussian"){

    Q_1_update_pr <- (max.Y-min.Y)*Q_1_update_pr+min.Y
    Q_0_update_pr <- (max.Y-min.Y)*Q_0_update_pr+min.Y

    D1 <- H1W*(input_data[,as.character(outcome)] - Q_1_update_pr) + Q_1_update_pr - mean(Q_1_update_pr,na.rm=T)
    D0 <- H0W*(input_data[,as.character(outcome)] - Q_0_update_pr) + Q_0_update_pr - mean(Q_0_update_pr,na.rm=T)
    EIC <- D1 - D0


    # Compute SE, CI and P-value based on EIC
    n <- nrow(input_data)
    tmle_var <- var(EIC,na.rm = T)/n
    tmle_se <- sqrt(tmle_var)
    conf_low <- tmle_ate- 1.96*tmle_se
    conf_high <- tmle_ate + 1.96*tmle_se
    pval <- round(2 * pnorm(-abs(tmle_ate / tmle_se)),4)
  }



  # Save all output
  final_output <- data.frame(one = tmle_ate,
                             two = tmle_se,
                             three = pval,
                             four = -log2(pval),
                             five = conf_low,
                             six = conf_high)

  names(final_output)[1:6] <- c("Estimate","Std.Error","P-value","S-value", "95%.CI.lower", "95%.CI.upper")
  rownames(final_output) <- paste(paste0(exposure,lvls[2]),"TMLE",sep = " ")


  return(final_output)
}






