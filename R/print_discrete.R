#' Output print function (discrete exposure)
#'
#' @param h CarefullyCausal object
#'
#' @return Prints all output of Carefully Causal object
#' @export
#'
#' @examples
print.ccdisc <- function(h){
  object = h[[3]][[4]]
  outcome = as.character(h[[3]][[1]])
  covariates = as.character(h[[3]][[2]])
  exposure = as.character(h[[3]][[3]])
  df = h[[3]][[4]]
  family= as.character(h[[3]][[5]])
  result_type = as.character(h[[3]][[6]])
  std = as.character(h[[3]][[8]])
  min_estimate = as.character(format(round(min(h[[2]][[1]]),4),nsmall=4))
  max_estimate = as.character(format(round(max(h[[2]][[1]]),4),nsmall=4))


  exposure_lvls = levels(factor(object[,exposure]))[-1]
  exposure_ref = levels(factor(object[,exposure]))[1]


  cat("\nEstimand: \n")
  cat("\nAverage Treatment Effect (Marginal) \n")
  for (i in 1:length(exposure_lvls)){
    cat(cat(cat(cat(paste0("E[",outcome,"^",exposure,"=",exposure_lvls[i],"]"))," -")," "),cat(paste0("E[",outcome,"^",exposure,"=",exposure_ref,"]", "\n")))
  }
  if (covariates==""){
    cat(cat(cat("\nAdjustment Set:"), "None", sep = " "), "\n")
  } else {
    cat(cat(cat("\nAdjustment Set:"), covariates, sep = " "), "\n")
  }

  cat("*Please see output at $Estimand_interpretation for details \n \n")



  cat("\nTreatment effect: \n")
  h[[2]] <- format(round(output[[2]],3),nsmall=3) #to keep 000 as decimals
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
    cat("\n[2] Positivity: is satisfied when both exposed and unexposed individuals are observed within every stratum \n")
    cat("of variables adjusted for (",covariates,"). \n")
    cat("This can be evaluated using the propensity plots saved in the output at $Assumptions$positivity$plots \n")
    cat("(or identically use the ps.plot() function), the table below ($Assumptions$positivity$ps_table) \n ")
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
