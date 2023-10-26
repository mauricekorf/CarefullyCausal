#' Propensity Scores Range Table
#'
#'Only applicable for discrete exposure variable
#' @param h Input a CarefullyCausal object
#'
#' @return Propensity Scores minimum and maximum values per exposure level
#' @export
#'
#' @examples
ps.table <- function(h){


  #filter on USED covariates and treatment variable
  outcome = as.character(h[[3]][[1]])
  covariates = as.character(h[[3]][[2]])
  exposure = as.character(h[[3]][[3]])
  object=h[[3]][[4]]

  df = object[,as.vector(all.vars(h$Output_GLM$terms[[3]]))]
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
  print(ps_ranges)
}
