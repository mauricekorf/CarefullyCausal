#' Covariate balance plots
#'
#'It checks covariate balance after running CarefullyCausal when exposure is continuous
#' @param h Input a CarefullyCausal object
#'
#' @return Covariate balance plots
#' @export
#'
#' @examples
gps_plot <- function(h){

  object=output[[3]][[4]]
  covariates = as.character(output[[3]][[2]])
  exposure = as.character(output[[3]][[3]])

  df = object[,as.vector(all.vars(output$Output_GLM$terms[[3]]))]
  covariates_ps = gsub(",","+",covariates)
  ps_output  <- CBPS(as.formula(paste0(exposure,"~",covariates_ps)),data = df, ATT = 0, method = "exact") #get ATE (ATT=0), exact method


  # get plots
  # remove interactions
  # add name per plot in list
  # add plots of discrete setting to output
  plots = list()
  sep = strsplit(covariates, split = ", ")

  plots[["Covariate_balance"]] <- love.plot(ps_output, stats = c("c"),
                                            thresholds = c(cor = .1),
                                            abs = TRUE, wrap = 20,
                                            limits = list(ks = c(0, .5)),
                                            var.order = "unadjusted", line = TRUE)

  for (i in 1:length(sep[[1]])){
    plots[[sep[[1]][[i]]]] = bal.plot(ps_output,sep[[1]][[i]], which = "both")
  }
  return(plots)
}
