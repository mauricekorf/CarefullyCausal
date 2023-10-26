#' Ridge plots
#'
#'Applicable for continuous exposure. It first bins the exposure into n (bins argument) categories and plots its conditional density propensity score plots
#' @param h Input a CarefullyCausal object
#' @param bins Number of categories to split the exposure variable
#'
#' @return Ridge plot, conditional density propensity score plots
#' @export
#'
#' @examples
ridge.plot <- function(h,bins=10){

  #filter on USED covariates and treatment variable
  object=h[[3]][[4]]
  outcome = as.character(h[[3]][[1]])
  covariates = as.character(h[[3]][[2]])
  exposure = as.character(h[[3]][[3]])

  df = object[,as.vector(all.vars(h$Output_GLM$terms[[3]]))]
  covariates_ps = gsub(",","+",covariates)
  no_cols_before = ncol(df)
  ps_output  <- CBPS(as.formula(paste0(exposure,"~",covariates_ps)),data = df, ATT = 0, method = "exact") #get ATE (ATT=0), exact method
  df <- as.data.frame(cbind(df,ps_output$fitted.values))

  exp_index = grep(exposure,colnames(df))


  #bin the exposure variable
  df$bins = as.factor((cut(df[,exp_index],breaks = bins)))



  # basic example
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
