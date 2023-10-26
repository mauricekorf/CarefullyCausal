#' Propensity score plots
#'
#' Provides all relevant plots with respect to the reference exposure category. Applicable when exposure is discrete.
#'
#' @param h Input a CarefullyCausal object
#'
#' @return Propensity score plots
#' @export
#'
#' @examples
ps.plot <- function(h){

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
    legend("topright",legend = levels(factor(eval(parse(text = paste0("df$",exposure))))),col = plotcolors[1:length(list1[[j]])], lty = 1, bty = "n")
  }

}#end of function
