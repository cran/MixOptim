#' Performs a full interval optimization
#'
#' This function performs a full interval optimization (0-1 for each \code{x} variable).
#' It allows the creation of the data frame used for plotting.
#'
#' @param functions An array of functions
#' @param desirabilityModel A desirability \code{overallD} model
#' @param xCount The amount of \code{x} variables used in the functions
#' @param step The ammount of each increment in the optimization
#' @param plot Define is the data frame that can be used for the \code{desirabilityPlot} function will be create. Strongly affects performance
#' @param verbose Defines if the user should be updated with the processing status (percentages)
#' @return A list containg the data regarding the maximum desirability found
#' @examples
#'library(MixOptim)
#'saPred<-function(x) 17.3359 * x[1] + 30.7765 * x[2] + 20.0501 * x[3] + 0.7506 * x[1] * x[2] +
#'   (-6.3443 * x[1] * x[3]) + (-5.9291 * x[2] * x[3]) +(-25.3093 * x[1] * x[2] * x[3])
#'pvPred<-function(x)  0.884640 * x[1] + 0.789863 * x[2] + 0.825016 * x[3] +
#'   (-0.108964 * x[1] * x[2]) + 0.107167 * x[1] * x[3] + (-0.005220 * x[2] * x[3]) +
#'   1.625246 * x[1] * x[2] * x[3]
#'
#'funcoes <- c(saPred, pvPred)
#'saD<-dMin(16.8293856, 31.41170555) #minimun value
#'pvD<-dMax(0.7796004, 0.9019796) #maximum value
#'overallD<-dOverall(saD, pvD)
#'
#'a <- mixtureOptim(funcoes, overallD, 3, step = 0.05, plot = TRUE)
#'a$bestComposition
#'# next line will generate a ggplot/patchwork grid
#'#desirabilityPlot(funcoes, a$plotData, a$bestComposition, list(saD, pvD), c("min", "max"))
mixtureOptim <- function(functions, desirabilityModel, xCount, step = 0.01, plot = TRUE, verbose = TRUE) {

  misturaRecursao <- function(functions, desirabilityModel, step, level, value, plot) {
    gap <- 1.0 - sum(value)
    value[level] = gap;
    if(level == length(value)) {
      #print(value)
      valores = rep(0.0, length(functions))
      for(i in 1:length(functions)) {
        valores[i] = functions[[i]](value)
      }
      tmp <- predict(desirabilityModel, as.data.frame(matrix(valores, nrow=1)))
      if(plot)
        grafico <<- rbind(grafico, c(value, tmp, valores))
      if(tmp > best) {
        # not acessing global environment, just the upper scope -- still inside the main function
        best <<- tmp
        bestValues <<- value
      }
    } else {
      while(value[level] >= 0.0) {
        misturaRecursao(functions, desirabilityModel, step, level + 1, value, plot)
        value[level] = value[level] - step
      }
    }

  }

  best <- 0
  if(plot) {
    grafico <- data.frame()
  }
  bestValues <- rep(0, xCount)
  if(verbose) {
    message("Optimizing values.. 0 % ")
  }
  if(xCount > 0) {
    vetor <- rep(0, xCount)
    count <- 0
    while(vetor[1] <= 1.0) {
      misturaRecursao(functions, desirabilityModel, step, 2, vetor, plot)
      vetor[1] = vetor[1] + step
      count = count + 1
      if(verbose) {
        if(count %% 10 == 0) {
          message(paste(count,"% "))
        }
      }
    }
  }
  retorno <- list("maxDesirability" = best, "bestComposition" = bestValues)
  if(plot) {
    retorno$plotData <- grafico
  }
  return(retorno)
}
