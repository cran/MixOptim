#' Performs a restrict interval optimization
#'
#' This function performs an optimization testing within an interval defined
#' by the user using starting points and an alpha value. Since it is designed
#' for more accurate searching, it does not allow the generation of the
#' data frame for plotting.
#'
#' @param functions An array of functions
#' @param desirabilityModel A desirability \code{overallD} model
#' @param startPoint An array with the references (mid-points) for the optimization
#' @param step The ammount of each increment in the optimization
#' @param alpha Defines the range of the seach, as \code{startPoint} +- \code{alpha} for each \code{x} value
#' @param verbose Defines if the user should be updated with the processing status (percentages)
#' @return A list containg the data regarding the maximum desirability found
#' @examples
#'library(MixOptim)
#'dados <- read.table(header = TRUE, dec = ",", sep = "\t", text = "
#'x1	x2	x3	R1	R2	R3
#'1	0	0	0,76	8	5
#'1	0	0	0,75	8	5
#'0,5	0,5	0	1,4	7	7,5
#'0,5	0	0,5	0,55	8	10
#'0	1	0	4,1	4	10
#'0	1	0	4,4	4	10
#'0	0,5	0,5	0,9	7	12,5
#'0	0	1	0,42	9	15
#'0	0	1	0,4	10	15
#'0,6667	0,1667	0,1667	0,8	7	7,5
#'0,1667	0,6667	0,1667	1,7	7	10
#'0,1667	0,1667	0,6667	0,55	8	12,5
#'0,3333	0,3333	0,3333	0,8	8	10")
#'
#'lm1 <- lm(data = dados, R1 ~ -1 + x1 + x2 + x3 + x1:x2 + x1:x3 + x2:x3)
#'summary(lm1)
#'flm1 <- function(x) 0.7678*x[1] + 4.2083*x[2] + 0.4274*x[3] - 4.3273*x[1]*x[2] +
#'       0.3070*x[1]*x[3] - 5.6101*x[2]*x[3]
#'lm2 <- lm(data = dados, R2 ~ -1 + x1 + x2 + x3)
#'summary(lm2)
#'flm2 <- function(x) 7.9742*x[1] + 4.5742*x[2] + 9.3742*x[3]
#'lm3 <- lm(data = dados, R3 ~ -1 + x1 + x2 + x3)
#'summary(lm3)
#'flm3 <- function(x) 4.9998461*x[1] + 9.9998461*x[2] + 14.9998461*x[3]
#'
#'funcoes2 <- c(flm1, flm2, flm3)
#'des1<-dTarget(0.5, 0.6, 0.7)
#'des2<-dMax(8, max(dados$R2))
#'des3<-dMin(5, 10)
#'finalD<-dOverall(des1, des2, des3)
#'
#'# code commented due to process time requirement
#'#teste <- mixtureOptim(funcoes2, finalD, 3, step = 0.01, plot = TRUE)
#'#desirabilityPlot(funcoes2, teste$plotData, teste$bestComposition,
#'#   list(des1, des2, des3), c("max", "max", "min"))
#'
#'#teste2 <- mixtureFineOptim(funcoes2, finalD, teste$bestComposition, step = 0.0001)
#'#desirabilityPlot(funcoes2, teste$plotData, teste2$bestComposition,
#'#   list(des1, des2, des3), c("max", "max", "min"))
mixtureFineOptim <- function(functions, desirabilityModel, startPoint, step = 0.001, alpha = 0.02, verbose = TRUE) {

  misturaRecursao <- function(functions, desirabilityModel, step, level, value, plot) {
    value[level] = startPoint[level] + alpha
    if(level == length(value)) {
      value[level] = 0.0;
      gap <- 1.0 - sum(value)
      if(gap >= 0) {
        value[level] = gap
        #print(value)
        valores = rep(0.0, length(functions))
        for(i in 1:length(functions)) {
          valores[i] = functions[[i]](value)
        }
        tmp <- predict(desirabilityModel, as.data.frame(matrix(valores, nrow=1)))
        if(tmp > best) {
          # not acessing global environment, just the upper scope -- still inside the main function
          best <<- tmp
          bestValues <<- value
        }
      }
    } else {
      while(value[level] >= startPoint[level] - alpha) {
        misturaRecursao(functions, desirabilityModel, step, level + 1, value, plot)
        value[level] = value[level] - step
      }
    }

  }

  best <- 0
  bestValues <- startPoint
  if(verbose) {
    message("Optimizing values.. 0 % ")
  }
  if(length(startPoint) > 0) {
    vetor <- startPoint - alpha;
    count <- 0
    steps = (alpha*2)/(step)
    while(vetor[1] <= startPoint[1] + alpha) {
      misturaRecursao(functions, desirabilityModel, step, 2, vetor, plot)
      vetor[1] = vetor[1] + step
      count = count + 1
      perc = (count * 100.0) / steps
      if(verbose) if(perc %% 10 == 0) {
        message(paste(perc,"% "))
      }
    }
  }
  retorno <- list("maxDesirability" = best, "bestComposition" = bestValues)
  return(retorno)
}
