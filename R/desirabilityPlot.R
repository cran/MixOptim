#' Plot desirability profile
#'
#' This function creates a graphical representation of the desirability profiles
#' within the data range. It requires a data frame object generated from
#' an optimization function. You can use a simpler data frame to plot the lines and
#' present a more accurate data from another optimization call.
#'
#' @param functions An array of functions
#' @param plotData A data frame generated from an optimization function
#' @param bestValues The optimal mixture composition to be presented
#' @param desirab An array of desirability functions
#' @param types An array of strings containing the attributes to be shown for each desirability function. Currently only accepts \code{"max"} or \code{"min"}
#' @return A ggplot composite object from patchwork
#' @export
desirabilityPlot <- function(functions, plotData, bestValues, desirab, types) {
  roundDf <- function(x, digits) {
    numeric_columns <- sapply(x, mode) == 'numeric'
    x[numeric_columns] <-  round(x[numeric_columns], digits)
    x
  }

  plotData <- roundDf(plotData, 5)

  dfs <- list()
  dfMax <- list()
  dfMin <- list()
  #dfMed <- list()
  for(i in 1:length(bestValues)) {
    index = length(bestValues) + 1
    dfs[[i]] <- data.frame(plotData[[i]], plotData[[index]])
    for(j in 1:length(functions)) {
      pos = length(plotData) - length(functions) + j
      dfs[[i]] <- cbind(dfs[[i]], plotData[[pos]])
    }
    dfMax[[i]] <- aggregate(dfs[[i]], list(dfs[[i]][,1]), max)
    dfMin[[i]] <- aggregate(dfs[[i]], list(dfs[[i]][,1]), min)
    #dfMed[[i]] <- aggregate(dfs[[i]], list(dfs[[i]][,1]), mean)

    colnames(dfMax[[i]]) <- sub("\\[.*\\]","", names(dfMax[[i]]))
    colnames(dfMax[[i]]) <- make.unique(names(dfMax[[i]]))
    colnames(dfMin[[i]]) <- sub("\\[.*\\]","", names(dfMin[[i]]))
    colnames(dfMin[[i]]) <- make.unique(names(dfMin[[i]]))
  }
  #print(head(dfMin[[1]], n=6))
  #print(head(dfMed[[1]], n=3))
  #print(head(dfMax[[1]], n=6))
  p <- ggplot()
  for(i in 1:length(functions)) {
    if(types[i] == "max") {
      for(j in 1:length(bestValues)) {
        p1 <- ggplot(dfMax[[j]], aes_string(x = names(dfMax[[j]])[1], y = names(dfMax[[j]])[3 + i])) +
          #geom_ribbon(aes(ymax = grafico.y2, ymin = grafico.y2 - (grafico.y2 - mediaY2)), fill = "grey90") +
          geom_line() +  theme_bw() +
          ylim(min(plotData[,length(plotData)-length(functions) + i]), max(plotData[,length(plotData)-length(functions) + i])) +
          labs(x=paste("x",j), y=paste("y",i)) + geom_vline(xintercept = bestValues[j], color="red") +
          geom_hline(yintercept = functions[[i]](bestValues), linetype="dashed", color = "blue")
        if(i == 1) {
          if(j == 1) {
            p <- p1
          } else {
            p <- p + p1
          }
        } else {
          p <- p + p1
        }
      }

      pvDData <- data.frame(inp = seq(from = desirab[[i]]$low, to = desirab[[i]]$high, length = 100))
      pvDData$outp <- predict(desirab[[i]], pvDData$inp)
      des1 <- ggplot(pvDData, aes(x = inp, y = outp)) + geom_line() +
        theme_bw() + labs(y="Desirability") + scale_y_continuous(position = "right")
      p <- p + des1;

    } else if(types[i] == "min") {
      for(j in 1:length(bestValues)) {
        p2 <- ggplot(dfMin[[j]], aes_string(x = names(dfMin[[j]])[1], y = names(dfMin[[j]])[3 + i])) +
          #geom_ribbon(aes(ymax = grafico.y2, ymin = grafico.y2 - (grafico.y2 - mediaY2)), fill = "grey90") +
          geom_line() +  theme_bw() +
          ylim(min(plotData[,length(plotData)-length(functions) + i]), max(plotData[,length(plotData)-length(functions) + i])) +
          labs(x=paste("x",j), y=paste("y",i)) + geom_vline(xintercept = bestValues[j], color="red") +
          geom_hline(yintercept = functions[[i]](bestValues), linetype="dashed", color = "blue")
        #print(functions[[i]](bestValues))
        if(i == 1) {
          if(j == 1) {
            p <- p2
          } else {
            p <- p + p2
          }
        } else {
          p <- p + p2
        }
      }
      pvDData <- data.frame(inp = seq(from = desirab[[i]]$low, to = desirab[[i]]$high, length = 100))
      pvDData$outp <- predict(desirab[[i]], pvDData$inp)
      des1 <- ggplot(pvDData, aes(x = inp, y = outp)) + geom_line() +
        theme_bw() + labs(y="Desirability") + scale_y_continuous(position = "right")
      p <- p + des1;
    }
  }

  for(j in 1:length(bestValues)) {
    p1 <- ggplot(dfMax[[j]], aes_string(x = names(dfMin[[j]])[1], y = names(dfMin[[j]])[3])) +
      geom_line() +  theme_bw() + ylim(min(plotData[,length(bestValues)+ 1]), max(plotData[,length(bestValues)+ 1])) +
      labs(x=paste("x",j), y="Desirability") + geom_vline(xintercept = bestValues[j], color="red") +
      geom_text(x=bestValues[j] + 0.01, y=0, angle=0, vjust=-0.4, hjust=0, label=round(bestValues[j], 4))
    p <- p + p1
  }

  grade <- p + plot_layout(ncol = length(bestValues) + 1)

  divisor <- length(bestValues) + 1
  limite <- (divisor*(length(functions) + 1)-1)
  for(k in 1:limite) {
    if(k > limite - divisor) {
      if(k%%divisor > 1) {
        grade[[k]] = grade[[k]] + theme(axis.text.y = element_blank(), axis.ticks.y = element_blank(), axis.title.y = element_blank())
      } else if(k%%divisor < 1) {
        grade[[k]] = grade[[k]] + theme(axis.text.x = element_blank(), axis.title.x = element_blank(), axis.ticks.x = element_blank())
      }
    } else {
      if(k%%divisor == 1) {
        grade[[k]] = grade[[k]] + theme(axis.text.x = element_blank(), axis.ticks.x = element_blank(), axis.title.x = element_blank())
      } else if(k%%divisor > 1) {
        grade[[k]] = grade[[k]] + theme(axis.text.y = element_blank(), axis.ticks.y = element_blank(), axis.title.y = element_blank(),
                                        axis.text.x = element_blank(), axis.ticks.x = element_blank(), axis.title.x = element_blank())
      } else {
        grade[[k]] = grade[[k]] + theme(axis.text.x = element_blank(), axis.ticks.x = element_blank(), axis.title.x = element_blank())
      }
    }
  }

  return(grade)
}
