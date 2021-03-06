% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/mixtureRangeOptim.R
\name{mixtureRangeOptim}
\alias{mixtureRangeOptim}
\title{Performs a specific range optimization}
\usage{
mixtureRangeOptim(
  functions,
  desirabilityModel,
  midPoints,
  alpha,
  step = 0.01,
  plot = TRUE,
  verbose = TRUE
)
}
\arguments{
\item{functions}{An array of functions}

\item{desirabilityModel}{A desirability \code{overallD} model}

\item{midPoints}{An array with the references (mid-points) for the optimization}

\item{alpha}{Defines the range of the seach, as \code{startPoint} +- \code{alpha} for each \code{x} value}

\item{step}{The ammount of each increment in the optimization}

\item{plot}{Define is the data frame that can be used for the \code{desirabilityPlot} function will be create. Strongly affects performance}

\item{verbose}{Defines if the user should be updated with the processing status (percentages)}
}
\value{
A list containg the data regarding the maximum desirability found
}
\description{
This function performs an optimization testing within an interval defined
by the user using alpha values for each middle point provided. It allows the
generation of the data frame required for plotting.
}
\examples{
library(MixOptim)
dados <- read.table(header = TRUE, sep = "\t", text = "
ID	TiO2	Vehicle	Extender A	Extender B	Hiding	Scrub
1	0.05	0.20	0.30	0.45	7.8953	533.67
2	0.45	0.20	0.30	0.05	32.862	749
3	0.05	0.60	0.30	0.05	3.721	39.5
4	0.05	0.20	0.70	0.05	9.2751	203.25
5	0.25	0.20	0.30	0.25	20.132	555.25
6	0.05	0.40	0.30	0.25	4.7137	51.75
7	0.05	0.20	0.50	0.25	8.3829	342.75
8	0.25	0.40	0.30	0.05	16.245	84.75
9	0.25	0.20	0.50	0.05	22.639	360.75
10	0.05	0.40	0.50	0.05	5.4645	48
11	0.05	0.33	0.43	0.18	5.8882	76
12	0.18	0.20	0.43	0.18	17.256	386.25
13	0.18	0.33	0.30	0.18	12.351	136
14	0.18	0.33	0.43	0.05	14.499	75.5
15	0.10	0.25	0.35	0.30	10.548	325.75
16	0.30	0.25	0.35	0.10	22.096	359
17	0.10	0.45	0.35	0.10	6.2888	40.75
18	0.10	0.25	0.55	0.10	10.629	136.67
19	0.15	0.30	0.40	0.15	11.777	114")

hiding<-function(x) 67.748*x[1] + 7.291*x[2] + 11.419*x[3] + 14.578*x[4] -
   64.32*x[1]*x[2] + 35.878*x[1]*x[3] - 15.696*x[1]*x[4] - 31.006*x[2]*x[3] -
   38.668*x[2]*x[4] - 6.59*x[3]*x[4]
scrub<-function(x) 3937.5*x[1] + 899.3*x[2] + 502*x[3] + 2354.8*x[4] -
   8227.2*x[1]*x[2] - 3227.4*x[1]*x[3] - 2447.7*x[1]*x[4] - 2435.3*x[2]*x[3] -
   6325.1*x[2]*x[4] - 1050.3*x[3]*x[4]

funcoes2 <- c(hiding, scrub)
des1<-dMax(min(dados$Hiding), max(dados$Hiding))
des2<-dMin(min(dados$Scrub), max(dados$Scrub))
finalD<-dOverall(des1, des2)

# code commented due to process time requirement
#teste <- mixtureRangeOptim(funcoes2, finalD, midPoints = c(0.25, 0.4, 0.5, 0.25),
#    alpha = c(0.2, 0.2, 0.2, 0.2), step = 0.01, plot = TRUE)
#desirabilityPlot(funcoes2, teste$plotData, teste$bestComposition, list(des1, des2),
#    c("max", "min"))
#teste2 <- mixtureRangeOptim(funcoes2, finalD, midPoints = teste$bestComposition,
#    alpha = c(0.01, 0.01, 0.01, 0), step = 0.001, plot = FALSE)
#teste2
#desirabilityPlot(funcoes2, teste$plotData, teste2$bestComposition, list(des1, des2),
#    c("max", "min"))
}
