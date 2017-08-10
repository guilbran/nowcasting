#' @title Nowcasting of a quarterly time serie using a dynamic factor.

#' @description Estimate nowcasting and foreacasting for a quarterly time serie. The method is based on 
#' \emph{Giannone, Domenico, Lucrezia Reichlin, and David Small. "Nowcasting: The real-time informational content of macroeconomic data." Journal of Monetary Economics 55.4 (2008): 665-676.}
#' @param y Quarterly time-series 
#' @param vintage A time series matrix (mts) representing the vintage of interest.
#' @param legenda \code{data.frame} or \code{vector}. A \code{data.frame} with two columns, the first one is the name, and the second is the transformation to let the series become stationary.
#' A \code{vector} where each coordinate is the transformation of the correspondent coordinate in the \code{mts} of the previous argument. 
#' The transformation is specified as follow:
#' # trans 0, the original serie is preserved.
#' # trans 1 \deqn{latex1}{100*\frac{X_t - X_{t-1}}{X_{t-1}}} 
#' # trans 2 \deqn{latex2}{X_t - X_{t-1}}
#' # trans 3 \deqn{latex3}{100*\frac{X_t - X_{t-12}}{X_{t-12}}  -  100*\frac{X_{t-1} - X_{t-13}}{X_{t-13}}}
#' @param q Dynamic rank. Number of error terms. If not specified q = 2.
#' @param r Static rank (r>=q), i.e. number of factors. If not specified r = 2.
#' @param p AR order of factors. If not specified p = 1.
#' @return A \code{list} containing two \code{data.frame}:
#' \code{prev} contains the original serie, the estimation in the sample, the estimation out of the sample;
#' \code{fatores} contains the common factors of vintage data set.
#' @examples 
#' # GDP index at market prices
#' pib<-BETS::BETS.get(22099)
#' 
#' # Creating real time data base with the series: 
#' # Vehicles production (1373);
#' # Credit Sales Indes (1453);
#' # Retail sales (1455);
#' # Industrial production, general index (21859).
#' mybase<-base_extraction(c(1373,1453,1455,21859))
#' 
#' # Estimate the nowcasting based on dynamic factors extracted from mybase:
#' now<-nowcast(y = pib,vintage = mybase,legenda = c(3,3,3,3),q = 1,r = 1,p = 1)
#' ts.plot(now$prev,col=1:3)
#' @seealso \code{\link[nowcasting]{base_extraction}}
#' @export


nowcast <- function(y, vintage, legenda, q = 2, r = 2, p = 1){
  
  vintageTRANSF <- arrumarVintage(vintage, legenda)
  
  fatores <- FactorExtraction(vintageTRANSF, q = q, r = r, p = p)
  
  prev <- forecasting(y,fatores)
  
  return(list(prev = prev, fatores = fatores))
  
}




