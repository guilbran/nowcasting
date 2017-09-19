#' @title Nowcasting of a quarterly time serie using a dynamic factor.

#' @description Estimate nowcasting and foreacasting for a quarterly time serie. The method is based on 
#' \emph{Giannone, Domenico, Lucrezia Reichlin, and David Small. "Nowcasting: The real-time informational content of macroeconomic data." Journal of Monetary Economics 55.4 (2008): 665-676.}
#' @param y Stationary quarterly time-series 
#' @param regressors A time series matrix (\code{mts}) representing the regressors of interest. The series must be stationary.
#' @param q Dynamic rank. Number of error terms. If not specified q = 2.
#' @param r Static rank (r>=q), i.e. number of factors. If not specified r = 2.
#' @param p AR order of factors. If not specified p = 1.
#' @return A \code{list} containing two elements:
#' 
#' A \code{data.frame} named \code{main} contains the original serie, the estimation in the sample, the estimation out of the sample;
#' 
#' A \code{list} named \code{factors} contains the estimated factors and coeffients..
#' @references Giannone, D., Reichlin, L., & Small, D. (2008). Nowcasting: The real-time informational content of macroeconomic data. Journal of Monetary Economics, 55(4), 665-676.<doi:10.1016/j.jmoneco.2008.05.010>
#' @examples
#' pib<-base_extraction(22099)
#' now<-nowcast(lag(pib,-2),Bpanel(vintage,rep(3,dim(vintage)[2])))
#' now$prev
#' now$factors
#' @seealso \code{\link[nowcasting]{base_extraction}}
#' @export

nowcast <- function(y, regressors, q = 2, r = 2, p = 1){
  
  vintageTRANSF <- regressors
  
  fatores0 <- FactorExtraction(vintageTRANSF, q = q, r = r, p = p)
  
  fatores <- fatores0$fator_final
  
  prev <- bridge(monqua(y),fatores)
  
  return(list(main = prev$main, reg = prev$reg, factors = fatores0))
  
}
