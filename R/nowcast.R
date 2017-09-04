#' @title Nowcasting of a quarterly time serie using a dynamic factor.

#' @description Estimate nowcasting and foreacasting for a quarterly time serie. The method is based on 
#' \emph{Giannone, Domenico, Lucrezia Reichlin, and David Small. "Nowcasting: The real-time informational content of macroeconomic data." Journal of Monetary Economics 55.4 (2008): 665-676.}
#' @param y Stationary quarterly time-series 
#' @param regressors A time series matrix (\code{mts}) representing the regressors of interest.
#' @param legend \code{data.frame} or \code{vector}. A \code{data.frame} with two columns, the first one is the name, and the second is the transformation to let the series become stationary.
#' A \code{vector} where each coordinate is the transformation of the correspondent coordinate in the \code{mts} of the previous argument. 
#' The transformation is specified as follow:
#' \itemize{
#' \item{transf = 0: the original serie is preserved;}
#' \item{transf = 1: \deqn{latex1}{100*\frac{X_t - X_{t-1}}{X_{t-1}}}}
#' \item{transf = 2: \deqn{latex2}{X_t - X_{t-1}}}
#' \item{transf = 3:\deqn{latex3}{100*\frac{X_t - X_{t-12}}{X_{t-12}}  -  100*\frac{X_{t-1} - X_{t-13}}{X_{t-13}}}}
#' }
#' @param q Dynamic rank. Number of error terms. If not specified q = 2.
#' @param r Static rank (r>=q), i.e. number of factors. If not specified r = 2.
#' @param p AR order of factors. If not specified p = 1.
#' @return A \code{list} containing two \code{data.frame}:
#' \code{prev} contains the original serie, the estimation in the sample, the estimation out of the sample;
#' \code{fatores} contains the common factors of vintage data set.
#' @examples
#' # GDP index at market prices at quarterly frequency
#' pib<-mestri(lag(base_extraction(22099),-2))
#' 
#' # Estimate the nowcasting based on dynamic factors extracted from vintage:
#' # 1 dynamic factor
#' # 1 static factor
#' # 1 autoregressive lag
#' now<-nowcast(y = pib,vintage = vintage,legend = rep(3,7),q = 1,r = 1,p = 1)
#' # nowcasting results
#' now$prev
#' # factor estimation
#' now$fatores
#' 
#' # Estimate the nowcasting based on dynamic factors extracted from vintage:
#' # 1 dynamic factor
#' # 1 static factor
#' # 2 autoregressive lag
#' now<-nowcast(y = pib,vintage = vintage,legend = rep(3,7),q = 1,r = 1,p = 2)
#' # nowcasting results
#' now$prev
#' # factor estimation
#' now$fatores
#' 
#' # Estimate the nowcasting based on dynamic factors extracted from vintage:
#' # 1 dynamic factor
#' # 2 static factor
#' # 1 autoregressive lag
#' # induce stationarity in first diference (transf = 2)
#' now<-nowcast(y = pib,vintage = vintage,legend = rep(3,7),q = 1,r = 2,p = 1)
#' ts.plot(now$prev,col=1:3)
#' # nowcasting results
#' now$prev
#' # factor estimation
#' now$fatores
#' 
#' # Estimate the nowcasting based on dynamic factors extracted from vintage:
#' # 2 dynamic factor
#' # 2 static factor
#' # 1 autoregressive lag
#' # induce stationarity in first diference (transf = 2)
#' now<-nowcast(y = pib,vintage = vintage,legend = rep(3,7),q = 2,r = 2,p = 1)
#' ts.plot(now$prev,col=1:3)
#' # nowcasting results
#' now$prev
#' # factor estimation
#' now$fatores
#' 
#' \dontrun{
#' # GDP index at market prices
#' pib<-mestri(lag(base_extraction(22099),-2))
#' 
#' # Creating real time data base with the series:
#' # Exchange rate - Free - United States dollar (1);
#' # Interest rate - CDI (12);
#' # Vehicles production (1373);
#' # Credit Sales Index (1453);
#' # Retail sales (1455);
#' # Current economic conditions index (4394);
#' # Industrial production, general index (21859).
#' mybase<-base_extraction(c(1453,1455,4394,21859))
# now<-nowcast(y = pib, vintage = mybase,legend=c(3,3,3,3),q=1,r=1,p=1)
#' }
#' @seealso \code{\link[nowcasting]{base_extraction}}
#' @export


nowcast <- function(y, regressors, legend, q = 2, r = 2, p = 1){
  
  vintageTRANSF <- Bpanel(vintage, legend)
  
  fatores0 <- FactorExtraction(vintageTRANSF, q = q, r = r, p = p)
  
  fatores <- fatores0$fator_final
  
  prev <- aux_nowcast2(y,fatores)
  
  return(list(main = prev$main, reg = prev$reg, factors = fatores0))
  
}




