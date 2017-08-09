#' @title Nowcasting of a quarterly time serie using a dynamic factor.

#' @description Essa função calcula uma estimativa para uma variável trimestral \(tipicamente o PIB\), baseado em um modelo de fatores dinâmicos, seguindo a estimação proposta
#' em Giannone et al. \(2008\).

#' @param y. Quarterly time-series 
#' @param vintage. A time series matrix \(mts\) representing the vintage of interest.
#' @param legenda. A data.frame with two columns, the first one is the name, and the second is the transformation to let the series become stationary.
#' @param q. Dynamic rank. Number of error terms. If not specified q = 2.
#' @param r. Static rank \(r>=q\), i.e. number of factors. If not specified r = 2.
#' @param p. AR order of factors. If not specified p = 1.
#' @export



nowcasting <- function(y, vintage, legenda, q = 2, r = 2, p = 1){
  
  vintageTRANSF <- arrumarVintage(vintage, legenda)
  
  fatores <- FactorExtraction(vintageTRANSF, q = q, r = r, p = p)
  
  prev <- forecasting(y,fatores)
  
  return(list(prev = prev, fatores = fatores))
  
}