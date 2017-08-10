#' @title  Seasonal adjustment
#' @description GDP seasonal ajustment. We use the univariate model SARIMA(0,1,1)(0,1,1) to specify the index variable
#' @param x Time series to be adjusted 
#' @import seasonal

ajustePIB <- function(x){
  ajuste <- seasonal::seas(x, x11 = "", arima.model = "(0 1 1)(0 1 1)")
  round(ajuste$series$d11,2)
}

