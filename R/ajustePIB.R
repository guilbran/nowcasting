#' @title  Seasonal ajustment
#' @description GDP seasonal ajustment. We use the univariate model SARIMA(0,1,1)(0,1,1) to specify the index variable
#' @import seasonal

# Função que faz o ajuste sazonal do PIB

ajustePIB <- function(x){
  ajuste <- seasonal::seas(x, x11 = "", arima.model = "(0 1 1)(0 1 1)")
  round(ajuste$series$d11,2)
}

