#' @title  Seasonal ajustment
#' @description DGP seasonal ajustment, set as SARIMA\(0,1,1\)\(0,1,1\) 

# Função que faz o ajuste sazonal do PIB

ajustePIB <- function(x){
  ajuste <- seas(x, x11 = "", arima.model = "(0 1 1)(0 1 1)")
  round(ajuste$series$d11,2)
}

