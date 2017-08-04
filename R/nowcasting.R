nowcasting <- function(y, vintage, legenda, q = 2, r = 2, p = 1){
  
  vintageTRANSF <- arrumarVintage(vintageok, legenda)
  
  fatores <- FactorExtraction(vintageTRANSF, q = q, r = r, p = p)
  
  prev <- forecasting(y,fatores)
  
  return(list(prev = prev, fatores = fatores))
  
}