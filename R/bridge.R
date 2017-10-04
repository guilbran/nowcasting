#' @importFrom stats cov end fitted lm median na.omit predict quantile sd start ts tsp window as.ts frequency
#' @import zoo

bridge <- function(y,x){
  
  # y: ts (trimestral)
  # x: fatores (mensais - output da função FactorExtraction)
  
  # tranformar fatores mensais em trimestrais, selecionando o último fator
  # fatoresTS <- stats::ts(x[,-1], end = as.numeric(c(substr(x[nrow(x),1],1,4),
  #                                                   substr(x[nrow(x),1],6,7))), frequency = 12)
  fatoresTS <- x
  fatoresTRI <- month2qtr(fatoresTS)
  

  # estimação do modelo de regressão
  dados <- cbind(y, fatoresTRI)
  colnames(dados) <- c("Y", paste0("X",1:ncol(data.frame(fatoresTRI))))
  reg <- stats::lm(Y ~ ., data = na.omit(data.frame(dados)))
  fit <- stats::ts(fitted(reg), end = end(na.omit(dados)), frequency = 4)
  
  Qmax <- max(which(!is.na(dados[,1])))
  edge<-zoo::as.Date(dados)[Qmax]
  
  # previsão
  newbase <- data.frame(dados[-(1:(Qmax-1)),-1])
  colnames(newbase) <- paste0("X",1:ncol(data.frame(fatoresTRI)))
  
  ## função auxiliar
  # tail.ts <- function(data,n) {
  #   data <- as.ts(data)
  #   window(data,start=tsp(data)[2]-(n-1)/frequency(data))
  # }
  
  ano<-as.numeric(substr(edge,1,4))
  tri<-as.numeric(substr(quarters(edge),2,2))
  
  prev <- stats::ts(predict(object = reg, newdata = newbase),
                    start = c(ano,tri),
                    frequency = 4) 
  
  dados_pib<-cbind(y,fit,prev)

  colnames(dados_pib) <- c("y", "in","out")
  
  # RETORNAR PREVISÃO DENTRO E FORA DA AMOSTRA
  list(main = dados_pib,reg = reg)
}

