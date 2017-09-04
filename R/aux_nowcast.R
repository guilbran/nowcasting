#' @title Auxiliar function for nowcast
#' @description Forecast quarterly series from monthly series.
#' @param y Dependent variable.
#' @param x Independent variable.
#' @importFrom stats cov end fitted lm median na.omit predict quantile sd start ts tsp window as.ts frequency
#' @export

aux_nowcast <- function(y,x){
  
  # y: ts (trimestral)
  # x: fatores (mensais - output da função FactorExtraction)
  
  pib<-y
  
  # tranformar fatores mensais em trimestrais, selecionando o último fator
  fatoresTS <- stats::ts(x[,-1], end = as.numeric(c(substr(x[nrow(x),1],1,4),
                                             substr(x[nrow(x),1],6,7))), frequency = 12)
  
  fatoresTRI <- mestri(fatoresTS)

  retpib <- diff(log(pib),4) # variação trimestre contra mesmo trimestre do ano anterior
  retpib2 <- diff(retpib) # diferença de ordem 1
  
  # estimação do modelo de regressão
  dados <- cbind(retpib2, fatoresTRI)
  colnames(dados) <- c("Y", paste0("X",1:ncol(data.frame(fatoresTRI))))
  reg <- stats::lm(Y ~ ., data = na.omit(data.frame(dados)))
  fit <- stats::ts(fitted(reg), end = end(na.omit(dados)), frequency = 4)
  
  Qmax <- max(which(is.na(dados[,1])))
  
  # previsão
  newbase <- data.frame(dados[(Qmax-4):Qmax,-1])
  colnames(newbase) <- paste0("X",1:ncol(data.frame(fatoresTRI)))
  
  ## função auxiliar
  tail.ts <- function(data,n) {
    data <- as.ts(data)
    window(data,start=tsp(data)[2]-(n-1)/frequency(data))
  }
  
  prev <- stats::ts(predict(object = reg, newdata = newbase), start = start(tail.ts(dados,5)), frequency = 4) 
  
  # unir fit e previsão
  dados_pib <- cbind(pib, log(pib), retpib2, fit, prev, NA)
  colnames(dados_pib) <- c("Y", "LOG Y", "D4D1 LOG Y", "AJUSTADOS", "PREVISAO", "LEVEL CHAPEU")
  
  for(i in 6:nrow(dados_pib)){
    if(!is.na(dados_pib[i,"Y"])){
      dados_pib[i,"LEVEL CHAPEU"] <- dados_pib[i,"AJUSTADOS"] + dados_pib[i-1,"LOG Y"] + dados_pib[i-4,"LOG Y"] - dados_pib[i-5,"LOG Y"] 
    }else{
      dados_pib[i,"LEVEL CHAPEU"] <- dados_pib[i,"PREVISAO"] + dados_pib[i-1,"LEVEL CHAPEU"] + dados_pib[i-4,"LEVEL CHAPEU"] - dados_pib[i-5,"LEVEL CHAPEU"] 
      
    }
  }
  prev_fit <- exp(cbind(dados_pib[,"LOG Y"], ts(dados_pib[1:(nrow(dados_pib)-5),"LEVEL CHAPEU"], start = start(dados_pib), frequency = 4),
                        stats::ts(dados_pib[(nrow(dados_pib)-4):nrow(dados_pib),"LEVEL CHAPEU"], end = end(dados_pib), frequency = 4)))
  colnames(prev_fit) <- c("y", "in","out")
  
  # RETORNAR PREVISÃO DENTRO E FORA DA AMOSTRA
  prev_fit
}

