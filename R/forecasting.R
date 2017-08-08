#' @title Forecasting
#' @description Faz previsão de variável trimestral a partir de variáveis mensais. Aqui utilizamos os fatores como variáveis mensais.
#' @param y. Variável de interesse, trimestre
#' @param x. Variáveis mensais usadas para fazer a previsão


forecasting <- function(y,x){
  
  # y: ts (trimestral)
  # x: fatores (mensais - output da função FactorExtraction)
  
  # tranformar fatores mensais em trimestrais
  fatoresTS <- ts(x[,-1], end = as.numeric(c(substr(x[nrow(x),1],1,4),
                                             substr(x[nrow(x),1],6,7))), freq = 12)
  
  fatoresTRI <- mestri(fatoresTS)
  
  retpib <- diff(log(pib),4) # variação trimestre contra mesmo trimestre do ano anterior
  retpib2 <- diff(retpib) # diferença de ordem 1
  
  # estimação do modelo de regressão
  dados <- cbind(retpib2, fatoresTRI)
  colnames(dados) <- c("Y", paste0("X",1:ncol(fatoresTRI)))
  reg <- lm(Y ~ ., data = na.omit(data.frame(dados)))
  fit <- ts(fitted(reg), end = end(na.omit(dados)), freq = 4)
  
  Qmax <- max(which(is.na(dados[,1])))
  
  # previsão
  newbase <- data.frame(dados[(Qmax-4):Qmax,-1])
  prev <- ts(predict(object = reg, newdata = newbase), start = start(tail(dados,5)), freq = 4) 
  
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
  prev_fit <- exp(cbind(dados_pib[,"LOG Y"], ts(dados_pib[1:(nrow(dados_pib)-5),"LEVEL CHAPEU"], start = start(dados_pib), freq = 4),
                        ts(dados_pib[(nrow(dados_pib)-4):nrow(dados_pib),"LEVEL CHAPEU"], end = end(dados_pib), freq = 4)))
  colnames(prev_fit) <- c("y", "in","out")
  
  # RETORNAR PREVISÃO DENTRO E FORA DA AMOSTRA
  prev_fit
}

