#' @title Balanced pannel
#' @description Create a balanced pannel for data. Select variables, and transform time series to its stationary representation
#' @param base. \code{data.frame} da base de dados que representa a vintage de interesse. Na primeira coluna um vetor de datas e nas demais colunas as variáveis que compõe o vetor.
#' @param legenda. \code{data.frame} contendo uma legenda de qual a transformação necessária para induzir a estacionaridade. O nome das variáveis deve ser 
#' o mesmo do \code{data.frame} utilizado no primeiro argumento. A transformação utilizada deve ser indicada em uma coluna com o nome de \code{transf} seguindo a especificação:
#' #' Transformação 1 \deqn{latex1}{100*\frac{X_t - X_{t-1}}{X_{t-1}}} 
#' @import zoo
#' @import stats

arrumarVintage <- function(base = NULL, legenda = NULL){
  
  # base: data.frame da base de dados lida .csv
  # legenda: data.frame da legenda lida em .xlsx
  
  # base <- read.csv2("base_mes_2017-07-19.csv", header = T, stringsAsFactors = F)
  # legenda <- data.frame(read_excel("07-24-legendas.xlsx"))
  
  # séries, legenda e tranformação
  DATA <- data.frame(base)
  dates <- data.frame(data = zoo::as.Date(base))
  TransfCode <- legenda[,"transf"]
  
  # transformar os dados
  X <- matrix(NA, nrow = nrow(DATA), ncol = ncol(DATA))
  colnames(X) <- colnames(DATA)
  
  for(j in colnames(X)){
    if(legenda[which(legenda$name == j),"transf"] == 1){  # VARIAÇÃO
      temp <- (DATA[2:nrow(DATA),j] - DATA[1:(nrow(DATA)-1),j])/DATA[1:(nrow(DATA)-1),j]*100
      X[-1,j] <- temp
    }else if(legenda[which(legenda$name == j),"transf"] == 2){ # DIFERENÇA MENSAL
      temp <- DATA[2:nrow(DATA),j] - DATA[1:(nrow(DATA)-1),j]
      X[-1,j] <- temp
    }else if(legenda[which(legenda$name == j),"transf"] == 3){ # DIFERENÇA MENSAL DA VARIAÇÃO ANUAL
      temp <- (DATA[13:nrow(DATA),j] - DATA[1:(nrow(DATA)-12),j])/DATA[1:(nrow(DATA)-12),j]*100
      X[-c(1:13),j] <- temp[2:length(temp)] - temp[1:(length(temp)-1)]
    }else{ # SEM TRANSFORMAÇÃO
      temp <- DATA[,j]
      X[,j] <- temp
    }
  }
  
  X <- X[-c(1:13),]
  
  # ajustar o vetor de data e tempo  
  dates <- data.frame(data = dates[14:nrow(dates),])
  time <- data.frame(matrix(NA, ncol = 6, nrow = nrow(dates)))
  time[,1] <- as.numeric(substr(as.character(dates[,1]),1,4))
  time[,2] <- as.numeric(substr(as.character(dates[,1]),6,7))
  time[,3] <- as.numeric(substr(as.character(dates[,1]),9,10))
  time[,4:6] <- 0
  
  
  # transformação de diferença mensal/variação em trimestral
  X_temp <- data.frame(stats::filter(X, c(1,2,3,2,1), sides = 1))
  
  # remover dados que se perderam após transformação trimestral
  X <- X_temp [5:nrow(X_temp),]
  rownames(X) <- 1:nrow(X)
  dates <- data.frame(data = dates[5:nrow(X_temp),])
  time <- time[5:nrow(X_temp),]
  
  # fazer a amostra iniciar sempre no primeiro mês do trimestre
  if(time[1,2] %% 3 == 2){ # se a amostra começa no segundo mês do trimestre
    X <- X[3:nrow(X),]
    dates <- data.frame(data = as.character(dates[3:nrow(dates),]))
    time <- time[3:nrow(time),]
  }else if(time[1,2] %% 3 == 0){ # se a amostra começa no último mês do trimestre
    X <-  X[2:nrow(X),]
    dates <- data.frame(data = as.character(dates[2:nrow(dates),]))
    time <- time[2:nrow(time),]
  }
  colnames(X) <- colnames(DATA)
  
  # Tamanho do painel
  t <- nrow(X)
  N <- ncol(X)
  
  # usar apenas as séries com menos de 1/3 de missings
  SerOk <- colSums(is.na(X)) < t/3
  x <- X[, which(SerOk)]
  
  # redefinir o tamanho do painel
  t <- nrow(x)
  N <- ncol(x)
  
  # substituir missings e outliers 
  xc <- x*NA
  for(i in 1:N){
    xc[,i] <- outliers_correction(x[,i])
  }
  
  # nao substituir nas ultimas 12 linhas
  x[1:(nrow(x)-12),] <- xc[1:(nrow(x)-12),]
  x <- data.frame(data = dates, x) 
  xx <- data.frame(matrix(NA, nrow = 12, ncol = N + 1))
  colnames(xx) <- colnames(x)
  x <- data.frame(rbind(x,xx))
  rownames(x) <- 1:nrow(x)
  ultima <- seq.Date(as.Date(as.character(x[nrow(x)-12,1])), by = "month", length.out = 13)[-1] 
  x[,1] <- as.Date(as.character(x[,1]))
  x[(nrow(x)-11):nrow(x),1] <- ultima
  
  # retornar vintage transformada
  return(x)
}