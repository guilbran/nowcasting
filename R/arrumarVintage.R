#' @title Balanced pannel
#' @description This function transforms the original time series to its stationary representation following
#' the user specification. The monthly variables are agregated to represent quarterly quantities.
#' The time series with more than 1/3 missings, i.e. \code{NA}s are deleted.
#' In the end the missings and outliers are corrected.
#' @param base A time series matrix (mts) representing the vintage of interest.
#' @param legenda \code{data.frame} or \code{vector}. A \code{data.frame} with two columns, the first one is the name, and the second is the transformation to let the series become stationary.
#' A \code{vector} where each coordinate is the transformation of the correspondent coordinate in the \code{mts} of the previous argument. 
#' The transformation is specified as follow:
#' # trans 0, the original serie is preserved.
#' # trans 1 \deqn{latex1}{100*\frac{X_t - X_{t-1}}{X_{t-1}}} 
#' # trans 2 \deqn{latex2}{X_t - X_{t-1}}
#' # trans 3 \deqn{latex3}{100*\frac{X_t - X_{t-12}}{X_{t-12}}  -  100*\frac{X_{t-1} - X_{t-13}}{X_{t-13}}}
#' @import zoo
#' @importFrom stats filter
#' @export



arrumarVintage <- function(base = NULL, legenda = NULL){
  
  # base: data.frame da base de dados lida .csv
  # legenda: data.frame da legenda lida em .xlsx
  
  # séries, legenda e tranformação
  DATA <- data.frame(base)
  dates <- data.frame(data = zoo::as.Date(base))
  
  if (is.data.frame(legenda)){
    if ('name' %in% colnames(legenda) & 'transf' %in% colnames(legenda)){
      TransfCode <- legenda[,"transf"]
    } else if (!('name' %in% colnames(legenda))){
      warning('There is no column called name')
    }else if (!('transf' %in% colnames(legenda))){
      warning('There is no column called transf')
    }else
      warning('The data.frame must have a column called name and a column called transf')
  } else if (is.vector(legenda)){
    legenda<-data.frame(name = colnames(base),transf = legenda)
    TransfCode <- legenda[,"transf"]
  } else 
    warning('legenda must be a vector or a data.frame')

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
  if (sum(SerOk)==1){
    t<-length(x)
    N<-1
  } else if (sum(SerOk>1)){
  t <- nrow(x)
  N <- ncol(x)
  }
  
  # substituir missings e outliers 
  xc <- x*NA
  if (sum(SerOk)==1){
    xc<-outliers_correction(x)
  } else if (sum(SerOk>1)){
  for(i in 1:N){
    xc[,i] <- outliers_correction(x[,i])
  }
  }
  
  # nao substituir nas ultimas 12 linhas (por que as informações recentes são NA pelo timeless)
  if (sum(SerOk)==1){
  x[1:(length(x)-12)] <- xc[1:(length(x)-12)] 
  } else if (sum(SerOk>1)){
  x[1:(nrow(x)-12),] <- xc[1:(nrow(x)-12),]
  }
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
  