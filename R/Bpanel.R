#' @title Balanced panel
#' @description This function transforms the original time series to its stationary representation following
#' the user specification. The monthly variables are agregated to represent quarterly quantities.
#' The time series with more than 1/3 missings, i.e. \code{NA}s are deleted.
#' The missings and outliers are corrected following the same method avaible in the \emph{replication files} of \emph{Giannone et al. 2008}.
#' In the end the monthly series are aggregated to quarterly quantities following the \emph{Mariano and Murasawsa 2003}.
#' 
#' We've made an important modifications on the \emph{outlier_correction} function found in the above mentioned files: Here the median of an even-sized sample is calculated by the mean of the two most central values, rather than using the largest of those numbers. Because of this modification the results obtained with the original \emph{replication files} are slightly different than those found here.
#' 
#' @param base A \code{mts} with the series to be transformed. 
#' @param trans \code{data.frame} or \code{vector}. 
#' 
#' A \code{data.frame} with two columns, the first one is the name, and the second is the transformation to let the series become stationary.
#' 
#' A \code{vector} where each coordinate is the transformation of the correspondent coordinate in the \code{mts} of the previous argument. 
#' The transformation is specified as follow:
#' \itemize{
#' \item{transf = 0: the original serie is preserved;}
#' \item{transf = 1: \deqn{\frac{X_t - X_{t-1}}{X_{t-1}}}}
#' \item{transf = 2: \deqn{X_t - X_{t-1}}}
#' \item{transf = 3: \deqn{\frac{X_t - X_{t-12}}{X_{t-12}}  -  \frac{X_{t-1} - X_{t-13}}{X_{t-13}}}}
#' \item{transf = 4: \deqn{(X_t - X_{t-12})  -  (X_{t-1} - X_{t-13})}
#' }
#' @param aggregate A \code{bolean} representing if you want aggregate the monthly variables to represent quarterly quantities.
#' @param k_ma A \code{numeric} representing the degrre of the moving average correction.
#' @references Giannone, D., Reichlin, L., & Small, D. (2008). Nowcasting: The real-time informational content of macroeconomic data. Journal of Monetary Economics, 55(4), 665-676.<doi:10.1016/j.jmoneco.2008.05.010>
#' 
#' Mariano, R. S., & Murasawa, Y. (2003). A new coincident index of business cycles based on monthly and quarterly series. Journal of applied Econometrics, 18(4), 427-443.
#' @examples 
#' # Example from database vintage:
#' Bpanel(vintage,rep(3,dim(vintage)[2]))
#' @import zoo
#' @importFrom stats filter
#' @export


Bpanel <- function(base = NULL, trans = NULL, aggregate = T,k_ma=3){
  
  if(!(is.vector(trans))){
    warning('trans must be a vector')
  }
  
  # Transformar os dados de acordo com a especificação dada
  base1<-base
  for(j in 1:dim(base)[2]){
    base1[,j]<-NA
    if(trans[j] == 1){  # TAXA DE VARIAÇÃO MENSAL
      temp <- diff(base[,j])/lag(base[,j],-1)
      base1[-1,j] <- temp
    }else if(trans[j] == 2){ # DIFERENÇA MENSAL
      temp <- diff(base[,j])
      base1[-1,j] <- temp
    }else if(trans[j] == 3){ # DIFERENÇA MENSAL DA TAXA DE VARIAÇÃO ANUAL
      temp <- diff(diff(base[,j],12)/lag(base[,j],-12))
      base1[-c(1:13),j] <- temp
    }else if(trans[j] == 4){ # DIFERENÇA MENSAL DA DIFERENÇA ANUAL
      temp <- diff(diff(base[,j],12))
      base1[-c(1:13),j] <- temp
    }else{ # SEM TRANSFORMAÇÃO
      base1[,j] <- base[,j]
    }
  }
  
  temp
  
  
  # transformação de diferença mensal/variação em trimestral
  if (aggregate==T){
  base1<-stats::filter(base1, c(1,2,3,2,1), sides = 1)
  }
  
  # fazer a amostra iniciar sempre no primeiro mês do trimestre (Por que?)
  # if(time[1,2] %% 3 == 2){ # se a amostra começa no segundo mês do trimestre
  #   X <- data.frame(X[3:nrow(X),])
  #   dates <- data.frame(data = as.character(dates[3:nrow(dates),]))
  #   time <- time[3:nrow(time),]
  # }else if(time[1,2] %% 3 == 0){ # se a amostra começa no último mês do trimestre
  #   X <-  data.frame(X[2:nrow(X),])
  #   dates <- data.frame(data = as.character(dates[2:nrow(dates),]))
  #   time <- time[2:nrow(time),]
  # }
  # colnames(X) <- nomes
  
  
  # usar apenas as séries com menos de 1/3 de missings
  SerOk <- colSums(is.na(base1)) < dim(base1)[1]/3
  base2 <- base1[, which(SerOk)]
  
  if (sum(!SerOk)>0){
  warning(paste(sum(!SerOk),'serie(s) ruled out due to lack in observations (more than 1/3 is NA).'))
  }
  
  # substituir missings e outliers 
  base3 <- base2*NA
  if (sum(SerOk)==1){
    base3<-outliers_correction(base2,k_ma)
  } else if (sum(SerOk)>1){
  for(i in 1:dim(base2)[2]){
    base3[,i] <- outliers_correction(base2[,i],k_ma)
  }
  }
  
  # nao substituir nas ultimas 12 linhas (por que as informações recentes são NA pelo timeless)
  base4<-base2
  if (sum(SerOk)==1){
  base4[1:(length(base4)-12)] <- base3[1:(length(base4)-12)] 
  } else if (sum(SerOk)>1){
  base4[1:(nrow(base4)-12),] <- base3[1:(nrow(base4)-12),]
  }
  
  
  base5<-ts(rbind(base4,matrix(NA, nrow = 12, ncol = dim(base4)[2]))
     ,start=start(base4)
     ,frequency = frequency(base4))
  
  
  return(base5)
  }
  
