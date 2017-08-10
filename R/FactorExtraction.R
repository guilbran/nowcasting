#' @title Factor Extraction
#' @description Extract factors from a data set of time series.
#' @param x Vintage transformada pela função \code{arrumarVintage}
#' @param q Dynamic rank. Number of error terms. If not specified q = 2.
#' @param r Static rank (r>=q), i.e. number of factors. If not specified r = 2.
#' @param p AR order of factors. If not specified p = 1.
#' @param A Matrix that update factors with VAR
#' @param C Matrix that combine factors to explain the transformed data.
#' @param Q Error variance in factor update.
#' @param R Error variance in explain data from factors
#' @param initx Initial condition of factor in Kalman filter estimation.
#' @param initV Initial condition of factor variance
#' @param ss Standard deviation in transformed data.
#' @param MM Mean in transformed data.
#' @param n.prevs Number of quarter previsions ahead.



FactorExtraction <- function(x = NULL,q = NULL,r = NULL,p = NULL, 
                             A = NULL,C = NULL,Q = NULL,R = NULL,
                             initx = NULL, initV = NULL,
                             ss = NULL, MM = NULL, n.prevs = NULL){
  
  # The model
  # x_t = C F_t + \xi_t
  # F_t = AF_{t-1} + B u_t
  # R = E(\xi_t \xi_t')
  # Q = BB'
  # u_t ~ WN(0,I_q)
  # initx = F_0
  # initV = E(F_0 F_0')
  # ss: std(x) 
  # MM: mean(x) 
  
  # q: dynamic rank
  # r: static rank (r>=q)
  # p: ar order of the state vector (default p=1)
  
  # Ft: estimated factors
  # VF: estimation variance for the common factors
  
  
  # x = series1
  # q = 2; r = 2; p = 1;
  # A = NULL; C = NULL; Q = NULL;R = NULL;
  # initx = NULL; initV = NULL; ss = NULL; MM = NULL
  
  #finalx <- x
  #x <- finalx[,1:(ncol(finalx)-1)]
  
  coluna1 <- x[,1]
  x <- x[,-1]
  
  # tamanho da base
  TT <- nrow(x)
  N <- ncol(x)
  
  # Construir o painel balanceado (sem missings) Z a partir de x (base de entrada)
  # ignorar séries com mais de 1/3 de missings
  
  n.missings <- colSums(is.na(x))
  m <- max(n.missings)
  
  # cálculo do número de argumentos fornecidos
  n.arg <- sum(c(!is.null(x),!is.null(q),!is.null(r),!is.null(p),
                 !is.null(A),!is.null(C),!is.null(Q),!is.null(R),
                 !is.null(initx), !is.null(initV), 
                 !is.null(ss), !is.null(MM)))
  
  if(n.arg < 5){ # estimar os parâmetros se eles não são fornecidos
    
    # padronizar a série balanceada para estimar os parâmetros no primeiro estágio (via PCA)
    z <- x[1:(TT - m),]
    s <- apply(z, MARGIN = 2, FUN = sd)
    M <- apply(z, MARGIN = 2, FUN = mean)
    
    for(i in 1:N){
      x[,i] <- (x[,i] - M[i])/s[i]
    }
    z <- x[1:(TT - m),]
    
    parametros <- pcatodfm(z,q,r,p)
    
    A <- parametros$A
    C <- parametros$C
    Q <- parametros$Q
    R <- parametros$R
    initx <- parametros$initx
    initV <- parametros$initV
    #print(A)
  }else{
    
    # se os parâmetros são fornecidos, basta padronizar
    
    for(i in 1:N){
      x[,i] <- (x[,i] - MM[i])/ss[i]
    }
    z <- x[1:(TT - m),]
    
  }
  
  # parâmetros pro modelo em espaço de estados
  
  AA <- array(A, dim = c(nrow(A), ncol(A), TT))
  QQ <- array(Q, dim = c(nrow(Q), ncol(Q), TT))
  CC <- array(C, dim = c(nrow(C), ncol(C), TT))
  miss <- is.na(x)
  RR <- array(NA, dim = c(nrow(R), ncol(R), TT))
  
  for(i in 1:TT){
    Rtemp <- diag(R)
    Rtemp[miss[i,]] <- 1e32
    RR[,,i] <- diag(Rtemp)
  }
  
  xx <- x
  xx[is.na(x)] <- 0 # atribuir valor arbitrário para missing
  
  # FUNÇÃO KALMAN SMOOTHER DIAG
  
  resul <- kalman_smoother_diag(t(xx), AA, CC, QQ, RR, initx, initV, list('model',1:TT))
  
  xsmooth <- resul$xsmooth
  Vsmooth <- resul$Vsmooth
  VVsmooth <- resul$VVsmooth
  loglik <- resul$loglik
  
  VF <- Vsmooth
  ind <- size(VF,3)
  fatores <-  t(xsmooth)
  
  nomes_colunas <- c("data", paste0("Fator",1:ncol(fatores)))
  fator_final <- data.frame(coluna1, fatores)
  colnames(fator_final) <- nomes_colunas
  fator_final
}