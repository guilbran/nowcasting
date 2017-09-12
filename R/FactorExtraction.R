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
  
  # Base dimension
  TT <- nrow(x)
  N <- ncol(x)
  
  # Number of missings - Here the missings are only new information
  n.missings <- colSums(is.na(x))
  m <- max(n.missings)
  
  # Number of arguments inputed
  n.arg <- sum(c(!is.null(x),!is.null(q),!is.null(r),!is.null(p),
                 !is.null(A),!is.null(C),!is.null(Q),!is.null(R),
                 !is.null(initx), !is.null(initV), 
                 !is.null(ss), !is.null(MM)))
  
  if(n.arg < 5){ # Estimate parameters if they are not inputed
    
    z <- x[1:(TT - m),]      # ONLY complete information for PCA
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
    a <- parametros$eigen
    #print(A)
  }else{
    
    # If parameters are inputed only need to standardize
    
    for(i in 1:N){
      x[,i] <- (x[,i] - MM[i])/ss[i]
    }
    z <- x[1:(TT - m),]
    
  }
  
  # Parameters for state space model
  
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
  xx[is.na(x)] <- 0 # Arbitrary value for missing
  
  # KALMAN SMOOTHER DIAG
  
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
  
  x<-fator_final
  fatoresTS <- stats::ts(x[,-1], end = as.numeric(c(substr(x[nrow(x),1],1,4),
                                                    substr(x[nrow(x),1],6,7))), frequency = 12)
  
  
  fator_final
  list(fator_final = fatoresTS,A = A,C = C,Q = Q,R =  R,initx =  initx,initV =  initV,eigen = a)
}
