#' @title Principal Components to Dynamic Factor Model
#' @description Estimates the first stage for dynamic factors on \emph{Giannone, Domenico, Lucrezia Reichlin, and David Small. "Nowcasting: The real-time informational content of macroeconomic data." Journal of Monetary Economics 55.4 (2008): 665-676}.
#' @param x. The \code{ts} used to extract the dynamic factors. 
#' @param q. Number of shocks in factors.
#' @param r. Number of factors.
#' @param p. Degree of autoregressive polynomial
#' @import matlab
#' @import corpcor


pcatodfm <- function(x, q, r, p){
  
  # x é a base de dados em formato ts
  # q é o número de choques nos fatores
  # r é a quantidade de fatores
  # p é o grau do polinômio autorregressivo
  # x = x_padronizado
  # r = 2
  # q = 2
  # p = 1
  
  # finalx2 = z
  # x = z
  x <- as.matrix(x)
  Mx <- colMeans(x)
  Wx <- apply(x, MARGIN = 2, FUN = sd)

  for(i in 1:ncol(x)){
    x[,i] <- (x[,i] - Mx[i])/Wx[i]
  }
  
  # tamanho da base
  TT <- nrow(x)
  N <- ncol(x)
  
  # restrição
  if(r < q){ stop("q precisa ser menor ou igual a r") }
  
  # nlag 
  nlag <- p - 1
  
  # A temporária - matriz de zeros
  A_temp <- t(zeros(r,r*p))
  # matriz identidade
  I <- diag(r*p)
  if(p == 1){ 
    A <- A_temp 
  }else{
    A <- rbind(t(A_temp), I[1:(nrow(I) - r), 1:ncol(I)]) 
  }
  
  Q <- zeros(r*p,r*p)
  Q[1:r,1:r] <- diag(r)
  
  # autovalores e autovetores
  a <- eigen(cov(x))
  d <- diag(a$values[1:r])
  v <- a$vectors[,1:r]
  
  # estimativa dos fatores comuns
  EF <- x %*% v
  # estimativa da matriz de covariância do termo de erro na equação dos fatores comuns
  R <- diag(diag(cov(x - x %*% v %*% t(v))))
  
  if(p > 0){
    # estimar o modelo autoregressivo para os fatores VAR: F(t) =  A1*F(t-1) + ... + Ap*F(t-p) + e(t)
    z <- EF
    Z <- NULL
    for(kk in 1:p){
      Z <- cbind(Z, z[(p - kk + 1):(nrow(z) - kk),])
    }
    z <- z[(p + 1):nrow(z),]
    
    # Estimador OLS para a matriz de transição do VAR
    A_temp <- solve(t(Z) %*% Z) %*% t(Z) %*% z
    A[1:r,1:(r*p)] <- t(A_temp)
    
    # Q
    e <- z - Z%*%A_temp # VAR residuals
    H <- cov(e) # VAR covariance matrix
    
    if(r == q){ # The covariance matrix of the VAR residuals is of full rank
      
      Q[1:r,1:r] <- H
      
    }else{ #The covariance matrix of the VAR residuals has reduced rank
      
      a <- eigen(H)
      P <- a$vectors[,1:q]
      
      if(is.matrix(P)){
        M <- diag(a$values[1:q])
        P <- P %*% diag(sign(P[1,]))
        Q[1:r,1:r] <- P %*% M %*% t(P)
      }else{
        M <- a$values[1:q]
        P <- P * sign(P[1])
        Q[1:r,1:r] <- P * M * t(P)
      }

      u_orth <- e %*% P %*% (M^(-.5)) # extracting the common shocks
      e_pc <- e %*% P %*% t(P)
      #Q[1:r,1:r] <- P %*% M %*% t(P)
      
    }
  }
  
  # Condições iniciais pro filtro de kalman
  
  if(p > 0){
    
    z <- EF
    Z <- NULL
    
    for(kk in 0:nlag){
      Z <- cbind(Z, z[(nlag - kk + 1):(nrow(z) - kk),]) # stacked regressors (lagged SPC)
    }
    
    initx <- t(Z[1,]) 
    initV <- matlab::reshape(pseudoinverse(eye(size(kronecker(A,A),1))- kronecker(A,A)) %*% matrix(Q, ncol = 1), r*p, r*p)
    
  }else{
    
    initx <- NA
    initV <- NA
    
  }
  
  if(nlag != 0){
    C <- cbind(v, zeros(N,r*(nlag)))
  }else{
    C <- as.matrix(v)
  }
  
  list(A = A, C = C, Q = Q, R = R, initx = initx, initV = initV)
}

