#' @title Kalman update diagonal
#' @description Kalman update diagonal
#' @param A Matrix that update factors with VAR
#' @param C Matrix that combine factors to explain the transformed data.
#' @param Q Error variance in factor update.
#' @param R Error variance in explain data from factors
#' @param y xxx
#' @param x xxx
#' @param V xxx
#' @param varagin xxx
#' @import matlab

kalman_update_diag <- function(A, C, Q, R, y, x, V, varagin){
  
  # A = A[,,m]
  # C = C[,,m]
  # Q = Q[,,m]
  # R = R[,,m]
  # y = y[,t]
  # x = prevx
  # V = prevV
  # varagin <- list('initial', initial)

  # parâmetros default
  u <- NULL
  B <- NULL
  initial <- 0
  
  args <- varagin
  nargs <- length(args)
  
  for(i in seq(1, nargs, by = nargs-1)){
    if(args[i] == "u"){ u <- args[[i+1]]
    }else if(args[i] == "B"){ B <- args[[i+1]]
    }else if(args[i] == "initial"){ initial <- args[[i+1]]
    }
  }
  
  #  xpred(:) = E[X_t+1 | y(:, 1:t)]
  #  Vpred(:,:) = Cov[X_t+1 | y(:, 1:t)]
  
  if(initial != 0){
    if(is.null(u)){ 
      xpred <- x
    }else{ 
      xpred <- x + B %*% u
    }
    Vpred <- V
  }else{
    if(is.null(u)){
      xpred <- t(A %*% t(x))
    }else{
      xpred <- t(A %*% t(x) + B %*% u)
    }
  Vpred <- A %*% V %*% t(A) + Q
  }
  
  e <- y - C %*% t(xpred) # error (innovation)
  n <- length(e)
  ss <- nrow(A)
  if(is.null(ss)){ ss <- length(A) }
  
  d <- size(e,1)
  
  S <- C %*% Vpred %*% t(C) + R
  GG <- t(C) %*% diag(1/diag(R)) %*% C
  Sinv <- diag(1/diag(R)) - diag(1/diag(R)) %*% C %*% pseudoinverse(eye(ss) + Vpred %*% GG) %*% Vpred %*% t(C) %*% diag(1/diag(R)) # works only with R diagonal
  
  # Sinv = inv(S)
  
  ################################################
  
  detS <- prod(diag(R)) %*% det(eye(ss) + Vpred %*% GG)
  denom <- (2*pi)^(d/2)*sqrt(abs(detS))
  mahal <- rowSums(t(e) %*% Sinv %*% e)
  loglik <- -0.5*mahal - log(denom)
              
  ################################################
              
  K <- Vpred %*% t(C) %*% Sinv # Kalman gain matrix
              
  # If there is no observation vector, set K = zeros(ss).
  xnew <- t(xpred) + (K %*% e)              #csi_est(t\t) formula 13.6. 5    
  Vnew <- (eye(ss) - K %*% C) %*% Vpred    #P(t\t) formula 13.2.16 hamilton
  VVnew <- (eye(ss) - K %*% C) %*% A %*% V
              
  list(xnew = xnew, Vnew = Vnew, VVnew = VVnew, loglik = loglik)          
  
}