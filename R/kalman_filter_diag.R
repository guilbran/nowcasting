#' @title Kalman Filter Diagonal
#' @description Kalman Filter Diagonal
#' @param y xxx
#' @param A Matrix that update factors with VAR
#' @param C Matrix that combine factors to explain the transformed data.
#' @param Q Error variance in factor update.
#' @param R Error variance in explain data from factors
#' @param init_x xxx
#' @param init_V xxx
#' @param varagin xxx
#' @export

kalman_filter_diag <- function(y, A, C, Q, R, init_x, init_V, varagin){
  
  # y = t(xx);
  # A = AA;
  # C = CC;
  # Q = QQ;
  # R = RR;
  # init_x = initx;
  # init_V = initV;
  # varagin = list("model", model, "u", u, "B", B)
   
  os <- nrow(y)
  TT <- ncol(y)
  ss <- dim(A)[1] # size of state space
  
  # parâmetros default
  model <- ones(1,TT)
  u <- NULL
  B <- NULL
  ndx <- NULL
  
  args <- varagin
  nargs <- length(args)
  
  for(i in seq(1, nargs, by = nargs-1)){
    if(args[i] == "model"){ model <- args[[i+1]]
    }else if(args[i] == "u"){ u <- args[[i+1]]
    }else if(args[i] == "B"){ B <- args[[i+1]]
    }else if(args[i] == "ndx"){ ndx <- args[[i+1]]
    }
  }
  
  x <- zeros(ss, TT)
  V <- zeros(ss, ss, TT)
  VV <- zeros(ss, ss, TT)
  
  loglik <- 0
  LL <- NULL
  for(t in 1:TT){
    m <- model[t]
    if(t == 1){
      prevx <- init_x
      prevV <- init_V
      initial <- 1
    }else{
      prevx <- t(matrix(x[,t-1]))
      prevV <- V[,,t-1]
      initial <- 0
    }
  
    if(is.null(u)){
      
      #print(paste0("entrei aqui no primeiro", t))
      resul <- kalman_update_diag(A[,,m], C[,,m], Q[,,m], R[,,m], y[,t], prevx, prevV, list('initial', initial))
      x[,t] <- resul$xnew
      V[,,t] <- resul$Vnew
      LL[t] <- resul$loglik
      VV[,,t] <- resul$VVnew
      
    }else if(is.null(ndx)){
      
      #print(paste0("entrei aqui em null ndx", t))
      resul <- kalman_update_diag(A[,,m], C[,,m], Q[,,m], R[,,m], y[,t], prevx, prevV, list('initial', initial, 'u', u[,t], 'B', B[,,m]))
      x[,t] <- resul$xnew
      V[,,t] <- resul$Vnew
      LL[t] <- resul$loglik
      VV[,,t] <- resul$VVnew
      
    }else{
      
      #print(paste0("entrei aqui em else", t))
      i <- ndx[t];
      # copy over all elements; only some will get updated
      x[,t] <- prevx;
      prevP <- solve(prevV);
      prevPsmall <- prevP[i,i]
      prevVsmall <- solve(prevPsmall)
      
      resul <- kalman_update_diag(A[i,i,m], C[,i,m], Q[i,i,m], R[,,m], y[,t], prevx[i], prevVsmall, list('initial', initial, 'u', u[,t], 'B', B[i,,m]))
      x[i,t] <- resul[[1]]
      smallV <- resul[[5]]
      LL[t] <- resul[[3]]
      VV[i,i,t] <- resul[[4]]
      
      smallP <- solve(smallV)
      prevP[i,i] <- smallP
      V[,,t] <- solve(prevP)
    }

  loglik <- loglik + LL[t]
  }

  list(x = x, V = V, VV = VV, loglik = loglik)
  
  
}