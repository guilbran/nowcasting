#' @title Smooth update
#' @description Smooth update
#' @param xsmooth_future. xxx
#' @param Vsmooth_future. xxx
#' @param xfilt. xxx
#' @param Vfilt. xxx
#' @param Vfilt_future. xxx
#' @param VVfilt_future. xxx
#' @param A. Matrix that update factors with VAR
#' @param Q. Error variance in factor update.
#' @param B. xxx
#' @param u. xxx
#' @import matlab


smooth_update <- function(xsmooth_future, Vsmooth_future, xfilt, Vfilt,  Vfilt_future, VVfilt_future, A, Q, B, u){
  
  # xsmooth_future = xsmooth[,t+1]
  # Vsmooth_future = Vsmooth[,,t+1]
  # xfilt = xfilt[,t]
  # Vfilt_future = Vfilt[,,t+1]
  # Vfilt = Vfilt[,,t]
  # 
  # VVfilt_future = VVfilt[,,t+1]
  # A = A[,,m]
  # Q = Q[,,m]
  # B = NULL
  # u = NULL
  
  
  if(is.null(B)){
    xpred <- A %*% xfilt
  }else{
    xpred <- A %*% xfilt + B %*% u
  }
  
  Vpred <- A %*% Vfilt %*% t(A) + Q # Vpred = Cov[X(t+1) | t]
  J <- Vfilt %*% t(A) %*% pseudoinverse(Vpred) # smoother gain matrix
  xsmooth <- xfilt + J %*% (xsmooth_future - xpred)
  Vsmooth <- Vfilt + J %*% (Vsmooth_future - Vpred) %*% t(J)
  
  if(is.matrix(Vfilt_future)){
    VVsmooth_future <- VVfilt_future + (Vsmooth_future - Vfilt_future) %*% pseudoinverse(Vfilt_future) %*% VVfilt_future
  }else{
    VVsmooth_future <- VVfilt_future + (Vsmooth_future - Vfilt_future) %*% 1/Vfilt_future %*% VVfilt_future
    
  }
  list(xsmooth = xsmooth, Vsmooth = Vsmooth, VVsmooth_future = VVsmooth_future)
}