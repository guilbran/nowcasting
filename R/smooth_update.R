#' @title Smooth update
#' @description Smoothe update
#' @param xsmooth_future. none
#' @param Vsmooth_future. none
#' @param xfilt. none
#' @param Vfilt. none
#' @param Vfilt_future. none
#' @param VVfilt_future. none
#' @param A. none
#' @param Q. none
#' @param B. none
#' @param u. none
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