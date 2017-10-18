# %%%  Replication files for:
# %%%  ""Nowcasting", 2010, (by Marta Banbura, Domenico Giannone and Lucrezia Reichlin), 
# %%% in Michael P. Clements and David F. Hendry, editors, Oxford Handbook on Economic Forecasting.
# %%%
# %%% The software can be freely used in applications. 
# %%% Users are kindly requested to add acknowledgements to published work and 
# %%% to cite the above reference in any resulting publications

para_const <- function(X = NULL, P = NULL, lag = NULL){
  
  Z_0 <- P$Z_0
  V_0 <- P$V_0
  A <- P$A
  C <- P$C
  Q <- P$Q
  R <- P$R
  Mx <- P$Mx
  Wx <- P$Wx
  
  # %--------------------------------------------------------------------------
  # % Preparation of the data
  # %--------------------------------------------------------------------------
  TT <- nrow(X)
  N <- ncol(X)
  
  #% Standardise x
  xNaN <- (X-repmat(Mx,TT,1))/repmat(Wx,TT,1)
  
  y <- t(xNaN)
  
  #%final run of the Kalman filter
  out <- runKF_lag(y, A, C, Q, R, Z_0, V_0, lag)
  Zsmooth <- out$Zsmooth
  P <- out$P
  
  Zsmooth <- t(Zsmooth)
  x_sm <- Zsmooth[2:nrow(Zsmooth),] %*% t(C)
  X_sm <- repmat(Wx,TT,1) %*% x_sm + repmat(Mx,TT,1)
  
  # %--------------------------------------------------------------------------
  # %   Loading the structure with the results
  # %--------------------------------------------------------------------------
  
  Res$P <- P
  Res$X_sm <- X_sm
  
  # output
  return(list(Res = Res))
}