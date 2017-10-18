# %%%  Replication files for:
# %%%  ""Nowcasting", 2010, (by Marta Banbura, Domenico Giannone and Lucrezia Reichlin), 
# %%% in Michael P. Clements and David F. Hendry, editors, Oxford Handbook on Economic Forecasting.
# %%%
# %%% The software can be freely used in applications. 
# %%% Users are kindly requested to add acknowledgements to published work and 
# %%% to cite the above reference in any resulting publications
# %--------------------------------------------------------------------------
# % KALMAN FILTER
# %--------------------------------------------------------------------------

runKF_lag <- function(y = NULL, A = NULL, C = NULL, Q = NULL, R = NULL, x_0 = NULL, Sig_0 = NULL, k = NULL){
  
  n <- nrow(C)
  r <- ncol(C)
  
  if(k > 0){
    C <- cbind(C, zeros(n,k*r))
    A <- bdiag(A,zeros(k*r))
    A[(r+1):nrow(A), 1:(k*r)] <- eye(k*r)
    Q <- bdiag(Q,zeros(k*r))
    x <- zeros(k*r,1)
    colnames(x) <- colnames(x_0)
    x_0 <- rbind(x_0,x)
    Sig_0 <- bdiag(Sig_0,zeros(k*r,k*r))
  }
  
  S <- SKF_lag(y,C,R,A,Q, x_0, Sig_0)
  S <- FIS(y,C,R,A,Q,S)
  
  xsmooth <- S$AmT[1:r,]
  Vsmooth <- S$PmT
  
  # output
  return(list(xsmooth = xsmooth, Vsmooth = Vsmooth))
  
}