# ---------------------------------------------------------------------
# Paper:       	P. Burdejova and W.K. Härdle
#			"Dynamics of Tail Event Curves"
# ---------------------------------------------------------------------
# Quantlet:    	DYTEC_temperature
# ---------------------------------------------------------------------
# Description: 	Do the simulation for DYTEC algorithm
# ---------------------------------------------------------------------
# Author:      	Petra Burdejova
# ---------------------------------------------------------------------

library(expectreg)
library(fda)
library(matrixcalc)
library(gglasso)
library(doParallel)
#library(parallel)
library(foreach)
library(fBasics)

install.packages("vars")
library("vars")


wgt_fun &lt;- function(tau,basis,mtx_hat,y_expl){	
  wgt_vec &lt;- y_expl &lt; (basis %*% mtx_hat)	
  wgt_vec &lt;- abs(wgt_vec - tau)
  return(wgt_vec)
}

my_tau=0.5
tau=0.5

#stat=1

for (stat in 1:1){
  data0 = t(temp_list_stations[[stat]])
  data0= data0 #[1:20,30:150]
  data0_exp = t(exp_list_stations[[stat]])
  data0_exp= data0_exp #[1:20,30:150]
  fcast_dytec = dytec_fcast(data0) 
  fcast_var = var_fcast(data0_exp)
} # end of for cycle stat



# 
# plot(data0[53,])
# lines(data0_exp[53,], col="red")
# lines(fcast_dytec)
# lines(fcast_var)
#lines(fcast_dytec_sm)
lines(unlist(fcast_dytec_sm$values), col="blue")

cl&lt;-makeCluster(2)
registerDoParallel(cl)

sim_output &lt;- 
  foreach (stat=c(1:159), .packages=c('fda','gglasso','expectreg','matrixcalc','vars')) %dopar% {
    data0 = t(temp_list_stations[[stat]])
    data0= data0 #[1:20,30:150]
    data0_exp = t(exp_list_stations[[stat]])
    data0_exp= data0_exp #[1:20,30:150]
    fcast_dytec = dytec_fcast(data0) 
    fcast_dytec_sm &lt;- expectreg.ls(fcast_dytec~rb(c(1:365),type="pspline"), smooth="aic",expectile=0.5)
    fcast_dytec_sm_disc &lt;- unlist(fcast_dytec_sm$values)
    fcast_var = var_fcast(data0_exp)
    compare_fcast(fcast_dytec_sm_disc,fcast_var,data0_exp[53,])
  }
stopCluster(cl)


compare_fcast &lt;- function(dytec_fun, var_fun, exp_fun){
  p=length(exp_fun)
  mse_dytec &lt;- sum(abs(exp_fun-dytec_fun)^2)/p  
  mse_var &lt;- sum(abs(exp_fun-var_fun)^2)/p
  return(list(mse_dytec,mse_var) )
}

# mse_var &lt;- sum(abs(data0_exp[53,]-fcast_var)^2)/365
# mse_dytec &lt;- sum(abs(data0_exp[53,]-fcast_dytec)^2)/365
# mse_var
# mse_dytec


var_fcast &lt;- function(data_exp){
  d1=dim(data_exp)[1]
  d2=dim(data_exp)[2] 
  pca &lt;- prcomp(data_exp[1:(d1-1),])
  components&lt;- pca$rotation[,1:3]
  #dim(components)
  projection &lt;- pca$x[,1:3]
  #dim(projection)
  mean &lt;- apply(data_exp[1:(d1-1),], 2,mean)

    # var na y tj. projection
    scores_var &lt;- VAR(projection, p = 3)
    scores_f &lt;-(predict(scores_var,n.ahead=1, ci=0))
    # forecast(a_pred, h=1)$mean }
    LOC&lt;-c("PC1","PC2","PC3")
    scores_fcast &lt;- sapply(scores_f $fcst[LOC], function (k) k[ , 1])
    e_fcast_var &lt;- components%*% scores_fcast
    return(e_fcast_var+mean)
} # end of var_fcast function




dytec_fcast &lt;- function(data){
  d1=dim(data)[1]
  d2=dim(data)[2] 
  nbas_time=round(d1+1)
  nbas_space=round(d2/2)
  
  #--- create time basis
  time_basis &lt;- dytec_create_time_basis(d1,nbas_time)
  time_basis_matrix &lt;- time_basis[[1]]
  # if pca used, then nbas_space could change
  nbas_time &lt;- time_basis[[2]]
  
  space_basis &lt;- dytec_create_space_basis(data,opt=1,nbas_space)
  space_basis_matrix &lt;- space_basis[[1]]
  # if pca used, then nbas_space could change
  nbas_space &lt;- space_basis[[2]]
  space_basis_matrix &lt;- t(space_basis_matrix)
  
  # delete last year for modelling
  x_vec &lt;- kronecker(t(space_basis_matrix), time_basis_matrix[1:d1-1,]) # delete last year for modelling
  #dim(x_vec)   # 200 x 1400
  data_vec &lt;- vec(data[1:d1-1,])   # delete last year for modelling
  #dim(data_vec)
  
  #--- minimize for mean
  m1 &lt;- gglasso(x=x_vec, y=data_vec, intercept=FALSE)
  #dim(m1$beta)
  beta_hat_mtx &lt;- matrix(m1$beta[,100],nrow=nbas_time, ncol=nbas_space)
  
  J=dim(x_vec)[2]
  #my_tau=0.8
  wgt_old&lt;- wgt_fun(my_tau,x_vec,m1$beta[,100], data_vec)
  w_iter_k &lt;- 0
  min_change &lt;- length(wgt_old)
  repeat {
    w_iter_k= w_iter_k+1
    print(paste("tau=", my_tau ,"iter:",w_iter_k))
    wgt_x_vec &lt;- x_vec
    for ( j in 1:J)  {wgt_x_vec[,j] &lt;- wgt_old * x_vec[,j] }
    mk &lt;- gglasso(x= wgt_x_vec , y= wgt_old * data_vec, intercept=FALSE)
    beta_k &lt;- mk$beta[,100]
    wgt_new &lt;- wgt_fun(my_tau, x_vec, beta_k, data_vec )
    print(paste("diff.w.:",sum(wgt_old != wgt_new)))
    # avoid cycling
    if (sum(wgt_old != wgt_new)&gt;=min_change){break}
    if (sum(wgt_old != wgt_new)&lt;min_change) {min_change &lt;- sum(wgt_old != wgt_new)}
    wgt_old &lt;- wgt_new
  } # end of repeat
  beta_hat_mtx_k &lt;- matrix(beta_k,nrow=nbas_time, ncol=nbas_space)
  y_fit &lt;-  (time_basis_matrix%*% beta_hat_mtx_k %*% space_basis_matrix)
  return(y_fit[d1,]) #return just forecast (made with d1-1 curves)
} # end of dytec fcast
