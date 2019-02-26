# ---------------------------------------------------------------------
# Paper:       	P. Burdejova and W.K. Härdle
#			"Dynamics of Tail Event Curves"
# ---------------------------------------------------------------------
# Quantlet:    	DYTEC_hurricanes
# ---------------------------------------------------------------------
# Description: 	Apply DYTEC algorithm for hurricanes data
# ---------------------------------------------------------------------
# Author:      	Petra Burdejova
# ---------------------------------------------------------------------


dim(year_data_nex_mtx)
data = year_data_nex_mtx[16:65,801:1200]

  d1=dim(data)[1]
  d2=dim(data)[2] 
  nbas_time=round((d1+1)/5)
  nbas_space=round(d2/7)
  
  #--- create time basis
  time_basis &lt;- dytec_create_time_basis(d1,nbas_time)
  time_basis_matrix &lt;- as.matrix(time_basis[[1]])
  dim(time_basis_matrix)
  matplot(time_basis_matrix, type="l")
  nbas_time &lt;- time_basis[[2]]  
  
  space_basis &lt;- dytec_create_space_basis(data,opt=1,nbas_space)
  space_basis_matrix &lt;- space_basis[[1]]
  # if pca used, then nbas_space could change
  nbas_space &lt;- space_basis[[2]]
  space_basis_matrix &lt;- t(space_basis_matrix)
  dim(space_basis_matrix)
  
  #--- create space basis
  l &lt;- prcomp(data)$sdev
  cumsum( l*l/ sum(l*l))
  
  pcs &lt;- prcomp(data)$rotation[,1:20]
  space_basis_matrix &lt;- pcs
  space_basis_matrix &lt;- t(space_basis_matrix)
  nbas_space=20
  
  # delete last year for modelling
  x_vec &lt;- kronecker(t(space_basis_matrix), time_basis_matrix[1:d1-1,]) # delete last year for modelling
  dim(x_vec)   # 200 x 1400
  data_vec &lt;- vec(data_used[1:d1-1,])   # delete last year for modelling
  dim(data_vec)
  
  #--- minimize for mean
  m1 &lt;- gglasso(x=x_vec, y=data_vec, intercept=FALSE)
  #dim(m1$beta)
  beta_hat_mtx &lt;- matrix(m1$beta[,100],nrow=nbas_time, ncol=nbas_space)
 
 wgt_fun &lt;- function(tau,basis,mtx_hat,y_expl){	
  wgt_vec &lt;- y_expl &lt; (basis %*% mtx_hat)	
  wgt_vec &lt;- abs(wgt_vec - tau)
  return(wgt_vec)
}
  
  J=dim(x_vec)[2]
  my_tau=0.8
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

  myImagePlot(beta_hat_mtx) 
  myImagePlot(beta_hat_mtx[12:14,])
  myImagePlot(beta_hat_mtx_k[12:14,])
