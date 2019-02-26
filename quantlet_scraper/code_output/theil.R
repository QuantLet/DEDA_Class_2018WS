theil = function(x,n){
# Converts the given time series into time series of Theil index
# n - size of moving window
# x - analysed time series
#
x         = as.matrix(x)
n_pocz    = nrow(x)
k         = ncol(x)

rozm      = n_pocz-n+1

retval    = matrix(0,rozm,k)
    for (i in 1:rozm){
    	sr          = apply(x[i:(i+n-1),],2,mean)
    	temp        = x[i:(i+n-1),]/(matrix(1,n,1)%*%sr)
    	temp        = temp*log(temp)
    	retval[i,]  = apply(temp,2,mean)
    }
}
