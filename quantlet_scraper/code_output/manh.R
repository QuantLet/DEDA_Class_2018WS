manh = function(x){
# Manhattan distence between time series normalised by the time series
# length.
# x - time series
#
x      = as.matrix(x)
h      = nrow(x)
k      = ncol(x)
result = matrix(0,k,k)
for (j in 1:k){
  for (i in 1:k){
	result[i,j] = abs(mean(x[,i]-x[,j]))
  }
}
retval= result
}
