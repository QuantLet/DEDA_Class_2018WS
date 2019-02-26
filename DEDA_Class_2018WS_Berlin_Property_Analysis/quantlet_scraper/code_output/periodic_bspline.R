library(splines)
##periodic-spline:
get.pbas&lt;- function( n , S=24, dK = S/4, ord=4){
	## ord=4 --&gt; cubic splines
	## dK = equidistance distance
	## S= season
	## support will be 1:n
	stp&lt;- dK
	x&lt;- 1:S ## must be sorted!
	lb&lt;- x[1]
	ub&lt;- x[length(x)]
	knots&lt;- seq(lb-(0)*stp,ub+(1)*stp, by=stp)
	derivs&lt;- numeric(length(x))
	## some stuff adjusted from pbc-package to be faster
	degree&lt;- ord-1
	nKnots = length(knots)
	Aknots = c( knots[1] - knots[nKnots] + knots[nKnots - degree:1] , knots,  knots[nKnots] + knots[1:degree + 1] - knots[1] )
	basisInterior &lt;- splineDesign(Aknots, x, ord, derivs) 
	basisInteriorLeft &lt;- basisInterior[, 1:(ord-1), drop = FALSE]
	basisInteriorRight &lt;- basisInterior[, (ncol(basisInterior) - ord+2):ncol(basisInterior), drop = FALSE]
	basis &lt;- cbind(basisInterior[, -c(1:(ord-1), (ncol(basisInterior) - ord+2):ncol(basisInterior)), drop = FALSE], basisInteriorLeft + basisInteriorRight)
	t(array(t(basis), dim= c(dim(basis)[2] ,n)))
}

# ## EXAMPLE hourly data
# S&lt;- 24
# A&lt;- 365.24
# n.total&lt;- 3*S*A
# BAS.daily&lt;- get.pbas(n.total, S=S, dK= S/8 ) 
# BAS.annual&lt;- get.pbas(n.total, S=S*A, dK= S*A/6 )  
# 
# ## example plot...
# ts.plot(BAS.annual, col=rainbow(dim(BAS.annual)[2]))
