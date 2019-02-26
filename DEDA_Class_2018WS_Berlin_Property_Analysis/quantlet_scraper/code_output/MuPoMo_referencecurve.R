referencecurve  = function(theta2, theta3, kt, kt.null) {
    t           = time(kt)
    sm.t        = theta3 * t + theta2  # time adjustment
    t.reference = Sweden$year
    ## common grid for kt and shifted kt.reference
    tmin = max(min(t.reference), min(sm.t))
    tmax = min(max(t.reference), max(sm.t))
    i0   = which(t.reference &gt;= tmin &amp; t.reference &lt;= tmax)
    if (length(i0) &gt; 0) {
        t0  = t.reference[i0]
        d   = data.frame(kt, sm.t)
        # sm: sm.regression with optimal smoothing parameter
        h.optimal6 = h.select(sm.t, kt)
        sm  = sm.regression(sm.t, kt, h = h.optimal6, eval.points = t0, model = "none", poly.index = 1, display = "none")
        mu1 = sm$estimate
        mu  = ts(mu1, start = t0[1], frequency = 1)
    } else {
        mu  = kt.null
    }
    return(mu)
}
