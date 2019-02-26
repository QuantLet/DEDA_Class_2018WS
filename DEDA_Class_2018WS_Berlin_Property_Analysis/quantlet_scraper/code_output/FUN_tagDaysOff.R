## Function: tagDaysOff

## Args:     data  - single time series of energy consumption or production
##                   values for 2017 in 3-minute intervals (175 200 obs.)
##                 
## Returns:  df   - data frame with original time series, 0/1-vector indicating
##                  weekends, and 0/1-vector indicating German holidays in 2017



tagDaysOff &lt;-  function(data){
    
    weekends &lt;- c(rep.int(1, (24*20 - 1)),
                  rep.int(c(rep.int(0, 5*24*20), rep.int(1, 2*24*20)), 52),
                  0)
    
    h0414 &lt;- interval(ymd_hm(201701010000, tz = "CET"),
                      ymd_hm(201704140000, tz = "CET"))/dminutes(3)
    h0417 &lt;- interval(ymd_hm(201701010000, tz = "CET"),
                      ymd_hm(201704170000, tz = "CET"))/dminutes(3)
    h0501 &lt;- interval(ymd_hm(201701010000, tz = "CET"),
                      ymd_hm(201705010000, tz = "CET"))/dminutes(3)
    h0525 &lt;- interval(ymd_hm(201701010000, tz = "CET"),
                      ymd_hm(201705250000, tz = "CET"))/dminutes(3)
    h0605 &lt;- interval(ymd_hm(201701010000, tz = "CET"),
                      ymd_hm(201706050000, tz = "CET"))/dminutes(3)
    h1003 &lt;- interval(ymd_hm(201701010000, tz = "CET"),
                      ymd_hm(201710030000, tz = "CET"))/dminutes(3)
    h1031 &lt;- interval(ymd_hm(201701010000, tz = "CET"),
                      ymd_hm(201710310000, tz = "CET"))/dminutes(3)
    h1225 &lt;- interval(ymd_hm(201701010000, tz = "CET"),
                      ymd_hm(201712250000, tz = "CET"))/dminutes(3)
    h1226 &lt;- interval(ymd_hm(201701010000, tz = "CET"),
                      ymd_hm(201712260000, tz = "CET"))/dminutes(3)
    
    holidays &lt;- c(rep.int(0, h0414-1), rep.int(1, 24*20),
                  rep.int(0, h0417-(h0414 + 24*20)), rep.int(1, 24*20),
                  rep.int(0, h0501-(h0417 + 24*20)), rep.int(1, 24*20),
                  rep.int(0, h0525-(h0501 + 24*20)), rep.int(1, 24*20),
                  rep.int(0, h0605-(h0525 + 24*20)), rep.int(1, 24*20),
                  rep.int(0, h1003-(h0605 + 24*20)), rep.int(1, 24*20),
                  rep.int(0, h1031-(h1003 + 24*20)), rep.int(1, 24*20),
                  rep.int(0, h1225-(h1031 + 24*20)), rep.int(1, 24*20),
                  rep.int(0, h1226-(h1225 + 24*20)), rep.int(1, 24*20),
                  rep.int(0, 5*24*20 + 1))
        
    df &lt;- cbind(data, weekends, holidays)
    
    return(df)
        
}
