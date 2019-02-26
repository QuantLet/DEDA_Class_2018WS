#Close windows and clear variables                                                                   
graphics.off()
rm(list = ls(all=TRUE))
#Read the datafile and calculate the average summary statistics of moving window betas

summ               = matrix(0,253,7)
colnames(summ)     = c( "No. of Coefficients", "No. of Nonzeros", "No. of Negatives", "No. of Positives", "Max", "Min", "Average")
for (s in 1:253){
  # beta_L_lowertail should be replaced by beta_L_median and beta_L_uppertail when you do calculations in different situations
  datafile         = paste("dynamiclassocoeff/beta_L_lowertail ", s, " .csv", sep ="")
  conn             = read.csv(datafile, header = TRUE, sep = ",", dec = ".")
  industrynames    = read.csv("industrynames.csv")
  conn             = conn[ ,-1]
  rownames(conn)   = industrynames[ ,2]
  colnames(conn)   = industrynames[ ,2]
  data             = data.matrix(conn)
  diag(data)       = 0
  maxvalue         = max(data)
  minvalue         = min(data)
  summ[s,1]        = sum(data!=0) + sum(data==0)
  summ[s,2]        = sum(data != 0)
  summ[s,3]        = sum(data &lt; 0)
  summ[s,4]        = sum(data &gt; 0)
  summ[s,5]        = maxvalue
  summ[s,6]        = minvalue
  summ[s,7]        = mean(data)
}
  dire             = colMeans(summ)
  write.table(dire, file = "directions_lowertail.txt", sep = "&amp;")
