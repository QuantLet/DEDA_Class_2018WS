AHT2 &lt;- function(AT){
  
  AT[,1] &lt;- as.character(AT[,1])
  ATS &lt;- as.data.frame(numeric(nrow(AT)))
  
  for(i in 1:nrow(AT)){
    for(n in 1:100){
      if(substring(AT[i,],n,n)[1] == "{")
        l &lt;- n+1
      if(substring(AT[i,],n,n)[1] == "}"){
        ATS[i,1] &lt;- substring(AT[i,],l,n-1)[1]
        m &lt;- n+1
        break
      }
    }
    for(n in m:100){
      if(substring(AT[i,],n,n)[1] == "{")
        r &lt;- n+1
      if(substring(AT[i,],n,n)[1] == "}"){
        ATS[i,2] &lt;- substring(AT[i,],r,n-1)[1]
        s &lt;- n+1
        break
      }
    }
  }
  
  ATS[,3] &lt;- AT[,2]
  ATS[,4] &lt;- AT[,3]
  ATS[,5] &lt;- AT[,4]
  
  colnames(ATS) &lt;- c("lhs","rhs","supp","conf","lift")
  
  return(ATS)
}
