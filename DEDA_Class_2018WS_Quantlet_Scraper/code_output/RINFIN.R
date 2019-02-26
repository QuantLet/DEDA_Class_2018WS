# FUNCTIONS FOR SIMULATIONS-TABLES 1 AND  2
# Note to the reader:   For Tables 1, 2,  the function # 1 creates each time a column for the data from F (Normal), with ???mesos??? (i.e. mean)=0,   ???s??? the parameter giving variance-covariance matrix,  dimension d (via J in the function as explained below), d=10,30,50,70, 90 when the sample size n=100, the number of simulations N=100.  The contaminated components follow also a normal distribution with contaminated mean \mu denoted in the function ???mesoscont??? and Covariance matrix the identity. The parameter p is the proportion of contaminated first  d*p (integer) coordinates.  When p=1, there is full contamination.
# DURATION TO OBTAIN RESULTS FOR EACH COLUMN OF THE TABLE WITH d=10,30,50,70, 90 in R-STUDIO: 10 hours; for d&gt;90 it takes much longer.
# The function # 1) is used; the other functions are in the background, i.e. inside function 1, then inside 2 etc. 
# Parameters: n=sample size, N=number of simulations, d=data???s dimension, s=parameter giving variance-covariance matrix, mesos=mean of data,  mesoscont=contaminated mean, parameter J helps creating the dimension of the data noted d (unlike p in thee paper), =5 gives dimension d=10*(2*i-1), i=1-J,to obtain results for d=10,30,50,70,90; p is the proportion of contaminated first  d*p (integer) coordinates.   
# 
# THE FUNCTIONS
# In function # 1 you can see that the functions in it are identified  ??? # 2 FUNCTION, # 3 FUNCTION etc???.
# 
# 1)
rinsq11EX2.1ALLd (J=5,n=100,s=.5,  mesos=0,mesoscont=.5,N=100, p=1) 
{
  #p=proportion of the same contaminated coordinates
  results&lt;-matrix(rep(0,9*J), ncol=9)
  print(results)
  for (j in 1:J)
  {
    #2 FUNCTION
    results[j,]&lt;-        rinsq11EX2.1(n,d=10*(2*j-1),s, mesos, mesoscont, N,p) 
  } 
  cat("RESULTS","\n")
  cat("PERCENTAGE OF CONTAMINATED COORDINATES Gamma=",p, "\n")
  cat("avmisclasrinf,Contaminated Coord, d, s, mesos, mesoscont, N, n, .1*n", "\n")
  print(results)
}

#2)
rinsq11EX2.1 (n=100,d=20,s=.5,  mesos=0,mesoscont=1,N=100, p=.2) 
{
  
  { library("MASS")   
    # misproprinfstar&lt;-rep(0,N)
    misproprinf&lt;-rep(0,N)
    for (j in 1:N)
    {
      # 3 FUNCTION 
      mis&lt;-rinsq11EX2.2gendatapartcont(n,d,s,mesos,mesoscont,p)
      cat("J=",j,"OUT OF",N,"ITERATIONS", "MISSCLASS=", mis,"\n")
      # NOT NEEDED IN VIEW OF AVMISCLASRINF misproprinfstar[j]&lt;-mis[1]
      misproprinf[j]&lt;-mis
    }
    avmisclasrinf&lt;-mean(misproprinf)  
    
    cat("AVERAGE MISCLAS PROP RINF=",avmisclasrinf,"\n")
    cat("PARAMETERS: n=",n, "d=",d,"MEAN F=", mesos,"MEAN G=", 
        mesoscont,"SD=",s, "NUMBER OF SIMULATIONS=",N, "CONTAMINATED
        OBSERVATIONS IN THE BEGINNING OF LIST 
        ARE TEN PERCENT=", .1*n,"\n")
  }
  cat("avmisclasrinf, contaminated coordinates,d, s, mesos, mesoscont, N, n, .1*n", "\n")
  c(avmisclasrinf,d*p,d,s, mesos, mesoscont,N,n, .1*n)
  }

#3)
rinsq11EX2.2gendatapartcont (n=50,d=10,s=.5,mesos=0,mesoscont=50, p=.1) 
{
  
  {
    #################################################ADDITIONS
    bsmall&lt;-c(1.5,.5,0,1,0,0,1.5,0,0,0,1) 
    if  (d&gt;=11) 
    {b&lt;-c(1.5,.5,0,1,0,0,1.5,0,0,0,1,rep(0,d-11))}
    else {b&lt;-bsmall[1:d]}
    ##################################################
    # 4 FUNCTION 
    Sigma&lt;-gensigma(d,s)
    x&lt;-mvrnorm(.9*n, Sigma, mu=rep(mesos,d))
    e1&lt;-rnorm(.9*n,mean=0, sd=1)
    cat("OBSERVATIONS OBTAINED","\n")
    #print(x)
    cat("ERRORS OBTAINED","\n")
    #print(e1)
    y&lt;-x%*%b+e1
    #xcont=xcontaminated
    xcontvoitheia&lt;-mvrnorm(.1*n,Sigma=diag(1,nrow=d),mu=rep(mesoscont,d))
    cat("CONTAMINATED OBSERVATIONS OBTAINED 10 PERCENT","\n")
    #y&lt;-x%*%b+e1
    xvoitheia&lt;-mvrnorm(.1*n, Sigma, mu=rep(mesos,d))
    e2&lt;-rnorm(.1*n,mean=0, sd=1)
    D&lt;-p*d
    if (D
<d) "\n")="" "for"="" "rinfin="" #="" ###########meat##############################################="" #####maybe="" ###the="" #4)="" #5)="" #6)="" #a<-matrix(a,ncol="d)" #and="" #below="" #calculating="" #cat("case")="" #cat("derivatives="" #cat("estimatesofe",="" #cat("highest="" #cat("influence="" #cat("influenceithcoordinateofx[k]")="" #cat("rinfin*="" #cat("sigma="" #cat("sigma",="" #cat("success="" #cat("these="" #cat("var-cov="" #cat("which="" #cat("x="" #checked="" #colorder<-c(1:nrows)="" #derivatives="" #doing="" #inf<-inverse%*%r*c(1,x[])="" #inf<-rep(0,d+1)="" #influence="" #matrix="" #mis<-simrinfgen(yall,xall)="" #misclassproprinfstar<-1-proprinfstar="" #o2mean<-order(rinfin)="" #orinf<-order(absindexrinf)="" #print(a)="" #print(cbind(1:n,="" #print(cbind(colorder[orinf],="" #print(derivativesofinfluencefunctions)="" #print(estimatesofe)="" #print(highestrinfstar)="" #print(influencefunctionsmanual)="" #print(rinfin[i])="" #print(sigma)="" #print(which(highestrinfstar="" #print(xall)="" #print(yall)="" #residual*sum="" #starting="" #starts="" #taking="" #the="" (.1*n)="" (1+multiples="" (a)="" (d+1)="" (d,s)="" (i="" (i+1):d)="" (in="" (inverse[j,]*xvoith)="" (j="" (m="" (mcont-m)="" (mesoscont-mesos)="" (y,x)="" )="" ),="" +residual*inverse[j,m+1]="" -="" .1*n","\n")="" 1="" 1,="" 10%="" 100="" 100).="" 1:(d+1))="" 1:(d-1))="" 1:d)="" 1:n)="" 1st="" 2="" 3="" 4="" 5="" 6="" <=".1*n))" <-sum((influenceithcoordinateofx)^2="" \n"="" \n")="" \n",="" a="" a)="" adjusted="" all="" and="" are="" array="" as="" avoitheiax<-x[-i,]="" avoitheiay<-y[-i]="" b="" b",="" b)),="" b<-="" background,="" be="" because="" below#######################################="" blocks="" by="" calculating="" can="" case="" cases="" cat("#="" cat("\n",="" cat("a",="" cat("all="" cat("b=", b, " cat("contaminated="" cat("d=",d, " cat("derivatives="" cat("estimatesofe",="" cat("estimatesofe2",="" cat("estimatesofe[i,j]=",
      #        ESTIMATESOFE[i,j], " cat("highest="" cat("influence="" cat("influenceithcoordinateofx[",="" cat("inverse="" cat("means="" cat("misclass="" cat("number="" cat("residual=", residual, " cat("rinfin="" cat("rinfini")="" cat("success="" cat("take="" cat("which="" closes="" code="" coefficient="" colorder<-c(1:n)="" column="" columns="" component<-rep(0,d+1)="" confused="" contaminated="" contaminated")="" covariates="" creates="" d="regression" d<-ncol(a)="" d<-ncol(x)="" data="" derivatives="" derivativesofinfluencefunctions<-matrix(rep(0,d*(d+1)),ncol="d+1)" e="" each="" else="" estimate="" estimatesofe1",="" estimatesofe2<-cbind(c(1,means),estimatesofe1)="" estimatesofe<-matrix(rep(0,d^2),="" estimatesofe[i,j]<-rin10prod(u,v)="" estimatesofe[j,i]<-estimatesofe[i,j]="" etc.="" find="" first="" for="" for(i="" for(j="" for(k="" from="" function="" functions="" functions")="" functions",="" gensigma="" gives="" has="" helps="" here="" highestrinf<-colorder[orinf][-c(1:(.9*n))]="" highestrinfstar<-colorder[orinf][-c(1:(.9*n))]="" i=", i, " i-th="" i.e.="" i:d)="" in="" indeed="" influence="" influence",="" influencefunctions<-inverse%*%residualtimesvector="" influencefunctions<-rep(0,d+1)="" influencefunctionsmanual<-rep(0,d+1)="" influencefunctionsmanual[j]<-="" influenceithcoordinateofx<-rep(0,d)="" influenceithcoordinateofx[k]<-influencefunctions[k+1]+="" inside="" inverse="" inverse<-rin10prodmatrix(avoitheiax)="" inverse<-solve(estimatesofe2)="" is="" it="" its="" j=", j, " k,"]",="" largest.="" last="" least="" lengthrinf<-length(which(highestrinf="" lengthrinfstar<-length(which(highestrinfstar="" line=", means)
    ESTIMATESOFE1&lt;-rbind(means,ESTIMATESOFE)
    cat(" loop="" manual="" matrix="" matrix",="" may="" means<-rep(0,d)="" meat="" micro="" micro-array="" mis="" mis<-misclassproprinf="" mis<-rinsq11ex1.3(yall,xall)="" misclassproprinf<-1-proprinf="" n="" n",="" n)="" n,="" n<-length(y)="" n<-nrow(a)="" n<-nrow(x)="" not="" note="" nrow="d)" nrows<-n="" number="" observations","\n")="" obtained="" of="" on="" one="" only")="" ordered="" orinf<-order(rinfin)="" other="" out="" out,="" partial="" partially="" percent="" print(a)="" print(cbind(colorder[orinf],="" print(colorder[orinf][-c(1:(.9*n))])="" print(derivativesofinfluencefunctions)="" print(estimatesofe)="" print(estimatesofe1)="" print(estimatesofe2)="" print(highestrinf)="" print(influencefunctions)="" print(influenceithcoordinateofx[k])="" print(inverse)="" print(rinfin[i])="" print(which(highestrinf="" properly="" proportion="" proprinf<-lengthrinf="" proprinfstar<-lengthrinfstar="" put="" reader:="" regression="" residual="" residual*component[j]="" residual<-y[i]-sum(b*c(1,x[i,]))="" residualtimesvector<-residual*c(1,x[i,])="" respect="" responses.="" results="" results$coefficients="" results.="" results:="" results<-lm(avoitheiay="" rin10prodmatrix="" rinf=",misclassproprinf," rinf*=",proprinfstar," rinfin="" rinfin","\n")="" rinfin))="" rinfin*","\n")="" rinfin<-rep(0,n)="" rinfin[i]="" rinfin[orinf]))="" rinsq11ex1.3="" s="," s,"10="" safe="" scores",="" set="" sets="" sets.="" side="" sigma="" sigma[i,j]<-s^{j-i}="" sigma[j,i]<-s^{j-i}="" size="" smaller="" smallest="" squares,="" starts="" sum(derivativesofinfluencefunctions[k,]*xvoith)="" tables="" than="" that="" the="" then="" there="" times="" to="" together="" total="" use="" used="" v<-a[,j]="" values="" values",="" way="" with="" xall<-rbind(xcont,x)="" xcont="" xvoith<-c(1,x[i,])="" xvoitheia[,(d+1):d])}="" y-column.="" y-vector="" yall<-c(ycont,y)="" ycont<-xcont%*%b+e2="" you="" {="" {component[j]<-sum="" {derivativesofinfluencefunctions[m,j]<--b[m+1]*component[j]="" {estimatesofe[i,i]<-rin10prodequalvector(a[,i])="" {means[i]<-mean(a[,i])="" {sigma<-matrix(rep(0,d^2),nrow="d)" {u<-a[,i]="" {xcont<-cbind(xcontvoitheia[,1:d],="" {xcont<-xcontvoitheia[,1:d]}="" }="" ~avoitheiax)="">
 100. The block size ???100??? can be changed.
# 
# DURATION TO OBTAIN THE RESULTS FOR THIS DATA SET IN R-STUDIO: 3 days, if I recall correctly for the results of Table 3. Similar duration for Table 4.
# 
# A) FUNCTIONS FOR TABLE 3-THE RINFIN RESULTS

# 1)
rinsq11EX3.1  (D) 
{    
  
  # This gives 
  lastcol&lt;-ncol(D)
  cat("LAST COL =", lastcol, "\n")
  print(lastcol)
  n&lt;-nrow(D)
  print(n)
  cat("NUMBER OF ROWS =", n, "\n")
  rinftotscore&lt;-rep(0,n)
  y&lt;-D[,lastcol]
  cat("LAST COLUMN=",lastcol,"\n")
  cat("NUMBER OF ROWS=",n, "\n")
  ###THE LINES BEFORE COULD BE ADJUSTED FOR BLOCK OF COLUMNS
  ####DIFFERENT THAN 100 TO CALCULATE SEVERAL RINFIN VALUES
  REPET&lt;-(lastcol-1)/100
  SCORES&lt;-rep(0,n)
  for (j in 1:REPET)
  {
    L&lt;-1+100*(j-1)
    U&lt;-100*j
    # FUNCTION 2
    x&lt;-rinsq11EX3.2(y, D[,L:U])
    SCORES&lt;-SCORES+x
    
  }
  cat("WHERE ARE THE SCORES?", "\n")
  print(cbind(1:n,SCORES))
  
  colorder&lt;-c(1:n)
  orinf&lt;-order(SCORES)
  cat("THESE ARE THE ORDERED SCORES FOR MICROARRAY-DATA", "\n")
  print(cbind(colorder[orinf], SCORES[orinf]))
  
}

#2) 
rinsq11EX3.2 (y,x) 
{
  
  
  {
    
    
    #STARTING { NOT TO BE CONFUSED BY IT
    n&lt;-nrow(x)
    d&lt;-ncol(x)
    cat("n=", n, "\n")
    RINFIN&lt;-rep(0,n)
    for (i in 1:n)
    { #STARTS ONE LOOP FOR CALCULATING ALL THE 
      #TAKING THE i-th CASE OUT, 
      #DOING LEAST SQUARES, FIND RESIDUAL
      # OF THE i-th CASE WITH RESPECT TO 
      #THE REGRESSION COEFFICIENT b OBTAINED
      #BELOW
      cat("ITERATION I=", i, "\n")
      AVoitheiax&lt;-x[-i,]
      AVoitheiay&lt;-y[-i]
      results&lt;-lm(AVoitheiay ~AVoitheiax) 
      b&lt;- results$coefficients
      cat("b=", b, "\n")
      residual&lt;-y[i]-sum(b*c(1,x[i,]))
      cat("Residual=", residual, "\n")
      
      # CALCULATING THE ESTIMATE OF E MATRIX
      #AND ITS INVERSE TO FIND INFLUENCE AND RINFIN
      # FUNCTION 3  
      INVERSE&lt;-rin10prodmatrix(AVoitheiax)  
      INFLUENCEFUNCTIONS&lt;-rep(0,d+1)
      cat("INFLUENCE FUNCTIONS=", INFLUENCEFUNCTIONS)
      residualtimesvector&lt;-residual*c(1,x[i,])
      INFLUENCEFUNCTIONS&lt;-INVERSE%*%residualtimesvector
      cat("INFLUENCE FUNCTIONS FOR CASE i=",i, "\n" )
      print(INFLUENCEFUNCTIONS)
      
      #CALCULATING INFLUENCE IN A WAY THAT HELPS CALCULATING
      #DERIVATIVES AND RINFIN
      
      xvoith&lt;-c(1,x[i,])
      INFLUENCEFUNCTIONSMANUAL&lt;-rep(0,d+1)
      COMPONENT&lt;-rep(0,d+1)
      for (j in 1:(d+1))
      {COMPONENT[j]&lt;-sum (INVERSE[j,]*xvoith)
      INFLUENCEFUNCTIONSMANUAL[j]&lt;-
        residual*COMPONENT[j]
      # residual*sum (INVERSE[j,]*xvoith)
      }
      cat("INFLUENCE FUNCTIONS MANUAL FOR CASE i=",i, "\n" )
      print(INFLUENCEFUNCTIONSMANUAL)
      #MATRIX OF DERIVATIVES HAS (d+1) COLUMNS AS THE NUMBER OF 
      #INFLUENCE FUNCTIONS AND THERE ARE d PARTIAL DERIVATIVES
      DERIVATIVESOFINFLUENCEFUNCTIONS&lt;-matrix(rep(0,d*(d+1)),ncol=d+1) 
      for (m in 1:d)
      {
        for (j in 1:(d+1))
        {DERIVATIVESOFINFLUENCEFUNCTIONS[m,j]&lt;--b[m+1]*COMPONENT[j]
        +residual*INVERSE[j,m+1]
        
        }
        
        #cat("DERIVATIVES OF INFLUENCE FUNCTIONS")
        #print(DERIVATIVESOFINFLUENCEFUNCTIONS)
        
        
      }
      cat("DERIVATIVES OF INFLUENCE FUNCTIONS")
      print(DERIVATIVESOFINFLUENCEFUNCTIONS)
      INFLUENCEITHCOORDINATEOFX&lt;-rep(0,d)
      for(k in 1:d)
      {
        INFLUENCEITHCOORDINATEOFX[k]&lt;-INFLUENCEFUNCTIONS[k+1]+
          sum(DERIVATIVESOFINFLUENCEFUNCTIONS[k,]*xvoith)
        # CLOSES THE "FOR" 
        cat("INFLUENCEITHCOORDINATEOFX[k]") 
        print(INFLUENCEITHCOORDINATEOFX[k])
      }    
      
      RINFIN[i]    &lt;-sum((INFLUENCEITHCOORDINATEOFX)^2/n)
      cat("RINFINI")
      print(RINFIN[i])
      #cat("INFLUENCEITHCOORDINATEOFX[k]")
      #print(RINFIN[i])
      
    }
    
    EXTRACTRINFIN&lt;-RINFIN
    
    #cat("RINFIN VALUES FOR EACH CASE")
    #colorder&lt;-c(1:n)
    #orinf&lt;-order(RINFIN)
    #cat("THESE ARE THE RINFIN ORDERED SCORES", "\n")
    #print(cbind(colorder[orinf], RINFIN[orinf]))
  }
  
}

#3)
rin10prodmatrix(A) 
{
  {
    
    
    print(A)
    n&lt;-nrow(A)
    d&lt;-ncol(A)
    #A&lt;-matrix(A,ncol=d)
    #print(A)
    ESTIMATESOFE&lt;-matrix(rep(0,d^2),
                         nrow=d)
    #cat("ESTIMATESOFE", "\n")
    #print(ESTIMATESOFE)
    for (i in 1:d)
      # FUNCTION 4 
    {ESTIMATESOFE[i,i]&lt;-rin10prodequalvector(A[,i])
    }
    
    for(i in 1:(d-1))
    {u&lt;-A[,i]
    
    for(j in (i+1):d)
    {
      v&lt;-A[,j]
      # FUNCTION 5 
      ESTIMATESOFE[i,j]&lt;-rin10prod(u,v)
      ESTIMATESOFE[j,i]&lt;-ESTIMATESOFE[i,j]
      #    cat("ESTIMATESOFE[i,j]=",
      #        ESTIMATESOFE[i,j], "i=", i, "j=", j, "\n")   
    }
    }
    
    
    cat("A", "\n")
    print(A)
    cat("ESTIMATESOFE", "\n")
    print(ESTIMATESOFE)
    means&lt;-rep(0,d)
    for (i in 1:d)
    {means[i]&lt;-mean(A[,i])
    }
    cat("Means 1st line=", means)
    ESTIMATESOFE1&lt;-rbind(means,ESTIMATESOFE)
    cat("ESTIMATESOFE1", "\n")
    print(ESTIMATESOFE1)
    ESTIMATESOFE2&lt;-cbind(c(1,means),ESTIMATESOFE1)
    cat("ESTIMATESOFE2", "\n")
    print(ESTIMATESOFE2)
    
    
    cat("INVERSE MATRIX THAT GIVES INFLUENCE", "\n")
    INVERSE&lt;-solve(ESTIMATESOFE2)
    cat("INVERSE MATRIX", "\n")
    print(INVERSE)
    #INF&lt;-rep(0,d+1) 
    #INF&lt;-INVERSE%*%r*c(1,x[])
  }
  INVERSE
  
  
}

#4)
rin10prodequalvector (u) 
{
  { n&lt;-length(u)
  #pr&lt;-matrix(rep(0, n^2), nrow=n)
  # cat("length of vector=",n,"\n")
  # cat("PRODUCT MATRIX", "\n")
  #  print(pr)
  MEQVECTOR&lt;-mean(u^2)
  cat("ESTIMATE OF SECOND MOMENT=",MEQVECTOR, "\n" )
  # cat("uv", "\n")
  #print(cbind(u,v))
  }
  MEQVECTOR
  
}
5) 
rin10prod  (u,v) 
{
  { n&lt;-length(u)
  #pr&lt;-matrix(rep(0, n^2), nrow=n)
  # cat("length of vector=",n,"\n")
  # cat("PRODUCT MATRIX", "\n")
  #  print(pr)
  pr&lt;-rep(0,n)
  for (i in 1:n)
  {pr[i]&lt;- u[i]*sum(v)}
  # cat("pr","\n")
  # print(pr)
  S&lt;-sum(pr)
  cat("SUM OF PRODUCTS=", S, "\n")
  M&lt;-sum(pr)/n^2
  cat("ESTIMATE OF MEAN PRODUCT=", M,"\n")
  # cat("uv", "\n")
  #print(cbind(u,v))
  }
  M
  
}


#B) FUNCTIONS FOR TABLE 4- THE RINFINABS RESULTS

#1)
rin10EX3.1 (D) 
{    
  
  # This gives 
  lastcol&lt;-ncol(D)
  cat("LAST COL =", lastcol, "\n")
  print(lastcol)
  n&lt;-nrow(D)
  print(n)
  cat("NUMBER OF ROWS =", n, "\n")
  rinftotscore&lt;-rep(0,n)
  y&lt;-D[,lastcol]
  cat("LAST COLUMN=",lastcol,"\n")
  cat("NUMBER OF ROWS=",n, "\n")
  ###THE LINES BEFORE COULD BE ADJUSTED FOR BLOCK OF COLUMNS
  ####DIFFERENT THAN 100 TO CALCULATE SEVERAL RINFIN VALUES
  REPET&lt;-(lastcol-1)/100
  SCORES&lt;-rep(0,n)
  for (j in 1:REPET)
  {
    L&lt;-1+100*(j-1)
    U&lt;-100*j
    # FUNCTION 2 
    x&lt;-rin10EX3.2(y, D[,L:U])
    SCORES&lt;-SCORES+x
    
  }
  cat("WHERE ARE THE SCORES?", "\n")
  print(cbind(1:n,SCORES))
  
  colorder&lt;-c(1:n)
  orinf&lt;-order(SCORES)
  cat("THESE ARE THE ORDERED SCORES FOR MICROARRAY-DATA", "\n")
  print(cbind(colorder[orinf], SCORES[orinf]))
  
}



#2)
rin10EX3.2 (y,x) 
{  
  {       
    #STARTING { NOT TO BE CONFUSED BY IT
    n&lt;-nrow(x)
    d&lt;-ncol(x)
    cat("n=", n, "\n")
    RINFIN&lt;-rep(0,n)
    for (i in 1:n)
    { #STARTS ONE LOOP FOR CALCULATING ALL THE 
      #TAKING THE i-th CASE OUT, 
      #DOING LEAST SQUARES, FIND RESIDUAL
      # OF THE i-th CASE WITH RESPECT TO 
      #THE REGRESSION COEFFICIENT b OBTAINED
      #BELOW
      cat("ITERATION I=", i, "\n")
      AVoitheiax&lt;-x[-i,]
      AVoitheiay&lt;-y[-i]
      results&lt;-lm(AVoitheiay ~AVoitheiax) 
      b&lt;- results$coefficients
      cat("b=", b, "\n")
      residual&lt;-y[i]-sum(b*c(1,x[i,]))
      cat("Residual=", residual, "\n")
      
      # CALCULATING THE ESTIMATE OF E MATRIX
      #AND ITS INVERSE TO FIND INFLUENCE AND RINFIN
      # FUNCTION 3   
      INVERSE&lt;-rin10prodmatrix(AVoitheiax)  
      INFLUENCEFUNCTIONS&lt;-rep(0,d+1)
      cat("INFLUENCE FUNCTIONS=", INFLUENCEFUNCTIONS)
      residualtimesvector&lt;-residual*c(1,x[i,])
      INFLUENCEFUNCTIONS&lt;-INVERSE%*%residualtimesvector
      cat("INFLUENCE FUNCTIONS FOR CASE i=",i, "\n" )
      print(INFLUENCEFUNCTIONS)
      
      #CALCULATING INFLUENCE IN A WAY THAT HELPS CALCULATING
      #DERIVATIVES AND RINFIN
      
      xvoith&lt;-c(1,x[i,])
      INFLUENCEFUNCTIONSMANUAL&lt;-rep(0,d+1)
      COMPONENT&lt;-rep(0,d+1)
      for (j in 1:(d+1))
      {COMPONENT[j]&lt;-sum (INVERSE[j,]*xvoith)
      INFLUENCEFUNCTIONSMANUAL[j]&lt;-
        residual*COMPONENT[j]
      # residual*sum (INVERSE[j,]*xvoith)
      }
      cat("INFLUENCE FUNCTIONS MANUAL FOR CASE i=",i, "\n" )
      print(INFLUENCEFUNCTIONSMANUAL)
      #MATRIX OF DERIVATIVES HAS (d+1) COLUMNS AS THE NUMBER OF 
      #INFLUENCE FUNCTIONS AND THERE ARE d PARTIAL DERIVATIVES
      DERIVATIVESOFINFLUENCEFUNCTIONS&lt;-matrix(rep(0,d*(d+1)),ncol=d+1) 
      for (m in 1:d)
      {
        for (j in 1:(d+1))
        {DERIVATIVESOFINFLUENCEFUNCTIONS[m,j]&lt;--b[m+1]*COMPONENT[j]
        +residual*INVERSE[j,m+1]
        
        }
        
        #cat("DERIVATIVES OF INFLUENCE FUNCTIONS")
        #print(DERIVATIVESOFINFLUENCEFUNCTIONS)
        
        
      }
      cat("DERIVATIVES OF INFLUENCE FUNCTIONS")
      print(DERIVATIVESOFINFLUENCEFUNCTIONS)
      INFLUENCEITHCOORDINATEOFX&lt;-rep(0,d)
      for(k in 1:d)
      {
        INFLUENCEITHCOORDINATEOFX[k]&lt;-INFLUENCEFUNCTIONS[k+1]+
          sum(DERIVATIVESOFINFLUENCEFUNCTIONS[k,]*xvoith)
        # CLOSES THE "FOR" 
        cat("INFLUENCEITHCOORDINATEOFX[k]") 
        print(INFLUENCEITHCOORDINATEOFX[k])
      }    
      
      RINFIN[i]    &lt;-sum(abs(INFLUENCEITHCOORDINATEOFX)/n)
      cat("RINFINI")
      print(RINFIN[i])
      #cat("INFLUENCEITHCOORDINATEOFX[k]")
      #print(RINFIN[i])
      
    }  
    EXTRACTRINFIN&lt;-RINFIN 
    #cat("RINFIN VALUES FOR EACH CASE")
    #colorder&lt;-c(1:n)
    #orinf&lt;-order(RINFIN)
    #cat("THESE ARE THE RINFIN ORDERED SCORES", "\n")
    #print(cbind(colorder[orinf], RINFIN[orinf]))
  }
}

#3) 
#rin10prodmatrix  IS ALREADY IN FOR TABLE 3 SO THE OTHER FUNCTIONS ARE IN.
</d)>