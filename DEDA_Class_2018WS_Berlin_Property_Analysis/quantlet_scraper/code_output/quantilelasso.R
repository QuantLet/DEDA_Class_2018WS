#---------------------------------------------------------------------------
#Author: Youjuan Li and Ji Zhu, University of Michigan (jizhu@umich.edu)
#---------------------------------------------------------------------------

#######################################################################
### All rights are reserved by the authors.
### Authors: Youjuan Li, Ji Zhu, University of Michigan (jizhu@umich.edu)
### Date:    07/01/2006
#######################################################################
#Compute the entire L1-norm Path
#Plot the Fitted Coefficients vs. s
qrL1 &lt;- function(x, y, a, max.steps,eps1=10^(-10), eps2=10^(-6), trace=T) {
  ### y in Real set 
  ### a_th quantile
  ### eps1: numerical
  ### eps2: lambda limit
  ### residual: y-f
  ### indL: index
  ### indV: index
  ### indE: index
  ### lambda save the -lambde values for all steps
  call &lt;- match.call()
  ### To guarantee distinct y's even for tied data
  tmpy &lt;- unique(y)
  if (length(tmpy) &lt; length(y)) {
    dif &lt;- min(diff(sort(tmpy)))
    y &lt;- y + rnorm(length(y), 0, dif/100)
  }
  ### Can also transfer x to x=h(x),arbitrary basis functions
  
  if(trace)
    #cat("LASSO sequencen")
    
    n &lt;- dim(x)[1]
  m &lt;- dim(x)[2]
  maxvars &lt;- min(m, n - 1)
  indm &lt;- seq(m)
  indn &lt;- seq(n)
  indR &lt;- indE &lt;- NULL
  indL &lt;- seq(n)
  ###z &lt;- y*x
  
  ini &lt;- qrL1Ini(x, y, a)
  indV &lt;- ini$indV
  indE &lt;- ini$indE
  indL &lt;- ini$indL
  indR &lt;- ini$indR
  u &lt;- ini$u
  u0 &lt;- ini$u0
  residual &lt;- ini$residual
  
  if(missing(max.steps))
    max.steps &lt;- n * maxvars 
  ###first column is for s=0, 2nd column is for the 1st event
  beta &lt;- matrix(0, max.steps + 1, m)
  beta0 &lt;- rep(0, max.steps + 1)
  Cgacv &lt;- double(max.steps + 1)
  Csic &lt;- double(max.steps + 1)
  fit &lt;- matrix(0, max.steps + 1, n)
  checkf &lt;- rep(0, max.steps + 1)
  lambda &lt;- rep(0, max.steps + 1)
  lamb.the &lt;- rep(0, max.steps + 1)
  sdistance &lt;- rep(0, max.steps + 1)
  Elbow.list &lt;- as.list(seq(max.steps+1))
  V.list &lt;- as.list(seq(max.steps+1))
  theta.g &lt;- matrix(0, max.steps + 1, n)
  beta0[1] &lt;- ini$quant
  lambda[1] &lt;- ini$lambda
  Elbow.list[[1]] &lt;- NULL
  V.list[[1]] &lt;- NULL
  ### Calculate criteria SIC and GACV
  fit[1,] &lt;- beta0[1] 
  checkf[1] &lt;- pf(fit[1,], y, a)
  trHat &lt;- 0
  Cgacv[1] &lt;- checkf[1] / (n-trHat)
  Csic[1] &lt;- log(checkf[1]/n) + (log(n)/(2*n))*trHat
  theta.g[1,][indL] &lt;- -(1-a)
  theta.g[1,][indR] &lt;- a
  theta.g[1,][indE] &lt;- a
  #cat("Initial variables:", indV, "n")
  #cat("Initial obs at elbow:", indE, "n")
  
  drop &lt;- F # to do with "lasso"
  
  k &lt;- 0
  ### Main loop for path
  while(k &lt; max.steps) {
    k &lt;- k + 1
    #cat("step:",k,"n")
    
    ### Check how far to go in u
    ### Consider a point hits elbow
    gam &lt;-  u0 + x[-indE,indV,drop=F] %*% u
    delta1 &lt;- residual[-indE]/gam
    if (length(delta1[delta1&lt;=0])==length(delta1)){
      delta &lt;- Inf
    } else {
      delta.add &lt;- delta1[delta1&gt;0]
      delta &lt;- min(delta.add[delta.add &gt; eps1],na.rm=T)
    }
    
    ####For situation that beta may leave from V when k&gt;1
    if(k &gt; 1) {
      delta2 &lt;- -beta[k, indV]/u
      if (length(delta2[delta2&lt;=0])==length(delta2)){
        tmpz.remove &lt;- Inf
      } else {
        tmpz &lt;- delta2[delta2&gt;0]
        tmpz.remove &lt;- min(tmpz[tmpz &gt; eps1], na.rm=T)
      }
      if (tmpz.remove &lt; delta) {
        drop &lt;- T
        delta &lt;- tmpz.remove
      } else {
        drop &lt;- F
      }  
    }
    #cat("Move distance:", delta, "n")
    
    if (delta == Inf) {
      #cat("Path stops here.n")
      break
    }
    
    sdistance[k+1] &lt;- sdistance[k]+delta
    
    if (drop == T) {
      tmpdelta &lt;- delta2[delta2 &gt; eps1]
      tmpind &lt;- indV[delta2 &gt; eps1]    
      j1 &lt;- tmpind[which.min(tmpdelta)]
      j2 &lt;- which(indV == j1)
      #cat("Var:", j1, "removedn")
    } else {
      
      ### next point hits elbow when the path go in u, denote it as istar
      tmpind &lt;- indn[-indE]
      tmpdelta &lt;- delta1[delta1 &gt; eps1]
      tmpind &lt;- tmpind[delta1 &gt; eps1]
      istar &lt;- tmpind[which.min(tmpdelta)]
      #cat("Obs:", istar, "hits elbow ")
      #if(match(istar, indL, FALSE))
      #cat("from left.n")
      #else
      #cat("from right.n")
    }
    
    ###Update parameters
    beta[k+1,] &lt;- beta[k,]
    beta[k+1,indV] &lt;- beta[k+1,indV] + delta * u
    beta0[k+1] &lt;- beta0[k] + delta * u0
    residual[-indE] &lt;- residual[-indE] - delta*gam
    Elbow.list[[k+1]] &lt;- indE
    V.list[[k+1]] &lt;- indV
    
    ### Calculate criteria SIC and GACV
    fit[k+1,] &lt;- beta0[k+1] + beta[k+1,] %*% t(x)
    checkf[k+1] &lt;- pf(fit[k+1,], y, a)
    trHat &lt;- length(V.list[[k+1]])
    Cgacv[k+1] &lt;- checkf[k+1] / (n-trHat)
    Csic[k+1] &lt;- log(checkf[k+1]/n) + (log(n)/(2*n))*trHat    
    
    if (length(indV) != length(indE))
      warning("No. var != No. obs at elbow.n")
    
    if (sum(indL)+sum(indR) == 1 &amp;&amp; drop == F) {
      lambda[k] &lt;- 0
      #cat("No point on left or right.n")
      break
    }
    
    ### check which event occurs
    ### Add a new variable to indV, we have already known istar hitting elbow
    ### lambdvar and lambdaobs are -lambda
    if (length(indV) == m){
      lambdavar &lt;- Inf
    } else {
      inactive &lt;- indm[ - indV ]
      tmpE &lt;- indE
      tmpL &lt;- indL
      tmpR &lt;- indR
      if (drop  == T){
        indV &lt;- indV[ - j2 ]
      } else {
        tmpE &lt;- c(tmpE, istar)
        if(match(istar, indL, FALSE)){
          tmpL&lt;-setdiff(indL,istar)
        } else {
          tmpR&lt;-setdiff(indR,istar)
        }
      }
      tmp &lt;- rbind(c(0, sign(beta[k+1, indV])), cbind(1, x[tmpE, indV]))
      tmpb &lt;- c(1, rep(0, length(indV)+1))
      tmplvar &lt;- length(tmpb)
      uvar &lt;- matrix(0, nrow=tmplvar, ncol=length(inactive))
      lambdavar &lt;- rep(Inf, length(inactive))
      ###Start j loop
      for (j in 1:length(inactive)) {
        jstar &lt;- inactive[j]
        tmpA1 &lt;- cbind(tmp, c(1, x[tmpE,jstar]))
        tmpqr &lt;- qr(tmpA1)
        if (tmpqr$rank &lt; tmplvar)
          tmplam1 &lt;- Inf
        else {
          tmpu1 &lt;- qr.solve(tmpqr, tmpb)
          if (tmpu1[tmplvar] &gt; 0) {
            tmpV &lt;- c(indV, jstar)
            tmplam1 &lt;- (1-a)*sum(cbind(rep(1,length(tmpL)), x[tmpL,tmpV,drop=F]) %*% tmpu1)-a*sum(cbind(rep(1,length(tmpR)), x[tmpR,tmpV,drop=F]) %*% tmpu1) 
          }
          else
            tmplam1 &lt;- Inf
        }
        tmpA2 &lt;- cbind(tmp, c(-1, x[tmpE,jstar]))
        tmpqr &lt;- qr(tmpA2)
        if (tmpqr$rank &lt; tmplvar)
          tmplam2 &lt;- Inf
        else {
          tmpu2 &lt;- qr.solve(tmpqr, tmpb)
          if (tmpu2[tmplvar] &lt; 0) {
            tmpV &lt;- c(indV, jstar)
            tmplam2 &lt;- (1-a)*sum(cbind(rep(1,length(tmpL)), x[tmpL,tmpV,drop=F]) %*% tmpu2)-a*sum(cbind(rep(1,length(tmpR)), x[tmpR,tmpV,drop=F]) %*% tmpu2)
          }
          else
            tmplam2 &lt;- Inf
        }
        if (tmplam1 == Inf &amp;&amp; tmplam2 == Inf)
          lambdavar[j] &lt;- Inf
        else if (tmplam1 &lt; tmplam2) {
          lambdavar[j] &lt;- tmplam1
          uvar[,j] &lt;- tmpu1
        }
        else if (tmplam1 &gt; tmplam2) {
          lambdavar[j] &lt;- tmplam2
          uvar[,j] &lt;- tmpu2
        }
        else {
          lambdavar[j] &lt;- tmplam1
          uvar[,j] &lt;- tmpu1
          warning("tmplam1 == tmplam2 n")
          #cat("Tie in variable:", jstar, "n")
        }
      }
      ###end of j loop
    }
    
    
    ### Remove an observation from indE
    tmp &lt;- rbind(c(0, sign(beta[k+1, indV])), cbind(1, x[tmpE, indV]))
    tmpb &lt;- c(1, rep(0, length(indV)))
    tmplobs &lt;- length(tmpb)
    uobs &lt;- matrix(0, nrow=length(indV)+1, ncol=length(tmpE))
    lambdaobs &lt;- rep(0, length(tmpE))
    leftobs &lt;- rep(F, length(tmpE))
    ###begin i loop
    for (i in 1:length(tmpE)) {
      tmpA &lt;- tmp[-(i+1),]
      tmpqr &lt;- qr(tmpA)
      if (tmpqr$rank &lt; tmplobs) {
        tmplam &lt;- Inf
      } else {
        tmpu &lt;- qr.solve(tmpqr, tmpb)
        uobs[,i] &lt;- tmpu
        tmplam &lt;- (1-a)*sum(cbind(rep(1,length(tmpL)), x[tmpL,indV,drop=F]) %*% tmpu)-a*sum(cbind(rep(1,length(tmpR)), x[tmpR,indV,drop=F]) %*% tmpu)                 
        tmpi &lt;- tmpE[i]
        tmpyf &lt;- c(1, x[tmpi,indV,drop=F]) %*% tmpu
        if (tmpyf &gt; 0) {
          ###y_i move to Left
          tmplam &lt;- tmplam + (1-a)*tmpyf
          leftobs[i] &lt;- T
        } else {
          ###y_i move to Right
          tmplam &lt;- tmplam - a*tmpyf
        }
      }
      lambdaobs[i] &lt;- tmplam
    }
    ##end i loop
    
    ### Compare var and obs
    lam1 &lt;- min(lambdavar)
    lam2 &lt;- min(lambdaobs)
    if (lam1 &lt; lam2 &amp;&amp;  lam1 &lt; 0) {
      ### one variable not in |V| is added into |V|
      tmpj &lt;- which.min(lambdavar)
      jstar &lt;- inactive[tmpj]
      indV &lt;- c(indV, jstar)
      ### If drop==T, on change for E
      if (drop == F) {
        indE &lt;- c(indE, istar)
        if(match(istar, indL, FALSE))
          indL &lt;- setdiff(indL,istar)
        else
          indR &lt;- setdiff(indR,istar)
      }
      u0 &lt;- uvar[1,tmpj]
      u &lt;- uvar[2:tmplvar,tmpj]
      lambda[k+1] &lt;- lam1
      #cat("Variable:", jstar, "addedn")
    } else if (lam2 &lt; lam1 &amp;&amp; lam2 &lt; 0) {
      ### remove a point from indE 
      if (drop == F) {
        ## istar stay on E, no change for V  
        if(match(istar, indL, FALSE)){
          indL &lt;- setdiff(indL,istar)
        } else{
          indR &lt;- setdiff(indR,istar)
        }   
      }
      tmpi &lt;- which.min(lambdaobs)
      ### update E
      istar &lt;- tmpE[tmpi]
      u0 &lt;- uobs[1,tmpi]
      u &lt;- uobs[2:tmplobs,tmpi]
      lambda[k+1] &lt;- lam2
      #cat("Observation:", istar,"removed ")
      if (leftobs[tmpi] == F) {
        #cat("to right.n")
        indR &lt;- c(indR,istar)
      } else {
        #cat("to left.n")
        indL &lt;- c(indL,istar)
      }
      indE &lt;- tmpE[-tmpi]
    } else {
      #cat("lam1:", lam1, "n")
      #cat("lam2:", lam2, "n")
      #cat("No descent!n")
      break
    }     
    if (abs(lambda[k+1]) &lt; eps2) {
      #cat("Descent too small.n")
      break
    }
    drop &lt;- F   
  }  
  ###end of main loop for path
  
  beta &lt;- beta[seq(k+1),]
  beta0 &lt;- beta0[seq(k+1)]
  lambda &lt;- lambda[seq(k+1)]
  #lamb.the &lt;- lamb.the[seq(k+1)]
  
  object &lt;- list(call = call, beta0 = beta0, beta = beta, Elbow = Elbow.list[seq(k+1)], V = V.list[seq(k+1)], lambda=lambda, s=sdistance[seq(k+1)], Csic=Csic[seq(k+1)], Cgacv=Cgacv[seq(k+1)])
  object
}
qrL1Ini &lt;- function(x, y, a, eps=10^(-10)) {
  ### y in Real set
  ### &gt; eps is defined as nonzero
  ### a_th quantile
  ### lambda is "-lambda"
  n &lt;- dim(x)[1]
  m &lt;- dim(x)[2]
  yr &lt;- sort(y)
  ### Two cases 
  quant &lt;- yr[ floor(n*a)+1 ]
  index &lt;- match(quant, y)
  indm &lt;- seq(m)
  indE &lt;- index
  indR &lt;- seq(y)[y &gt; y[index]]
  indL &lt;- seq(y)[y &lt; y[index]]
  indV &lt;- NULL
  beta0&lt;-quant
  beta &lt;- rep(0,m)
  ###current f=beta0
  residual &lt;- y-beta0
  inactive &lt;- indm
  tmpE &lt;- indE
  tmpL &lt;- indL
  tmpR &lt;- indR
  tmpb &lt;- c(1,0)
  tmplvar &lt;- length(tmpb)
  uvar &lt;- matrix(0, nrow=tmplvar, ncol=length(inactive))
  lambdavar &lt;- rep(Inf, length(inactive))
  ###beginning of loop j
  for (j in 1:length(inactive)) {
    jstar &lt;- inactive[j]
    tmpA1 &lt;- cbind(c(0,1), c(1, x[tmpE,jstar]))
    tmpqr &lt;- qr(tmpA1)
    if (tmpqr$rank &lt; tmplvar)
      tmplam1 &lt;- Inf
    else { 
      tmpu1 &lt;- qr.solve(tmpqr, tmpb)
      if (tmpu1[tmplvar] &gt; 0) {
        tmpV &lt;- jstar
        tmplam1 &lt;- (1-a)*sum(cbind(rep(1,length(tmpL)), x[tmpL,tmpV,drop=F]) %*% tmpu1)-a*sum(cbind(rep(1,length(tmpR)), x[tmpR,tmpV,drop=F]) %*% tmpu1) 
      } else tmplam1 &lt;- Inf
    }
    tmpA2 &lt;- cbind(c(0,1), c(-1, x[tmpE,jstar]))
    tmpqr &lt;- qr(tmpA2)
    if (tmpqr$rank &lt; tmplvar)
      tmplam2 &lt;- Inf
    else { tmpu2 &lt;- qr.solve(tmpqr, tmpb)
           if (tmpu2[tmplvar] &lt; 0) {
             tmpV &lt;- jstar
             tmplam2 &lt;- (1-a)*sum(cbind(rep(1,length(tmpL)), x[tmpL,tmpV,drop=F]) %*% tmpu2)-a*sum(cbind(rep(1,length(tmpR)), x[tmpR,tmpV,drop=F]) %*% tmpu2)
           }
           else tmplam2 &lt;- Inf
    }
    
    if (tmplam1 == Inf &amp;&amp; tmplam2 == Inf)
      lambdavar[j] &lt;- Inf
    else if (tmplam1 &lt; tmplam2) {
      lambdavar[j] &lt;- tmplam1
      uvar[,j] &lt;- tmpu1
    }
    else if (tmplam1 &gt; tmplam2) {
      lambdavar[j] &lt;- tmplam2
      uvar[,j] &lt;- tmpu2
    }
    else {
      lambdavar[j] &lt;- tmplam1
      uvar[,j] &lt;- tmpu1
      warning("tmplam1 == tmplam2 n")
      #cat("Tie in variable:", jstar, "n")
    }
  }
  ###end of loop j 
  lambda &lt;- min(lambdavar)
  tmpj &lt;- which.min(lambdavar)
  jstar &lt;- inactive[tmpj]
  indV &lt;- jstar
  u0 &lt;- uvar[1,tmpj]
  u &lt;- uvar[2:tmplvar,tmpj]
  ####################Not updat right now
  ###Compute how far to go in u
  #gamL &lt;- (1-a)*( u0 + x[indL,indV,drop=F] %*% u)
  #gamR &lt;- -a*( u0 + x[indR,indV,drop=F] %*% u) 
  #delta1L &lt;- residual[indL]/gamL
  #delta1R &lt;- residual[indR]/gamR
  #delta1 &lt;- c(delta1L,delta1R)
  #delta &lt;- min(delta1[delta1 &gt; eps], na.rm=T)
  #cat("Move distance:", delta, "n")
  #beta[indV] &lt;- delta * u
  #beta0 &lt;- beta0 + delta * u0
  #residual[indL] &lt;- residual[indL] - delta*gamL
  #residual[indR] &lt;- residual[indR] - delta*gamR
  #residual [indE] &lt;- 0 
  ########################
  return(list(beta=beta, beta0=beta0, u=u, u0=u0, quant=quant,lambda = lambda,
              indV=indV, indE=indE, indR=indR, indL=indL, residual=residual))
}

qrL1.predict &lt;- function(obj, x, y=NULL) {
  ### obj: output of svmL1
  ### y in {1,-1}
  N &lt;- length(y)
  f &lt;- obj$beta %*% t(x) + obj$beta0
  predict &lt;- sign(f)
  if (is.null(y))
    return(list(f=f, error=NULL))
  error &lt;- apply(apply(predict, 1, FUN="!=", y), 2, sum)/N
  return(list(f=f, error=error))
}

###this defines the check function
pf &lt;- function(beta0, y0, tau) {
  tmp &lt;- y0 - beta0
  sum(tau*tmp[tmp&gt;0]) - sum((1-tau)*tmp[tmp&lt;=0])  
}

##Get path of coefficients
"plot.L1qr" &lt;-
  function(x, breaks = TRUE, eps = 1e-10, ...)
  {
    object &lt;- x  
    coef1 &lt;- object$beta  ### Get rid of many zero coefficients
    ##coef1 &lt;- scale(coef1, FALSE, 1/object$normx)
    ##coef1[,4066] &lt;- 0 
    c1 &lt;- drop(rep(1, nrow(coef1)) %*% abs(coef1))
    nonzeros &lt;- c1 &gt; eps
    cnums &lt;- seq(nonzeros)[nonzeros]
    coef1 &lt;- coef1[, nonzeros]
    
    s1 &lt;- apply(abs(coef1), 1, sum)
    s1 &lt;- s1/max(s1)
    
    #xname&lt;-"|beta|/max|beta|"                
    xname&lt;-"s" 
    matplot(s1, coef1, xlab = xname, ..., type = "b", lty=rep(1,dim(coef1)[2]),lwd=2,
            pch = "*", ylab = "Coefficients", cex.lab=2, cex.axis=1.5)
    #title("Quantile Lasso Path",line=2.5)
    abline(h = 0, lty = 3)
    axis(4, at = coef1[nrow(coef1),  ], label = paste(cnums
    ), cex = 2, cex.lab=2, cex.axis=1.5, adj = 1)
    if(breaks) {
      #axis(3, at = s1, labels = paste(seq(s1)-1),cex=.8)
      abline(v = s1,lwd=0.5,col="grey")
    }
    
    invisible()
  }
