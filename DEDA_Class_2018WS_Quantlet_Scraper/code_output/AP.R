AP &lt;- function(Q="Q15",Cex=1,As=25,Ms=50,Supp=0.01,Conf=0.1,Lift=1.3,l=2,Lhs="M"){
  
#  AD &lt;- read.csv("questionnaire.csv",row.names=1)
#  ML &lt;- read.csv("monitor.csv",row.names=1)
  attach(ML)
#  QD &lt;- read.csv("Content.csv",row.names=1)
  MC="Media"
  
  ADS &lt;- AD[,substring(colnames(AD),1,nchar(Q)+1)==paste0(Q,"_")]
  if(Q=="Q15")
    ADS &lt;- AD[,substring(colnames(AD),1,nchar(Q)+1)=="Q14_"]
  for(j in 1:ncol(ADS))
    colnames(ADS)[j] &lt;- as.character(QD[row.names(QD)==Q,j+4])
  
  cross &lt;- 0
  for(j in 1:ncol(ADS)){
    cr &lt;- table(ADS[,j],ML[,MC])
    if(sum(cr[1,])==nrow(ADS))
      cr &lt;- rbind(cr,0)
    if(row.names(cr)[1]==0)
      row.names(cr)[2] &lt;- 1
    if(row.names(cr)[1]==1)
      row.names(cr)[2] &lt;- 0
    cross &lt;- rbind(cross,cr)
  }
  cross &lt;- cross[row.names(cross)==1,]
  row.names(cross) &lt;- colnames(ADS)
  colnames(cross) &lt;- c(levels(ML[,MC]))
  
  cross &lt;- cross[rowSums(cross)!=0,]
  
  
  TB &lt;- corresp(cross,2)
  TOB &lt;- rbind(TB$rscore,TB$cscore)
  
  ADS &lt;- as.data.frame(ADS)
  AD2 &lt;- cbind(ADS,ML[,MC])
  for(j in 1:ncol(AD2))
    AD2[,j] &lt;- factor(AD2[,j])
  ADT &lt;- as(AD2,"transactions")
  ADM &lt;- as(ADT,"matrix")
  ADMS &lt;- ADM[,substring(colnames(ADM),nchar(colnames(ADM))) != "0"]
  colnames(ADMS) &lt;- c(row.names(cross),colnames(cross))
  ADTS &lt;- as(ADMS,"transactions")
  
  if(Lhs=="M")
    MT &lt;- apriori(ADTS,p=list(support=Supp,confidence=Conf,maxlen=l),
                  appearance=list(lhs=colnames(cross),default="rhs"))
  if(Lhs=="A")
    MT &lt;- apriori(ADTS,p=list(support=Supp,confidence=Conf,maxlen=l),
                  appearance=list(rhs=colnames(cross),default="lhs"))
                
  MT &lt;- subset(MT,subset=(lift&gt;Lift))
  AT &lt;- as(sort(MT, by = "lift"),"data.frame")

  source("AHT2.R")
  AR &lt;- AHT2(AT)
  
  
  
  MTA &lt;- apriori(ADTS,p=list(support=0,confidence=0,maxlen=1),
                 appearance=list(lhs=colnames(cross),default="rhs"))
  ATA &lt;- as(MTA,"data.frame")
  ARA &lt;- AHT2(ATA)[,2:3]
  ARA2 &lt;- as.data.frame(matrix(0,nrow(ARA),1,dimnames=list(row.names(cross),"supp")))
  for(i in 1:nrow(ARA2)){
    ARA2[i,] &lt;- ARA[ARA[,1]==row.names(ARA2)[i],2]
  }
  
  MTM &lt;- apriori(ADTS,p=list(support=0.1,confidence=0.1,maxlen=1),
                 appearance=list(rhs=colnames(cross),default="lhs"))
  ATM &lt;- as(MTM,"data.frame")
  ARM &lt;- AHT2(ATM)[,2:3]
  
  ARM2 &lt;- as.data.frame(matrix(0,nrow(ARM),1,dimnames=list(colnames(cross),"supp")))
  for(i in 1:nrow(ARM2)){
    ARM2[i,] &lt;- ARM[ARM[,1]==row.names(ARM2)[i],2]
  }
  
  color &lt;- "gray"
  if(MC=="Media")
    color &lt;- c("#ffa07a","#ff6347","#ff0000","#87cefa","#6495ed","#0000ff")
 
  
  plot(TOB,type="n",xlab="",ylab="",xaxt="n",yaxt="n",main=as.character(QD[row.names(QD)==Q,4]))
  
  #abline(v=(max(TOB[,1])+min(TOB[,1]))/2,h=(max(TOB[,2])+min(TOB[,2]))/2,col="blue")
  #abline(v=0,h=0,col="green")
  
  points(TB$rscore,cex=ARA2[,1]*As,pch=19,col="limegreen")
  if(Q=="Q41")
    points(TB$rscore,cex=ARA2[,1]*As,pch=19,col=terrain.colors(nrow(TB$rscore)))
  points(TB$cscore,cex=ARM2[,1]*Ms,pch=18,col=color)
  text(TOB,label=row.names(TOB),cex=Cex)
  
  for(i in 1:nrow(AR)){
    lhsz &lt;- TOB[AR[i,1]==row.names(TOB),]
    rhsz &lt;- TOB[AR[i,2]==row.names(TOB),]
    aw &lt;- (AR[i,3]/max(AR[,3]))*3
    if(lhsz[1] &lt; rhsz[1])
      rhsz[1] = rhsz[1]-0.05
    if(lhsz[1] &gt; rhsz[1])
      rhsz[1] = rhsz[1]+0.05
    if(lhsz[2] &lt; rhsz[2])
      rhsz[2] = rhsz[2]-0.05
    if(lhsz[2] &gt; rhsz[2])
      rhsz[2] = rhsz[2]+0.05
    arrows(lhsz[1],lhsz[2],rhsz[1],rhsz[2],
           length = 0.15,angle = 15,lwd = aw,col=paste0("gray",100-round(AR[i,5]/max(AR[,5])*100)))  
  }
  #identify(TOB[,1],TOB[,2],row.names(TOB),cex = 1.5)
  
  return(inspect(sort(MT, by = "lift")))
}
