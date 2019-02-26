#install.packages("arules")
#install.packages("arulesViz")
#install.packages("manipulate")

library("arules")
library("arulesViz")
library("MASS")

#AD &lt;- read.csv("questionnaire.csv",row.names=1)
#ML &lt;- read.csv("monitor.csv",row.names=1)
attach(ML)
#QD &lt;- read.csv("Content.csv",row.names=1)
MC="Media"

source("AP.R")
AP(Q="Q15",Cex=1,As=25,Ms=50,Supp=0.01,Conf=0.1,Lift=1.3,l=2,Lhs="M")

library("manipulate")
manipulate(AP(Q,Cex,As,Ms,supp,conf,lift,2,Lhs),
           Q=picker(as.list(row.names(QD[QD[,1]=="MA",])),initial="Q15"),
           Lhs=picker("M","A"),
           supp=slider(0,0.1,initial=0.01,step=0.001),
           conf=slider(0,1,initial=0.1,step=0.01),
           lift=slider(0.1,2,initial=1.3,step=0.1),
           Cex=slider(0.1,2,1),
           As=slider(0,100,25),
           Ms=slider(0,100,50)
           )
