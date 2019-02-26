# clear all variables
rm(list = ls(all = TRUE))
graphics.off()

# install and load packages
libraries = c("plotly","quantreg")
lapply(libraries, function(x) if (!(x %in% installed.packages())) {
  install.packages(x)
})
lapply(libraries, library, quietly = TRUE, character.only = TRUE)


##### Make sure to have run all the files corresponding to all strategies function
##### The function could be found in their respective folder in CRIX_tedas quanlet

##### Initial Parameters
tau=c(0.05, 0.15, 0.25, 0.35, 0.50)  ### &lt;-- Default tau used in all calculus in Master thesis

##### Tau spine function
tau_spine=function(x,tau){
  
  return(min(tau[tau &gt;= x]))
  
}


##### Cumulative return for the Core


Wealth_CORE_SP500  = cumsum(c(1,SP500[window_length:(length(SP500))]))
Wealth_CORE_SP500  = Wealth_CORE_SP500[-length(Wealth_CORE_SP500)]

Wealth_CORE_NASDAQ  = cumsum(c(1,NASDAQ[window_length:(length(NASDAQ))]))
Wealth_CORE_NASDAQ  = Wealth_CORE_NASDAQ[-length(Wealth_CORE_NASDAQ)]

Wealth_CORE_FTSE100  = cumsum(c(1,FTSE100[window_length:(length(FTSE100))]))
Wealth_CORE_FTSE100  = Wealth_CORE_FTSE100[-length(Wealth_CORE_FTSE100)]

Wealth_CORE_DAX_30  = cumsum(c(1,DAX_30_PERFORMANCE[window_length:(length(DAX_30_PERFORMANCE))]))
Wealth_CORE_DAX_30  = Wealth_CORE_DAX_30[- length(Wealth_CORE_DAX_30_PERFORMANCE) ]

Wealth_CORE_NIKKEI_225   = cumsum(c(1,NIKKEI_225[window_length:(length(NIKKEI_225))]))
Wealth_CORE_NIKKEI_225   = Wealth_CORE_NIKKEI_225[-length(Wealth_CORE_NIKKEI_225 )]


### Clear variables
remove(Wealth_CORE_SP500,Wealth_CORE_NASDAQ,Wealth_CORE_FTSE100,Wealth_CORE_DAX_30_PERFORMANCE,Wealth_CORE_NIKKEI_225)


##### SP500 Cumulative return for the different strategies 

Wealth_NAIVE_SP500 = TEDAS_NAIVE(data_cryptos,window_length,SP500,tau)
Wealth_HYBRID_MV_SP500 = TEDAS_HYBRID_MV(data_cryptos,window_length,SP500,tau)
Wealth_HYBRID_SP500 = TEDAS_HYBRID(data_cryptos,window_length,SP500,tau)
Wealth_RISK_WEIGHTED_SP500 = RISK_WEIGHTED_ALLOCATION(data_cryptos,window_length,SP500)

##### NASDAQ Cumulative return for the different strategies 

Wealth_NAIVE_NASDAQ = TEDAS_NAIVE(data_cryptos,window_length,NASDAQ,tau)
Wealth_HYBRID_MV_NASDAQ = TEDAS_HYBRID_MV(data_cryptos,window_length,NASDAQ,tau)
Wealth_HYBRID_NASDAQ = TEDAS_HYBRID(data_cryptos,window_length,NASDAQ,tau)
Wealth_RISK_WEIGHTED_NASDAQ = RISK_WEIGHTED_ALLOCATION(data_cryptos,window_length,NASDAQ)

##### FTSE100 Cumulative return for the different strategies 

Wealth_NAIVE_FTSE100 = TEDAS_NAIVE(data_cryptos,window_length,FTSE100,tau)
Wealth_HYBRID_MV_FTSE100 = TEDAS_HYBRID_MV(data_cryptos,window_length,FTSE100,tau)
Wealth_HYBRID_FTSE100 = TEDAS_HYBRID(data_cryptos,window_length,FTSE100,tau)
Wealth_RISK_WEIGHTED_FTSE100 = RISK_WEIGHTED_ALLOCATION(data_cryptos,window_length,FTSE100)

##### DAX 30 Cumulative return for the different strategies 

Wealth_NAIVE_DAX_30 = TEDAS_NAIVE(data_cryptos,window_length,DAX_30_PERFORMANCE,tau)
Wealth_HYBRID_MV_DAX_30 = TEDAS_HYBRID_MV(data_cryptos,window_length,DAX_30_PERFORMANCE,tau)
Wealth_HYBRID_DAX_30 = TEDAS_HYBRID(data_cryptos,window_length,DAX_30_PERFORMANCE,tau)
Wealth_RISK_WEIGHTED_DAX_30 = RISK_WEIGHTED_ALLOCATION(data_cryptos,window_length,DAX_30_PERFORMANCE)

##### NIKKEI 225 Cumulative return for the different strategies 

Wealth_NAIVE_NIKKEI_225 = TEDAS_NAIVE(data_cryptos,window_length,NIKKEI_225,tau)
Wealth_HYBRID_MV_NIKKEI_225 = TEDAS_HYBRID_MV(data_cryptos,window_length,NIKKEI_225,tau)
Wealth_HYBRID_NIKKEI_225 = TEDAS_HYBRID(data_cryptos,window_length,NIKKEI_225,tau)
Wealth_RISK_WEIGHTED_NIKKEI_225 = RISK_WEIGHTED_ALLOCATION(data_cryptos,window_length,NIKKEI_225)

##### Clear variables 

remove(Wealth_NAIVE_SP500,Wealth_HYBRID_MV_SP500,Wealth_HYBRID_SP500,Wealth_RISK_WEIGHTED_SP500)
remove(Wealth_NAIVE_NASDAQ,Wealth_HYBRID_MV_NASDAQ,Wealth_HYBRID_NASDAQ,Wealth_RISK_WEIGHTED_NASDAQ)
remove(Wealth_NAIVE_FTSE100,Wealth_HYBRID_MV_FTSE100,Wealth_HYBRID_FTSE100,Wealth_RISK_WEIGHTED_FTSE100)
remove(Wealth_NAIVE_DAX_30,Wealth_HYBRID_MV_DAX_30,Wealth_HYBRID_DAX_30,Wealth_RISK_WEIGHTED_DAX_30)
remove(Wealth_NAIVE_NIKKEI_225,Wealth_HYBRID_MV_NIKKEI_225,Wealth_HYBRID_NIKKEI_225,Wealth_RISK_WEIGHTED_NIKKEI_225)




##### Change the wealth and the core wealth in the below function

plot_ly(x = Core$date[window_length: length(Core$date)]) %&gt;%
  add_lines(y = Wealth_CORE_DAX_30[1 : (length(Core$date)-window_length+1) ], name = "Core",mode="lines",line = list(color = 'rgb(0, 0, 0)'))%&gt;%
  add_lines(y = Wealth_NAIVE_DAX_30[1 :( length(Core$date)-window_length+1 )], name = "Wealth_NAIVE",line = list(mode = "lines"))%&gt;%
  add_lines(y = Wealth_HYBRID_DAX_30[1 :( length(Core$date)-window_length+1 )], name = "Wealth_HYBRID",line = list(mode = "lines"))%&gt;%
  add_lines(y = Wealth_HYBRID_MV_DAX_30[1 :( length(Core$date)-window_length+1 )], name = "Wealth_HYBRID_MV",line = list(mode = "lines"))%&gt;%
  add_lines(y = Wealth_RISK_WEIGHTED_DAX_30[1 :( length(Core$date)-window_length+1 )], name = "Wealth_RISK_WEIGHTED",line = list(mode = "lines",color = 'rgb(0, 0, 255)'))%&gt;%
  layout(title = paste("Cumulative return for",tochar(DAX_30), "with window length=",as.character(window_length),sep=" "))
