# clear all variables
rm(list = ls(all = TRUE))
graphics.off()

# install and load packages
libraries = c("quadprog", "quantreg", "plotly")
lapply(libraries, function(x) if (!(x %in% installed.packages())) {
  install.packages(x)
})
lapply(libraries, library, quietly = TRUE, character.only = TRUE)


### Initial Parameters 
tau=c(0.05, 0.15, 0.25, 0.35, 0.50) # &lt;- Default setting
window_length = # &lt;- to modify
  
### tau spine function
tau_spine=function(x,tau){
  return(min(tau[tau &gt;= x]))
}
  
### Function for plotting
plot_wealth = function(strategy_wealth,core,strategy_string,core_string,date_range,windowLength){
    
    core_wealth = cumsum(c(1,core[windowLength:(length(core))]))
    core_wealth  = core_wealth[-length(core_wealth)]
    
    plot_ly(x = date_range[windowLength: length(date_range)]) %&gt;%
      add_lines(y = core_wealth[1 : (length(date_range)-windowLength+1) ], name = paste("Core ",core_string),mode="lines")%&gt;%
      add_lines(y = strategy_wealth[1 :( length(date_range)-windowLength+1 )], name = strategy_string,line = list(mode = "lines"))%&gt;%
      layout(title = paste("Cumulative return for",core_string, "with window length=",toString(windowLength),sep=" "))
    
    
}


## Minimum Variance Portfolio function ####
getMinVariancePortfolio &lt;- function(mu,covMat,assetSymbols) {
  U &lt;- rep(1, length(mu)) 
  O &lt;- solve(covMat)     
  w &lt;- O%*%U /as.numeric(t(U)%*%O%*% U)
  Risk &lt;- sqrt(t(w) %*% covMat %*% w)
  ExpReturn &lt;- t(w) %*% mu
  Weights &lt;- `names&lt;-`(round(w, 5), assetSymbols)
  list(Weights = t(Weights),
       ExpReturn = round(as.numeric(ExpReturn), 5),
       Risk = round(as.numeric(Risk), 5))
}



### Minium_Variance_portfolio Function

Minium_Variance_portfolio = function(data_cryptos,l,core,tau){
  
  Wealth = c(1,rep(0,length(data_cryptos[,1])-l+1))
  
  for (i in l : length(data_cryptos[,1])){
    
    Fn=ecdf(core[(i-l+1):i])
    
    if( tau_spine(Fn(core[i]),tau) != Inf ){
      
      model =rq(formula = core[(i-l+1):i] ~ ., tau = tau_spine(Fn(core[i]),tau), data = data_cryptos[(i-l+1):i,])
      
      assets_beta_negative = model$coefficients[(model$coefficients &lt; 0) &amp; names(model$coefficients)!="(Intercept)"]
      
      selected_assets = data_cryptos[(i-l+1):i,names(assets_beta_negative)]
      
      cov_selected_assets = cov(selected_assets,selected_assets)
      
      mu = colMeans(selected_assets)
      
      if(!is.null(assets_beta_negative)){
        
        weight = getMinVariancePortfolio(mu,cov_selected_assets,colnames(cov_selected_assets))
        
        weight = as.numeric(weight$Weights)
        
        X_i_plus_1 = data_cryptos[i+1,colnames(cov_selected_assets)]
        
        Wealth[i-l+1 +1] = Wealth[i-l+1] + sum(weight*X_i_plus_1)
        
        
      }
      
      else{
        
        Wealth[i-l+ 2] = Wealth[i-l+1] 
        
      }
      
    }
    
    else  {
      
      
      Wealth[i-l+2] = Wealth[i-l+1] + core[i+1]
      
      
    }
    
  }
  
  return(Wealth[-length(Wealth)])
  
}


##################################                RESULTS            #########################################



Wealth_MV = Minium_Variance_portfolio(data_crypto,window_length,SP500,tau)
Wealth_MV = Minium_Variance_portfolio(data_cryptos,window_length,NASDAQ,tau)
Wealth_MV = Minium_Variance_portfolio(data_cryptos,window_length,FTSE100,tau)
Wealth_MV = Minium_Variance_portfolio(data_cryptos,window_length,DAX_30_PERFORMANCE,tau)
Wealth_MV = Minium_Variance_portfolio(data_cryptos,window_length,NIKKEI_225,tau)



plot_wealth(Wealth_MV,SP500,"Wealth_MV","SP500",Core$date,window_length)
plot_wealth(Wealth_MV,NASDAQ,"Wealth_MV","NASDAQ",Core$date,window_length)
plot_wealth(Wealth_MV,FTSE100,"Wealth_MV","FTSE100",Core$date,window_length)
plot_wealth(Wealth_MV,DAX_30_PERFORMANCE,"Wealth_MV","DAX_30_PERFORMANCE",Core$date,window_length)
plot_wealth(Wealth_MV,NIKKEI_225_STOCK_AVERAGE,"Wealth_MV","NIKKEI_225_STOCK_AVERAGE",Core$date,window_length)
