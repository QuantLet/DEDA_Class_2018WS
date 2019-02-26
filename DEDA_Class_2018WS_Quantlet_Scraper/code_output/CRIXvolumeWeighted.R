# clear all variables
rm(list = ls(all = TRUE))
graphics.off()

# install and load packages
libraries = c("quadprog", "quantreg", "plotly","dplyr")
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


### Plot function
plot_wealth = function(strategy_wealth,core,strategy_string,core_string,date_range,windowLength){
    
    core_wealth = cumsum(c(1,core[windowLength:(length(core))]))
    core_wealth  = core_wealth[-length(core_wealth)]
    
    plot_ly(x = date_range[windowLength: length(date_range)]) %&gt;%
      add_lines(y = core_wealth[1 : (length(date_range)-windowLength+1) ], name = paste("Core ",core_string),mode="lines")%&gt;%
      add_lines(y = strategy_wealth[1 :( length(date_range)-windowLength+1 )], name = strategy_string,line = list(mode = "lines"))%&gt;%
      layout(title = paste("Cumulative return for",core_string, "with window length=",toString(windowLength),sep=" "))
    
}

  
### Creation of the Volume table


#### Initial data with the cryptos and market cap
data_cg_daily = readRDS("data_cg_daily.rds")

date_range = rownames(data_cryptos)
cryptos=colnames(data_cryptos)


data_cryptos_marketcap=filter(data_cg_daily,data_cg_daily$crypto_symbol %in% cryptos)
data_cryptos_marketcap=filter(data_cryptos_marketcap,as.Date(data_cryptos_marketcap$date) %in% as.Date(date_range))
data_cryptos_marketcap$total_volume_usd

data_cryptos_marketcap=data_cryptos_marketcap[,c("crypto_symbol","date","total_volume_usd")]
data_cryptos_marketcap=spread(data_cryptos_marketcap,crypto_symbol,total_volume_usd)
rownames(data_cryptos_marketcap)=data_cryptos_marketcap$date
data_cryptos_marketcap=data_cryptos_marketcap[,-1]




data_cryptos_marketcap = data.frame(apply(data_cryptos_marketcap, 2, function(x) as.numeric(x)))

#######################################################################################################

VOLUME_WEIGHTED_ALLOCATION = function(data_cryptos,l,SP500,cyptos_volume,tau){
  
  Wealth = c(1,rep(0,length(data_cryptos[,1])-l+1))
  
  for (i in l : length(data_cryptos[,1])){
    
    Fn=ecdf(SP500[(i-l+1):i])
    
    if( tau_spine(Fn(SP500[i]),tau) != Inf ){
      
      model =rq(formula = SP500[(i-l+1):i] ~ ., tau = tau_spine(Fn(SP500[i]),tau), data = data_cryptos[(i-l+1):i,])
      
      assets_beta_negative = model$coefficients[model$coefficients &lt; 0]
      
      selected_assets = data_cryptos[(i-l+1):i,names(assets_beta_negative)[2:length(names(assets_beta_negative))]]
      
      
      
      if(!is.null(assets_beta_negative)){
        
        
        
        weight=cyptos_volume[i,names(selected_assets)]/sum(cyptos_volume[i,names(selected_assets)])
        
        X_i_plus_1 = data_cryptos[i+1,names(selected_assets)]
        
        
        Wealth[i-l+1 +1] = Wealth[i-l+1] + sum(weight*X_i_plus_1)
        
      }
      
      else{
        
        Wealth[i-l+ 2] = Wealth[i-l+1] 
        
      }
      
    }
    
    else  {
      
      
      Wealth[i-l+2] = Wealth[i-l+1] + SP500[i+1]
      
      
    }
    
  }
  
  return(Wealth)
  
}


##################################                RESULTS            #########################################



Wealth_VOLUME_WEIGHTED = VOLUME_WEIGHTED_ALLOCATION(data_cryptos,window_length,SP500,data_cryptos_marketcap,tau)
Wealth_VOLUME_WEIGHTED = VOLUME_WEIGHTED_ALLOCATION(data_cryptos,window_length,FTSE100,data_cryptos_marketcap,tau)
Wealth_VOLUME_WEIGHTED = VOLUME_WEIGHTED_ALLOCATION(data_cryptos,window_length,SP400_TECH_DISTRIBUTRS,data_cryptos_marketcap,tau)
Wealth_VOLUME_WEIGHTED = VOLUME_WEIGHTED_ALLOCATION(data_cryptos,window_length,SP_EURO_INFO_TECH,data_cryptos_marketcap,tau)


plot_wealth(Wealth_VOLUME_WEIGHTED,SP500,"Wealth_VOLUME_WEIGHTED","SP500",Core$date,window_length)
plot_wealth(Wealth_VOLUME_WEIGHTED,SP500,"Wealth_VOLUME_WEIGHTED","NASDAQ",Core$date,window_length)
plot_wealth(Wealth_VOLUME_WEIGHTED,FTSE100,"Wealth_VOLUME_WEIGHTED","FTSE100",Core$date,window_length)
plot_wealth(Wealth_VOLUME_WEIGHTED,DAX_30_PERFORMANCE,"Wealth_VOLUME_WEIGHTED","DAX_30_PERFORMANCE",Core$date,window_length)
plot_wealth(Wealth_VOLUME_WEIGHTED,NIKKEI_225,"Wealth_VOLUME_WEIGHTED","NIKKEI_225",Core$date,window_length)
