#install required packages if necessary
pkg = c('plsdepot')
lapply(pkg, require, character.only = TRUE)
lapply(pkg, library, character.only = TRUE)

#path where dataset is 
dir_data = '...'

#load data
avg_emo = read.csv2(dir_data) 

#sort data, that y-variable is in last position 
emodata = avg_emo[,c(2:9,11)]

#estimate model
pls_fit = plsreg1(emodata[,1:8], emodata[,9], comps = 4, crosval = TRUE)

#loads for each variable in components
pls_fit$x.loads

#R2-fit of model
pls_fit$R2
