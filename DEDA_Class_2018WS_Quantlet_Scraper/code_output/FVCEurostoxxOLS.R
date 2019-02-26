#set working directory
#setwd(.....)

#read data
data = read.csv2('emo_and_stock_per_pc.csv')


#different models, results displayed in Table 3
fit1 = lm(ret_d0 ~ ret_AR1, data = data)
summary(fit1)

fit2 = lm(ret_d0 ~ ret_AR1 + anger, data = data)
summary(fit2)

fit3 = lm(ret_d0 ~ ret_AR1 + anger + contempt, data = data)
summary(fit3)

fit4 = lm(ret_d0 ~ ret_AR1 + anger + contempt + disgust, data = data)
summary(fit4)

fit5 = lm(ret_d0 ~ ret_AR1 + anger + contempt + disgust + fear, data = data)
summary(fit5)

fit6 = lm(ret_d0 ~ ret_AR1 + anger + contempt + disgust + fear + happiness, data = data)
summary(fit6)

fit7 = lm(ret_d0 ~ ret_AR1 + anger + contempt + disgust + fear + happiness + sadness, data = data)
summary(fit7)

fit8 = lm(ret_d0 ~ ret_AR1 + anger + contempt + disgust + fear + happiness + sadness + surprise, data = data)
summary(fit8)

fit9 = lm(ret_d0 ~ ret_AR1 + neutral, data = data)
summary(fit9)
