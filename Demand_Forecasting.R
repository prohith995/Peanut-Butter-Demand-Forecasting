
# Total Demand Estimation

dT = read.csv("Demand-Total-Month.csv")[-1]
dT
pT = dT$AvgPrice
pT

par(mfrow = c(1,1))
plot(pT, dT$Dtotal)

price_demand = lm(dT$Dtotal[61:120]~pT[61:120])
summary(price_demand)

# ?????
dTotalTS <- ts(log(dT$Dtotal[1:131]), start=2001, frequency=12)
dTotalTS
plot(dTotalTS)
par(mfrow = c(1,2))
acf(dTotalTS, main='ACF')
pacf(dTotalTS, main='PACF')


pT = ts(pT, start=2001, frequency=12)

par(mfrow = c(1,2))
acf(pT, main='ACF')
pacf(pT, main='PACF')

model_price = auto.arima(pT)
model_price = arima(pT, order = c(3,1,0), seasonal = c(2,0,1))
summary(model_price)
checkresiduals(model_price)
fPrice = forecast(model_price, h=13, level=c(80,95))
autoplot(fPrice)

price_predict = data.frame(fPrice$mean)

model1 = auto.arima(dTotalTS, xreg=pT[1:131])
#model1 = arima(dTotalTS, order = c(0,1,1), seasonal = c(2,0,0), xreg=pT[1:131])
summary(model1)
checkresiduals(model1)

fDemand = forecast(model1, h=13, level=c(80,95), xreg = price_predict)
fDemand
autoplot(fDemand)


# Estimating the effect of price on the demand on Peanut Butter

# First, price is forecasted as the price data is not available for all years
dT_Uni = read.csv("Demand-Total-Month-Uni.csv")[-1]

pT_Uni = dT_Uni$AvgPrice
pT_Uni = ts(pT_Uni[1:131], start=2001, frequency=12)

par(mfrow = c(1,2))
acf(pT_Uni, main='ACF')
pacf(pT_Uni, main='PACF')

model_price_Uni = auto.arima(pT_Uni)
#model_price_Uni = arima(pT_Uni, order = c(3,1,0), seasonal = c(2,0,1))
summary(model_price_Uni)
checkresiduals(model_price_Uni)
fPrice_Uni = forecast(model_price_Uni, h=13, level=c(80,95))
autoplot(fPrice_Uni)

price_predict_Uni = data.frame(fPrice_Uni$mean)

####################
dT_Uni <- ts(log(dT_Uni$Dtotal[1:131]), start=c(2001,1), frequency=12)
dT_Uni
par(mfrow=c(1,1))
plot(dT_Uni, xlim=c(2006,2010))
plot(diff(dTotalTS_Uni))

adf.test(dTotalTS_Uni)
adf.test(diff(dTotalTS_Uni))

par(mfrow = c(1,2))
acf(diff(dTotalTS_Uni), main='ACF')
pacf(diff(dTotalTS_Uni), main='PACF')

model1_Uni = auto.arima(dTotalTS_Uni, xreg = pT_Uni[1:131])
#model1_Uni = arima(dTotalTS_Uni, order=c(0,1,2), seasonal = c(1,0,1))
model1_Uni = arima(dTotalTS_Uni, xreg = pT_Uni[1:131], order=c(4,1,2), seasonal = c(2,1,3))
summary(model1_Uni)
checkresiduals(model1_Uni)

fDemand_Uni = forecast(model1_Uni, h=12, level=c(80,95))
fDemand_Uni
autoplot(fDemand_Uni)

par(mfrow = c(1,2))
plot(fDemand)
plot(fDemand_Uni)

df_price = read.csv("PB_price.csv")
View(df_price)


### Price effect
reg_model = lm(dT_Uni$Dtotal ~ df_price$PGNUTSUSDM)    
summary(reg_model)

cor(df_price$PGNUTSUSDM, dT_Uni$Dtotal)

dynamic_fit = arima(dTotalTS_Uni , xreg = df_price$PGNUTSUSDM, order=c(2,1,0), seasonal = c(1,0,1))
summary(dynamic_fit)
checkresiduals(dynamic_fit)

# Preparing 8-week sales forecast for LA and CH markets
dT_LA = read.csv("Demand-Total-Week-LA.csv")[-1]
#View(dT_LA)

pT_LA = dT_LA$AvgPrice
pT_LA = ts(pT_LA, start=1114)

plot(pT_LA, dT_LA$Dtotal)

par(mfrow = c(1,2))
acf(pT_LA, main='ACF')
pacf(pT_LA, main='PACF')

model_price_LA = auto.arima(pT_LA)
#model_price_LA = arima(pT_LA, order = c(3,1,0), seasonal = c(2,0,1))
summary(model_price_LA)
checkresiduals(model_price_LA)
fPrice_LA = forecast(model_price_LA, h=8, level=c(80,95))
autoplot(fPrice_LA)

price_predict_LA = data.frame(fPrice_LA$mean)

dT_LA <- ts(log(dT_LA$Dtotal), start=1114)
dT_LA

par(mfrow=c(1,1))
plot(dT_LA)
plot(diff(dT_LA))

adf.test(dT_LA)

par(mfrow = c(1,2))
acf(dT_LA, main='ACF')
pacf(dT_LA, main='PACF')


model_LA = auto.arima(dT_LA, xreg=pT_LA)
model_LA = arima(dT_LA, order=c(2,1,3), seasonal = c(1,0,0))
summary(model_LA)
checkresiduals(model_LA)

fDemand_LA = forecast(model_LA, h=8, xreg = price_predict_LA, level=c(80,95))
fDemand_LA
autoplot(fDemand_LA)

#df_price = read.csv("PB_price.csv")
#View(df_price)

dT_CH = read.csv("Demand-Total-Week-CH.csv")[-1]
#View(dT_CH)

pT_CH = dT_CH$AvgPrice

plot(pT_CH, dT_CH$Dtotal)

pT_CH = ts(pT_CH, start=1114)
pT_CH
par(mfrow = c(1,2))
acf(pT_CH, main='ACF')
pacf(pT_CH, main='PACF')

model_price_CH = auto.arima(pT_CH)
#model_price_CH = arima(pT_CH, order = c(3,1,0), seasonal = c(2,0,1))
summary(model_price_CH)
checkresiduals(model_price_CH)
fPrice_CH = forecast(model_price_CH, h=8, level=c(80,95))
autoplot(fPrice_CH)

price_predict_CH = data.frame(fPrice_CH$mean)
price_predict_CH
dT_CH <- ts(log(dT_CH$Dtotal), start=1114)
dT_CH

par(mfrow=c(1,1))
plot(dT_CH)
plot(diff(dT_CH))

adf.test(dT_CH)

par(mfrow = c(1,2))
acf(dT_CH, main='ACF')
pacf(dT_CH, main='PACF')


model_CH = auto.arima(dT_CH, xreg=pT_CH)
#model_CH = arima(dT_CH, order=c(2,1,3), seasonal = c(1,0,0))
summary(model_CH)
checkresiduals(model_CH)

fDemand_CH = forecast(model_CH, h=8, xreg = price_predict_CH, level=c(80,95))
fDemand_CH
autoplot(fDemand_CH)

