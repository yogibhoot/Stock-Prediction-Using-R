# Load library
install.packages("quantmod")
library(quantmod)

#fetch dataset from online by doing getsymbol function
getSymbols("^GSPC", from=as.Date("2009-01-01"), to=as.Date("2020-12-31"), periodicity="daily")

# display dataset
View(GSPC)

#create time series by fetching adj_value with time
price_adj = GSPC$GSPC.Adjusted

# display time series and generate graph for more understand which has a trend or seasonality in series
View(price_adj)
plot(price_adj, main = "S&P500: Price Vs Year")

# now, perfome log return logic
# first we do difference. here we are able to doing difference because time series is daily basis.
# difference series gives return value which is store in ret object
# t_diff = (t) - (t-1)
#log = (t)/(t-1)
retn = na.omit(diff(log(price_adj)))
View(retn)
retn
#plot(ret, main = "Log Return Series")

# ret has NA value so we remove those values 
#retn = ret[!is.na(ret)]
#View(retn)
#retn

# generate plot for more easily understand trend

plot(retn, main = "S&P500: Return Value", lty = "solid")

summary(retn)
# as per summary max is almost 0.1 as well as mean is 0 and min is negative value

#now we have to find p value using ADF test
# first we need to load tseries library
install.packages("tseries")
library(tseries)

adf.test(retn)
# p = 0.01 so alternative hypo is stationary

acf(retn, main = "Autocorrelation for S&P500")
pacf(retn, main = "Partial Autocorrelation for S&P500")
#we use AR model because PACF has low significant compare to ACF significant.

#again we perfome one test which called Ljung-Box test for calculate [p value]
Box.test(retn, type = "Ljung-Box")
# Box.test(retn, lag = 10, type = "Ljung-Box")


# checking our data is stationary or not??, so, we use alternative method for testing data. 
adf.test(retn, alternative = c("stationary"))
adf.test(retn, alternative = c("explosive"))


#here, we test normality test on out return series
shapiro.test(as.vector(retn))

install.packages("forecast")
install.packages("fpp2")
library(forecast)
library(fpp2)

armodel = auto.arima(retn, trace = TRUE)
armodel
confint(armodel,level = 0.98)

model = arima(retn, order = c(0,4,0))
model
summary(model)
summary(armodel)
armodel$coef

checkresiduals(armodel)

mean(armodel$residuals)
acf(armodel$residuals)


Box.test(armodel$residuals, lag = 320, type = "Ljung-Box")

forecasting <- forecast(armodel, h =10)
plot(forecasting)
