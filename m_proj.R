##  IMPORT WHOLE DATASET OF NAUPADA

library(readxl)
naupada <- read_excel("F:/EXCEL/POWER_Point_Hourly_20010101_20210331_020d1515N_082d5154E_LST(1).xlsx", 
                      sheet = "FULL DATA SET")
View(naupada)

## RETRIVING ALL ATTRIBUTES OF THE DATASET

library(readxl)
all <- read_excel("F:/EXCEL/POWER_Point_Hourly_20010101_20210331_020d1515N_082d5154E_LST(1).xlsx", 
                  sheet = "all")
View(all)

plot(all)  ## plot all attributes


library(corrplot) ## loading library corrplot


cor(all)  ## find correlation coefficient of all attributes

cr <- cor(all)

corrplot(cr)  ## no strong correlation between temperature and all other attributes


##IMPORT DATA OF THE MONTH JANUARY

library(readxl)
january <- read_excel("F:/EXCEL/month jan to oct timeseries.xlsx", 
                      sheet = "jan")
View(january)

## RETRIVING ALL ATTRIBUTES OF JANUARY

library(readxl)
temp1 <- read_excel("F:/EXCEL/POWER_Point_Hourly_20010101_20210331_020d1515N_082d5154E_LST(1).xlsx", 
                    sheet = "temp")
View(temp1)

cor(temp1)  ## find correlation coefficient of all attributes

cr1 <- cor(temp1)

corrplot(cr1)  ## no strong correlation between temperature and all other attributes in the month of january

TEMP_JAN <- ts(january$T2M, frequency = 24) 

View(TEMP_JAN)


plot.ts(TEMP_JAN)

library(tseries)  ##loading library tseries

library(fpp2)

adf.test(TEMP_JAN)  ## stationarity checking

library(tidyverse)

## seasonal naive model 


fit_jan_naive <- snaive(TEMP_JAN)

print(summary(fit_jan_naive))

checkresiduals(fit_jan_naive)

forecast_jan_naive = forecast(fit_jan_naive)  ##forecast model 

forecast_jan_naive

plot(forecast_jan_naive)

## exponential smoothing model


fit_jan_exp <- ets(TEMP_JAN)

print(summary(fit_jan_exp))

checkresiduals(fit_jan_exp)

forecast_jan_exp = forecast(fit_jan_exp) ## forecast model

forecast_jan_exp

plot(forecast_jan_exp)


## ARIMA model

fit_jan_arima <- auto.arima(TEMP_JAN)

print(summary(fit_jan_arima))

accuracy(fit_jan_arima)

checkresiduals(fit_jan_arima)

forecast_jan_arima = forecast(fit_jan_arima) ## forecast model

forecast_jan_arima

plot(forecast_jan_arima)

##IMPORT DATA OF THE MONTH FEBRUARY

library(readxl)
february <- read_excel("F:/EXCEL/month jan to oct timeseries.xlsx", 
                      sheet = "feb")
View(february)

## RETRIVING ALL ATTRIBUTES OF FEBRUARY

library(readxl)
temp_feb <- read_excel("F:/EXCEL/POWER_Point_Hourly_20010101_20210331_020d1515N_082d5154E_LST(1).xlsx", 
                    sheet = "temp2")
View(temp_feb)

cor(temp_feb)  ## find correlation coefficient of all attributes

cr2 <- cor(temp_feb)

corrplot(cr2)  ## no strong correlation between temperature and all other attributes in the month of february

TEMP_FEB <- ts(february$Temperature, frequency = 24) 

View(TEMP_FEB)


plot.ts(TEMP_FEB)

adf.test(TEMP_FEB)  ## stationarity checking

## seasonal naive model 


fit_feb_naive <- snaive(TEMP_FEB)

print(summary(fit_feb_naive))

checkresiduals(fit_feb_naive)

forecast_feb_naive = forecast(fit_feb_naive)  ##forecast model 

forecast_feb_naive

plot(forecast_feb_naive)

## exponential smoothing model


fit_feb_exp <- ets(TEMP_FEB)

print(summary(fit_feb_exp))

checkresiduals(fit_feb_exp)

forecast_feb_exp = forecast(fit_feb_exp) ## forecast model

forecast_feb_exp

plot(forecast_feb_exp)


## ARIMA model

fit_feb_arima <- auto.arima(TEMP_FEB)

print(summary(fit_feb_arima))

checkresiduals(fit_feb_arima)

forecast_feb_arima = forecast(fit_feb_arima) ## forecast model

forecast_feb_arima

plot(forecast_feb_arima)

##IMPORT DATA OF THE MONTH MARCH

library(readxl)
march <- read_excel("F:/EXCEL/month jan to oct timeseries.xlsx", 
                       sheet = "mar")
View(march)

## RETRIVING ALL ATTRIBUTES OF FEBRUARY

library(readxl)
temp_mar <- read_excel("F:/EXCEL/POWER_Point_Hourly_20010101_20210331_020d1515N_082d5154E_LST(1).xlsx", 
                       sheet = "temp3")
View(temp_mar)

cor(temp_mar)  ## find correlation coefficient of all attributes

cr3 <- cor(temp_mar)

corrplot(cr3)  ## no strong correlation between temperature and all other attributes in the month of february

TEMP_MAR <- ts(march$Temperature, frequency = 24) 

View(TEMP_MAR)


plot.ts(TEMP_MAR)

adf.test(TEMP_MAR)  ## stationarity checking

## seasonal naive model 


fit_mar_naive <- snaive(TEMP_MAR)

print(summary(fit_mar_naive))

checkresiduals(fit_mar_naive)

forecast_mar_naive = forecast(fit_mar_naive)  ##forecast model 

forecast_mar_naive

plot(forecast_mar_naive)

## exponential smoothing model


fit_mar_exp <- ets(TEMP_MAR)

print(summary(fit_mar_exp))

checkresiduals(fit_mar_exp)

forecast_mar_exp = forecast(fit_mar_exp) ## forecast model

forecast_mar_exp

plot(forecast_mar_exp)


## ARIMA model

fit_mar_arima <- auto.arima(TEMP_MAR)

print(summary(fit_mar_arima))

checkresiduals(fit_mar_arima)

forecast_mar_arima = forecast(fit_mar_arima) ## forecast model

forecast_mar_arima

plot(forecast_mar_arima)



