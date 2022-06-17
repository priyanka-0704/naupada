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

jan_70 <-temp1[1:10417 , 1]  ## 70% data extracting for training

View(jan_70)

## forecast for 70% training data

## seasonal naive model 

fit_jan70_naive <- snaive(jan_70)

print(summary(fit_jan70_naive))

checkresiduals(fit_jan70_naive)

forecast_jan70_naive = forecast(fit_jan70_naive)  ##forecast model 

forecast_jan70_naive

plot(forecast_jan70_naive)

jan_70 <- ts(temp1$T2M[1:10417])

View(jan_70)

## exponential smoothing model

fit_jan70_exp <- ets(jan_70)

print(summary(fit_jan70_exp))

checkresiduals(fit_jan70_exp)

forecast_jan70_exp = forecast(fit_jan70_exp) ## forecast model

forecast_jan70_exp

plot(forecast_jan70_exp)

## ARIMA model

fit_jan70_arima <- auto.arima(jan_70)

print(summary(fit_jan70_arima))

checkresiduals(fit_jan70_arima)

forecast_jan70_arima = forecast(fit_jan70_arima) ## forecast model

forecast_jan70_arima

plot(forecast_jan70_arima)

jan_80 <-temp1[1:11905 , 1]  ## 80% data extracting for training

View(jan_80)

## forecast for 80% training data

## seasonal naive model 

fit_jan80_naive <- snaive(jan_80)

print(summary(fit_jan80_naive))

checkresiduals(fit_jan80_naive)

forecast_jan80_naive = forecast(fit_jan80_naive)  ##forecast model 

forecast_jan80_naive

plot(forecast_jan80_naive)

jan_80 <- ts(temp1$T2M[1:11905])

View(jan_80)

## exponential smoothing model

fit_jan80_exp <- ets(jan_80)

print(summary(fit_jan80_exp))

checkresiduals(fit_jan80_exp)

forecast_jan80_exp = forecast(fit_jan80_exp) ## forecast model

forecast_jan80_exp

plot(forecast_jan80_exp)

## ARIMA model

fit_jan80_arima <- auto.arima(jan_80)

print(summary(fit_jan80_arima))

checkresiduals(fit_jan80_arima)

forecast_jan80_arima = forecast(fit_jan80_arima) ## forecast model

forecast_jan80_arima

plot(forecast_jan80_arima)

jan_90 <-temp1[1:13394 , 1]  ## 90% data extracting for training

View(jan_90)

## forecast for 90% training data

## seasonal naive model 

fit_jan90_naive <- snaive(jan_90)

print(summary(fit_jan90_naive))

checkresiduals(fit_jan90_naive)

forecast_jan90_naive = forecast(fit_jan90_naive)  ##forecast model 

forecast_jan90_naive

plot(forecast_jan90_naive)

jan_90 <- ts(temp1$T2M[1:13394])

View(jan_90)

## exponential smoothing model

fit_jan90_exp <- ets(jan_90)

print(summary(fit_jan90_exp))

checkresiduals(fit_jan90_exp)

forecast_jan90_exp = forecast(fit_jan90_exp) ## forecast model

forecast_jan90_exp

plot(forecast_jan90_exp)

## ARIMA model

fit_jan90_arima <- auto.arima(jan_90)

print(summary(fit_jan90_arima))

checkresiduals(fit_jan90_arima)

forecast_jan80_arima = forecast(fit_jan80_arima) ## forecast model

forecast_jan80_arima

plot(forecast_jan80_arima)

##for whole data set

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


feb_70 <-temp_feb[1:9476 , 1]  ## 70% data extracting for training

View(feb_70)

## forecast for 70% training data

## seasonal naive model 

fit_feb70_naive <- snaive(feb_70)

print(summary(fit_feb70_naive))

checkresiduals(fit_feb70_naive)

forecast_feb70_naive = forecast(fit_feb70_naive)  ##forecast model 

forecast_feb70_naive

plot(forecast_feb70_naive)

feb_70 <- ts(temp_feb[1:9476])

View(feb_70)

## exponential smoothing model

fit_feb70_exp <- ets(feb_70)

print(summary(fit_feb70_exp))

checkresiduals(fit_feb70_exp)

forecast_feb70_exp = forecast(fit_feb70_exp) ## forecast model

forecast_feb70_exp

plot(forecast_feb70_exp)

## ARIMA model

fit_feb70_arima <- auto.arima(feb_70)

print(summary(fit_feb70_arima))

checkresiduals(fit_feb70_arima)

forecast_feb70_arima = forecast(fit_feb70_arima) ## forecast model

forecast_feb70_arima

plot(forecast_feb70_arima)

feb_80 <-temp_feb[1:10843 , 1]  ## 80% data extracting for training

View(feb_80)

## forecast for 80% training data

## seasonal naive model 

fit_feb80_naive <- snaive(feb_80)

print(summary(fit_feb80_naive))

checkresiduals(fit_feb80_naive)

forecast_feb80_naive = forecast(fit_feb80_naive)  ##forecast model 

forecast_feb80_naive

plot(forecast_feb80_naive)

feb_80 <- ts(temp_feb[1:10843])

View(feb_80)

## exponential smoothing model

fit_feb80_exp <- ets(feb_80)

print(summary(fit_feb80_exp))

checkresiduals(fit_feb80_exp)

forecast_feb80_exp = forecast(fit_feb80_exp) ## forecast model

forecast_feb80_exp

plot(forecast_feb80_exp)

## ARIMA model

fit_feb80_arima <- auto.arima(feb_80)

print(summary(fit_feb80_arima))

checkresiduals(fit_feb80_arima)

forecast_feb80_arima = forecast(fit_feb80_arima) ## forecast model

forecast_feb80_arima

plot(forecast_feb80_arima)

feb_90 <-temp_feb[1:12185]  ## 90% data extracting for training

View(feb_90)

## forecast for 90% training data

## seasonal naive model 

fit_feb90_naive <- snaive(feb_90)

print(summary(fit_feb90_naive))

checkresiduals(fit_feb90_naive)

forecast_feb90_naive = forecast(fit_feb90_naive)  ##forecast model 

forecast_feb90_naive

plot(forecast_feb90_naive)

feb_90 <- ts(temp_feb[1:12185])

View(feb_90)

## exponential smoothing model

fit_feb90_exp <- ets(feb_90)

print(summary(fit_feb90_exp))

checkresiduals(fit_feb90_exp)

forecast_feb90_exp = forecast(fit_feb90_exp) ## forecast model

forecast_feb90_exp

plot(forecast_feb90_exp)

## ARIMA model

fit_feb90_arima <- auto.arima(feb_90)

print(summary(fit_feb90_arima))

checkresiduals(fit_feb90_arima)

forecast_feb90_arima = forecast(fit_feb90_arima) ## forecast model

forecast_feb90_arima

plot(forecast_feb90_arima)

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

## RETRIVING ALL ATTRIBUTES OF MARCH

library(readxl)
temp_mar <- read_excel("F:/EXCEL/POWER_Point_Hourly_20010101_20210331_020d1515N_082d5154E_LST(1).xlsx", 
                       sheet = "temp3")
View(temp_mar)

cor(temp_mar)  ## find correlation coefficient of all attributes

cr3 <- cor(temp_mar)

corrplot(cr3)  ## no strong correlation between temperature and all other attributes in the month of march

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


##IMPORT DATA OF THE MONTH APRIL

library(readxl)
april <- read_excel("F:/EXCEL/month jan to oct timeseries.xlsx", 
                       sheet = "apr")
View(april)

## RETRIVING ALL ATTRIBUTES OF APRIL

library(readxl)
temp_apr <- read_excel("F:/EXCEL/POWER_Point_Hourly_20010101_20210331_020d1515N_082d5154E_LST(1).xlsx", 
                       sheet = "temp4")
View(temp_apr)

cor(temp_apr)  ## find correlation coefficient of all attributes

cr4 <- cor(temp_apr)

corrplot(cr4)  ## no strong correlation between temperature and all other attributes in the month of april

TEMP_APR <- ts(april$Temperature, frequency = 24) 

View(TEMP_APR)


plot.ts(TEMP_APR)

adf.test(TEMP_APR)   ## check stationarity

## seasonal naive model 


fit_apr_naive <- snaive(TEMP_APR)

print(summary(fit_apr_naive))

checkresiduals(fit_apr_naive)

forecast_apr_naive = forecast(fit_apr_naive)  ##forecast model 

forecast_apr_naive

plot(forecast_apr_naive)

## exponential smoothing model


fit_apr_exp <- ets(TEMP_APR)

print(summary(fit_apr_exp))

checkresiduals(fit_apr_exp)

forecast_apr_exp = forecast(fit_apr_exp) ## forecast model

forecast_apr_exp

plot(forecast_apr_exp)


## ARIMA model

fit_apr_arima <- auto.arima(TEMP_APR)

print(summary(fit_apr_arima))

checkresiduals(fit_apr_arima)

forecast_apr_arima = forecast(fit_apr_arima) ## forecast model

forecast_apr_arima

plot(forecast_apr_arima)



##IMPORT DATA OF THE MONTH MAY

library(readxl)
may <- read_excel("F:/EXCEL/month jan to oct timeseries.xlsx", 
                    sheet = "may")
View(may)

## RETRIVING ALL ATTRIBUTES OF MAY

library(readxl)
temp_may <- read_excel("F:/EXCEL/POWER_Point_Hourly_20010101_20210331_020d1515N_082d5154E_LST(1).xlsx", 
                       sheet = "temp5")
View(temp_may)

cor(temp_may)  ## find correlation coefficient of all attributes

cr5 <- cor(temp_may)

corrplot(cr5)  ## no strong correlation between temperature and all other attributes in the month of may

TEMP_MAY <- ts(may$Temperature, frequency = 24) 

View(TEMP_MAY)


plot.ts(TEMP_MAY)

adf.test(TEMP_MAY)   ## check stationarity

## seasonal naive model 


fit_may_naive <- snaive(TEMP_MAY)

print(summary(fit_may_naive))

checkresiduals(fit_may_naive)

forecast_may_naive = forecast(fit_may_naive)  ##forecast model 

forecast_may_naive

plot(forecast_may_naive)

## exponential smoothing model


fit_may_exp <- ets(TEMP_MAY)

print(summary(fit_may_exp))

checkresiduals(fit_may_exp)

forecast_may_exp = forecast(fit_may_exp) ## forecast model

forecast_may_exp

plot(forecast_may_exp)


## ARIMA model

fit_may_arima <- auto.arima(TEMP_MAY)

print(summary(fit_may_arima))

checkresiduals(fit_may_arima)

forecast_may_arima = forecast(fit_may_arima) ## forecast model

forecast_may_arima

plot(forecast_may_arima)


##IMPORT DATA OF THE MONTH JUNE

library(readxl)
june <- read_excel("F:/EXCEL/month jan to oct timeseries.xlsx", 
                  sheet = "jun")
View(june)

## RETRIVING ALL ATTRIBUTES OF JUNE

library(readxl)
temp_jun <- read_excel("F:/EXCEL/POWER_Point_Hourly_20010101_20210331_020d1515N_082d5154E_LST(1).xlsx", 
                       sheet = "temp6")
View(temp_jun)

cor(temp_jun)  ## find correlation coefficient of all attributes

cr6 <- cor(temp_jun)

corrplot(cr6)  ## no strong correlation between temperature and all other attributes in the month of june

TEMP_JUN <- ts(june$Temperature, frequency = 24) 

View(TEMP_JUN)


plot.ts(TEMP_JUN)

adf.test(TEMP_JUN)   ## check stationarity

## seasonal naive model 


fit_jun_naive <- snaive(TEMP_JUN)

print(summary(fit_jun_naive))

checkresiduals(fit_jun_naive)

forecast_jun_naive = forecast(fit_jun_naive)  ##forecast model 

forecast_jun_naive

plot(forecast_jun_naive)

## exponential smoothing model


fit_jun_exp <- ets(TEMP_JUN)

print(summary(fit_jun_exp))

checkresiduals(fit_jun_exp)

forecast_jun_exp = forecast(fit_jun_exp) ## forecast model

forecast_jun_exp

plot(forecast_jun_exp)


## ARIMA model

fit_jun_arima <- auto.arima(TEMP_JUN)

print(summary(fit_jun_arima))

checkresiduals(fit_jun_arima)

forecast_jun_arima = forecast(fit_jun_arima) ## forecast model

forecast_jun_arima

plot(forecast_jun_arima)


##IMPORT DATA OF THE MONTH JULY

library(readxl)
july <- read_excel("F:/EXCEL/month jan to oct timeseries.xlsx", 
                   sheet = "jul")
View(july)

## RETRIVING ALL ATTRIBUTES OF JULY

library(readxl)
temp_jul <- read_excel("F:/EXCEL/POWER_Point_Hourly_20010101_20210331_020d1515N_082d5154E_LST(1).xlsx", 
                       sheet = "temp7")
View(temp_jul)

cor(temp_jul)  ## find correlation coefficient of all attributes

cr7 <- cor(temp_jul)

corrplot(cr7)  ## no strong correlation between temperature and all other attributes in the month of july

TEMP_JUL <- ts(july$Temperature, frequency = 24) 

View(TEMP_JUL)


plot.ts(TEMP_JUL)

adf.test(TEMP_JUL)   ## check stationarity

## seasonal naive model 


fit_jul_naive <- snaive(TEMP_JUL)

print(summary(fit_jul_naive))

checkresiduals(fit_jul_naive)

forecast_jul_naive = forecast(fit_jul_naive)  ##forecast model 

forecast_jul_naive

plot(forecast_jul_naive)

## exponential smoothing model


fit_jul_exp <- ets(TEMP_JUL)

print(summary(fit_jul_exp))

checkresiduals(fit_jul_exp)

forecast_jul_exp = forecast(fit_jul_exp) ## forecast model

forecast_jul_exp

plot(forecast_jul_exp)


## ARIMA model

fit_jul_arima <- auto.arima(TEMP_JUL)

print(summary(fit_jul_arima))

checkresiduals(fit_jul_arima)

forecast_jul_arima = forecast(fit_jul_arima) ## forecast model

forecast_jul_arima

plot(forecast_jul_arima)
