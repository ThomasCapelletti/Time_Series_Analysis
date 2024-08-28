# JAPAN

rm(list=ls())
setwd("C:/Users/thoma/Desktop/Progetto")
getwd()

library(readxl)
library(tidyverse)
library(tseries)
library(dynlm)
library(lmtest)
library(sandwich)
library(car)
library(AER)
library(lmtest)
library(car)
library(stargazer)
library(ggplot2)

# Load the data
Japan <-read.csv("Japan.csv",header =TRUE, sep=";")
View(Japan)
str(Japan)

colnames(Japan)<- c("Year", "Real GDP US$", "Real GDP rate of growth",
                     "GDP rate of variation",	"Rate of growth as log difference",
                     "Rate of variation as the second difference",
                     "Government Debt to GDP US$","DIFF Log Governement Debt to GDP",
                     "Rate of variation as the second difference Gov Debt", 
                    "Rate of variation as the third difference Gov Debt",
                    "Private Debt to GDP US$",	"DIFF Log Private Debt to GDP",
                    "Rate of variation as the second difference Private Debt",
                    "Rate of variation as the third difference Private Debt"
)

attach(Japan)

#Graphs 
ggplot(data = Japan, aes(x = Year, y = `Real GDP US$`, colour = "type")) + 
  geom_line() +
  ggtitle("Real GDP US$") 
ggplot(data = Japan, aes(x = Year, y = `Government Debt to GDP US$`, colour = "type")) + 
  geom_line() +
  ggtitle("Government Debt to GDP US$") 
ggplot(data = Japan, aes(x = Year, y = `Private Debt to GDP US$`, colour = "type")) + 
  geom_line() +
  ggtitle("Private Debt to GDP US$") 

# Stationarity analysis
# ACF
acf(`Real GDP US$`, main = "ACF for Real GDP") 
acf(`Government Debt to GDP US$`, main = "ACF for Government Debt to GDP")
acf(`Private Debt to GDP US$`, main = "ACF for Private Debt to GDP")

#Ljung-Box test (p-value > 0.05)
Box.test(`Real GDP US$`, type = "Ljung-Box") 
Box.test(`Government Debt to GDP US$`, type = "Ljung-Box")
Box.test(`Private Debt to GDP US$`, type = "Ljung-Box")

#ADF test (k = 3 default)
adf.test(`Real GDP US$`)
adf.test(`Government Debt to GDP US$`)
adf.test(`Private Debt to GDP US$`)

#PP test
pp.test(`Real GDP US$`)
pp.test(`Government Debt to GDP US$`)
pp.test(`Private Debt to GDP US$`)


#make the data stationary 

#we prefere to compute the differences on excel
#for Real GDP we use this formulation (Yt0 - Yt-1)/Yt-1

# ACF
acf(`Real GDP rate of growth`, main = "ACF for Real GDP rate of growth") 
acf(`GDP rate of variation`, main = "ACF for GDP rate of variation") 
acf(`Rate of growth as log difference`, main = "Rate of growth as a log difference")
acf(`Rate of variation as the second difference`, main ="Rate of variation as the second difference")

acf(`DIFF Log Governement Debt to GDP`, main = "ACF for Government Debt to GDP")
acf(`Rate of variation as the second difference Gov Debt`, main = "Rate of variation as the second difference Gov Debt")
acf(`Rate of variation as the third difference Gov Debt`)

acf(`DIFF Log Private Debt to GDP`, main = "ACF for Private Debt to GDP")
acf(`Rate of variation as the second difference Private Debt`)
acf(`Rate of variation as the third difference Private Debt`)


#Ljung-Box test
Box.test(`Real GDP rate of growth`, type = "Ljung-Box") #no
Box.test(`GDP rate of variation`, type = "Ljung-Box") #no
Box.test(`Rate of growth as log difference`, type = "Ljung-Box") #no
Box.test(`Rate of variation as the second difference`, type = "Ljung-Box") #no

Box.test(`DIFF Log Governement Debt to GDP`, type = "Ljung-Box") #no
Box.test(`Rate of variation as the second difference Gov Debt`, type = "Ljung-Box") #yes
Box.test(`Rate of variation as the third difference Gov Debt`) #no

Box.test(`DIFF Log Private Debt to GDP`, type = "Ljung-Box") #no
Box.test(`Rate of variation as the second difference Private Debt`, type = "Ljung-Box") #no
Box.test(`Rate of variation as the third difference Private Debt`, type = "Ljung-Box") #no

#ADF test
adf.test(`Real GDP rate of growth`) #stationary
adf.test(`GDP rate of variation`) #stationary 
adf.test(`Rate of growth as log difference`) #not stationary
adf.test(`Rate of variation as the second difference`) #stationary

adf.test(`DIFF Log Governement Debt to GDP`) #not stationary
adf.test(`Rate of variation as the second difference Gov Debt`) #stationary
adf.test(`Rate of variation as the third difference Gov Debt`) #stationary

adf.test(`DIFF Log Private Debt to GDP`) #not stationary
adf.test(`Rate of variation as the second difference Private Debt`) #not stationary per poco
adf.test(`Rate of variation as the third difference Private Debt`) #stationary

#PP test
pp.test(`Real GDP rate of growth`) #stationary
pp.test(`GDP rate of variation`) #stationary
pp.test(`Rate of growth as log difference`) #stationary
pp.test(`Rate of variation as the second difference`) #stationary

pp.test(`DIFF Log Governement Debt to GDP`) #not stationary
pp.test(`Rate of variation as the second difference Gov Debt`) #stationary
pp.test(`Rate of variation as the third difference Gov Debt`) #stationary

pp.test(`DIFF Log Private Debt to GDP`) #stationary
pp.test(`Rate of variation as the second difference Private Debt`) #stationary
pp.test(`Rate of variation as the third difference Private Debt`) #stationary

#Graphs 
ggplot(data = Japan, aes(x = Year, y = `GDP rate of variation`, colour = "type")) + 
  geom_line() +
  ggtitle("GDP rate of variation") 
ggplot(data = Japan, aes(x = Year, y = `Rate of variation as the second difference Gov Debt`, colour = "type")) + 
  geom_line() +
  ggtitle("Rate of variation as the second difference Government Debt ") 
ggplot(data = Japan, aes(x = Year, y = `Rate of variation as the second difference Private Debt`, colour = "type")) + 
  geom_line() +
  ggtitle("Rate of variation as the second difference Private Debt ") 



#Make the model
#Model
Japan_ts = Japan %>% 
  dplyr::select(c("GDP rate of variation", "Rate of variation as the second difference Gov Debt", "Rate of variation as the second difference Private Debt")) %>% 
  mutate( `GDP rate of variation`= as.ts(`GDP rate of variation`)) %>% 
  mutate( `Rate of variation as the second difference Gov Debt`= as.ts(`Rate of variation as the second difference Gov Debt`))%>%
  mutate( `Rate of variation as the second difference Private Debt`= as.ts(`Rate of variation as the second difference Private Debt`));
View(Japan_ts)

#Model
VARselect(Japan[,c(4,9,13)], lag.max = 8)

VAReq1 = dynlm(data = Japan_ts, formula = `GDP rate of variation`  ~
                 L(`GDP rate of variation`, 1:8) + L(`Rate of variation as the second difference Gov Debt`, 1:8) + L(`Rate of variation as the second difference Private Debt`, 1:8))
VAReq2 = dynlm(data = Japan_ts, formula = `Rate of variation as the second difference Gov Debt` ~
                 L(`GDP rate of variation`, 1:8) + L(`Rate of variation as the second difference Gov Debt`, 1:8) + L(`Rate of variation as the second difference Private Debt`, 1:8))
VAReq3 = dynlm(data = Japan_ts, formula = `Rate of variation as the second difference Private Debt`  ~
                 L(`GDP rate of variation`, 1:8) + L(`Rate of variation as the second difference Gov Debt`, 1:8) + L(`Rate of variation as the second difference Private Debt`, 1:8))
VAReq1
VAReq2
VAReq3

coeftest(VAReq1, vcov. = NeweyWest(VAReq1, lag = 1, prewhite = F))
coeftest(VAReq2, vcov. = NeweyWest(VAReq2, lag = 1, prewhite = F))
coeftest(VAReq3, vcov. = NeweyWest(VAReq3, lag = 1, prewhite = F))


# Granger causality tests
# il Private Debt influisce su GDP mentre Gov Debt no

linearHypothesis(VAReq1, 
                 hypothesis.matrix = c("L(`Rate of variation as the second difference Gov Debt`, 1:8)1",
                                       "L(`Rate of variation as the second difference Gov Debt`, 1:8)2"),
                 vcov. = NeweyWest(VAReq1, lag = 1, prewhite = F)) #non è una granger causality 


linearHypothesis(VAReq1, 
                 hypothesis.matrix = c("L(`Rate of variation as the second difference Private Debt`, 1:8)1",
                                       "L(`Rate of variation as the second difference Private Debt`, 1:8)2"),
                 vcov. = NeweyWest(VAReq1, lag = 1, prewhite = F)) #è una granger causality 

#Impulse response
irfs = lp_lin(endog_data = Japan [,c(4,9,13)],  # the variables
              lags_endog_lin = 2,        # the number of lags
              trend = 0,                 # we do not include a trend
              shock_type = 1,            # shock equal to 1 standard deviation
              confint = 1.96,            # 95% confidence interval
              hor = 12)                  # how many periods ahead

plot(irfs)





