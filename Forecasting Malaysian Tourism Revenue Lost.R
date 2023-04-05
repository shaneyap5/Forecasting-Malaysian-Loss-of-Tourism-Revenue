### Forecasting the loss of Malaysia's tourism revenue loss from 2020-2021 ###
library(fpp2)

#### Data Preliminaries ###
tourist <- read.csv("Tourist Arrivals Monthly Malaysia.csv",header=T)
tourist #Monthly tourist data in Malaysia from January 1989 to Mar 2021
tdata <- ts(tourist[,-1],start=c(1989,1),end=c(2021,3),frequency = 12) #converting tourist data into a time series
tdata
autoplot(tdata)+
  ggtitle('Malaysian Monthly Tourist Arrivals')+xlab('Years')+ylab('Tourist Arrivals')

### Full Dataset ###
fulldata <-window(tdata,start=c(2010,1)) #cannot be certain that the data displays similar pattern as the trend before.Env in period before is not similar 
tdata
fulldata #check to see if full data is reflective of Jan 2010 - Mar 2021

### Partitioning Dataset into Precovid and Covid & training and test set ###
precovid <-window(fulldata,start=c(2010,1),end=c(2020,2)) #lockdown and border closure only occurred March onwards
covid <-window(fulldata,start=c(2020,3)) #Covid dataset from the implementing of border restriction onward

fulldata
precovid
covid #check to see if precovid and covid dataset is paritioned correctly
length(covid) #13 
length(precovid) #122

## Phase 1 Model Identification ##
## Partion into training and test set ##
train <-window(precovid,end=c(2019,1))
train
length(train) #109
test <-window(precovid,start=c(2019,2))
length(test) #13 months to mimic test forecast horizon
test

autoplot(train)+ggtitle('Malaysian Monthly Tourist Arrivals')+xlab('Years')+ylab('Tourist Arrivals') #non constant mean but variance is arguably constant. 
bcl <- BoxCox.lambda(train) #safety check to see if stablizing variance makes a difference
bcl #Lambda value = 1.999924
train %>% BoxCox(lambda = bcl) %>% ggtsdisplay(main='Plot with BoxCox Transformation')
train %>% ggtsdisplay(main='Plot without BoxCox Transformation') #plot looks the same but only axis values change. thus likely do not need to stablizie the variance
ggseasonplot(train)+ylab('Tourist Arrivals')+ggtitle('Seasonal Plot for Monthly Malaysian Tourist Arrivals')
train %>% nsdiffs() #nsdiffs value is 1, conduct a seasonal difference
train %>% diff(lag=12) %>% ggtsdisplay(main='Training Set Data with Seasonal Differencing') #mean and variance looks arguably constant
train %>% diff(lag=12) %>% ndiffs() #ndiff value is 0 so we proceed with just a seasonal differnce
train %>% diff(lag=12) %>% ggtsdisplay(main='Malaysian Tourist Arrival')
#Based on the transformation ealirer we have set d=0 and D=1. When observing the PACF and ACF we find the following values
#p=1 and p=2 (based on the second lag in the PACF being close to the CV) and P=1 (lag 12 significant but lag 24 isn't)
#q = 3 based on the ACF and Q = 1 as lag 12 in the ACF is significant but lag 24 is not. 

#Models identified
#ARIMA(1,0,0)(1,1,0)
#ARIMA (2,0,0)(1,1,0)
#ARIMA (0,0,3)(0,1,1)

## Phase 2 Estimation and Testing ##
autoplot(train) #in the training set a plateu forms so we will not include a constant
fit1 <-Arima(train,order=c(1,0,0),seasonal = c(1,1,0),include.constant = F) 
fit2<-Arima(train,order=c(2,0,0),seasonal = c(1,1,0),include.constant = F)
fit3<-Arima(train,order=c(0,0,3),seasonal = c(0,1,1),include.constant = F)

fit1
fit2
fit3

checkresiduals(fit1) #pvalue is 0.208 so is significant at both 5 and 10%
checkresiduals(fit2) #pvalue is 0.1181 so it is signficant at both 5 and 10%
checkresiduals(fit3) #pvalue is 0.2808 so it is signfiicant at both 5 and 10%

fit4 <-auto.arima(train,stepwise = F)
fit4 #ARIMA (0,0,4)(1,1,0)
checkresiduals(fit4) #pvalue is 0.7765 so is significant at both 5 and 10%

c(fit1$aicc,fit2$aicc,fit3$aicc,fit4$aicc)
#2566.629 2566.697 2565.051 2559.336
#fit 3 and fit 4 have the lowest aicc values thus they are our chosen models

checkresiduals(fit3) #pvalue is 0.2808 so it is signfiicant at both 5 and 10%
checkresiduals(fit4) #pvalue is 0.7765 so is significant at both 5 and 10%

## Produce forecast for test set using 
fc1 <-forecast(fit3,h=length(test))
fc2 <-forecast(fit4,h=length(test))

## Evaluate ##
accuracy(fc1,test)
accuracy(fc2,test)
#forecast 2 using fit4 [ARIMA(0,0,4)(1,1,0)] performs the best. Based on training set error

### Phase 3 Application ###
model.fit <-Arima(precovid,order=c(0,0,4),seasonal=c(1,1,0),include.constant = F)
model.fit

fcast.arima <- forecast(model.fit,h=length(covid))
fcast.arima

autoplot(fulldata)+
  xlab('Years')+
  ylab('Tourist Arrivals')+
  ggtitle("Malaysian Tourist Arrivals with Forecasted Tourist Arrivals Using ARIMA(0,0,4)(1,1,0) model")+
  autolayer(fcast.arima,series='ARIMA Forecast',alpha=0.8)

### Comparison with ETS model ###
pc.ets <- ets(precovid)
pc.ets
fcast.ets <- forecast(pc.ets,h=length(covid))
fcast.ets

#Plot ARIMA and ETS forecast against actual Tourist Arrivals #
#ARIMA AND ETS PLOT
autoplot(fulldata)+
  xlab('Years')+
  ylab('Tourist Arrivals')+
  ggtitle("Malaysian Tourist Arrivals with Forecasted Tourist Arrivals Using ARIMA(0,0,4)(1,1,0) model")+
  autolayer(fcast.arima,series='ARIMA Forecast',alpha=0.9)+
  autolayer(fcast.ets,series = "ETS Forecast",alpha=0.6)

accuracy(fcast.arima,covid)
accuracy(fcast.ets,covid)

## Quantify Average Forecasted Loss in Tourism Revenues Using the two models ##
## ARIMA ##
# Based on website the average spending per touris is RM3000/tourist ##
# tourism.gov.my/statistics #

#Loss in tourism numbers #
tloss.arima <-sum(fcast.arima$mean - covid)
tloss.arima
tloss.arimaB <- (tloss.arima*3000)/1000000000
tloss.arimaB #RM77,907,825,114 or RM77.9 billion

tloss.alower <- sum(fcast.arima$lower[,2]-covid)
tloss.aupper <-sum(fcast.arima$upper [,2] - covid)
tloss.alowerB <- (tloss.alower*3000)/1000000000
tloss.aupperB <- (tloss.aupper*3000)/1000000000
tloss.alowerB #RM65.56311 billion or RM65,563,114,765
tloss.aupperB #Rm 90.25254 billion or RM90,252,535,462

## ETS ##
#Loss in tourism numbers #
tloss.ets <-sum(fcast.ets$mean - covid)
tloss.ets
tloss.etsB <- (tloss.ets*3000)/1000000000
tloss.etsB #RM68,315,397,406 or RM68.3154 billion

tloss.elower <- sum(fcast.ets$lower[,2]-covid)
tloss.eupper <-sum(fcast.ets$upper [,2] - covid)
tloss.elowerB <- (tloss.elower*3000)/1000000000
tloss.eupperB <- (tloss.eupper*3000)/1000000000
tloss.elowerB #RM 54.97021 billion or RM 54,970,211,494
tloss.eupperB #Rm 81.66058 billion or RM81,660,583,319


#plotting the loss for ARIMA and ETS against the actual
# Based on MATA " Malaysian Association of Tour and Travel Agents, the loss was estimated to be RM100 billion in 2020 due to the pandemic
# While neither came close to the actual forecast, the ARIMA model selected came the closest between the 2, what could be done in the future is to further refine the ARIMA model in hopes to come close to the actual reported number of RM100 billion
tloss <- data.frame(Bound = c("Lower Bound", "Upper Bound", "Forecast"),
                    ARIMA = c(tloss.alowerB, tloss.aupperB, tloss.arimaB),
                    ETS = c(tloss.elowerB, tloss.eupperB, tloss.etsB))

#REVENUE LOSS PLOT
ggplot(tloss, aes(x = Bound)) +
  geom_hline(aes(yintercept = tloss.alowerB, color = "ARIMA Lower Bound", linetype = "ARIMA Lower Bound")) +
  geom_hline(aes(yintercept = tloss.arimaB, color = "ARIMA Forecast", linetype = "ARIMA Forecast")) +
  geom_hline(aes(yintercept = tloss.aupperB, color = "ARIMA Upper Bound", linetype = "ARIMA Upper Bound")) +
  geom_hline(aes(yintercept = tloss.elowerB, color = "ETS Lower Bound", linetype = "ETS Lower Bound")) +
  geom_hline(aes(yintercept = tloss.etsB, color = "ETS Forecast", linetype = "ETS Forecast")) +
  geom_hline(aes(yintercept = tloss.eupperB, color = "ETS Upper Bound", linetype = "ETS Upper Bound")) +
  geom_hline(yintercept = 100, color = "black", linetype = "solid") +
  scale_color_manual(values = c("ARIMA Lower Bound" = "blue", "ARIMA Forecast" = "blue", "ARIMA Upper Bound" = "blue", 
                                "ETS Lower Bound" = "red", "ETS Forecast" = "red", "ETS Upper Bound" = "red")) +
  scale_linetype_manual(values = c("ARIMA Lower Bound" = "dashed", "ARIMA Forecast" = "solid", "ARIMA Upper Bound" = "dashed", 
                                   "ETS Lower Bound" = "dashed", "ETS Forecast" = "solid", "ETS Upper Bound" = "dashed")) +
  labs(title = "Tourism Revenue Loss in Billions of RM (ARIMA vs ETS vs Actual)",
       x = "Bound", y = "Revenue Loss (in billions of RM)") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5),
        axis.title = element_text(size = 12, face = "bold"),
        axis.text = element_text(size = 10),
        legend.title = element_blank(),
        legend.position = "right",
        legend.text = element_text(size = 10)) +
  guides(color = guide_legend(override.aes = list(color = c("blue", "blue", "blue", "red", "red", "red"), 
                                                  linetype = c("dashed", "solid", "dashed", "dashed", "solid", "dashed")))) 



