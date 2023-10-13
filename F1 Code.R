library(lubridate)
library(lmtest)
library(tidyr)
library(dplyr)
library(broom)
library(car)
library(ggplot2)
library(ggfortify)
library(purrr)
library(olsrr)
laptime=read.csv("C://lap_times.csv")
quali=read.csv("C://quali3.csv")
pits=read.csv("C://pits.csv")
speed=read.csv("C://results1.csv")
redflag=read.csv("C://redflag.csv")
#######################################
######################################

min(laptime$raceId)
max(laptime$raceId)


##converting m:s into seconds
laptime$time<- ms(laptime$time)
laptime$time<- as.numeric(laptime$time)

#combining individual pitstops to one total time
pitstop=pits %>% group_by(driverId,raceId) %>% summarize(totallap = sum(milliseconds)/1000)

#trying to not include results where the drivers did not finish the whole race
try=laptime %>% group_by(raceId,driverId) %>% summarize(lapnum = max(lap))
try2=try %>% group_by(raceId) %>% summarize(lapnummax = max(lapnum))
try$lapnummax=try2$lapnummax[match(try$raceId,try2$raceId)]

#trying
dataset=laptime %>% group_by(driverId,raceId) %>% summarize(avglaptime = sum(time))
dataset=left_join(dataset,try,by=c("raceId","driverId"))

#tr
dataset=dataset[dataset$lapnum==dataset$lapnummax, ]
dataset=left_join(dataset,quali,by=c("raceId","driverId"))

#all blanks turned to NA
dataset[dataset==""]<- NA
dataset[dataset=="#N/A"]<- NA
dataset=dataset %>%
  group_by(driverId,raceId) %>%
  mutate(qfastest = min(q1,q2,q3, na.rm=TRUE))

#converting qualifying times to seconds
dataset$qfastest<- ms(dataset$qfastest)
dataset$qfastest<- as.numeric(dataset$qfastest)

#merging
dataxna=dataset[complete.cases(dataset[,c("qfastest")]),]
dataxna1=left_join(dataxna,pitstop,by=c("raceId","driverId"))
dataxna1=left_join(dataxna1,speed,by=c("raceId","driverId"))
dataxna1=left_join(dataxna1,redflag,by=c("raceId"))
dataxna1$redflag[is.na(dataxna1$redflag)]=0
dataxna1$redflag=as.numeric(dataxna1$redflag)

#Remove cases that don't have pitstops,budget, and a fastest lap speed( optional)
dataxna1=dataxna1[complete.cases(dataxna1[,c("totallap")]),]
# dataxna1=dataxna1[complete.cases(dataxna1[,c("budget")]),]
# dataxna1=dataxna1[complete.cases(dataxna1[,c("fastestLapSpeed")]),]

#For the dummy variables
dataxna1$circuitId=as.factor(dataxna1$circuitId)
dataxna1$budget=as.numeric(dataxna1$budget)
dataxna1$Weather=as.factor(dataxna1$Weather)

#trying subsets with avglaptime>135 and totallap<500
#subset1=dataxna1[dataxna1$totallap>500, ]
#subset2=dataxna1[dataxna1$totallap<500, ]
#subset3=subset2[subset2$Weather==0, ]
#subset4=subset2[subset2$Weather==1, ]
#subset5=dataxna1[dataxna1$raceId!=967, ]
subset6=dataxna1[dataxna1$redflag==0,]
#subset7=dataxna1[dataxna1$year==2021,]
#sub6remove=subset6[-c(48,263,417,798,979,343),]


#######REG3
reg3=lm(avglaptime~qfastest+circuitId+prop+totallap+Weather+factor(year)+factor(constructorId), data=subset6)
reg3cont=lm(avglaptime~qfastest+prop+totallap, data=subset6)
bptest(reg3,varformula =  ~ fitted.values(reg3))
resid3=reg3$residuals
resid3=as.data.frame(resid3)
boxcox(reg3)
summary(reg3)
ncvTest(reg3)
plot(reg3)
ols_test_normality(reg3)
dwtest(reg3)
ols_plot_cooksd_chart(reg3)
anova(reg3)

# Reg 3 without constructor

stepwise.2=lm(avglaptime~qfastest+circuitId+prop+totallap+Weather+factor(year)+factor(constructorId), data=subset6)
ols_step_both_aic(stepwise.2)
reg4=lm(avglaptime~qfastest+circuitId+prop+totallap+Weather+factor(year), data=subset6)
reg4cont=lm(avglaptime~qfastest+prop+totallap, data=subset6)
summary(reg4)
plot(reg4)
ols_test_normality(reg4)
ols_test_breusch_pagan(reg4)
dwtest(reg4)
vif(reg4)

# 1339 Observations

table(subset6$constructorId)
summary(reg3)


## Determining Outliers and Influential Observations
leverage <- ols_leverage(reg4)
stud_resids <- ols_plot_resid_stud(reg4)$data
dffits <- ols_plot_dffits(reg4)$data
cooks <- ols_plot_cooksd_chart(reg4)$data
dfbetas <- summary(influence.measures(reg4))

      
## Merging information
full <- data.frame(subset6, leverage, stud_resids$dsr, stud_resids$txt, dffits$dbetas, dffits$txt, cooks$cd, cooks$txt)

write.csv(full, "C:/Users/majud/OneDrive/Desktop/full.csv")
full.data <- read.csv("C:/Users/majud/OneDrive/Desktop/full.csv")

# Leverage Analysis
leverage.data <- full.data[full.data$leverage > 80/1339, ]

# Studentized Deleted Residuals
stud_resids.data <- full.data[complete.cases(full.data$stud_resids.txt), ]

# Outliers
outliers <- rbind(leverage.data, stud_resids.data)
outliers.unique <- unique(outliers)

# Cook's D
cooks.data <- full.data[complete.cases(full.data$cooks.txt), ]

# DFFITS
dffits.data <- full.data[complete.cases(full.data$dffits.txt), ]

# DFBETAS
dffits.data <- full.data[complete.cases(full.data$dfbetas.txt), ]

# Influential Observations
influential <- rbind(cooks.data, dffits.data)
influential.unique <- unique(influential)

# Influential Outliers
influential.outliers <- merge(outliers.unique, influential.unique)

# Prediction Intervals 
no.outliers <- full.data[-c(influential.outliers$X), ] # data without IO
reg.predict <- lm(avglaptime~qfastest+circuitId+prop+totallap+Weather+factor(year), data=no.outliers)

predictions <- predict(reg.predict, influential.outliers, interval="prediction")
predictions
decisions <- cbind(influential.outliers, predictions)

decisions.data <- decisions %>% 
  mutate(Decision = case_when(
    avglaptime > upr | avglaptime < lwr ~ "OUT",
    avglaptime <= upr & avglaptime > lwr ~ "IN"
  ))

write.csv(influential.outliers, "C:/Users/majud/OneDrive/Desktop/complete_influential.csv")
