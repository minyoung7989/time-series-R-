library(readxl)
library(corrplot)
stat<- read_excel("month.xlsx")
plot(stat[,2:6])
stat_cor<- cor(stat)
stat_xy<-stat[,-c(1)]
##상관분석 표
corrplot(stat_cor,
     method="color",
     type="lower",
     order="hclust",
     addCoef.col="black",
     tl.col="black",
     tl.srt=45,
     diag=F)
stat_cor #연도 포함
plot(stat_xy)

statxy_cor<-cor(stat_xy)
corrplot(statxy_cor,
        method="color",
        type="lower",
        order="hclust",
        addCoef.col="black",
        tl.col="black",
        tl.srt=45,
        diag=F)#연도 비포함

##두 변수간 상관관계
library(Hmisc)
rcorr(as.matrix(stat_xy), type="pearson")#피어슨 상관걔수 + H0에대한 피벨류값


###다중회귀분석
multi_stat<- lm(stat$건수 ~ stat$`풍속(m/s)`
              +stat$`습도(%)`+stat$`면적(ha)`+stat$`피해금액(백만원)`, data=stat)
summary(multi_stat)


###회귀모형
plot(lm_xy)
plot(multi_stat)
###########################전처리#############################
library(readxl)
library(tidyverse)
library(TTR)
library(forecast)
dat <- read_excel("month.xlsx")

month <- data.frame(t(dat)) %>% 
  select(X1,X3,X5,X7,X9, X11, X13, X15, X17, X19, X21, X23, X25) %>% 
  '['(c(-1,-2),)%>% 
  mutate_if(is.character, funs(as.numeric))


month <- month[,-1]

colnames(month) <- c('Jan','Feb','Mar','Apr','May','Jun','Jul','Aug','Sep','Oct','Nov','Dec')

month[is.na(month)] <- 0
############################### 1월 #######################################
jan.ts <- ts(month$Jan) ###시계열변환

jan.sma1 <- SMA(jan.ts, n = 5) ###이동평균
plot.ts(jan.sma1)

jan.diff <- diff(jan.ts, differences = 1)###1차차분

jan.acf <- acf(jan.diff, lag.max = 20, plot = T) ###acf모형

jan.pacf <- pacf(jan.diff, lag.max = 20, plot = T) ###pacf모형 

auto.arima(jan.ts) ###auto arima

jan.arima <- arima(month$Jan, order = c(0,0,0))

jan.forecast <- forecast(jan.arima, h = 1) ####예측
jan.forecast

################################ 2월 ###################################
feb.ts <- ts(month$Feb)

feb.sma1 <- SMA(feb.ts, n = 5)
plot.ts(feb.sma1)

feb.diff <- diff(feb.ts, differences = 1)

feb.acf <- acf(feb.diff, lag.max = 20, plot = T)

feb.pacf <- pacf(feb.diff, lag.max = 20, plot = T)

auto.arima(feb.ts)

feb.arima <- arima(month$Feb, order = c(0,0,0))

feb.forecast <- forecast(feb.arima, h = 1)
feb.forecast

################################# 3월 #################################
mar.ts <- ts(month$Mar)

mar.sma1 <- SMA(mar.ts, n = 5)
plot.ts(mar.sma1)

mar.diff <- diff(mar.ts, differences = 1)

mar.acf <- acf(mar.diff, lag.max = 20, plot = T)

mar.pacf <- pacf(mar.diff, lag.max = 20, plot = T)

auto.arima(mar.ts)

mar.arima <- arima(month$Mar, order = c(0,0,0))

mar.forecast <- forecast(mar.arima, h = 1)
mar.forecast


############################# 4월 ####################################
apr.ts <- ts(month$Apr)

apr.sma1 <- SMA(apr.ts, n = 5)
plot.ts(apr.sma1)

apr.diff <- diff(apr.ts, differences = 1)

apr.acf <- acf(apr.diff, lag.max = 20, plot = T)

apr.pacf <- pacf(apr.diff, lag.max = 20, plot = T)

auto.arima(apr.ts)

apr.arima <- arima(month$Apr, order = c(0,0,0))

apr.forecast <- forecast(apr.arima, h = 5)
apr.forecast

############################# 5월 #####################################
may.ts <- ts(month$May)

may.sma1 <- SMA(may.ts, n = 5)
plot.ts(may.sma1)

may.diff <- diff(may.ts, differences = 1)

may.acf <- acf(may.diff, lag.max = 20, plot = T)

may.pacf <- pacf(may.diff, lag.max = 20, plot = T)

auto.arima(may.ts)

may.arima <- arima(month$May, order = c(0,0,0))

may.forecast <- forecast(may.arima, h = 1)
may.forecast

########################### 6월 #################################
jun.ts <- ts(month$Jun)

jun.sma1 <- SMA(jun.ts, n = 5)
plot.ts(jun.sma1)

jun.diff <- diff(jun.ts, differences = 1)

jun.acf <- acf(jun.diff, lag.max = 20, plot = T)

jun.pacf <- pacf(jun.diff, lag.max = 20, plot = T)

auto.arima(jun.ts)

jun.arima <- arima(month$Jun, order = c(0,0,0))

jun.forecast <- forecast(jun.arima, h = 1)
jun.forecast

########################## 7월 ##################################(
jul.ts <- ts(month$Jul)

jul.sma1 <- SMA(jul.ts, n = 5)
plot.ts(jul.sma1)

jul.diff <- diff(jul.ts, differences = 1)

jul.acf <- acf(jul.diff, lag.max = 20, plot = T)

jul.pacf <- pacf(jul.diff, lag.max = 20, plot = T)

auto.arima(jul.ts)

jul.arima <- arima(month$Jul, order = c(0,1,0))

jul.forecast <- forecast(jul.arima, h = 1)
jul.forecast

########################### 8월 ##################################
aug.ts <- ts(month$Aug)

aug.sma1 <- SMA(aug.ts, n = 5)
plot.ts(aug.sma1)

aug.diff <- diff(aug.ts, differences = 1)

aug.acf <- acf(aug.diff, lag.max = 20, plot = T)

aug.pacf <- pacf(aug.diff, lag.max = 20, plot = T)

auto.arima(aug.ts)

aug.arima <- arima(month$Aug, order = c(0,1,1))

aug.forecast <- forecast(aug.arima, h = 1)
aug.forecast

############################### 9월 ############################
sep.ts <- ts(month$Sep)

sep.sma1 <- SMA(sep.ts, n = 5)
plot.ts(sep.sma1)

sep.diff <- diff(sep.ts, differences = 1)

sep.acf <- acf(sep.diff, lag.max = 20, plot = T)

sep.pacf <- pacf(sep.diff, lag.max = 20, plot = T)

auto.arima(sep.ts)

sep.arima <- arima(month$Sep, order = c(0,0,0))

sep.forecast <- forecast(sep.arima, h = 1)
sep.forecast

#################################### 10 월 ############################
oct.ts <- ts(month$Oct)

oct.sma1 <- SMA(oct.ts, n = 5)
plot.ts(oct.sma1)

oct.diff <- diff(oct.ts, differences = 1)

oct.acf <- acf(oct.diff, lag.max = 20, plot = T)

oct.pacf <- pacf(oct.diff, lag.max = 20, plot = T)

auto.arima(oct.ts)

oct.arima <- arima(month$Oct, order = c(0,0,1))

oct.forecast <- forecast(oct.arima, h = 1)
oct.forecast

#################################### 11월 ##############################
nov.ts <- ts(month$Nov)

nov.sma1 <- SMA(nov.ts, n = 5)
plot.ts(nov.sma1)

nov.diff <- diff(nov.ts, differences = 1)

nov.acf <- acf(nov.diff, lag.max = 20, plot = T)

nov.pacf <- pacf(nov.diff, lag.max = 20, plot = T)

auto.arima(nov.ts)

nov.arima <- arima(month$Nov, order = c(0,0,0))

nov.forecast <- forecast(nov.arima, h = 1)
nov.forecast

################################## 12 월 #################################


dec.ts <- ts(month$Dec)

dec.sma1 <- SMA(dec.ts, n = 5) 


dec.diff <- diff(dec.ts, differences = 1)

dec.acf <- acf(dec.diff, lag.max = 20, plot = T)

dec.pacf <- pacf(dec.diff, lag.max = 20, plot = T)

auto.arima(dec.ts)

dec.arima <- arima(month$Dec, order = c(0,0,0))

dec.forecast <- forecast(dec.arima, h = 1)
dec.forecast




