setwd("D:/limworkspace/R_Statistics/Assignment/AM_11")
library(dplyr)

### **문제 1**
#### mtcars 데이터셋에서 자동차 기어 종류(am: 오토/수동)에 따른 mpg의 차이가 통계적으로 유의한지 t-test를 통해 확인해 보시오.

mtcars
str(mtcars)

0. 정규성 및 등분산성 검정 
am0 <- select(mtcars, am, mpg) %>% filter(am==0)
am1 <- select(mtcars, am, mpg) %>% filter(am==1)

shapiro.test(am0$mpg); qqnorm(am0$mpg); qqline(am0$mpg, col="red") # p = 0.8998로 정규성 만족 
shapiro.test(am1$mpg); qqnorm(am1$mpg); qqline(am1$mpg, col="red") # p = 0.5363로 정규성 만족 

var.test(ex1$mpg ~ ex1$am) # p = 0.0669로 등분산성 만족 

1. 가설검정 

H0 : 자동차 기어 종류(am)별 mpg의 차이가 없다 
H1 : 자동차 기어 종류(am)별 mpg의 차이가 있다 (양측)

2. 판정 (독립표본)
t.test(mtcars$mpg ~ mtcars$am, mu=0, alternative = "two.sided", var.equal=T)

mean(am0$mpg); sd(am0$mpg) # am == 0 인 그룹의 mpg 평균과 표준편차 
mean(am1$mpg); sd(am1$mpg) # am == 1 인 그룹의 mpg 평균과 표준편차 

3. 결론 
자동차 기어 종류별에 따라 mpg의 차이가 있는지 알아보기 위해 32개의 자동차 표본을 추출해 분석한 결과,
기어종류가 0인 그룹의 mpg는 17.14±3.83, 기어 종류가 1인 그룹의 mpg는 24.37±6.16으로 나타났다.
이를 유의수준 0.05에서 가설검정하면 검정통계량과 유의확률이 -4.1061(p=0.0002)으로 나타나
자동차 기어종류별 mpg는 통계적으로 유의한 결론을 내릴 수 있다.
즉 자동차 기어종류별로 mpg의 차이가 있다고 할 수 있다.


### **문제 2**
#### MASS 패키지에 내장된 Cars93 데이터프레임에 대해서 생산국(Origin)이 USA vs. non-USA 2개의 group 에 대해서 차 가격(Price)의 평균이 차이가 있는지를 검정해보시오.
library(MASS)
str(Cars93)


0. 정규성 및 등분산성 검정 

USA <- dplyr::select(Cars93, Origin, Price) %>% filter(Origin == "USA")
n_USA <- dplyr::select(Cars93, Origin, Price) %>% filter(Origin == "non-USA")

shapiro.test(USA$Price); qqnorm(USA$Price); qqline(USA$Price, col="red")       # p = 0.0002로 정규성을 만족하지 않음 -> 비모수검정 
shapiro.test(n_USA$Price); qqnorm(n_USA$Price); qqline(n_USA$Price, col="red") # p = 0.0002로 정규성을 만족하지 않음

var.test(Cars93$Price ~ Cars93$Origin) # p = 0.01387로 등분산성도 만족하지 않음 

1. 가설검정 # 실제로 정규성을 만족하지 않지만 .. 정규성을 만족한다는 가정 하에 분석을 진행함 

H0 : 지역(Origin)별 차 가격(Price)의 평균이 차이가 없다 
H1 : 지역(Origin)별 차 가격(Price)의 평균이 차이가 있다 (양측)

2. 판정 (독립표본)

t.test(Cars93$Price ~ Cars93$Origin, mu=0, alternative = "two.sided", var.equal=F) # 등분산성을 만족하지 않음 

mean(USA$Price); sd(USA$Price)     # USA 지역의 평균 차 가격과 표준편차 
mean(n_USA$Price); sd(n_USA$Price) # non_USA 지역의 평균 차 가격과 표준편차

3. 결론 

지역에 따라 차 가격의 평균이 차이가 있는지 알아보기 위해 93개의 자동차 표본을 추출해 분석한 결과,
지역이 USA인 그룹의 차 가격은 18.57±+7.81, 지역이 non_USA인 그룹의 차 가격은 20.56±11.3으로 나타났다.
이를 유의수준 0.05에서 가설검정하면 검정통계량과 유의확률이 -0.9544(p=0.3428)으로 나타나
지역에 따른 차 가격의 평균 차이가 통계적으로 유의하지 않다고 결론내릴 수 있다.
즉 지역(Origin)에 따른 차 가격(Price)의 평균은 차이가 없다. 


### **문제 3**
#### mpg 데이터셋에서 다음을 검정해 보시오. 

#### 3-1) subcompact 자동차와 midsize 자동차의 고속도로 연비

library(ggplot2)
str(mpg)

0. 정규성 및 등분산성 검정 

subcm <- dplyr::select(mpg, class, hwy) %>% filter(class == "subcompact")
mid <- dplyr::select(mpg, class, hwy) %>% filter(class == "midsize")

shapiro.test(subcm$hwy); qqnorm(subcm$hwy); qqline(subcm$hwy, col="red")  # p = 0.01로 정규성을 만족하지 않음
shapiro.test(mid$hwy); qqnorm(mid$hwy); qqline(mid$hwy, col="red")        # p = 0.013로 정규성을 만족하지 않음

mpg_sub_mid <- filter(mpg, class %in% c("subcompact","midsize"))          # class가 subcompact와 midsize인 데이터만 추출 

var.test(mpg_sub_mid$hwy ~ mpg_sub_mid$class) # p = 0.0000 으로 등분산성 만족하지 않음 

1. 가설검정 # 실제로 정규성을 만족하지 않지만 .. 정규성을 만족한다는 가정 하에 분석을 진행함 

H0 : 자동차 종류(subcompact, midsize)에 따른 고속도로 연비가 차이가 없다
H1 : 자동차 종류(subcompact, midsize)에 따른 고속도로 연비가 차이가 있다 (양측)

2. 판정 

t.test(mpg_sub_mid$hwy ~ mpg_sub_mid$class, mu=0, alternative = "two.sided", var.equal=F) # 등분산성을 만족하지 않음 

mean(subcm$hwy); sd(subcm$hwy)  # 차량종류가 subcompact인 차량의 고속도로 평균 연비와 표준편차 
mean(mid$hwy); sd(mid$hwy)      # 차량종류가 mid인 차량의 고속도로 평균 연비와 표준편차

3. 결론 

차량종류(class)에 따라 고속도로 평균 연비가 차이가 있는지 알아보기 위해 76개의 자동차 표본을 추출해 분석한 결과,
차량종류가 subcompact인 차량의 고속도로 평균 연비와 표준편차는 28.14±+5.37, 
차량종류가 midsize인 차량의 고속도로 평균 연비와 표준편차는 27.29±2.13으로 나타났다.
이를 유의수준 0.05에서 가설검정하면 검정통계량과 유의확률이 -0.8784(p=0.3846)으로 나타나
차량 종류에 따른 고속도로 평균 연비 차이가 통계적으로 유의하지 않다는 결론을 내릴 수 있다. 
즉 차량 종류(subcompact, midsize)에 따른 고속도로 평균 연비(hwy)는 차이가 없다.


#### 3-2) 일반 휘발유(r)와 고급 휘발유(p)의 도시 연비
str(mpg$fl)

0. 정규성 및 등분산성 검정 

r <- dplyr::select(mpg, fl, cty) %>% filter(fl == "r")
p <- dplyr::select(mpg, fl, cty) %>% filter(fl == "p")

shapiro.test(r$cty); qqnorm(r$cty); qqline(r$cty, col="red")  # p = 0.000027로 정규성을 만족하지 않음
shapiro.test(p$cty); qqnorm(p$cty); qqline(p$cty, col="red")  # p = 0.04984로 정규성을 근소하게 만족하지 않음

mpg_r_p <- filter(mpg, fl %in% c("r","p"))  # fl(연료종류)가 r(일반휘발유)과 p(고급휘발유)인 데이터만 추출 
var.test(mpg_r_p$cty ~ mpg_r_p$fl)          # p = 0.0428로 등분산성도 만족하지 않음 

1. 가설검정 # 실제로 정규성을 만족하지 않지만 .. 정규성을 만족한다는 가정 하에 분석을 진행함 

H0 : 자동차 연료(r, p)에 따른 도시 평균 연비의 차이가 없다
H1 : 자동차 연료(r, p)에 따른 도시 평균 연비의 차이가 없다 (양측)


2. 판정 

t.test(mpg_r_p$cty ~ mpg_r_p$fl, mu=0, alternative = "two.sided", var.equal=F) # 등분산성을 만족하지 않음 

mean(r$cty); sd(r$cty)  # 연료종류(fl)가 일반휘발유(r)인 차량의 도시 평균 연비와 표준편차 
mean(p$cty); sd(p$cty)  # 연료종류(fl)가 고급휘발유(p)인 차량의 도시 평균 연비와 표준편차 


3. 결론 

연료종류(fl)에 따라 도시 평균 연비가 차이가 있는지 알아보기 위해 220개의 자동차 표본을 추출해 분석한 결과,
연료종류(fl)가 일반휘발유(r)인 차량의 도시 평균 연비와 표준편차는 16.73±3.88, 
연료종류(fl)가 고급휘발유(p)인 차량의 도시 평균 연비와 표준편차는 17.37±3.04으로 나타났다.
이를 유의수준 0.05에서 가설검정하면 검정통계량과 유의확률이 1.2118(p=0.2283)으로 나타나
연료종류(fl)에 따른 도시 평균 연비 차이가 통계적으로 유의하지 않다는 결론을 내릴 수 있다. 
즉 연료종류(fl)에 따른 도시 평균 연비(hwy)는 차이가 없다.


#### 3-3) subcompact 자동차의 전륜구동(f)이냐 후륜구동(r)이냐에 따른 도시 연비

subcompact <- dplyr::select(mpg, class, drv, cty) %>% filter(class == "subcompact" & drv == c("r","f"))
str(subcompact)

0. 정규성 및 등분산성 검정 

fowa <- filter(subcompact, drv == "f")
rear <- filter(subcompact, drv == "r")

shapiro.test(fowa$cty); qqnorm(fowa$cty); qqline(fowa$cty, col="red")  # p = 0.095로 정규성 만족 
shapiro.test(rear$cty); qqnorm(rear$cty); qqline(rear$cty, col="red")  # p = 0.1로 정규성 만족 

var.test(subcompact$cty ~ subcompact$drv) # 등분산성 만족하지 않음 

1. 가설검정 

H0 : subcompact 자동차의 구동방식(drv)에 따른 도시 평균 연비가 차이가 없다
H1 : subcompact 자동차의 구동방식(drv)에 따른 도시 평균 연비가 차이가 있다

2. 판정 

t.test(subcompact$cty ~ subcompact$drv, mu=0, alternative = "two.sided", var.equal=F) # 등분산성을 만족하지 않음 

mean(fowa$cty); sd(fowa$cty)  # 연료종류(fl)가 일반휘발유(r)인 차량의 도시 평균 연비와 표준편차 
mean(rear$cty); sd(rear$cty)  # 연료종류(fl)가 고급휘발유(p)인 차량의 도시 평균 연비와 표준편차 

3. 결론 
