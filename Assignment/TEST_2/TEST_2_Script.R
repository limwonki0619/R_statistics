setwd("D:/limworkspace/R_statistics/Assignment/TEST_2")

library(dplyr)


### 문제 1.
#### 다음의 값을 R 내장함수를 이용하여 구하시오 

# 1)
dbinom(3, size=6, prob=1/3)

# 2) 
z <- qnorm(0.8, mean=0, sd=1) # 표준정규분포에서 상위 20%에 해당하는 z값 
# z = x-u/sigam
X <- (z*6)+170

# 175cm 이상

# 3) 
pchisq(0.95, df=3)

# 4)
pt(0.975, df=2)

# 5)
pnorm(1, mean=0, sd=1)

### 문제 2.
#### 다음의 문항이 베르누이 시행인지 판단하시오.

# 1) 아니다
# 2) 맞다
# 3) 아니다
# 4) 맞다
# 5) 맞다


### 문제 3.
#### R 내장 데이터인 iris를 이용하여 setosa중 sepal.length의 모평균에 대한 95% 신뢰구간
ex3 <- iris %>% select(Sepal.Length, Species) %>% filter(Species == "setosa")
n <- length(ex3$Sepal.Length)
s <- sd(ex3$Sepal.Length)
x_bar <- mean(ex3$Sepal.Length)
alpha <- 0.05
z <- qnorm(1-(alpha/2))

( ll <- x_bar - z*s/sqrt(n) ) # 신뢰구간 하한
( ul <- x_bar + z*s/sqrt(n) ) # 신뢰구간 상한 

### 문제 4. 
#### 한 농구 선수가 자유투를 던지면 10번중에 7번을 성공한다고 할 때 다음을 풀이하시오
# X~B(10, 7/10)
# 1) 이 선수가 자유투를 10번 던져 9번 이상 성공할 확률
1-pbinom(8, size=10, prob=7/10)

# 2) 이 선수가 자유투를 10번 던질 때 5번 이상 8번 이하로 성공할 확률 
pbinom(8, size=10, prob=7/10) - pbinom(4, size=10, prob=7/10)

### 문제 5. 
#### 한국인의 평균 알코올 섭취량이 8.1g이다 2008년 무작위로 뽑은 알코올 섭취량이 다음과 같다. 재작년과 달라졌다고 할 수 있나
ex5 <- c(16.9, 13.21, 15.67, 9.87, 13.15, 9.98, 3.56, 14.5, 8.12, 6.97)

# 0.정규성 검정 
shapiro.test(ex5); qqnorm(ex5); qqline(ex5, col="red")
# 정규성 검정결과 p-value 값이 0.818로 정규성을 만족한다.


# 모분산을 모르고 소표본이므로 t-test 검정을 사용한다 

# 가설 
# H0 : mu = 8.1(g)
# H1 : mu != 8.1(g)

# 판정
t.test(ex5, mu=8.1, alternative = "two.sided")
mean(ex5); sd(ex5)

# 결론 
# t-test 검정걸과 2008년 한국인의 알코올 섭취량은 평균 11.193 ± 4.22로 나타났으며, p-value 값이 0.04562로 귀무가설을 기각한다.
# 즉, 2008년 한국인의 알코올 섭취량이 재작년에 비해 달라졌다고 할 수 있다. 



### 문제 6. 
#### 정규분포에서 form <= X <= to의 확률을 구하는 함수를 작성하시오
rangenorm <- function(from, to, mean, sd){
  result <- pnorm(to, mean, sd) - pnorm(from, mean, sd)
  return(result)
}
rangenorm(-1.96, 1.96, 0, 1)


### 문제 7. 
#### mpg데이터셋에서 다음을 검정해 보시오.

# 1) subcompact 자동차와 midsize 자동차의 도시연비
library(ggplot2)
subcm <- dplyr::select(mpg, class, cty) %>% filter(class == "subcompact")
mid <- dplyr::select(mpg, class, cty) %>% filter(class == "midsize")

# 정규성 검정 
par(mfrow=c(1,2))
shapiro.test(subcm$cty); qqnorm(subcm$cty); qqline(subcm$cty, col="red")
shapiro.test(mid$cty); qqnorm(mid$cty); qqline(mid$cty, col="red")
par(mfrow=c(1,1))

## 두 집단 모두 p-value 값이 유의수준 0.05보다 작아 정규성을 만족하지 않으므로 Wilcoxon 순위 검정을 시행  
mpg_sub_mid <- filter(mpg, class %in% c("subcompact","midsize"))  
str(mpg_sub_mid)

# 가설설정 
# H0 : mu1 == mu2 
# H1 : mu1 != mu2

wilcox.test(mpg_sub_mid$cty ~ mpg_sub_mid$class)
# 검정결과 p-value 값이 0.1748로 유의수준 0.05 하에서 귀무가설을 기각할만한 충분한 근거가 없다. 즉, subcompact 자동차와 midsize 자동차 간에 연비 차이는 없다.

## 두 집단이 정규성을 만족한다는 가정하에 모수적 방법(t-test)으로 검정 

# 가설설정 
# H0 : mu1 == mu2 
# H1 : mu1 != mu2

# 등분산성 검정 
var.test(mpg_sub_mid$cty ~ mpg_sub_mid$class) 
# 등분산성 검정결과 p-value 값이 유의수준 0.05보다 작으므로 등분산성 가정이 성립되지 않는다. 따라서 이분산을 가정한 t-test를 실시한다.


# 이분산 t-test 검정 
t.test(mpg_sub_mid$cty ~ mpg_sub_mid$class, mu=0, alternative = "two.sided", var.equal=F) 
mean(subcm$cty); sd(subcm$cty) 
mean(mid$cty); sd(mid$cty)

# 검정결과, 사이즈가 subcompact인 차량의 평균 연비와 표준편차는 20.3714 ± 4.60 이고
# midsize인 차량의 평균 연비와 표준편차는 18.7561 ± 1.946로 나타났으며
# p-value 값이 0.05952로 유의수준 0.05하에서 귀무가설을 기각할만한 충분한 근거가 없다. 즉, subcompact 자동차와 midsize 자동차의 연비 차이는 없다.




# 2) 일반 휘발유(r)와 고급 휘발유(p)의 고속도로 연비 

r <- dplyr::select(mpg, fl, hwy) %>% filter(fl == "r")
p <- dplyr::select(mpg, fl, hwy) %>% filter(fl == "p")

# 정규성 검정
par(mfrow=c(1,2))
shapiro.test(r$hwy); qqnorm(r$hwy); qqline(r$hwy, col="red") 
shapiro.test(p$hwy); qqnorm(p$hwy); qqline(p$hwy, col="red") 
par(mfrow=c(1,1))
#정규성 검정결과 두 집단 모두 정규성을 만족하지 않는다. 따라서 비모수적 방법을 이용한 검정을 사용한다.

mpg_r_p <- filter(mpg, fl %in% c("r","p"))  # fl(연료종류)가 r(일반휘발유)과 p(고급휘발유)인 데이터만 추출 

# 가설설정 
# H0 : mu1 == mu2 
# H1 : mu1 != mu2

wilcox.test(mpg_r_p$hwy ~ mpg_r_p$fl)
# 윌콕스 순위검정 결과 p-value값이 0.008495로 유의수준 0.05하에서 귀무가설이 기각된다. 즉, 휘발유 종류에 따른 고속도로 연비 차이가 있는 것으로 판단된다.

## 두 집단이 정규성을 만족한다는 가정하에 모수적 방법(t-test)으로 검정 

# 가설설정 
# H0 : mu1 == mu2 
# H1 : mu1 != mu2

# 등분산성 검정 
var.test(mpg_r_p$hwy ~ mpg_r_p$fl)  
# 등분산성 검정 결과 p-value값이 0.006139로 유의수준 0.05하에서 등분산성이 성립되지 않는다. 따라서 이분산을 가정한 t-test를 시행한다.

# 이분산 t-test 검정 
t.test(mpg_r_p$hwy ~ mpg_r_p$fl, mu=0, alternative = "two.sided", var.equal=F)
mean(r$hwy); sd(r$hwy)
mean(p$hwy); sd(p$hwy)

# 검정결과 일반휘발유의 고속도로 연비의 평균과 표준편차는 22.9940 ± 5.505,
# 고급휘발유의 고속도로 연비의 평균과 표준편차는 25.2307 ± 3.9338로 나타났으며,
# p-value값이 0.001576으로 유의수준 0.05 하에서 귀무가설을 기각한다. 즉, 휘발유 종류에 따른 고속도로연비는 차이가 있는 것으로 판단된다.



### 문제 8. 
#### 적합도를 검정하시오 

x <- c(322, 109, 99, 29)

# 가설설정 
# H0 : p1=9/16, p2=3/16, p3=3/16, p4=3/16을 따른다.
# H1 : not H0

# 검정 
chisq.test(x, p=c(9,3,3,1)/16)

# 완두콩이 멘델의 유전법칙을 따르는지 알아보기 위해 적합도 검정을 실시한 결과, p-value가 0.6413으로 유의수준 0.05하에서 귀무가설을 기각할만한 충분한 근거가 없다. 즉, 형질별 개체수가 멘델의 법칙을 따른다고 할 수 있다. 


### 문제 9. 
#### R 내장 데이터인 "women"을 용해 키와 몸무게의 곡선회귀분석을 통한 회귀식을 구하라. (단, 2차식으로 구할 것) 
str(women)
model <- lm(weight ~ height, data=women)

# 정규성 검정 
shapiro.test(women$height)
shapiro.test(women$weight)

# 회귀식 기본가정 검정 
summary(gvlma::gvlma(model))
par(mfrow=c(2,2))
plot(model)
par(mfrow=c(1,1))

# 기본 가정에서 전반적으로 가정이 성립하지 않음을 알 수 있다. 따라서 모델을 2차 식으로 변경 후 기본 가정을 살펴보자.
model2 <- lm(weight ~ height + I(height^2), data=women)
summary(gvlma::gvlma(model2))
par(mfrow=c(2,2))
plot(model2)
par(mfrow=c(1,1))

# height를 2차식으로 변경해도 기본 가정이 잘 성립하지 않는다. 

model3 <- lm(weight ~ height + I(weight^2), data=women)
summary(gvlma::gvlma(model3))
par(mfrow=c(2,2))
plot(model3)
par(mfrow=c(1,1))

# 마찬가지로 weight를 2차식으로 변경해도 기본 가정이 잘 성립하지 않는다. 따라서 기본가정이 성립한다는 가정하에 model2로 분석을 진행을 해보겠다.

# 검정 
summary(model2)
# 회귀검정결과 F 검정통계량과 유의확률이 1.139e+04 (p = 2.2e-16)으로 유의수준 0.05에서 회귀식이 유의하다고 판단되며 
# 설명력은 99.95%로 나타난다. 또한 회귀계수는 유의수준 0.05하에서 모두 유의하며, 최종적으로 추정된 휘귀식은 
# 몸무게 = 261.87818 + (-7.34832*height) + 0.00306*height^2 으로 나타난다. 

