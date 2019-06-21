setwd("D:/limworkspace/R_Statistics/Ch_04")

library(extrafont)

windowsFonts(hanna=windowsFont("BM HANNA 11yrs old")) # 폰트이름 변경 
windowsFonts(dohyeon=windowsFont("BM DoHyeon"))
windowsFonts(jalnan=windowsFont("Jalnan"))
windowsFonts(tb=windowsFont("TmonMonsori Black"))
windowsFonts(binggrae=windowsFont("Binggrae Taom"))
windowsFonts(nbg=windowsFont("나눔바른고딕"))

library(dplyr)
library(ggplot2)
library(reshape)


# Chapter 4. 표본분포

# Section 2. 중심극한정리

# 모집단의 분포와 상관없이 모집단의 평균과 표준편차가 존재할 때,
# 표본 크기 n이 충분히 크다면(일반적으로 30이상), 표본평균의 분포는 근사적으로 정규분포를 따른다.
# 또한 표본평균의 분포가 정규분포를 따르므로 다음과 같이 표준화 하여 사용할 수 있다. 

# Z = ((x_bar) - μ) / (σ / sqrt(n))

# 2.1 모집단이 정규분포 일 때 

# 예제 4-2) 정규분포로부터 추출된 표본평균(x_bar)의 분포 
set.seed(9)


r.1.mean <- rep(NA, 100000)
r.2.mean <- rep(NA, 100000)

for (i in 1:10000) {
  r.1.mean[i] <- mean(rnorm(4, mean = 3, sd = 1))
  r.2.mean[i] <- mean(rnorm(4, mean = 170, sd = 6))
}

options(digits = 4)
c(mean(r.1.mean), sd(r.1.mean))
c(mean(r.2.mean), sd(r.2.mean))

par(mfrow=c(1,2))
hist(r.1.mean, prob=T, xlab="표본평균", ylab="밀도", main ="", col="Coral1")
x1 <- seq(min(r.1.mean), max(r.1.mean), length.out = 100000)
y1 <- dnorm(x=x1, mean=3, sd=(1/sqrt(4))) # N(3, 0.5^2)
lines(x1, y1, lwd=2, col = "grey40")


hist(r.2.mean, probability = T, xlab="표본평균", ylab="밀도", col = "royalblue1", main = "")
x2<- seq(min(r.2.mean), max(r.2.mean), length.out = 100000)
y2 <- dnorm(x=x2, mean=170, sd=(6/sqrt(4))) # N(3, 0.5^2)
lines(x2, y2, lwd=2, col = "grey40")


# 2.2 모집단이 정규분포가 아닌 임의의 분포일 때 

# 예제 4-3) 임의의 분포에서 추출된 표본평균 (x_bar)의 분포 
# 모집단의 분포가 한쪽으로 치우쳐진 이항분포 B(10, 0.1)로 부터 표본의 개수를 변화시킴에 따라 
# 표본평균의 분포 모양이 어떻게 변하는지 확인해보자 

set.seed(9)

t <- 10
p <- 0.1
x <- 0:10
n <- 1000

b.2.mean <- rep(NA, n)
b.4.mean <- rep(NA, n)
b.32.mean <- rep(NA, n)
b.64.mean <- rep(NA, n)

for (i in 1:n) {
  b.2.mean[i] <- mean(rbinom(2, size=t, prob = p))
  b.4.mean[i] <- mean(rbinom(4, size=t, prob = p))
  b.32.mean[i] <- mean(rbinom(32, size=t, prob = p))
  b.64.mean[i] <- mean(rbinom(64, size=t, prob = p))
}

options(digits = 4)
c(mean(b.2.mean), sd(b.2.mean))
c(mean(b.4.mean), sd(b.4.mean))
c(mean(b.32.mean), sd(b.32.mean))


# 2.3 이항 분포에서 표본 크기에 따른 분포 변화 
# N(평균, 표준편차/sqrt(n))

par(mfrow = c(2,2))
hist(b.2.mean, probability = T, xlim = c(0, 4), main = "표본크기 : 2", col = "Coral1", xlab="")
x1 <- seq(min(b.2.mean), max(b.2.mean), length.out = 1000)
y1 <- dnorm(x=x1, mean = 1, sd=sqrt(0.9/2))
lines(x1, y1, lty=2, lwd=2, col = "grey40")

hist(b.4.mean, probability = T, xlim = c(0, 4), main = "표본크기 : 4", col = "royalblue1", xlab="")
x2 <- seq(min(b.4.mean), max(b.4.mean), length.out = 1000)
y2 <- dnorm(x=x2, mean = 1, sd=sqrt(0.9/4))
lines(x2, y2, lty=2, lwd=2, col = "grey40")

hist(b.32.mean, probability = T, xlim = c(0.3, 1.7), main = "표본크기 : 32", col = "olivedrab3", xlab="")
x3 <- seq(min(b.32.mean), max(b.32.mean), length.out = 1000)
y3 <- dnorm(x=x3, mean = 1, sd=sqrt(0.9/32))
lines(x3, y3, lty=2, lwd=2, col = "grey40")

hist(b.64.mean, probability = T, xlim = c(0.3, 1.7), main = "표본크기 : 64", col = "pink1", xlab="")
x4 <- seq(min(b.64.mean), max(b.64.mean), length.out = 1000)
y4 <- dnorm(x=x3, mean = 1, sd=sqrt(0.9/64))
lines(x4, y4, lty=2, lwd=2, col = "grey40")
par(mfrow = c(1,1))

