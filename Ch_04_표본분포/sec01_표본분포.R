setwd("D:/limworkspace/R_Statistics/Ch_04")

# Chapter 4. 표본분포

# Section 1. 표본분포

library(extrafont)

windowsFonts(hanna=windowsFont("BM HANNA 11yrs old")) # 폰트이름 변경 
windowsFonts(dohyeon=windowsFont("BM DoHyeon"))
windowsFonts(jalnan=windowsFont("Jalnan"))
windowsFonts(tb=windowsFont("TmonMonsori Black"))
windowsFonts(binggrae=windowsFont("Binggrae Taom"))
windowsFonts(nbg=windowsFont("나눔바른고딕"))

library(dplyr)
library(ggplot2)

# 예제 4-1) 표본평균(x_bar)의 분포 

set.seed(9) # 9번에 동일한 결과값 세팅 (하나만)

m10 <- rep(NA, 1000)
m40 <- rep(NA, 1000)

for (i in 1:1000) {
  m10[i] <- mean(rnorm(10)) # 정규분포에서의 표본평균(x_bar, 10개) 
  m40[i] <- mean(rnorm(40)) # 정규분포에서의 표본평균(x_bar, 40개) 표본 수가 4배 차이
}

options(digits = 4)         # 소수점 지정 
c(mean(m10), sd(m10))       # 표본평균(x_bar)과 표본표준편차(s) 확인
c(mean(m40), sd(m40))       # 표본 4배 = 표준편차 1/2배 감소   

par(mfrow=c(1,2))
hist(m10, xlim = c(-1.5, 1.5), col="royalblue", main = "n=10")
hist(m40, xlim = c(-1.5, 1.5), col="Coral", main = "n=40")

par(mfrow=c(1,1))

