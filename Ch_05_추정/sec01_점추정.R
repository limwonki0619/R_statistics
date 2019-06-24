setwd("D:/limworkspace/R_Statistics/Ch_05_추정")

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

# Chapter 5. 추정

# Section 1. 점추정 ---------------------------------------------------------------------

# 점추정은 표본의 특성을 나타낸느 통계량 중 모수를 유추하는데 있어 최적의 계산식을 통해 구한 하나의 
# 추정값을 구하는 방법. 
# 하지만 점추정은 표본으로부터 게산되는 값이기에 추출되는 표본에 따라 오류가 발생할 가능성이 크다.

# 구간추정은 이런 점추정의 단점을 보완하기 위한 방법으로 점값이 아닌 모수의 참값이 포함될 것으로 
# 기대하는 구간을 추정하는 방법


# 2. 모비율(P)에 대한 점추정 
# 모비율 P에 대한 추정량은 표본비율 p_hat으로 여기서 p_hat은 모집단으로부터 n개의 확률표본을 추출했을 때 원하는 개수 X의 비율
# 즉, p_hay = X/n이다.
# 여기서 원하는 결과의 개수X는 시행횟수가 n이고, 성공확률이 모집단에서 원하는 결과가 나타날 비율 P인 이항분포를 따르는 확률변수이다.

# E(p_hat) = P
# SE(p_hat) = sqrt(P(1-P)/n)
# 만약 모비율 P를 알지 못하는 경우 표본비율 p_hat을 이용하여 구한다.
# SE(P_hat)_hat = sqrt(p_hat(1-p_hat)/n)

# 예제 5-3) 모비율에 대한 점추정 
# 주사위를 던져 짝수의 눈이 나올 비율을 추정해보자. 표본의 크기가 3이고 복원으로 추출하여 표본비율들의 분포를 구해본다. 
library(prob)

n <- 3
smps.all <- rolldie(n) # 눈이 6인 주사위 던지기의 모든 경우의 수 

is.even <- function(x) return(ifelse(x%%2 == 0, T,F)) 
var.p <- function(x) { # 분산 
  return(sum(x-mean(x))^2 / length(x))
}

p.even <- function(x, s.size=3) {
  return(sum(is.even(x))/s.size)
}

p_hat <- apply(smps.all, 1, p.even)
mean(p_hat)
p.p <- 0.5
var.p(p_hat)
p.p*(1-p.p)/3
sqrt(var.p(p_hat))
