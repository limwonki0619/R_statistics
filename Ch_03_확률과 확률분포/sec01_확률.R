# Chapter 3. 확률과 확률분포

setwd("D:/limworkspace/R_Statistics/Ch_03")

library(extrafont)

windowsFonts(hanna=windowsFont("BM HANNA 11yrs old")) # 폰트이름 변경 
windowsFonts(dohyeon=windowsFont("BM DoHyeon"))
windowsFonts(jalnan=windowsFont("Jalnan"))
windowsFonts(tb=windowsFont("TmonMonsori Black"))
windowsFonts(binggrae=windowsFont("Binggrae Taom"))
windowsFonts(nbg=windowsFont("나눔바른고딕"))

library(dplyr)
library(ggplot2)

# Section 1. 확률 

install.packages("prob")
library(prob)

tosscoin(5)                                  # n횟수만큼의 모든 동전던지기 경우의 수   
rolldie(1)                                   # n횟수만큼의 모든 주사위 던지기 경우의 수
urnsamples(1:3, size=2)                      # size 횟숨만큼 구슬 뽑기 (구슬 갯수 3개) 
urnsamples(1:3, size=2, replace = T)         # 복원추출 
urnsamples(c(rep("R",3),rep("B",2)), size=2) # 5C2 = 10 
tosscoin(2, makespace = T)                   # 각 경우의 수에 대한 확률까지 출력


# 확률변수의 평균(기댓값)과 분산 
# 동전을 두 번 던져 앞면이 나오는 사건 

# 기대값 : sigma(모든 x) * P(X=x)
x <- 0:2               # 확률변수 X 
px <- c(1/4, 2/4, 1/4) # 확률 (P(X=x))
Ex <- sum(x*px)        # X의 기댓값  

# 분산 : E(X^2) - [E(X)]^2
x2 <- x^2            # X제곱 
Ex2 <- sum(x2*px)    # X제곱의 기댓값 
VAR.x <- Ex2 - Ex^2  # 분산 


