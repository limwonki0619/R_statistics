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

# 1. 추정량(estimator) ----------------------------------------------------------------------------------------------------------------------
# 알고자 하는 모수를 추측하기 위해 표본으로부터 관찰된 값으로 계산되는 표본의 통계량 
# 표본으로부터 관측된 자료를 통해 계산된 추정량의 결과를 추정치(estimate)라고 한다.


# 1.1 불편성(unbiasedness)
# 불편성은 추정량이 갖춰야 할 가장 기본적인 성질로 치우치지 않음을 의미한다.
# 치우쳐지지 않았다는 것은 추정량의 기댓값이 모수와 같음을 의미하고 이를 불편성을 만족하는 추정량으로 불편추정량(unbiased estimator)으로 불린다.
# E(θ_hat) = θ

# 1.2 유효성(efficiency)
# 특정 모수에 대한 불편추정량은 한 가지 이상 존재할 수 있다.
# 여러개의 불편추정량이 있을 때 좋은 추정량을 결정하는 것은 유효성이다.
# 모수 θ에 대한 모든 불편추정량 중에서 분산이 가장은 추정량을 모수 θ에 대한 최소분산불편추정량(MVUE, Minimun Variance Unbiased Estimator)이라 한다.

# 예제 5-1) 유효성 --------------------------------------------------------------------------------------------------------------------------

# Y1_bar = X1+X2+X3 / 3
# Y2_bar = X1+ 2*X2 + 3*X3 / 6
# 두 개의 추정량 Y1_bar와 Y2_bar의 분산은 각각 1/3*σ^2, 7/18*σ^2으로 Y1_bar의 분산이 근소하게 작음을 보았다.
# 모집단이 표준정규분포인 경우 두 추정량의 분포도를 작성하고 모양을 확인해보자.

x <- seq(-3,3,0.01)
y <- dnorm(x)
y1_bar <- dnorm(x, sd=sqrt(1/3))
y2_bar <- dnorm(x, sd=sqrt(7/18))

par(mfrow=c(1,1))
plot(x, y, type = "l", ylim = c(0, 0.8), ylab="", lwd=3, col="yellow") # 표준정규분포
lines(x, y1_bar, col="red", lwd=3) # y1_bar 분포 : y2보다 평균 주위에 몰려있음 
lines(x, y2_bar, col="green", lty=2, lwd=3) # y2_bar 분포

# 예제 5-2) 유효성 모의실험 : Y1_bar를 구하기 위한 함수 -------------------------------------------------------------------------------------

mean.seq <- function(x) { # Y2 = (1*x1 + 2*x2 + 3*x3)/6 을 계산하기 위한 함수 
  n <- length(x)
  sum <- 0
  n2 <- 0
  for (i in 1:n) {
    new_x <- i*x[i]
    sum <- sum+new_x
    n2 <- n2 + i
  }
  return(sum / n2)
}

y1 <- rep(NA, 1000) # 빈 변수 생성 
y2 <- rep(NA, 1000)

for (i in 1:1000) { # 난수 1000개 생성 
  smp <- rnorm(3)
  y1[i] <- mean(smp)
  y2[i] <- mean.seq(smp)
}

n1 <- length(y1[(y1 > -0.1) & ( y1 < 0.1 )]) # 3개로 구성된 표본평균 1000개가 저장된 y1에서 그 값이 -0.1보다 크고 0.1보다 작게 나온 횟수
n2 <- length(y2[(y2 > -0.1) & ( y2 < 0.1 )]) # Y2의 값 1000개가 저장된 y2에서 그 값이 -0.1보다 크고 0.1보다 작게 나온 횟수 

data.frame(mean=mean(y1), var=var(y1), n=n1) # y1과 y2비교 : y1이 y2보다 중심에 더 몰려있음을 확인 
data.frame(mean=mean(y2), var=var(y2), n=n2) # 따라서 y1이 더 유효한 추정량이 됨

par(mfrow=c(1,2))
hist(y1, probability = T, xlim=c(-2,2), ylim=c(0, 0.65), main = "(x1+x2+x3)/3", xlab="", col="orange") # y1이 y2보다 평균 주변의 높이가 높음(더 좋은 추정량) 
hist(y2, probability = T, xlim=c(-2,2), ylim=c(0, 0.65), main = "(1*x1 + 2*x2 + 3*x3)/6", xlab="", col="orange")


# 모수θ에 대한 불편추정량 θ_hat1, θ_hat2에 대해 각각의 분산을 Var(θ_hat1), Var(θ_hat2)라 할 때,
# 다음을 만족하면 θ_hat1이 θ_hat2보다 더 유효한 추정량이라고 한다.
#  Var(θ_hat1) < Var(θ_hat2)

# 1.3 일치성(consistency) ------------------------------------------------------------------------

# 일치성은 표본의 크기와 관련이 있는 추정량의 성질로 다음을 만족하는 추정량을 일치추정량이라 한다.

# 일치추정량 
# 모수 θ에 대한 추정량이 θ_hat이라 할 때, 임의의 양수 ε에 대해 다음을 만족하면 θ_hat은 θ에 대한
# 일치추정량 이라 한다. p197
# lim(x->∞)P(|θ_hat - θ| > ε) = 0




# 2. 표준오차(standard error) --------------------------------------------------------------------

# 추정에서는 추정량의 표준편차에 대해 그 ㄱ밧이 작으면 모평균 추정에 대한 신뢰도가 높아지고 
# 크면 신뢰도가 낮아지게 되므로 작을수록 좋은 개념은 오차를 사용하는데, 이를 표준오차라고 한다.
# 즉, 추정에서 표준오차는 추정량의 신뢰도를 나타내는 역할을 한다.

# 표준오차는 SE(0_hat)으로 표기하며, 모평균 추정에서 표본평균의 표준오차는 다음과 같다.
# SE(0_hat) = σ/sqrt(n) : 제곱근이기에 표준오차를 반으로 줄이려면 표본은 4배가 필요 

# 모집단의 표준편차는 계산할 수 없는 경우가 많기 때문에, 표준오차 역시 우리가 추정해야할 추정량이다.
# 따라서 표준오차의 추정은 모집단 표준편차 대신 표본으로부터 관찰하는 표본표준편차를 추정량으로 사용한다.
# SE(0_hat)_hat = s/sqrt(n)















