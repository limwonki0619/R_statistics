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


# R 분포함수의 첫글자(의미) : 모든 분포에 적용가능 --------------------------

# |          역할       |    Prefix  |              전달인자                |
# |---------------------|------------|--------------------------------------|
# | 확률(질량/밀도)함수 |  d + 분포  | x | 확률을 구할 값, P(X=x)           |
# |       분포함수      |  p + 분포  | x | 확률을 구할 값, P(X <= x)        |
# |      분위수함수     |  q + 분포  | q | 알고 싶은 분위수, P(X <= x) = q  |
# |       난수생성      |  r + 분포  | n | 생성할 난수의 개수               |



# Section 2. 확률 분포 

# 2.1 베르누이 시행 : 기댓값 = p, 분산 = pq ------------------------------------------------------------
# p의 확률로 원하는 결과가 나타났을 때 '성공'으로, 
# (1-p)의 확률로 그렇지 않은 결과가 나타났을 때 '실패'로 하는 두 가지 결과가 나타나는 확률실험 


# 2.2 이항분포 : 기댓값 = np, 분산 = npq ---------------------------------------------------------------

# 성공 확률이 p로 동일한 베르누이 시행을 n번 반복해서 하는 실험에서
# 각 실험이 서로 독립적으로 시행되며 이와 같은 실험에서 성공 횟수가 따르는 분포함수를 이항분포라고 한다. 
# 확률변수 X가 이항분포를 따를 때 X ~ B(n, p)로 나타낸다.
# nCx = n! / (x! * n-x!)

# X ~ B(3,1/3)
# X = 0 : 3C0 (2/3)^3          = 8/27
# X = 1 : 3C1 (1/3)^1 (2/3)^2  = 12/27
# X = 2 : 3C2 (1/3)^2 (2/3)^1  = 6/27
# X = 3 : 3C3 (1/3)^3          = 1/27

# R을 이용한 이항분포 계산
# X ~ B(6,1/3)
n <- 6
p <- 1/3

x <- 0:n

# 이항분포의 확률질량함수 : dbinom(x, size, prob)
# x : 이항분포의 성공 횟수
# size : 시행횟수
# prob : 성공의 확률 

# dbinom(x, size, prob)
# pbinom(q, size, prob)
# qbinom(p, size, prob)
# dbinom(n, size, prob)

dbinom(2, size = n, prob = p) # P(X=2) / x는 확률변수 X의 값 
dbinom(4, size = n, prob = p) # P(X=4)
px <- dbinom(x, size=n, prob=p); px
plot(x, px, type="s", xlab="성공횟수", ylab="확률(P[X=x])",
     main = "X ~ B(6, 1/3)")

plot(x, px, type="h", xlab="성공횟수", ylab="확률(P[X=x])", lwd = 10, col = "olivedrab4",
     main = "X ~ B(6, 1/3)")

pbinom(2, n, p) # P(X <= 2) = P(X=0) + P(X=1) + P(X=2)
pbinom(4, n, p) # P(X <= 4) = P(X=0) + P(X=1) + P(X=2) + P(X=3) + P(X=4)

pbinom(4, n, p) - pbinom(2, n, p) # P(X=3) + P(X=4) 
dbinom(3, n, p) + dbinom(4, n, p)

qbinom(0.1, n, p) # 누적확률이 0.1이 될 때의 X값 
qbinom(0.5, n, p) # 누적확률이 0.5가 될 때의 X값 

?rbinom
rbinom(10, n, p) # B(6 ,1/3)을 따르는 모집단으로부터 10개의 확률표본 추출 

# R의 분포함수를 이용한 기댓값과 분산
n <- 6
p <- 1/3
x <- 0:n
px <- dbinom(x, size=n, prob=p)

Ex <- sum(x * px)    # E(X)
Ex2 <- sum(x^2 * px) # E(X^2)
var <- Ex2 - Ex^2    # E(X^2) - [E(X)]^2

n*p       # 이항분포의 기댓값
n*p*(1-p) # 이항분포의 분산 

# p = 0.3 일 때 시횡횟수 n의 변화에 따른 이항분포의 변화 : n이 충분히 크다면 이산형이 아닌 연속형 처럼 다루는 것이 가능함
x <- 0:30
{plot(x=x, y=dbinom(x, size = 5, prob = 0.3), type="l", col="olivedrab", lwd = 3,
     main= paste0("p = 0.3 일 때","\n","시횡횟수 n의 변화에 따른 이항분포의 변화"))
lines(x=x, y=dbinom(x, size = 10, prob = 0.3), type="l", col="royalblue", lwd = 3 )
lines(x=x, y=dbinom(x, size = 30, prob = 0.3), type="l", col="Coral", lwd = 3)
lines(x=x, y=dbinom(x, size = 50, prob = 0.3), type="l", col="palevioletred4", lwd = 3)
legend("topright", legend=c("n=5", "n=10", "n=30", "n=50"), pch=15, col=c("olivedrab", "royalblue", "Coral", "palevioletred4"))
}


# 정규분포 : 확률변수 X가 정규분포를 따를 때 X ~ N(μ, σ^2)로 나타낸다. ------------------------------

# 종 모양의 형태를 가진다. 양 끝이 아주느린 속도로 감소하지만, 축에 닿지않고 -Inf ~ Inf 까지 계속된다 
# 평균을 중심으로 좌우대칭이다
# 평균 주변에 많이 몰려 있으며, 양 끝으로 갈수록 줄어든다
# 평균과 표준편차로 분포의 모양을 결정한다 
# 표준화 : Z = X - μ / σ : Z ~ N(0,1^2) 

x <- seq(-3,3,0.01)
plot(x=x, y=dnorm(x, mean=0, sd=1), type="l", col="olivedrab", lwd = 3,
     main= paste0("정규분포"))
lines(x=x, dnorm(x, mean = 1, sd=1), col="Coral", lwd=3)
lines(x=x, dnorm(x, mean = 0, sd=3), col="royalblue", lwd=3)
legend("topleft", legend=c("mean=0, sd=1", "mean = 1, sd=1", "mean = 0, sd=3"), pch=15, col=c("olivedrab", "Coral", "royalblue"))


# ggplot2로 그려보기 1 : 평균 변화에 따른 정규분포 변화 
library(dplyr)
library(reshape)
axis_x <- seq(-3,3,by=0.001)
norm_mean_move <- data.frame(x = axis_x,
                             mu_0_sd_1 = dnorm(x=axis_x, 0, 1),
                             mu_0.5_sd_1 = dnorm(x=axis_x, 0.5, 1),
                             mu_1_sd_1 = dnorm(x=axis_x, 1, 1))

melt_norm_mean_move <- melt(norm_mean_move, id=c("x"))
ggplot(melt_norm_mean_move, aes(x=x,y=value,group=variable, color=variable)) +
  geom_line(lwd=1.5) + 
    theme_bw(base_family = "dohyeon", base_size = 15) +
    labs(title = "평균 변화에 따른 정규분포 변화") +
    theme(plot.title = element_text(hjust = 0.5)) +
    theme(axis.title = element_blank(),
          legend.title = element_blank(),
          legend.position = "top")
  

# ggplot2로 그려보기 2 : 표준편차 변화에 따른 정규분포 변화
axis_x <- seq(-3,3,by=0.001)
norm_sd_move <- data.frame(x = axis_x,
                           mu_0_sd_1 = dnorm(x=axis_x, 0, 1),
                           mu_0_sd_0.5 = dnorm(x=axis_x, 0, 0.5),
                           mu_0_sd_2 = dnorm(x=axis_x, 0, 2))

melt_norm_sd_move <- melt(norm_sd_move, id=c("x"))
ggplot(melt_norm_sd_move, aes(x=x,y=value,group=variable, color=variable)) +
  geom_line(lwd=1.5) + 
  theme_bw(base_family = "dohyeon", base_size = 15) +
  labs(title = "표준편차 변화에 따른 정규분포 변화") +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(axis.title = element_blank(),
        legend.title = element_blank(),
        legend.position = "top")


# 예제 3-5. R을 이용한 정규분포 계산 
options(digits = 3)

mu <- 170
sd <- 6
ll <- mu - 3*sd
ul <- mu + 3*sd

x <- seq(ll, ul, by=0.01)
nd <- dnorm(x, mean=mu, sd=sd)
plot(x, nd, type="l", xlab="x", ylab="P(X=x)", lwd=2, col="red",
     main = "평균 170, 표준편차 6인 정규분포")

pnorm(mu, mean=mu, sd=sd)
pnorm(158, mean=mu, sd=sd)                              # 키가 158 이하인 사람들이 포함될 확률 
pnorm(180, mean=mu, sd=sd) - pnorm(160, mean=mu, sd=sd) # 키가 180이하~160이상인 사람들이 포함될 확률 
qnorm(0.25, mean = mu, sd=sd)                           # 누적면적이 0.25(1분위수)가 되는 키 
qnorm(0.75, mean = mu, sd=sd)                           # 누적면적이 0.75(3분위수)가 되는 키

options(digits = 5)
set.seed(5)
smp <- rnorm(400, mean = mu, sd=sd)
c(mean(smp), sd(smp))
hist(smp, probability = T, main = "N(170, 6^2)으로부터 추출한 표본의 분포(n=400)",
     xlab="", ylab="", col="white", border = "black")
lines(x, nd, lty=2, lwd=2, col="red")


# 예제 3-6. 정규분포의 특징 
options(digits = 4)
mu <- 0
sd <- 1
p0.05 <- qnorm(0.05, mean=mu, sd=sd); p0.05    # 90% 신뢰수준
p0.025 <- qnorm(0.025, mean=mu, sd=sd); p0.025 # 95% 신회수준 

pnorm(1.645) - pnorm(-1.645) # 누적확률
pnorm(1.96) - pnorm(-1.96)   # 누적확률 


# 그림 3-17. 표준정규분포에서 확률이 90%가 되는 점(양측) 표시하기 
z <- seq(-3, 3, by=0.001)
z.p <- dnorm(z)
plot(z, z.p, axes=F, type="l",
     main = "표준정규분포 (95%)", ylab="", ylim=c(-0.04,0.4)) # 기본 차트
axis(1) # x 축 그리기 

lines(c(-3,3),c(0,0))
points(-1.96, -0.02, pch=17, col="coral")
points(1.96, -0.02, pch=17, col="coral")
text(1.96, -0.035, "-1.96", col="coral")
text(-1.96, -0.035, "-1.96", col="coral")

s <- seq(-1.96,1.96,by=0.001)
s.z <- dnorm(s, mean = 0, sd=1)
s <- c(-1.96, s, 1.96)
s.z <- c(0, s.z, 0)
polygon(s, s.z, col="Coral", density = 10, angle = 305)
