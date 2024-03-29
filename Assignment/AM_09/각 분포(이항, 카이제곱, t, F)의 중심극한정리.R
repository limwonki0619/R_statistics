setwd("D:/limworkspace/R_Statistics/Assignment/AM_9")

# Asignment 9 각 분포의 중심극한정리 적용해보기

library(extrafont)
windowsFonts(dohyeon=windowsFont("BM DoHyeon"))
windowsFonts(jalnan=windowsFont("Jalnan"))
windowsFonts(binggrae=windowsFont("Binggrae Taom"))
windowsFonts(nbg=windowsFont("나눔바른고딕"))

# 1. 이항 분포 B(n, p) : 평균 np, 분산 npq ~ N(np, sqrt(npq/n))
# n = 10, p = 0.1, np = 1, npq = 0.9
n <- 1000
b.2.mean <- rep(NA, n)
b.4.mean <- rep(NA, n)
b.32.mean <- rep(NA, n)
b.64.mean <- rep(NA, n)

for (i in 1:n) {
  b.2.mean[i] <- mean(rbinom(2, size=10, prob = 0.1))
  b.4.mean[i] <- mean(rbinom(4, size=10, prob = 0.1))
  b.32.mean[i] <- mean(rbinom(32, size=10, prob = 0.1))
  b.64.mean[i] <- mean(rbinom(64, size=10, prob = 0.1))
}

par(mar = c(4,4,4,4), oma=c(0,0,4,0)) # 한번에 그리기 및 여백조정으로 toptitle 쓰기
hist(b.2.mean, probability = T, xlim = c(0, 4), col = "Coral1", xlab="", ylab="", family="binggrae",
     main = "표본=2 ~ N(1, sqrt(0.9/2)")
b1 <- seq(min(b.2.mean), max(b.2.mean), length.out = 1000)
y_b1 <- dnorm(x=b1, mean = 1, sd=sqrt(0.9/2))
lines(b1, y_b1, lty=1, lwd=2, col = "black")

hist(b.4.mean, probability = T, xlim = c(0, 4), col = "Coral1", xlab="", ylab="", family="binggrae",
     main = "표본=4")
b2 <- seq(min(b.4.mean), max(b.4.mean), length.out = 1000)
y_b2 <- dnorm(x=b2, mean = 1, sd=sqrt(0.9/4))
lines(b2, y_b2, lty=1, lwd=2, col = "black")

hist(b.32.mean, probability = T, xlim = c(0.3, 1.7), col = "Coral1", xlab="", ylab="", family="binggrae",
     main = "표본=32")
b3 <- seq(min(b.32.mean), max(b.32.mean), length.out = 1000)
y_b3 <- dnorm(x=b3, mean = 1, sd=sqrt(0.9/32))
lines(b3, y_b3, lty=1, lwd=2, col = "black")

hist(b.64.mean, probability = T, xlim = c(0.3, 1.7), col = "Coral1", xlab="", ylab="", family="binggrae",
     main = "표본=64 ~ N(1, sqrt(0.9/64)")
b4 <- seq(min(b.64.mean), max(b.64.mean), length.out = 1000)
y_b4 <- dnorm(x=b4, mean = 1, sd=sqrt(0.9/64))
lines(b4, y_b4, lty=1, lwd=2, col = "black")
mtext("표본갯수 변화에 따른 이항분포", 
      side = 3, # which margin to place text. 1=bottom, 2=left, 3=top, 4=right
      line = 1, # to indicate the line in the margin starting with 0 and moving out
      cex = 2, # font size
      outer = T, # outer = TRUE : to place text at outer margin
      family = "jalnan")




# 2. 카이제곱분포 : 평균 = k, 분산 = 2k ~ N(df, sqrt(2k/n))

n <- 1000
chi.4.mean <- rep(NA, n)
chi.16.mean <- rep(NA, n)
chi.64.mean <- rep(NA, n)
chi.256.mean <- rep(NA, n)

rchisq(4,df=20)

for (i in 1:n) {
  chi.4.mean[i]   <- mean(rchisq(4,  df=20))
  chi.16.mean[i]  <- mean(rchisq(16, df=20))
  chi.64.mean[i]  <- mean(rchisq(64, df=20))
  chi.256.mean[i] <- mean(rchisq(256,df=20))
}

par(mar = c(4,4,4,4), oma=c(0,0,4,0)) # 한번에 그리기 및 여백조정으로 toptitle 쓰기

hist(chi.4.mean, probability = T, xlim=c(-3, 40), col = "cyan2", xlab="", ylab="", family="binggrae",
     main = "표본=4")
chi1 <- seq(min(chi.4.mean), max(chi.4.mean), length.out = 1000)
y_chi1 <- dnorm(x=chi1,20, sqrt(40/4))
lines(chi1, y_chi1, lty=1, lwd=2, col = "black")

hist(chi.16.mean, probability = T, xlim=c(-3, 40), col = "cyan2", xlab="", ylab="", family="binggrae",
     main = "표본=16")
chi2 <- seq(min(chi.16.mean), max(chi.16.mean), length.out = 1000)
y_chi2 <- dnorm(x=chi2,20, sqrt(40/16))
lines(chi2, y_chi2, lty=1, lwd=2, col = "black")

hist(chi.64.mean, probability = T, xlim=c(-3, 40), col = "cyan2", xlab="", ylab="", family="binggrae",
     main = "표본=64")
chi3 <- seq(min(chi.64.mean), max(chi.64.mean), length.out = 1000)
y_chi3 <- dnorm(x=chi3,20, sqrt(40/64))
lines(chi3, y_chi3, lty=1, lwd=2, col = "black")

hist(chi.256.mean, probability = T, xlim=c(-3, 40), col = "cyan1", xlab="", ylab="", family="binggrae",
     main = "표본=256")
chi4 <- seq(min(chi.256.mean), max(chi.256.mean), length.out = 1000)
y_chi4 <- dnorm(x=chi4,20, sqrt(40/256))
lines(chi4, y_chi4, lty=1, lwd=2, col = "black")
mtext("표본갯수 변화에 따른 χ2(20)분포", side = 3, line = 1, cex = 2, outer = T, family = "jalnan")


# 3. t-분포 

# 확률변수 X가 자유도가 k인 t-분포를 따를 때, 기대값과 분산은 다음과 같다
# E(X) = 0, Var(X) = k/(k-2) 단, 자유도가 InF일 때 표준정규분포를 따른다 

# t(InF) ~ N(0, sqrt(1/n)
# t(10000) ~ N(0, sqrt(1/n))

n <- 1000
t.2.mean <- rep(NA, n)
t.4.mean <- rep(NA, n)
t.8.mean <- rep(NA, n)
t.16.mean <- rep(NA, n)

rt(2,df=10000)

for (i in 1:n) {
  t.2.mean[i]  <- mean(rt(2, df=10000))
  t.4.mean[i]  <- mean(rt(4, df=10000))
  t.8.mean[i]  <- mean(rt(8, df=10000))
  t.16.mean[i] <- mean(rt(16,df=10000))
}

par(mar = c(4,4,4,4), oma=c(0,0,4,0)) # 한번에 그리기 및 여백조정으로 toptitle 쓰기
hist(t.2.mean, probability = T, xlim=c(-3, 3), col = "olivedrab1", xlab="", ylab="", family="binggrae",
     main = "표본=2")
t1 <- seq(min(t.2.mean), max(t.2.mean), length.out = 1000)
y_t1 <- dnorm(x=t1, 0, sqrt(1/2))
lines(t1, y_t1, lty=1, lwd=2, col = "grey40")

hist(t.4.mean, probability = T, xlim=c(-3, 3), col = "olivedrab1", xlab="", ylab="", family="binggrae",
     main = "표본=4")
t2 <- seq(min(t.4.mean), max(t.4.mean), length.out = 1000)
y_t2 <- dnorm(x=t2, 0, sqrt(1/4))
lines(t2, y_t2, lty=1, lwd=2, col = "grey40")

hist(t.8.mean, probability = T, xlim=c(-3, 3), col = "olivedrab1", xlab="", ylab="", family="binggrae",
     main = "표본=8")
t3 <- seq(min(t.8.mean), max(t.8.mean), length.out = 1000)
y_t3 <- dnorm(x=t3, 0, sqrt(1/8))
lines(t3, y_t3, lty=1, lwd=2, col = "grey40")

hist(t.16.mean, probability = T, xlim=c(-3, 3), col = "olivedrab1", xlab="", ylab="", family="binggrae",
     main = "표본=16")
t4 <- seq(min(t.8.mean), max(t.8.mean), length.out = 1000)
y_t4 <- dnorm(x=t4, 0, sqrt(1/16))
lines(t4, y_t4, lty=1, lwd=2, col = "grey40")
mtext("표본갯수 변화에 따른 t(InF)분포", side = 3, line = 1, cex = 2, outer = T, family = "jalnan")



# 4. F-분포
# 두 개의 독립인 카이제곱분포의 비율을 이용하는 것으로 두 모집단의 분산 비율을 알고자 할 때 사용 
# 확률변수 X가 자유도(df1, df2)인 F-분포를 따를 때 기댓값과 분산 
# E(X) = df2 / (df2-2) 단 , m >= 3
# Var(X) = (2*(df2^2)*(df1+df2-2)) / ((df1(df2-2))^2 * (df2-4))

df1 <- 100
df2 <- 100
f_ex <- df2 / (df2-2)
f_var <- (2*(df2^2)*(df1+df2-2)) / (df1*((df2-2)^2)*(df2-4))

# F(100, 100) ~ N(1, sqrt(f_var/n))

n <- 1000
f.2.mean <- rep(NA, n)
f.4.mean <- rep(NA, n)
f.8.mean <- rep(NA, n)
f.16.mean <- rep(NA, n)

rt(2,df=10000)

for (i in 1:n) {
  f.2.mean[i]  <- mean(rf(2, df1=df1, df2=df2))
  f.4.mean[i]  <- mean(rf(4, df1=df1, df2=df2))
  f.8.mean[i]  <- mean(rf(8, df1=df1, df2=df2))
  f.16.mean[i] <- mean(rf(16,df1=df1, df2=df2))
}

par(mar = c(4,4,4,4), oma=c(0,0,4,0)) # 한번에 그리기 및 여백조정으로 toptitle 쓰기
hist(f.2.mean, probability = T, xlim=c(0, 2), col = "pink1", xlab="", ylab="", family="binggrae",
     main = "표본=2")
f1 <- seq(min(f.2.mean), max(f.2.mean), length.out = 1000)
y_f1 <- dnorm(x=f1, 1, sqrt(f_var/2))
lines(f1, y_f1, lty=1, lwd=2, col = "black")

hist(f.4.mean, probability = T, xlim=c(0, 2), col = "pink1", xlab="", ylab="", family="binggrae",
     main ="표본=4")
f2 <- seq(min(f.4.mean), max(f.4.mean), length.out = 1000)
y_f2 <- dnorm(x=f2, 1, sqrt(f_var/4))
lines(f2, y_f2, lty=1, lwd=2, col = "black")

hist(f.8.mean, probability = T, xlim=c(0, 2), col = "pink1", xlab="", ylab="", family="binggrae",
     main = "표본=8")
f3 <- seq(min(f.8.mean), max(f.8.mean), length.out = 1000)
y_f3 <- dnorm(x=f3, 1, sqrt(f_var/8))
lines(f3, y_f3, lty=1, lwd=2, col = "black")

hist(f.16.mean, probability = T, xlim=c(0, 2), col = "pink1", xlab="", ylab="", family="binggrae",
     main = "표본=16")
f4 <- seq(min(f.16.mean), max(f.16.mean), length.out = 1000)
y_f4 <- dnorm(x=f4, 1, sqrt(f_var/16))
lines(f4, y_f4, lty=1, lwd=2, col = "black")
mtext("표본갯수 변화에 따른 F(100,100)분포", side = 3, line = 1, cex = 2, outer = T, family = "jalnan")


