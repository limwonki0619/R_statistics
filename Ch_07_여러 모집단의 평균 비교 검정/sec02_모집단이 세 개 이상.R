setwd("D:/limworkspace/R_Statistics/Ch_07_여러 모집단의 평균 비교 검정")

# Section 2. 모집단이 세 개 이상 : ANOVA (F-분포)

# 1. 일원배치분산분석 (독립변수[요인]가 1개 인 경우)

# < 일원배치분산분석표 > 독립변수 1, 종속변수 1

# |  요인    | 자유도 | 제곱합(SS) | 평균제곱합(MS) | 분산비(F(k-1, n-k))
# -----------------------------------------------------------------------
# |  처리(k) |  k-1   |  SSW (SSt) |   SSW / k-1    |     MSW
# |  오차    |  n-k   |  SSB (SSE) |   SSB / n-k    |     MSB
# |  전체    |  n-1   |  SST       |                | 


# R을 이용한 검정 - 직접계산  
ad <- read.csv("data/age.data.csv")
str(ad)

ad$scale <- factor(ad$scale)
ad$sex <- factor(ad$sex)
str(ad)

y1 <- ad$age[ad$scale == "1"]
y2 <- ad$age[ad$scale == "2"]
y3 <- ad$age[ad$scale == "3"]

y1.mean <- mean(y1)
y2.mean <- mean(y2)
y3.mean <- mean(y3)

sse.1 <- sum((y1 - y1.mean)^2)
sse.2 <- sum((y2 - y2.mean)^2)
sse.3 <- sum((y3 - y3.mean)^2)
sse <- sse.1 + sse.2 + sse.3 # 오차제곱합[집단 간 제곱합] (SSE) or (SSB ) 
df.e <- (length(y1)-1) + (length(y2)-1) +(length(y3)-1) # 오차제곱합 자유도 (n-k)

y.mean <- mean(ad$age)
sst.1 <- length(y1) + sum((y1.mean - y.mean)^2)
sst.2 <- length(y2) + sum((y2.mean - y.mean)^2)
sst.3 <- length(y3) + sum((y3.mean - y.mean)^2)
sst <- sst.1 + sst.2 + sst.3 # 처리제곱합[집단 내 제곱합] (SSt) or (SSW)
df.t <- length(levels(ad$scale)) - 1 

mst <- sst/df.t # MSW (MSt)
mse <- sse/df.e # MSB (MSE)

f <- mst/mse # F(k-1, n-k) 검정통계량 

qf(0.95, 2,147)
tol <- qf(1-0.05,2,147) # 양측 검정에서 임계치 

p_value <- 1-pf(f, 2, 147) # 유의확률 5%에서 우측 검정시 유의확률 


# R을 이용한 검정 - lm, anova 함수 사용 ------------------------------------------
# lm(종속변수 ~ 독립변수)

ow <- lm(age~scale, data=ad)
anova(ow)

# 결론

# 지역의 규모에 따라 나이의 평균에 차이가 나는지 규모별로 50명, 총 150명의 표본추출을 통해 확인한 결과,
# 지역규모 1의 나이의 평균과 표준편차는 45.94±14.46
# 지역규모 2의 나이의 평균과 표준편차는 45.68±13.59
# 지역규모 3의 나이의 평균과 표준편차는 47.92±14.88로 나타났다.
# 또한 일원분산분석을 통해 나이 차이를 검정한 결과 검정통계량 0.366, 유의확률 0.6941로 
# 유의수준 5%에서 통계적으로 유의한 차이를 보이지 않았다. 즉, 지역규모에 따라 나이의 평균은 차이가 나지 않는다.



