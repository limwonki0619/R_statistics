# Section 2. 회귀분석 https://rstudio-pubs-static.s3.amazonaws.com/190997_40fa09db8e344b19b14a687ea5de914b.html

# 0. 분석 절차 
#    1) 모델 확인
#    2) summary
#    3) plot으로 회귀모델 가정 확인
#    4) AIC

# 1. 독립변수(설명변수)와 종속변수(반응변수) -------------------------------------------------------

# 종속변수 : 변수간의 관계에서 다른 변수에 의해 영향을 받아 그 값이 결정되는 변수
# 독립변수 : 영향을 미치는 변수

# 원인과 결과 관계를 뜻하는 인과관계는 상관관계처럼 계산을 통해 구하는 것이 아닌,
# 주의 깊은 자료의 관찰을 통해 얻을 수 있는 관계다.
# 다시말해 사회현상은 관찰연구(Observational study)를 많이 사용하는데 이 경우에는 실험을 통제할 수 없음을 인정하고,
# 사전지식과 사회에 대한 깊은 통찰력을 가져야 한다. 
# 즉, 연구를 진행하기 앞서 다음과 같은 고민을 가져야 할 필요가 있다.

# (1) 두 변수의 연관성 : 미세먼지와 교통사고 간의 연관을 따진느 것이 타당한지 살펴본다
# (2) 원인과 결과에 대한 고민 : 뒤바뀐 인과관계는 아닌지 살펴본다
# (3) 제3의 요인 : 종속변수에 또 다른 요인이 영향을 주지는 않는지 고려해볼 필요가 있다. 



# 2. 단순선형회귀분석 (독립변수가 1개 ) 338p ----------------------------------------------------------
# linear Regression (선형회귀-직선형태)


# |  요인  |            자유도          | 제곱합(SS) | 평균제곱합(MS) | 분산비(F(1, n-2))
# ----------------------------------------------------------------------------------------- 
# |  회귀  |   1 (회귀계수의 수 - 1)    |     SSR    |   SSR / 1      |     MSW
# |  잔차  |  n-2 (n - 회귀계수의 수)   |     SSE    |   SSE / n-2    |     MSB
# |   합   |  n-1                       |     SST    |                | 


hf <- read.table("http://www.randomservices.org/random/data/Galton.txt", header = T, stringsAsFactors = F)
str(hf)

hf$Gender <- factor(hf$Gender, levels = c("M","F"))
hf.son <- subset(hf, Gender=="M")  
hf.son <- hf.son[c("Father","Height")]
str(hf.son)

mean.x <- mean(hf.son$Father)
mean.y <- mean(hf.son$Height)

s_xy <- sum( (hf.son$Father - mean.x)*(hf.son$Height - mean.y) )
s_xx <- sum( (hf.son$Father - mean.x)^2)

b1 <- s_xy / s_xx
b0 <- mean.y - b1 * mean.x

# R에서 lm()함수 이용 - 단순회귀분석 
out <- lm(Height ~ Father, data=hf.son) # lm(y ~ x)
summary(out)
# Height = 38.2589 + 0.44775Father # 회귀추정식 

# 회귀모델 가정 확인 *** 
par(mfrow = c(2,2))
plot(out)
par(mfrow = c(1,1))


# R base chart
plot(hf.son$Height, hf.son$Father)
abline(out, col="red")

# R ggplot chart 
ggplot(hf.son, aes(x=Height, y=Father)) +
  geom_point() +
  geom_smooth(method = lm, color="orange") + # 회귀선 및 신뢰구간 
  theme_bw() 

# *** 잔차분석을 통한 회귀분석의 가정 확인 ------------------------------------------------------------- 

# 회귀분석을 통해 모형을 구축하기 위해서는 기본적인 가정들을 만족해야 한다.

# (1) 독립성   : 오차(e_{i})는 서로 독립이다.
#                독립변수 상호간에는 상관관계가 없어야 한다. (다중회귀의 중요한 가정)

# (2) 선형성   : 오차항의 평균(기대값)은 0이다. 1 chart

# (3) 정규성   : 오차(e_{i})는 평균이  이고 분산이 σ^2인 정규분포를 따른다. 2-chart
#                수집된 데이터의 확률 분포는 정규분포를 이루고 있다. shaporo.test(residuals(out))

# (4) 등분산성 : 오차(e_{i})의 분산은 σ^2으로 모두 동일하다. 3-chart
#                오차항은 모든 독립변수 값에 대하여 동일한 분산을 갖는다.

# (5)          : 시간에 따라 수집한 데이터들은 잡음의 영향을 받지 않아야 한다. (시계열 자료)



