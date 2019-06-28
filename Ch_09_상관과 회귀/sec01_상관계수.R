setwd("D:/limworkspace/R_Statistics/Ch_09_상관과 회귀")

# Section 1. 상관계수 

# 1. 공분산 (Covariance)

# 두 변수 사이의 상관의 정도를 나타냄 
# 두 확률변수 X, Y의 공분산은 Cov(X, Y)로 표기하고, 공분산이 갖는 값에 따라 두 확률변수의 관계를 확인할 수 있다.


#     Cov(X, Y) > 0 : X와 Y의 변화가 같은 방향임을 나타낸다
#     Cov(X, Y) < 0 : X와 Y의 변화가 다른 방향임을 나타낸다
#     Cov(X, Y) = 0 : X와 Y간에 어떠한 (선형)관계가 없음을 나타낸다.


# 2. 상관계수 ρxy

# 두 확률변수 X, Y의 공분산을 각 확률변수의 표준편차 간의 곱으로 나눈 값을 피어슨의 모상관계수라 부른다.
# 모 상관계수는 -1 <= ρxy <= 1의 값을 가지며, 공분산의 성질을 그대로 이어받아 
# 두 변수 간의 변화의 방향이 같으면 양수, 다르면 음수를 갖는다.
# 다만, 모 상관계수는 모집단의 특성 중 하나로 일반적으로 알 수 없으며
# 두 확률변수로부터 추출한 표본의 특성을 통해 구하는 피어슨의 표본상관계수를 이용하여 추정한다.

# cov(x, y) = sigma(i=1 ~ n) (xi-x_bar)(yi-y_bar) / n - 1
# 표본상관계수 r = cov(x, y) / s[x] s[y  ┃ page 332

# 표본상관계수 r은 모상관게수와 동일한 성질을 가지며, r이 ┃1┃에 가까울수록 강한 상관을 나타내고,
# 0에 가까이 갈수록 약한 상관을 나타낸다. 

# 예제 9-1) 아버지와 아들키의 공분산과 상관계수 ---------------------------------------------------------

# R 상관계수 직접 계산 -------------------------- 
hf <- read.table("http://www.randomservices.org/random/data/Galton.txt", header = T, stringsAsFactors = F)
str(hf)

hf$Gender <- factor(hf$Gender, levels = c("M","F"))
hf.son <- subset(hf, Gender=="M")  
hf.son <- hf.son[c("Father","Height")]
str(hf.son)

f.mean <- mean(hf.son$Father)
s.mean <- mean(hf.son$Height)

cov.num <- sum( (hf.son$Father-f.mean ) * ( hf.son$Height - s.mean ))

# R 함수를 이용한 공분산 (Covariance)
cov.xy <- cov.num / (nrow(hf.son)-1) # 표본공분산 
cov(hf.son$Father, hf.son$Height)    # 표본공분산 


# R 함수를 이용한 상관계수(Correlation)
r.xy <- cov.xy / (sd(hf.son$Father) * sd(hf.son$Height)) # 표본상관계수 
cor(hf.son$Father, hf.son$Height)                        # 표본상관계수 


