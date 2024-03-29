setwd("D:/limworkspace/R_Statistics/Assignment/AM_10")


# 모비율 추정 -------------------------------------------------------------- 

# 문제 1 ---------------------------------------

# 어느 대학교에서 대중교통을 이용하여 등교하는 학생의 비율을 알아보기 위하여 
# 이 학교 학생 중 n 명을 임의 추출하여 조사한 결과 50%의 학생이 대중교통을 이용하여 등교하는 것으로 나타났다. 
# 이 결과를 이용하여 이 대학교 전체 학생 중에서 대중교통을 이용하여 등교하는 학생의 비율 p에 대한 신뢰도 95%의 신뢰 구간을 구하시오.

# n이 충분히 크다면 p_hat과 p_0의 차이가 없고, 표본비율 p_hat은 근사적으로 정규분포를 따른다. 
# 표본비율은 N(p, pq/n)이 되고,  Z = p_hat - p_0 / sqrt( (p*q)/n )가 근사적으로 표준정규분포 N(0,1)을 따른다. 
# 이 때 신뢰구간 95%에서 모비율의 신뢰구간은 
# P( -k <= Z <= k)에서 Z를 간소화하면 p = p_0, q = (1-p_0)
# P( -k <= p_hat - p_0 / sqrt( (p*q)/n ) -> P(p_hat - ( k*sqrt((p*q)/n) )) <= p <= p_hat + ( k*sqrt((p*q)/n) )이 된다

# 식에 따라 게산하면, 
n <- 1000
p_hat <- 0.5
p_0 <- 500/1000
k <- 1.96
p_hat - (k*sqrt( (p_0*(1-p_0))/n )) # 신뢰구간 하한
p_hat + (k*sqrt( (p_0*(1-p_0))/n )) # 신뢰구간 상한 

# 모비율 검정 함수로 계산하면, 
prop.test(500, 1000, 0.5, alternative = "two.sided", conf.level = 0.95)


# 문제 2 ---------------------------------------

# 어느 음식점에서 새로운 메뉴를 개발하여 이 메뉴에 대한 선호도를 조사하기로 하였다.
# 고객 100명을 임의추출하여 이 메뉴에 대한 반응을 조사하였더니 이들 중 4/5가 선호한다고 하였다. 
# 전체 고객의 새로운 메뉴에 대한 선호도를 p라 할 때, 모비율 p에 대한 신뢰도 95%의 신뢰구간을 구하시오.

# 여기서도 표본의 갯수가 충분하다고 가정하면, p_hat과 p_0의 차이가 없다고 가정하고 계산한다. 
n <- 1000
p_hat <- 4/5
p_0 <- 800/1000

# 직접 계산하면,
0.8 - (1.96*sqrt( (0.8*0.2)/1000 ))
0.8 + (1.96*sqrt( (0.8*0.2)/1000 ))

# 모비율 검정함수로 계산하면,
prop.test(800, 1000, 4/5, alternative = "two.sided", conf.level = 0.95)



# 문제 3 ---------------------------------------

# 우리나라 성인 남성의 흡연율을 조사한다고 한다. 이에 성인 남자 1,000명을 무작위로 뽑아 흡연 여부를 조사하였더니, 
# 430명이 흡연을 하고 있었다. 이때 흡연율(모비율)에 대한 90% 신뢰구간을 추정하시오.

# 여기서도 마찬가지,
n = 1000
p_hat = 0.43
p_0 = 430/1000

qnorm(0.9)

0.43 - (1.28*sqrt( (0.43*(1-0.43))/1000 )) # 신뢰구간 하한 
0.43 + (1.28*sqrt( (0.43*(1-0.43))/1000 )) # 신뢰구간 상한 

prop.test(430, 1000, 0.43, alternative = "two.sided", conf.level = 0.9)


# One-sample t.test --------------------------------------------------------------------------------

# 문제 1 ---------------------------------------

# A회사의 건전지의 수명시간이 1000시간 일 때, 무작위로 뽑은 10개의 건전지에 대한 수명은 다음과 같다. 
# 980, 1008, 968, 1032, 1012, 996, 1021, 1002, 996, 1017 샘플이 모집단과 같다고 할 수 있는가?

# Shapiro-Wilk test 
# 오차항이 정규분포를 따르는지 알아보는 검정으로, 회귀분석에서 모든 독립변수에 대해서 종속변수가 정규분포를 따르는지 알아보는 방법이다.
# 귀무가설은 ‘H0:정규분포를 따른다’는 것으로 p-value가 0.05보다 크면 정규성을 가정하게 된다.
# 다만 유의할 점은 여기서 귀무가설을 기각하지 못 했다는 것은 정규분포를 따르지 않는다고 말할 근거가 부족한 것일 뿐 100% 정규성이 만족된다는 뜻은 아니다. 참고하는 정도로 보는 것이 좋다.

# 정규성 검정 Q-Q plot
qqnorm(ex1); qqline(ex1,col="red")

ex1 <- c(980, 1008, 968, 1032, 1012, 996, 1021, 1002, 996, 1017)

shapiro.test(ex1) # 정규성을 가정할 수 있다. 
t.test(ex1, mu = 1000, alternative = "two.sided")

# A회사의 건전지의 수명이 1000(시간)인지 알아보기 위해 10개의 샘플을 추출하여 측정한 결과 평균과 표준편차는 1003±19.2이고,
# 검정통계량(t)은 0.5(p-value = 0.6)으로 나타났다. 따라서 A회사의 건전지의 수명이 1000시간이다는 귀무가설을 기각할 수 없다.
# 즉, A회사 건전지의 수명은 1000시간이라 할 수 있다. 

# 문제 2 ---------------------------------------

# 어떤 반의 학생들의 수학 평균성적은 55점이었다. 
# 0교시 수업을 시행하고 나서 학생들의 시험 성적은 다음과 같다.	
# 58, 49, 39, 99, 32, 88, 62, 30, 55, 65, 44, 55, 57, 53, 88, 42, 39
# 0교시 수업을 시행한 후, 학생들의 성적은 올랐다고 할 수 있는가?

ex2 <- c(58, 49, 39, 99, 32, 88, 62, 30, 55, 65, 44, 55, 57, 53, 88, 42, 39)

shapiro.test(ex2) # 정규성을 가정할 수 있다. 
t.test(ex2, mu=55, alternative = "greater")

# 학생들의 시험성적이 증가한지 알아보기 위해 17명의 학생으로부터 측정한 성적의 평균과 표준편차는 56.2±19.8(점)이다.
# 검정통계량(t)은 0.2(p-value=0.4)로 나타났다. 따라서 "학생들의 시험성적이 증가했다"는 통계적으로 유의하지 않다고 볼 수 있다.
# 즉, 학생들의 시험성적은 이 전 시험에 비해 증가하지 않았다. 


# 문제 3 ---------------------------------------

# 2006년 조사에 의하면 한국인의 1인 1일 평균 알코올 섭취량이 8.1g 이다. 
# 2008년 무작위로 뽑은 알코올 섭취량은 다음과 같다.
# 15.50, 11.21, 12.67, 8.87, 12.15, 9.88, 2.06, 14.50, 0, 4.97
# 평균 알코올 섭취량이 달라졌다고 할 수 있는가?

ex3 <- c(15.50, 11.21, 12.67, 8.87, 12.15, 9.88, 2.06, 14.50, 0, 4.97)

shapiro.test(ex3) # 정규성을 가정할 수 있다.
t.test(ex3, mu=8.1, alternative = "two.sided")

# 한국인의 1인 1인 평균 알코올 섭취량이 재작년에 비해 달라졌는지 알아보기 위해 10명의 표본을 측정한 평균과 표준편차는 9.18±5.23(g)이다.
# 검정통계량은 0.7(p-value=0.5)로 나타났다. 따라서 "한국인의 1일 평균 알코올 섭취량이 재작년과 다르다"는 주장은 통계적으로 유의하지 않다.
# 즉, 한국인의 1인당 평균 알코올 섭취량은 통계적으로 재작년과 같고 할 수 있다.
