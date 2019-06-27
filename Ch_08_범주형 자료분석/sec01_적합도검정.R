setwd("D:/limworkspace/R_Statistics/Ch_08_범주형 자료분석")
# 참고자료 : https://rfriend.tistory.com/137


# 범주형 자료분석 : 값으로부터 직접 계산을 할 수 없는 질력자료의 경우 실시할 수 있는 분석이다. 여기서 중요한 계산은
# 질적자료의 각 범주의 개수를 세는 것이다.

# Section 2. 동질섬 검정

# 가설수립
# H1 : p1=9/16, p2=3/16, p3=3/16, p4=1/16
# H0 : not H1

# 기대도수 (Expected Frequency, E)
# 전체개수 * 각 범주의 영가설 하에서의 비율 
# E1 = 556 * 9/16, E2 = 556*6/16, E3 = 556*3/16, E4 = 556*1/16

# 관찰도수 
# 실제 관찰된 도수(Observed frequency, O)

# chisq(k-1) 검정통계량 
# 모집단을 k로 나누는 범주형 자료에 대해, i번쨰 범주의 관찰도수를 Oi, 기대도수를 Ei라고 하면,
# 카이제곱 검정통계량은 자유도가 (범주[k]-1)인 카이제곱 분포를 따른다. 

# R함수 chisq.test()를 이용한 검정 
x <- c(315, 101, 108, 32)
chisq.test(x, p=c(9,3,3,1)/16)

axis_x <- seq(0,15,by=0.01)
data <- data.frame(x=axis_x,
                   y=dchisq(axis_x, df=3))
# 1-pchisq(7.81, df=3)
# 1-pchisq(0.470024, df=3)
# qchisq(1-0.05, df=3)
# qchisq(1-0.9254, df=3)


library(ggplot2)
ggplot(data, aes(x=x,y=y)) + 
  geom_line() +
  geom_area(data=data[data$x > qchisq(1-0.9254, df=3),], alpha=0.2) +
  geom_area(data=data[data$x > qchisq(1-0.05, df=3),], fill="red", alpha=0.5)
?geom.pol
