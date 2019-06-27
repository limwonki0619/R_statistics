setwd("D:/limworkspace/R_Statistics/Ch_08_범주형 자료분석")
# 참고자료 : https://rfriend.tistory.com/137

# Section 2. 동질성 검정과 독립성 검정 

# 1. 동질성 검정 

sns.c <- read.csv("data/snsbyage.csv",header = T, stringsAsFactors = F)
sns.c <- transform(sns.c, service.c = 
                     factor(service, levels = c("F","T","K","C","E"), ordered = T))

sns.c <- transform(sns.c, age.c = 
                     factor(age, levels = c("1","2","3"), labels = c("20대","30대","40대"),ordered=T))

str(sns.c)

c.tab <- table(sns.c$age.c, sns.c$service.c)
a.n <- margin.table(c.tab, margin = 1) # 행 기준 더하기 
s.n <- margin.table(c.tab, margin = 2) # 열 기준 더하기 
s.p <- s.n / margin.table(c.tab)
expected <- a.n %*% t(s.p)

o.e <- c.tab-expected
t.t <- sum( (o.e)^2 / expected)
1-pchisq(t.t, df=8)

chisq.test(c.tab)

# 2. 독립성 검정
# 어떤 모집단에서 관찰한 두 개의 속성 R과 C가 범주형 변수일 때 두 변수가 서로 연관이 있는지를 검정하는 것 

# 예제 3. 성별에 따른 대학원 입학 여부의 독립성 검정 
data(UCBAdmissions)
str(UCBAdmissions)

ucba.tab <- apply(UCBAdmissions, c(1,2), sum)
chisq.test(ucba.tab, correct = F) # 예이츠 보정의 목적은 통계적 유의성을 과대 평가하는 것을 막기 위한 것으로 (특히 표본수가 작은 경우) 기본이 보정을 하는 것으로 되어 있습니다. 만약 하지 않으려면 correct=F의 옵션을 주면 됩니다. 

data1 <- data.frame(x=seq(-3,3,0.01),
                    norm=dnorm(seq(-3,3,0.01)),
                    t=dt(seq(-3,3,0.01),3))


