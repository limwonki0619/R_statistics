setwd("D:/limworkspace/R_Statistics/Ch_07_여러 모집단의 평균 비교 검정")

# 7장을 위한 준비 : 데이터 프레임 다루기와 데이터 정제하기 ---------------------------------------------------------------------

# 목차 

# 1. 데이터 프레임 다루기 
#   1.1 데이터 프레임의 정보 취득하기
#        - norw와 ncol를 이용한 행과 열의 수 확인 
#        - str를 이용한 구조 확인 
#        - head와 tail을 통한 자료의 앞부분, 뒷부분 확인
#        - names를 이용한 열의 이름 확인 및 변경

#   1.2 추출하고자 하는 열 선택하기
#        - $를 이용한 열 지정 
#        - []를 이용한 열 지정
#        - ["변수명"]을 이용한 열 지정
#        - [[]]을 이용한 열 지정

#   1.3 조건에 맞는 행 추출하기
#        - 남아 신생아의 자료 가져오기
#        - 남아 신생아의 자료에서 평균 체중보다 큰 자료만 가져오기

#   1.4 조건에 맞는 행과 열 선택하기

#   1.5 데이터 프레임 저장하기


# 2. 데이터 정제하기
#   2.1 결측치 처리하기
#       - 데이터 불러오기
#       - 결측치 처리 
#       - 위와 같은 결과지만 다른 방법 
#       - 결측처리에 따른 수학함수(mean 등)의 수행 비교
#       - 결측치 판별 함수 is.na()

#   2.2 factor형 자료처리 
#       - 범주의 값이 1일 때의 기초통계량 

#   2.3 "doBy" 패키지를 이용한 기초통계량 계산 
#       - doBy 패키지의 사용 
#       - 지역 규모별 표본의 수
#       - 지역 규모별 평균과 분산
#       - summaryBy를 이용해 지역규모별 관찰 자료의 수

# ------------------------------------------------------------------------------------------------------------------------------

# 1. 데이터 프레임 다루기 -------------------------------------------------
# 1.1 데이터 프레임의 정보 취득하기 ---------------------------------------

data <- read.table("http://www.amstat.org/publications/jse/datasets/babyboom.dat.txt", header = F)

# - norw와 ncol를 이용한 행과 열의 수 확인 
nrow(data); ncol(data)

# - str를 이용한 구조 확인 
str(data)

# - head와 tail을 통한 자료의 앞부분, 뒷부분 확인
head(data,10); tail(data,5)

# - names를 이용한 열의 이름 확인 및 변경
names(data)
names(data) <- c("time", "gender", "weight", "minutes"); names(data)
row.names(data) # 행번호 

# 1.2 추출하고자 하는 열 선택하기 ------------------------------------------
# - $를 이용한 열 지정 
g1 <- data$gender; head(g1)

# - []를 이용한 열 지정
g2 <- data[,2]; head(g2)

# - ["변수명"]을 이용한 열 지정
g3 <- data["gender"]; head(g3)

# - [[]]을 이용한 열 지정
g4 <- data[[2]]; head(g4) # 두개의 괄호를 사용하는 경우 열 번호와 열 이름의 문자열 모두를 전달할 수 있고, 해당 열의 모든 자료를 나타냄, 결과는 벡터 

# 1.3 조건에 맞는 행 추출하기 ----------------------------------------------
# [], base::subset, dplyr::filter
library(dplyr)
# - 남아 신생아의 자료 가져오기
data[data$gender==2,]
subset(data, gender==2)
filter(data, gender==2)

# - 남아 신생아의 자료에서 평균 체중보다 큰 자료만 가져오기
male.m <- mean(data$weight)
data[data$gender==2 & data$weight > male.m,]
subset(data, gender==2 & weight > male.m)
filter(data, gender==2 & weight > male.m)

# - 1.4 조건에 맞는 행과 열 선택하기
data[data$gender==2 & data$weight > male.m , c(2,4)]
subset(data, gender==2 & weight > male.m, select = c(2,4))
filter(data, gender==2 & weight > male.m) %>% select(c(2,4)) 

# - 1.5 데이터 프레임 저장하기
chapter7 <- data[, c(2, 3)]
write.table(chapter7, "./data/chapter7.txt")
write.table(chapter7, "./data/chapter7.txt", row.names = F) # 행번호 제외 ㅜㅠㅍ
# write.table(x, file, row.names, col.names, sep, na, append)
# x         : 저정할 데이터 프레임 이름
# file      : 저장할 파일의 경로와 이름
# row.names : 행 이름의 저장 여부
# col.naems : 열 이름의 저장 여부
# sep       : 열 구분자, 기본값은 공백문자 " "
# na        : 결측값으로 저장할 문자열, 기본값은 "NA"
# append    : 기존 파일의 뒤에 붙일 것인지 여부, 디폴트는 F = 덮어쓰기 



# 2. 데이터 정제하기 

# 데이터 설명 
# scale (num) : 거주지역의 규모(범주형) 1. 특별시, 광역시 2. 시 지역 3. 읍면 지역
# sex   (num) : 성별(범주형) 1. 여성 2. 남성
# score (num) : 서비스 평가점수(순서형) 0~10까지 점수로 높을수록 좋음
# age   (num) : 나이

# - 데이터 불러오기
age <- read.csv("data/age.data.csv", header = T)
str(age)
head(age); tail(age)
summary(age)

# - 결측치 처리 
age$score <- ifelse(age$score == 99, NA, age$score)
summary(age) # 결측치 4개 

# - 위와 같은 결과지만 다른 방법 
age2 <- read.csv("data/age.data.csv", header = T, na.strings = c("99")) # 결측치 정해주기 
summary(age2)

# - 결측처리에 따른 수학함수(mean 등)의 수행 비교
mean(age$score)            # 결측치가 포함되면 계산이 불가
mean(age$score, na.rm = T) # 결측치를 제외한 계산 

# - 결측치 판별 함수 is.na()
na <- is.na(c(1, NA, 3, NA, 5))
count_na <- sum(na) # 결측치 갯수 세기

# 2.2 factor형 자료처리 
age$scale <- factor(age$scale)
age$sex <- factor(age$sex)
str(age) # 자료형태 변경 확인

# - 범주의 값이 1일 때 기초통계량 
length(age$age[age$scale=="1"])
mean(age$age[age$scale=="1"])
sd(age$age[age$scale=="1"])

# 2.3 "doBy" 패키지를 이용한 통계량 계산 
install.packages("doBy")
library(doBy)

# - doBy 패키지의 사용 
# doBy 패키지 중 많이 사용하는 summaryBy 함수를 사용해보자.

# summaryBy(fumula, data, FUN, ...)
# 이 함수는 다음과 같은 전달인자를 가진다.
# 첫 번째 전달인자로 R의 수식을 이용하여 각종 통계를 구할 변수와 집단을 구분할 변수를 표현하는데, 이는 다음과 같다
# "통계를 구할 변수 ~ 집단을 구분할 변수"

# - 지역 규모별 표본의 수
summaryBy(age~scale, data=age, FUN=c(length))

# - 지역 규모별 평균과 분산 
summaryBy(age~scale, data=age, FUN=c(mean, var), na.rm=T)
?summaryBy
