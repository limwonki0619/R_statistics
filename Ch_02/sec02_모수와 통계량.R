# Section 2. 모수와 통계량
# 라니의 카페 

setwd("D:/limworkspace/R_Statistics/Ch_02")

library(extrafont)

windowsFonts(hanna=windowsFont("BM HANNA 11yrs old")) # 폰트이름 변경 
windowsFonts(dohyeon=windowsFont("BM DoHyeon"))
windowsFonts(jalnan=windowsFont("Jalnan"))
windowsFonts(tb=windowsFont("TmonMonsori Black"))
windowsFonts(binggrae=windowsFont("Binggrae Taom"))
windowsFonts(nbg=windowsFont("나눔바른고딕"))

# 데이터 준비 
ranicafe <- read.csv("data/cafedata.csv", stringsAsFactors = F, na.strings = "na") # na.strings 결측치로 처리할 문자 지정 
str(ranicafe)                                                                      # stringsAsFactors : factor 
head(ranicafe)
summary(ranicafe$Coffees)
dim(ranicafe)
na.omit(ranicafe$Coffees) # NA값 삭제

rc <- as.numeric(ranicafe$Coffees) # 커피판매량을 숫자형태로 변환 

# ====================================================
# rc[!is.na(rc)]                   # 이상치 제거 방법 
# ====================================================

# 최댓값과 최솟값 ----------------------------------------------------------------------------------------------------
rc
sort(rc)[1]              # 정렬된 값의 첫번째 
sort(rc, decreasing = T) # 내림차순 정렬 
min(rc, na.rm = T)       # 결측치 제거 후 최솟값 
max(rc, na.rm = T)       # 결측치 제거 후 최댓값 


# 최빈값
stem(rc)  # 줄기-잎 그림  
hist(rc)  # 히스토그램
table(rc) # 테이블 표로 확인 

Mode <- function(x) {  # 최빈값 구하기 함수만들기 
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

Mode(rc) # 최빈값 


# 평균과 중앙값 -----------------------------------------------------------------------------------------------------

# 대표값 : 전체 자료를 대표하는 값
#          대표값은 중심 위치를 나타내는 특성을 사용함 
#          최빈값또 대표값 중 하나 

# 자료의 중심은 두 가지
# 무게중심 : (산술)평균 : Sigma(1~n) / n
#          : 중앙값  
#                  1. 홀수 일때 
#                  2. 짝수 일때 

# 중앙값 예제
x_홀수 <- 1:5
(1+5)/2 # 3번째 값이 중앙값

x_짝수 <- 1:6
(1+6)/2 # 3번째 값과 4번쨰 값의 평균이 중앙값 


# 평균 (mean)
mean(ranicafe$Coffees, na.rm = T)           # mean함수

weight <- 1 / (length(rc)-sum(is.na(rc)))   # length에서 NA까지 벡터갯수가 포함되므로 NA갯수를 빼줌 
sum(rc * weight, na.rm = T)                 # 직접 계산 

# 평균의 약점 : 이상치값의 변화에 민감함 
rc[rc == max(rc, na.rm = T)] <- 480
mean(rc, na.rm = T)

# 중앙값 
median(rc, na.rm = T)

length(rc)                                  # NA값 포함 개수 
median.idx <- length(rc)/2
sort(rc)[median.idx]

x <- c(1,1,2,2,3,3,3,4,4)
mean(x)
median(x)

x2 <- c(2,2,3,3,3,4,4,5,5)
mean(x2)
median(x2)



# 표준편차와 사분위수 범위 -----------------------------------------------------------------------------------------
# 각 자료들이 평균에서 평균적으로 얼마나 떨어져 있을까? 

# 편차 
height <- c(164,166,168,170,172,174,176)
height.m <- mean(height)
h.dev <- sum(height - height.m)               # 편차
sum(height.dev)                               # 편차 합 

h.dev2 <- h.dev^2                             # 편차 제곱 
sum(h.dev2)                                   # 편차 제곱합 

# 모집단의 분산과 표준편차 : 편차제곱합 / N,   sqrt(편차제곱합 / N)
variance <- sum(h.dev2)/length(h.dev2)        # 편차 제곱의 평균 = 분산 
standard_deviation <- sqrt(variance)          # 표준편차 


# 표본집단의 분산과 표준편차 : 편차제곱합 / n-1,   sqrt(편차제곱합 / n-1) 
variance2 <- sum(h.dev2)/(length(h.dev2)-1)   # 편차 제곱의 평균 = 분산 
standard_deviation2 <- sqrt(variance)         # 표준편차 

var(height)                                   # 표본분산으로 계산된 
sd(height)                                    # 표본표준편차로 계산됨 


# 자유도 
# n개의 편차에서 편차 중 n-1개는 원하는 값을 가질 수 있는데, 이를 '자유도'라고 한다. 




# 사분위수(quantile) 범위와 상자도표(boxplot) ----------------------------------------------------------------------

# 사분위수 구하기 
qs <- quantile(rc, na.rm = T); qs
summary(rc, na.rm=T)
boxplot(rc)$stat

# IQR(InterQuartile Range) : 3분위수 - 1분위수 
qs[4] - qs[2]      # IQR 구하는 방법 1 
IQR(rc, na.rm = T) # IQR 구하는 방법 2 

rc[rc==max(rc,na.rm = T)] <- 48 # 이상치 원래값으로 돌리기 
bp <- boxplot(rc, main = "커피 판매량에 대한 상자도표")

# 이상치(Outlier) ---------------------------------------------------------------------------------------------------
boxplot(cars$dist)
cars_qs <- quantile(cars$dist)
cars_IQR <- cars_qs[4] - cars_qs[2]

upperlimit <- cars_qs[4] + 1.5*cars_IQR
lowerlimit <- cars_qs[2] - 1.5*cars_IQR

# Upper-Limit보다 크거나, Lower-LimitL보다 작은 값이 이상치가 된다. 
upperlimit; lowerlimit

# 이상치 확인 해보기 
cars$dist[cars$dist > upperlimit |
          cars$dist < lowerlimit] 

# 이상치 제거 후 박스플랏 그리기
cars$dist_이상치제거 <- ifelse(cars$dist > upperlimit |
                               cars$dist < lowerlimit, NA, cars$dist)

boxplot(cars$dist)
boxplot(cars$dist_이상치제거) # NA값은 그래프에 나타나지 않음 


