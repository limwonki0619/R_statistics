setwd("D:/limworkspace/R_Statistics/Ch_06_가설검정")

library(extrafont)

windowsFonts(hanna=windowsFont("BM HANNA 11yrs old")) # 폰트이름 변경 
windowsFonts(dohyeon=windowsFont("BM DoHyeon"))
windowsFonts(jalnan=windowsFont("Jalnan"))
windowsFonts(tb=windowsFont("TmonMonsori Black"))
windowsFonts(binggrae=windowsFont("Binggrae Taom"))
windowsFonts(nbg=windowsFont("나눔바른고딕"))

library(dplyr)
library(ggplot2)
library(reshape)

# Chapter 6. 가설검정


# 들어가기 전에.. 6장의 위한 준비 : 외부로부터 자료 가져오기 및 표본추출 --------------------------------------------------------------------------------------

# 예제 5-4) sizekorea 데이터 가져오기 : 한국인 인체표준 정보

data <- read.csv("data/2010_6차_직접측정 데이터.csv", header=T)
str(data)

tmp <- filter(data, 나이==7) # 조건에 맞는 행만 추출 
height.p <- tmp$X104.키      # 키 변수 

set.seed(9)
height <- height.p[sample(length(height.p), 15)]; height # sample()함수를 이용해 15개의 확률표본을 생성, sample(238,15) :  238개 중에서 임의로 15개 뽑기 

sample(10) # 숫자의 순서를 랜덤으로 뽑음
sample(10, 2) # 랜덤으로 뽑은 값에서 원하는 갯수만큼 추출 
