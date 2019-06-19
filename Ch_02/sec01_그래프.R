# Section 1. 그래프

setwd("D:/limworkspace/R_Statistics/Ch_02")

library(extrafont)

windowsFonts(hanna=windowsFont("BM HANNA 11yrs old")) # 폰트이름 변경 
windowsFonts(dohyeon=windowsFont("BM DoHyeon"))
windowsFonts(jalnan=windowsFont("Jalnan"))
windowsFonts(tb=windowsFont("TmonMonsori Black"))
windowsFonts(binggrae=windowsFont("Binggrae Taom"))
windowsFonts(nbg=windowsFont("나눔바른고딕"))

library(ggplot2)

# 산점도 : geom_point ----------------------------------------------------------------------
ggplot(cars,aes(x=cars$speed, y=cars$dist, color="Coral")) +
   geom_point() +
   theme_bw(base_family = "hanna") +
   labs(title = "속도와 제동거리", x="속도(mph)", y="제동거리(ft)") # labs에서 axis.title 설정 


# 산점도 : geom_jitter ---------------------------------------------------------------------
# point와 차이점 : 둘 다 산포도라는 점에서 공통점이 있지만 점을 어느위치에 출력하는지 차이를 보임. 
ggplot(cars,aes(x=cars$speed, y=cars$dist, color="Coral")) +
  geom_jitter(height = 0.1, width = 0.2) + # 흩어지는 정도 설정 
  theme_bw(base_family = "hanna") +
  labs(title = "속도와 제동거리", x="속도(mph)", y="제동거리(ft)") # labs에서 axis.title 설정 



# 시계열 : 일반함수 plot ---------------------------------------------------------------------
plot(Nile, main = "Nile강의 연도별 유량 변화", xlab="연도", ylab="유량")


# 시계열 : ggplot_line ----------------------------------------------------------------------
date <- seq(as.Date("1871-01-01"), as.Date("1970-01-01"),by="year")
str(date)

Nile_2 <- cbind(as.data.frame(Nile),date)
Nile_2 <- rename(Nile_2, "유량" = x)

ggplot(Nile_2, aes(x=date, y=유량)) + 
  geom_line(alpha=0.8) +
  theme_bw(base_family = "dohyeon", base_size = 14) +
  labs(title = "Nile강의 연도별 유량 변화", x="연도", y="유량") +
  theme(plot.title = element_text(hjust = 0.5, color="grey20"))


# 그래프 그리기 실습 ------------------------------------------------------ 
# 데이터 준비
data <- read.csv("data/2010년 인구사항.csv", header = F, na.strings=c("."))
str(data)

data$V1 <- factor(data$V1, levels = c(1,2), 
                  labels = c("남자","여자"))

data$V3 <- factor(data$V3, levels = 1:14, 
                  labels = c("가구주", "가구주의 배우자", "자녀", 
                             "자녀의 배우자", " 가구주의 부모", "배우자의 부모",
                             "손자녀, 그 배우자", "증손자녀, 그 배우자", 
                             "조부모", "형제자매, 그 배우자", 
                             "형제자매의 자녀, 그 배우자", 
                             "부모의 형제자매, 그 배우자",
                             "기타 친인척", "그외같이사는사람"))

data$V4 <- factor(data$V4, levels = 1:8,
                  labels = c("안받았음", "초등학교", "중학교",
                             "고등학교", "대학-4년제 미만", "대학-4년제 이상",
                             "석사과정","박사과정"))

library(dplyr)
data <-rename(data,
              "성별" = V1,
              "가구주와의 관계" = V3,
              "학력" = V4)

table(data$V5)       # 데이터 확인 
sum(is.na(data$V5))  # 결측치 개수 확인
sum(!is.na(data$V5)) # 결측치가 아닌 데이터 개수 확인

# 막대그래프 : 일반함수 barplot ----------------------------------------------------
# 출생아별 빈도
table(data$V5) %>% barplot(main="출생아(남자)별 빈도", xlab="출생아수", ylab="빈도")

# 학력에 따른 성별 인원수 
barplot(table(data$성별, data$학력), legend.text=T, col=c("orange","green"),
              main = "학력에 따른 성별 인원수", xlab="연령",ylab="빈도")

# 막대그래프 : geom_col

library(dplyr)
# 출생아별 빈도 
baby <- data.frame(table(data$V5))
baby2 <- rbind(baby[1:7,], data.frame(Var1="7_up",Freq=sum(baby[8:13, 2])))

ggplot(baby2, aes(x=Var1, y=Freq, fill=Var1)) +
  geom_col() +
  geom_hline(yintercept = seq(0,7,1), linetype = 2, color="grey40", lwd=0.08) +
  geom_text(aes(y=Freq+2000, label = paste0(Freq,"명")), color="grey40", family = "dohyeon") +
  theme_bw(base_family = "dohyeon", base_size = 14) +
  labs(title = "출생아(남자)별 빈도", x="출생아 수", y="빈도") +
  theme(plot.title = element_text(family = "hanna", color="grey20", hjust=0.5),
        axis.text.y = element_blank(),
        legend.position =  "none")


# 히스토그램 
# hist(x, breaks="sturges", nclass=Null)

# x : 히스토그램을 그릴 벡터
# breaks : 계급구간의 점을 나타내는 벡터로 계급의 경계를 지정, 4가지 방법이 존재
# nclass : 계급구간의 개수 지정 

#   hist 에서 closed 디폴트값은 left : closed | right : open   || ~초과 ~이하
# ggplot 에서 closed 디폴트값은 left : open   | right : closed || ~이상 ~미만
# 0은 포함하던 안하던 무조건 포함(?) *** 

sum(table(data$V2)[1:10])  #  1~10번째(0 <= x < 10) 값 == hist(right=F값) == ggplot(closed=left값) == 왼쪽을 포함, 오른쪽을 포함하지 않음 == ~이상 ~미만
sum(table(data$V2)[11:20]) # 11~21번째(10 <= x < 20) 
sum(table(data$V2)[21:30]) # 21~31번째(20 <= x < 30)
sum(table(data$V2)[31:40]) # 31~41번째(30 <= x < 40)
sum(table(data$V2)[41:50]) # 41~51번째(40 <= x < 50)
sum(table(data$V2)[51:60]) # 51~61번째(50 <= x < 60)
sum(table(data$V2)[61:70]) # 61~71번째(60 <= x < 70)
sum(table(data$V2)[71:80]) # 71~81번째(70 <= x < 80)
sum(table(data$V2)[81:90]) # 81~91번째(80 <= x < 90)

sum(table(data$V2)[1:11])  # 1~11번째(0 < x <= 10) 값 == hist(right=T값) == ggplot(closed=left값) == 왼쪽을 포함하지 않음, 오른쪽을 포함 == ~초과 ~이하
sum(table(data$V2)[12:21]) # 11~20번째 값
sum(table(data$V2)[12:21]) 
sum(table(data$V2)[22:31])
sum(table(data$V2)[82:91],na.rm = T) # 82~91번째 값(81~90)



# 히스토그램 : 일반함수 hist -------------------------------------------------------
hist(data$V2, main = "연령별 분포", xlab="연령", ylab="빈도")
hist(data$V2, breaks = c(seq(0,90,10)), right=F,    # right = F 옵션으로 각 구간의 왼쪽을 포함하고, 오른쪽을 포함하지 않는 그래프 (0~90)
     main = "연령별분포", xlab="연령", ylab="빈도") # 빈도로 나타냄 

hist(data$V2, breaks = c(seq(0,90,10)), right=T,    # right = T 옵션으로 각 구간의 왼쪽을 포함하지 않고, 오른쪽을 포함하는 그래프 
     main = "연령별분포", xlab="연령", ylab="빈도") # 빈도로 나타냄 

hist(data$V2, probability=T, breaks = c(seq(0,90,10)), right=F, 
     main = "연령별분포", xlab="연령", ylab="밀도") # 전체에서의 비중(밀도)으로 나타냄 (probability=T)

# 히스토그램 : geom_hist 빈도 : 기본 색상차트 
ggplot(data, aes(x=V2)) +
  geom_histogram(color="black", fill="lightpink", breaks = seq(0,90,10), closed = "left") +
  scale_x_continuous(breaks = c(seq(0,90,10))) +
  theme_bw(base_family = "dohyeon", base_size = 15) +
  labs(title = "연령별 분포 빈도 히스토그램", x="연령", y="빈도") +
  theme(plot.title = element_text(face = "bold", hjust=0.5, size=15)) +
  stat_bin(breaks = seq(0,90,10), 
           closed =  "left", 
           geom = "text", 
           aes(label=paste0(..count..,"명")), 
           vjust=-1, 
           color="grey20", family = "hanna") +
  theme(axis.text.y = element_blank())


# 히스토그램 : geom_hist 빈도 : 그라데이션 색상 
ggplot(data,aes(x=V2)) +
  geom_histogram(aes(x=V2,fill=..x..),color="black",breaks = seq(0,90,10), closed = "left", alpha = 0.8) +
  scale_x_continuous(breaks = c(seq(0,90,10))) +
  stat_bin(breaks = seq(0,90,10), 
           closed =  "left", geom = "text", aes(label=paste0(..count..,"명")), vjust=-1, color="grey20", family = "hanna") +
  scale_fill_gradient(low = "hotpink", high = "royalblue")+               # 그라데이션 
    theme_bw(base_family = "dohyeon", base_size = 15) +
    labs(title = "연령별 분포 빈도 히스토그램", x="연령", y="빈도") +
    theme(plot.title = element_text(face = "bold", hjust=0.5, size=20)) +
    theme(axis.text.y = element_blank())


# 히스토그램 : geom_hist 빈도  : 직접 색상 지정 
ggplot(data,aes(x=V2, fill=cut(..x..,9))) +                               # x축을 기준으로 막대기 9등븐 후 색상 채우기
  geom_histogram(breaks = seq(0,90,10),                                   # breaks : 계급구간 설정, x축 0~90까지 10씩 조정
                 color="black",                                           # 막대기 테두리 색상 지정 
                 closed = "left",                                         # 왼쪽을 포함한다 == [0, 10) == (0 <= x < 10)
                 alpha = 0.8) +                                           # 투명도 설정 
  scale_x_continuous(breaks = c(seq(0,90,10))) +                          # X축 설정 
  stat_bin(breaks = seq(0,90,10),                 
           closed =  "left",                 
           geom = "text",                                                 # text 만들기
           aes(label=paste0(..count..,"명")),                             # text 위치지정  
           vjust=-1,                                                      # text 위치지정  
           color="grey20",                                                # text 색상 지정 
           family = "hanna") +                                            # text 폰트 지정 
  scale_fill_discrete(h = c(180, 360), c = 150, l = 80) +                 # 막대 색상 직접 지정 
    theme_bw(base_family = "dohyeon", base_size = 15) +                   # 이후 테마적용 
    labs(title = "연령별 분포 빈도 히스토그램", x="연령", y="빈도") +
    theme(plot.title = element_text(face = "bold", hjust=0.5, size=20)) +
    theme(axis.text.y = element_blank(),
          legend.position = "none")




# 히스토그램 : geom_hist 밀도  : 직접 색상 지정 
ggplot(data,aes(x=V2, fill=cut(..x..,9))) +                             
  geom_histogram(aes(y=..density..), breaks = seq(0,90,10), color="black", closed = "left", alpha = 0.8) +                                         
  scale_x_continuous(breaks = c(seq(0,90,10))) +                        
  scale_fill_discrete(h = c(180, 360), c = 150, l = 80) +               
    theme_bw(base_family = "dohyeon", base_size = 15) +                   
    labs(title = "연령별 분포 밀도 히스토그램", x="연령", y="밀도") +
    theme(plot.title = element_text(face = "bold", hjust=0.5, size=20)) +
    theme(legend.position = "none") +
  geom_density(fill="Coral", alpha = 0.2) 

  
# 원 그래프 : 일반함수 pie ---------------------------------------------------------
pie(table(data$학력), main="학력수준별 비중", cex=0.8) # cex : 각 조각별 이름표의 크기를 기존 크기의 0.8로 축소

edu <- data.frame(table(data$학력))

ggplot(edu, aes(x="",y=Freq, fill=Var1)) +
  geom_bar(stat = "identity", width = 1, position = "fill") +
  coord_polar(theta = "y")

ggplot(data, aes(x=학력,fill=학력)) +
  geom_bar() +
  coord_polar() + 
  theme_bw(base_family = "jalnan", base_size = 14) +                   
  labs(title = "학력수준별 비중") +
  theme(plot.title = element_text(face = "bold", hjust=0.5, size=20)) +
  theme(axis.title = element_blank(),
        axis.text.y = element_blank()) +
  theme(legend.position = "none")
