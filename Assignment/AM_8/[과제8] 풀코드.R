library(extrafont)

windowsFonts(hanna=windowsFont("BM HANNA 11yrs old")) # 폰트이름 변경 
windowsFonts(dohyeon=windowsFont("BM DoHyeon"))
windowsFonts(jalnan=windowsFont("Jalnan"))
windowsFonts(binggrae=windowsFont("Binggrae Taom"))

library(dplyr)
library(ggplot2)
library(reshape)

# 이항분포 -----------------------------------------------------------------

# 1. 다음의 문제가 베르누이 시행인지 판단하시오. 

#   1) 영화관에서 줄을 기다리는 시간을 측정한다. - 아니다
#   2) 전화가 왔을 때, 전화를 한 사람이 여자인지를 측정한다. - 맞다
#   3) 주사위를 한 번 던졌을 때, 나오는 숫자를 체크한다.  - 아니다
#   4) 주사위를 한 번 던졌을 때, 숫자 2가 나오는지를 체크한다.  - 맞다 

# 2. 한 축구 선수가 페널티킥을 차면 5번 중 4번을 성공한다고 한다.
#    이 선수가 10번의 페널티킥을 차서 7번 성공할 확률을 구하시오.

# 성공확률 0.8
# 시행횟수 10
# 성공횟수 7

dbinom(x=7, size=10, prob = 0.8)


# 3. A라는 회사는 스마트폰의 한 부품을 만드는 회사로, 이 A사의 불량률은 5%로 알려져 있다.
#    이 회사의 제품 20개를 조사했을 때, 불량이 2개 이하로 나올 확률을 구하시오.

# 불량률 5/100 
# 시행횟수 20
# 불량 2개 이하 = 0개 + 1개 + 2개

pbinom(2, size=20, prob = 0.05)
dbinom(0, 20, 0.05) + dbinom(1, 20, 0.05) + dbinom(2, 20, 0.05)

# 4. 어떤 희귀 바이러스에 감염되었을 때, 회복할 수 있는 치료율은 20%라고 한다. 
# 이 바이러스에 감염된 환자 20명을 치료했을 때, 적어도 2명 이상은 회복될 확률을 구하시오.

# 치료율 0.2
# 환자 20명
# 적어도 2명 이상 = 1 - 2명 미만

1 - pbinom(1, size=20, prob=0.2)

# 5. 주사위 두 개를 던졌을 때, 눈금의 합이 6이 될 확률을 구하시오.
# [1,5] 
# [2,4]
# [3,3]
# [4,2]
# [5,1]
5/36

# 정규분포 ----------------------------------------------------------------

# 1. A라는 전구회사에서 생산하는 전구의 수명은 800일이고 표준편차는 40일인 정규분포를 따른다고 한다.  
#    이때 전구의 수명이 750일 이하일 확률을 구하시오.

# X ~ B(800, 40^2)
# P(X<=750)
pnorm(750, mean=800, sd=40)
(750-800)/40 # Z-점수

x <- seq(-3,3,by=0.01)
data <- tibble(x = x, y = dnorm(x, mean = 0, sd = 1)) %>% 
  mutate(variable = case_when( # 범주형 변수 
    (x <= -1.25) ~ "area_1",
    TRUE ~ NA_character_)) # 그외 것들은 NA처리 

ggplot(data, aes(x, y)) + 
  geom_line() + 
  geom_area(data = filter(data, variable == 'area_1'), fill = 'hotpink', alpha = 0.3) +  
    theme_bw(base_family = "jalnan", base_size = 20) +
    theme(axis.title = element_blank(),
          plot.title = element_text(hjust=0.5)) +
    labs(title = "정규분포 문제 1 : P(Z <= -1.25) 면적") +
    scale_x_continuous(breaks = seq(-3,3,by=1)) # axis.text.x 변경 


# 2. 어느 한 회사에 다니는 종업원들의 근무기간을 조사하였더니, 평균은 11년이고 분산이 16년인 정규분포를 따른다고 한다.     

#    1) 20년 이상 근무한 종업원의 비율을 구하시오.   
#       X ~ N(11,4^2) P(X>=20) = 1 - P(X<=19)
1-pnorm(19, mean=11, sd=4) # 2%

(11-19)/4 # Z-점수

x <- seq(-3,3,by=0.01)
data <- tibble(x = x, y = dnorm(x, mean = 0, sd = 1)) %>% 
  mutate(variable = case_when( # 범주형 변수 
    (x <= -1.25) ~ "area_1",
    TRUE ~ NA_character_)) # 그외 것들은 NA처리 

ggplot(data, aes(x, y)) + 
  geom_line() + 
  geom_area(data = filter(data, variable == 'area_1'), fill = 'olivedrab3', alpha = 0.3) +  
    theme_bw(base_family = "jalnan", base_size = 20) +
    theme(axis.title = element_blank(),
        plot.title = element_text(hjust=0.5)) +
    labs(title = "정규분포 문제 2-1 : P(Z <= -2) 면적") +
    scale_x_continuous(breaks = seq(-3,3,by=1)) # axis.text.x 변경 

#    2) 근무연수가 가장 오래된 10%의 종업원은 이 회사에서 몇 년 이상 근무했다고 볼 수 있는가?
qnorm(0.9, mean = 11, sd=4) # 16년 이상 

# 3. 어느 고등학교 3학년 학생들의 수학성적은 평균이 70이고 표준편차가 8인 정규분포를 따른다고 한다.  
#    이때 점수가 80점 이상이고 90점 이하인 학생의 비율을 구하시오.

# X ~ B(70, 8^2)
# P(80 <= X <= 90)
pnorm(90, 70, 8) - pnorm(80, 70, 8) # 약 10% 
# P(80 <= X <= 90)
# P(1.25 <= Z <= 2.5)

x <- seq(-3,3,by=0.01)
data <- tibble(x = x, y = dnorm(x, mean = 0, sd = 1)) %>% 
  mutate(variable = case_when( # 범주형 변수 
    (x >= 1.25 & x <= 2.5) ~ "area_1",
    TRUE ~ NA_character_)) # 그외 것들은 NA처리 

ggplot(data, aes(x, y)) + 
  geom_line() + 
  geom_area(data = filter(data, variable == 'area_1'), fill = 'royalblue', alpha = 0.3) +  
    theme_bw(base_family = "jalnan", base_size = 20) +
    theme(axis.title = element_blank(),
        plot.title = element_text(hjust=0.5)) +
    labs(title = "정규분포 문제 3 : P(1.25 <= Z <= 2.5) 면적") +
    scale_x_continuous(breaks = seq(-3,3,by=1)) # axis.text.x 변경 






# 4. 확률변수 X가 평균이 1.5, 표준편차가 2인 정규분포를 따를 때, 
#    실수 전체의 집합에서 정의된 함수 H(t)는 H(t) = P(t ≤ X ≤ t+1) 이다. 
#    H(0) + H(2)의 값을 구하시오.

# X ~ B(1.5, 2^2)
# H(0) = P(0 <= X <= 1)
pnorm(1, 1.5, 2) - pnorm(0, 1.5, 2)


# H(2) = P(2 <= X <= 3)
pnorm(3, 1.5, 2) - pnorm(2, 1.5, 2)

# H(0) + H(2)
pnorm(1, 1.5, 2) - pnorm(0, 1.5, 2) + pnorm(3, 1.5, 2) - pnorm(2, 1.5, 2)

x <- seq(-3,6,by=0.001)
norm <- data.frame(x=x, dnorm = dnorm(x, 1.5, 2))

library(ggplot2)
library(dplyr)
filter(norm, x >= 0 & x <= 1) 

ggplot(norm, aes(x, dnorm)) + 
  geom_line(lwd=1.2) + 
  geom_area(data=norm[x >= 0 & x <= 1,], fill = 'hotpink', alpha = 0.3) +   # H(0)
  geom_area(data=norm[x >= 2 & x <= 3,], fill = 'turquoise', alpha = 0.3) + # H(2)
  theme_bw(base_family = "jalnan", base_size = 20) +
  theme(axis.title = element_blank(),
        plot.title = element_text(hjust=0.5)) +
  labs(title = "정규분포 문제 4 : H(O)와 H(2)의 면적") +
  scale_x_continuous(breaks = seq(-3,6,by=1)) # axis.text.x 변경 





