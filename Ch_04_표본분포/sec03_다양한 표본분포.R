setwd("D:/limworkspace/R_Statistics/Ch_04")

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


# Chapter 4. 표본분포

# Section 3. 다양한 표본분포 

# 3.1 χ^2-분포 (CHI-SQuared distribution) : 모수는 자유도(k)

# 자유도가 작을 때 꼬리가 오른쪽으로 길게 늘어진 형태
# 자유도가 증가할수록 정규분포와 유사하게 평균을 중심으로 좌우대칭 형태를 갖는다

# 자유도(Degree of freedom)란
# n개의 편차에서 편차 중 n-1개는 원하는 값을 가질 수 있는데, 이를 '자유도'라고 한다. 



# 3.2 t-분포

# t-분포는 정규분포와 비슷한 형태지만, 평균 주변에서 상대적으로 밀도가 낮고
# 양 끝으로 갈수록 꼬리 부분이 두툼한 형태를 갖는다. 또한 자유도가 증가할수록 표준정규분포를 닮아간다.



