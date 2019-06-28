# 5. 다중회귀모형에서 변수의 선택 ------------------------------------------------------------------------------
# 참고자료 https://rstudio-pubs-static.s3.amazonaws.com/190997_40fa09db8e344b19b14a687ea5de914b.html


# AIC 값으로 최고의 모델 선택
AIC(fit, fit2)

# 변수선택법 - 단계 선택법 

# 1. Backward : 모든 변수부터 영향이 적은 변수를 하나씩 제거해 AIC 값이 가장 낮은 모델 선택 
# 2. Forward  : 상수항부터 시작해 변수를 하나씩 추가해 AIC값이 가장 낮은 모델 선택 

# step() 함수는 AIC 값을 이용하여 단계적 회귀를 수행하는 함수로 forward, backward stepwise regression을 모두 할 수 있다. 
# Backward regression은 가능한 많은 변수에서 시작해 하나씩 제거하는 방법이고,
# Forward regression은 적은 수의 변수에서 시작해 변수를 하나씩 추가하는 방법이다.


# 5.1 Backward regression --------------------------------------------------------------------------------------

# 먼저 가능한 많은 변수들을 포함하는 full.model을 만들고 의미 없는 변수들을 제거하기 위해 step 함수를 사용한다. 
# 그리고 그 결과를 reduced.model에 저장한다.

full.model <- lm(Murder~.,data=states)
backward.model <- step(full.model,direction="backward")

# step() 함수의 출력을 보면 모형을 어떻게 적용해 나갔는지 알 수 있다. AIC 값이 97.75에서 시작해서 93.76까지 감소하였다. 
# 최종 모형의 요약을 보면 두 개의 변수가 남아 있는 것을 알 수 있다. 최종선택된 모형은 다음과 같다.
summary(backward.model)

# Backward regression은 쉬운 방법이지만 대상이 되는 변수가 아주 많은 경우 모든 변수를 포함하여 시작하는 것이 어려울 수 있다. 
# 이러한 경우 forward stepwise regression을 시행하여 무에서 시작하여 더 이상 개선이 없을 때까지 하나씩 변수를 추가한다. 


# 5.2 Forward Regression ----------------------------------------------------------------------------------------
# Forward regression은 아무것도 없는 곳에서부터 출발한다.
min.model <- lm(Murder~1,data=states) # Forward selection을 시작하는 모형은 예측인자가 없고 반응인자만 있는 모형이다.

# Forward selection을 하기 위해서는 대상이 되는 변수들이 어떤 것인지 step() 함수에 알려주어야 한다. 
# scope 인수에 변수들을 알려주고 장황한 출력을 피하기 위해 trace=0을 추가했다.
fwd.model <- step(min.model,direction="forward",
                  scope=(Murder~Population+Illiteracy+Income+Frost))

fwd.model <- step(min.model,direction="forward",
                  scope=(Murder~Population+Illiteracy+Income+Frost),trace=0)

summary(fwd.model)


# Forward selection에서도 같은 결과를 얻었다. 마지막으로 이러한 stepwise regression을 너무 과신하지 말아야 한다. 
# 이 방법은 만병통치약이 아니다. 이 방법으로 고철을 황금으로 바꿀 수 없으며 주의깊고 현명하게 변수를 고르는 것을 대체할 수 없다. 
# 이렇게 생각하는 사람이 있을 수 있겠다. “ 음…가능한 모든 상호작용을 내 모형에 다 집어 넣은 후 step함수가 가장 좋은 것을 고르게 하자! 뭘 고르나 한 번 보자! ” 
# 이런 생각을 하는 사람은 다음과 같은 것을 한번 생각해보라.

# total.model <- lm(y~(x1+x2+x3+x4)^4)  # all-possible interaction
# reduced.model <- step(total.model,direction="backward")

# 아마도 생각대로 잘 안 될 것 같다. 대부분의 상호작용은 의미가 없을 것이고 step() 함수는 과부하가 걸릴 것이고, 
# 엄청나게 많은 무의미한 출력을 보게 될 것이다.



# 6. BIC 값으로 모델을 선택하는 방법 (stepwise 방법의 한계를 극복하고자 함)
install.packages("leaps")
library(leaps)

subsets <- regsubsets(Murder~., data=states,
                      method = "seqrep", nbest = 4)

subsets <- regsubsets(Murder~., data=states,
                      method = "exhaustive", nbest = 4)

summary(subsets)
plot(subsets)
