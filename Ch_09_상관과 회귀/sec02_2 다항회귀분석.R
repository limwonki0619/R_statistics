# 3. Polynomial Regression (다항회귀-곡선형태) -------------------------------------------------------------------------
str(women) # height(inch), weight(lbs)

# 신장에 따른 몸무게 - liner model
plot(weight ~ height, data=women) # 실제로는 곡선형태 
fit <- lm(weight ~ height, data=women) # liner model 
abline(fit, col="red")

summary(fit)

par(mfrow = c(2,2))
plot(fit)
par(mfrow = c(1,1))

# 신장에 따른 몸무게 - polynomial model (2차식)
fit2 <- lm(weight ~ height + I(height^2), data=women)
plot(weight ~ height, data=women)
lines(women$height, fitted(fit2), col="green") # poly
abline(fit, col="red") # liner

summary(fit2)

par(mfrow=c(2,2))
plot(fit2)
par(mfrow=c(1,1))

# 신장에 따른 몸무게 - ployomial model (3차식)
fit3 <- lm(weight ~ height + I(height^2) + I(height^3), data=women)
summary(fit3)

par(mfrow=c(2,2))
plot(fit3)
par(mfrow=c(1,1))

# 두 개의 서로 다른 선형 회귀 모형의 성능을 비교할 때는 보통 다음과 같은 선택 기준을 사용한다.

# 1. 조정 결정 계수 (Adjusted determination coefficient)
# 2. AIC (Akaike Information Criterion) # AIC값이 작을수록 좋은(과소적합, 과대적합 하지 않는)모델 
# 3. BIC (Bayesian Information Criterion)


# Akaike’s Information Criterion 이란 무엇인가? [참고자료 https://chukycheese.github.io/statistics/aic/]

# Akaike’s Information Criterion(AIC; 아카이케 정보 기준) 은 여러 통계 모델들의 성능을 서로 비교할 수 있게 해준다. 
# 예를 들어, 개인의 낮은 사회경제학적 지위에 기여하는 변수가 어떤 것인지, 그리고 이 변수들이 어떻게 그 지위에 영향을 주는지 알고 싶다고 해보자. 
# 이를 분석하기 위해 교육수준, 가족 구성원의 수, 또는 장애 여부 등을 이용해서 여러 회귀 모형을 만들었다. 
# AIC 는 각각의 모델을 가지고 순위를 매겨서 최고부터 최악의 모형을 보여줄 수 있다. 
# “최고의” 모형은 과소적합(under-fit)을 하지도, 과대적합(over-fit)을 하지도 않는 모형일 것이다.
# 비록 AIC 가 한 집단에서 최고의 모형을 찾아주지만, 절대적인 성능에 대해서는 알려주지 못한다. 
# 다시 말하자면, 여러분이 만든 모든 모형의 성능이 낮다면, 성능이 좋지 않은 모형들 사이에서 그나마 나은 것을 선택한다는 것이다. 
# 그렇기 때문에 최고의 모형을 선택한 다음에는 그 모형에 속한 설명변수와 반응변수 사이의 관계를 찾아내기 위한 가설 검정을 실시하는 것을 고려해봐야 한다.

# Delta 점수(Scores)
# AIC 점수는 ΔAIC 점수나 Akaike 가중치들로 표현된다. 
# ΔAIC 는 최고의 성능을 가진 모형(ΔAIC 값이 0인)과 각 모델의 상대적 차이를 의미한다. 그 공식은 아래와 같다:

# ΔAIC=AICi−minAIC

#     AICi: i 번째 모델의 점수
#     min AIC: “최고의 성능”을 가진 모형의 점수
#     Burnham 과 Anderson(2003) 은 ΔAIC 점수를 해석하는 데에 다음과 같은 방법을 제안했다:

# ΔAIC < 2: 해당 모형이 맞다고 할 충분한 근거가 있다
# 3 < ΔAIC < 7: 해당 모형이 맞다고 할 근거가 충분하지는 않다
# ΔAIC > 10: 해당 모형이 맞다고 할 근거가 거의 없다

AIC(fit2)
AIC(fit3)

# Overfit 문제도 생각해야 한다 (training set : 70% test set : 30% )
