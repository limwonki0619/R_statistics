# 7. 로지스틱 회귀분석 (범주형 자료)

log_data <- read.csv("http://stats.idre.ucla.edu/stat/data/binary.csv")
str(log_data)

log_data$rank <- as.factor(log_data$rank)
str(log_data)

train <- data[1:200]
test <- data[201:400]

model <- glm(formula = admit ~ gre+gpa+rank, family = "binomial", data=log_data)
summary(model)

model2 <- glm(admit ~ gpa + rank, data=log_data, family="binomial")
summary(model2)

AIC(model, model2)
