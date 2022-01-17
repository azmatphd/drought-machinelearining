#Drought prediction using K-Nearest Neighbors (KNN)
install.packages("caret")
library(caret)
#Data
weather <- read_excel("D:/azmat/WESTERN RAJASTHAN/SIKAR/sikar weather.xlsx")
attach(weather)
#data partition
set.seed(123)
ind <- sample(2, nrow(data), replace = T, prob = c(0.7, 0.3))
training <- data[ind == 1,]
testing <- data[ind == 2,]
# KNN modelling
Train <- trainControl(method = 'repeatedcv',
                          number = 20,
                          repeats = 5)
#fitting
set.seed(1234)
fit <- train(SPEI ~.,
             data = training,
             tuneGrid = expand.grid(k=1:70),
             method = 'knn',
             metric = 'Rsquared',
             trControl = trControl,
             preProc = c('center', 'scale'))
#IMPORTANCE OF PARAMETERS ON SPEI
varImp(fit)
plot(varImp(fit))
#Prediction using KNN model
prediction <- predict(fit, newdata = testing)

