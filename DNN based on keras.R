#SPEI drought prediction using DNN (deep neural network) using keras
install.packages("keras")
install.packages("neuralnet")
install.packages("dplyr")
install.packages("magrittr")
library(keras)
library(dplyr)
library(magrittr)
library(neuralnet)
#data
weather <- read_excel("D:/azmat/WESTERN RAJASTHAN/SIKAR/sikar weather.xlsx")
# Neural Network 
n <- neuralnet(SPEI ~ Tmax+Tmin+prcp+wind+Relativehumidity+solar+PET,
               data = weather,
               hidden = c(8,5),
               linear.output = F,
               lifesign = 'full',
               rep=1)
plot(n)
# Matrix
weather <- as.matrix(weather)
dimnames(weather) <- NULL
# data Partition
set.seed(123)
ind <- sample(2, nrow(weather), replace = T, prob = c(.75, .25))
training <- weather[ind==1,1:7]
test <- weather[ind==2, 1:7]
trainingtarget <- weather[ind==1, 8]
testtarget <- weather[ind==2, 8]
# Normalization of data
m <- colMeans(training)
s <- apply(training, 420, sd)
training <- scale(training, center = m, scale = s)
test <- scale(test, center = m, scale = s)

# Creating Model for keras
model <- keras_model_sequential()
model %>% 
  layer_dense(units = 5, activation = 'relu', input_shape = c(7)) %>%
  layer_dense(units = 1)
summary(model)

# Compiling
model %>% compile(loss = 'mse',
                  optimizer = 'rmsprop',
                  metrics = 'mae')

# Fiting the Model
kerasmodel <- model %>%
  fit(training,
      trainingtarget,
      epochs = 200,
      batch_size = 32,
      validation_split = 0.2)
plot(kerasmodel)

# prediction
model %>% evaluate(test, testtarget)
pred <- model %>% predict(test)
mean((testtarget-pred)^2)
plot(testtarget, pred)

#fine-tuning (increase the dense layer)
model <- keras_model_sequential()
model %>% 
  layer_dense(units = 50, activation = 'relu', input_shape = c(7)) %>%
  layer_dropout(rate = 0.4) %>%
  layer_dense(units = 30, activation = 'relu', input_shape = c(7)) %>%
  layer_dropout(rate = 0.2) %>%
  layer_dense(units = 20, activation = 'relu', input_shape = c(7)) %>%
  layer_dropout(rate = 0.15) %>%
  layer_dense(units = 1)
# Compiling
model %>% compile(loss = 'mse',
                  optimizer = 'rmsprop',
                  metrics = 'mae')

# Fiting the Model
kerasmodel <- model %>%
  fit(training,
      trainingtarget,
      epochs = 200,
      batch_size = 32,
      validation_split = 0.2)
# prediction
model %>% evaluate(test, testtarget)
pred <- model %>% predict(test)
mean((testtarget-pred)^2)
plot(testtarget, pred)
#Change of activation function
model <- keras_model_sequential()
model %>% 
  layer_dense(units = 50, activation = 'selu', input_shape = c(7)) %>%
  layer_dropout(rate = 0.4) %>%
  layer_dense(units = 30, activation = 'selu', input_shape = c(7)) %>%
  layer_dropout(rate = 0.2) %>%
  layer_dense(units = 20, activation = 'selu', input_shape = c(7)) %>%
  layer_dropout(rate = 0.15) %>%
  layer_dense(units = 1)
# Compiling
model %>% compile(loss = 'mse',
                  optimizer = 'rmsprop',
                  metrics = 'mae')

# Fiting the Model
kerasmodel <- model %>%
  fit(training,
      trainingtarget,
      epochs = 200,
      batch_size = 32,
      validation_split = 0.2)
# prediction
model %>% evaluate(test, testtarget)
pred <- model %>% predict(test)
mean((testtarget-pred)^2)
plot(testtarget, pred)
#adding the learning rate
model <- keras_model_sequential()
model %>% 
  layer_dense(units = 50, activation = 'selu', input_shape = c(7)) %>%
  layer_dropout(rate = 0.4) %>%
  layer_dense(units = 30, activation = 'selu', input_shape = c(7)) %>%
  layer_dropout(rate = 0.2) %>%
  layer_dense(units = 20, activation = 'selu', input_shape = c(7)) %>%
  layer_dropout(rate = 0.15) %>%
  layer_dense(units = 1)
# Compiling
model %>% compile(loss = 'mse',
                  optimizer = optimizer_rmsprop(lr=0.001),
                  metrics = 'mae')

# Fiting the Model
kerasmodel <- model %>%
  fit(training,
      trainingtarget,
      epochs = 200,
      batch_size = 32,
      validation_split = 0.2)
# prediction
model %>% evaluate(test, testtarget)
pred <- model %>% predict(test)
mean((testtarget-pred)^2)
plot(testtarget, pred)