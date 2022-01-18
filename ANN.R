#SPEI drought prediction using ANN (Artificial neural network)

install.packages("neuralnet")
library(neuralnet)
#Data
weather <- read_excel("D:/azmat/WESTERN RAJASTHAN/SIKAR/sikar weather.xlsx")
attach(weather)
#Normalization of data/scaling
maximum<-apply(weather, 420, max)
minimum<-apply(weather, 420, min)
weather <- as.data.frame(scale(weather,center = minimum,scale =maximum-minimum ))

#data partition
set.seed(123)
ind <- sample(2, nrow(data), replace = T, prob = c(0.7, 0.3))
training <- data[ind == 1,]
testing <- data[ind == 2,]
#Neural network
SPEI<-Tmax+Tmin+prcp+wind+Relativehumidity+solar+PET
allvaribale<-colnames(weather)
predvaribale<-allvaribale[!allvaribale%in%'SPEI']
predvaribale<-paste(predvaribale,collapse = '+')
SPEIformula<-as.formula(paste('SPEI',predvaribale,collapse = '+'))
#neural network 
set.seed(1234)
n <- neuralnet(formula=SPEIformula,
               data = training,
               hidden = c(5,3),
               linear.output = TRUE)
plot(n)
#prediction 
output <- compute(n, training[,-1])
head(output$net.result)
