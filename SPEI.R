#Standardized precipitation evapotranspiration index (SPEI) drought assessment using SPEI package
install.packages("SPEI")
library(SPEI)
#weather data
weather <- read_excel("D:/azmat/WESTERN RAJASTHAN/SIKAR/sikar weather.xlsx")
#computation of evapotranspiration
#using hargreaves method
weather$PET1<-hargreaves(Tmin =weather$Tmin,Tmax = weather$Tmax,lat = 25.44)
plot(weather$PET1)
#Using PEN-MAN FAO 56 method
weather$PET2<-penman(Tmin =weather$Tmin,Tmax = weather$Tmax,lat = 25.44,U2=weather$wind,Ra=weather$solar,RH=weather$humidity)
plot(weather$PET2)
#water deficit/surplus
waterbalance<-weather$Precipitation-weather$PET2
#SPEI calculation
SPEI<-spei(waterbalance,1) # here 1 reprsent time scale
plot.spei(SPEI)