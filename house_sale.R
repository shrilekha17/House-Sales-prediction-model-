library(readxl)
install.packages("caTools")
library(caTools)  

kc_house_data <- read.csv("/Users/shrilekha/Downloads/kc_house_data 2.csv")
hs<-kc_house_data
hs<-hs[,c(-1,-2)]
head(hs,6)


##--1.Sampling dataset into train and test

ind<-(sample(nrow(hs),nrow(hs)*0.7))
train_hs<-hs[ind,] 
test_hs<-hs[-ind,] 


##--2.We will use train dataset now onwards
colnames(train_hs)
#1a.Bedrooms (Log tranformation on price gives linear relationship)
boxplot((price)~bedrooms,data=train_hs)
#median of the boxplot of each bedroom shows some relationship between bedroom
#and housing prices

#if we transform price to log(price) we get almost "linear relationship" between
#bedrooms and housing price
plot(train_hs$bedrooms,log(train_hs$price))

boxplot(log(price)~bedrooms,data=train_hs)
lines(c(0,33),c(12.25,17))



#1b.Bathroom (Log tranformation on price gives linear relationship)
boxplot((price)~bathrooms,data=train_hs)
#median of the boxplot of each bathroom shows some relationship between bathroom
#and housing prices

#if we transform price to log(price) we get almost "linear relationship" between
#bedrooms and housing price
boxplot(log(price)~bathrooms,data=train_hs)
lines(c(0,100),c(12,25))



#1c.sqft_living(shows linear transformation) sqrt transformation on x and log on price
plot(train_hss$sqft_living,train_hss$price)
plot(train_hss$sqft_living,log(train_hss$price))
train_hs$sqft_living_new<-sqrt(train_hs$sqft_living)
plot(train_hs$sqft_living_new,log(train_hs$price))

#1d.sqft_lot(exponential (sqft_lot) +log(price) transformation)
plot(hs$sqft_lot,hs$price)
#it seems some exponential relationship between sqft_lot and price
#asymptote at price=0.5*10^6
train_hs$sqft_lot_new<-10^6*(6.5*(0.2)^(train_hs$sqft_lot/10^4)+0.5)
plot(train_hs$sqft_lot_new,(train_hs$price))
plot(train_hs$sqft_lot_new,log((train_hs$price)))


#1d.floors (Log tranformation on price gives linear relationship)
boxplot((price)~floors,data=train_hs)
#median of the boxplot of each floors shows some relationship between floors
#and housing prices

#if we transform price to log(price) we get almost "linear relationship" between
#floors and housing price
boxplot(log(price)~floors,data=train_hs)
lines(c(0,10),c(12.5,15))

colnames(hs)



#1e.waterfront
#consider 0 as no waterfront view and 1 as waterfront view

boxplot((price)~waterfront,data=train_hs)
#From the graph we can see avg price is high for the houses
#havihng waterfrint view
boxplot(log(price)~waterfront,data=train_hs)


colnames(hs)
#1.f.view
boxplot((price)~view,data=train_hs)
boxplot(log(price)~view,data=train_hs)
lines(c(0,15),c(12.5,17))
#avg price increases  if it has been viewed more than once


#1.g condition
boxplot((price)~condition,data=train_hs)
boxplot(log(price)~condition,data=train_hs)
lines(c(0,25),c(12.5,17))
#avg price increases  with the condition of the house


#1.h.grade
boxplot((price)~grade,data=train_hs)
boxplot(log(price)~grade,data=train_hs)
lines(c(0,20),c(11.5,17))
#avg price increases  with the grade of the house


#1.i&j.sqft_above and sqft_basement are linear combination of sqft_living
plot(train_hs$sqft_above,train_hs$price)
plot(train_hs$sqft_basement,train_hs$price)
hs_dum<-train_hs
hs_dum$new<-hs_dum$sqft_above+hs_dum$sqft_basement
head(hs_dum$new)
head(hs_dum$sqft_living)
cor(hs_dum$sqft_living,hs_dum$new)

colnames(hs)
#1.k.yr_built
plot(train_hs$yr_built,train_hs$price)



#1.l. yr_renovated modified
train_hs$yr_renovated_new<-ifelse(train_hs$yr_renovated==0,train_hs$yr_built,train_hs$yr_renovated )
plot(hs$yr_renovated,hs$price)

plot(train_hs$yr_renovated_new,train_hs$price)



#1.m. zipcode #remove
plot(train_hs$zipcode,train_hs$price)



#1.n. lat
plot(train_hss$lat,((train_hss$price)))

plot(train_hss$lat,log((train_hss$price)))
train_hs$lat_new<-poly(train_hs$lat-mean(train_hs$lat),2)
plot(train_hs$lat_new,log((train_hs$price)))

lines(c(47.2,49),c(12,17),col="red")


#1.o long

plot(train_hs$long,((train_hs$price)))

plot(train_hs$long,log((train_hs$price)))
lines(c(-122.4,-121.4),c(13.5,13),col="red")


#1.p sqft_living15
plot(train_hss$sqft_living15,train_hss$price)
plot(train_hss$sqft_living15,log(train_hss$price))
train_hs$sqft_living15_new<-sqrt(train_hs$sqft_living15)
plot(train_hs$sqft_living15_new,log(train_hs$price))

#1.q sqft_lot15
plot(hs$sqft_lot15,hs$price)
train_hs$sqft_lot15_new<-10^6*(6.5*(0.2)^(train_hs$sqft_lot15/10^4)+0.5)
plot(train_hs$sqft_lot15_new,(train_hs$price))
plot(train_hs$sqft_lot_new,log((train_hs$price)))



##2.models#########################

#2a.--without any transformed variable
fit<-lm((price)~bedrooms+bathrooms+sqft_living+sqft_lot+floors+waterfront+view+condition+
          grade+yr_built+yr_renovated+
          lat+long+sqft_living15+sqft_lot15,data=hs)

summary(fit)
#problem with the summary

#2b. try using transformed model
fit<-lm(I(log(price))~bedrooms+bathrooms+sqft_living_new+sqft_lot_new+floors+waterfront+view+condition+
          grade+yr_built+yr_renovated_new+
          lat_new+long+sqft_living15_new+sqft_lot15_new,data=train_hs)

summary(fit)
qqnorm(residuals(fit))
plot(fitted.values(fit),rstudent(fit))
cooks.distance(fit)


library(leaps)
attach(train_hs)
colnames(train_hs)

all <- regsubsets(x=cbind(bedrooms,bathrooms,sqft_living_new,sqft_lot_new,floors,waterfront,view,condition,
                          grade,yr_built,yr_renovated_new,
                          lat_new,long,sqft_living15_new,sqft_lot15_new), y=log(price),  method = "exhaustive", 
                  data=train_hs, all.best = FALSE, nbest = 3)


train_hs<-train_hs[,c(-5,-11,-12,-14,-19)]
colnames(train_hs)
summary(all)
Cp <- summary(all)$cp
AdjR2 <- summary(all)$adjr2
SSRes <- summary(all)$rss
R2 <- summary(all)$rsq
Matrix <- summary(all)$which
p <- apply(Matrix,1, sum)
MSE <- SSRes/(nrow(train_hs)-p)
output <- cbind(p, Matrix, SSRes, R2, AdjR2, MSE, Cp)

#exaustive method	
#sqft_living	grade	lat year_built
#sqft_living	grade	 view lat yr_built
#sqft_living	gradeview condition	lat yr_built view

################################fwd########################
fit.0 <- lm(I(log(price))~1, data = train_hs)
add1(fit.0, I(log(price))~bedrooms+bathrooms+sqft_living_new+sqft_lot_new+floors+waterfront+view+condition+
     grade+yr_built+yr_renovated_new+zipcode+
     lat+long+sqft_living15+sqft_lot15_new, test = "F")
#grade is having high F value and P and (14864.371 < 2.2e-16 ***)

fit.1 <- lm(I(log(price))~grade, data = train_hs)
add1(fit.1, I(log(price))~bedrooms+bathrooms+sqft_living+sqft_lot_new+floors+waterfront+view+condition+
       grade+yr_built+yr_renovated_new+zipcode+
       lat+long+sqft_living15+sqft_lot15_new, test = "F")
#lat is havung high F and p (5665.4782 < 2.2e-16 ***)


fit.2 <- lm(I(log(price))~grade+lat, data = train_hs)
add1(fit.2, I(log(price))~bedrooms+bathrooms+sqft_living+sqft_lot_new+floors+waterfront+view+condition+
       grade+yr_built+yr_renovated_new+zipcode+
       lat+long+sqft_living15+sqft_lot15_new, test = "F")
#sqft_liing is havung high F and p ( 3732.9101 < 2.2e-16 ***)


fit.3 <- lm(I(log(price))~grade+lat+sqft_living, data = train_hs)
add1(fit.3, I(log(price))~bedrooms+bathrooms+sqft_living+sqft_lot_new+floors+waterfront+view+condition+
       grade+yr_built+yr_renovated_new+zipcode+
       lat+long+sqft_living15+sqft_lot15_new, test = "F")
#yr_built is havung high F and p ( 1644.4441 < 2.2e-16 ***)



fit.4 <- lm(I(log(price))~grade+lat+sqft_living+yr_built, data = train_hs)
add1(fit.4, I(log(price))~bedrooms+bathrooms+sqft_living+sqft_lot_new+floors+waterfront+view+condition+
       grade+yr_built+yr_renovated_new+zipcode+
       lat+long+sqft_living15+sqft_lot15_new, test = "F")
#view is havung high F and p (822.9863 < 2.2e-16 ***)



fit.5 <- lm(I(log(price))~grade+lat+sqft_living+yr_built+view, data = train_hs)
add1(fit.5, I(log(price))~bedrooms+bathrooms+sqft_living+sqft_lot_new+floors+waterfront+view+condition+
       grade+yr_built+yr_renovated_new+zipcode+
       lat+long+sqft_living15+sqft_lot15_new, test = "F")
#bathroom is havung high F and p (290.5905 < 2.2e-16)





fit.6 <- lm(I(log(price))~grade+lat+sqft_living+yr_built+view+bathrooms, data = train_hs)
add1(fit.6, I(log(price))~bedrooms+bathrooms+sqft_living+sqft_lot_new+floors+waterfront+view+condition+
       grade+yr_built+yr_renovated_new+zipcode+
       lat+long+sqft_living15+sqft_lot15_new, test = "F")
#sqft_living15 is havung high F and p (321.5034 < 2.2e-16)



fit.7 <- lm(I(log(price))~grade+lat+sqft_living+yr_built+view+bathrooms+sqft_living15, data = train_hs)
add1(fit.7, I(log(price))~bedrooms+bathrooms+sqft_living+sqft_lot_new+floors+waterfront+view+condition+
       grade+yr_built+yr_renovated_new+zipcode+
       lat+long+sqft_living15+sqft_lot15_new, test = "F")
#sqft_lot15_new is havung high F and p (351.7112 < 2.2e-16 ***)


fit.8 <- lm(I(log(price))~grade+lat+sqft_living+yr_built+view+bathrooms+sqft_living15
            +sqft_lot15_new, data = train_hs)
add1(fit.8, I(log(price))~bedrooms+bathrooms+sqft_living+sqft_lot_new+floors+waterfront+view+condition+
       grade+yr_built+yr_renovated_new+zipcode+
       lat+long+sqft_living15+sqft_lot15_new, test = "F")
#condition is havung high F and p (310.1990 < 2.2e-16 ***)



fit.9 <- lm(I(log(price))~grade+lat+sqft_living+yr_built+view+bathrooms+sqft_living15
            +condition+sqft_lot15_new, data = train_hs)
add1(fit.9, I(log(price))~bedrooms+bathrooms+sqft_living+sqft_lot_new+floors+waterfront+view+condition+
       grade+yr_built+yr_renovated_new+zipcode+
       lat+long+sqft_living15+sqft_lot15_new, test = "F")
#yr_renovated_new is havung high F and p (65.5922 5.966e-16 )





fit.10 <- lm(I(log(price))~grade+lat+sqft_living+yr_built+view+bathrooms+sqft_living15
            +yr_renovated_new+condition+sqft_lot15_new, data = train_hs)
add1(fit.10, I(log(price))~bedrooms+bathrooms+sqft_living+sqft_lot_new+floors+waterfront+view+condition+
       grade+yr_built+yr_renovated_new+zipcode+
       lat+long+sqft_living15+sqft_lot15_new, test = "F")
#waterfront is havung high F and p ( 268.6819 < 2.2e-16 *** )




fit.11 <- lm(I(log(price))~grade+lat+sqft_living+yr_built+view+bathrooms+sqft_living15
             +waterfront+yr_renovated_new+condition+sqft_lot15_new, data = train_hs)
add1(fit.11, I(log(price))~bedrooms+bathrooms+sqft_living+sqft_lot_new+floors+waterfront+view+condition+
       grade+yr_built+yr_renovated_new+zipcode+
       lat+long+sqft_living15+sqft_lot15_new, test = "F")
#zipcode is havung high F and p ( 186.9851 < 2.2e-16 *** )




fit.12 <- lm(I(log(price))~grade+lat+sqft_living+yr_built+view+bathrooms+sqft_living15
            +zipcode +waterfront+yr_renovated_new+condition+sqft_lot15_new, data = train_hs)
add1(fit.12, I(log(price))~bedrooms+bathrooms+sqft_living+sqft_lot_new+floors+waterfront+view+condition+
       grade+yr_built+yr_renovated_new+zipcode+
       lat+long+sqft_living15+sqft_lot15_new, test = "F")
#floors is havung high F and p (36.8814 1.285e-09 *** )




fit.13 <- lm(I(log(price))~grade+lat+sqft_living+yr_built+view+bathrooms+sqft_living15
            +floors +zipcode +waterfront+yr_renovated_new+condition+sqft_lot15_new, data = train_hs)
add1(fit.13, I(log(price))~bedrooms+bathrooms+sqft_living+sqft_lot_new+floors+waterfront+view+condition+
       grade+yr_built+yr_renovated_new+zipcode+
       lat+long+sqft_living15+sqft_lot15_new, test = "F")
#bedrooms is havung high F and p (18.0178 2.202e-05 )

fit.14 <- lm(I(log(price))~grade+lat+sqft_living+yr_built+view+bathrooms+sqft_living15
             +bedrooms+floors +zipcode +waterfront+yr_renovated_new+condition+sqft_lot15_new, data = train_hs)
add1(fit.14, I(log(price))~bedrooms+bathrooms+sqft_living+sqft_lot_new+floors+waterfront+view+condition+
       grade+yr_built+yr_renovated_new+zipcode+
       lat+long+sqft_living15+sqft_lot15_new, test = "F")









################################bck########################
fit.f_1 <- lm(I(log(price))~bedrooms+bathrooms+sqft_living_new+sqft_lot_new+floors+waterfront+view+condition+
              grade+yr_built+yr_renovated_new+zipcode+
              lat_new+long+sqft_living15_new+sqft_lot15_new
              , data = train_hs)
drop1(fit.f_1, I(log(price))~bedrooms+bathrooms+sqft_living_new+sqft_lot_new+floors+waterfront+view+condition+
        grade+yr_built+yr_renovated_new+zipcode+
        lat_new+long+sqft_living15_new+sqft_lot15_new
      , test = "F")

#sqft_lot_new gone
fit.f_1 <- lm(I(log(price))~bedrooms+bathrooms+sqft_living_new+floors+waterfront+view+condition+
                grade+yr_built+yr_renovated_new+zipcode+
                lat_new+long+sqft_living15_new+sqft_lot15_new
              , data = train_hs)
drop1(fit.f_1, I(log(price))~bedrooms+bathrooms+sqft_living_new+floors+waterfront+view+condition+
        grade+yr_built+yr_renovated_new+zipcode+
        lat_new+long+sqft_living15_new+sqft_lot15_new
      , test = "F")


summary(fit.f_1)


qqnorm(residuals(fit.f_1))
plot(fitted.values(fit.f_2),rstudent(fit.f_1))



#correlation between yr_built and yr_renovated_new, sqft_living and sqft_living15 is very high
#sqft_lot_new and sqft_lot15_new and grade&sqft_living_new
#zipcode removal no relation

cor(train_hs)
diag(solve(cor(train_hs)))
#########exaustive fwd backward and correelation
final<-lm(I(log(price))~sqft_living_new+yr_built+view+grade+
           lat_new, data=train_hs)
  
summary(final)
qqnorm(residuals(final))
plot(fitted.values(final),rstudent(final))
(cooks.distance(final))[(cooks.distance((final)))>1]

##################3.correlation
diag(solve(cor(train_hs[,-1])))


#####.4 residual


#Created X matrix 
#X<-as.matrix(cbind(1,train_hs$bedrooms,train_hs$bathrooms,train_hs$sqft_living,
                  # train_hs$floors,train_hs$waterfront,train_hs$view,
                   #train_hs$condition,train_hs$grade,train_hs$yr_built,
                   #train_hs$yr_renovated_new,train_hs$zipcode,train_hs$lat,
                   #train_hs$long+train_hs$sqft_living15))

X<-as.matrix(cbind(1,(train_hs$sqft_living_new),
                   train_hs$grade,train_hs$yr_built,
                   train_hs$view,train_hs$lat_new)
                   )
#Created Hat Matrix
H<-X%*%solve((t(X)%*%X))%*%t(X)
head(H)
#Diagonal elements of Hat matrix
diag(H)[diag(H)>0.0007931787]
12/15129


plot(train_hs$condition,rstudent(final))


plot(train_hs$yr_renovated_new,rstudent(final))



colnames(train_hs)











base_world() +
  geom_point(data=train_hs, 
             aes(x=Longitude, y=Latitude, size=price), colour="Deep Pink", 
             fill="Pink",pch=21, alpha=I(0.7)) 

head((rstandard(final)),6)
as.matrix(rstudent(final)[rstudent(final)>=3])
a<-head(train_hs$price,6)
b<-head(exp((predict(final,newdata = train_hs))),6)
(log(a)-log(b))/0.2488
summary(final)
biacomanager
library(gmap)
ggplot() + geom_polygon(aes(x=long,y=lat,group=zipcode,fill=price),data=train_hs) + theme_minimal()
