brkfst = read.csv("file://C:/Users/moshf/OneDrive/Desktop/Data/cereals.dat",sep = "",)
brkfst
install.packages("rgl")
library(rgl)
attach(brkfst)
plot3d(SUGARS,FIBER,RATING)
#c_perform a simple linear regression
summary(lm(RATING~SUGARS))
#estimated regression line: sugar = 59.28-2.4*sugar
#d
summary(lm(RATING~FIBER))
#e
summary(lm(RATING~.))
#f_residual analysis
brkfst[66,]
#What is the meaning of a positive/negative sign of the residual
#positive means underestimate and negative means overestimate
#2
plot(RATING,SHELF)
dim(brkfst)
#creat a dummy variable
shelf1 = seq(0,0,length.out = 77)
shelf2 = seq(0,0,length.out =77)
#compare
shelf1[SHELF==1]=1
shelf1[SHELF==2]=0
shelf1[SHELF==3]=0
shelf2[SHELF==1]=0
shelf2[SHELF==2]=1
shelf2[SHELF==3]=0
#cereat a regression model
summary(lm(RATING~SUGARS+FIBER+shelf1+shelf2))
#3a_AIC
library(MASS)
attach(Cars93)
View(Cars93)
sss = lm(Price~RPM+Horsepower+Weight+Passengers+MPG.city+MPG.highway)
extractAIC(sss)
ss = lm(Price~RPM+Horsepower+Weight+Passengers+MPG.city)
extractAIC(ss)
s = lm(Price~RPM+Horsepower+Weight+Passengers)
extractAIC(s)
#3.b_BIC
n = length(Cars93)
n
extractAIC(sss,k=log(n))
extractAIC(ss,k=log(n))
extractAIC(s,k=log(n))
#e_mallow cP
summ = summary(lm(Price~RPM))
summ
sig =summ$sigma#sig is residual standard error
sig
extractAIC(sss,scale = sig*sig)#sig square is square of residual standard error
extractAIC(s,scale = sig*sig)
extractAIC(ss,scale = sig*sig)
 #4_1backward elimination_worsest parameter exclude first
myback = step(lm(Price~RPM+EngineSize+MPG.city+MPG.highway+Cylinders+Horsepower+Weight+Passengers))
#forward selection_empty bucket and add the best one
mymatrix =data.frame(RPM,EngineSize,MPG.city,MPG.highway,Cylinders,Horsepower,Weight,Passengers)
nullmodel = lm(Price~1,data = mymatrix)
myforwrd = step(nullmodel,scope = formula(mymatrix),direction = "both")
#stepwise selection_adding and deleting and check
stepwise = step(lm(Price~RPM+EngineSize+MPG.city+MPG.highway+Cylinders+Horsepower+Weight+Passengers),direction = "both")
