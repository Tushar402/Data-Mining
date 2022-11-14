#creat a scatter ploat
#distance
y = c(10,11,12,13,14,15,20,18,22,25)
#time
x = c(2,2,3,4,4,5,6,7,8,9)
#plot the values
plot(x,y)
#linear regression lm function(dependent~independent)
myvalues = lm(y~x)
myvalues
plot(y~x)
#creat the regression line
abline(myvalues)
#estimated regression line
y = 6+2*x
#summary function tells how high the coefficient of determination
summary(myvalues)
#pr(>|t|) is the significance level_p value
#r square is .94 or 94% means x is explaining 94% of the variation in y
#For one variable no difference but if the variable increase R square increases but adjusted R square penalize  
#prediction for 3 hours/ be careful with zero
y = 6+2*3
y
#prediction for 30 hours/not much out of range
y = 6+2*30
y
#e residual analysis-how good the model is
#get residuals into myres from the model
myres = residuals(myvalues)
mean(myres)#near to zero
#get predicted values into myfit
myfit = fitted(myvalues)
#plot myfit myres
plot(x,myfit)
plot(x,myres)
#or can ploat together this is good
plot(myfit,myres)
#f_updated the data set
xx = c(2,2,3,4,4,5,6,7,8,9,16)
yy = c(10,11,12,13,14,15,20,18,22,25,39)
#scatter plot with updated data set
plot(xx,yy)
#g_estimate the regression line
myvalues = lm(yy~xx)
summary(myvalues)
#h_influential observation
myinfluence = influence.measures(myvalues)
 myinfluence
#it says that 11 observation is influential but dont say which one exactly influencing
 which(apply(myinfluence$is.inf,1,any))
 summary(myinfluence)
#is tells that coocks d dont tell the influence other parameter says that which is marked with *
 
 #i_return to the original data
 yyy = c(10,11,12,13,14,15,20,18,22,25,20)
 xxx = c(2,2,3,4,4,5,6,7,8,9,5) 
plot(yyy,xxx) 
myvalues = lm(yyy~xxx)
summary(myvalues)
#Q_2
#load the dataset basebsll into r
baseball = read.csv("file://C:/Users/moshf/OneDrive/Desktop/Data/baseball.txt",sep = "",)
baseball
#b_restrict data set only observation x>=5 who has 100 at bat/make a subset from original dataset
baseball_new = baseball[baseball$at_bats>=100,]
baseball_new$at_bats
dim(baseball_new)
#d_scatter plot
plot(baseball_new$bat_ave,baseball_new$homeruns)
#not linear transformation required
#f_regression analysis without any transformation
myreg = lm(baseball_new$homeruns~baseball_new$bat_ave)
summary(myreg)
#estimated regression line y= -28.14+153.55*x

#perform a residual analysis
myres = residuals(myreg)
plot(baseball_new$bat_ave,myres)
#qq plot says the deviation from normality
qqnorm(myres)
qqline(myres)
#use a logarithimic transformation
ln_home_runs =log(baseball_new$homeruns)
bat_avg = baseball_new$bat_ave
plot(bat_avg,ln_home_runs)
total = data.frame(ln_home_runs,bat_avg)
total_new = total[total$ln_home_runs>=-10,]
myreg = lm(total_new$ln_home_runs~total_new$bat_avg)
summary(myreg)
#perform a new residual analysis
myres = residuals(myreg)
plot(total_new$bat_avg,myres)
qqnorm(myres)
qqline(myres)
