#read file
ff = read.csv(file = "C:/Users/moshf/OneDrive/Desktop/Data/disease.txt",head=T,sep = ",")
ff
attach(ff)
plot(Age,Disease)
#by seeing scatter plot,here the target variable is categorical so we use logistic regression

#c_reg model
mymodel = glm(Disease~Age,data = ff,family = binomial())
summary(mymodel)

#Formula for logistic regression and estimated logit_vvi
#g(x) = log(P(x)/1-P(x)) = ßo+ß1(x) = -4.37+.06*x
#probability
# p(x)=e^(ßo+ß1*x)/1+e^ßo+ß1*x
gx = -4.37+0.06696*50
exp(gx)/(1+exp(gx))
#2
churn = read.csv(file = "C:/Users/moshf/OneDrive/Desktop/Data/churn.txt")
churn
names(churn)
attach(churn)
dim(churn)
length(churn)
#factorization
charnfactor = factor(Churn.)
charnfactor
mymodel = glm(charnfactor~VMail.Plan, family = binomial())
mymodel = glm(charnfactor~.,data = churn, family = binomial())
summary(mymodel)
#customer with churn
sum(Churn. == "True.")

#Odds ratio
pc1 = 0.0868
pc0 = 0.1671
(pc1/(1-pc1))/(pc0/(1-pc0))
#alternative
exp(-0.74780)
