library(nnet)
churn = read.csv("file://C:/Users/moshf/OneDrive/Desktop/Data/churn.txt")
churn
set.seed(42)
n = nrow(churn)
train.id = sample(n,size = .70*n)
test.id = setdiff(1:n,train.id)
traindata = churn[train.id,] 
test= churn[test.id,]
#c_construct neural network
churnfactor =factor(traindata$Churn.)
mynn = nnet(churnfactor~.,data = traindata,size = 2,maxit =200)
#d too many weights_overfitting and the algorithm cant converge
#e
day.min = traindata$Day.Mins
vmail.plan = traindata$VMail.Plan
use_data = data.frame(day.min,vmail.plan,churnfactor)
mynn = nnet(churnfactor~.,data = use_data,size = 2)
mynn
#g
summary(mynn)
#g
mynn = nnet(churnfactor~.,data = use_data,size = 20,maxit =200)
summary(mynn)
