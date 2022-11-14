library(randomForest)
library(rpart)
n = nrow(iris)
set.seed(42)
train.id = sample(n,size = 0.7*n)
test.id = setdiff(1:n,train.id)
train= iris[train.id,]
test = iris[test.id,]
tree1 = rpart(Species~.,data = iris,method = "class",subset = train.id)
tree1
pred = predict(tree1,iris[-train.id,],type= "class")
table(pred, iris[-train.id, "Species"])
#random forest
rf = randomForest(Species~.,data = iris,subset = train.id)
pred2=predict(rf,iris[-train.id,],type = "class")
confmat=table(pred2,iris[-train.id,"Species"])
confmat
#same result in both case
#q2
churn = read.csv(file= "C:/Users/moshf/OneDrive/Desktop/Data/churn.txt")
attach(churn)
n =nrow(churn)
set.seed(42)
train.id = sample(n,size = 0.7*n)
test.id = setdiff(1:n,train.id)
train= churn[train.id,]
test = churn[test.id,]
names(churn)
train=train[,-4]
test=test[,-4]
tree2=rpart(Churn.~.,data = train,method = "class")
pred2=predict(tree2,churn[-train.id,],type = "class")
table(pred2, churn[-train.id, "Churn."])
churnfactor=factor(train$Churn.)
churn2=factor(test$Churn.)
testrf =data.frame(test,churn2)
rf2 = randomForest(churnfactor~.,data = train, method ="class")
pred3=predict(rf2,churn[-train.id,],type = "class")
table(pred3,churn[-train.id,"Churn."])
