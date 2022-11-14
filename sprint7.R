#1.a
library(rpart)
library(rpart.plot)
data(iris)
#b_separate the data
n = nrow(iris)
set.seed(42)
train.id = sample(n,size = 2/3*n)
test.id = setdiff(1:n,train.id)
trainingdata = iris[train.id,]
validationdata = iris[test.id,]
dim(trainingdata)
dim(validationdata)
#c create a decision tree for training data
tree1 = rpart(Species~., data = iris, method = "class", subset = train.id)
tree1
#create a prediction for validation data
predc = predict(tree1,iris[-train.id,],type = "class") 
head(predc)

#d.summary(getting information of variables occurring in the data set)
list(train = summary(trainingdata), test = summary(validationdata))

#e. confusion matrix to check how good our model is
table(predc, iris[-train.id, "Species"])
# got two misclassifications

#f_hyperparameters- are argument that can be modified to reduce overfitting
rpart.control()

#g_draw a decision tree
prp(tree1, box.col = c("pink","red","green")[tree1$frame$yval])

#Q2-a
churn = read.csv(file = "C:/Users/moshf/OneDrive/Desktop/Data/churn.txt")
churn

#b-Data Partition
n = nrow(churn)
set.seed(42)
train.id = sample(n,size = .70*n)
test.id = setdiff(1:n,train.id)
training_data = churn[train.id,]
test_data = churn[test.id,]
training_data

#c-Tree construction
tree2 = rpart(Churn. ~ .,data = training_data, method = "class")
tree2
rpart.plot(tree2)
#draw the tree
prp(tree2, box.col = c("pink","red")[tree2$frame$yval])

names(churn)#check the variables name and phone is in the 4th column

#d-remove the variable phone
train = training_data[,-4]
test = test_data[,-4]

#e-construct a tree without phone for training dataset
tree3 = rpart(Churn. ~ .,data = train, method = "class")
tree3
rpart.plot(tree3)
#f_draw the tree
prp(tree3, box.col = c("pink","green")[tree2$frame$yval])

#g_hyperparameter to tune the tree
tree4 = rpart(Churn. ~ .,data = train, method = "class", control = rpart.control(maxcompete = 3, maxdepth = 2))
tree4
#h_draw the tree
prp(tree4, box.col = c("pink","green")[tree4$frame$yval])
rpart.plot(tree4)
