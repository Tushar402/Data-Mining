#load the data set iris
data(iris)
#load the package rpart
library(rpart)
#load the package rplot
library(rpart.plot)
#construct a decision tree model for classification task
#iris data set and target variable is species and method=class means we are use a classification method
iristree = rpart(Species ~ .,data = iris, method = "class")
#c,#no of row in iris data set
n = nrow(iris)
#set seed 42 not to change the value for further calculation
set.seed(42)
#how many observation(row number) in total data set n and how many i need for sample
train.id =sample(n,size = 2/3*n)
#set difference with the train row and take other row no for test data
test.id = setdiff(1:n, train.id)
#d#rpart recursive partitioning and regression tree# predict from train tree by removing train.id and type is classification
traintree = rpart(Species ~ .,data = iris, method ="class", subset = train.id)
#e# predict from train tree by removing train.id and type is classification
pred =predict(traintree, iris[-train.id,],type="class")
head(pred)
#g-make a confusion matrix-we are very successfull just 1 misclasification for versicolor and one for verginica.
table(pred, iris[-train.id, "Species"])
#2-a
#read the data set
ff = read.csv(file = "D:/FRA-UAS/5Th Semester/2. Data Mining/Sprint 2_Data Pre-processing/Sprint 2 Data pre-processing-20220423/Data/disease.txt", header = T, sep = ",")
ff
#decision tree to predict target disease
diseasetree =rpart(Disease ~ ., data = ff, method = "class")
pred = predict(diseasetree, ff, type = "class")
pred
#d-construct confusion matrix-5 misclassification for false negative
table(pred, ff[,"Disease"])
#e-target level is probability instead of class
p2 = predict(diseasetree,ff, type = "prob")
p2#probability for being 0 and prob for 0
#f_construct a ROC curve
install.packages("ROCR")
library(ROCR)
pr3 = p2[,2]#[,2 means 2 no column and 2,means 2 no row] take 2no column from p2
pr = prediction(pr3, ff$Disease)
perf = performance(pr,"tpr","fpr")
plot(perf)
#calculate the area under the ROC curve
performance(pr,"auc")
?rpart.control()
 