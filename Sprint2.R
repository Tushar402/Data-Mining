#Q1
library(datasets)
data(airquality)
attach(airquality)
mean(Ozone)
is.na(Ozone)
sum(is.na(Ozone))
sum(is.na(airquality))
mean(na.rm=T,Ozone)
new_data=na.omit(airquality)
a= dim(airquality)
b= dim(new_data)
complete.cases(Ozone)
sum(complete.cases(Ozone))
a=which(is.na(airquality),arr.ind=T)
dim(a)  
#q2
library(DataExplorer)
a=plot_missing(airquality)
a
#Q3.a
cloathing = read.csv(file = "C:/Users/moshf/OneDrive/Desktop/Data/clothing_store.txt") #Load a data set

View(cloathing) #to see the data set view

names(cloathing)#see the variables names in the data set

dim(cloathing) #dim is the dimension of the data set, tells the observation and variable number in the data set

set.seed(42)#generate a random number which is not change for further calculation 

cloathing_nobs = nrow(cloathing)#to get the total row number(number of observation) of clothing data set

cloathing_train =sample(cloathing_nobs,0.70*cloathing_nobs)#cut the sample 70% from the total number of row(randomly) for the training data

cloathing_validation = sample(setdiff(seq_len(nrow(cloathing)),cloathing_train),0.30*cloathing_nobs) #setdiff sets the difference of two vectors, seq_len generates a sequence length
#we take the random number of only row, not observation and the sequence length of clothing and want a difference which are selected for the train data for the validation data.
#this code tells us later the row number for validation data when i create a validation data set.

length(cloathing_train)#length of training data

length(cloathing_validation)# length of validation data

#above code i just divide the training data and validation data
#now create two data set one is training data and another one is validation data from the total cloathing data set using randomly selected row numbers above.

training_data = cloathing[cloathing_train,] #take the row number from cloathing_train data and put them into training_data

validation_data = cloathing[cloathing_validation,]#take the row number from cloathing_validation data and put them into validation_data

dim(training_data)#dimension check how many observation in training data

dim(validation_data)#how many observation in validation data
 #now we done with partitioning our data set into training data and validation data
#e_find a variable with 2 unique level and 15217 unique level
attach(training_data)#to make the data set active so that we dont need to write everytime §...
names(training_data)
length(unique(HHKEY))#unique level/values for each of the observation in the training data
length(unique(ZIP_CODE))
length(unique(REC))
length(unique(FRE))
length(unique(MON))
length(unique(CC_CARD))
length(unique(AVRG))
length(unique(RESP))
#f distribution of the target variable RESP
hist(RESP)#create a histogram of the target variable
unique(RESP)#which unique values we have got
data_one = training_data[RESP==1,]#how many one we got

dim(data_one)
data_zero =training_data[RESP==0,]
dim(data_zero)

proportion_zero = 12682/sum(RESP)
proportion_zero
proportion_one =2535/sum(RESP)
proportion_one
#g
names(training_data)
training_wt_hhkey = training_data[,-1] #remove hhkey from training data
tri_wt_hhke_zip =training_wt_hhkey[,-1]#remove zip code
tri_wt_hhke_zip
names(tri_wt_hhke_zip)#check is it actually remove
#remove same for the validation data
vald_wt_hhkey = validation_data[,-1]
vald_wt_hhkey_zipcode = vald_wt_hhkey[,-1]
names(vald_wt_hhkey_zipcode)
attach(tri_wt_hhke_zip)
hi_trans = log(HI) #this variable describe product uniformity called HI. lig transformation for remove skewness
tr = tri_wt_hhke_zip[,-45]#remove 45 no column which is HI
tr=data.frame(tr,hi_trans)
names(tr)
#same for validation daaata
attach(vald_wt_hhkey_zipcode)
hi_trans = log(HI)#create a variable with log value
val=vald_wt_hhkey_zipcode[,-45]#remove the original variable HI
val= data.frame(val,hi_trans)
names(val)
#i
attach(tr)
amount23 =TMONSPEND-OMONSPEND
amount45= SMONSPEND-TMONSPEND
names(tr)
tr=tr[,-26]
names(tr)
tr=tr[,-27]
names(tr)
tr=data.frame(tr,amount23,amount45)
names(tr)
attach(val)
amount23 =TMONSPEND-OMONSPEND
amount45= SMONSPEND-TMONSPEND
names(val)
val=val[,-26]
names(val)
val=val[,-27]
val=data.frame(val,amount23,amount45)
names(val)
write.table(tr,"tr_cloth_prepare.txt",row.names = F)
write.table(val,"val_cloth_prepare.txt",row.names = F)
