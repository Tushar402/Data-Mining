library(arules)
data(Groceries)
Groceries
myrules = apriori(Groceries)
inspect(myrules)
myrules=apriori(Groceries)
#writing 0 rules so we don't create any rule,with this high level of support and confidence 
#for this data set no rules could be generated.so we make the requirement for support and confidence lower.
#_c_minimize the support and confidence
myrules = apriori(Groceries,parameter = list(supp=0.05,conf=0.05))#34 rules made
inspect(myrules)#_d
#the lhs is empty which is not good as empty transaction
#so we should remove this empty lhs from the data set
#to avoid use minlength=2 means one item in lhs and one item in rhs that means true rules not empty rules

#_f
myrules = apriori(Groceries,parameter = list(supp=0.01,conf=0.01,minlen=3)) #minlength=3 means atleast total 3 items
#we remove empty rules
#_g
inspect(myrules[20])#find specific rule like 20no
#lift value letts >1 is good means strong association and the larger the better
#_h
cc=sort(myrules,by="lift")
inspect(cc[1:10])
#_i subset
hh=subset(myrules,subset=lhs%pin%"pork" & lift>1.2)
inspect(hh)
