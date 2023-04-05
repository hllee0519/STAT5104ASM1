source("dm.r")
#### Q1 start ###
#### Q1a start ###
# read file and save to d
d<-read.csv("bank-market.csv", na.strings="")


#select all data with deposit = no and save to d0
d0<-d[(d$deposit=="no"),]

#select all data with deposit = no and save to d1
d1<-d[(d$deposit=="yes"),]

#### Q1a end ###

#### Q1b start ###
mdist<-function(x) {
	t<-as.matrix(x) # transform x to a matrix
	m<-apply(t,2,mean) # compute column mean
	s<-var(t) # compute sample covariance matrix
	mahalanobis(t,m,s) # using built-in mahalanobis function
}

delout<-function(d, id, prop=0.99){
	# find Mahalnobis distance and save to m
	m<-mdist(d[,id])

	#finding outliers
	c<-qchisq(0.99, df=length(id))
	print(c)

	#del outliers
	dc<-d[m<c,]

	# see the differencesqaz
	n<-dim(d)
	nc<-dim(dc)
	cat('size of input=',n,' size of cleaned data=',nc, 'no of outliers deleted=',n-nc,'\n')

	# return
	dc
}
#### Q1b end ###

#### Q1c start ###
# delete outliers
x0 <- delout(d0, c(1,4,7))
x1 <- delout(d1, c(1,4,7))
#### Q1c end ###

#### Q1d start ###
# combine x0 and x1 to x
x<-rbind(x0, x1)
write.table(x,file="bank-market1.csv",sep=",",row.names=F)
#### Q1d end ###
#### Q1 end ###

#### Q2 start ###
#### Q2a start ###

# read cleaned file exported in q1
d<-read.csv("bank-market1.csv")

#set seed
set.seed(82475)

# save random 80% data of d to d0 as training set
out<-ransub(d, 0.85)
d0<-out$train
d1<-out$test

#### Q2a end ###

#### Q2b start ###
library(rpart)
# install.packages("rpart.plot")
library(rpart.plot)
y<-d0$deposit
ctree<-rpart(y~.,data=d0[,1:11],method="class")
rpart.plot(ctree,extra=101,box.palette="Grays")

print(ctree)

#### Q2b end ###

#### Q2c start ###
#training data
pr<-predict(ctree)
cl<-max.col(pr)
tab<-table(cl,d0$deposit)

print(tab)
print(erate(tab))

#testing data
pr2<-predict(ctree,d1)
c2<-max.col(pr2)
tab2<-table(c2,d1$deposit)

print(tab2)
print(erate(tab2))
#### Q2c end ###

#### Q2 end ###



