#This program is used to perform analysis.

set.seed(222122)
#Load packages.
library("tree")
library("e1071")
library("randomForest")
library("ggplot2")
library("glmnet")

#Read stocks
DD <- read.csv("DD_050610_14%3A42_14%3A50_5.csv",header=T)
TSCO <- read.csv("TSCO_042111_07%253A00_15%253A55_5.csv",header=T)
XOM <- read.csv("XOM_042111_07%253A00_15%253A55_5.csv",header=T)
#Plot sampled stocks
plot.ts(TSCO$LastTransactedPrice[100000:105000],ylab="stock price",main="Up case")
plot.ts(XOM$LastTransactedPrice[100000:110000],ylab="stock price",main="Three cases")
plot.ts(DD$LastTransactedPrice[32000:50000],,ylab="stock price",main="Down case")

#Load data
tsco <- read.csv("data.TSCO.csv",header=T)
dd <- read.csv("data.DD.csv",header=T)
xom <- read.csv("data.XOM.csv",header=T)
#Drop the first column, which is meaningless.
tsco <- tsco[,-1]
dd <- dd[,-1]
xom <- xom[,-1]

#Get the correct y label.
#Up case, with value = 1.
#Stationary, with value = 0
stock <- TSCO
a <- 100000
k <- 3000
data <- stock[(a+1):(a+k),]
#s=350, 1 second.
y <- rep(-0,k)
y[which(stock$Bid_Price1[(a+1+350):(a+k+350)]>data$Ask_Price1)] <- 1
#replace
tsco$y <- as.factor(y)

#Stationary, with value = 0
#Down case, which value = 1
stock <- DD
a <- 32000
k <- 3000
data <- stock[(a+1):(a+k),]
#s=500, 2 seconds.
y <- rep(0,k)
y[which(stock$Ask_Price1[(a+1+500):(a+k+500)]<data$Bid_Price1)] <- 1
#replace
dd$y <- as.factor(y)

#Mix, down with value = -1 and up with value = 1, stationary = 0.
stock <- XOM
a <- 100000
k <- 3000
data <- stock[(a+1):(a+k),]
#s=300, 10 seconds.
y <- rep(0,k)
y[which(stock$Bid_Price1[(a+1+300):(a+k+300)]>data$Ask_Price1)] <- 1
y[which(stock$Ask_Price1[(a+1+300):(a+k+300)]<data$Bid_Price1)] <- -1
#replace
xom$y <- as.factor(y)

#1

#Build single tree for "Up" case.
tree.up = tree(y~., data=tsco)
#Prune by misclassification rate.
cv.up = cv.tree(tree.up,FUN =prune.misclass )
cv.up
#7 nodes is the best.
prune.up = prune.misclass(tree.up,best =7)
plot(prune.up)
text(prune.up, pretty =0)

#Build single tree for "Down" case.
tree.down = tree(y~., data=dd)
#Prune by misclassification rate.
cv.down = cv.tree(tree.down,FUN =prune.misclass )
cv.down
#7 nodes is the best.
prune.down = prune.misclass(tree.down,best =10)
plot(prune.down)
text(prune.down, pretty =0)

#Build single tree for "mix" case.
tree.mix = tree(y~., data=xom)
#Prune by misclassification rate.
cv.mix = cv.tree(tree.mix,FUN =prune.misclass )
cv.mix
#7 nodes is the best.
prune.mix = prune.misclass(tree.mix,best =9)
plot(prune.mix)
text(prune.mix, pretty =0)

#2

#Compare accuracy between SVM and trees.
#Define functions to testing 10-fold cross validation.
#10-fold CV for linear kernel SVM
#data is the input stock data, l is the cost.
cv.linear <- function(data,l){
  #Get the number of observations
  n <- nrow(data)
  #Define 10-fold CV error
  error <- rep(0,n)
  #Define the predicted values.
  y.linear <- rep(0,n) 
  #Define random indices.
  random.ind <- sample(n)
  for (i in 1:10){
    #Divide the entire data set as training and test by indices
    ind <- random.ind[(i*n/10-n/10+1):(i*n/10)]
    #Get SVM model
    mod.linear =svm(y~., data=data[-ind,], kernel ="linear",cost =l,scale=T)
    #Predict
    y.linear[ind] <- predict(mod.linear,data[ind,],scale=T)
    print(i)
  }
  y.linear <- as.numeric(y.linear)-1
  #Compute the test error.
  error[which((data$y == y.linear)==F)] <- 1
  #Print the table
  print(table(predict=y.linear , truth = data$y))
  return(error)
}

#10-fold CV for cubic kernel SVM
#data is the input stock data, l is the cost.
cv.cubic <- function(data,l){
  #Get the number of observations
  n <- nrow(data)
  #Define 10-fold CV error
  error <- rep(0,n)
  #Define the predicted values.
  y.cubic <- rep(0,n) 
  #Define random indices.
  random.ind <- sample(n)
  for (i in 1:10){
    #Divide the entire data set as training and test by indices
    ind <- random.ind[(i*n/10-n/10+1):(i*n/10)]
    #Get SVM model
    mod.cubic =svm(y~., data=data[-ind,], kernel ="polynomial", cost =l,scale=T)
    #Predict
    y.cubic[ind] <- predict(mod.cubic,data[ind,],scale=T)
    print(i)
  }
  y.cubic <- as.numeric(y.cubic)-1
  #Compute the test error.
  error[which((data$y == y.cubic)==F)] <- 1
  #Print the table
  print(table(predict=y.cubic , truth = data$y))
  return(error)
}

#10-fold CV for radial kernel SVM
#data is the input stock data, g is the tuning parameter, l is the cost.
cv.radial <- function(data,g,l){
  #Get the number of observations
  n <- nrow(data)
  #Define 10-fold CV error
  error <- rep(0,n)
  #Define the predicted values.
  y.radial <- rep(0,n) 
  #Define random indices.
  random.ind <- sample(n)
  for (i in 1:10){
    #Divide the entire data set as training and test by indices
    ind <- random.ind[(i*n/10-n/10+1):(i*n/10)]
    #Get SVM model
    mod.radial =svm(y~.,data=data[-ind,],kernel="radial",cost =l,gamma=g,scale=T)
    #Predict
    y.radial <- predict(mod.radial,data[ind,],scale=T)
    #Compute the test error.
    error[which((data$y[ind] == y.radial)==F)] <- 1
    print(i)
  }
  y.radial <- as.numeric(y.radial)-1
  #Compute the test error.
  error[which((data$y == y.radial)==F)] <- 1
  #Print the table
  print(table(predict=y.radial , truth = data$y))
  return(error)
}

#10-fold CV for random forests
#data is the stock data.
#m is the number of predictors considered in each split.
#b is the number of trees.
cv.rf <- function(data,m,b){
  #Get the number of observations
  n <- nrow(data)
  #Define 10-fold CV error
  error <- rep(0,n)
  #Define random indices.
  random.ind <- sample(n)
  for (i in 1:10){
    #Divide the entire data set as training and test by indices
    ind <- random.ind[(i*n/10-n/10+1):(i*n/10)]
    #Get random forest model
    mod.rf = randomForest(y~., data=data[-ind,], mtry =m,ntree=b)
    #Predict
    y.rf <- predict(mod.rf,data[ind,])
    #Compute the test error.
    error[which((data$y[ind] == y.rf)==F)] <- 1
    print(i)
  }
  return(error)
}

#Get prediction accuracy for 3000 obvservations with 10 fold.
#Down case
#Simple forests
error.rf.s <- cv.rf(dd[,-c(75,79)],10,50)
#Complex forests
error.rf.c <- cv.rf(dd[,-c(75,79)],20,200)
#SVM linear kernel
error.linear <- cv.linear(dd[,-c(75,79)],5)
#SVM cubic kernel
error.cubic <- cv.cubic(dd[,-c(75,79)],10)
#SVM radial kernel
error.radial <- cv.radial(dd[,-c(75,79)],2,5)

#Define a function calculating 10-fold CV sd for test error.
error.sd <- function(error){
  #Get the length.
  n <- length(error)
  error.var <- 0
  for (i in 1:10){
    error.var <- error.var+var(error[(i*n/10-n/10+1):(i*n/10)])
  }
  return(sqrt(error.var/(n-1)))
}
error.down <- c(sum(error.rf.s),sum(error.rf.c),sum(error.linear),
              sum(error.cubic),sum(error.radial))/3000
error.down.sd <- c(error.sd(error.rf.s),error.sd(error.rf.c),
                 error.sd(error.linear),error.sd(error.cubic),
                 error.sd(error.radial))
#Get time costs.
time.rf.s <- system.time(cv.rf(dd[,-c(75,79)],10,50))[1]
time.rf.c <- system.time(cv.rf(dd[,-c(75,79)],20,200))[1]
time.linear <- system.time(cv.linear(dd[,-c(75,79)],5))[1]
time.cubic <- system.time(cv.cubic(dd[,-c(75,79)],10))[1]
time.radial <- system.time(cv.radial(dd[,-c(75,79)],2,5))[1]
time.down <- c(time.rf.s,time.rf.c,time.linear,time.cubic,time.radial)
#plot
label <- 1:5
df.down <- data.frame(label,error.down,error.down.sd)
ggplot(df.down,aes(x=label,y=error.down)) + 
  geom_errorbar(aes(ymin=error.down-error.down.sd,ymax=error.down+error.down.sd),width=.1) +
  geom_line() +
  geom_point()

#Test the same thing for "Up case".
#Get prediction accuracy for 3000 obvservations with 10 fold.
#Simple forests
error.rf.s <- cv.rf(tsco[,-c(75,76,77,79)],10,50)
#Complex forests
error.rf.c <- cv.rf(tsco[,-c(75,76,77,79)],20,100)
#SVM linear kernel
error.linear <- cv.linear(tsco[,-c(75,76,77,79)],5)
#SVM cubic kernel
error.cubic <- cv.cubic(tsco[,-c(75,76,77,79)],10)
#SVM radial kernel
error.radial <- cv.radial(tsco[,-c(75,76,77,79)],2,5)
error.up <- c(sum(error.rf.s),sum(error.rf.c),sum(error.linear),
              sum(error.cubic),sum(error.radial))/3000
error.up.sd <- c(error.sd(error.rf.s),error.sd(error.rf.c),
                 error.sd(error.linear),error.sd(error.cubic),
                 error.sd(error.radial))
#Get time costs.
time.rf.s <- system.time(cv.rf(tsco[,-c(75,76,77,79)],10,50))[1]
time.rf.c <- system.time(cv.rf(tsco[,-c(75,76,77,79)],20,100))[1]
time.linear <- system.time(cv.linear(tsco[,-c(75,76,77,79)],5))[1]
time.cubic <- system.time(cv.cubic(tsco[,-c(75,76,77,79)],10))[1]
time.radial <- system.time(cv.radial(tsco[,-c(75,76,77,79)],2,5))[1]
time.up <- c(time.rf.s,time.rf.c,time.linear,time.cubic,time.radial)
#plot
label <- 1:5
df.up <- data.frame(label,error.up,error.up.sd)
ggplot(df.up,aes(x=label,y=error.up)) + 
  geom_errorbar(aes(ymin=error.up-error.up.sd,ymax=error.up+error.up.sd),width=.1) +
  geom_line() +
  geom_point()

#Test the same thing for "Mix case".
#Get prediction accuracy for 3000 obvservations with 10 fold.
#Simple forests
error.rf.s <- cv.rf(xom[,-c(76,77)],10,50)
#Complex forests
error.rf.c <- cv.rf(xom[,-c(76,77)],20,100)
#SVM linear kernel
error.linear <- cv.linear(xom[,-c(76,77)],5)
#SVM cubic kernel
error.cubic <- cv.cubic(xom[,-c(76,77)],10)
#SVM radial kernel
error.radial <- cv.radial(xom[,-c(76,77)],2,5)
error.mix <- c(sum(error.rf.s),sum(error.rf.c),sum(error.linear),
              sum(error.cubic),sum(error.radial))/3000
error.mix.sd <- c(error.sd(error.rf.s),error.sd(error.rf.c),
                 error.sd(error.linear),error.sd(error.cubic),
                 error.sd(error.radial))
#Get time costs.
time.rf.s <- system.time(cv.rf(xom[,-c(75,76,77,79)],10,50))[1]
time.rf.c <- system.time(cv.rf(xom[,-c(75,76,77,79)],20,100))[1]
time.linear <- system.time(cv.linear(xom[,-c(75,76,77,79)],5))[1]
time.cubic <- system.time(cv.cubic(xom[,-c(75,76,77,79)],10))[1]
time.radial <- system.time(cv.radial(xom[,-c(75,76,77,79)],2,5))[1]
time.mix <- c(time.rf.s,time.rf.c,time.linear,time.cubic,time.radial)
#plot
plot(c(time.rf.s,time.rf.c,time.linear,time.cubic,time.radial),
     ylab="seconds",main="time cost for each method")
label <- 1:5
df.mix <- data.frame(label,error.mix,error.mix.sd)
ggplot(df.mix,aes(x=label,y=error.mix)) + 
  geom_errorbar(aes(ymin=error.mix-error.mix.sd,ymax=error.mix+error.mix.sd),width=.1) +
  geom_line() +
  geom_point()
#plot the time cost.
plot.ts(time.mix,col="red",ylim=c(5,70),ylab="seconds",main="time costs for each method")
lines(time.down,col="blue")
lines(time.up,col="green")
legend(1,70,c("up","down","statinary"), lty=c(1,1,1), col=c("green","blue","red")) 

#3

#Discover feature importance from random forests.
data = tsco
mod.rf <- randomForest(y~., data, mtry=20, ntree=200,importance = TRUE)
varImpPlot(mod.rf)
#Repeat for 100 times.
importance.up <- rep(0,82)
importance.down <- rep(0,82)
importance.mix <- rep(0,82)
for (i in 1:100){
  mod.rf.up <- randomForest(y~., tsco, mtry=10, ntree=50,importance = TRUE)
  mod.rf.down <- randomForest(y~., dd, mtry=10, ntree=50,importance = TRUE)
  mod.rf.mix <- randomForest(y~., xom, mtry=10, ntree=50,importance = TRUE)
  importance.up <- importance(mod.rf.up)[,4] + importance.up
  importance.down <- importance(mod.rf.down)[,4] + importance.down
  importance.mix <- importance(mod.rf.mix)[,4] + importance.mix
  print(i)
}
#Get contributions by different feature sets
#Basic sets.
importance.basic <- c(sum(importance.up[1:20])/sum(importance.up),
                      sum(importance.down[1:20])/sum(importance.down),
                      sum(importance.mix[1:20])/sum(importance.mix))
#Time-insensitive Sets.
importance.ins <- c(sum(importance.up[21:46])/sum(importance.up),
                      sum(importance.down[21:46])/sum(importance.down),
                      sum(importance.mix[21:46])/sum(importance.mix))
#Time-sensitive Sets.
importance.sen <- c(sum(importance.up[47:82])/sum(importance.up),
                    sum(importance.down[47:82])/sum(importance.down),
                    sum(importance.mix[47:82])/sum(importance.mix))
#plot
plot.ts(importance.basic,col="red",type="o",ylim=c(0,1.2),ylab="percentage",main="contribution of different feature sets")
lines(importance.ins,col="blue",type="o", pch=22)
lines(importance.sen,col="green",type="o", pch=23)
legend(1,1.2,c("basic","insensitive","sensitive"), lty=c(1,1,1), col=c("red","green","blue")) 

#Get most significant features.
sort(importance.up,decreasing = TRUE)[1:10]
sort(importance.down,decreasing = TRUE)[1:10]
sort(importance.mix,decreasing = TRUE)[1:10]

#Discover feature importance from lasso.
#up case
x= model.matrix(y~., tsco)[,-1]
y= tsco$y
#Fit lasso.
mod.up <- glmnet(x, y, alpha =1,family="binomial")
#Get estimated coefficients.
sort(coef(mod.up)[,60])

#down case
x= model.matrix(y~., dd)[,-1]
y= dd$y
#Fit lasso.
mod.up <- glmnet(x, y, alpha =1,family="binomial")
#Get estimated coefficients.
sort(coef(mod.up)[,60],decreasing=T)

#4

#Test for robustness.
#Up case.
data <- tsco[,-c(74,75,76,77,78,79)]
#Divide the data as 5 subset in terms of time period.
#Fit ramdon forests and svm to the first 1/5 data and predict the rest.
#Repeat for 10 times.
n <- nrow(data)/6

#Define errors.
error1.rf <- rep(0,10)
error2.rf <- rep(0,10)
error3.rf <- rep(0,10)
error4.rf <- rep(0,10)
error1.svm <- rep(0,10)
error2.svm <- rep(0,10)
error3.svm <- rep(0,10)
error4.svm <- rep(0,10)
importance.rf <- rep(0,10)
for (i in (1:10)){
  #Fit ramdon forests model.
  mod.rf = randomForest(y~.,data=data[(n*i/10-n/10+1):(n+i*50-50),],mtry =10,ntree=200)
  #Predict
  y.rf <- predict(mod.rf,data[(n+i*50-50+1):(n+i*50-50+2000),])
  error.rf <- y.rf != data$y[(n+i*50-50+1):(n+i*50-50+2000)]
  error1.rf[i] <- sum(error.rf[1:500])/500
  error2.rf[i] <- sum(error.rf[501:1000])/500
  error3.rf[i] <- sum(error.rf[1001:1500])/500
  error4.rf[i] <- sum(error.rf[1501:2000])/500
  #Fit SVM.
  mod.svm = svm(y~.,data=data[(n*i/10-n/10+1):(n+i*50-50),],kernel="polynomial",cost=5,scale=T)
  #Predict
  y.svm <- predict(mod.svm,data[(n+i*50-50+1):(n+i*50-50+2000),])
  error.svm <- y.svm != data$y[(n+i*50-50+1):(n+i*50-50+2000)]
  error1.svm[i] <- sum(error.svm[1:500])/500
  error2.svm[i] <- sum(error.svm[501:1000])/500
  error3.svm[i] <- sum(error.svm[1001:1500])/500
  error4.svm[i] <- sum(error.svm[1501:2000])/500
}
#Get mean and sd.
error.rf.mean <- c(mean(error1.rf),mean(error2.rf),mean(error3.rf),mean(error4.rf))
error.rf.sd <- c(sd(error1.rf),sd(error2.rf),sd(error3.rf),sd(error4.rf))
error.svm.mean <- c(mean(error1.svm),mean(error2.svm),mean(error3.svm),mean(error4.svm))
error.svm.sd <- c(sd(error1.svm),sd(error2.svm),sd(error3.svm),sd(error4.svm))
#plot for random forests.
label <- 1:4
df.up <- data.frame(label,error.rf.mean,error.rf.sd)
ggplot(df.up,aes(x=label,y=error.rf.mean)) + 
  geom_errorbar(aes(ymin=error.rf.mean-error.rf.sd,ymax=error.rf.mean+error.rf.sd),width=.1) +
  geom_line() +
  geom_point()
#plot for SVM.
label <- 1:4
df.up <- data.frame(label,error.svm.mean,error.svm.sd)
ggplot(df.up,aes(x=label,y=error.svm.mean)) + 
  geom_errorbar(aes(ymin=error.svm.mean-error.svm.sd,ymax=error.svm.mean+error.svm.sd),width=.1) +
  geom_line() +
  geom_point()

#Test feature importance.
n <- nrow(data)
for (i in c(1,3,8)){
  #Fit ramdon forests model.
  mod.rf = randomForest(y~.,data=data[(n*i/10-n/10+1):(n/10*i),],mtry =10,ntree=200,importance=T)
  #Get feature importance. 
  importance.rf <- importance(mod.rf)[,4] 
  #Print the top 5.
  print(sort(importance.rf,decreasing=T)[1:5])
}
  
#Repeat for down case.
#Up case.
data <- dd[601:3000,]
#Divide the data as 5 subset in terms of time period.
#Fit ramdon forests and svm to the first 1/5 data and predict the rest.
#Repeat for 10 times.
n <- nrow(data)/6

#Define errors.
error1.rf <- rep(0,10)
error2.rf <- rep(0,10)
error3.rf <- rep(0,10)
error4.rf <- rep(0,10)
error1.svm <- rep(0,10)
error2.svm <- rep(0,10)
error3.svm <- rep(0,10)
error4.svm <- rep(0,10)

for (i in (1:10)){
  #Fit ramdon forests model.
  mod.rf = randomForest(y~.,data=data[(n*i/10-n/10+1):(n+i*40-40),],mtry =10,ntree=200)
  #Predict
  y.rf <- predict(mod.rf,data[(n+i*40-40+1):(n+i*40-40+1600),])
  error.rf <- y.rf != data$y[(n+i*40-40+1):(n+i*40-40+1600)]
  error1.rf[i] <- sum(error.rf[1:400])/400
  error2.rf[i] <- sum(error.rf[401:800])/400
  error3.rf[i] <- sum(error.rf[801:1200])/400
  error4.rf[i] <- sum(error.rf[1201:1600])/400
  #Fit SVM.
  mod.svm = svm(y~.,data=data[(n*i/10-n/10+1):(n+i*40-40),-c(20,35,39,75,76,77,79)],kernel="polynomial",cost=5)
  #Predict
  y.svm <- predict(mod.svm,data[(n+i*40-40+1):(n+i*40-40+1600),])
  error.svm <- y.svm != data$y[(n+i*40-40+1):(n+i*40-40+1600)]
  error1.svm[i] <- sum(error.svm[1:400])/400
  error2.svm[i] <- sum(error.svm[401:800])/400
  error3.svm[i] <- sum(error.svm[801:1200])/400
  error4.svm[i] <- sum(error.svm[1201:1600])/400
}
#Get mean and sd.
error.rf.mean <- c(mean(error1.rf),mean(error2.rf),mean(error3.rf),mean(error4.rf))
error.rf.sd <- c(sd(error1.rf),sd(error2.rf),sd(error3.rf),sd(error4.rf))
error.svm.mean <- c(mean(error1.svm),mean(error2.svm),mean(error3.svm),mean(error4.svm))
error.svm.sd <- c(sd(error1.svm),sd(error2.svm),sd(error3.svm),sd(error4.svm))
#plot for random forests.
label <- 1:4
df.up <- data.frame(label,error.rf.mean,error.rf.sd)
ggplot(df.up,aes(x=label,y=error.rf.mean)) + 
  geom_errorbar(aes(ymin=error.rf.mean-error.rf.sd,ymax=error.rf.mean+error.rf.sd),width=.1) +
  geom_line() +
  geom_point()
#plot for SVM.
label <- 1:4
df.up <- data.frame(label,error.svm.mean,error.svm.sd)
ggplot(df.up,aes(x=label,y=error.svm.mean)) + 
  geom_errorbar(aes(ymin=error.svm.mean-error.svm.sd,ymax=error.svm.mean+error.svm.sd),width=.1) +
  geom_line() +
  geom_point()

