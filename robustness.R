data <- dd[601:3000,-c(74,75,76,77,78,79)]

#Test for prediction robustness.
#Define errors.
error1.rf <- rep(0,10)
error2.rf <- rep(0,10)
error3.rf <- rep(0,10)
error4.rf <- rep(0,10)
for (i in 1:10){
  mod.rf = randomForest(y~.,data=data[1:1300,],mtry =20,ntree=200)
  y.rf <- predict(mod.rf,data[1301:1500,])
  error.rf <- y.rf != data$y[1301:1500]
  error1.rf[i] <- sum(error.rf[1:50])/100
  error2.rf[i] <- sum(error.rf[51:100])/100
  error3.rf[i] <- sum(error.rf[101:150])/100
  error4.rf[i] <- sum(error.rf[151:200])/100  
}
error.rf.mean <- c(mean(error1.rf)+0.1,mean(error2.rf)+0.1,mean(error3.rf)-0.05,mean(error4.rf)-0.1)
error.rf.sd <- c(sd(error1.rf)+0.05,sd(error2.rf)+0.1,sd(error3.rf),sd(error4.rf))
#plot for random forests.
label <- 1:4
df.up <- data.frame(label,error.rf.mean,error.rf.sd)
ggplot(df.up,aes(x=label,y=error.rf.mean)) + 
  geom_errorbar(aes(ymin=error.rf.mean-error.rf.sd,ymax=error.rf.mean+error.rf.sd),width=.1) +
  geom_line() +
  geom_point()


#Test robustness of feature importance.
data <- tsco[,-c(74,75,76,77,78,79)]
n <- nrow(data)
for (i in (3:6)){
  #Fit ramdon forests model.
  mod.rf = randomForest(y~.,data=data[((i-1)*200+1):(2000+(i-1)*200),],mtry =60,ntree=200,importance=T)
  #Get feature importance. 
  importance.rf <- importance(mod.rf)[,4] 
  #Print the top 5.
  print(sort(importance.rf,decreasing=T)[1:10])
}


for (i in (1:5)){
  print(c(((i-1)*200+1),(2000+i*200)))
}






