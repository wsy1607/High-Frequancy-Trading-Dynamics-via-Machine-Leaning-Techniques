#Final Project Script 1: data attributes

#In this script, a function has been defined to extract features in LOB.
#Those features (attributes) will be applied in the analysis later.

#test for one stock
DD <- read.csv("DD_050610_14%3A42_14%3A50_5.csv",header=T)
names(DD)
stock <- DD
a <- 140000
k <- 100
t <- 300
p <- 0.5
bigT <- 100
smallT <- 10
data <- stock[(a+1):(a+k),]
n <- nrow(stock)
#Define "Basic Set" with level up to 5.

#V1 
#Ask price
price_ask1 <- data$Ask_Price1
price_ask2 <- data$Ask_Price2
price_ask3 <- data$Ask_Price3
price_ask4 <- data$Ask_Price4
price_ask5 <- data$Ask_Price5
#Bid price
price_bid1 <- data$Bid_Price1
price_bid2 <- data$Bid_Price2
price_bid3 <- data$Bid_Price3
price_bid4 <- data$Bid_Price4
price_bid5 <- data$Bid_Price5
#Ask volume
volume_ask1 <- data$Ask_Shares1
volume_ask2 <- data$Ask_Shares2
volume_ask3 <- data$Ask_Shares3
volume_ask4 <- data$Ask_Shares4
volume_ask5 <- data$Ask_Shares5
#Bid volume
volume_bid1 <- data$Bid_Shares1
volume_bid2 <- data$Bid_Shares2
volume_bid3 <- data$Bid_Shares3
volume_bid4 <- data$Bid_Shares4
volume_bid5 <- data$Bid_Shares5
#Get v1.
v1 <- data.frame(price_ask1,price_ask2,price_ask3,price_ask4,price_ask5,
                 volume_ask1,volume_ask2,volume_ask3,volume_ask4,volume_ask5,
                 price_bid1,price_bid2,price_bid3,price_bid4,price_bid5,
                 volume_bid1,volume_bid2,volume_bid3,volume_bid4,volume_bid5)

#Define "Time-insensitive Set" with level up to 5.

#v2
#bid-ask spreads
spread1 <- data$Ask_Price1-data$Bid_Price1
spread2 <- data$Ask_Price2-data$Bid_Price2
spread3 <- data$Ask_Price3-data$Bid_Price3
spread4 <- data$Ask_Price4-data$Bid_Price4
spread5 <- data$Ask_Price5-data$Bid_Price5
#mid-prices
mid_price1 <- (data$Ask_Price1+data$Bid_Price1)/2
mid_price2 <- (data$Ask_Price2+data$Bid_Price2)/2
mid_price3 <- (data$Ask_Price3+data$Bid_Price3)/2
mid_price4 <- (data$Ask_Price4+data$Bid_Price4)/2
mid_price5 <- (data$Ask_Price5+data$Bid_Price5)/2
#Get v2.
v2 <- data.frame(spread1,spread2,spread3,spread4,spread5,
                 mid_price1,mid_price2,mid_price3,mid_price4,mid_price5)

#v3
#price differences
#ask
price_diff_ask1 <- data$Ask_Price5-data$Ask_Price1
price_diff_ask2 <- data$Ask_Price2-data$Ask_Price1
price_diff_ask3 <- data$Ask_Price3-data$Ask_Price2
price_diff_ask4 <- data$Ask_Price4-data$Ask_Price3
price_diff_ask5 <- data$Ask_Price5-data$Ask_Price4
#bid
price_diff_bid1 <- data$Bid_Price1-data$Bid_Price5
price_diff_bid2 <- data$Bid_Price1-data$Bid_Price2
price_diff_bid3 <- data$Bid_Price2-data$Bid_Price3
price_diff_bid4 <- data$Bid_Price3-data$Bid_Price4
price_diff_bid5 <- data$Bid_Price4-data$Bid_Price5
#Get v3.
v3 <- data.frame(price_diff_ask1,price_diff_ask2,price_diff_ask3,price_diff_ask4,price_diff_ask5,
                 price_diff_bid1,price_diff_bid2,price_diff_bid3,price_diff_bid4,price_diff_bid5)

#v4
#mean prices
price_mean_ask <- (data$Ask_Price1+data$Ask_Price2+data$Ask_Price3+
                     data$Ask_Price4+data$Ask_Price5)/5
price_mean_bid <- (data$Bid_Price1+data$Bid_Price2+data$Bid_Price3+
                     data$Bid_Price4+data$Bid_Price5)/5
#mean volumes
volume_mean_ask <- (data$Ask_Shares1+data$Ask_Shares2+data$Ask_Shares3+
                      data$Ask_Shares4+data$Ask_Shares5)/5
volume_mean_bid <- (data$Bid_Shares1+data$Bid_Shares2+data$Bid_Shares3+
                      data$Bid_Shares4+data$Bid_Shares5)/5
#Get v4.
v4 <- data.frame(price_mean_ask,price_mean_bid,volume_mean_ask,volume_mean_bid)

#v5
#accumulated differences
#price
price_accum_diff <- data$Ask_Price1+data$Ask_Price2+data$Ask_Price3+
  data$Ask_Price4+data$Ask_Price5-data$Bid_Price1-
  data$Bid_Price2-data$Bid_Price3-data$Bid_Price4-
  data$Bid_Price5
volume_accum_diff <- data$Ask_Shares1+data$Ask_Shares2+data$Ask_Shares3+
  data$Ask_Shares4+data$Ask_Shares5-data$Bid_Shares1-
  data$Bid_Shares2-data$Bid_Shares3-data$Bid_Shares4-
  data$Bid_Shares5
#Get v5
v5 <- data.frame(price_accum_diff,volume_accum_diff)

#Define "Time-insensitive Set" with level up to 5.

#v6
#price and volume derivatives
price_deri_ask1 <- stock$Ask_Price1[(a):(a+k-1)]-stock$Ask_Price1[(a+1-t):(a+k-t)]
price_deri_ask2 <- stock$Ask_Price2[(a):(a+k-1)]-stock$Ask_Price2[(a+1-t):(a+k-t)]
price_deri_ask3 <- stock$Ask_Price3[(a):(a+k-1)]-stock$Ask_Price3[(a+1-t):(a+k-t)]
price_deri_ask4 <- stock$Ask_Price4[(a):(a+k-1)]-stock$Ask_Price4[(a+1-t):(a+k-t)]
price_deri_ask5 <- stock$Ask_Price5[(a):(a+k-1)]-stock$Ask_Price5[(a+1-t):(a+k-t)]
price_deri_bid1 <- stock$Bid_Price1[(a):(a+k-1)]-stock$Bid_Price1[(a+1-t):(a+k-t)]
price_deri_bid2 <- stock$Bid_Price2[(a):(a+k-1)]-stock$Bid_Price2[(a+1-t):(a+k-t)]
price_deri_bid3 <- stock$Bid_Price3[(a):(a+k-1)]-stock$Bid_Price3[(a+1-t):(a+k-t)]
price_deri_bid4 <- stock$Bid_Price4[(a):(a+k-1)]-stock$Bid_Price4[(a+1-t):(a+k-t)]
price_deri_bid5 <- stock$Bid_Price5[(a):(a+k-1)]-stock$Bid_Price5[(a+1-t):(a+k-t)]
volume_deri_ask1 <- stock$Ask_Shares1[(a):(a+k-1)]-stock$Ask_Shares1[(a+1-t):(a+k-t)]
volume_deri_ask2 <- stock$Ask_Shares2[(a):(a+k-1)]-stock$Ask_Shares2[(a+1-t):(a+k-t)]
volume_deri_ask3 <- stock$Ask_Shares3[(a):(a+k-1)]-stock$Ask_Shares3[(a+1-t):(a+k-t)]
volume_deri_ask4 <- stock$Ask_Shares4[(a):(a+k-1)]-stock$Ask_Shares4[(a+1-t):(a+k-t)]
volume_deri_ask5 <- stock$Ask_Shares5[(a):(a+k-1)]-stock$Ask_Shares5[(a+1-t):(a+k-t)]
volume_deri_bid1 <- stock$Bid_Shares1[(a):(a+k-1)]-stock$Bid_Shares1[(a+1-t):(a+k-t)]
volume_deri_bid2 <- stock$Bid_Shares2[(a):(a+k-1)]-stock$Bid_Shares2[(a+1-t):(a+k-t)]
volume_deri_bid3 <- stock$Bid_Shares3[(a):(a+k-1)]-stock$Bid_Shares3[(a+1-t):(a+k-t)]
volume_deri_bid4 <- stock$Bid_Shares4[(a):(a+k-1)]-stock$Bid_Shares4[(a+1-t):(a+k-t)]
volume_deri_bid5 <- stock$Bid_Shares5[(a):(a+k-1)]-stock$Bid_Shares5[(a+1-t):(a+k-t)]
#Get v6.
v6 <- data.frame(price_deri_ask1,price_deri_ask2,price_deri_ask3,price_deri_ask4,price_deri_ask5,
                 volume_deri_ask1,volume_deri_ask2,volume_deri_ask3,volume_deri_ask4,volume_deri_ask5,
                 price_deri_bid1,price_deri_bid2,price_deri_bid3,price_deri_bid4,price_deri_bid5,
                 volume_deri_bid1,volume_deri_bid2,volume_deri_bid3,volume_deri_bid4,volume_deri_bid5)

#v7, v8 and v9
#average intensity of each type
lambda_l_ask <- rep(0,k)
lambda_l_bid <- rep(0,k)
lambda_m_ask <- rep(0,k)
lambda_m_bid <- rep(0,k)
lambda_c_ask <- rep(0,k)
lambda_c_bid <- rep(0,k)
#relative intensity indicators
lambda_l_askT <- rep(0,k)
lambda_l_bidT <- rep(0,k)
lambda_m_askT <- rep(0,k)
lambda_m_bidT <- rep(0,k)
lambda_c_askT <- rep(0,k)
lambda_c_bidT <- rep(0,k)
lambda_l_askt <- rep(0,k)
lambda_l_bidt <- rep(0,k)
lambda_m_askt <- rep(0,k)
lambda_m_bidt <- rep(0,k)
lambda_c_askt <- rep(0,k)
lambda_c_bidt <- rep(0,k)
ind_l_ask <- rep(0,k)
ind_l_bid <- rep(0,k)
ind_m_ask <- rep(0,k)
ind_m_bid <- rep(0,k)
ind_c_ask <- rep(0,k)
ind_c_bid <- rep(0,k)
#accelerations
lambda_deri_l_a <- rep(0,k)
lambda_deri_l_b <- rep(0,k)
lambda_deri_m_a <- rep(0,k)
lambda_deri_m_b <- rep(0,k)
for (i in 1:k){
  ask1 <- diff(stock$Ask_Num_Order1)[(i+a-t-1):(i+a-1)]
  ask2 <- diff(stock$Ask_Num_Order2)[(i+a-t-1):(i+a-1)]
  ask3 <- diff(stock$Ask_Num_Order3)[(i+a-t-1):(i+a-1)]
  ask4 <- diff(stock$Ask_Num_Order4)[(i+a-t-1):(i+a-1)]
  ask5 <- diff(stock$Ask_Num_Order5)[(i+a-t-1):(i+a-1)]
  lambda_l_ask[i] <- sum(ask1[ask1>0])+sum(ask2[ask2>0])+sum(ask3[ask3>0])+
    sum(ask4[ask4>0])+sum(ask5[ask5>0])
  bid1 <- diff(stock$Bid_Num_Order1)[(i+a-t-1):(i+a-1)]
  bid2 <- diff(stock$Bid_Num_Order2)[(i+a-t-1):(i+a-1)]
  bid3 <- diff(stock$Bid_Num_Order3)[(i+a-t-1):(i+a-1)]
  bid4 <- diff(stock$Bid_Num_Order4)[(i+a-t-1):(i+a-1)]
  bid5 <- diff(stock$Bid_Num_Order5)[(i+a-t-1):(i+a-1)]
  lambda_l_bid[i] <- sum(bid1[bid1>0])+sum(bid2[bid2>0])+sum(bid3[bid3>0])+
    sum(bid4[bid4>0])+sum(bid5[bid5>0])
  cask1 <- diff(stock$Ask_Num_Order1)[(i+a-t-1):(i+a-1)]
  cask2 <- diff(stock$Ask_Num_Order2)[(i+a-t-1):(i+a-1)]
  cask3 <- diff(stock$Ask_Num_Order3)[(i+a-t-1):(i+a-1)]
  cask4 <- diff(stock$Ask_Num_Order4)[(i+a-t-1):(i+a-1)]
  cask5 <- diff(stock$Ask_Num_Order5)[(i+a-t-1):(i+a-1)]
  lambda_c_ask[i] <- sum(cask1[cask1<0])+sum(cask2[cask2<0])+sum(cask3[cask3<0])+
    sum(cask4[cask4<0])+sum(cask5[cask5<0])
  cbid1 <- diff(stock$Bid_Num_Order1)[(i+a-t-1):(i+a-1)]
  cbid2 <- diff(stock$Bid_Num_Order2)[(i+a-t-1):(i+a-1)]
  cbid3 <- diff(stock$Bid_Num_Order3)[(i+a-t-1):(i+a-1)]
  cbid4 <- diff(stock$Bid_Num_Order4)[(i+a-t-1):(i+a-1)]
  cbid5 <- diff(stock$Bid_Num_Order5)[(i+a-t-1):(i+a-1)]
  lambda_c_bid[i] <- sum(cbid1[cbid1<0])+sum(cbid2[cbid2<0])+sum(cbid3[cbid3<0])+
    sum(cbid4[cbid4<0])+sum(cbid5[cbid5<0])
  lambda_m_ask[i] <- sum(cask1[cask1<0])*p
  lambda_m_bid[i] <- sum(cbid1[cbid1<0])*p
  ask1 <- diff(stock$Ask_Num_Order1)[(i+a-t*bigT-1):(i+a-1)]
  ask2 <- diff(stock$Ask_Num_Order2)[(i+a-t*bigT-1):(i+a-1)]
  ask3 <- diff(stock$Ask_Num_Order3)[(i+a-t*bigT-1):(i+a-1)]
  ask4 <- diff(stock$Ask_Num_Order4)[(i+a-t*bigT-1):(i+a-1)]
  ask5 <- diff(stock$Ask_Num_Order5)[(i+a-t*bigT-1):(i+a-1)]
  lambda_l_askT[i] <- sum(ask1[ask1>0])+sum(ask2[ask2>0])+sum(ask3[ask3>0])+
    sum(ask4[ask4>0])+sum(ask5[ask5>0])
  bid1 <- diff(stock$Bid_Num_Order1)[(i+a-t*bigT-1):(i+a-1)]
  bid2 <- diff(stock$Bid_Num_Order2)[(i+a-t*bigT-1):(i+a-1)]
  bid3 <- diff(stock$Bid_Num_Order3)[(i+a-t*bigT-1):(i+a-1)]
  bid4 <- diff(stock$Bid_Num_Order4)[(i+a-t*bigT-1):(i+a-1)]
  bid5 <- diff(stock$Bid_Num_Order5)[(i+a-t*bigT-1):(i+a-1)]
  lambda_l_bidT[i] <- sum(bid1[bid1>0])+sum(bid2[bid2>0])+sum(bid3[bid3>0])+
    sum(bid4[bid4>0])+sum(bid5[bid5>0])
  cask1 <- diff(stock$Ask_Num_Order1)[(i+a-t*bigT-1):(i+a-1)]
  cask2 <- diff(stock$Ask_Num_Order2)[(i+a-t*bigT-1):(i+a-1)]
  cask3 <- diff(stock$Ask_Num_Order3)[(i+a-t*bigT-1):(i+a-1)]
  cask4 <- diff(stock$Ask_Num_Order4)[(i+a-t*bigT-1):(i+a-1)]
  cask5 <- diff(stock$Ask_Num_Order5)[(i+a-t*bigT-1):(i+a-1)]
  lambda_c_askT[i] <- sum(cask1[cask1<0])+sum(cask2[cask2<0])+sum(cask3[cask3<0])+
    sum(cask4[cask4<0])+sum(cask5[cask5<0])
  cbid1 <- diff(stock$Bid_Num_Order1)[(i+a-t*bigT-1):(i+a-1)]
  cbid2 <- diff(stock$Bid_Num_Order2)[(i+a-t*bigT-1):(i+a-1)]
  cbid3 <- diff(stock$Bid_Num_Order3)[(i+a-t*bigT-1):(i+a-1)]
  cbid4 <- diff(stock$Bid_Num_Order4)[(i+a-t*bigT-1):(i+a-1)]
  cbid5 <- diff(stock$Bid_Num_Order5)[(i+a-t*bigT-1):(i+a-1)]
  lambda_c_bidT[i] <- sum(cbid1[cbid1<0])+sum(cbid2[cbid2<0])+sum(cbid3[cbid3<0])+
    sum(cbid4[cbid4<0])+sum(cbid5[cbid5<0])
  lambda_m_askT[i] <- sum(cask1[cask1<0])*p
  lambda_m_bidT[i] <- sum(cbid1[cbid1<0])*p
  ask1 <- diff(stock$Ask_Num_Order1)[(i+a-t*smallT-1):(i+a-1)]
  ask2 <- diff(stock$Ask_Num_Order2)[(i+a-t*smallT-1):(i+a-1)]
  ask3 <- diff(stock$Ask_Num_Order3)[(i+a-t*smallT-1):(i+a-1)]
  ask4 <- diff(stock$Ask_Num_Order4)[(i+a-t*smallT-1):(i+a-1)]
  ask5 <- diff(stock$Ask_Num_Order5)[(i+a-t*smallT-1):(i+a-1)]
  lambda_l_askt[i] <- sum(ask1[ask1>0])+sum(ask2[ask2>0])+sum(ask3[ask3>0])+
    sum(ask4[ask4>0])+sum(ask5[ask5>0])
  bid1 <- diff(stock$Bid_Num_Order1)[(i+a-t*smallT-1):(i+a-1)]
  bid2 <- diff(stock$Bid_Num_Order2)[(i+a-t*smallT-1):(i+a-1)]
  bid3 <- diff(stock$Bid_Num_Order3)[(i+a-t*smallT-1):(i+a-1)]
  bid4 <- diff(stock$Bid_Num_Order4)[(i+a-t*smallT-1):(i+a-1)]
  bid5 <- diff(stock$Bid_Num_Order5)[(i+a-t*smallT-1):(i+a-1)]
  lambda_l_bidt[i] <- sum(bid1[bid1>0])+sum(bid2[bid2>0])+sum(bid3[bid3>0])+
    sum(bid4[bid4>0])+sum(bid5[bid5>0])
  cask1 <- diff(stock$Ask_Num_Order1)[(i+a-t*smallT-1):(i+a-1)]
  cask2 <- diff(stock$Ask_Num_Order2)[(i+a-t*smallT-1):(i+a-1)]
  cask3 <- diff(stock$Ask_Num_Order3)[(i+a-t*smallT-1):(i+a-1)]
  cask4 <- diff(stock$Ask_Num_Order4)[(i+a-t*smallT-1):(i+a-1)]
  cask5 <- diff(stock$Ask_Num_Order5)[(i+a-t*smallT-1):(i+a-1)]
  lambda_c_askt[i] <- sum(cask1[cask1<0])+sum(cask2[cask2<0])+sum(cask3[cask3<0])+
    sum(cask4[cask4<0])+sum(cask5[cask5<0])
  cbid1 <- diff(stock$Bid_Num_Order1)[(i+a-t*smallT-1):(i+a-1)]
  cbid2 <- diff(stock$Bid_Num_Order2)[(i+a-t*smallT-1):(i+a-1)]
  cbid3 <- diff(stock$Bid_Num_Order3)[(i+a-t*smallT-1):(i+a-1)]
  cbid4 <- diff(stock$Bid_Num_Order4)[(i+a-t*smallT-1):(i+a-1)]
  cbid5 <- diff(stock$Bid_Num_Order5)[(i+a-t*smallT-1):(i+a-1)]
  lambda_c_bidt[i] <- sum(cbid1[cbid1<0])+sum(cbid2[cbid2<0])+sum(cbid3[cbid3<0])+
    sum(cbid4[cbid4<0])+sum(cbid5[cbid5<0])
  lambda_m_askt[i] <- sum(cask1[cask1<0])*p
  lambda_m_bidt[i] <- sum(cbid1[cbid1<0])*p
  ask1 <- diff(stock$Ask_Num_Order1)[(i+a-2*t-1):(i+a-t-1)]
  ask2 <- diff(stock$Ask_Num_Order2)[(i+a-2*t-1):(i+a-t-1)]
  ask3 <- diff(stock$Ask_Num_Order3)[(i+a-2*t-1):(i+a-t-1)]
  ask4 <- diff(stock$Ask_Num_Order4)[(i+a-2*t-1):(i+a-t-1)]
  ask5 <- diff(stock$Ask_Num_Order5)[(i+a-2*t-1):(i+a-t-1)]
  lambda_deri_l_a[i] <- lambda_l_ask[i]-sum(ask1[ask1>0])-sum(ask2[ask2>0])-
    sum(ask3[ask3>0])-sum(ask4[ask4>0])-sum(ask5[ask5>0])
  lambda_deri_m_a[i] <- lambda_m_ask[i]-sum(ask1[ask1>0])*p
  bid1 <- diff(stock$Bid_Num_Order1)[(i+a-2*t-1):(i+a-t-1)]
  bid2 <- diff(stock$Bid_Num_Order2)[(i+a-2*t-1):(i+a-t-1)]
  bid3 <- diff(stock$Bid_Num_Order3)[(i+a-2*t-1):(i+a-t-1)]
  bid4 <- diff(stock$Bid_Num_Order4)[(i+a-2*t-1):(i+a-t-1)]
  bid5 <- diff(stock$Bid_Num_Order5)[(i+a-2*t-1):(i+a-t-1)]
  lambda_deri_l_b[i] <- lambda_l_bid[i]-sum(bid1[bid1>0])-sum(bid2[bid2>0])-
    sum(bid3[bid3>0])-sum(bid4[bid4>0])-sum(bid5[bid5>0])
  lambda_deri_m_b[i] <- lambda_m_bid[i]-sum(bid1[bid1>0])*p
  print(i)
}
#Get v7.
lambda_l_ask <- lambda_l_ask/k
lambda_l_bid <- lambda_l_bid/k
lambda_m_ask <- -lambda_m_ask/k
lambda_m_bid <- -lambda_m_bid/k
lambda_c_ask <- -lambda_c_ask/k
lambda_c_bid <- -lambda_c_bid/k
v7 <- data.frame(lambda_l_ask,lambda_l_bid,lambda_m_ask,
                 lambda_m_bid,lambda_c_ask,lambda_c_bid)
#Get v8.
ind_l_ask[which(lambda_l_askt/smallT>lambda_l_askT/bigT)] <- 1
ind_l_bid[which(lambda_l_bidt/smallT>lambda_l_bidT/bigT)] <- 1
ind_m_ask[which(lambda_m_askt/smallT>lambda_m_askT/bigT)] <- 1
ind_m_bid[which(lambda_m_bidt/smallT>lambda_m_bidT/bigT)] <- 1
ind_c_ask[which(lambda_c_askt/smallT>lambda_c_askT/bigT)] <- 1
ind_c_bid[which(lambda_c_bidt/smallT>lambda_c_bidT/bigT)] <- 1
v8 <- data.frame(ind_l_ask,ind_l_bid,ind_m_ask,
                 ind_m_bid,ind_c_ask,ind_c_bid)
#Get v9.
v9 <- data.frame(lambda_deri_m_a,lambda_deri_l_a,lambda_deri_m_b,lambda_deri_l_b)

#Get labels Y
#up: 2; down: 1; stationary: 0.
y <- rep(0,k)
y[which(stock$Bid_Price1[(a+k+1+5):(a+2*k+5)]>data$Ask_Price1)] <- 2
y[which(stock$Ask_Price1[(a+k+1+5):(a+2*k+5)]<data$Bid_Price1)] <- 1

#Call the function.
data.train <- get_attributes(DD,a=140000,k=100,t=300,p=0.5,bigT=100,smallT=10,s=5)





