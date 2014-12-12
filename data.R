#Final Project Script 2: extract data from 8 different stocks

DD <- read.csv("DD_050610_14%3A42_14%3A50_5.csv",header=T)
EXP <- read.csv("EXP_042111_07%253A00_15%253A55_5.csv",header=T)
HMSY <- read.csv("HMSY_042111_07%253A00_15%253A55_5.csv",header=T)
IRBT <- read.csv("IRBT_042111_07%253A00_15%253A55_5.csv",header=T)
SWI <- read.csv("SWI_042111_07%253A00_15%253A55_5.csv",header=T)
TSCO <- read.csv("TSCO_042111_07%253A00_15%253A55_5.csv",header=T)
HLX <- read.csv("HLX_042111_07%253A00_15%253A55_5.csv",header=T)
XOM <- read.csv("XOM_042111_07%253A00_15%253A55_5.csv",header=T)

plot(TSCO$LastTransactedPrice[100000:103000])
plot(XOM$LastTransactedPrice[100000:103000])
plot(DD$LastTransactedPrice[32000:35000])

#Down data
data.DD <- get_attributes(DD,a=32000,k=3000,t=300,p=0.5,bigT=100,smallT=10,s=5)
#Up data
data.TSCO <- get_attributes(TSCO[1:150000,],a=100000,k=3000,t=300,p=0.5,bigT=100,smallT=10,s=5)
#mixture
data.XOM <- get_attributes(XOM[1:150000,],a=100000,k=3000,t=300,p=0.5,bigT=100,smallT=10,s=5)

#save data.
write.csv(data.DD, file = "data.DD.csv")
write.csv(data.TSCO, file = "data.TSCO.csv")
write.csv(data.XOM, file = "data.XOM.csv")

