library(ggplot2)

dataall<-read.csv("rate.csv")
head(dataall,40)

FriSemi = dataall[dataall$TERM == 180&dataall$DAY == 5,]
FriYear = dataall[dataall$TERM == 365&dataall$DAY == 5,]

## for the semi data
n = nrow(FriSemi)

diff = abs(FriSemi$RATE[2:n]-FriSemi$RATE[1:(n-1)])
rate = FriSemi$RATE[1:(n-1)]

beta0=0
beta1=1

for(i in 1:1000){
  beta5 = (beta0+beta1)/2
  result0 = sum(n*diff^2*log(rate)*rate^(-2*beta0))/sum(diff^2*rate^(-2*beta0))-sum(log(rate))
  result5 = sum(n*diff^2*log(rate)*rate^(-2*beta5))/sum(diff^2*rate^(-2*beta5))-sum(log(rate))
  if(result0*result5 > 0)
    beta0 = beta5
  else
    beta1 = beta5
}

betasemi = beta5
sigmasemi = sum(diff^2/rate^betasemi/n)

# for the one year data
n = nrow(FriYear)

diff = abs(FriYear$RATE[2:n]-FriYear$RATE[1:(n-1)])
rate = FriYear$RATE[1:(n-1)]

beta0=0
beta1=1

for(i in 1:1000){
  beta5 = (beta0+beta1)/2
  result0 = sum(n*diff^2*log(rate)*rate^(-2*beta0))/sum(diff^2*rate^(-2*beta0))-sum(log(rate))
  result5 = sum(n*diff^2*log(rate)*rate^(-2*beta5))/sum(diff^2*rate^(-2*beta5))-sum(log(rate))
  if(result0*result5 > 0)
    beta0 = beta5
  else
    beta1 = beta5
}

betayear = beta5
sigmayear = sum(diff^2/rate^betayear/n)

# Calculate VaR for one year maturity bond
# with semi-anually 4% coupon 

rsemi = FriSemi$RATE[n]
ryear = FriYear$RATE[n]

FV = 2/(1+rsemi)^0.5+102/(1+ryear)

sen = 500

PandL = rep(0,sen)

for (i in 1:sen){
  k = sample(1:(n-1),1)
  rseminew = rsemi+rsemi^betasemi/(FriSemi$RATE[k])^betasemi*(FriSemi$RATE[k+1]-FriSemi$RATE[k])
  ryearnew = ryear+ryear^betayear/(FriSemi$RATE[k])^betayear*(FriYear$RATE[k+1]-FriYear$RATE[k])
  PandL[i] = 1 - (2/(1+rseminew)^0.5+102/(1+ryearnew))/FV
}

PandL = sort(PandL)
VaR = PandL[floor(sen*0.01)]*100

# Calculate VaR for one year maturity zero coupon bond

ryear = FriYear$RATE[n]

FV = 100/(1+ryear)

sen = 500

PandL = rep(0,sen)

for (i in 1:sen){
  k = sample(1:(n-1),1)
  ryearnew = ryear+ryear^betayear/(FriSemi$RATE[k])^betayear*(FriYear$RATE[k+1]-FriYear$RATE[k])
  PandL[i] = 1 - 100/(1+ryearnew)/FV
}

PandL = sort(PandL)
VaR1 = PandL[floor(sen*0.01)]*100

Frinext = FriYear[2:n,]
rate1 = FriYear[FriYear$RATE<0.011&FriYear$RATE>0.009,]
rate1d = Frinext[FriYear$RATE<0.011&FriYear$RATE>0.009,]
diffplot1 = data.frame(rate = rate1d$RATE-rate1$RATE, level = rep(1,nrow(rate1)) )
rate3 = FriYear[FriYear$RATE<0.027&FriYear$RATE>0.025,]
rate3d = Frinext[FriYear$RATE<0.027&FriYear$RATE>0.025,]
diffplot3 = data.frame(rate = rate3d$RATE-rate3$RATE, level = rep(3,nrow(rate3)) )
plotdata = rbind(diffplot1,diffplot3)
plotdata

ggplot(diffplot1, aes(rate)) +
  geom_density() +
  stat_function(fun = dnorm, colour = "red", args = list(mean = 0,sd = 0.01^betayear*sqrt(sigmayear)))

ggplot(diffplot3, aes(rate)) +
  geom_density() +
  stat_function(fun = dnorm, colour = "red", args = list(mean = 0,sd = 0.026^betayear*sqrt(sigmayear)))
