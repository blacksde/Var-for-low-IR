data<-read.csv("rate.csv")
head(data)

rateYear = data[data$TERM == 365,]$RATE

n = length(rateYear)

diff = rateYear[1:n]
k = floor(log(n)/log(2))

sd = rep(0, k)

for (i in 1:k){
  num = floor(n/2^i)
  var = 0
  for (j in 1:num) {
    var = var+(max(diff[(((j-1)*2^i+1):(j*2^i))])-min(diff[(((j-1)*2^i+1):(j*2^i))]))/sqrt(var(diff[(((j-1)*2^i+1):(j*2^i))]))
    #var = var+sqrt(var(diff[(((j-1)*2^i+1):(j*2^i))]))
  }
  sd[i] = log(var/num)
}

level = 1:k
fm=lm(sd~level)

plotdata = data.frame(logRS = sd, logScale = level)

ggplot(plotdata, aes(x=level, y=logRS))+
  geom_point() + 
  stat_smooth(method=lm, colour = "red")
summary(fm)
