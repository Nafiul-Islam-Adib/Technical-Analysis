## Quant Fin workshop 3

library(quantmod)


start <- as.Date('2016-01-01')
end <- as.Date('2020-12-31')

aapl<-getSymbols('AAPL', from = start, to = end)

class(AAPL)
head(AAPL)
View (AAPL)
str(AAPL)

class(aapl)
str(aapl)

plot(AAPL[,'AAPL.Adjusted'], main='AAPL')
candleChart(AAPL)

## Arithmetic vs Geometric returns

p1<-105
p0<-100

p2 <- 434
p3 <- 464
ar<-(p3-p2)/p2
ar

ar<-(p1-p0)/p0
ar<-p1/p0-1
ar

gr<-log(p1/p0)
gr

#Convert log returns to simple returns
exp(gr)-1

#Convert simple to geometric
log(1+ar)

#Calculate more returns
#AAPL returns
AAPL.returns <- allReturns(AAPL)
AAPL.returns 

AAPL.ar <- periodReturn(AAPL[,'AAPL.Adjusted'], period='daily', type='arithmetic', leading=TRUE)

AAPL.gr<-periodReturn(AAPL[,'AAPL.Adjusted'], period='daily', type='log', leading=TRUE)

View(AAPL.ar*100)
View(AAPL.gr*100)

min(AAPL.ar*100, na.rm=TRUE)
min(AAPL.gr*100)

#Histogram and normal fit

par(mfrow=c(1,1))
aapl.hist1<-hist(AAPL.gr, plot=T, breaks=30)
aapl.hist1

hist(AAPL.ar, main='Arithmetic returns', col='cornflowerblue', xlab='returns', breaks=aapl.hist1$breaks)

xfit<-seq(min(AAPL.ar),max(AAPL.ar),length=40) 
xfit
yfit<-dnorm(xfit,mean=mean(AAPL.ar),sd=sd(AAPL.ar)) 
yfit
yfit1 <- yfit*diff(aapl.hist1$mids[1:2])*length(AAPL.ar) 
yfit1
lines(xfit, yfit1, col="red", lwd=2)

#for details on plotting a normal curve, watch https://www.youtube.com/watch?v=EPP7coCbBeI

#End of session

aapl.hist2 = hist(AAPL.gr,plot=F,breaks=30)
hist(AAPL.gr,main="Geometric", col="cornflowerblue",
     xlab="returns",
     breaks=aapl.hist2$breaks)
xfit<-seq(min(AAPL.gr),max(AAPL.gr),length=40) 
yfit<-dnorm(xfit,mean=mean(AAPL.gr),sd=sd(AAPL.gr)) 
yfit<-dnorm(xfit,mean=0,sd=0.01) 
yfit <- yfit*diff(aapl.hist2$mids[1:2])*length(AAPL.gr) 
lines(xfit, yfit, col="red", lwd=2)

#Asset returns simulation

##Monte carlo: throwing a dice

#roll = sample(1:6, 10000, replace=T)

RNGkind(sample.kind = "Rounding")

set.seed(1999)
sample(1:6, 10, replace=T)

roll = sample(1:6, 100, replace=T)
outcome <- as.data.frame(table(roll))
#View(outcome)
barplot(table(roll),col=rainbow(6),main="Dice throws",ylab="frequency")

##Monte Carlo Returns Simulation

mu=0.01
sigma=0.02
n=1000
m=100
set.seed(82)

#Generate m*n Gaussian simulations stored in matrix u (n*m)
u <- matrix(rnorm(n*m,0,1), n,m)
#Generate m returns paths of size n
Rt <- mu + sigma*u
#Plot
matplot(Rt,type = 'l')

#Simulate a Brownian Motion
diffinv(c(1,2,3,4,5))

Wt<- apply(u,2, diffinv)
BM <- mu + sigma*Wt
matplot(BM,type = 'l')

#S&P500 Statistical Analysis

#get prices from Yahoo for S&P500 (GSPC is the ticker)
start <- as.Date("2010-01-01")
end <- as.Date("2019-12-31")
getSymbols("^gspc", src = "yahoo", from = start, to = end)

#get information about S&P500
class(GSPC)
dim(GSPC)
head(GSPC)
View(GSPC)
GSPC.prices = na.omit(GSPC[, "GSPC.Adjusted"])
plot(GSPC.prices)
summary(GSPC.prices)

#Derive S&P500 returns -- only geometric
GSPC.returns <- periodReturn(GSPC.prices,period='daily',subset=NULL,type='log',leading=TRUE)

#Plot S&P500 returns
plot(GSPC.returns, main = "SPX")

#Histogram
GSPC.hist = hist(GSPC.returns,plot=F,breaks=10)
hist(GSPC.returns,main="SP500", col="cornflowerblue",
     xlab="returns",
     breaks=GSPC.hist$breaks)

#qqplot
qqnorm(GSPC.returns, main="SP500 Returns", col="slateblue1")
qqline(GSPC.returns)

#Autocorrelation
SPX.acorr=acf(GSPC.returns, main="Fz. autocorrelation SPX")

#Statistical summaries
summary(GSPC.returns)
