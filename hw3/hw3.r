f3.1
library(zoo)
#create a dataframe containing all the biaises
bias  <- data.frame(factor(c('25', '50', '100', '200')), 0, 0)
names(bias) <- c('obs', 'intbias', 'slbias')

############################################Generate sample of 25, 50, 100, 200
bsl25  <- array()
bcte25  <- array()
for (j in 1:1000){ 
data25 <- data.frame(1:25,0)
names(data25) <- c('obs', 'y')

for (i in 1:25){
data25[i+1,2] <- 1 + 0.8*data25[i,2] + rnorm(1)
}

data25  <- zoo(data25)
data25$y1 <- lag(data25$y, -1)
data25.lm  <- lm(y~y1, data=data25)

bcte25[j] <- ( 1-summary(data25.lm)$coefficient[1])
bsl25[j] <- ( 0.8-summary(data25.lm)$coefficient[2])
}

bias[1,2] <- mean(bcte25)
bias[1,3] <- mean(bsl25)
    
##############
bsl50  <- array()
bcte50  <- array()
for (j in 1:1000){ 
data50 <- data.frame(1:50,0)
names(data50) <- c('obs', 'y')

for (i in 1:50){
data50[i+1,2] <- 1 + 0.8*data50[i,2] + rnorm(1)
}
data50  <- zoo(data50)
data50$y1 <- lag(data50$y, -1)
data50.lm  <- lm(y~y1, data=data50)

bcte50[j] <- ( 1-summary(data50.lm)$coefficient[1])
bsl50[j] <- ( 0.8-summary(data50.lm)$coefficient[2])
}

bias[2,2] <- mean(bcte50)
bias[2,3] <- mean(bsl50) 

###############
bsl100  <- array()
bcte100  <- array()
for (j in 1:1000){ 
data100 <- data.frame(1:100,0)
names(data100) <- c('obs', 'y')

for (i in 1:100){
data100[i+1,2] <- 1 + 0.8*data100[i,2] + rnorm(1)
}
data100  <- zoo(data100)
data100$y1 <- lag(data100$y, -1)
data100.lm  <- lm(y~y1, data=data100)

bcte100[j] <- ( 1-summary(data100.lm)$coefficient[1])
bsl100[j] <- ( 0.8-summary(data100.lm)$coefficient[2])
}

bias[3,2] <- mean(bcte100)
bias[3,3] <- mean(bsl100)
###############
bsl200  <- array()
bcte200  <- array()
for (j in 1:1000){ 
data200 <- data.frame(1:200,0)
names(data200) <- c('obs', 'y')

for (i in 1:200){
data200[i+1,2] <- 1 + 0.8*data200[i,2] + rnorm(1)
}
data200  <- zoo(data200)
data200$y1 <- lag(data200$y, -1)
data200.lm  <- lm(y~y1, data=data200)

bcte200[j] <- ( 1-summary(data200.lm)$coefficient[1])
bsl200[j] <- ( 0.8-summary(data200.lm)$coefficient[2])
}

bias[4,2] <- mean(bcte200)
bias[4,3] <- mean(bsl200)

#########################################Plot the result
bias$obs <- factor(bias$obs, level=bias$obs)

library(ggplot2)

ggplot(bias, aes(x = obs, y = intbias, width = 0.25, order=TRUE )) + geom_bar(fill='black', stat = 'identity',position = "identity") + theme(axis.line = element_line(colour = "gray"), axis.title.x=element_blank(), axis.title.y=element_blank(), panel.background=element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank()) + geom_hline(yintercept=0, colour = "gray")

ggplot(bias, aes(x = obs, y = slbias, width = 0.25 )) + geom_bar(fill='black', stat = 'identity') + theme(axis.line = element_line(colour = "gray"), axis.title.x=element_blank(), axis.title.y=element_blank(), panel.background=element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank()) + geom_hline(yintercept=0, colour = "gray")
#3.23

cons  <-  read.table('consumption.txt')
names(cons) <- c('obs', 'year', 'quarter', 'y', 'c')
cons  <- zoo(cons)

cons$logc  <- log(cons$c)
cons$logc1  <- lag(cons$logc, -1)
cons$logy  <- log(cons$y)
cons$logy1  <- lag(cons$logy, -1)
cons$diffc  <- diff(cons$logc)
cons$diffy  <- diff(cons$logy)

cons <- subset(cons, year>1952)

#3.23 equation 3.04
cons.lm <- lm(logc ~ logc1 + logy + logy1, data = cons)
summary(cons.lm)
cons.res <- residuals(cons.lm)
xx <- summary(cons.lm)$coefficient

#3.23 equation 3.05
diff.lm <- lm(diffc ~ logc1 + diffy + logy1, data=cons)
summary(diff.lm)

#3.34

conssim <- cons
conssim$logc <- 0
conssim[1,]  <- cons[1,]

for (i in 2:176){

conssim[i,6]  <-  xx[1] + xx[2]*as.numeric(conssim[i,7]) + xx[3]*as.numeric(conssim[i,8]) + xx[4]*as.numeric(conssim[i,9]) + rnorm(1, sd=sd(residuals(cons.lm)))
if(i!=176){
conssim[i+1,7] <- conssim[i,6]
}
}

conssim.lm <- lm(logc ~ logc1 + logy + logy1, data = conssim)
conssim.res <- residuals(conssim.lm)
plot(cons$year, cons.res) 
points(cons$year,conssim.res, pch=19)

