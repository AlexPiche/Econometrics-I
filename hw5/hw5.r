##question 5.3

earning <- read.table('earnings.txt')
ear.lm <- lm(V5~V2+V3+V4+0, data = earning)

# question 5.4
earsub <- subset(earning, V4==1)
earsub.lm <- lm(V5~V4 + 0, data =  earsub)

#question 5.6
library(boot)

myMean <- function(data, indices){
    d <- data[indices,]
    return(mean(d$V5))

}

results <- boot(data=earsub, myMean, R=10000)
bootCI <- boot.ci(results)
bootCI$percent

# question 5.15
library(sandwich)

mw <- read.table('mw.txt')

#simulation 1
cons1ctr <- 0
hc11ctr <- 0
hc21ctr <- 0
hc31ctr <- 0

hc01ctr <- 0
hc02ctr <- 0
cons2ctr <- 0
hc12ctr <- 0
hc22ctr <- 0
hc32ctr <- 0

N <- 1000
for(i in 1:N){
    
    sim <- data.frame()

    for(i in 1:50){
ind  <- sample(1:50,1)
sim[i,1] <-  1 + mw[ind,]$V2 + mw[ind,]$V3 + rnorm(1)
sim[i,2] <- mw[ind,]$V2
sim[i,3] <- mw[ind,]$V3
    }

    sim.lm <- lm(V1~V2+V3, data = sim)
    cons <- vcovHC(sim.lm,type='cons')
    hc0 <- vcovHC(sim.lm,type='HC0')
    hc1 <- vcovHC(sim.lm,type='HC1')
    hc2 <- vcovHC(sim.lm,type='HC2')
    hc3 <- vcovHC(sim.lm,type='HC3')
    if(sim.lm$coefficient[1]<1){
    if(2*sqrt(cons[1,1])+sim.lm$coefficient[1]>=1){cons1ctr=1+cons1ctr}
    if(1.96*sqrt(hc0[1,1])+sim.lm$coefficient[1]>=1){hc01ctr=1+hc01ctr}
    if(1.96*sqrt(hc1[1,1])+sim.lm$coefficient[1]>=1){hc11ctr=1+hc11ctr}
    if(1.96*sqrt(hc2[1,1])+sim.lm$coefficient[1]>=1){hc21ctr=1+hc21ctr}
    if(1.96*sqrt(hc3[1,1])+sim.lm$coefficient[1]>=1){hc31ctr=1+hc31ctr}
    }else{
    
    if(-2*sqrt(cons[1,1])+sim.lm$coefficient[1]<=1){cons1ctr=1+cons1ctr}
    if(-1.96*sqrt(hc0[1,1])+sim.lm$coefficient[1]<=1){hc01ctr=1+hc01ctr}
    if(-1.96*sqrt(hc1[1,1])+sim.lm$coefficient[1]<=1){hc11ctr=1+hc11ctr}
    if(-1.96*sqrt(hc2[1,1])+sim.lm$coefficient[1]<=1){hc21ctr=1+hc21ctr}
    if(-1.96*sqrt(hc3[1,1])+sim.lm$coefficient[1]<=1){hc31ctr=1+hc31ctr}
    }

    if(sim.lm$coefficient[2]<1){
    if(2*sqrt(cons[2,2])+sim.lm$coefficient[2]>=1){cons2ctr=1+cons2ctr}
    if(1.96*sqrt(hc0[2,2])+sim.lm$coefficient[2]>=1){hc02ctr=1+hc02ctr}
    if(1.96*sqrt(hc1[2,2])+sim.lm$coefficient[2]>=1){hc12ctr=1+hc12ctr}
    if(1.96*sqrt(hc2[2,2])+sim.lm$coefficient[2]>=1){hc22ctr=1+hc22ctr}
    if(1.96*sqrt(hc3[2,2])+sim.lm$coefficient[2]>=1){hc32ctr=1+hc32ctr}
    }else{
    
    if(-2*sqrt(cons[2,2])+sim.lm$coefficient[2]<=1){cons2ctr=1+cons2ctr}
    if(-1.96*sqrt(hc0[2,2])+sim.lm$coefficient[2]<=1){hc02ctr=1+hc02ctr}
    if(-1.96*sqrt(hc1[2,2])+sim.lm$coefficient[2]<=1){hc12ctr=1+hc12ctr}
    if(-1.96*sqrt(hc2[2,2])+sim.lm$coefficient[2]<=1){hc22ctr=1+hc22ctr}
    if(-1.96*sqrt(hc3[2,2])+sim.lm$coefficient[2]<=1){hc32ctr=1+hc32ctr}
    }
}

cons1ctr/N
hc01ctr/N
hc11ctr/N
hc21ctr/N
hc31ctr/N

cons2ctr/N
hc02ctr/N
hc12ctr/N
hc22ctr/N
hc32ctr/N


#simulation 2
cons1ctr <- 0
hc11ctr <- 0
hc21ctr <- 0
hc31ctr <- 0

hc01ctr <- 0
hc02ctr <- 0
cons2ctr <- 0
hc12ctr <- 0
hc22ctr <- 0
hc32ctr <- 0

N <- 1000
for(i in 1:N){
    
    sim <- data.frame()

    for(i in 1:50){
ind  <- sample(1:50,1)
expect <- 1 + mw[ind,]$V2 + mw[ind,]$V3 
sim[i,1] <-  1 + mw[ind,]$V2 + mw[ind,]$V3 + rnorm(1, sd=expect)
sim[i,2] <- mw[ind,]$V2
sim[i,3] <- mw[ind,]$V3
    }

    sim.lm <- lm(V1~V2+V3, data = sim)
    cons <- vcovHC(sim.lm,type='cons')
    hc0 <- vcovHC(sim.lm,type='HC0')
    hc1 <- vcovHC(sim.lm,type='HC1')
    hc2 <- vcovHC(sim.lm,type='HC2')
    hc3 <- vcovHC(sim.lm,type='HC3')
    if(sim.lm$coefficient[1]<1){
    if(2*sqrt(cons[1,1])+sim.lm$coefficient[1]>=1){cons1ctr=1+cons1ctr}
    if(1.96*sqrt(hc0[1,1])+sim.lm$coefficient[1]>=1){hc01ctr=1+hc01ctr}
    if(1.96*sqrt(hc1[1,1])+sim.lm$coefficient[1]>=1){hc11ctr=1+hc11ctr}
    if(1.96*sqrt(hc2[1,1])+sim.lm$coefficient[1]>=1){hc21ctr=1+hc21ctr}
    if(1.96*sqrt(hc3[1,1])+sim.lm$coefficient[1]>=1){hc31ctr=1+hc31ctr}
    }else{
    
    if(-2*sqrt(cons[1,1])+sim.lm$coefficient[1]<=1){cons1ctr=1+cons1ctr}
    if(-1.96*sqrt(hc0[1,1])+sim.lm$coefficient[1]<=1){hc01ctr=1+hc01ctr}
    if(-1.96*sqrt(hc1[1,1])+sim.lm$coefficient[1]<=1){hc11ctr=1+hc11ctr}
    if(-1.96*sqrt(hc2[1,1])+sim.lm$coefficient[1]<=1){hc21ctr=1+hc21ctr}
    if(-1.96*sqrt(hc3[1,1])+sim.lm$coefficient[1]<=1){hc31ctr=1+hc31ctr}
    }

    if(sim.lm$coefficient[2]<1){
    if(2*sqrt(cons[2,2])+sim.lm$coefficient[2]>=1){cons2ctr=1+cons2ctr}
    if(1.96*sqrt(hc0[2,2])+sim.lm$coefficient[2]>=1){hc02ctr=1+hc02ctr}
    if(1.96*sqrt(hc1[2,2])+sim.lm$coefficient[2]>=1){hc12ctr=1+hc12ctr}
    if(1.96*sqrt(hc2[2,2])+sim.lm$coefficient[2]>=1){hc22ctr=1+hc22ctr}
    if(1.96*sqrt(hc3[2,2])+sim.lm$coefficient[2]>=1){hc32ctr=1+hc32ctr}
    }else{
    
    if(-2*sqrt(cons[2,2])+sim.lm$coefficient[2]<=1){cons2ctr=1+cons2ctr}
    if(-1.96*sqrt(hc0[2,2])+sim.lm$coefficient[2]<=1){hc02ctr=1+hc02ctr}
    if(-1.96*sqrt(hc1[2,2])+sim.lm$coefficient[2]<=1){hc12ctr=1+hc12ctr}
    if(-1.96*sqrt(hc2[2,2])+sim.lm$coefficient[2]<=1){hc22ctr=1+hc22ctr}
    if(-1.96*sqrt(hc3[2,2])+sim.lm$coefficient[2]<=1){hc32ctr=1+hc32ctr}
    }

}

cons1ctr/N
hc01ctr/N
hc11ctr/N
hc21ctr/N
hc31ctr/N

cons2ctr/N
hc02ctr/N
hc12ctr/N
hc22ctr/N
hc32ctr/N
