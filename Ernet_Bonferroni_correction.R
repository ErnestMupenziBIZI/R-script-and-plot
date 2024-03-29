
set.seed(2653)
library(TeachingDemos)
library(MASS)
biv.ttest <- function(nn=100,mu=c(0,0),sigma1=1,sigma2=1,rho=0.8,alp=0.05)
{
  Sigma<-matrix(c(sigma1^2,rho*sigma1*sigma2,rho*sigma1*sigma2,sigma2^2),2,2) 
  #we got problem when we replace rho by rho*sigma1*sigma2
  mvrn <- mvrnorm(nn,mu ,Sigma)
  x <- mvrn[,1]
  y <- mvrn[,2]
  #print(cor(x,y))
  
  z.test(x,mu=0,1,"less")
  z.test(y,mu=0,1,"less")
  resx <- z.test(x,mu=0,1,"less")
  resy <- z.test(y,mu=0,1,"less")
  aa<-resx$p.value< alp
  bb<-resy$p.value < alp
  #print(c(resx$p.value, resy$p.value))
  #print(resx)
  
  return (c(aa , bb ,aa|bb))
}

biv.ttest(nn=100,mu=c(0,0),sigma1=1,sigma2=1,rho=0.8,alp=0.05)

############Estimating the family wise error rate  1000000 time
looptest <- function(loops=1000000,nn=100,mu=c(0,0),sigma1=1,sigma2=1,rho=0.8,alp=0.05)
{
  restest <- matrix(0,loops,3)
  for(ii in 1:loops)
    restest[ii,] <- biv.ttest(nn=nn,mu=mu,sigma1=sigma1,sigma2=sigma2,rho=rho,alp=alp)    
  c(mean(restest[,1]),mean(restest[,2]),(mean(restest[,3])))
}
##incorrect family wise error rate plotting 

looptest(loops=1000000,nn=100,mu=c(0,0),sigma1=1,sigma2=1,rho=0.8,alp=0.05)

rhos <- seq(from=-1, to=1, by = 0.1)
As <-rhos
for ( ii in 1:length(rhos)){
  As[ii] <- looptest(loops=1000000,nn=100,mu=c(0,0),sigma1=1,sigma2=1,rho=rhos[ii],alp=0.05)[3]
}
plot(rhos,As, type='l',main = "FWER against Correlation rho unadjusted",xlab = "ρ",ylab = "Family ise error rate")

# Bonferroni adjustment (alpha/2)
rhos <- seq(from=-1, to=1, by = 0.1)
As <-rhos

for ( ii in 1:length(rhos)){
  As[ii] <- looptest(loops=1000000,nn=100,mu=c(0,0),sigma1=1,sigma2=1,rho=rhos[ii],alp=0.025)[3]
}

plot(rhos,As,type='l',xlab = "ρ",ylab = "Corrected FWER")
abline(h =0.025, untf = FALSE,col=2,lty=2)
####################################################trying estimators###################
