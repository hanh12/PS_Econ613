#Exercise 1
set.seed(1)
X1 = as.matrix(runif(10000,1,3),ncol = 1) #construct X1
X2 = as.matrix(rgamma(10000,shape = 3,scale = 2),ncol = 1) #construct X2
X3 = as.matrix(rbinom(10000,size = 1,prob = 0.3),ncol = 1) #construct X3
eps = as.matrix(rnorm(10000,mean = 2,sd = 1),ncol = 1) #construct eps
Y = as.matrix(0.5 + 1.2 * X1 + (-0.9) * X2 + eps, ncol = 1) #construct Y
ydum1 = matrix(as.numeric(Y > colMeans(Y)),ncol = 1) #construct ydum

#Exercise 2
#2.1 calculate correlation between Y and X1
cor_Y_X1 = (cov(Y,X1))/sqrt(var(Y)*var(X1))
#2.2 calculate the coefficients on this regression
n0.5 = as.matrix(rep(0.5,times = 10000)) #create a vector with dimension 10000 * 1
X = cbind(n0.5,X1,X2,X3) #combine all variables
coeff_Y = solve(t(X) %*% X) %*% t(X) %*% Y #calculate beta
coeff_Y
#2.3 calculate standard errors
resid = Y - X %*% coeff_Y
sigma_2 = (t(resid) %*% resid) / (nrow(X) - ncol(X)) # estimate sigma2
#2.4Using bootstrap with 49 and 499 replications respectively
#49 replications
M = cbind(Y,X)
sam1.beta <- as.data.frame(matrix(numeric(0),ncol = 4))
for(i in 1:49){
  sam1.X <- M[sample(10000,10000,replace = T),]
  beta <- matrix(c(solve(t(sam1.X[,2:5])%*%sam1.X[,2:5])%*%t(sam1.X[,2:5])%*%sam1.X[,1]),1,4)
  sam1.beta <- rbind(sam1.beta,beta)
}
se49 <- data.frame(apply(sam1.beta, 2,sd))
se49
#499 replications
M = cbind(Y,X)
sam2.beta <- as.data.frame(matrix(numeric(0),ncol = 4))
for(i in 1:499){
  sam2.X <- M[sample(10000,10000,replace = T),]
  beta <- matrix(c(solve(t(sam2.X[,2:5])%*%sam2.X[,2:5])%*%t(sam2.X[,2:5])%*%sam2.X[,1]),1,4)
  sam2.beta <- rbind(sam2.beta,beta)
}
se499 <- data.frame(apply(sam1.beta, 2,sd))
se499

#Exercise 3
#write a function that returns the likelihood of the probit

likelihoodf <- function(beta){
  return(sum(ydum * log(pnorm(X %*% beta))) + sum((1-ydum) * log(1-pnorm(X %*% beta))))
}

#apply the steepest ascent optimization algorithm

#i <- matrix(c(0.03,0.2,0.2,0.1),4,1)
i <- matrix(c(1.5,0.1,0.1,-0.1),4,1)
a <- diag(0.00001,4,4)
#i0 <- matrix(c(0.02999,0.19999,0.19999,0.09999),4,1)
i0 <- matrix(c(1.49999,0.09999,0.09999,-0.10001),4,1)
while (likelihoodf(i)<likelihoodf(i0)) {
  i <- i0
  idiag <- matrix(c(rep(i,4)),4,4)
  idiag0 <- idiag + a
  b0 <- (likelihoodf(idiag0[,1]) - likelihoodf(idiag[,1]))/0.00001
  b1 <- (likelihoodf(idiag0[,2]) - likelihoodf(idiag[,2]))/0.00001
  b2 <- (likelihoodf(idiag0[,3]) - likelihoodf(idiag[,3]))/0.00001
  b3 <- (likelihoodf(idiag0[,4]) - likelihoodf(idiag[,4]))/0.00001
  dk <- matrix(c(b0,b1,b2,b3),4,1)
  i0 <- i + 0.00001*dk
}
print(i)

#Exercise 4
#probit model
data_dum = as.data.frame(cbind(ydum,X))
probit_y = glm(ydum~X-1,family = binomial(link = "probit"),data = data_dum)

#logit model
logit_y = glm(ydum~X-1,family = binomial(link = "logit"),data = data_dum)
#linear probability model
linear_y = lm(ydum~X-1)

summary(probit_y)
summary(logit_y)
summary(linear_y)

#The coefficients are not significantly different among the three models.

#Exercise 5
#Marginal effect of probit model
coeff_probit = coefficients(probit_y)
cdf_probit = (pnorm(mean(X %*% coeff_probit) + 0.00001) - pnorm(mean(X %*% coeff_probit)))/0.00001
me_probit = as.data.frame(cdf_probit * coeff_probit)
#Marginal effect of logit model
coeff_logit = coefficients(logit_y)
cdf_logit = (pnorm(mean(X %*% coeff_logit) + 0.00001) - pnorm(mean(X %*% coeff_logit)))/0.00001
me_logit = as.data.frame(cdf_logit * coeff_logit)

#compute the sd using the delta method
#probit model
H_probit <- vcov(probit_y)
coeff_probit_new <- data.frame(coeff_probit,coeff_probit,coeff_probit,coeff_probit)
coeff_probit_new <- coeff_probit_new + diag(0.00001,4)
M1 <- function(beta) mean(dnorm(X %*% beta) * beta[1])
g11 <- (M1(coeff_probit_new[,1]) - M1(coeff_probit))/0.00001
g12 <- (M1(coeff_probit_new[,2]) - M1(coeff_probit))/0.00001
g13 <- (M1(coeff_probit_new[,3]) - M1(coeff_probit))/0.00001
g14 <- (M1(coeff_probit_new[,4]) - M1(coeff_probit))/0.00001
g1 <- matrix(c(g11,g12,g13,g14),1,4) # The first row of Jacobian matrix

M2 <- function(beta) mean(dnorm(X%*%beta)*beta[2])
g21 <- (M2(coeff_probit_new[,1]) - M2(coeff_probit))/0.00001
g22 <- (M2(coeff_probit_new[,2]) - M2(coeff_probit))/0.00001
g23 <- (M2(coeff_probit_new[,3]) - M2(coeff_probit))/0.00001
g24 <- (M2(coeff_probit_new[,4]) - M2(coeff_probit))/0.00001
g2 <- matrix(c(g21,g22,g23,g24),1,4) # The second row of Jacobian matrix

M3 <- function(beta) mean(dnorm(X%*%beta)*beta[3])
g31 <- (M3(coeff_probit_new[,1]) - M3(coeff_probit))/0.00001
g32 <- (M3(coeff_probit_new[,2]) - M3(coeff_probit))/0.00001
g33 <- (M3(coeff_probit_new[,3]) - M3(coeff_probit))/0.00001
g34 <- (M3(coeff_probit_new[,4]) - M3(coeff_probit))/0.00001
g3 <- matrix(c(g31,g32,g33,g34),1,4) # The third row of Jacobian matrix

M4 <- function(beta) mean(dnorm(X%*%beta)*beta[4])
g41 <- (M4(coeff_probit_new[,1]) - M4(coeff_probit))/0.00001
g42 <- (M4(coeff_probit_new[,2]) - M4(coeff_probit))/0.00001
g43 <- (M4(coeff_probit_new[,3]) - M4(coeff_probit))/0.00001
g44 <- (M4(coeff_probit_new[,4]) - M4(coeff_probit))/0.00001
g4 <- matrix(c(g41,g42,g43,g44),1,4)

J_probit <- rbind(g1,g2,g3,g4)
delta_probit <- t(J_probit) %*% H_probit %*% J_probit# The result of probit model
sd_probit <- diag(delta_probit)
#logit model
H_logit <- vcov(logit_y)
coeff_logit_new <- data.frame(coeff_logit,coeff_logit,coeff_logit,coeff_logit)
coeff_logit_new <- coeff_logit_new + diag(0.00001,4)
M1 <- function(beta) mean(dnorm(X %*% beta) * beta[1])
g11 <- (M1(coeff_logit_new[,1]) - M1(coeff_logit))/0.00001
g12 <- (M1(coeff_logit_new[,2]) - M1(coeff_logit))/0.00001
g13 <- (M1(coeff_logit_new[,3]) - M1(coeff_logit))/0.00001
g14 <- (M1(coeff_logit_new[,4]) - M1(coeff_logit))/0.00001
g1 <- matrix(c(g11,g12,g13,g14),1,4) # The first row of Jacobian matrix

M2 <- function(beta) mean(dnorm(X%*%beta)*beta[2])
g21 <- (M2(coeff_logit_new[,1]) - M2(coeff_logit))/0.00001
g22 <- (M2(coeff_logit_new[,2]) - M2(coeff_logit))/0.00001
g23 <- (M2(coeff_logit_new[,3]) - M2(coeff_logit))/0.00001
g24 <- (M2(coeff_logit_new[,4]) - M2(coeff_logit))/0.00001
g2 <- matrix(c(g21,g22,g23,g24),1,4) # The second row of Jacobian matrix

M3 <- function(beta) mean(dnorm(X%*%beta)*beta[3])
g31 <- (M3(coeff_logit_new[,1]) - M3(coeff_logit))/0.00001
g32 <- (M3(coeff_logit_new[,2]) - M3(coeff_logit))/0.00001
g33 <- (M3(coeff_logit_new[,3]) - M3(coeff_logit))/0.00001
g34 <- (M3(coeff_logit_new[,4]) - M3(coeff_logit))/0.00001
g3 <- matrix(c(g31,g32,g33,g34),1,4) # The third row of Jacobian matrix

M4 <- function(beta) mean(dnorm(X%*%beta)*beta[4])
g41 <- (M4(coeff_logit_new[,1]) - M4(coeff_logit))/0.00001
g42 <- (M4(coeff_logit_new[,2]) - M4(coeff_logit))/0.00001
g43 <- (M4(coeff_logit_new[,3]) - M4(coeff_logit))/0.00001
g44 <- (M4(coeff_logit_new[,4]) - M4(coeff_logit))/0.00001
g4 <- matrix(c(g41,g42,g43,g44),1,4)

J_logit <- rbind(g1,g2,g3,g4)
delta_logit <- t(J_logit) %*% H_logit %*% J_logit# The result of logit model
sd_logit <- diag(delta_logit)

#compute the sd using the bootstrap
#probit model
#boot = 499
mesample<-as.data.frame(matrix(numeric(0),ncol = 4))
for (i in 1:499) {
  M <- cbind(ydum,X)
  samprobit.X <- M[sample(10000,10000,replace = T),]
  probit_pb <- glm(M[,1]~0+M[,2:5], family = binomial(link = "probit"))
  coeff_pb <- coefficients(probit_pb)
  mean_pb <- mean(samprobit.X[,2:5] %*% coeff_pb)
  cdf_pb <- (pnorm(mean_pb + 0.00001) - pnorm(mean_pb))/0.00001
  me_bp <- matrix(cdf_pb * coeff_pb,1,4)
  mesample <- rbind(mesample, me_bp)
}
me_pb <- apply(mesample,2,sd)

mesample<-as.data.frame(matrix(numeric(0),ncol = 4))
for (i in 1:499) {
  M <- cbind(ydum,X)
  samlogit.X <- M[sample(10000,10000,replace = T),]
  logit_lb <- glm(M[,1]~0+M[,2:5], family = binomial(link = "logit"))
  coeff_lb <- coefficients(logit_lb)
  mean_lb <- mean(samlogit.X[,2:5] %*% coeff_lb)
  cdf_lb <- (pnorm(mean_lb + 0.00001) - pnorm(mean_lb))/0.00001
  me_bl <- matrix(cdf_lb * coeff_lb,1,4)
  mesample <- rbind(mesample, me_bl)
}
me_lb <- apply(mesample,2,sd)






