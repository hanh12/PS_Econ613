#Assignment 4_ Linear Panel Data
install.packages("nlme")
library(nlme)
install.packages("dplyr")
library(dplyr)
#Exercise 1 Data
d = read.csv("KoopTobias.csv") # import data
personid = matrix(d$PERSONID,ncol = 1)
logwage = matrix(d$LOGWAGE,ncol = 1)
time = matrix(d$TIMETRND,ncol = 1)
ex1 = matrix(cbind(personid, logwage, time),ncol = 3)
colnames(ex1) = c("personid","logwage","time")
#personid = 1
x1 = matrix(ex1[1:4,],ncol = 3)
colnames(x1) = c("personid","logwage","time")
time1 = x1[,3]
logwage1 = x1[,2]
plot(time1,logwage1) #plot personid = 1
#personid = 2
x2 = matrix(ex1[5:13,],ncol = 3)
colnames(x1) = c("personid","logwage","time")
time2 = x2[,3]
logwage2 = x2[,2]
plot(time2,logwage2) #plot personid = 2
#personid = 3
x3 = matrix(ex1[14,],ncol = 3)
colnames(x3) = c("personid","logwage","time")
time3 = x3[,3]
logwage3 = x3[,2]
plot(time3,logwage3) #plot personid = 3 
#personid = 4
x4 = matrix(ex1[15:26,],ncol = 3)
colnames(x4) = c("personid","logwage","time")
time4 = x4[,3]
logwage4 = x4[,2]
plot(time4,logwage4) #plot personid = 4
#personid = 5
x5 = matrix(ex1[27:29,],ncol = 3)
colnames(x5) = c("personid","logwage","time")
time5 = x5[,3]
logwage5 = x5[,2]
plot(time5,logwage5) #plot personid = 5

#Exercise 2
data2 <- data.frame(cbind(d[,3],d[,2],d[,4]),ncol = 3)
colnames(data2) = c("logwage","educ","potexper")

gls(logwage~educ+potexper,data = data2) #estimate the random effect model

#Exercise 3
#between estimator

t = table(d[,1])
data3 = data.frame(d[,1:4])
colnames(data3) = c("personid","educ","logwage","potexper")
sum_edu <- data3 %>%
  group_by(personid) %>%
  summarize(sum_edu = sum(educ))
edu_bar = sum_edu/t         #calculate average of education conditional on individual

sum_logwage <- data3 %>%
  group_by(personid) %>%
  summarize(sum_logwage = sum(logwage))
logwage_bar = sum_logwage/t  #calculate average of logwage conditional on individual

sum_potexper <- data3 %>%
  group_by(personid) %>%
  summarize(sum_potexper = sum(potexper))
potexper_bar = sum_potexper/t    #calculate average of potexper conditional on individual

Data3_bar = data.frame(cbind(logwage_bar[,2],edu_bar[,2],potexper_bar[,2]))
colnames(Data3_bar) = c("logwage","edu","potexper")
lm(logwage~edu+potexper,data = Data3_bar) #estimate the between estimator model

#within estimator
s = matrix(seq(1,2178),ncol = 1)
Data3 = cbind(s,Data3_bar)
colnames(Data3) = c("personid","logwage","edu","potexper")

Data4 = d[,1:4]
colnames(Data4) = c("personid","edu","logwage","potexper")

me = Data4 %>% left_join(Data3, by = "personid") #merge the average of the three variables with the raw data 

bet_logwage = me[,3] - me[,5]  #calculate the variables
bet_edu = me[,2] - me[,6] 
bet_potexper = me[,4] - me[,7]

Data4_bana = data.frame(cbind(bet_logwage,bet_edu,bet_potexper))
colnames(Data4_bana) = c("logwage","edu","potexper")
r_within = lm(logwage~edu+potexper-1,data = Data4_bana) #estimate the within estimator 

#first-time difference estimator
Data4_apple = d[1:17918,1:4]
Data4_pen = d[2:17919,1:4]
Data4_pineapple = Data4_pen - Data4_apple
#for(i in 1:17918){
#  if(Data4_pineapple[i,1] == 1){
#    Data4_pineapple[i,] = NA
#  }
#}
lagData4 = Data4_pineapple[!Data4_pineapple[,1] == 1,]
colnames(lagData4) = c("personid","edu","logwage","potexper")
lm(logwage~edu+potexper,data = lagData4)  #estimate the first time difference model

#Exercise 4
#random selected 100 individuals
r = sample(1:2178,100) #randomly produce 100 numbers
Data5 = d[,1:4]
Data5_inv = d[,c(1,6:10)]
colnames(Data5) = c("personid","edu","logwage","potexper")
colnames(Data5_inv) = c("personid","ability","mothered","fathered","brknhome","siblings")
Data5_rs = Data5[Data5[,1] %in% r,] #select the corresponding rows from raw data
Data5_invrs = Data5_inv[Data5_inv[,1] %in% r,]
Data5_rs = as.matrix(Data5_rs,ncol = 4)
Data5_invrs = as.matrix(Data5_invrs,ncol = 6)
Data5_inva = Data5_invrs[!duplicated(Data5_invrs[,1]),] #delete duplicated rows conditional on individuals

#write and optimize the likelihood function associated to the problem
likelihoodf <- function(beta){
  return(-sum(Data5_rs[,3] * log(pnorm(Data5_rs[,c(2,4)] %*% beta))) + sum((1 - Data5_rs[,3]) * log(1 - pnorm(Data5_rs[,c(2,4)] %*% beta))))
}
beta = c(0.1,0.05)
betar = optim(par = beta,likelihoodf)$par #optimize the likelihood function
print(betar)
#estimate the individual fixed effect estimators
# Data5_rs = data.frame(Data5_rs)
# res_rs = lm(logwage~edu+potexper,data = Data5_rs)
#between estimator
t1 = table(Data5_rs[,1])
Data5_rs = as.data.frame(Data5_rs)

sum_edu1 <- Data5_rs %>%
  group_by(personid) %>%
  summarize(sum_edu1 = sum(edu))
edu_bar1 = sum_edu1/t1

sum_logwage1 <- Data5_rs %>%
  group_by(personid) %>%
  summarize(sum_logwage1 = sum(logwage))
logwage_bar1 = sum_logwage1/t1

sum_potexper1 <- Data5_rs %>%
  group_by(personid) %>%
  summarize(sum_potexper1 = sum(potexper))
potexper_bar1 = sum_potexper1/t1

Data5_bar = data.frame(cbind(logwage_bar1[,2],edu_bar1[,2],potexper_bar1[,2]))
colnames(Data5_bar) = c("logwage","edu","potexper")
beta5 = lm(logwage~edu+potexper,data = Data5_bar)
b_edu = beta5$coefficients[2]
b_potexper = beta5$coefficients[3]
alpha = logwage_bar1[,2] - b_edu * edu_bar1[,2] - b_potexper * potexper_bar1[,2]
alpha = matrix(alpha, ncol = 1)
colnames(alpha) = c("individual fixed effect")

Data5_ife = cbind(alpha,Data5_inva[,2:6])
Data5_ife = data.frame(Data5_ife)
colnames(Data5_ife) = c("ife","ability","mothered","fathered","brknhome","siblings")

#Run a regression of estimated individual fixed effets on the invariant variables
r_ife = lm(ife~ability+mothered+fathered+brknhome+siblings,data = Data5_ife)
print(r_ife)
#explain why the standard errors in the previous may not be correctly estimated.
#because the previous model made an assumption that the variables are independent from time, but in practice, they are possibly positive correlated with time.

#propose an alternative method to compute standard errors
# install.packages("sandwich")
# library(sandwich)
#se = diag(vcovHC(r_within,type = "HC3"))^0.5
#print(se)
#According to Prof. Sidibe's hints, abort the "sandwich" package and use bootstrap to calculate se.
rbot = sample(1:2178,100) #randomly produce 100 numbers
Databot = d[d[,1] %in% rbot,]
colnames(Databot) = c("personid","educ","logwage","potexper","timetrend","ability","mothered","fatherecd","brknhome","siblings")
boot = matrix(0,nrow=49,ncol=6)
for (b in 1:49) {
  i = matrix(sample(rbot,100,replace=TRUE),ncol = 1) #subsampling
  e = matrix(0,ncol=10,nrow=1)
  for(j in 1:100){
    e = rbind(e,as.matrix(Databot[Databot[,1] %in% i[j,],]))
  }
  data6 = as.data.frame(e[2:nrow(e),])
  mb = aggregate(cbind(logwage,educ,potexper) ~ personid, data = data6, mean)
  alpha1 = matrix(mb[,2] - (mb[,3]*betar[1]+mb[,4]*betar[2]),ncol=1)
  data7 = data6[!duplicated(data6$personid),]
  X2_bot = as.matrix(data7[,6:10])
  model6 = lm(alpha1 ~ X2_bot)$coef
  boot[b,] = model6
}
se_boot = cbind(sd(boot[,1]),sd(boot[,2]),sd(boot[,3]),sd(boot[,4]),sd(boot[,5]),sd(boot[,6]))
colnames(se_boot) = c("intercept","ability","mothered","fathered","brknhome","siblings")
rownames(se_boot) = c("corrected_se_bot")
se_boot











