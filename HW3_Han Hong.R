#HW3
#==========================================================
#Exercise 1
#==========================================================
library(bayesm)
data("margarine")
p = read.csv("product.csv")
d = read.csv("demos.csv")
hhid = matrix(p$hhid,ncol=1)
choice = matrix(p$choice,ncol=1)
ppks = matrix(p$PPk_Stk,ncol=1)
pbbs = matrix(p$PBB_Stk,ncol=1)
pfls = matrix(p$PFl_Stk,ncol=1)
phses = matrix(p$PHse_Stk,ncol=1)
pgens = matrix(p$PGen_Stk,ncol=1)
pimps = matrix(p$PImp_Stk,ncol=1)
psst = matrix(p$PSS_Tub,ncol=1)
ppkt = matrix(p$PPk_Tub,ncol=1)
pflt = matrix(p$PFl_Tub,ncol=1)
phset = matrix(p$PHse_Tub,ncol=1)

hhidd = matrix(d$hhid,ncol = 1)
income = matrix(d$Income,ncol = 1)
fs34 = matrix(d$Fs3_4,ncol = 1)
fs5 = matrix(d$Fs5.,ncol = 1)
fs = matrix(d$Fam_Size,ncol = 1)
college = matrix(d$college,ncol = 1)
wcollar = matrix(d$whtcollar,ncol = 1)
retire = matrix(d$retired,ncol = 1)
d.all = cbind(hhidd,income,fs34,fs5,fs,college,wcollar,retire)
colnames = c("hhid","income","fs34","fs5","fs","college","wcollar","retire")
#==========================================================
#exercise1.1
#==========================================================
#average and dispersion in product characteristic
summary(margarine)
summary(margarine$choicePrice) #average and dispersion in product characteristics
#exercise 1.2
#Market share by product
dec = matrix(rep(0,4470*10),nrow = 4470,ncol = 10)
for(i in 1:4470){
  for(j in 1:10){
    if(choice[i] == j){
      dec[i,j] = 1
    }
  }
}
choiceprice = cbind(ppks,pbbs,pfls,phses,pgens,pimps,psst,ppkt,pflt,phset)
dec.pro = dec*choiceprice
s = matrix(apply(dec.pro,2,sum),nrow = 1)
colnames(s) = c("ppkstk","pbbstk","pflstk","phsestk","pgenstk","pimpstk","psstub","ppktub","pfltub","phsetub")
all = sum(s)
mks_pro = s/all #market share by product
View(mks_pro)

#market share by brand
dec.brand = matrix(rep(0,4470*7),ncol = 7, nrow = 4470)
dec.brand[,1] = dec.pro[,1] + dec.pro[,8]
dec.brand[,2] = dec.pro[,2]
dec.brand[,3] = dec.pro[,3] + dec.pro[,9]
dec.brand[,4] = dec.pro[,4] + dec.pro[,10]
dec.brand[,5] = dec.pro[,5]
dec.brand[,6] = dec.pro[,6]
dec.brand[,7] = dec.pro[,7]
s.brand = matrix(apply(dec.brand,2,sum),nrow = 1)
all_brand = sum(s.brand)
mks_brand = s.brand/all_brand
colnames(mks_brand) = c("ppk","pbb","pfl","phse","pgen","pimp","pss")
View(mks_brand)
#market share by stk/tub
dec.type = matrix(rep(0,4470*2),ncol = 2,nrow = 4470)
dec.type[,1] = dec.pro[,1]+dec.pro[,2]+dec.pro[,3]+dec.pro[,4]+dec.pro[,5]+dec.pro[,6]
dec.type[,2] = dec.pro[,7]+dec.pro[,8]+dec.pro[,9]+dec.pro[,10]
s.stk = matrix(apply(dec.type,2,sum),nrow = 1)

all_st = sum(s.stk)
mks_st = s.stk/all_st #market share bt stk/tub
colnames(mks_st) = c("stk","tub")
View(mks_st)
#exercise 1.3
#mapping
p.all = cbind(hhid,choice,ppks,pbbs,pfls,phses,pgens,pimps,psst,ppkt,pflt,phset)
colnames(p.all) = c("hhid","choice","ppks","pbbs","pfls","phses","pgens","pimps","psst","ppkt","pflt","phset")
ch1 = subset(p.all,choice==1,select=c(hhid,ppks,choice))
ch2 = subset(p.all,choice==2,select=c(hhid,pbbs,choice))
ch3 = subset(p.all,choice==3,select=c(hhid,pfls,choice))
ch4 = subset(p.all,choice==4,select=c(hhid,phses,choice))
ch5 = subset(p.all,choice==5,select=c(hhid,pgens,choice))
ch6 = subset(p.all,choice==6,select=c(hhid,pimps,choice))
ch7 = subset(p.all,choice==7,select=c(hhid,psst,choice))
ch8 = subset(p.all,choice==8,select=c(hhid,ppkt,choice))
ch9 = subset(p.all,choice==9,select=c(hhid,pflt,choice))
ch10 = subset(p.all,choice==10,select=c(hhid,phset,choice))
colnames(ch1) = c("hhid","product","choice")
colnames(ch2) = c("hhid","product","choice")
colnames(ch3) = c("hhid","product","choice")
colnames(ch4) = c("hhid","product","choice")
colnames(ch5) = c("hhid","product","choice")
colnames(ch6) = c("hhid","product","choice")
colnames(ch7) = c("hhid","product","choice")
colnames(ch8) = c("hhid","product","choice")
colnames(ch9) = c("hhid","product","choice")
colnames(ch10) = c("hhid","product","choice")
ch = rbind(ch1,ch2,ch3,ch4,ch5,ch6,ch7,ch8,ch9,ch10)
colnames(ch) = c("hhid","product","choice")
e1.3 = matrix(cbind(hhidd,income),ncol = 2)
colnames(e1.3) = c("hhid","income")
ch_s = merge(ch,e1.3,by="hhid",all = T, sort = T)
colnames(ch_s) = c("hhid","product","choice","income")
ch_ss = ch_s[,c(4,3)]
colnames(ch_ss) = c("income","choice")
sp = split(ch_ss,ch_ss[,c("income")],drop = TRUE)
f = as.data.frame(matrix(nrow = 0,ncol = 10))
colnames(f) = c("product1","product2","product3","product4","product5","product6","product7","product8","product9","product10")
for(i in 1:14){
  level = as.data.frame(sp[i])
  colnames(level) = c("income","choice")
  c = data.frame(matrix(nrow = 1,ncol = 0))
  for(j in 1:10){
    num = sum(level$choice==j)
    c = cbind.data.frame(c,num)
  }
  colnames(c) = c("product1","product2","product3","product4","product5","product6","product7","product8","product9","product10")
  f = rbind(f,c)
}
i = c("2.5","7.5","12.5","17.5","22.5","27.5","32.5","37.5","42.5","47.5","55","67.5","87.5","130")
f = cbind(i,f)
colnames(f) = c("income level","product1","product2","product3","product4","product5","product6","product7","product8","product9","product10")
View(f)


#Exercise 2
#propose a model specification
#Use multinomial models, Y is defined as the choices from 1 to 10, X is defined as the price of the ten choices.
fc <- function(x){
  x - ppks
}
df1 = as.matrix(cbind(ppks,pbbs,pfls,phses,pgens,pimps,psst,ppkt,pflt,phset),ncol=10)
df0 = apply(df1,2,fc)
#alpha = matrix(rep(0,4470),rep(2,4470),rep(3,4470),rep(4,4470),rep(5,4470),rep(6,4470),rep(7,4470),rep(8,4470),rep(9,4470),rep(10,4470),ncol = 10)
dec = matrix(rep(0,4470*10),nrow = 4470,ncol = 10)
for(i in 1:4470){
  for(j in 1:10){
    if(choice[i] == j){
      dec[i,j] = 1
    }
  }
}
likelihoodf <- function(theta){
  #  beta = matrix(rep(theta[1],4470),nrow = 4470,ncol = 1)
  beta = as.numeric(theta[1])
  alpha = matrix(rep(theta[2:10],each = 4470),nrow = 4470,ncol = 9)
  z = c(rep(0,4470))
  alpha = matrix(cbind(z,alpha),nrow=4470,ncol=10)
  V <- df0*beta + alpha
  pro = matrix(c(rep(0,4470*10)),nrow = 4470,ncol = 10)
  lw = matrix(rep(0,4470),nrow = 4470, ncol = 1)
  V = exp(V)
  for(i in 1:4470){
    lw[i,1] = sum(V[i,])
  }
  for(i in 1:4470){
    for(j in 1:10)
      pro[i,j] = (V[i,j])/lw[i,1]
  }
  pro=log(pro)

  dec = dec * pro
  return(-sum(dec))
}
#theta = c(0.1,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5)
#theta = c(-0.5,-0.5,-0.5,-0.5,-0.5,-0.5,-0.5,-0.5,-0.5,-0.5)
theta = c(-0.505,-0.505,-0.505,-0.505,-0.505,-0.505,-0.505,-0.505,-0.505,-0.505)
#theta =  c(-0.51,-0.51,-0.51,-0.51,-0.51,-0.51,-0.51,-0.51,-0.51,-0.51)
result = optim(par = theta,likelihoodf)$par
print(result)


#Exercise 3
t1 = matrix(cbind(hhid,choice),ncol = 2)
colnames(t1) = c("hhid","choice")
t2 = matrix(cbind(hhidd,income),ncol = 2)
colnames(t2) = c("hhid","choice")
hhinc = as.data.frame(merge(t1,t2,by = "hhid",all = T,sort = T))
colnames(hhinc) = c("hhid","choice","income")
rincome = t(matrix(rep(t(hhinc$income),each = 10),nrow = 10,ncol = 4470))
dec = matrix(rep(0,4470*10),nrow = 4470,ncol = 10)
for(i in 1:4470){
  for(j in 1:10){
    if(choice[i] == j){
      dec[i,j] = 1
    }
  }
}
likelihoodf1 <- function(theta){
  z = matrix(rep(0,4470),ncol = 1)
  beta = matrix(rep(theta[1:9],each = 4470),nrow = 4470,ncol = 9)
  beta = matrix(cbind(z,beta),ncol = 10)
  alpha = matrix(rep(theta[10:18],each = 4470),nrow = 4470,ncol = 9)
  alpha = matrix(cbind(z,alpha),ncol = 10)
  V <- rincome*beta + alpha
  pro = matrix(c(rep(0,4470*10)),nrow = 4470,ncol = 10)
  lw = matrix(rep(0,4470),nrow = 4470, ncol = 1)
  
  V = exp(V)
 
  for(i in 1:4470){
    lw[i,1] = sum(V[i,])
  }
  for(i in 1:4470){
    for(j in 1:10)
      pro[i,j] = (V[i,j])/lw[i,1]
  }
  pro=log(pro)
  
  dec = dec * pro
  return(-sum(dec))
}
theta = c(0.01,0.01,0.01,0.01,0.01,0.01,0.01,0.01,0.01,0.01,0.01,0.01,0.01,0.01,0.01,0.01,0.01,0.01)
#theta = c(-0.01,-0.5,0,0,0.5,0,0,0.01,0,-0.5,0,0,0,0,0,0,0,0)
#theta =  c(-0.51,-0.51,-0.51,-0.51,-0.51,-0.51,-0.51,-0.51,-0.51,-0.51)
result1 = optim(par = theta,likelihoodf1)$par
print(result1)

#Exercise 4
#Compute and interpret the marginal effect for the first and second models
#First model
bet = result[1]
alp = result[2:10]
z = c(rep(0,4470))
alp = matrix(rep(result[2:10],each = 4470),nrow=4470,ncol=9)
alp = cbind(z,alp)
V4 <- df0 * bet + alp
pro4 = matrix(c(rep(0,4470*10)),nrow = 4470,ncol = 10)
lw4 = matrix(rep(0,4470),nrow = 4470, ncol = 1)
V4 = exp(V4)
for(i in 1:4470){
  lw4[i,1] = sum(V4[i,])
}
for(i in 1:4470){
  for(j in 1:10)
    pro4[i,j] = (V4[i,j])/lw4[i,1]
}


dec4 = matrix(rep(0,4470*10),nrow = 4470,ncol = 10)
for(i in 1:4470){
  for(j in 1:10){
    if(choice[i] == j){
      dec[i,j] = 1
    }
  }
}

S = matrix(rep(0,100),ncol = 10, nrow = 10)
A = diag(1,10,10)
for(i in 1:4470){
    Temp = matrix(rep(pro4[i,1:10],each = 10),ncol = 10,nrow = 10)
    S = S + t(Temp)*(A-Temp)*bet
}
S = S/4470
colnames(S) = c("AMEP1","AMEP2","AMEP3","AMEP4","AMEP5","AMEP6","AMEP7","AMEP8","AMEP9","AMEP10")
View(S)

#Second Model
bet2 = result1[1:9]
alp2 = result1[10:18]
z = c(rep(0,4470))
bet2 = matrix(rep(result1[1:9],each = 4470),nrow = 4470,ncol = 9)
alp2 = matrix(rep(result1[10:18],each = 4470),nrow=4470,ncol=9)
alp2 = cbind(z,alp2)
bet2 = cbind(z,bet2)
V42 <- rincome*bet2 + alp2
pro42 = matrix(c(rep(0,4470*10)),nrow = 4470,ncol = 10)
lw42 = matrix(rep(0,4470),nrow = 4470, ncol = 1)
V42 = exp(V42)
for(i in 1:4470){
  lw42[i,1] = sum(V42[i,])
}
for(i in 1:4470){
  for(j in 1:10)
    pro42[i,j] = (V42[i,j])/lw42[i,1]
}

S2 = matrix(rep(0,10),ncol = 10, nrow = 1)
be2 = matrix(bet2[1,],ncol = 10, nrow = 1)

for(i in 1:4470){
  Temp1 = matrix(pro42[i,1:10],ncol = 10,nrow = 1)
  betb = matrix(rep(Temp1 %*% t(be2),10),nrow = 1, ncol = 10)
  S2 = S2 + Temp1 * (be2 - betb)
}
S2 = S2/4470
colnames(S2) = c("AMEP1","AMEP2","AMEP3","AMEP4","AMEP5","AMEP6","AMEP7","AMEP8","AMEP9","AMEP10")
View(S2)

#Exercise 5
#mixed logit model
dec = matrix(rep(0,4470*10),nrow = 4470,ncol = 10)
for(i in 1:4470){
  for(j in 1:10){
    if(choice[i] == j){
      dec[i,j] = 1
    }
  }
}
likelihoodf2 <- function(theta){
  beta = theta[1]
  alpha1 = matrix(rep(theta[2:10],each = 4470),nrow = 4470,ncol = 9)
  gama = matrix(rep(theta[11:19],each = 4770),nrow = 4470,ncol = 9)
  alpha2 = matrix(rep(theta[20:28],each = 4470),nrow = 4470,ncol = 9)
  z = c(rep(0,4470))
  alpha1 = matrix(cbind(z,alpha1),nrow=4470,ncol=10)
  alpha2 = matrix(cbind(z,alpha2),nrow = 4470,ncol = 10)
  gama = matrix(cbind(z,gama),nrow = 4470,ncol = 10)
  #====================================================================
  V5 <- df0*beta + alpha1 + rincome*gama + alpha2
  V5 = exp(V5)
  pro5 = matrix(c(rep(0,4470*10)),nrow = 4470,ncol = 10)
  lw5 = matrix(rep(0,4470),nrow = 4470, ncol = 1)
  
  for(i in 1:4470){
    lw5[i,1] = sum(V5[i,])
  }
  for(i in 1:4470){
    for(j in 1:10)
      pro5[i,j] = (V5[i,j])/lw5[i,1]
  }
  pro5=log(pro5)
  
  dec5 = dec * pro5
  return(-sum(dec5))
}
theta = c(-0.51,-0.51,-0.51,-0.51,-0.51,-0.51,-0.51,-0.51,-0.51,-0.51,-0.51,-0.51,-0.51,-0.51,-0.51,-0.51,-0.51,-0.51,-0.51,-0.51,-0.51,-0.51,-0.51,-0.51,-0.51,-0.51,-0.51,-0.51)
#theta =  c(-0.51,-0.51,-0.51,-0.51,-0.51,-0.51,-0.51,-0.51,-0.51,-0.51)
betaf = optim(par = theta,likelihoodf2)$par
print(betaf)

#remove choice 2
apple = matrix(cbind(hhid,choice,ppks,pbbs,pfls,phses,pgens,pimps,psst,ppkt,pflt,phset),ncol = 12,nrow = 4470)
colnames(apple) = c("hhid","choice","ppks","pbbs","pfls","phses","pgens","pimps","psst","ppkt","pflt","phset")
t2 = matrix(cbind(hhidd,income),ncol = 2)
colnames(t2) = c("hhid","choice")
happle = as.matrix(merge(apple,t2,by = "hhid",all = T,sort = T))
happle = matrix(cbind(happle,dec),nrow = 4470,ncol = 23)
colnames(happle) = c("hhid","choice","ppks","pbbs","pfls","phses","pgens","pimps","psst","ppkt","pflt","phset","income","d1","d2","d3","d4","d5","d6","d7","d8","d9","d10")

drophapple = happle[!happle[,2] == 2,]
droppro1 = subset(drophapple,select=c(ppks,pfls,phses,pgens,pimps,psst,ppkt,pflt,phset))
dropinc1 = subset(drophapple,select=c(income))
dropinc = t(matrix(rep(t(dropinc1),each = 9),ncol = 3771,nrow = 9))
dropdec = subset(drophapple,select=c(d1,d3,d4,d5,d6,d7,d8,d9,d10))

fc1 <- function(x){
  x - droppro[,1]
}
droppro = apply(droppro1,2,fc1)

likelihoodf3 <- function(theta){
  beta = theta[1]
  alpha1 = matrix(rep(theta[2:10],each = 3771),nrow = 3771,ncol = 9)
  gama = matrix(rep(theta[11:19],each = 3771),nrow = 3771,ncol = 9)
  alpha2 = matrix(rep(theta[20:27],each = 3771),nrow = 3771,ncol = 9)
  z = c(rep(0,3771))
  alpha1[,1] = z
  alpha2[,1] = z
  V6 <- droppro * beta + alpha1 + dropinc*gama + alpha2
  V6 = exp(V6)
  pro6 = matrix(c(rep(0,3771*9)),nrow = 3771,ncol = 9)
  lw6 = matrix(rep(0,3771),nrow = 3771, ncol = 1)
  for(i in 1:3771){
    lw6[i,1] = sum(V6[i,])
  }
  for(i in 1:3771){
    for(j in 1:9)
      pro6[i,j] = (V6[i,j])/lw6[i,1]
  }
  pro6=log(pro6)
  
  dec6 = dropdec * pro6
  return(-sum(dec6))
}
theta = c(-0.51,-0.51,-0.51,-0.51,-0.51,-0.51,-0.51,-0.51,-0.51,-0.51,-0.51,-0.51,-0.51,-0.51,-0.51,-0.51,-0.51,-0.51,-0.51,-0.51,-0.51,-0.51,-0.51,-0.51,-0.51,-0.51,-0.51)
#theta =  c(-0.51,-0.51,-0.51,-0.51,-0.51,-0.51,-0.51,-0.51,-0.51,-0.51)
betar = optim(par = theta,likelihoodf3)$par

print(betar)

#chi-sq test

MTT = 2 * (likelihoodf2(betaf) - likelihoodf3(betar))
resultchi = chisq.test(abs(betar))
print(resultchi)










