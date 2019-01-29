sink("Outcome.pdf")
d=read.csv("datstu.csv")
e=read.csv("datsss.csv")
f=read.csv("datjss.csv")
install.packages("dplyr")
library(dplyr)
#Exercise 1
#Number of students
number.students=length(d$X)
print("number of students is")
print(number.students)
#Number of schools
d.school=c(d$schoolcode1,d$schoolcode2,d$schoolcode3,d$schoolcode4,d$schoolcode5,d$schoolcode6)
f.school=as.factor(d.school)
number.school=length(levels(f.school))
print("number of schools is")
print(number.school)
#Number of programs
d.program1=matrix(d$choicepgm1,ncol=1)
d.program2=matrix(d$choicepgm2,ncol=1)
d.program3=matrix(d$choicepgm3,ncol=1)
d.program4=matrix(d$choicepgm4,ncol=1)
d.program5=matrix(d$choicepgm5,ncol=1)
d.program6=matrix(d$choicepgm6,ncol=1)
d.program=rbind(d.program1,d.program2,d.program3,d.program4,d.program5,d.program6)
number.program=nrow(unique(na.omit(d.program)))
print("number of programs is")
print(number.program)

#Number of choices
d.choice=cbind(d.school,d.program)
d.choice[d.choice==""]=NA
d.choice=na.omit(d.choice)
d.choice=unique.data.frame(d.choice)
number.choices = dim(d.choice)[1]
print("number of choices is ")
print(number.choices)

#Number of Missing test score
score=matrix(d$score,ncol=1)
number.na.score=sum(is.na(score))
print("number of missing test score is")
print(number.na.score)

#Apply to the same school
stucode=matrix(d$X,ncol=1)
stucoderep6=rbind(stucode,stucode,stucode,stucode,stucode,stucode)
sc1=matrix(d$schoolcode1,ncol=1)
sc2=matrix(d$schoolcode2,ncol=1)
sc3=matrix(d$schoolcode3,ncol=1)
sc4=matrix(d$schoolcode4,ncol=1)
sc5=matrix(d$schoolcode5,ncol=1)
sc6=matrix(d$schoolcode6,ncol=1)
sccode=rbind(sc1,sc2,sc3,sc4,sc5,sc6)
application=cbind(stucoderep6,sccode)
application=na.omit(application)
a.factor=as.factor(application[,2])
table=as.data.frame(table(a.factor))
names(table)=c("schoolcode","frequency")
View(table)

#Apply to less than 6 schools
fullchoices=data.frame(s1=d$schoolcode1,s2=d$schoolcode2,s3=d$schoolcode3,s4=d$schoolcode4,s5=d$schoolcode5,s6=d$schoolcode6)
less6choices=na.omit(fullchoices)
n_fullchoices=dim(less6choices)[1]
n_allchoices=dim(fullchoices)[1]
n_less6choices=n_allchoices-n_fullchoices
print("number of students applying to less than 6 schools is")
print(n_less6choices)

#Exercise 2
sss=data.frame(schoolname=e$schoolname,schoolcode=e$schoolcode,sssdistrict=e$sssdistrict,slong=e$ssslong,slat=e$ssslat)
sss=na.omit(sss)
sss=sss[!duplicated(sss),]
sssl=sss[!duplicated(sss$schoolcode),]
stu=data.frame(X=d$X,score=d$score,s1=d$schoolcode1,s2=d$schoolcode2,s3=d$schoolcode3,s4=d$schoolcode4,s5=d$schoolcode5,s6=d$schoolcode6,c1=d$choicepgm1,c2=d$choicepgm2,c3=d$choicepgm3,c4=d$choicepgm4,c5=d$choicepgm5,c6=d$choicepgm6,d=d$jssdistrict,r=d$rankplace)
admitted1=subset(stu,r==1,select=c(X,score,s1,c1))
admitted2=subset(stu,r==2,select=c(X,score,s2,c2))
admitted3=subset(stu,r==3,select=c(X,score,s3,c3))
admitted4=subset(stu,r==4,select=c(X,score,s4,c4))
admitted5=subset(stu,r==5,select=c(X,score,s5,c5))
admitted6=subset(stu,r==6,select=c(X,score,s6,c6))
colnames(admitted1)=c("X","score","schoolcode","program")
colnames(admitted2)=c("X","score","schoolcode","program")
colnames(admitted3)=c("X","score","schoolcode","program")
colnames(admitted4)=c("X","score","schoolcode","program")
colnames(admitted5)=c("X","score","schoolcode","program")
colnames(admitted6)=c("X","score","schoolcode","program")
admittedall=rbind(admitted1,admitted2,admitted3,admitted4,admitted5,admitted6)
colnames(admittedall)=c("X","score","schoolcode","program")
admitted_min=aggregate(score~schoolcode+program,admittedall,FUN=min)
cutoff=as.data.frame(admitted_min)
colnames(cutoff)=c("schoolcode","program","cutoff")
admitted_mean=aggregate(score~schoolcode+program,admittedall,FUN=mean)
quality=as.data.frame(admitted_mean)
colnames(quality)=c("schoolcode","program","quality")
admitted_size=table(admittedall[,3:4])
size=as.data.frame(admitted_size)
colnames(size)=c("schoolcode","program","size")
admitted=cbind(quality,cutoff=cutoff[,3])
admitted=admitted %>% left_join(sssl,by=c("schoolcode"))
admitted=merge(admitted,size,by=c("schoolcode","program"),all.x = T,sort=T)
View(admitted)

#Exercis3
jscdis=data.frame(f[,2:4])
colnames(jscdis)=c("jdist","jlong","jlat")
sscdis=data.frame(e[,3:6])
colnames(sscdis)=c("schoolcode","sdist","slong","slat")
sscdis1 = sscdis %>% filter(!is.na(slong)&!is.na(slat))
ad1=subset(stu,r==1,select=c(s1,d))
ad2=subset(stu,r==2,select=c(s2,d))
ad3=subset(stu,r==3,select=c(s3,d))
ad4=subset(stu,r==4,select=c(s4,d))
ad5=subset(stu,r==5,select=c(s5,d))
ad6=subset(stu,r==6,select=c(s6,d))
colnames(ad1)=c("schoolcode","jdist")
colnames(ad2)=c("schoolcode","jdist")
colnames(ad3)=c("schoolcode","jdist")
colnames(ad4)=c("schoolcode","jdist")
colnames(ad5)=c("schoolcode","jdist")
colnames(ad6)=c("schoolcode","jdist")
ad=rbind(ad1,ad2,ad3,ad4,ad5,ad6)
colnames(ad)=c("schoolcode","jdist")
ad=na.omit(as.data.frame(ad))
ad=ad[!duplicated(ad),]
#ad_s=merge(ad,sscdis,by="schoolcode",all=TRUE,sort=TRUE)
#ad=merge(ad_s,jscdis,by="jdist",all=TRUE,sort=TRUE)
#ad=na.omit(ad)
#ad=ad[!duplicated(ad),]
ad_s=ad %>% left_join(sscdis1,by = "schoolcode")
ad=ad_s %>% left_join(jscdis,by = "jdist")
distance_sj=as.data.frame(sqrt((69.172*(ad[,4]-ad[,6])*cos(ad[,7]/57.3))^2+(69.172*(ad[,5]-ad[,7]))^2))
distance=cbind(ad[,2],ad[,3],ad[,4:7],distance_sj)
colnames(distance)=c("jssdistrict","sssdistrict","slong","slat","jlong","jlat","distance")
distance=unique.data.frame(distance)
View(distance)

#Exercise4
rank1=as.data.frame(d[c(1,2,5,11,17)])
rank2=as.data.frame(d[c(1,2,6,12,17)])
rank3=as.data.frame(d[c(1,2,7,13,17)])
rank4=as.data.frame(d[c(1,2,8,14,17)])
rank5=as.data.frame(d[c(1,2,9,15,17)])
rank6=as.data.frame(d[c(1,2,10,16,17)])
colnames(rank1)=c("X","score","schoolcode","program","jssdistrict")
colnames(rank2)=c("X","score","schoolcode","program","jssdistrict")
colnames(rank3)=c("X","score","schoolcode","program","jssdistrict")
colnames(rank4)=c("X","score","schoolcode","program","jssdistrict")
colnames(rank5)=c("X","score","schoolcode","program","jssdistrict")
colnames(rank6)=c("X","score","schoolcode","program","jssdistrict")
rank1[rank1==""] = NA
rank2[rank2==""] = NA
rank3[rank3==""] = NA
rank4[rank4==""] = NA
rank5[rank5==""] = NA
rank6[rank6==""] = NA
#rank1
rank_1=rank1 %>% left_join(admitted,by=c("schoolcode","program"))
rank.1=rank_1 %>% left_join(distance,by=c("sssdistrict","jssdistrict"))
rank_1=rank.1[,c(7,6,17)]
avrank_1=colMeans(rank_1,na.rm=T)
avrank_1
sd_cutoff1=sd(rank_1[,1],na.rm=T)
sd_cutoff1
sd_quality1=sd(rank_1[,2],na.rm=T)
sd_quality1
sd_distance1=sd(rank_1[,1],na.rm=T)
sd_distance1
#rank2
rank_2=rank2 %>% left_join(admitted,by=c("schoolcode","program"))
rank.2=rank_2 %>% left_join(distance,by=c("sssdistrict","jssdistrict"))
rank_2=rank.2[,c(7,6,17)]
avrank_2=colMeans(rank_2,na.rm=T)
avrank_2
sd_cutoff2=sd(rank_2[,1],na.rm=T)
sd_cutoff2
sd_quality2=sd(rank_2[,2],na.rm=T)
sd_quality2
sd_distance2=sd(rank_2[,1],na.rm=T)
sd_distance2
#rank3
rank_3=rank3 %>% left_join(admitted,by=c("schoolcode","program"))
rank.3=rank_3 %>% left_join(distance,by=c("sssdistrict","jssdistrict"))
rank_3=rank.3[,c(7,6,17)]
avrank_3=colMeans(rank_3,na.rm=T)
avrank_3
sd_cutoff3=sd(rank_3[,1],na.rm=T)
sd_cutoff3
sd_quality3=sd(rank_3[,2],na.rm=T)
sd_quality3
sd_distance3=sd(rank_3[,1],na.rm=T)
sd_distance3
#rank4
rank_4=rank4 %>% left_join(admitted,by=c("schoolcode","program"))
rank.4=rank_4 %>% left_join(distance,by=c("sssdistrict","jssdistrict"))
rank_4=rank.4[,c(7,6,17)]
avrank_4=colMeans(rank_4,na.rm=T)
avrank_4
sd_cutoff4=sd(rank_4[,1],na.rm=T)
sd_cutoff4
sd_quality4=sd(rank_4[,2],na.rm=T)
sd_quality4
sd_distance4=sd(rank_4[,1],na.rm=T)
sd_distance4
#rank5
rank_5=rank5 %>% left_join(admitted,by=c("schoolcode","program"))
rank.5=rank_5 %>% left_join(distance,by=c("sssdistrict","jssdistrict"))
rank_5=rank.5[,c(7,6,17)]
avrank_5=colMeans(rank_5,na.rm=T)
avrank_5
sd_cutoff5=sd(rank_5[,1],na.rm=T)
sd_cutoff5
sd_quality5=sd(rank_5[,2],na.rm=T)
sd_quality5
sd_distance5=sd(rank_5[,1],na.rm=T)
sd_distance5
#rank6
rank_6=rank6 %>% left_join(admitted,by=c("schoolcode","program"))
rank.6=rank_6 %>% left_join(distance,by=c("sssdistrict","jssdistrict"))
rank_6=rank.6[,c(7,6,17)]
avrank_6=colMeans(rank_6,na.rm=T)
avrank_6
sd_cutoff6=sd(rank_6[,1],na.rm=T)
sd_cutoff6
sd_quality6=sd(rank_6[,2],na.rm=T)
sd_quality6
sd_distance6=sd(rank_6[,1],na.rm=T)
sd_distance6

#Redo by student test score quantiles
admittedall_quantile=admittedall %>% left_join(admitted,by=c("schoolcode","program"))
admittedall_quantile=admittedall_quantile %>% left_join(d,by=c("X"))
admittedall_quantile=admittedall_quantile %>% left_join(distance,by = c("sssdistrict","jssdistrict"))
admittedall_quantile=admittedall_quantile[,c(1:6,33)]
colnames(admittedall_quantile)=c("X","score","schoolcode","program","quality","cutoff","distance")
admittedall_quantile=admittedall_quantile[order(admittedall_quantile$score),]
ad_q1=admittedall_quantile[1:34806,c(5,6,7)]
ad_q2=admittedall_quantile[34807:(34806*2),c(5,6,7)]
ad_q3=admittedall_quantile[(34806*2+1):(34806*3),c(5,6,7)]
ad_q4=admittedall_quantile[(34806*3+1):(34806*4),c(5,6,7)]
#q1
av_ad1=colMeans(ad_q1, na.rm = TRUE) #take average of cutoff, quality and distance of group 1
av_ad1
sd_ad1_quality=sd(ad_q1[,1],na.rm = TRUE) #take sd of cutoff of group 1
sd_ad1_quality
sd_ad1_cutoff=sd(ad_q1[,2],na.rm = TRUE) #take sd of quality of group 1
sd_ad1_cutoff
sd_ad1_distance=sd(ad_q1[,3],na.rm = TRUE) #take sd of distance of group 1
sd_ad1_distance
#q2
av_ad2=colMeans(ad_q2, na.rm = TRUE) #take average of cutoff, quality and distance of group 1
av_ad2
sd_ad2_quality=sd(ad_q2[,1],na.rm = TRUE) #take sd of cutoff of group 1
sd_ad2_quality
sd_ad2_cutoff=sd(ad_q2[,2],na.rm = TRUE) #take sd of quality of group 1
sd_ad2_cutoff
sd_ad2_distance=sd(ad_q2[,3],na.rm = TRUE) #take sd of distance of group 1
sd_ad2_distance
#q3
av_ad3=colMeans(ad_q3, na.rm = TRUE) #take average of cutoff, quality and distance of group 1
av_ad3
sd_ad3_quality=sd(ad_q3[,1],na.rm = TRUE) #take sd of cutoff of group 3
sd_ad3_quality
sd_ad3_cutoff=sd(ad_q3[,2],na.rm = TRUE) #take sd of quality of group 3
sd_ad3_cutoff
sd_ad3_distance=sd(ad_q3[,3],na.rm = TRUE) #take sd of distance of group 3
sd_ad3_distance
#q4
av_ad4=colMeans(ad_q4, na.rm = TRUE) #take average of cutoff, quality and distance of group 4
av_ad4
sd_ad4_quality=sd(ad_q4[,1],na.rm = TRUE) #take sd of cutoff of group 4
sd_ad4_quality
sd_ad4_cutoff=sd(ad_q4[,2],na.rm = TRUE) #take sd of quality of group 4
sd_ad4_cutoff
sd_ad4_distance=sd(ad_q4[,3],na.rm = TRUE) #take sd of distance of group 4
sd_ad4_distance

#Exercise 5
cutofflist=as.data.frame(admitted[,c(1,2,4)])
cutofflist=cutofflist[order(cutofflist$cutoff),]
cutoff_d1=as.data.frame(cutofflist[1:230,])
cutoff_d2=as.data.frame(cutofflist[(230+1):(2*230),])
cutoff_d3=as.data.frame(cutofflist[(2*230+1):(3*230),])
cutoff_d4=as.data.frame(cutofflist[(3*230+1):(4*230),])
cutoff_d5=as.data.frame(cutofflist[(4*230+1):(5*230),])
cutoff_d6=as.data.frame(cutofflist[(5*230+1):(6*230),])
cutoff_d7=as.data.frame(cutofflist[(6*230+1):(7*230),])
cutoff_d8=as.data.frame(cutofflist[(7*230+1):(8*230),])
cutoff_d9=as.data.frame(cutofflist[(8*230+1):(9*230),])
cutoff_d10=as.data.frame(cutofflist[(9*230+1):2300,])
#d1
n=data.frame(dnumber=1)
n=as.vector(n[rep(1:nrow(n),each=230),])
cutoff_d1=cbind(cutoff_d1,n)
#d2
n=data.frame(dnumber=2)
n=as.vector(n[rep(1:nrow(n),each=230),])
cutoff_d2=cbind(cutoff_d2,n)
#d3
n=data.frame(dnumber=3)
n=as.vector(n[rep(1:nrow(n),each=230),])
cutoff_d3=cbind(cutoff_d3,n)
#d4
n=data.frame(dnumber=4)
n=as.vector(n[rep(1:nrow(n),each=230),])
cutoff_d4=cbind(cutoff_d4,n)
#d5
n=data.frame(dnumber=5)
n=as.vector(n[rep(1:nrow(n),each=230),])
cutoff_d5=cbind(cutoff_d5,n)
#d6
n=data.frame(dnumber=6)
n=as.vector(n[rep(1:nrow(n),each=230),])
cutoff_d6=cbind(cutoff_d6,n)
#d7
n=data.frame(dnumber=7)
n=as.vector(n[rep(1:nrow(n),each=230),])
cutoff_d7=cbind(cutoff_d7,n)
#d8
n=data.frame(dnumber=8)
n=as.vector(n[rep(1:nrow(n),each=230),])
cutoff_d8=cbind(cutoff_d8,n)
#d9
n=data.frame(dnumber=9)
n=as.vector(n[rep(1:nrow(n),each=230),])
cutoff_d9=cbind(cutoff_d9,n)
#d10
n=data.frame(dnumber=10)
n=as.vector(n[rep(1:nrow(n),each=230),])
cutoff_d10=cbind(cutoff_d10,n)

cutofflist=cbind(cutoff_d1,cutoff_d2,cutoff_d3,cutoff_d4,cutoff_d5,cutoff_d6,cutoff_d7,cutoff_d8,cutoff_d9,cutoff_d10)
colnames(cutofflist)=c("schoolcode","program","cutoff","groupnumber")
stuall=rbind(rank1,rank2,rank3,rank4,rank5,rank6)
stuall=stuall[,c(1,3,4)]
stuall[stuall==""]=NA
stuall=na.omit(stuall)
stuall_gn=merge(stuall,cutofflist,by=c("schoolcode","program"),all.x = T,sort = T)
stuall_gn=stuall_gn[,c(3,5)]
stuall_gn=unique(stuall_gn)
numberofgroups=as.data.frame(table(stuall_gn[,1]))
colnames(numberofgroups)=c("X","number of groups")
View(numberofgroups)

#Redo
qualitylist=as.data.frame(admitted[,c(1,2,3)])
qualitylist=qualitylist[order(qualitylist$quality),]
quality_q1=qualitylist[1:575,]
quality_q2=qualitylist[(575+1):(575*2),]
quality_q3=qualitylist[(2*575+1):(575*3),]
quality_q4=qualitylist[(3*575+1):(575*4),]
#q1
n=data.frame(qnumber=1)
n=as.vector(n[rep(1:nrow(n),each=575),])
quality_q1=cbind(quality_q1,n)
#q2
n=data.frame(qnumber=2)
n=as.vector(n[rep(1:nrow(n),each=575),])
quality_q2=cbind(quality_q2,n)
#q3
n=data.frame(qnumber=3)
n=as.vector(n[rep(1:nrow(n),each=575),])
quality_q3=cbind(quality_q3,n)
#q4
n=data.frame(qnumber=4)
n=as.vector(n[rep(1:nrow(n),each=575),])
quality_q4=cbind(quality_q4,n)
qualitylist=cbind(quality_q1,quality_q2,quality_q3,quality_q4)
colnames(qualitylist)=c("schoolcode","program","quality","quantilenumber")
schall=rbind(rank1,rank2,rank3,rank4,rank5,rank6)
schall=schall[,c(1,3,4)]
schall[schall==""]=NA
schall=na.omit(schall)
schall_qn=merge(schall,qualitylist,by=c("schoolcode","program"),all.x = T,sort=T)
schall_qn=schall_qn[,c(3,5)]
schall_qn=unique(schall_qn)
numberofgroups1=as.data.frame(table(schall_qn[,1]))
colnames(numberofgroups1)=c("X","number of groups")
View(numberofgroups1)
sink()