## Citaiton: Data associated with:
## Schnyder D, Albano G, Kucharczyk P, Dolder S, Siegrist M, Anderegg M, Pathare G, Hofstetter W, Baron R, Fuster DG.
## Deletion of the sodium/hydrogen exchanger 6 causes low bone volume in adult mice. Bone. 2021 Sep 8;116178. doi: 10.1016/j.bone.2021.116178

## First: import data and clean it up

library(readr)
X8A <- read_csv("doi_10.5061_dryad.9w0vt4bdt__v4/Fig8Table1/8A.csv")
View(X8A)

library(tidyverse)

#removing NA entries and relabelling
dat <- subset(X8A, select =  -c(10:13))
protocol <- dat[0,]
L3<- unlist(dat[1,])
L4 <- unlist(dat[2,])
L5 <- unlist(dat[3,])
x <- rep("KO",8)
y <- rep("WT",12)
z <- c(x,y)
z

#Making entries numeric
dat<- data.frame(L3,L4,L5)
view(dat)
sapply(dat,class)
dat_num <- as.data.frame(apply(dat, 2, as.numeric))

#Removing row storing column names
dat_num<-dat_num[-1,]
q<-c(0:(length(dat_num$L3)-1))
rownames(dat_num)<-q

#Adding in the row of labels (which were not supposed to be numeric)
dat_num$z <- z
dat_num

## Means, t-stat, pval
## L3
controlL3 <- filter(dat_num, z=="KO") %>% select(L3) %>% unlist
treatmentL3 <- filter(dat_num, z=="WT") %>% select(L3) %>% unlist

diffL3 <- mean(treatmentL3)-mean(controlL3)
seL3<- sqrt(var(controlL3)/length(controlL3) + var(treatmentL3)/length(treatmentL3))

##L4

controlL4 <- filter(dat_num, z=="KO") %>% select(L4) %>% unlist
treatmentL4 <- filter(dat_num, z=="WT") %>% select(L4) %>% unlist

diffL4 <- mean(treatmentL4)-mean(controlL4)
seL4<- sqrt(var(controlL4)/length(controlL4) + var(treatmentL4)/length(treatmentL4))

##L5
controlL5 <- filter(dat_num, z=="KO") %>% select(L5) %>% unlist
treatmentL5 <- filter(dat_num, z=="WT") %>% select(L5) %>% unlist

diffL5 <- mean(treatmentL5)-mean(controlL5)
seL5<- sqrt(var(controlL5)/length(controlL5) + var(treatmentL5)/length(treatmentL5))

##complete report

treatmentmeans<-c(mean(treatmentL3),mean(treatmentL4),mean(treatmentL5))
controlmeans<-c(mean(controlL3),mean(controlL4),mean(controlL5))
diffs<-c(diffL3,diffL4,diffL5)
ses<-c(seL3,seL4,seL5)
tvals<-c(t.test(treatmentL3, controlL3, var.equal=TRUE)$statistic,t.test(treatmentL4, controlL4, var.equal=TRUE)$statistic,t.test(treatmentL5, controlL5,var.equal=TRUE)$statistic)
pvals<-c(t.test(treatmentL3, controlL3, var.equal=TRUE)$p.value,t.test(treatmentL4, controlL4, var.equal=TRUE)$p.value,t.test(treatmentL5, controlL5, var.equal=TRUE)$p.value)
report<-data.frame(controlmeans,treatmentmeans,diffs,ses,tvals,pvals)
colnames(report) <- c("Mean KO", "Mean WT", "diff", "se", "tval", "pval")
rownames(report)<-c("L3","L4","L5")
report

## Testing for interaction

#2 way ANOVA
#Setup dat_num where column names are now a variable - so we have columns Weight, L, z.
x<-rep("L3",20)
y<-rep("L4",20)
w<-rep("L5",20)
L<-c(x,y,w)
Z<-rep(z,3)
Weight<-c(dat_num[,1],dat_num[,2],dat_num[,3])
dat_an<-data.frame(Weight,L,Z)

#Doing the test and recording it in a variable
anova2<-aov(Weight~as.factor(L)*as.factor(Z),data=dat_an)

#plotting the residuals to check by eye they're normally distributed
res<-anova2$residuals 
hist(res,main="Histogram of residuals",xlab="Residuals")

#Checking the equality of variances
library(car)
leveneTest(Weight~as.factor(L)*as.factor(Z),data=dat_an)

#Now looking at the output
summary(anova2)

#expanding that using posthoc tests (Tukey)
TukeyHSD(anova2)

## plotting figure 8A
#We're going to use ggplot2.
library(ggplot2)

q<-ggplot(dat_an,aes(x=Z, y=Weight))+
  geom_point(aes(color=Z))+
  stat_summary(fun = "mean", geom = "point")+
  stat_summary(fun.data = "mean_se", geom = "errorbar") +
  xlab(NULL)+
  ylab("BV/TV (%)")+
  facet_grid(~L)
q