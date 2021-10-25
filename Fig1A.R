## Citaiton: Data associated with:
## Schnyder D, Albano G, Kucharczyk P, Dolder S, Siegrist M, Anderegg M, Pathare G, Hofstetter W, Baron R, Fuster DG.
## Deletion of the sodium/hydrogen exchanger 6 causes low bone volume in adult mice. Bone. 2021 Sep 8;116178. doi: 10.1016/j.bone.2021.116178

##Figure 1A, importing and cleaning the data into an easily workable dataframe.
#importing
library(readr)
X1A <- read_csv("doi_10.5061_dryad.9w0vt4bdt__v4/Figure1/1A.csv")
view(X1A)

#Cleaning up NA columns, moving labels, regrouping.
library(tidyverse)

dat <- subset(X1A, select =  -c(5,6,10,11))
view(dat)
x <- unlist(rep(dat[,1],6))
y <- unlist(c(dat[,2],dat[,3],dat[,4],dat[,5],dat[,6],dat[,7]))
q <- rep("-RANKL",36)
p <- rep("+RANKL",36)
z<-unlist(c(q,p))

dat1A<-data.frame(y)
dat1A <- as.data.frame(apply(dat1A, 2, as.numeric))
dat1A$Labels <-x
dat1A$Protocol <-z
view(dat1A)

## Means (±RANKL), diff, se, t-stat, pval, qval.
Labels<-dat[,1]%>%unlist
t <- vector("numeric", 0)
mean_minus<- vector("numeric", 0)
mean_plus<- vector("numeric", 0)
diffs<- vector("numeric", 0)
ses<- vector("numeric", 0)
pvals<- vector("numeric", 0)
dfs<- vector("numeric", 0)

for (label in Labels){
  tmp_minus<-filter(dat1A, Labels==label & Protocol=="-RANKL")$y
  tmp_plus<-filter(dat1A, Labels==label & Protocol=="+RANKL")$y
  mean_minus[label]<-mean(tmp_minus)
  mean_plus[label]<-mean(tmp_plus)
  diffs[label]<-mean(tmp_plus)-mean(tmp_minus)
  ses[label] <-t.test(tmp_minus,tmp_plus,var.equal=TRUE)$stderr
  t[label]<-t.test(tmp_minus,tmp_plus, var.equal=TRUE)$statistic
  pvals[label]<-t.test(tmp_minus,tmp_plus, var.equal=TRUE)$p.value
  dfs[label]<-t.test(tmp_minus,tmp_plus, var.equal=TRUE)$parameter
}

summ1A<-data.frame(mean_minus,mean_plus,diffs,ses,t,dfs,pvals)
view(summ1A)


##qvalue calculations - having issues with the packages
if (!requireNamespace("BiocManager", quietly = TRUE))
  install.packages("BiocManager")

BiocManager::install("qvalue")
library(qvalue)
#This isn't working for some reason. The rest of the table is now the same as in their data!

##PLOTS

library(ggplot2)

q<-ggplot(dat1A ,aes(x=Protocol, y=y))+
  geom_boxplot()+
  #geom_point(aes(color=Protocol))+
  #stat_summary(fun = "mean", geom = "point")+
  #stat_summary(fun.data = "mean_se", geom = "errorbar") +
  xlab(NULL)+
  ylab("Target/GAPDH")+
  facet_grid(~Labels)
q

