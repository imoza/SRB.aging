rm(list=ls())

## input the results obtained from the file "Simu.project.r"
## preparing the data for figures and tables

source("Functions.r")

yrs <- seq(1950, 2095, by=5)
yrs80 <- yrs[yrs>=1980]



##                OUTPUT
## ----------------------------------------
## birth
## 1. births, boys and girls

## reference level=105
bth.105 <- read.csv("./proj.out/birth.105.csv", head=T)%>%
    filter(year>=1980 & country%in%srb.name)

f.105 <- read.csv("./proj.out/popf.105.csv", head=T)%>%
    filter(country%in%srb.name)%>%get.ind(x1=15, x2=50, srb.name)


## set the threshold age for older people
x.old <- 65
tot.105 <- read.csv("./proj.out/poptot.105.csv", head=T)%>%
    filter(country%in%srb.name)%>%get.ind(x1=20, x2=x.old, srb.name)



bth.105.by.y <- apply(as.matrix(yrs80), 1, function(x) sum(filter(bth.105, year==x)$both.sex)/1000)
bth.105.boy.by.y <- apply(as.matrix(yrs80), 1, function(x) sum(filter(bth.105, year==x)$boy)/1000)
bth.105.girl.by.y <- apply(as.matrix(yrs80), 1, function(x) sum(filter(bth.105, year==x)$girl)/1000)

## Table
cbind(year=yrs80, both.sex=bth.105.by.y, girl=bth.105.girl.by.y, boy=bth.105.boy.by.y)%>%write.csv("./data/table.total.birth.csv", row.names=F)


## Chas'e estimated SRB continacated with the WPP2019's predicted SRB
bth.bmj <- read.csv("./proj.out/birth.bmj.csv", head=T)%>%
    filter(year>=1980 & country%in%srb.name)

f.bmj <- read.csv("./proj.out/popf.bmj.csv", head=T)%>%
    filter(country%in%srb.name)%>%get.ind(x1=15, x2=50, srb.name)

tot.bmj <- read.csv("./proj.out/poptot.bmj.csv", head=T)%>%
    filter(country%in%srb.name)%>%get.ind(x1=20, x2=x.old, srb.name)


bth.bmj.by.y <- apply(as.matrix(yrs80), 1, function(x) sum(filter(bth.bmj, year==x)$both.sex)/1000)
bth.bmj.boy.by.y <- apply(as.matrix(yrs80), 1, function(x) sum(filter(bth.bmj, year==x)$boy)/1000)
bth.bmj.girl.by.y <- apply(as.matrix(yrs80), 1, function(x) sum(filter(bth.bmj, year==x)$girl)/1000)

cbind(year=yrs80, both.sex=bth.bmj.by.y, girl=bth.bmj.girl.by.y, boy=bth.bmj.boy.by.y)%>%write.csv("./data/.table.total.birth.bmj.csv", row.names=F)



## The number of births estimated SRB vs SRB=105
## reference level is 105
bth.to.105 <- cbind(bth.bmj[, c(1:3)], bth.bmj[,-c(1:3)]-bth.105[,-c(1:3)], comp="diff.105")

bth.tot.105 <- cbind(srb.name, as.data.frame(t(apply(as.matrix(srb.code), 1, function(i)
    filter(bth.105, country_code==i)%>%select(c(4:6))%>%colSums()))))
bth.tot.diff.105 <- cbind(srb.name, as.data.frame(t(apply(as.matrix(srb.code), 1, function(i)
    filter(bth.to.105, country_code==i)%>%select(c(4:6))%>%colSums()))))

bth.rev.diff.105 <- cbind(srb.name, round(bth.tot.diff.105[,-1]/bth.tot.105[,-1]*100,3), comp="diff.105")

## relative difference in the number of births 
o.rev <- order(bth.rev.diff.105$girl)
bth.rev.diff.105.rank <- bth.rev.diff.105[o.rev, ]
bth.rev.diff.105.rank$rank <- n.ct:1

o.rev <- order(bth.rev.diff.105$girl)
bth.rev.diff.bar <- reshape2::melt(bth.rev.diff.105.rank[,c(1, 3, 4)], id.var="srb.name")
names(bth.rev.diff.bar)[2] <- "Sex"
bth.rev.diff.bar$Sex <- recode(bth.rev.diff.bar$Sex, "girl"="Female", "boy"="Male")

bth.rev.diff.bar$srb.name <- factor(bth.rev.diff.bar$srb.name,
                                    levels=rev(bth.rev.diff.bar$srb.name[1:12]))
