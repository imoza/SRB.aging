
## contour of the trajectories of population aging 


rm(list=ls())

library(metR)
library(tidyverse)
library(RColorBrewer)
library(demogR)
library(ggrepel)

simu.srb <- function(srb, tfr, asfr0, bth.posi, old.posi, lab.posi,
                     n.x, n.t,
                     pxf, pxm, W0, M0){
    
    asfr0 <- asfr0/sum(asfr0)

    mx.f <- asfr0*tfr*(1/(1+srb))
    mx.m <- asfr0*tfr*(srb/(1+srb))

    ## female
    A <- matrix(0, nrow=n.x, ncol=n.x)

    A[1,bth.posi] <- mx.f
    A[row(A) == col(A) + 1] <- pxf

    proj.w <- project.leslie(A, W0, tmax=50)
    
    ## male
    ## the number of male births
    boy <- colSums(mx.m*proj.w)

    Am <- matrix(0, nrow=n.x, ncol=n.x) 
    Am[row(Am) == col(Am) + 1] <- pxm

    proj.m <- matrix(0, nrow=n.x, ncol=n.t+1)
    proj.m[,1] <- M0
    pop <- M0
    for(j in 1:n.t){
        pop <- Am %*% pop
        pop[1] <- boy[j]
        proj.m[,j+1] <- pop
    }

    WM <- proj.w+proj.m
    
    p65 <- colSums(WM[old.posi,])/colSums(WM)*100
    oadr <- colSums(WM[old.posi,])/colSums(WM[lab.posi,])*100

    return(c(p65[length(p65)], oadr[length(oadr)]))
}


## ==================== initial

age <- seq(0, 100, by=5)
n.x <- length(age)

bth.age <- seq(15, 45, by=5)
bth.posi <- match(bth.age, age)

lab.age <- seq(20, 60, by=5)
lab.posi <- match(lab.age, age)

old.age <- seq(65, 100, by=5)
old.posi <- match(old.age, age)

y.pick <- which(2020==seq(1950, 2095, by=5))


## ==================== fertility

yr <- 9
n.t <- 50

## contour
n.srb <- 80
n.tfr <- 80

srbs <- seq(1.03, 1.2, length=n.srb)
tfrs <- seq(0.9, 6, length=n.tfr)

surf.ctry <- NULL
for(j in 1:12){
    gg <- expand.grid(x=srbs, y=tfrs)
    pxf <- Lxf[[j]][-1,y.pick]/Lxf[[j]][-nrow(Lxf[[j]]),y.pick]
    pxm <- Lxm[[j]][-1,y.pick]/Lxm[[j]][-nrow(Lxm[[j]]),y.pick]
    asfr0 <- asfr[[j]][,y.pick]/sum(asfr[[j]][,y.pick])

    for(i in 1:nrow(gg)){
        tmp <- simu.srb(gg$x[i], gg$y[i], asfr0,
                        bth.posi, old.posi, lab.posi,
                        n.x, n.t,
                        pxf, pxm,
                        rep(1000000, n.x), rep(1000000, n.x))
        gg$p65[i] <- tmp[1]
        gg$oadr[i] <- tmp[2]
    }
    ## surf.ctry[[j]] <- gg

    surf.ctry <- rbind(surf.ctry, cbind(gg, country=srb.name[j]))
}

## real trajectory
st.ctry <- NULL
for(j in 1:12){
    
    tfr.j <- colSums(asfr[[j]])*5
    srb.j <- unlist(filter(bmj.wide, country_code==srb.code[j])%>%select(-c(1,2)))

    pxf <- Lxf[[j]][-1,y.pick]/Lxf[[j]][-nrow(Lxf[[j]]),y.pick]
    pxm <- Lxm[[j]][-1,y.pick]/Lxm[[j]][-nrow(Lxm[[j]]),y.pick]
    asfr0 <- asfr[[j]][,y.pick]/sum(asfr[[j]][,y.pick])
    
    real.st <- NULL
    for(i in 1:length(tfr.j)){
        tmp <- simu.srb(srb.j[i], tfr.j[i], asfr0, 
                        bth.posi, old.posi, lab.posi,
                        n.x, n.t,
                        pxf, pxm,
                        rep(1e6, n.x), rep(1e6, n.x))
        
        real.st <- rbind(real.st, c(srb.j[i], tfr.j[i], tmp))
    }
    real.st <- data.frame(real.st)
    real.st$year <- yrs
    real.st <- real.st[,c(5, 1:4)]
    names(real.st) <- c("year", "SRB", "TFR", "p65", "oadr")
    st.ctry <- rbind(st.ctry, cbind(real.st, country=srb.name[j]))
}

surf.ctry$country <- factor(surf.ctry$country, level=srb.name)
st.ctry$country <- factor(st.ctry$country, level=srb.name)

write.csv(surf.ctry, "./data/surf.ctry.csv", row.names=F)
write.csv(st.ctry, "./data/st.ctry.csv", row.names=F)
