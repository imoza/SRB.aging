## library(data.table)
library(ggpubr)
library(tidyverse)
library(bayesPop)
library(wpp2019)
library(viridis)
library(scales)
library(reshape2)

## projection with TFR estimated by the WPP 
proj.srb <- function(srb0, popf, popm, asfr,
                     Lxf, Lxm, Txf, Txm, migf, migm,
                     srb.ver,
                     srb.name, srb.code,
                     zero.mig=FALSE,
                     yrs=seq(1955, 2100, by=5),
                     age=seq(0, 100, by=5),
                     outdir="./proj.out", change.asfr=1){

    ## srb.ver: specify the version of senario
    
    ## age <- seq(0, 85, by=5)
    n <- length(age)

    bth.age <- seq(15, 45, by=5)
    bth.posi <- match(bth.age, age)

    n.ct <- length(srb.code)
    n.yr <- length(yrs)

    outf <- outm <- outb <- outbth <- NULL
    for(i in 1:n.ct){
        ## the ith country
        srb.ct <- filter(srb0, country_code==srb.code[i])%>%select(-c(1:2))

        ## -------------------- survival
        ## female
        Lxf.mat <- data.frame(Lxf[[i]])
        pxf.mat <- Lxf.mat[-1,]/Lxf.mat[-nrow(Lxf.mat),]

        Txf.mat <- data.frame(Txf[[i]])
        pxf.last.mat <- unlist(Txf.mat[nrow(Txf.mat),]/Txf.mat[nrow(Txf.mat)-1,])
        pxf.last.mat[is.nan(pxf.last.mat)] <- 0

        ## male
        Lxm.mat <- data.frame(Lxm[[i]])
        pxm.mat <- Lxm.mat[-1,]/Lxm.mat[-nrow(Lxm.mat),]

        Txm.mat <- data.frame(Txm[[i]])
        pxm.last.mat <- unlist(Txm.mat[nrow(Txm.mat),]/Txm.mat[nrow(Txm.mat)-1,])
        pxm.last.mat[is.nan(pxm.last.mat)] <- 0

        ## -------------------- migration
        migf.mat <- migf[[i]]
        migm.mat <- migm[[i]]
        if(zero.mig){
            migm.mat[, 2:30] <- 0
            migf.mat[, 2:30] <- 0
        }
        ## -------------------- fertility
        fx.mat <- asfr[[i]]*change.asfr
        
        ## -------------------- jump-off population
        popf.mat <- popf[[i]]
        popm.mat <- popm[[i]]
        popF.yr <- as.matrix(popf[[i]][,1])
        popM.yr <- as.matrix(popm[[i]][,1])
        
        Births <- NULL
        for(j in 1:(n.yr-1)){
            srb.y <- srb.ct[, j]

            ## age-specific numbers of females
            popF.tplus1 <- pxf.mat[,j]*(popF.yr[-n,j]+migf.mat[-n,j]/2) + migf.mat[-1,j]/2
            popF.last <- pxf.last.mat[j]*(sum(popF.yr[(n-1):n,j])+sum(migf.mat[(n-1):n,j])/2) + migf.mat[n,j]/2
            popF.tmp <- c(0, popF.tplus1[-length(popF.tplus1)], unlist(popF.last))
            popF.yr <- cbind(popF.yr, round(popF.tmp,3))

            ## nr. of birth
            bth.tot <- sum(5*fx.mat[,j]*((unlist(popF.yr[bth.posi,j]) + unlist(popF.yr[bth.posi,j+1]) + unlist(migf.mat[bth.posi,j])/2)/2))
            bth.girl <- bth.tot*(1/(srb.y+1))
            bth.boy <- bth.tot - bth.girl
            alive.girl <- bth.girl*Lxf.mat[1,j]/500000
            alive.boy <- bth.boy*Lxm.mat[1,j]/500000

            Births <- rbind(Births, c(year=yrs[j]-5, round(c(bth.tot, bth.girl, bth.boy), 3)))

            popF.yr[1,j+1] <- alive.girl + migf.mat[1,j]/2

            ## age-specific numbers of males
            popM.tplus1 <- pxm.mat[,j]*(popM.yr[-n,j]+migm.mat[-n,j]/2) + migm.mat[-1,j]/2
            popM.last <- pxm.last.mat[j]*(sum(popM.yr[(n-1):n,j])+sum(migm.mat[(n-1):n,j])/2) + migm.mat[n,j]/2
            popM.tmp <- c(alive.boy+migm.mat[1,j]/2, popM.tplus1[-length(popM.tplus1)],
                          unlist(popM.last))
            popM.yr <- cbind(popM.yr, round(popM.tmp,3))
        }

        ## the number of births in the last prodiction year
        bth.tot <- sum(5*fx.mat[,j+1]*unlist(popF.yr[bth.posi,j]))
        bth.girl <- bth.tot*(1/(srb.ct[, j+1]+1))
        bth.boy <- bth.tot - bth.girl
        Births <- rbind(Births, c(year=yrs[j], round(c(bth.tot, bth.girl, bth.boy), 3)))

        popF.yr <- as.data.frame(popF.yr)
        popM.yr <- as.data.frame(popM.yr)
        popB.yr <- popF.yr+popM.yr
        names(popF.yr) <- names(popM.yr) <-names(popB.yr) <- paste("", yrs, sep="")

        outbth <- as.data.frame(rbind(outbth, cbind(country=srb.name[i], country_code=srb.code[i], Births)))
    
        outf <- rbind(outf, cbind(country=srb.name[i], country_code=srb.code[i], age=age, round(popF.yr,3)))
        outm <- rbind(outm, cbind(country=srb.name[i], country_code=srb.code[i], age=age, round(popM.yr,3)))
        outb <- rbind(outb, cbind(country=srb.name[i], country_code=srb.code[i], age=age, round(popB.yr,3)))
    }
    names(outbth) <- c("country", "country_code", "year", "both.sex", "girl", "boy")
    write.csv(outf, paste(outdir, "/popf", srb.ver, "csv", sep="."), row.names=F)
    write.csv(outm, paste(outdir, "/popm", srb.ver, "csv", sep="."), row.names=F)
    write.csv(outb, paste(outdir, "/poptot", srb.ver, "csv", sep="."), row.names=F)
    write.csv(outbth, paste(outdir, "/birth", srb.ver, "csv", sep="."), row.names=F)
}


## output
get.ind <- function(dat, x1, x2, ct.list, yrs=seq(1955, 2100, by=5)){
    
    ## dat: age-sex-specific data

    output <- NULL
    for(ct in ct.list){
        dat.ct <- filter(dat, country==ct)
        total <- round(select(dat.ct, -c(1:3))%>%colSums())
        labor <- round(filter(dat.ct, age>=x1 & age<x2)%>%select(-c(1:3))%>%colSums())
        old <- round(filter(dat.ct, age>=x2)%>%select(-c(1:3))%>%colSums())
        output <- rbind(output, cbind(country=ct, year=yrs, total, labor, old))
    }
    output <- as.data.frame(output)
    write.table(output, "tmp", row.names=F); output <- read.table("tmp", head=T)
    return(output)
}


mean.age <- function(dat, ct.list){
    
    output <- NULL
    for(ct in ct.list){
        dat.ct <- filter(dat, country==ct)
        tmp <- apply(as.matrix(4:33), 1, function(j) weighted.mean(dat.ct[,3]+2.5, dat.ct[, j]))
        output <- rbind(output, cbind(country=ct, year=seq(1950, 2095, by=5),
                                      a.bar=round(tmp,3)))
    }
    
    return(data.frame(output))
    
}





squish_trans <- function(from, to, factor) {
  trans <- function(x) {
    # get indices for the relevant regions
    isq <- x > from & x < to
    ito <- x >= to
    # apply transformation
    x[isq] <- from + (x[isq] - from) / factor
    x[ito] <- from + (to - from) / factor + (x[ito] - to)
    return(x)
  }

  inv <- function(x) {
    # get indices for the relevant regions
    ## isq <- x > from & x < to + (to - from) / factor
    isq <- x < (from + (to - from) / factor)
    ito <- x >= (from + (to - from) / factor)
    # apply transformation
    x[isq] <- from + (x[isq] - from) * factor
    x[ito] <- to + x[ito] - (from + (to - from) / factor)
    return(x)
  }

  # return the transformation
  return(trans_new("squished", trans, inv))
}

plot.ct <- function(ct, yr0=1980, mid.yr=2020){
    dat.ct <- filter(tot.105wpp, country==ct & year>=yr0)
    srb.2100.ct <- filter(srb.2100, country==ct & year>=yr0)
    
    max.y <- ceiling(max(dat.ct$prop60))
    mid.y <- floor(dat.ct[dat.ct$year==mid.yr, "prop60"][3]/10)*10
    
    step.0 <- ifelse(max.y>40, 5, 2.5)

    col.brks <- seq(104, 118, by=1)
    n.brks <- length(col.brks)

    ggplot(dat.ct, aes(year, prop60, linetype=Scenario, col=Scenario))+geom_line(size=.8)+
        labs(subtitle=ct)+
        ylab("Population ages 65 and above (% of total population)")+xlab("Year")+
        geom_col(data=srb.2100.ct, aes(x=year, y=-max.y*0.020, fill=SRB), width=1, inherit.aes=F)+
        scale_x_continuous(breaks=seq(yr0, 2100, by=20))+
        scale_color_manual(values=c("red", "green3", "violet", "blue"))+
        geom_ribbon(data=filter(tot.lowup, country==ct & year>=1980),
                    aes(year, ymin=p65.lower, ymax=p65.upper),
                    inherit.aes=F, fill="steelblue", alpha=.3)+
        coord_trans(y=squish_trans(0, mid.y, 5))+
        scale_y_continuous(breaks = c(seq(0, mid.y, by=5), #=step.0*2),
                                      seq(mid.y, max.y, by=2.5)))+#step.0)))+
        scale_fill_gradientn(colors=rev(inferno(n.brks*3)[seq(1, 3*n.brks, by=3)]), breaks=col.brks)+
        theme(panel.grid.minor = element_blank(), axis.title=element_blank())
}
