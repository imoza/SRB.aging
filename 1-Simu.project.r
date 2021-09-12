rm(list=ls())

source("Functions.r")

## ==================================================
## data preparation
yrs <- seq(1950, 2095, by=5)

srb.ctry.list <- read.csv("srb.ctry.list.csv", head=T)
srb.code <- srb.ctry.list[,2]

n.ct <- nrow(srb.ctry.list)

## WPP SRB for affected populations
data(sexRatio)
srb <- filter(sexRatio, country_code %in% srb.code)

srb$name <- recode(srb$name, "China, Hong Kong SAR"="Hong Kong, SAR of China", "China, Taiwan Province of China"="Taiwan, Province of China")

data.table::setorder(srb, country_code)
srb <- srb[c(1:5, 7, 8, 6, 9:12),]
srb.code <- srb$country_code
srb.name <- srb$name

## SRB from Chao et al. BMJ GH 2021
bmj.est <- read.csv("./Data/bmjgh.estimated.csv", head=T, skip=2)%>%filter(Country.Code%in%srb.code)%>%select(c(1,2, "Reference.Year", "Quantile", "Model.Estimate"))

bmj.prj <- read.csv("./Data/bmjgh.projected.csv", head=T, skip=3)%>%filter(Country.Code%in%srb.code)%>%select(c(1,2, "Reference.Year", "Quantile", "Model.Projection"))

## use the median value
bmj.est.med <- filter(bmj.est, Quantile=="Median")
names(bmj.est.med) <- c("name", "country_code", "year", "quantile", "value")

bmj.prj.med <- filter(bmj.prj, Quantile=="Median")
names(bmj.prj.med)  <- c("name", "country_code", "year", "quantile", "value")

bmj <- rbind(bmj.est.med,  bmj.prj.med)[,-4]



## SRB from Chao et al. BMJ GH 2021
get.5yr.mean <- function(dat){
    yrs <- seq(1950, 2095, by=5)
    srb.ave <- NULL
    for(ctry in unique(dat$name)){
        dat.ct <- filter(dat, name==ctry)
        tmp <- apply(as.matrix(yrs), 1, function(t)
            mean(filter(dat.ct, year>=t & year<=t+4)[, "value"]))
        srb.ave <- rbind(srb.ave, cbind(name=ctry, country_code=dat.ct$country_code[1],
                                        year=yrs, value=round(tmp,3)))
    }
    return(as.data.frame(srb.ave))
}

## 5-yr SRB
bmj5y <- get.5yr.mean(bmj)

write.csv(bmj5y, "./data/bmj5y.csv", row.names=F)

bmj.wide <- reshape(bmj5y, idvar=c("country_code", "name"), timevar="year", direction="wide")
bmj.wide <- bmj.wide[,c(2, 1, 3:dim(bmj.wide)[2])]
names(bmj.wide) <- names(srb)

write.csv(bmj.wide, "./data/bmj.wide.csv", row.names=F)

bmj.wide <- read.csv("./data/bmj.wide.csv", head=T)
names(bmj.wide) <- names(srb)



## ============================================================
##           INPUT
## ============================================================

## ---------- fertility ----------
## estimated fertilty in 1950-2020
fert.est <- read.csv("./data/WPP2019_FERT_F07_AGE_SPECIFIC_FERTILITY_Estimates.csv", head=T, skip=12)%>% filter(Country.code %in% srb.code) %>% select(c("Location", "Country.code", "Period",starts_with("X")))

## predicted fertilty in 2020-2100
fert.medi <- read.csv("./data/WPP2019_FERT_F07_AGE_SPECIFIC_FERTILITY_Medium.csv", head=T, skip=16) %>% filter(Country.code %in% srb.code) %>%select(c(3, "Country.code",8:15))
names(fert.medi)[1] <- "Location"

## fertility in 1950-2100
fert <- rbind(fert.est, fert.medi)

asfr <- list()
for(i in 1:n.ct) {
    tmp <- as.data.frame(t(filter(fert, Country.code==srb.code[i])%>%select(starts_with("X"))))
    write.csv(tmp, "tmp.csv", row.names=F); tmp <- read.csv("tmp.csv", head=T)
    asfr[[i]] <- tmp/1000
}
file.remove("tmp.csv")


## -------------------- survival medium
## 1950-2015
lt <- read.csv("./data/WPP2019_LifeTable.for.high.SRB.countries.csv", head=T)

## 1955-2100
ltf <- filter(lt, Sex=="Female")
ltm <- filter(lt, Sex=="Male")

mid.yrs <- unique(ltf$MidPeriod)
n.mid.yrs <- length(mid.yrs)

age <- seq(0, 100, by=5)
n.ages <- length(unique(ltf$AgeGrp))

Lxf <- Lxm <-Txm <- Txf <- list()
for(i in 1:n.ct){

    Lxf.tmp <- filter(ltf, LocID==srb.code[i])
    Lxf.04 <- apply(as.matrix(mid.yrs),1, function(y) sum(Lxf.tmp[Lxf.tmp$MidPeriod==y, "Lx"][1:2]))
    Lxf.mat <- matrix(apply(as.matrix(mid.yrs),1, function(y) Lxf.tmp[Lxf.tmp$MidPeriod==y, "Lx"][3:n.ages]), nrow=n.ages-2)
    Lxf[[i]] <- rbind(Lxf.04, Lxf.mat)

    Lxm.tmp <- filter(ltm, LocID==srb.code[i])
    Lxm.04 <- apply(as.matrix(mid.yrs),1, function(y) sum(Lxm.tmp[Lxm.tmp$MidPeriod==y, "Lx"][1:2]))
    Lxm.mat <- matrix(apply(as.matrix(mid.yrs),1, function(y) Lxm.tmp[Lxm.tmp$MidPeriod==y, "Lx"][3:n.ages]), nrow=n.ages-2)
    Lxm[[i]] <- rbind(Lxm.04, Lxm.mat)

    Txf[[i]] <- matrix(unlist(filter(ltf, LocID==srb.code[i])%>%select("Tx")), nrow=n.ages)[-2,]
    Txm[[i]] <- matrix(unlist(filter(ltm, LocID==srb.code[i])%>%select("Tx")), nrow=n.ages)[-2,]
}



## ----------------- migration -------------
## 
data(migration)

mig.num <- filter(migration, country_code%in%srb.code)

mig.retr <- age.specific.migration(wpp.year=2019, rescale=TRUE, smooth=FALSE, countries=srb.code)

migf <- migm <- list()
for(i in 1:n.ct){
    migf[[i]] <- filter(mig.retr$female, country_code==srb.code[i])%>%select(-c(1:3))
    migm[[i]] <- filter(mig.retr$male, country_code==srb.code[i])%>%select(-c(1:3))
}


## -------------------- jump-off population -----------------

## WPP female
wppf.est <- read.csv("./data/WPP2019_POP_F07_3_POPULATION_BY_AGE_FEMALE_est.csv", skip=16, head=T)%>%filter(Referencedate.asof1July.<2020)
wppf.med <- read.csv("./data/WPP2019_POP_F07_3_POPULATION_BY_AGE_FEMALE_medium.csv", skip=16, head=T)
wppf.tot <- select(rbind(wppf.est, wppf.med), c(3,5,8:29))
names(wppf.tot) <- c("country", "country_code", "year", seq(0, 100, by=5))

## WPP male
wppm.est <- read.csv("./data/WPP2019_POP_F07_2_POPULATION_BY_AGE_MALE_est.csv", skip=16, head=T)%>%filter(Referencedate.asof1July.<2020)
wppm.med <- read.csv("./data/WPP2019_POP_F07_2_POPULATION_BY_AGE_MALE_medium.csv", skip=16, head=T)
wppm.tot <- select(rbind(wppm.est, wppm.med), c(3,5,8:29))
names(wppm.tot) <- c("country", "country_code", "year", seq(0, 100, by=5))

popf <- popm <- NULL
for(i in 1:n.ct){
    tmp <- filter(wppf.tot, country_code==srb.code[i] & year<2100)%>%select(-c(1:3))
    write.table(tmp, "tmp", row.names=F); tmp <- read.table("tmp", head=TRUE)
    popf[[i]] <- t(tmp)
    tmp <- filter(wppm.tot, country_code==srb.code[i] & year<2100)[,-c(1:3)]
    write.table(tmp, "tmp", row.names=F); tmp <- read.table("tmp", head=T)
    popm[[i]] <- t(tmp)
}
file.remove("tmp")

## ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## projection based on given fertility regime
## ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


## SRB=105
srb105 <- srb; srb105[,-c(1,2)] <- 1.05
proj.srb(srb105, popf, popm, asfr,
         Lxf, Lxm, Txf, Txm, migf, migm,
         srb.ver="105",
         srb.name, srb.code)

## synthetic SRB from 1950-2100
proj.srb(bmj.wide, popf, popm, asfr,
         Lxf, Lxm, Txf, Txm, migf, migm,
         srb.ver="bmj",
         srb.name, srb.code)



## ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## Counterfactual analysis
## ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

tot.105.ca <- tot.105
tot.bmj.ca <- tot.bmj

tot.bmj.ca$p65 <- with(tot.bmj.ca, round(old/total*100, 3))
tot.bmj.ca$oadr <- with(tot.bmj.ca, round(old/labor*100, 3))

tot.105.ca$p65 <- with(tot.105.ca, round(old/total*100, 3))
tot.105.ca$oadr <- with(tot.105.ca, round(old/labor*100, 3))

y.0 <- 14
y.1 <- 6

## different settings for the rate at which the TFR in different areas would increase  
mid.tfr <- c(rep(1.05, 3), 1.06, rep(1.02, 2), rep(1.03, 2), rep(1.02, 4))

asfr.ca <- asfr
tfr <- NULL
for(i in 1:length(asfr)){
    tmp <- asfr[[i]]
    tfr.i <- colSums(tmp)
    asfr.i <- t(t(tmp)/tfr.i)
    tfr.inc <- c(rep(1, y.0), rep(mid.tfr[i], y.1), rep(1, 30-y.0-y.1))
    asfr.ca[[i]] <- t(t(asfr.i)*c(tfr.i*tfr.inc))
    tfr <- rbind(tfr, c(i, tfr.i))
}

proj.srb(bmj.wide, popf, popm, asfr.ca,
         Lxf, Lxm, Txf, Txm, migf, migm,
         srb.ver="CA",
         srb.name, srb.code)
