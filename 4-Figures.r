###########################################################################################
## Figure 1

## SRB from Chao et al. BMJ GH 2021

bmj5y <- read.csv("./data/bmj5y.csv", head=T)

bmj.fig <- bmj5y
names(bmj.fig)[4] <- "SRB"

bmj.fig$SRB <- bmj.fig$SRB*100


bmj.fig$name <- factor(bmj.fig$name,
                       levels=rev(srb.name[c(4, 9, 2, 3, 11, 1, 6, 7, 8, 5, 10, 12)]))

## set the scales for color
yrs10 <- c(seq(1970, 2090, by=10), 2095)
col.brks <- seq(104, 120, by=1)
n.brks <- length(col.brks)

ggplot(filter(bmj.fig, year>=1970), aes(y=name, x=year, fill=SRB))+
    ylab("")+xlab("Year")+
    scale_x_continuous(breaks=seq(0, 51000, length=length(yrs10)), labels=yrs10)+
    geom_bar(stat="identity", width=.7)+theme_bw()+
    theme (axis.text=element_text (size=9))+
    scale_fill_gradientn(colors=rev(inferno(n.brks)), breaks=col.brks)+
    theme (legend.key.size = unit (.9, 'cm'))

ggsave("./Figs/Figure1.jpg", height=3, width=8)



#########################################################################################
## Figure 2

## ==================================================
## Fig. 2A total loss of births 

(cum.diff.bth <- ggplot(bth.rev.diff.bar, aes(y=srb.name, x=value, group=Sex, fill=Sex))+
     geom_bar(stat="identity", position="dodge", width=.5)+theme_bw()+
     theme(panel.border = element_blank())+
     xlab("Percentage change in the number of births (%)")+
     ylab("Countries/regions"))


## ==================================================
##  Fig. 2B female and male births

bth.to.105.redif <- cbind(bth.105[,c(1,3)],
                          Female=(bth.bmj$girl-bth.105$girl)/bth.105$girl*100,
                          Male=(bth.bmj$boy-bth.105$boy)/bth.105$boy*100)

bth.to.105.redif$country <- factor(bth.to.105.redif$country, level=srb.name)

##  [1] "Albania"                   "Azerbaijan"               
##  [3] "Armenia"                   "China"                    
##  [5] "Taiwan, Province of China" "Hong Kong, SAR of China"  
##  [7] "India"                     "Georgia"                  
##  [9] "Republic of Korea"         "Montenegro"               
## [11] "Viet Nam"                  "Tunisia"                  

bth.to.105.redif$country <- factor(bth.to.105.redif$country,
                                   levels=srb.name[c(1, 2, 3, 4, 11, 8, 7,
                                                     9, 5, 6, 10, 12)])

(f.bth <- ggplot(bth.to.105.redif, aes(year, Female, group=country, 
                                     col=country, linetype=country, shape=country))+
     theme_bw()+
     theme(legend.title=element_blank(),## panel.grid.minor = element_blank (),
           plot.title = element_text (hjust = 0.5))+
     geom_hline(yintercept=0, linetype=1, col="gray50")+
     geom_line()+geom_point()+
     scale_shape_manual(values=(c(rep(NA, 2), 1, 19, 17, 7, 1, 8, rep(NA, 2), 6, 3)))+
     scale_color_manual(values=(rep(c("orangered", "#386CB0"), rep(6,2))))+
     scale_linetype_manual(values=(rep(c(1:4, 2, 3), 2)))+
     ylab("Relative change in annual births (%)")+xlab("Year")+
     scale_x_continuous(breaks=seq(1980, 2100, by=20), labels=seq(1980, 2100, by=20))+
     scale_y_continuous(breaks=seq(-15, 5, by=2), limits=c(-14.5, 5))+
     ggtitle("Female births"))

(m.bth <- ggplot(bth.to.105.redif, aes(year, Male, group=country, 
                                     col=country, linetype=country, shape=country))+
     theme_bw()+
     theme(legend.title=element_blank(),## panel.grid.minor = element_blank (),
           plot.title = element_text (hjust = 0.5))+
     geom_hline(yintercept=0, linetype=1, col="gray50")+
     geom_line()+geom_point()+
     scale_shape_manual(values=(c(rep(NA, 2), 1, 19, 17, 7, 1, 8, rep(NA, 2), 6, 3)))+
     scale_color_manual(values=(rep(c("orangered", "#386CB0"), rep(6,2))))+
     scale_linetype_manual(values=(rep(c(1:4, 2, 3), 2)))+
     ylab("Relative change in annual births (%)")+xlab("Year")+
     scale_x_continuous(breaks=seq(1980, 2100, by=20), labels=seq(1980, 2100, by=20))+
     scale_y_continuous(breaks=seq(-15, 5, by=2), limits=c(-14.5, 5))+
     ggtitle("Male births"))

re.bth.diff <- ggarrange(f.bth, m.bth, nrow=2, common.legend=T, legend="bottom")



## ==================================================
## Fig 2C percentage of loss of women of childbearing age  
mother <- cbind(f.bmj[,c(1,2,4)], f.105[,4])
names(mother)[c(3,4)] <- c("est", "s105")

mother$rd <- with(mother, (s105-est)/est*100)

mother$country <- factor(mother$country,
                         levels=srb.name[c(1,3,2,4,8,11,5:7,9,10,12)])

(loss.women <- ggplot(mother, aes(year, rd, facet=country))+geom_area(fill="steelblue")+
     xlab("Year")+ylab("Percentage loss of women of childbearing age (%)")+
     xlim(2000, 2095)+
    facet_wrap(~country, nrow=12))


library(patchwork)

layout <- "
ABBBCC
#BBBCC
"

(cum.diff.bth+theme(plot.margin = unit(c(0,30,0,0), "pt")))+
    (re.bth.diff+theme(plot.margin = unit(c(0,30,00,0), "pt")))+
    loss.women+plot_layout(design=layout)+
    plot_annotation(tag_levels = 'A')

ggsave("./Figs/Figure2.jpg", height=9, width=12)



#########################################################################################
## Figure 3 Increase in the proportion of aged 65+ and OADR

diff.p65 <- cbind(tot.bmj[,c(1,2)],
                  diff=with(tot.bmj, old/total*100)-with(tot.105, old/total*100))
diff.p65$country <- factor(diff.p65$country,
                           levels=srb.name[c(1, 2, 3, 4, 11, 8, 7,
                                             9, 5, 6, 10, 12)])

                           ## levels=srb.name[c(1, 2, 3, 4, 7, 8,
                           ##                   11, 9, 5, 6, 10, 12)])

diff.oadr <- cbind(tot.bmj[,c(1,2)],
                   diff=with(tot.bmj, old/labor*100)-with(tot.105, old/labor*100))

diff.oadr$country <- factor(diff.oadr$country,
                            levels=srb.name[c(1, 2, 3, 4, 11, 8, 7,
                                              9, 5, 6, 10, 12)])
                           ## levels=srb.name[c(1, 2, 3, 4, 7, 8,
                           ##                   11, 9, 5, 6, 10, 12)])

diff.p65.oadr <- rbind(cbind(diff.p65, variable="Percentage 65+ (%)"),
                       cbind(diff.oadr, variable="OADR"))

diff.p65.oadr$country <- factor(diff.p65.oadr$country,
                                levels=srb.name[c(1, 2, 3, 4, 11, 8, 7,
                                                  9, 5, 6, 10, 12)])
                                ## levels=srb.name[c(1, 2, 3, 4, 7, 8,
                                ##                   11, 9, 5, 6, 10, 12)])

(fig.dif.p65 <- ggplot(filter(diff.p65, year>=2020),
       aes(year, diff, col=country, linetype=country, shape=country))+
    theme_bw()+theme(legend.title=element_blank())+
    geom_line()+geom_point()+
    scale_shape_manual(values=(c(rep(NA, 2), 1, 19, 17, 7, 1, 8, rep(NA, 2), 6, 3)))+
    scale_color_manual(values=(rep(c("orangered", "#386CB0"), rep(6,2))))+
    scale_linetype_manual(values=(rep(c(1:4, 2, 3), 2)))+
    xlab("Year")+ylab("Percentage 65+ (%)")+theme_bw()+
    theme(legend.title=element_blank()))

(fig.dif.oadr <- ggplot(filter(diff.oadr, year>=2020),
       aes(year, diff, col=country, linetype=country, shape=country))+
    theme_bw()+theme(legend.title=element_blank())+
    geom_line()+geom_point()+
    scale_shape_manual(values=(c(rep(NA, 2), 1, 19, 17, 7, 1, 8, rep(NA, 2), 6, 3)))+
    scale_color_manual(values=(rep(c("orangered", "#386CB0"), rep(6,2))))+
    scale_linetype_manual(values=(rep(c(1:4, 2, 3), 2)))+
    xlab("Year")+ylab("OADR")+theme_bw()+
    theme(legend.title=element_blank()))

ggarrange(fig.dif.p65, fig.dif.oadr, ncol=2, common.legend=TRUE, legend="right")

ggsave("./Figs/Figure3.jpg", height=5, width=10)


########################################################################################
##  Figure 4 Contour of population aging

surf.ctry <- read.csv("./data/surf.ctry")
st.ctry <- read.csv("./data/st.ctry")

## select four countires for illustration
ct.pick1 <- c(2, 4, 9, 11)
y0 <- 1980

ggplot(filter(surf.ctry, y>=1 & y<3.5 & p65<40 & country%in%srb.name[ct.pick1]),
       aes(x, y, z=p65, facet=country))+
    geom_contour_filled(aes(fill=stat(level)), alpha=.9)+
    scale_fill_brewer(palette = "Spectral", direction=-1)+
    geom_contour(color="gray60", size=.3)+
    labs(fill="Percentage 65+ (%)")+
    scale_x_continuous(breaks=seq(1.05, 1.20, by=.02), labels=seq(105, 120, by=2))+
    ylab("Total fertility rate")+xlab("Sex ratio at birth")+theme_bw()+
    geom_path(data=filter(st.ctry, year>=y0 & year<=2020 & TFR<3.6 & country%in%c(srb.name[ct.pick1])), aes(SRB, TFR, z=p65), col="gray40", inherit.aes=F)+
    geom_path(data=filter(st.ctry, year>=2020 & TFR<3.6 & country%in%c(srb.name[ct.pick1])), aes(SRB, TFR, z=p65), col="gray40", linetype=2, inherit.aes=F)+
    geom_point(data=filter(st.ctry, year>=y0 & TFR<3.6 & year<2020 & country%in%c(srb.name[ct.pick1])), aes(SRB, TFR, z=p65), col="blue", shape=17,inherit.aes=F, size=2)+
    geom_label_repel(data=filter(st.ctry, year>=y0 & TFR<3.6 & year<=2020 & country%in%c(srb.name[ct.pick1])), aes(SRB, TFR, label = year), label.padding=.15, size = 2)+
    geom_label_repel(data=filter(st.ctry, year>=y0 & TFR<3.6 & year==2095 & country%in%c(srb.name[ct.pick1])), aes(SRB, TFR, label = year), label.padding=.15, size = 2)+
    geom_point(data=filter(st.ctry, year>=2020 & TFR<3.6 & country%in%c(srb.name[ct.pick1])), aes(SRB, TFR, z=p65), col="red", shape=1, inherit.aes=F, size=1.4)+
    facet_wrap(~country, scales="free", ncol=2)

ggsave("./Figs/Figure4.jpg", width=8, height=6)


##                 Supplementary material: Figures
#########################################################################################
## Figure S1  Difference in population decline

diff.num <- cbind(tot.bmj[,c(1:2)], tot.bmj[,c(3:5)]-tot.105[,c(3:5)])
diff.num.l <- reshape2::melt(diff.num, id.var=c("country", "year"))
diff.num.l$country <- factor(diff.num.l$country, levels=srb.name)
diff.num.l$variable <- recode(diff.num.l$variable, "total"="Total population", "labor"="Population 20-64", "old"="Population 65+")
diff.num.l$country <- factor(diff.num.l$country, levels=srb.name)

ggplot(filter(diff.num.l, year>=1980), aes(year, value, col=variable, linetype=variable, facet=country))+
    theme_bw()+ylab("Reductions in population size (thousand)")+xlab("Year")+
    xlim(1980, 2100)+
    geom_line()+theme(legend.title=element_blank())+
    facet_wrap(~country, ncol=3, scales="free_y")

ggsave("./Figs/Figure S1.jpg", width=10, height=7)


#########################################################################################
## Figures S2 and S3 Population aging given the increasing TFR and the unchanging SRB 

tot.tfr.ca <- read.csv("./proj.out/poptot.CA.csv", head=T)%>%
    filter(country%in%srb.name)%>%get.ind(x1=20, x2=x.old, srb.name)
tot.tfr.ca$p65 <- with(tot.tfr.ca, round(old/total*100, 3))
tot.tfr.ca$oadr <- with(tot.tfr.ca, round(old/labor*100, 3))

tot.3grp <- rbind(cbind(tot.bmj.ca, variable="Estimated SRB"),
                  cbind(tot.tfr.ca, variable="Increased TFR"),
                  cbind(tot.105.ca, variable="SRB=105"))%>%filter(year>2030)

tot.3grp$country <- factor(tot.3grp$country, levels=srb.name)
inc.tfr <- round(mid.tfr-1,2)*100

tfr.text <- data.frame(country=srb.name,
                       tfr=sprintf("TFR increased by %.0f%%", inc.tfr),
                       min.oadr=apply(as.matrix(srb.name), 1, function(x) round(min(filter(tot.3grp, country==x)$oadr))),
                       min.p65=apply(as.matrix(srb.name), 1, function(x) round(min(filter(tot.3grp, country==x)$p65))))

(ca.p65 <- ggplot(tot.3grp,
                  aes(year, p65, col=variable, linetype=variable, facet=country))+
     geom_line()+theme_bw()+
     scale_color_manual(values=c("green3", "red", "blue"))+
     scale_linetype_manual(values=c(2, 1, 3))+
     geom_text(data=tfr.text, aes(x=2080, y=min.p65, label=tfr), size=4,
               inherit.aes=F)+
     theme(legend.title=element_blank())+
     ylab("Percentage of older people")+xlab("Year")+
     facet_wrap(~country, scale="free", ncol=3))

ggsave("./Figs/Figure S2.jpg", height=10, width=10)


(ca.oadr <- ggplot(tot.3grp,
                   aes(year, oadr, col=variable, linetype=variable, facet=country))+
     geom_line()+theme_bw()+
     scale_color_manual(values=c("green3", "red", "blue"))+
     scale_linetype_manual(values=c(2, 1, 3))+
     geom_text(data=tfr.text, aes(x=2080, y=min.oadr, label=tfr), size=4,
               inherit.aes=F)+
     ylab("OADR")+xlab("Year")+
     theme(legend.title=element_blank())+
     facet_wrap(~country, scale="free", ncol=3))

ggsave("./Figs/Figure S3.jpg", height=10, width=10)


#########################################################################################
## Figure S4 and S5 Contour of the proportion of aged 65+ and OADR

## contour plot of the proportion of aged 65+ for other populations
ct.pick2 <- c(1:12)[!c(1:12)%in%ct.pick1]

ggplot(filter(surf.ctry, y<4.3 & country%in%srb.name[ct.pick2]),
       aes(x, y, z=p65, facet=country))+
    geom_contour_filled(aes(fill=stat(level)), alpha=.9)+
    scale_fill_brewer(palette = "Spectral", direction=-1)+
    geom_contour(color="gray60", size=.3)+
    labs(fill="Percentage 65+ (%)")+
    scale_x_continuous(breaks=seq(1.05, 1.20, by=.02), labels=seq(105, 120, by=2))+
    ylab("Total fertility rate")+xlab("Sex ratio at birth")+theme_bw()+
    geom_path(data=filter(st.ctry, year>=y0 & TFR<3.6 & country%in%c(srb.name[ct.pick2])), aes(SRB, TFR, z=p65), col="gray20", inherit.aes=F)+
    geom_point(data=filter(st.ctry, year>=y0 & TFR<3.6 & year<2020 & country%in%c(srb.name[ct.pick2])), aes(SRB, TFR, z=p65), col="blue", shape=17,inherit.aes=F, size=2)+
    geom_label_repel(data=filter(st.ctry, year>=y0 & TFR<3.6 & year<2020 & country%in%c(srb.name[ct.pick2])), aes(SRB, TFR, label = year), label.padding=.15, size = 2.5)+
    geom_point(data=filter(st.ctry, year>=2020 & TFR<3.6 & country%in%c(srb.name[ct.pick2])), aes(SRB, TFR, z=p65), col="red", shape=1, inherit.aes=F, size=1.4)+
    facet_wrap(~country, scales="free_y", ncol=3)

ggsave("./Figs/Figure S4.jpg", width=9, height=8)


## contour plot for OADR 
ggplot(filter(surf.ctry, y<3.8), aes(x, y, z=oadr, facet=country))+
    geom_contour_filled(aes(fill=stat(level)), alpha=.9)+
    scale_fill_brewer(palette = "Spectral", direction=-1)+
    geom_contour(color="gray60", size=.3)+
    labs(fill="OADR")+
    scale_x_continuous(breaks=seq(1.05, 1.20, by=.02), labels=seq(105, 120, by=2))+
    ylab("Total fertility rate")+xlab("Sex ratio at birth")+theme_bw()+
    geom_path(data=filter(st.ctry, year>=y0 & TFR<3.6), aes(SRB, TFR, z=p65), col="gray20", inherit.aes=F)+
    geom_point(data=filter(st.ctry, year>=y0 & TFR<3.6 & year<2020), aes(SRB, TFR, z=oadr), col="blue", shape=17,inherit.aes=F, size=2)+
    geom_label_repel(data=filter(st.ctry, year>=y0 & TFR<3.6 & year<2020), aes(SRB, TFR, label = year), label.padding=.15, size = 2.5)+
    geom_point(data=filter(st.ctry, year>=2020 & TFR<3.6), aes(SRB, TFR, z=oadr), col="red", shape=1, inherit.aes=F, size=1.4)+
    facet_wrap(~country, scales="free_y", ncol=3)

ggsave("./Figs/Figure S5.jpg", width=10, height=9)



                                                

