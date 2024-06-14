library(tidyverse)
# install.packages("rioplot")
library(rioplot)

rm(list=ls())
#####DATA######
nation <- c("Aul89","Bel92","Can91","Den92","Fin91","Fr89","Ger89","Ire87","It91","Neth91","Nor91","Swe92","Swi82","UK91","US91")
postpov <- c(11.9,6,6.5,5.9,3.7,9.8,4.3,29.4,14.3,7.3,1.7,5.8,3.8,16.8,11.7)
gdp <- c(7734.418,6258.508,7894.777,7450.465,5712.95,6938.063,6746.054,3905.857,5507.27,7390.348,6507.316,7965.854,11419.35,7982.377,11871.49)
prepov <- c(23.3,26.8,22.5,26.4,11.9,36.1,15.2,39.2,30.7,22.1,9.2,23.7,12.5,29.6,21)
tran <- c(7.3,19.3,9.5,13.5,10.4,17.8,14.8,10.5,14.5,21.5,13.4,14.6,9.4,10.1,8.8)
postpov.z<-scale(postpov)
tran.z<-scale(tran)
gdp.z<-scale(gdp)
prepov.z<-scale(prepov)
country.names<-c("Australia", "Belgium", "Canada", "Denmark", "Finland", "France", "Germany", "Ireland", "Italy", "Netherlands", "Norway", "Sweden", "Switzerland", "UK", "USA")
country.abbr<-c("AUS", "BEL", "CAN", "DEN", "FIN", "FRA", "DEU", "IRL", "ITA", "NLD", "NOR", "SWE", "CHE", "UK", "USA")
variables<-c("TRAN","GDP","POV","YHAT")
######FUNCTIONS########
aggregate.rows <- function(M, membs, myfun="sum") {
  step4 <- aggregate(M, by=list(membs), FUN=myfun)
  step5 <- step4[,-1]
  colnames(step5) <- colnames(M)
  return(step5) }

cosine <- function(x,y) {
  # This assumes that x and y are vectors of the same length.
  Sxy <- sum(x * y) # this is the sum of crossproducts
  distX <- sqrt(sum(x^2)) # This is distance of x from the origin
  distY <- sqrt(sum(y^2)) # distance of y from the origin
  out <- Sxy / (distX * distY)
  return(out)
}

###########KENWORTHY RE-ANALYSIS########
#Table 7.2
km1<-lm(postpov.z~tran.z+gdp.z+prepov.z-1)
summary(km1)

#Figure 7.1 - Typical and Deviant Cases Cook's Distance, Kenworthy
#Compare with other methods for algorithmic case selection identified by Seawright (2016)
#First, Seawright identifies typical vs. deviant cases by selecting based on largest/smallest
#absolute value of the model residuals
typical.k<-postpov.z-km1$fitted
country.names<-c("Australia", "Belgium", "Canada", "Denmark", "Finland", "France", "Germany", "Ireland", "Italy", "Netherlands", "Norway", "Sweden", "Switzerland", "UK", "USA")
dat <- data.frame(resids=abs(typical.k[1:15]),names.arg=country.names)
dat <- mutate(dat,fille=c(rep(0,7),1,1,rep(0,6)))
ggplot(dat,aes(x=names.arg,y=resids,fill=as.factor(fille))) + theme_classic() + geom_bar(stat="identity") + 
  labs(x="",y="Residuals (absolute values)") + scale_y_continuous(limits=c(0,1),breaks=seq(0,1,.2)) +
  theme(axis.text.x=element_text(angle = 90, hjust = 0)) + scale_fill_manual(values=c("grey30","grey60")) +
  guides(fill=FALSE) +
  theme(axis.title = element_text(size = 16),
        axis.text = element_text(size = 15)) # Figure 7.1





#Figure 7.2 - Influential Cases, Kenworthy
#for a more general test of influential cases, we could also use Cook's d
cooksd.k<-cooks.distance(km1)
dat <- data.frame(cd=cooksd.k,country.names=country.names)
ggplot(dat,aes(x=country.names,y=cooksd.k))  + theme_classic() +
  labs(x="",y="Cook's Distance") + theme(axis.text.x=element_text(angle = 90, hjust = 0)) +
  geom_hline(yintercept=1,color="black",linetype="longdash") + 
  geom_hline(yintercept=0,color="black",linetype="solid") + 
  geom_hline(yintercept=.36,color="black",linetype="dotted") + geom_bar(stat="identity",width=.01,color="grey60")+ geom_point(size=1.5) +
  theme(axis.title = element_text(size = 16),
        axis.text = element_text(size = 15)) # Figure 7.2



#Figure 7.3 - Influential Cases DFBETA, Kenworthy
#DFBETAS to identify influential cases for specific variables. This should be used for when
#there's one particular variable of interest, and you want to see which case is most influential
dfbeta.k<-dfbeta(km1)
pdat <- data.frame(dfbeta.k,country.names=country.names)


f7.3.1 <-ggplot(pdat,aes(x=country.names,y=tran.z)) + theme_classic() +
  labs(x="",y="DFBETA Govt. Transfers") + geom_hline(yintercept=0) +
  theme(axis.text.x=element_text(angle = 90, hjust = 0)) +
  geom_bar(stat="identity",width=.01,color="grey60") + geom_point() +
  scale_y_continuous(limits=c(-.2,.21),breaks=seq(-.2,.2,.1))+
  theme(axis.title = element_text(size = 16),
        axis.text = element_text(size = 15))

f7.3.2 <-ggplot(pdat,aes(x=country.names,y=gdp.z)) + theme_classic() +
  labs(x="",y="DFBETA GDP") + geom_hline(yintercept=0) +
  theme(axis.text.x=element_text(angle = 90, hjust = 0)) +
  geom_bar(stat="identity",width=.01,color="grey60") + geom_point() +
  scale_y_continuous(limits=c(-.21,.2),breaks=seq(-.2,.2,.1))+
  theme(axis.title = element_text(size = 16),
        axis.text = element_text(size = 15))

f7.3.3 <-ggplot(pdat,aes(x=country.names,y=prepov.z)) + theme_classic() +
  labs(x="",y="DFBETA Pretax/pretransfer Poverty") + geom_hline(yintercept=0) +
  theme(axis.text.x=element_text(angle = 90, hjust = 0)) +
  geom_bar(stat="identity",width=.01,color="grey60") + geom_point() +
  scale_y_continuous(limits=c(-.2,.2),breaks=seq(-.2,.2,.1))+
  theme(axis.title = element_text(size = 16),
        axis.text = element_text(size = 15))




X.k<-cbind(tran,gdp,prepov)
H.k<-svd(X.k)$u %*% t(svd(X.k)$u)
leverage.k<-diag(H.k)
pdat <- data.frame(leverage.k,country.names)
ggplot(pdat,aes(x=country.names,y=leverage.k)) + theme_classic() +
  labs(x="",y="Leverage") +
  theme(axis.text.x=element_text(angle = 90, hjust = 0)) +
  geom_hline(yintercept=0,color="black",linetype="solid") + 
  geom_bar(stat="identity",width=.01,color="grey60") + geom_point() +
  theme(axis.title = element_text(size = 16),
        axis.text = element_text(size = 15)) # Figure 7.4



extreme.k<-abs((tran-mean(tran)/sd(tran)))
pdat <- data.frame(extreme.k,country.names)
# 7.5
ggplot(pdat,aes(x=country.names,y=extreme.k)) + theme_classic() +
  labs(x="",y="Residuals (absolute values)") +
  theme(axis.text.x=element_text(angle = 90, hjust = 0)) +
  geom_bar(stat="identity",fill=c(rep("grey40",9),"grey70", rep("grey40",5))) +   theme(axis.title = element_text(size = 16),
                                                                                        axis.text = element_text(size = 15))



mvars<-c( "GOVTRANS", "GDPPC60", "PVABPR40")
dropvars<-c("PVABPO40","NATION", "PVABPO50", "PVABPO30", "DECOMMOD", "SOCWAGE", "PVABPR50", "PVABPR30")
X <- cbind(tran, gdp, prepov)
rownames(X) <- country.abbr

Z <- scale(X) # Turns each column into a set of Z-scores
dv<-scale(postpov)
m1<-lm(dv~Z-1)
#summary(km1)

s<-svd(cbind(Z,m1$fitted.values))
V<-s$v
U<-s$u
S<-diag(s$d)


Z5 <- mutate(data.frame(Z),dv=dv)
m1<-lm(dv~ tran + gdp + prepov -1,data=Z5)
UV<-rbind(V,U%*%S)
names<-c(variables,toupper(country.abbr))
p1 <- rio.plot(m1,include.int="no")
case.dat <- data.frame(x=p1$row.dimensions[,1],y=p1$row.dimensions[,2],country.abbr,groups=c("grey30","grey70","grey30","grey70","grey70","grey30","grey70","grey30","grey30","grey30","grey70","grey30","grey70","grey30","grey30"))
# Fig 7.6
p1$gg.obj + geom_point(data=case.dat,aes(x=x,y=y,shape=groups), size=3) +
  scale_shape_manual(values=c(21,15)) +
  geom_text(data=case.dat,aes(x=x,y=y,label=tolower(country.abbr)),hjust=-.2,vjust=0) + 
  theme(legend.position = "none") + scale_x_continuous(limits=c(-2.5,3.4),breaks=seq(-2,3,1))+   theme(axis.title = element_text(size = 16),
                                                                                                       axis.text = element_text(size = 15))



coordinates<-as.data.frame(UV[,1:2])
rownames(coordinates)<-names
coordinates # Table 7.5

coord.ita<-c(coordinates[13,1], coordinates[13,2])
coord.aut<-c(coordinates[5,1], coordinates[5,2])
coord.bel<-c(coordinates[6,1], coordinates[6,2])
coord.can<-c(coordinates[7,1], coordinates[7,2])
coord.den<-c(coordinates[8,1], coordinates[8,2])
coord.fin<-c(coordinates[9,1], coordinates[9,2])
coord.fra<-c(coordinates[10,1], coordinates[10,2])
coord.deu<-c(coordinates[11,1], coordinates[11,2])
coord.irl<-c(coordinates[12,1], coordinates[12,2])
coord.nld<-c(coordinates[14,1], coordinates[14,2])
coord.nor<-c(coordinates[15,1], coordinates[15,2])
coord.swe<-c(coordinates[16,1], coordinates[16,2])
coord.che<-c(coordinates[17,1], coordinates[17,2])
coord.uk<-c(coordinates[18,1], coordinates[18,2])
coord.usa<-c(coordinates[19,1], coordinates[19,2])

italy.cosines<-rep(1,15)
italy.cosines[1]<-cosine(coord.ita,coord.aut)
italy.cosines[2]<-cosine(coord.ita,coord.bel)
italy.cosines[3]<-cosine(coord.ita,coord.can)
italy.cosines[4]<-cosine(coord.ita,coord.den)
italy.cosines[5]<-cosine(coord.ita,coord.fin)
italy.cosines[6]<-cosine(coord.ita,coord.fra)
italy.cosines[7]<-cosine(coord.ita,coord.deu)
italy.cosines[8]<-cosine(coord.ita,coord.irl)
italy.cosines[10]<-cosine(coord.ita,coord.nld)
italy.cosines[11]<-cosine(coord.ita,coord.nor)
italy.cosines[12]<-cosine(coord.ita,coord.swe)
italy.cosines[13]<-cosine(coord.ita,coord.che)
italy.cosines[14]<-cosine(coord.ita,coord.uk)
italy.cosines[15]<-cosine(coord.ita,coord.usa)
italy.cosines<-as.data.frame(italy.cosines)
rownames(italy.cosines)<-country.abbr

pdat <- data.frame(italy.cosines,country.names)
# Fig 7.7
ggplot(pdat,aes(x=country.names,y=italy.cosines)) + theme_classic() +
  theme(axis.text.x=element_text(angle = 90, hjust = 0)) +
  geom_bar(stat="identity",fill=c(rep("grey40",8),"grey70", rep("grey40",6))) +
  labs(x="",y="")+   theme(axis.title = element_text(size = 16),
                           axis.text = element_text(size = 15))


#Check this against similarities using three dimensions
coordinates2<-as.data.frame(UV[,1:3])
rownames(coordinates2)<-names
#Coordinates for each point in the graph:
coord.ita2<-c(coordinates2[13,1], coordinates2[13,2],coordinates2[13,3])
coord.aut2<-c(coordinates2[5,1], coordinates2[5,2],coordinates2[5,3])
coord.bel2<-c(coordinates2[6,1], coordinates2[6,2], coordinates2[6,3])
coord.can2<-c(coordinates2[7,1], coordinates2[7,2], coordinates2[7,3])
coord.den2<-c(coordinates2[8,1], coordinates2[8,2], coordinates2[8,3])
coord.fin2<-c(coordinates2[9,1], coordinates2[9,2], coordinates2[9,3])
coord.fra2<-c(coordinates2[10,1], coordinates2[10,2],coordinates2[10,3])
coord.deu2<-c(coordinates2[11,1], coordinates2[11,2],coordinates2[11,3])
coord.irl2<-c(coordinates2[12,1], coordinates2[12,2],coordinates2[12,3])
coord.nld2<-c(coordinates2[14,1], coordinates2[14,2],coordinates2[14,3])
coord.nor2<-c(coordinates2[15,1], coordinates2[15,2],coordinates2[15,3])
coord.swe2<-c(coordinates2[16,1], coordinates2[16,2],coordinates2[16,3])
coord.che2<-c(coordinates2[17,1], coordinates2[17,2],coordinates2[17,3])
coord.uk2<-c(coordinates2[18,1], coordinates2[18,2],coordinates2[18,3])
coord.usa2<-c(coordinates2[19,1], coordinates2[19,2],coordinates2[19,3])

italy.cosines2<-rep(1,15)
italy.cosines2[1]<-cosine(coord.ita2,coord.aut2)
italy.cosines2[2]<-cosine(coord.ita2,coord.bel2)
italy.cosines2[3]<-cosine(coord.ita2,coord.can2)
italy.cosines2[4]<-cosine(coord.ita2,coord.den2)
italy.cosines2[5]<-cosine(coord.ita2,coord.fin2)
italy.cosines2[6]<-cosine(coord.ita2,coord.fra2)
italy.cosines2[7]<-cosine(coord.ita2,coord.deu2)
italy.cosines2[8]<-cosine(coord.ita2,coord.irl2)
italy.cosines2[10]<-cosine(coord.ita2,coord.nld2)
italy.cosines2[11]<-cosine(coord.ita2,coord.nor2)
italy.cosines2[12]<-cosine(coord.ita2,coord.swe2)
italy.cosines2[13]<-cosine(coord.ita2,coord.che2)
italy.cosines2[14]<-cosine(coord.ita2,coord.uk2)
italy.cosines2[15]<-cosine(coord.ita2,coord.usa2)
italy.cosines2<-as.data.frame(italy.cosines2)
rownames(italy.cosines2)<-country.abbr
#barplot(italy.cosines2[,1], names.arg = country.names, density=c(-1,-1,-1,-1,-1,-1,-1,-1,25,-1,-1,-1,-1,-1,-1), cex.names = .8, las=2, ylim=c(-1,1))#plot the results

#Figure 8.8 - reproduce plot with countries divided by regime type
p1 <- rio.plot(m1,include.int="no",col.names=c("tran","gdp","prepov","YHAT"))
case.dat <- data.frame(x=p1$row.dimensions[,1],y=p1$row.dimensions[,2],country.abbr,type= c("Liberal","Corp","Liberal","SocDem","SocDem","Corp","Corp","Corp","Corp","Corp","SocDem","SocDem","Liberal","Liberal","Liberal"))

# Fig 7.8
p1$gg.obj + geom_point(data=case.dat,aes(x=x,y=y,shape=type), size=3) +
  geom_text(data=case.dat,aes(x=x,y=y,label=tolower(country.abbr)),hjust=-.2,vjust=0) + 
  scale_x_continuous(limits=c(-2.5,3.4),breaks=seq(-2,3,1)) + labs(shape="") +
  theme(legend.position="bottom")+   theme(axis.title = element_text(size = 16),
                                           axis.text = element_text(size = 15),
                                           legend.text= element_text(size=15))


B<-solve(t(Z) %*% Z) %*% t(Z) %*% diag(as.numeric(dv))
colnames(B)<-country.abbr
regime.type<-c(1,2,1,3,3,2,2,2,2,2,3,3,1,1,1)#1=liberal, 2=corporatist, 3=social democratic
b.by.regime<-t(aggregate(t(B),by=list(regime.type),FUN=sum))[-1,]
b.by.regime # Table 7.6
#Table 7.6 - Individual cases' contributions to overall coefficients
round(t(B), 3)

contrib.weight<-300*abs(t(B))
contrib.weight<-contrib.weight[,1]

pdat <- data.frame(x=lm(Z[,1]~ Z[,c(2,3)] -1)$residuals,y=as.numeric(dv),sizes=contrib.weight,country.abbr)
# Fig 7.7
ggplot() + theme_classic() +
  geom_point(data=pdat,aes(x=x,y=y,size=sizes),shape=1) + geom_hline(yintercept=0,color="grey50") + geom_vline(xintercept=0,color="grey50") +
  geom_smooth(data=pdat,aes(x=x,y=y),method="lm",se=FALSE,size=.4,color="black") +
  geom_text(data=pdat,aes(x=x,y=y,label=country.abbr),vjust=1.5) + coord_fixed() +
  theme(legend.position = "none") + scale_y_continuous(limits=c(-1.3,3)) +
  labs(x="Transfers residualized from other predictors",y="Post-transfer poverty")+   theme(axis.title = element_text(size = 16),
                                                                                            axis.text = element_text(size = 15),
                                                                                            legend.text= element_text(size=15))



contrib.weight<-30*abs(t(B))
contrib.weight<-contrib.weight[,2]
pdat <- data.frame(x=lm(Z[,2]~ Z[,c(1,3)] -1)$residuals,y=as.numeric(dv),sizes=contrib.weight,country.abbr)
ggplot() + theme_classic() +
  geom_point(data=pdat,aes(x=x,y=y,size=sizes),shape=1) + geom_hline(yintercept=0,color="grey50") + geom_vline(xintercept=0,color="grey50") +
  geom_smooth(data=pdat,aes(x=x,y=y),method="lm",se=FALSE,size=.4,color="black") +
  geom_text(data=pdat,aes(x=x,y=y,label=country.abbr),vjust=1.5) +
  theme(legend.position = "none") + coord_fixed() +
  scale_y_continuous(limits=c(-1.3,3)) +
  labs(x="GDP residualized from other predictors",y="Post-transfer poverty")+   theme(axis.title = element_text(size = 16),
                                                                                      axis.text = element_text(size = 15),
                                                                           legend.text= element_text(size=15))


#Figure 10.10b - Residualized pre-pov by post-poverty
#contrib.weight<-30*abs(t(B))
#contrib.weight<-contrib.weight[,3]
#plotRB(lm(Z[,3]~ Z[,c(1,2)] -1)$residuals,as.numeric(dv),pch=1, pointcex=contrib.weight, ylim=c(-1.2,3),labels=country.abbr,xlab="Pre-transfer poverty residualized from other predictors",ylab="Post-transfer poverty",main="")
#abline(a=0,b=.783)

contrib.weight<-30*abs(t(B))
contrib.weight<-contrib.weight[,3]
pdat <- data.frame(x=lm(Z[,3]~ Z[,c(1,2)] -1)$residuals,y=as.numeric(dv),sizes=contrib.weight,country.abbr)
# Fig 7.10b
p1 <- ggplot() + theme_classic() +
  geom_point(data=pdat,aes(x=x,y=y,size=sizes),shape=1) + geom_hline(yintercept=0,color="grey50") + geom_vline(xintercept=0,color="grey50") +
  geom_smooth(data=pdat,aes(x=x,y=y),method="lm",se=FALSE,size=.4,color="black") +
  geom_text(data=pdat,aes(x=x,y=y,label=country.abbr),vjust=1.5) +
  theme(legend.position = "none") + coord_fixed() +
  #scale_y_continuous(limits=c(-1.3,3)) +
  labs(x="Pre-transfer poverty residualized from other predictors",y="Post-transfer poverty")+   theme(axis.title = element_text(size = 16),
                                                                                                       axis.text = element_text(size = 15),
                                                                                                       legend.text= element_text(size=15))
p1


pch.msmd<-pch.k.ea<-c(20,20,20,20,1,20,1,20,20,20,20,20,20,1,20,20,1,20,20)
#plotRB(UV[,1],UV[,2],labels=names, pch=pch.msmd, xlab="First Column of UxS and V",ylab="Second Column of UxS and V", main="", ylim=c(-2.5,2.5),xlim=c(-2.5,2.5))
#abline(a=0,b=(V[4,2]/V[4,1]))
#segments(x0 = UV[5,1], y0 = UV[5,2], x1 = 0, y1 = 0, lty="dotted", lwd=3)
#segments(x0 = UV[7,1], y0 = UV[7,2], x1 = 0, y1 = 0, lty="dotted", lwd=3)

Z2 <- mutate(data.frame(Z),dv=dv)
m1<-lm(dv~tran + gdp + prepov -1,data=Z2)
summary(m1)
names<-c(variables,country.abbr)

p1 <- rio.plot(m1,include.int="no",r1=1:15,case.names=names[5:19],case.col="black",h.just=1.1)
pdat <- data.frame(x1.1 = p1$row.dimensions[1,1], x2.1 = p1$row.dimensions[1,2], y1 = 0, y2 = 0,
           x1.2= p1$row.dimensions[3,1], x2.2 = p1$row.dimensions[3,2])
p1$gg.obj + geom_segment(data=pdat,aes(x = x1.1, y = x2.1, xend = y1, yend = y2), colour = "black",size=1.5,linetype="dashed") +
  geom_segment(data=pdat,aes(x = x1.2, y = x2.2, xend = y1, yend = y2), colour = "black",size=1.5,linetype="dashed") +
  scale_x_continuous(limits=c(-2.7,3.1)) +   theme(axis.title = element_text(size = 16),
                                                   axis.text = element_text(size = 15),
                                                   legend.text= element_text(size=15))
# Fig 7.11


#########WIMMER, CEDERMAN, AND MIN RE-ANALYSIS########
###For Wimmer, Cederman and Min examples
rm(list=ls())
data("Wimmer_et_al_EPR")
aggregate.rows <- function(M, membs, myfun="sum") {
  step4 <- aggregate(M, by=list(membs), FUN=myfun)
  step5 <- step4[,-1]
  colnames(step5) <- colnames(M)
  return(step5) }


onsetstatus<-Wimmer_et_al_EPR$onsetstatus
lrexclpop<-Wimmer_et_al_EPR$lrexclpop
pimppast<-Wimmer_et_al_EPR$pimppast
egipgrps<-Wimmer_et_al_EPR$egipgrps
ethfrac<-Wimmer_et_al_EPR$ethfrac
gdpcapl<-Wimmer_et_al_EPR$gdpcapl
lpopl<-Wimmer_et_al_EPR$lpopl
ongoingwar<-Wimmer_et_al_EPR$ongoingwarl
year_wcm<-Wimmer_et_al_EPR$year
npeaceyears<-Wimmer_et_al_EPR$npeaceyears
spline1<-Wimmer_et_al_EPR$nspline1
spline2<-Wimmer_et_al_EPR$nspline2
spline3<-Wimmer_et_al_EPR$nspline3
onset_excl<-onsetstatus
onset_excl[which(onsetstatus==1)]<-0
onset_excl[which(onsetstatus==2)]<-1
country<-Wimmer_et_al_EPR$country
cowcode<-Wimmer_et_al_EPR$cowcode
year<-toString(year_wcm)
country.year<-paste(country,year_wcm, sep="_")
#Here I'll create the dataset for Wimmer analysis. There are a lot of missing values, which 
#will drop in the regression analysis, but for the RIO application I want to clean things up
#and drop the missing rows myself. So, I'm starting with Y and X in a single matrix, dropping
#all rows with missing values, then separating out Y and X for the regression later.
X.wcm<-cbind(country,year_wcm,cowcode,onset_excl,lrexclpop,egipgrps,pimppast, ethfrac,gdpcapl,lpopl,ongoingwar,year_wcm,npeaceyears, spline1,spline2,spline3)
X.wcm<-na.omit(X.wcm)
Y.wcm<-X.wcm[,4]
Y.wcm<-as.numeric(Y.wcm)
country.nomiss<-X.wcm[,1]
year.nomiss<-X.wcm[,2]
cowcode.nomiss<-X.wcm[,3]
X.wcm<-cbind(lrexclpop,egipgrps,pimppast, ethfrac,gdpcapl,lpopl,ongoingwar,year_wcm,npeaceyears, spline1,spline2,spline3)
X.wcm<-na.omit(X.wcm)
rownames(X.wcm)<-country.nomiss
Z.wcm<-scale(X.wcm)


#Table 7.9
t3.m2.wcm<-glm(onset_excl~lrexclpop+egipgrps+pimppast+ethfrac+gdpcapl+lpopl+ongoingwar+year_wcm+npeaceyears+spline1+spline2+spline3, family="binomial")
m1.wcm<-glm(Y.wcm~Z.wcm, family="binomial")
summary(t3.m2.wcm)

#Table 7.10  - Nielsen's Mahalanobis matching
#without dependent variable
X.wcm.df<-data.frame(X.wcm)
cowcode.nomiss<-as.numeric(cowcode.nomiss)
X.wcm.df<-cbind(X.wcm.df, cowcode.nomiss, country.nomiss)
countries.aggregate.wcm<-aggregate.rows(country.nomiss, cowcode.nomiss, "min")
X.wcm.maha<-aggregate.rows(X.wcm.df, country.nomiss, "mean")
X.wcm.maha<-cbind(X.wcm.maha,countries.aggregate.wcm)
dropvars<-c("spline1","spline2", "spline3", "country.nomiss", "cowcode.nomiss")
library(caseMatch)
out2<-case.match(data=X.wcm.maha, id.var="countries.aggregate.wcm", leaveout.vars = dropvars, distance="mahalanobis", case.N = 2, number.of.matches.to.return = 105, greedy.match="all")


#Figure 7.12a-c - Observations rather than cases
s.wcm<-svd(cbind(1, Z.wcm,m1.wcm$linear.predictors)) 
V.wcm<-s.wcm$v
U.wcm<-s.wcm$u
S.wcm<-diag(s.wcm$d)
UV.wcm<-rbind(V.wcm,U.wcm%*%S.wcm)

#7.12a
Z5 <- mutate(data.frame(Z.wcm),Y.wcm=Y.wcm)
m1.wcm<-glm(Y.wcm~ lrexclpop + egipgrps + pimppast +
              ethfrac + gdpcapl + lpopl + ongoingwar +
              year_wcm + npeaceyears + spline1 +
              spline2 + spline3, family="binomial",data=Z5)

rio.plot(m1.wcm,model.type="logit")$gg.obj +
  scale_x_continuous(limits = c(-.5,1.5))+   theme(axis.title = element_text(size = 16),
                                                   axis.text = element_text(size = 15),
                                                   legend.text= element_text(size=15))

#7.12b
rpwcm1<-rio.plot(m1.wcm,model.type="logit")
rdwcm<-data.frame(rpwcm1$row.dimensions)

rpwcm1$gg.obj + geom_point(data=rdwcm, aes(x=X1,y=X2), shape=1) + 
  scale_x_continuous(limits=c(-20,10))+   theme(axis.title = element_text(size = 16),
                                                axis.text = element_text(size = 15),
                                                legend.text= element_text(size=15))

#7.12c
rpwcm1<-rio.plot(m1.wcm,model.type="logit")
rdwcm<-data.frame(rpwcm1$row.dimensions)

rpwcm1$gg.obj + 
  geom_point(data=rdwcm[1:64,], aes(x=X1,y=X2), shape=1) +
  scale_x_continuous(limits=c(-15,15)) +   scale_y_continuous(limits=c(-5,10))+   theme(axis.title = element_text(size = 16),
                                                                                        axis.text = element_text(size = 15),
                                                                                        legend.text= element_text(size=15))


#Figure 7.13 - Plot of model with observations aggregated by case
rp1 <- rio.plot(m1.wcm,model.type="logit",h.just=1.2) 
rp1$gg.obj
rp1 <- rio.plot(m1.wcm,case.col="grey40",r1=1:nrow(m1.wcm$model),group.cases=country.nomiss,model.type="logit",h.just=.2) 
rp1$gg.obj + scale_x_continuous(limits=c(-14,4)) +
  scale_y_continuous(limits=c(-2.5,3))+   theme(axis.title = element_text(size = 16),
                                                axis.text = element_text(size = 15),
                                                legend.text= element_text(size=15))





#Figure 7.14 - Plot of model space, Kosovo Included
rd <- data.frame(rp1$row.dimensions)
rd <- mutate(rd,country=country.nomiss)
rd2 <- rd %>% group_by(country) %>% summarize(x=mean(no1),y=mean(no2))
rp1$gg.obj + 
  #geom_point(data=rd2,aes(x=x,y=y),shape=1) + 
  scale_y_continuous(limits=c(-15,20)) +   theme(axis.title = element_text(size = 16),
                                                 axis.text = element_text(size = 15),
                                                 legend.text= element_text(size=15))

#Figure 7.15 - Aggregated residuals for WCM
#m1.wcm<-glm(y~ EXCL + GROUP + IMP + FRAC + GDP + POP + WAR + YEAR + PEACE + S1 + S2 + S3, family="binomial",data=dat)
deviant.wcm<-abs(residuals(m1.wcm))
country.agg<-list(cowcode.nomiss)
deviant.wcm.agg<-aggregate(deviant.wcm, country.agg, "mean")
countries.aggregate.wcm<-aggregate.rows(country.nomiss, cowcode.nomiss, "min")
deviant.wcm.agg<-cbind(deviant.wcm.agg, countries.aggregate.wcm)


dat <- data.frame(cd=deviant.wcm.agg,country.names=countries.aggregate.wcm)
ggplot(dat,aes(x=country.names,y=cd.x))  + theme_classic() +
  labs(x="",y="Residuals") + 
  theme(axis.text.x=element_blank(), axis.ticks.x=element_blank()) +
  geom_hline(yintercept=0,color="black",linetype="solid") + 
  geom_bar(stat="identity",width=.01,color="grey60")+ geom_point() +
  geom_text_repel(data=dat[c(88,140),],aes(x=country.names,y=cd.x,label=country.names), nudge_y = 1, size=3.5)+   theme(axis.title = element_text(size = 16),
                                                                                                                        axis.text = element_text(size = 15),
                                                                                                                        legend.text= element_text(size=15))




#Figure 7.16 - Plot of model, most-similar cases highlighted
rp1 <- rio.plot(m1.wcm,model.type="logit",h.just=.2) 
which(rp1$row.dimensions[,1] < -20) # These two cases have super low linear predictions
rd <- data.frame(rp1$row.dimensions)
rd <- mutate(rd,country=country.nomiss)
rd <- data.frame(rd[-which(rd[,1] < -20),])
rd2 <- rd %>% group_by(country) %>% summarize(x=mean(X1),y=mean(X2))
segments<-data.frame(x1=0, x2=rd2[8,2], x3=rd2[28,2], x4=rd2[78,2], x5=rd2[85,2], x6=rd2[129,2], y1=0, y2=rd2[8,3], y3=rd2[28,3], y4=rd2[78,3], y5=rd2[85,3], y6=rd2[129,3])
require(ggrepel)
rp1$gg.obj + scale_x_continuous(limits=c(-10,1.5)) +
  geom_point(data=rd2[c(8,28,78,85,129),],aes(x=x,y=y),color="grey40") +
  geom_point(data=rd2[-c(8,28,78,85,129),],aes(x=x,y=y),color="grey30",shape=1,size=.5) +
  geom_text_repel(data=rd2[c(8,28,78,85,129),],aes(x=x,y=y,label=country), nudge_y=.5, hjust=.9) +
  geom_segment(data=segments,aes(x = x1, y = y1, xend = x, yend = y), colour = "black",size=1.5,linetype="dotted") +
  geom_segment(data=segments,aes(x = x1, y = y1, xend = x.1, yend = y.1), colour = "black",size=1.5,linetype="dotted")+
  geom_segment(data=segments,aes(x = x1, y = y1, xend = x.2, yend = y.2), colour = "black",size=1.5,linetype="dotted")+
  geom_segment(data=segments,aes(x = x1, y = y1, xend = x.3, yend = y.3), colour = "black",size=1.5,linetype="dotted")+
  geom_segment(data=segments,aes(x = x1, y = y1, xend = x.4, yend = y.4), colour = "black",size=1.5,linetype="dotted")+   theme(axis.title = element_text(size = 16),
                                                                                                                                axis.text = element_text(size = 15),
                                                                                                                                legend.text= element_text(size=15))





#Cosine similarity for Colombia and Cambodia
coord.austria<-as.numeric(c(rd2[8,2], rd2[8,3]))
coord.malaysia<-as.numeric(c(rd2[85,2], rd2[85,3]))
coord.colombia<-as.numeric(c(rd2[28,2], rd2[28,3]))
coord.sweden<-as.numeric(c(rd2[129,2], rd2[129,3]))
coord.lesotho<-as.numeric(c(rd2[78,2], rd2[78,3]))
cosine(coord.colombia,coord.malaysia)
cosine(coord.sweden, coord.austria)
cosine(coord.austria, coord.lesotho)
cosine(coord.austria, coord.sweden)


#Data for comparisons
X.for.comparison<-Wimmer_et_al_EPR[,c("country","onsetstatus","exclgrps","exclpop", "pimppast", "ethfrac", "ongoingwarl")]
