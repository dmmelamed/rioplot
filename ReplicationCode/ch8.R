rm(list=ls())
#Figure 8.1
X<-c(0.5,1,2,2,2,2.5,3,3,3,3.5,3.5,4,4,4.5,4.5,5,5.5,5.5,6,6,6,6)
Y<-c(0,.5,1.5,1,1,2,1,2,2.5,3,0,2,3.5,2,1,3,5,4,3,2,5.5,0)
reg<-glm(Y~X)
plot(X,Y, pch=19, ylim=c(0,6), xlim=c(0,6))
segments(x0=-1,y0=-1,x1=7,y1=7)

dat <- data.frame(X,Y)
require(tidyverse)
ggplot(dat,aes(x=X,y=Y)) + geom_point(size=2) + theme_classic() + geom_abline(slope=1,intercept=0) +
  scale_x_continuous(limits=c(0,6),breaks=seq(0,6,2)) + scale_y_continuous(limits=c(0,6),breaks=seq(0,6,2)) +
  theme(axis.title = element_text(size = 15),
        axis.text = element_text(size = 15)) 


##########################################
##   SCHNEIDER AND MAKSZIN (2014)       ##
##########################################

#Data
require(rioplot)
rm(list=ls())
data("SchneiderAndMakszin06")
y05<-sm.data.05$fde
wcoord05<-sm.data.05$wcoord
govint05<-sm.data.05$govint
union.density05<-sm.data.05$ud
epl05<-sm.data.05$epl
socexp05<-sm.data.05$socexp
vocational205<-sm.data.05$vet_un
vocational305<-sm.data.05$vet_isced3
lmexp05<-sm.data.05$lmexp
turnout_reg05<-sm.data.05$vt_reg
turnout_age05<-sm.data.05$vt_vap
year05<-sm.data.05$year
eduexp05<-sm.data.05$eduexp
country05<-sm.data.05$country


###########################################
#   Cases as intersection of Variables:   #
###########################################
X<-cbind(lmexp05,socexp05)
Z<-scale(X)
rownames(Z)<-country05
colnames(Z)<-c("lmexp05","socexp05")

svdZ <- svd(Z)
U <- svdZ$u
V <- svdZ$v
S <- diag(svdZ$d)

dimnames <- paste("Dim", 1:2, sep="")
rownames(U) <- country05
colnames(U) <- dimnames
rownames(V) <- colnames(Z)
colnames(V) <- dimnames
rownames(S) <- colnames(S) <- dimnames

# Compute US = U %*% S
US <- U %*% S

# Table 8.2a-b:
round(US, digits=4)
round(V, digits = 4)
round(Z, digits=4)

#FOR IN-TEXT ONLY
# Let's consider Austria, which is the country
#  listed first in our dataset. Here is the
#  data (in Z-score form):
Z[1,]

# The first component will be Z[1,"tran"] * the "tran" row in V:
(comp1 <- Z[1,1] * V[1,])
(comp2 <- Z[1,2] * V[2,])
Z1V<-comp1+comp2


#Table 8.3
t1.m4<-lm(y05~union.density05+epl05+lmexp05+socexp05+wcoord05+govint05+turnout_age05)
summary(t1.m4)





#Table 8.4
cons<-rep(1,30)
X<-cbind(cons,union.density05,epl05,lmexp05,socexp05,wcoord05,govint05,turnout_age05)
y<-y05
b<-solve(t(X)%*%X)%*%t(X)%*%y #Matrix Solution for Coefficients
yhat = X%*%b
r = y-yhat
resid_sqrd = r*r
mse = (t(r)%*%r)/(dim(X)[1] - dim(X)[2])
mse = as.numeric(mse)
variance = mse*diag(solve(t(X)%*%X))
round(variance, 8)
se = sqrt(variance)
pvalue = summary(t1.m4)$coefficients[,4]

mse_j = resid_sqrd/(dim(X)[1] - dim(X)[2])
mse_j = round(mse_j,8)

var = matrix(0, 30, 8)
denom = dim(var)[1]-dim(var)[2]
for (i in 1:dim(var)[1]) {
  var[i,] = (resid_sqrd[i]/denom)*diag(solve(t(X)%*%X))
}
var=as.data.frame(var)
names(var)=c("Intercept","Union Density","EPL","LMExp","SocExp", "Wage Coord", "Gov Int", "Turnout")
var_j <- round(var,8) #variance by case

b.cases.sm<-solve(t(X)%*%X)%*%t(X)%*%diag(y) #Individual observations' contribution to overall coefficients

wftype1<-rep(0,30) 
wftype1[which(sm.data.05$p1_y>.5)]<-1 #Membership in first configuration identified by Schneider and Makszin
wftype2<-rep(0,30)
wftype2[which(sm.data.05$p2_y>.5)]<-1 #Membership in second configuration identified by Schneider and Makszin
wftype3<-rep(0,30)
wftype3[which(sm.data.05$p3_y>.5)]<-1 #Membership in third configuration identified by Schneider and Makszin

ref<-wftype1+wftype2+wftype3
wftype.in<-rep(0,30)
wftype.in[which(ref==0)]<-1 #Countries/cases that are more in than out of any of the three protective/supportive

wftn_coef<-t(aggregate(t(b.cases.sm),by=list(wftype.in),FUN=sum))[-1,] #Intensities/contributions by protective/supportive vs. non-protective/non-supportive
wftn_var<-t(aggregate(var_j,by=list(wftype.in),FUN=sum))[-1,] #Contributions to MSE by protective/supportive vs. non-protective/non-supportive

wftn_coef
wftn_coef <- cbind(wftn_coef,sqrt(wftn_var))
colnames(wftn_coef) <- rep(c("Supportive and Protective","Non-Supportive and Non-Protective"),2)
rownames(wftn_coef) <- c("Intercept","Union Density","EPL","Labor Expenditures as % of GDP","Social Expenditures as % of GDP","Wage Coordination","Govt Intervention in Wage Coord","Electoral Turnout")

wftn_coef <- data.frame(wftn_coef)
wftn_coef <- mutate(wftn_coef,item=rownames(wftn_coef))


wftn_coef2 <- data.frame(item=rep(wftn_coef$item,2),coef=c(wftn_coef$Supportive.and.Protective,wftn_coef$Non.Supportive.and.Non.Protective),
                         se=c(wftn_coef$Supportive.and.Protective.1,wftn_coef$Non.Supportive.and.Non.Protective.1),
                         group=rep(c("Supportive and Protective","Non-Supportive and Non-Protective"),each=8))
wftn_coef2 # Entries in Table 8.4

wftn_coef2$item <- factor(wftn_coef2$item,levels=rev(c("Union Density","EPL","Labor Expenditures as % of GDP","Social Expenditures as % of GDP","Wage Coordination","Govt Intervention in Wage Coord","Electoral Turnout","Intercept")))
require(ggrepel)

wftn_coef2 <- mutate(wftn_coef2,yaxs=(c(0,.98,.84,.7,.56,.42,.28,.14,.04,1.02,.88,.74,.6,.46,.32,.18)))
p1 <- ggplot(wftn_coef2,aes(x=coef,xmin=coef-1.96*se,xmax=coef+1.96*se,y=yaxs,color=group,shape=group,label=round(coef,2))) + 
  geom_pointrange(size=1.3) +
  theme_bw() +
  theme(legend.position="bottom",
        axis.title = element_text(size = 15),
        axis.text = element_text(size = 15),
        title = element_text(size = 15),
        legend.text= element_text(size=15)) + labs(color="",shape="",y="",x="Contributions to Regression Coefficients") +
  scale_color_manual(values=c("black","grey50")) +
  geom_text(vjust=-.7,
            hjust=c(0,0,0,0,1.2,0,0,1.2,0,0,0,0,0,0,0,0),size=5) +
  scale_y_continuous(breaks=c(.02,.16,.3,.44,.58,.72,.86,1),labels=c(rev(c("Union Density","EPL","Labor Expenditures as % of GDP","Social Expenditures as % of GDP","Wage Coordination","Govt Intervention in Wage Coord","Electoral Turnout","Intercept"))))
p1 #8.2


wftn_var
sqrt(wftn_var)




#############################
#   RAGIN AND FISS (2017)   #
#############################
rm(list=ls())
data("RaginData")
incrat<-raginfiss[,1]
pinc<-raginfiss[,2]
ped<-raginfiss[,3]
resp_ed<-raginfiss[,4]
afqt<-raginfiss[,5]
kids<-raginfiss[,6]
married<-raginfiss[,7]
black<-raginfiss[,8]
male<-raginfiss[,9]
povd<-raginfiss[,10]


#Table 8.5
#Create Table 4
m1<-glm(povd~afqt+pinc+ped+resp_ed+married+kids, family="binomial", data=subset(raginfiss, black==0 & male==1))
m2<-glm(povd~afqt+pinc+ped+resp_ed+married+kids, family="binomial", data=subset(raginfiss, black==0 & male==0))
m3<-glm(povd~afqt+pinc+ped+resp_ed+married+kids, family="binomial", data=subset(raginfiss, black==1 & male==1))
m4<-glm(povd~afqt+pinc+ped+resp_ed+married+kids, family="binomial", data=subset(raginfiss, black==1 & male==0))
m5<-glm(povd~afqt+pinc+ped+resp_ed+married+kids, family="binomial",data=raginfiss)
summary(m1)
summary(m2)
summary(m3)
summary(m4)
summary(m5) # Table 8.5

##require(sjPlot)
#tab_model(m1, m2, m3, m4, m5, show.se=TRUE, show.ci=FALSE, transform = NULL, auto.label = FALSE, digits = 3)

#Table 8.6
povdM1<-glm(povd~afqt+pinc+ped+resp_ed+married+kids, family="binomial",data=raginfiss)
X<-cbind(afqt,pinc,ped,resp_ed,married,kids)
cons<-rep(1,nrow(X))
X<-cbind(cons,X)
phat<-povdM1$fitted.values
W<-diag((phat*(1-phat)))
y<-povd
X<- as.matrix(X)
b <- as.numeric(povdM1$coefficients)
W <- as.matrix(W)
y<- as.matrix(y)
phat<- as.numeric(as.vector(phat))
Z<-as.numeric(X%*%b+solve(W)%*%(y-phat))
b.cases.rf<-solve(t(X)%*%X)%*%t(X)%*%diag(povdM1$linear.predictors)

r = povdM1$residuals
resid_sqrd = r*r
mse = (t(r)%*%r)/(dim(X)[1] - dim(X)[2])
mse = as.numeric(mse)
variance = mse*diag(solve(t(X)%*%X))
round(variance, 8)
se = sqrt(variance)
pvalue = summary(povdM1)$coefficients[,4]

mse_j = resid_sqrd/(dim(X)[1] - dim(X)[2])
mse_j = round(mse_j,8)

var = matrix(0, 4185, 7)
denom = dim(var)[1]-dim(var)[2]
for (i in 1:dim(var)[1]) {
  var[i,] = (resid_sqrd[i]/denom)*diag(solve(t(X)%*%X))
}
#var=as.data.frame(var)
##names(var)=c("Intercept","Union Density","EPL","LMExp","SocExp", "Wage Coord", "Gov Int", "Turnout")
#var_j <- round(var,8) #variance by case

inpov<-rep(0,4185)
inpov[which(raginfiss$povd==1)]<-1

inpov_coef<-t(aggregate(t(b.cases.rf),by=list(inpov),FUN=sum))[-1,] #subset contributions by values on the outcome
inpov_coef # Table 8.6

#Table 8.7
binpov<-rep(0,4185)
binpov[which(raginfiss$povd==1 & raginfiss$black==1)]<-1
winpov<-rep(0,4185)
winpov[which(raginfiss$povd==1 & raginfiss$black==0)]<-1
boutpov<-rep(0,4185)
boutpov[which(raginfiss$povd==0 & raginfiss$black==1)]<-1
woutpov<-rep(0,4185)
woutpov[which(raginfiss$povd==0 & raginfiss$black==0)]<-1

binpov_coef<-t(aggregate(t(b.cases.rf),by=list(binpov),FUN=sum))[-1,]
winpov_coef<-t(aggregate(t(b.cases.rf),by=list(winpov),FUN=sum))[-1,]
boutpov_coef<-t(aggregate(t(b.cases.rf),by=list(boutpov),FUN=sum))[-1,]
woutpov_coef<-t(aggregate(t(b.cases.rf),by=list(woutpov),FUN=sum))[-1,]

binpov.b<-binpov_coef[,2]
winpov.b<-winpov_coef[,2]
boutpov.b<-boutpov_coef[,2]
woutpov.b<-woutpov_coef[,2]

winpov.b
binpov.b
woutpov.b
boutpov.b

