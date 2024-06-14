
rm(list=ls())
library(tidyverse)
# install.packages("rioplot")
library(rioplot)
library(MASS) # To estimate the negative binomial regression models. 
# Load the file "Hilbe.RData"
data("Hilbe")

Hilbe<- mutate(Hilbe,hapmar=hapavg+vryhap,relig=smerel+vryrel,AFFAIRS=1*(naffairs>0))

# Logistic regression
m1<-glm(AFFAIRS~ yrsmarr4 + yrsmarr5 + yrsmarr6 + hapmar + relig ,data=Hilbe,family="binomial")
summary(m1) # Table 3.1

p1 <- rio.plot(m1,model.type = "logit",h.just=.3,v.just=c(-1.2,rep(-.6,5)),exclude.vars=1,col.names=c("Married 7 Years","Married 10 Years","Married 15 Years","Happy Marriage","Religious","AFFAIRS"))
p1$gg.obj + scale_x_continuous(limits=c(-.5,.85)) + scale_y_continuous(limits=c(-1,.25))
p1 <- p1$gg.obj + scale_x_continuous(limits=c(-.5,.85)) + scale_y_continuous(limits=c(-1,.25)) + theme(axis.title = element_text(size = 15),
              axis.text = element_text(size = 15))
p1 # Fig. 3.1

# In all dimensions, the SVD perfectly captures the regression coefficients
Xadj <- cbind(1,m1$model[,2:6],m1$linear.predictors)
svd2 <- svd(Xadj)
U2 <- svd2$u
V2 <- svd2$v
S2 <- diag(svd2$d)
a<-V2[7,1]*V2[1:6,1]+V2[7,2]*V2[1:6,2]+V2[7,3]*V2[1:6,3]+V2[7,4]*V2[1:6,4]+V2[7,5]*V2[1:6,5]+V2[7,6]*V2[1:6,6]
cor(coef(m1),a) # a is the second column in Table 3.1
a


# Poisson
m2<-glm(naffairs~ yrsmarr4 + yrsmarr5 + yrsmarr6 + hapmar + relig ,data=Hilbe,family=poisson(link="log"))
summary(m2) # Table 3.2
p2 <- rio.plot(m2,h.just=c(rep(-0.05,5),1.2),v.just=c(1.3,rep(0,4),1),model.type = "poisson",exclude.vars=1,col.names=c("Married 7 Years","Married 10 Years","Married 15 Years","Happy Marriage","Religious","AFFAIRS"))
p2 <- rio.plot(m2,v.just=c(1.5,-.5,-.3,-.3,1,-.2),h.just=c(rep(.5,3),rep(-0.05,2),.8),model.type = "poisson",exclude.vars=1,col.names=c("Married 7 Years","Married 10 Years","Married 15 Years","Happy Marriage","Religious","AFFAIRS"))
p2$gg.obj + scale_x_continuous(limits=c(-.6,.2)) + theme(axis.title = element_text(size = 15),
                                                         axis.text = element_text(size = 15))
p2 <-p2$gg.obj + scale_x_continuous(limits=c(-.6,.2)) + theme(axis.title = element_text(size = 15),
                                                              axis.text = element_text(size = 15))
p2 # Fig. 3.2


Xadj <- cbind(1,m2$model[,2:6],m2$linear.predictors)
svd2 <- svd(Xadj)
U2 <- svd2$u
V2 <- svd2$v
S2 <- diag(svd2$d)
a<-V2[7,1]*V2[1:6,1]+V2[7,2]*V2[1:6,2]+V2[7,3]*V2[1:6,3]+V2[7,4]*V2[1:6,4]+V2[7,5]*V2[1:6,5]+V2[7,6]*V2[1:6,6]
cor(coef(m2),a) # now a is the second column in 3.2


# Negative Binomial
m3<-glm.nb(naffairs~ yrsmarr4 + yrsmarr5 + yrsmarr6 + hapmar + relig ,data=Hilbe)
summary(m3) # Table 3.3
p3 <- rio.plot(m3,h.just=c(rep(-0.05,5),1.2),v.just=c(1.3,0,1,.8,1,-1),model.type = "nb",exclude.vars=1,col.names=c("Married 7 Years","Married 10 Years","Married 15 Years","Happy Marriage","Religious","AFFAIRS"))
p3 <- p3$gg.obj + scale_x_continuous(limits=c(-.6,.3)) + theme(axis.title = element_text(size = 15),
                                                                                                          axis.text = element_text(size = 15))
p3 # Fig. 3.3


Xadj <- cbind(1,m3$model[,2:6] ,log(m3$fitted.values))
svd2 <- svd(Xadj)
U2 <- svd2$u
V2 <- svd2$v
S2 <- diag(svd2$d)
a<-V2[7,1]*V2[1:6,1]+V2[7,2]*V2[1:6,2]+V2[7,3]*V2[1:6,3]+V2[7,4]*V2[1:6,4]+V2[7,5]*V2[1:6,5]+V2[7,6]*V2[1:6,6]
cor(coef(m3),a) # now a is the second column of 3.3


