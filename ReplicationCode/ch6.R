
rm(list=ls()) #remove everything in the environment
data("GSS 2016")
require(tidyverse)

X <- na.omit(data.frame(educ=GSS.2016$educ,occprestige_father=GSS.2016$occprestige_father,female=GSS.2016$female,white=GSS.2016$white))
m1 <- lm(scale(educ) ~ scale(female) + scale(white) + scale(occprestige_father) -1, data=X)
summary(m1) # Table 6.1, Model 1
m1 <- lm(scale(educ) ~ scale(female)*scale(white) + scale(occprestige_father) -1, data=X)
summary(m1) # model 2

x <- as.matrix(scale(X[,2:4]))
y <- scale(X[,1])
b.long <- solve(t(x) %*% x) %*% t(x) %*% diag(as.numeric(y))
aggregate(t(b.long),by=list(X$white),FUN="sum") # Columns 2 and 3 in Table 6.1
 
round(aggregate(t(b.long*1000),by=list(X$white),FUN="mean"),4)

X2 <- data.frame(scale(X))
m1 <- lm(educ ~ female*white + occprestige_father -1, data=X2)
summary(m1)
require(emmeans)
ma1 <- as.data.frame(emmeans(m1,~female,at=list(white=0)))
ma2 <- as.data.frame(emmeans(m1,~female,at=list(white=1)))
mar1 <- rbind(ma1,ma2)
mar1 <- mutate(mar1,Gender=rep(c("Male","Female"),2))
mar1 <- mutate(mar1,xaxs=c(-.05,.05,.45,.55))
ggplot(mar1,aes(x=xaxs,y=emmean,ymin=lower.CL,ymax=upper.CL,shape=Gender)) +
  theme_classic() + geom_point(size=3) + geom_errorbar(width=.1) +
  labs(x="Race",y="Marginal Scaled Education") + 
  scale_x_continuous(breaks=c(0,.5),labels=c("Non-White","White")) +
  theme(axis.title = element_text(size = 16),
        axis.text = element_text(size = 15),
        legend.title = element_text(size=15),
        legend.text=element_text(size=14)) # Figure 6.1


X <- na.omit(data.frame(educ=GSS.2016$educ,occprestige_father=GSS.2016$occprestige_father,female=GSS.2016$female,white=GSS.2016$white,occprestige=GSS.2016$occprestige))
X <- mutate(X,feduc = rep(NA,nrow(X)))
X$feduc[X$educ<13]<-1
X$feduc[X$educ > 12 & X$educ < 16 ] <- 2
X$feduc[X$educ > 15 ] <- 3

mod1<- lm(occprestige ~ educ + occprestige_father + white + female , data=X )
summary(mod1) # Table 6.2, model 1

head(X)
p<-ggplot(X,aes(x=educ))
p + geom_histogram(bins=20)+ theme_classic() + scale_x_continuous(breaks=1:20) + 
  labs(x="Years of Schooling",y="Count of Respondents")+
  theme(axis.title = element_text(size = 16),
        axis.text = element_text(size = 15)) # Figure 6.2
p1<-p + geom_histogram(bins=20)+ theme_classic() + scale_x_continuous(breaks=1:20) + 
  labs(x="Years of Schooling",y="Count of Respondents")+
  theme(axis.title = element_text(size = 16),
        axis.text = element_text(size = 15))


x <- as.matrix(X[,c(1:2,4,3)])
x <- cbind(1,x)
y <- X[,5]
b.long <- solve(t(x) %*% x) %*% t(x) %*% diag(as.numeric(y))
(aggregate(t(b.long),by=list(X$feduc),FUN="sum"))
aggregate(t(b.long*1000),by=list(X$feduc),FUN="mean")


mod2<- lm(occprestige ~ educ*occprestige_father + female + white , data=X )
summary(mod2) # Table 6.2, Model 2


margins.mod3<- expand.grid(female = mean(X$female,na.rm=TRUE),white = mean(X$white,na.rm=TRUE),
                           occprestige_father = seq(from = min(X$occprestige_father,na.rm=TRUE), to = max(X$occprestige_father,na.rm=TRUE),length.out=10),
                           educ = quantile(X$educ))  
mod3.predictions <- predict(object=mod2,newdata=margins.mod3,interval="confidence") # Compute predicted values from the regression model, including confidence intervals
mod3whatever <- cbind(margins.mod3,mod3.predictions) # column bind them together
mod3whatever
mod3whatever$educ2 <- mod3whatever$educ
mod3whatever$educ2 <- recode_factor(mod3whatever$educ2,`1` = "1st %",`12` = "25st %",`13` = "50th %",`16` = "75th %",`20` = "99th %")

m3 <- ggplot(mod3whatever, mapping=aes(x=occprestige_father,y=fit,ymin=lwr,ymax=upr,group=educ2))
m3 + geom_line(aes(linetype=educ2)) + geom_ribbon(alpha=.2) + theme_classic() + labs(x="Paternal Occupational Prestige",y="Marginal Mean",linetype="Education",fill="Education") +
  theme(legend.position = "bottom") +
  theme(axis.title = element_text(size = 16),
        axis.text = element_text(size = 15),
        legend.title = element_text(size=15),
        legend.text = element_text(size=14)) # Figure 6.3

mod3<- lm(occprestige ~ educ*female + occprestige_father +  white , data=X )
summary(mod3)

m4 <- lm(occprestige ~ educ*female + occprestige_father +  white , data=X )
edlevs<- seq(from = min(X$educ,na.rm=TRUE), to = max(X$educ,na.rm=TRUE),length.out=10)

ma1 <- as.data.frame(emmeans(m4,~female,at=list(educ=edlevs[1])))
for(i in 2:length(edlevs)){
  ma2 <- as.data.frame(emmeans(m4,~female,at=list(educ=edlevs[i])))
  ma1 <- rbind(ma1,ma2)}
ma1 <- mutate(ma1,ed=rep(edlevs,each=2),Gender=rep(c("Male","Female"),10))
ggplot(ma1,aes(x=ed,y=emmean,ymin=lower.CL,ymax=upper.CL,group=Gender,linetype=Gender)) +
  theme_classic() + geom_line() + geom_ribbon(alpha=.2) +
  theme(legend.position = "bottom") + labs(x="Education",y="Marginal Occupational Prestige")  +
  theme(axis.title = element_text(size = 16),
        axis.text = element_text(size = 15),
        legend.title = element_text(size=15),
        legend.text = element_text(size=14)) # Figure 6.4




