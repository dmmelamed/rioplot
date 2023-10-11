rm(list=ls())
require(tidyverse)
require(plm)
require(rioplot)
#####################   BECKFIELD RE-ANALYSIS  #####################
#Beckfield (2006) Data
year<- 3899 + c(-1912,-1904,-1914,-1911,-1907,-1902,-1912,-1907,-1904,-1902,-1919,-1909,-1920,-1918,-1910,-1905,-1925,-1920,-1913,-1908,-1905,-1904,-1926,-1921,-1918,-1916,-1915,-1910,-1905,-1905,-1904,-1903,-1913,-1908,-1904,-1916,-1912,-1908,-1905,-1920,-1913,-1908,-1904,-1924,-1918,-1912,-1907,-1904)
polint<-c(0,2,8,31,17,17,5,3,8,7,0,6,18,17,28,35,1,8,8,13,23,21,37,46,41,36,38,47,37,2,3,0,5,36,56,19,19,17,12,0,0,0,0,0,0,0,0,8)
ecoint<-c(68.20874,66.21734,74.95358,78.75028,79.34477,75.46396,63.93915,66.60501,65.08947,65.19057,55.78206,74.71386,61.09775,55.80552,65.1618,63.91496,44.21205,51.44665,53.54802,61.54767,55.65159,56.48184,61.31574,58.89897,59.44844,61.13906,60.34082,66.41505,58.27416,74.48531,74.34239,71.27193,59.27832,64.80491,58.20404,79.09013,80.11526,81.59113,78.24973,79.63827,79.28141,78.79653,78.15234,57.29886,53.6151,55.70865,60.25546,59.14145)
ecoints<-c(4652.432,4384.736,5618.04,6201.607,6295.593,5694.809,4088.215,4436.228,4236.639,4249.811,3111.638,5582.161,3732.935,3114.256,4246.06,4085.122,1954.705,2646.758,2867.39,3788.116,3097.1,3190.198,3759.62,3469.089,3534.117,3737.985,3641.015,4410.959,3395.878,5548.062,5526.791,5079.688,3513.919,4199.676,3387.71,6255.249,6418.456,6657.112,6123.02,6342.253,6285.542,6208.894,6107.789,3283.159,2874.579,3103.454,3630.72,3497.711)
gdp<-c(17.40276,21.02415,16.9869,18.61667,20.32037,21.84525,20.94771,22.00813,23.66092,24.77634,11.51984,14.46919,15.9664,16.30102,19.49013,19.82194,13.28278,14.70124,16.37715,17.9484,19.12377,19.61006,13.58087,15.09322,15.87049,16.18005,16.73478,19.01055,20.72628,15.76118,17.26802,18.49438,16.77273,19.55091,20.29185,15.85393,17.60579,19.79963,20.3699,16.00574,19.94186,20.94067,23.89001,16.41058,17.01159,19.77045,19.73646,20.69604)
trans<-c(18.9,19.5,18.3,16.9,16.6,16.2,15.8,18.9,20.4,18.8,14.2,15.9,15.1,16.4,16.7,18.4,9.7,11.3,14.2,13.9,15.6,15.3,13.5,16.9,17.3,17.1,16.5,15.7,17.7,12.7,11.8,11.5,15.1,15.6,16.7,28.9,25.7,26,25.4,11.6,12.7,16.4,15.8,14.2,18,18.7,23.4,21.3)
outflo<-c(4.515128,5.672211,4.278111,6.821026,7.898127,7.419333,5.386508,6.644313,6.966607,7.329962,3.145135,5.457269,4.443552,5.258288,6.670434,6.872423,5.138331,6.154644,6.428186,6.365525,7.111818,7.371762,4.269769,4.97528,5.063889,4.853684,5.103009,6.239017,6.17287,5.722658,6.33213,6.177577,4.634419,5.724502,5.727164,6.506483,7.196592,7.5123,7.802735,3.115223,6.622483,6.731249,7.176234,4.664026,5.247028,6.986582,4.539943,7.861706)
gini<-c(22.7,27.7,22.7,23.2,22.4,25.0,25.4,23.6,26.3,25.7,31.8,30.3,29.3,28.8,28.7,28.8,26.8,27.0,30.3,33.6,33.9,34.4,27.1,26.4,24.4,26.0,24.9,24.7,26.1,33.3,33.6,32.5,30.6,28.9,34.2,26.0,25.6,26.6,25.3,22.3,23.3,23.1,23.8,21.5,19.7,21.8,22.9,22.1)
itcode<-c("AUT87","AUT95","BEL85", "BEL88", "BEL92", "BEL97", "DNK87", "DNK92", "DNK95","DNK97", "ESP80", "ESP90", "FRA79", "FRA81", "FRA89", "FRA94", "GBR74", "GBR79","GBR86", "GBR91", "GBR94", "GBR95", "GER73", "GER78", "GER81", "GER83", "GER84","GER89", "GER94", "IRL94", "IRL95", "IRL96", "ITA86", "ITA91", "ITA95", "NLD83","NLD87", "NLD91", "NLD94", "NOR79", "NOR86", "NOR91", "NOR95", "SWE75", "SWE81","SWE87", "SWE92", "SWE95")
countryid <- c(rep("aut",2),rep("bel",4),rep("dnk",4),rep("esp",2),rep("fra",4),
            rep("grb",6),rep("ger",7),rep("irl",3),rep("ita",3),rep("nld",4),
            rep("nor",4),rep("swe",5))

beck.dat <- data.frame(year,polint,ecoint,ecoints,gdp,trans,outflo,gini,countryid)

m1 <- plm(gini ~ year + polint + ecoint + ecoints + gdp + trans + outflo, data=beck.dat,model="within",index="countryid")
summary(m1) # Table 5.2, Model 1


dat <- beck.dat %>% group_by(countryid) %>% 
  mutate(year2=year-mean(year),
         polint=polint-mean(polint),
         ecoint=ecoint-mean(ecoint),
         ecoints= ecoints-mean(ecoints),
         gdp=gdp-mean(gdp),
         trans=trans-mean(trans),
         outflo=outflo-mean(outflo),
         y=gini-mean(gini))
m1.ols <- lm(y ~ year2 + polint + ecoint + ecoints + gdp + trans + outflo -1,data=dat)
# Coefficients are the same
cbind(coef(m1),coef(m1.ols))
# Standard errors are not
cbind(sqrt(diag(vcov(m1))),sqrt(diag(vcov(m1.ols))))

dat <- beck.dat[,1:8]
dat <- scale(dat)
dat <- mutate(data.frame(dat),countryid=countryid)
dat <- dat %>% group_by(countryid) %>% 
  mutate(year2=year-mean(year),
         polint=polint-mean(polint),
         ecoint=ecoint-mean(ecoint),
         ecoint2= ecoints-mean(ecoints),
         gdp=gdp-mean(gdp),
         trans=trans-mean(trans),
         outflo=outflo-mean(outflo),
         y=gini-mean(gini))
m1.ols.scaled <- lm(y ~ year2 + polint + ecoint + ecoint2 + gdp + trans + outflo -1,data=dat)
m1.scaled <- plm(y ~ year2 + polint + ecoint + ecoint2 + gdp + trans + outflo, data=dat,model="within",index="countryid")
summary(m1.scaled) # Table 5.2, Model 2
cbind(coef(m1.scaled),coef(m1.ols.scaled))

summary(m1) # Table 5.2, Model 1
summary(m1.scaled) # Table 5.2, Model 2

p1 <- rio.plot(m1.ols.scaled,include.int="no")
yhat.dat <- data.frame(p1$col.dimensions)

p2 <- ggplot() + geom_point(data=yhat.dat[8,],aes(x=X1,y=X2)) + 
  scale_x_continuous(limits=c(-3.2,2.5)) +
  theme_classic() + 
  scale_y_continuous(limits=c(-1.2,1.7)) +
  geom_vline(xintercept=0) +
  geom_hline(yintercept=0) + geom_abline(slope=yhat.dat[8,2]/yhat.dat[8,1],intercept=c(0,0)) +
  geom_text(data=yhat.dat[8,],aes(x=X1,y=X2,label="YHAT"),nudge_y=.04,color="grey25") + 
  labs(x="Dim 1",y="Dim 2")
p2

case.dat <- data.frame(p1$row.dimensions)
case.dat <- mutate(case.dat,Names=itcode)
p2 <- p2 + geom_point(data=case.dat,aes(x=X1,y=X2)) +
  geom_text(data=case.dat,aes(x=X1,y=X2,label=Names),nudge_y=-.04) + coord_fixed() +
  theme(axis.title = element_text(size = 14),
        axis.text = element_text(size = 14))
p2 # Figure 5.1

# Denmark
tp1 <- data.frame(rbind(unlist(project.point(case.dat[7,1:2],yhat.dat[8,])$newpoint),case.dat[7,1:2]))
tp2 <- data.frame(rbind(unlist(project.point(case.dat[8,1:2],yhat.dat[8,])$newpoint),case.dat[8,1:2]))
tp3 <- data.frame(rbind(unlist(project.point(case.dat[9,1:2],yhat.dat[8,])$newpoint),case.dat[9,1:2]))
tp4 <- data.frame(rbind(unlist(project.point(case.dat[10,1:2],yhat.dat[8,])$newpoint),case.dat[10,1:2]))
p2 + geom_point(data=case.dat[7:10,],aes(x=X1,y=X2)) +
  geom_text(data=case.dat[7:10,],aes(x=X1,y=X2,label=Names),nudge_y=-.05) +
  labs(title="Demark") +  coord_fixed() +
  geom_line(data=tp1,aes(x=X1,y=X2),color="grey65") +
  geom_line(data=tp2,aes(x=X1,y=X2),color="grey65") +
  geom_line(data=tp3,aes(x=X1,y=X2),color="grey65") +
  geom_line(data=tp4,aes(x=X1,y=X2),color="grey65") +
  theme(axis.title = element_text(size = 16),
        axis.text = element_text(size = 15),
        title = element_text(size=16))
f5.2.1 <- p2 + geom_point(data=case.dat[7:10,],aes(x=X1,y=X2)) +
  geom_text(data=case.dat[7:10,],aes(x=X1,y=X2,label=Names),nudge_y=-.05) +
  labs(title="Demark") +  coord_fixed() +
  geom_line(data=tp1,aes(x=X1,y=X2),color="grey65") +
  geom_line(data=tp2,aes(x=X1,y=X2),color="grey65") +
  geom_line(data=tp3,aes(x=X1,y=X2),color="grey65") +
  geom_line(data=tp4,aes(x=X1,y=X2),color="grey65") +
  theme(axis.title = element_text(size = 16),
        axis.text = element_text(size = 15),
        title = element_text(size=16))

tp1 <- data.frame(rbind(unlist(project.point(case.dat[17,1:2],yhat.dat[8,])$newpoint),case.dat[17,1:2]))
tp2 <- data.frame(rbind(unlist(project.point(case.dat[18,1:2],yhat.dat[8,])$newpoint),case.dat[18,1:2]))
tp3 <- data.frame(rbind(unlist(project.point(case.dat[19,1:2],yhat.dat[8,])$newpoint),case.dat[19,1:2]))
tp4 <- data.frame(rbind(unlist(project.point(case.dat[20,1:2],yhat.dat[8,])$newpoint),case.dat[20,1:2]))
tp5 <- data.frame(rbind(unlist(project.point(case.dat[21,1:2],yhat.dat[8,])$newpoint),case.dat[21,1:2]))
tp6 <- data.frame(rbind(unlist(project.point(case.dat[22,1:2],yhat.dat[8,])$newpoint),case.dat[22,1:2]))
p2 + geom_point(data=case.dat[17:22,],aes(x=X1,y=X2)) +
  geom_text(data=case.dat[17:22,],aes(x=X1,y=X2,label=Names),nudge_y=-.05) +
  labs(title="Great Britain") +  coord_fixed() +
  geom_line(data=tp1,aes(x=X1,y=X2),color="grey65") +
  geom_line(data=tp2,aes(x=X1,y=X2),color="grey65") +
  geom_line(data=tp3,aes(x=X1,y=X2),color="grey65") +
  geom_line(data=tp4,aes(x=X1,y=X2),color="grey65") +
  geom_line(data=tp5,aes(x=X1,y=X2),color="grey65") +
  geom_line(data=tp6,aes(x=X1,y=X2),color="grey65")  +
  theme(axis.title = element_text(size = 16),
        axis.text = element_text(size = 15),
        title = element_text(size=16))
f5.2.2 <- p2 + geom_point(data=case.dat[17:22,],aes(x=X1,y=X2)) +
  geom_text(data=case.dat[17:22,],aes(x=X1,y=X2,label=Names),nudge_y=-.05) +
  labs(title="Great Britain") +  coord_fixed() +
  geom_line(data=tp1,aes(x=X1,y=X2),color="grey65") +
  geom_line(data=tp2,aes(x=X1,y=X2),color="grey65") +
  geom_line(data=tp3,aes(x=X1,y=X2),color="grey65") +
  geom_line(data=tp4,aes(x=X1,y=X2),color="grey65") +
  geom_line(data=tp5,aes(x=X1,y=X2),color="grey65") +
  geom_line(data=tp6,aes(x=X1,y=X2),color="grey65")  +
  theme(axis.title = element_text(size = 16),
        axis.text = element_text(size = 15),
        title = element_text(size=16))
  
  
  
tp1 <- data.frame(rbind(unlist(project.point(case.dat[11,1:2],yhat.dat[8,])$newpoint),case.dat[11,1:2]))
tp2 <- data.frame(rbind(unlist(project.point(case.dat[12,1:2],yhat.dat[8,])$newpoint),case.dat[12,1:2]))
p2 + geom_point(data=case.dat[11:12,],aes(x=X1,y=X2)) +
  geom_text(data=case.dat[11:12,],aes(x=X1,y=X2,label=Names),nudge_y=-.05) + 
  labs(title="Spain") + coord_fixed() +
  geom_line(data=tp1,aes(x=X1,y=X2),color="grey65") +
  geom_line(data=tp2,aes(x=X1,y=X2),color="grey65")  +
  theme(axis.title = element_text(size = 16),
        axis.text = element_text(size = 15),
        title = element_text(size=16)) 
f5.2.3 <- p2 + geom_point(data=case.dat[11:12,],aes(x=X1,y=X2)) +
  geom_text(data=case.dat[11:12,],aes(x=X1,y=X2,label=Names),nudge_y=-.05) + 
  labs(title="Spain") + coord_fixed() +
  geom_line(data=tp1,aes(x=X1,y=X2),color="grey65") +
  geom_line(data=tp2,aes(x=X1,y=X2),color="grey65")  +
  theme(axis.title = element_text(size = 16),
        axis.text = element_text(size = 15),
        title = element_text(size=16)) 

tp1 <- data.frame(rbind(unlist(project.point(case.dat[33,1:2],yhat.dat[8,])$newpoint),case.dat[33,1:2]))
tp2 <- data.frame(rbind(unlist(project.point(case.dat[34,1:2],yhat.dat[8,])$newpoint),case.dat[34,1:2]))
tp3 <- data.frame(rbind(unlist(project.point(case.dat[35,1:2],yhat.dat[8,])$newpoint),case.dat[35,1:2]))
p2 + geom_point(data=case.dat[33:35,],aes(x=X1,y=X2)) +
  geom_text(data=case.dat[33:35,],aes(x=X1,y=X2,label=Names),nudge_y=-.05)+ 
  labs(title="Italy") +  coord_fixed() +
  geom_line(data=tp1,aes(x=X1,y=X2),color="grey65") +
  geom_line(data=tp2,aes(x=X1,y=X2),color="grey65") +
  geom_line(data=tp3,aes(x=X1,y=X2),color="grey65") +
  theme(axis.title = element_text(size = 16),
        axis.text = element_text(size = 15),
        title = element_text(size=16)) 
f5.2.4 <- p2 + geom_point(data=case.dat[33:35,],aes(x=X1,y=X2)) +
  geom_text(data=case.dat[33:35,],aes(x=X1,y=X2,label=Names),nudge_y=-.05)+ 
  labs(title="Italy") +  coord_fixed() +
  geom_line(data=tp1,aes(x=X1,y=X2),color="grey65") +
  geom_line(data=tp2,aes(x=X1,y=X2),color="grey65") +
  geom_line(data=tp3,aes(x=X1,y=X2),color="grey65") +
  theme(axis.title = element_text(size = 16),
        axis.text = element_text(size = 15),
        title = element_text(size=16)) 
require(ggpubr)
f5.2.revised <- ggarrange(f5.2.1,f5.2.2,f5.2.3,f5.2.4,nrow=2,ncol=2)
f5.2.revised # Figure 5.2


#p3<- p2 + geom_point(data=case.dat[7:10,],aes(x=X1,y=X2)) +
#  geom_text(data=case.dat[7:10,],aes(x=X1,y=X2,label=Names),nudge_y=-.05)
#p4 <- p2 + geom_point(data=case.dat[11:12,],aes(x=X1,y=X2)) +
#  geom_text(data=case.dat[11:12,],aes(x=X1,y=X2,label=Names),nudge_y=-.05)
#p5 <- p2 + geom_point(data=case.dat[33:35,],aes(x=X1,y=X2)) +
#  geom_text(data=case.dat[33:35,],aes(x=X1,y=X2,label=Names),nudge_y=-.05)
#p6 <- p2 + geom_point(data=case.dat[17:22,],aes(x=X1,y=X2)) +
#  geom_text(data=case.dat[17:22,],aes(x=X1,y=X2,label=Names),nudge_y=-.05)

#tp1 <- data.frame(rbind(unlist(project.point(case.dat[7,1:2],yhat.dat[8,])$newpoint),case.dat[7,1:2]))
#tp2 <- data.frame(rbind(unlist(project.point(case.dat[8,1:2],yhat.dat[8,])$newpoint),case.dat[8,1:2]))
#tp3 <- data.frame(rbind(unlist(project.point(case.dat[9,1:2],yhat.dat[8,])$newpoint),case.dat[9,1:2]))
#tp4 <- data.frame(rbind(unlist(project.point(case.dat[10,1:2],yhat.dat[8,])$newpoint),case.dat[10,1:2]))
#p3 + labs(title="Demark") +  coord_fixed() +
#  geom_line(data=tp1,aes(x=X1,y=X2),color="grey65") +
#  geom_line(data=tp2,aes(x=X1,y=X2),color="grey65") +
#  geom_line(data=tp3,aes(x=X1,y=X2),color="grey65") +
#  geom_line(data=tp4,aes(x=X1,y=X2),color="grey65") 

#p2.1<- p3 + labs(title="Demark") +  coord_fixed() +
#         geom_line(data=tp1,aes(x=X1,y=X2),color="grey65") +
#         geom_line(data=tp2,aes(x=X1,y=X2),color="grey65") +
#         geom_line(data=tp3,aes(x=X1,y=X2),color="grey65") +
#         geom_line(data=tp4,aes(x=X1,y=X2),color="grey65")

#tp1 <- data.frame(rbind(unlist(project.point(case.dat[17,1:2],yhat.dat[8,])$newpoint),case.dat[17,1:2]))
#tp2 <- data.frame(rbind(unlist(project.point(case.dat[18,1:2],yhat.dat[8,])$newpoint),case.dat[18,1:2]))
#tp3 <- data.frame(rbind(unlist(project.point(case.dat[19,1:2],yhat.dat[8,])$newpoint),case.dat[19,1:2]))
#tp4 <- data.frame(rbind(unlist(project.point(case.dat[20,1:2],yhat.dat[8,])$newpoint),case.dat[20,1:2]))
#tp5 <- data.frame(rbind(unlist(project.point(case.dat[21,1:2],yhat.dat[8,])$newpoint),case.dat[21,1:2]))
#tp6 <- data.frame(rbind(unlist(project.point(case.dat[22,1:2],yhat.dat[8,])$newpoint),case.dat[22,1:2]))
#p6 + labs(title="Great Britain") +  coord_fixed() +
#  geom_line(data=tp1,aes(x=X1,y=X2),color="grey65") +
#  geom_line(data=tp2,aes(x=X1,y=X2),color="grey65") +
#  geom_line(data=tp3,aes(x=X1,y=X2),color="grey65") +
#  geom_line(data=tp4,aes(x=X1,y=X2),color="grey65") +
#  geom_line(data=tp5,aes(x=X1,y=X2),color="grey65") +
#  geom_line(data=tp6,aes(x=X1,y=X2),color="grey65") 

#p2.2<- p6 + labs(title="Great Britain") +  coord_fixed() +
#         geom_line(data=tp1,aes(x=X1,y=X2),color="grey65") +
#         geom_line(data=tp2,aes(x=X1,y=X2),color="grey65") +
#         geom_line(data=tp3,aes(x=X1,y=X2),color="grey65") +
#         geom_line(data=tp4,aes(x=X1,y=X2),color="grey65") +
#         geom_line(data=tp5,aes(x=X1,y=X2),color="grey65") +
#         geom_line(data=tp6,aes(x=X1,y=X2),color="grey65")


#tp1 <- data.frame(rbind(unlist(project.point(case.dat[11,1:2],yhat.dat[8,])$newpoint),case.dat[11,1:2]))
#tp2 <- data.frame(rbind(unlist(project.point(case.dat[12,1:2],yhat.dat[8,])$newpoint),case.dat[12,1:2]))
#p4 + labs(title="Spain") + coord_fixed() +
#  geom_line(data=tp1,aes(x=X1,y=X2),color="grey65") +
#  geom_line(data=tp2,aes(x=X1,y=X2),color="grey65") 
#p2.3<- p4 + labs(title="Spain") + coord_fixed() +
#         geom_line(data=tp1,aes(x=X1,y=X2),color="grey65") +
#         geom_line(data=tp2,aes(x=X1,y=X2),color="grey65")


#tp1 <- data.frame(rbind(unlist(project.point(case.dat[33,1:2],yhat.dat[8,])$newpoint),case.dat[33,1:2]))
#tp2 <- data.frame(rbind(unlist(project.point(case.dat[34,1:2],yhat.dat[8,])$newpoint),case.dat[34,1:2]))
#tp3 <- data.frame(rbind(unlist(project.point(case.dat[35,1:2],yhat.dat[8,])$newpoint),case.dat[35,1:2]))
#p5 + labs(title="Italy") +  coord_fixed() +
#  geom_line(data=tp1,aes(x=X1,y=X2),color="grey65") +
#  geom_line(data=tp2,aes(x=X1,y=X2),color="grey65") +
#  geom_line(data=tp3,aes(x=X1,y=X2),color="grey65") 
#p2.4<- p5 + labs(title="Italy") +  coord_fixed() +
#         geom_line(data=tp1,aes(x=X1,y=X2),color="grey65") +
#         geom_line(data=tp2,aes(x=X1,y=X2),color="grey65") +
#         geom_line(data=tp3,aes(x=X1,y=X2),color="grey65")
#require(ggpubr)
#p2 <- ggarrange(p2.1,p2.2,p2.3,p2.4,nrow=2,ncol=2)

#ggsave(p2.1 ,
#       file="Figure5.2.1.eps", device=cairo_ps)
#ggsave(p2.2 ,
#       file="Figure5.2.2.eps", device=cairo_ps)
#ggsave(p2.3 ,
#       file="Figure5.2.3.eps", device=cairo_ps)
#ggsave(p2.4 ,
#       file="Figure5.2.4.eps", device=cairo_ps)

# Decomposition
dat <- beck.dat %>% group_by(countryid) %>% 
  mutate(year2=year-mean(year),
         polint=polint-mean(polint),
         ecoint=ecoint-mean(ecoint),
         ecoints= ecoints-mean(ecoints),
         gdp=gdp-mean(gdp),
         trans=trans-mean(trans),
         outflo=outflo-mean(outflo),
         y=gini-mean(gini))
m1.ols <- lm(y ~ year2 + polint + ecoint + ecoints + gdp + trans + outflo -1,data=dat)
X <- cbind(dat$year2,dat$polint,dat$ecoint,dat$ecoints,dat$gdp,dat$trans,dat$outflo)
B <- solve(t(X) %*% X) %*% t(X) %*%  diag(dat$y)
cbind(coef(m1.ols),rowSums(B))

Groups <- rep("Others",48)
Groups[33:35] <- "Italy"
Groups[17:22] <- "Great Britain"
b.parts<-aggregate(t(B),by=list(Groups),FUN="sum")
colnames(b.parts)[2:8]<-names(coef(m1.ols))
b.parts # Table 5.3
cbind(coef(m1.ols),colSums(b.parts[,2:8]))



beck.dat <- data.frame(year,polint,ecoint,ecoints,gdp,trans,outflo,gini,countryid)

m1 <- plm(gini ~ year + polint + ecoint + ecoints + gdp + trans + outflo, data=beck.dat,model="within",index="countryid")
summary(m1) # Table 5.4, Model 1
m2 <- plm(gini ~ year + polint + ecoint + ecoints + gdp + trans + outflo, beck.dat %>% filter(countryid != "grb" , countryid !="ita"),model="within",index="countryid")
summary(m2) # Table 5.4, Model 2


# Cook's D
cooksd<-cooks.distance(m1.ols)
cooksd <- data.frame(cooksd=cooksd,Names=itcode)
p1 <-ggplot(cooksd,aes(x=Names,y=cooksd)) + theme_classic() +
  geom_bar(stat="identity",width=.01,color="grey60") + geom_point() + theme(axis.text.x=element_blank(),axis.ticks = element_blank()) 
p1 + geom_hline(yintercept=4/39,color="black",linetype="dashed") +
  geom_text(data=cooksd[which(cooksd[,1]>4/39),],aes(x=Names,y=cooksd,label=c("GER","ITA")),hjust=1.2) +
  labs(x="",y="Cook's Distance") +   theme(axis.title = element_text(size = 16),
                                           axis.text = element_text(size = 15)) 
# Fig. 5.3

m1 <- plm(gini ~ year + polint + ecoint + ecoints + gdp + trans + outflo, beck.dat %>% filter( countryid !="ita"),model="within",index="countryid")
summary(m1) # Table 5.5, Model 1
m2 <- plm(gini ~ year + polint + ecoint + ecoints + gdp + trans + outflo, beck.dat %>% filter( countryid !="grb"),model="within",index="countryid")
summary(m2) # Table 5.5, Model 2




# Switch to random effect model
require(lme4)
# Model 4 in Table 1 of Beckfield's paper
m1 <- lmer (gini ~ year + polint + ecoint + ecoints + gdp + trans + outflo + 
              (1|countryid),data=beck.dat)
summary(m1) # Table 5.2, MOdel 3


# Standardized version of the model above
m2 <- lmer (scale(gini) ~ scale(year) + scale(polint) + scale(ecoint) + scale(ecoints) + scale(gdp) + scale(trans) + scale(outflo) + 
              (1|countryid),data=beck.dat)
summary(m2) # Table 5.2, Model 4


X <- cbind(1,scale(beck.dat$year),scale(beck.dat$polint),scale(beck.dat$ecoint),scale(beck.dat$ecoints),scale(beck.dat$gdp),scale(beck.dat$trans),scale(beck.dat$outflo))
X <- cbind(X,Yhat=predict(m2,re.form=NA))

svd2 <- svd(X)
U2 <- svd2$u
V2 <- svd2$v
V2 <- data.frame(V2)
V2 <- mutate(V2,Names=c(names(fixef(m1))[1:8],"YHAT"))
U2 <- data.frame(U2)
U2 <- mutate(U2,Names=itcode)
p1 <- ggplot() + geom_point(data=V2[9,],aes(x=X1,y=X2)) + geom_text(data=V2[9,],aes(x=X1,y=X2,label=Names),color="grey25") + theme_classic() + 
  geom_hline(yintercept=0) + geom_vline(xintercept=0) + labs(x="Dim 1",y="Dim 2") + scale_x_continuous(limits=c(-.6,.6)) +
  scale_y_continuous(limits=c(-.6,.6)) + geom_abline(slope=V2[9,2]/V2[9,1],intercept=c(0,0))

p3 <- p1 +  geom_point(data=U2[7:10,],aes(x=X1,y=X2)) + 
  geom_text(data=U2[7:10,],aes(x=X1,y=X2,label=Names))

p4 <- p1 +  geom_point(data=U2[11:12,],aes(x=X1,y=X2)) + 
  geom_text(data=U2[11:12,],aes(x=X1,y=X2,label=Names))

p5 <- p1 +  geom_point(data=U2[33:35,],aes(x=X1,y=X2)) + 
  geom_text(data=U2[33:35,],aes(x=X1,y=X2,label=Names))

p6 <- p1 +  geom_point(data=U2[17:22,],aes(x=X1,y=X2)) + 
  geom_text(data=U2[17:22,],aes(x=X1,y=X2,label=Names))
#ggarrange(p3,p4,p5,p6,nrow=2,ncol=2,labels=c("Denmark","Spain","Italy","Great Britain"),font.label=list(size=10),label.x=.6)
p3
tp1 <- data.frame(rbind(unlist(project.point(U2[7,1:2],V2[9,1:2])$newpoint),U2[7,1:2]))
tp2 <- data.frame(rbind(unlist(project.point(U2[8,1:2],V2[9,1:2])$newpoint),U2[8,1:2]))
tp3 <- data.frame(rbind(unlist(project.point(U2[9,1:2],V2[9,1:2])$newpoint),U2[9,1:2]))
tp4 <- data.frame(rbind(unlist(project.point(U2[10,1:2],V2[9,1:2])$newpoint),U2[10,1:2]))
p3 + labs(title="Demark") +  coord_fixed() +
  geom_line(data=mutate(tp1,Names=NA),aes(x=X1,y=X2),color="grey65")+
  geom_line(data=tp2,aes(x=X1,y=X2),color="grey65") +
  geom_line(data=tp3,aes(x=X1,y=X2),color="grey65") +
  geom_line(data=tp4,aes(x=X1,y=X2),color="grey65") +
  theme(axis.title = element_text(size = 16),
        axis.text = element_text(size = 15),
        title = element_text(size=16)) 
f5.4.1 <- p3 + labs(title="Demark") +  coord_fixed() +
  geom_line(data=mutate(tp1,Names=NA),aes(x=X1,y=X2),color="grey65")+
  geom_line(data=tp2,aes(x=X1,y=X2),color="grey65") +
  geom_line(data=tp3,aes(x=X1,y=X2),color="grey65") +
  geom_line(data=tp4,aes(x=X1,y=X2),color="grey65") +
  theme(axis.title = element_text(size = 16),
        axis.text = element_text(size = 15),
        title = element_text(size=16)) 



tp1 <- data.frame(rbind(unlist(project.point(U2[11,1:2],V2[9,1:2])$newpoint),U2[11,1:2]))
tp2 <- data.frame(rbind(unlist(project.point(U2[12,1:2],V2[9,1:2])$newpoint),U2[12,1:2]))
p4 + labs(title="Spain") +  coord_fixed() +
  geom_line(data=mutate(tp1,Names=NA),aes(x=X1,y=X2),color="grey65")+
  geom_line(data=tp2,aes(x=X1,y=X2),color="grey65") + 
  theme(axis.title = element_text(size = 16),
        axis.text = element_text(size = 15),
        title = element_text(size=16)) 
f5.4.2 <- p4 + labs(title="Spain") +  coord_fixed() +
  geom_line(data=mutate(tp1,Names=NA),aes(x=X1,y=X2),color="grey65")+
  geom_line(data=tp2,aes(x=X1,y=X2),color="grey65") + 
  theme(axis.title = element_text(size = 16),
        axis.text = element_text(size = 15),
        title = element_text(size=16)) 
  

tp1 <- data.frame(rbind(unlist(project.point(U2[33,1:2],V2[9,1:2])$newpoint),U2[33,1:2]))
tp2 <- data.frame(rbind(unlist(project.point(U2[34,1:2],V2[9,1:2])$newpoint),U2[34,1:2]))
tp3 <- data.frame(rbind(unlist(project.point(U2[35,1:2],V2[9,1:2])$newpoint),U2[35,1:2]))
p5 + labs(title="Italy") +  coord_fixed() +
  geom_line(data=mutate(tp1,Names=NA),aes(x=X1,y=X2),color="grey65")+
  geom_line(data=tp2,aes(x=X1,y=X2),color="grey65") +
  geom_line(data=tp3,aes(x=X1,y=X2),color="grey65")  + 
  theme(axis.title = element_text(size = 16),
        axis.text = element_text(size = 15),
        title = element_text(size=16))
f5.4.3 <- p5 + labs(title="Italy") +  coord_fixed() +
  geom_line(data=mutate(tp1,Names=NA),aes(x=X1,y=X2),color="grey65")+
  geom_line(data=tp2,aes(x=X1,y=X2),color="grey65") +
  geom_line(data=tp3,aes(x=X1,y=X2),color="grey65")  + 
  theme(axis.title = element_text(size = 16),
        axis.text = element_text(size = 15),
        title = element_text(size=16))

tp1 <- data.frame(rbind(unlist(project.point(U2[17,1:2],V2[9,1:2])$newpoint),U2[17,1:2]))
tp2 <- data.frame(rbind(unlist(project.point(U2[18,1:2],V2[9,1:2])$newpoint),U2[18,1:2]))
tp3 <- data.frame(rbind(unlist(project.point(U2[19,1:2],V2[9,1:2])$newpoint),U2[19,1:2]))
tp4 <- data.frame(rbind(unlist(project.point(U2[20,1:2],V2[9,1:2])$newpoint),U2[20,1:2]))
tp5 <- data.frame(rbind(unlist(project.point(U2[21,1:2],V2[9,1:2])$newpoint),U2[21,1:2]))
tp6 <- data.frame(rbind(unlist(project.point(U2[22,1:2],V2[9,1:2])$newpoint),U2[22,1:2]))
p6 + labs(title="Great Britain") +  coord_fixed() +
  geom_line(data=mutate(tp1,Names=NA),aes(x=X1,y=X2),color="grey65")+
  geom_line(data=tp2,aes(x=X1,y=X2),color="grey65") +
  geom_line(data=tp3,aes(x=X1,y=X2),color="grey65") +
  geom_line(data=tp4,aes(x=X1,y=X2),color="grey65") +
  geom_line(data=tp5,aes(x=X1,y=X2),color="grey65") +
  geom_line(data=tp6,aes(x=X1,y=X2),color="grey65")   + 
  theme(axis.title = element_text(size = 16),
        axis.text = element_text(size = 15),
        title = element_text(size=16))
f5.4.4 <- p6 + labs(title="Great Britain") +  coord_fixed() +
  geom_line(data=mutate(tp1,Names=NA),aes(x=X1,y=X2),color="grey65")+
  geom_line(data=tp2,aes(x=X1,y=X2),color="grey65") +
  geom_line(data=tp3,aes(x=X1,y=X2),color="grey65") +
  geom_line(data=tp4,aes(x=X1,y=X2),color="grey65") +
  geom_line(data=tp5,aes(x=X1,y=X2),color="grey65") +
  geom_line(data=tp6,aes(x=X1,y=X2),color="grey65")   + 
  theme(axis.title = element_text(size = 16),
        axis.text = element_text(size = 15),
        title = element_text(size=16))
require(ggpubr)
ggarrange(f5.4.1,f5.4.3,f5.4.2,f5.4.4,nrow=2,ncol=2) # Fig. 5.4




# Decompose his model
# Model 4 in Table 1 of Beckfield's paper
#m1 <- lmer (gini ~ year + polint + ecoint + ecoints + gdp + trans + outflo + 
#              (1|countryid),data=beck.dat)
#y <- beck.dat$gini
#X <- cbind(1,beck.dat$year,beck.dat$polint,beck.dat$ecoint,beck.dat$ecoints,beck.dat$gdp,beck.dat$trans,beck.dat$outflo)
#Z <- matrix(0,nr=nrow(X),nc=length(unique(countryid)))
#for(i in 1:nrow(Z)){
#  Z[i,which(countryid[i]==unique(countryid))]<-1}
#var.comps<-data.frame(VarCorr(m1))[,c(1,4)]
#V <- Z %*% diag(rep(var.comps[1,2],12)) %*% t(Z) + diag(rep(var.comps[2,2],48))
#require(MASS)
# Linear solution to the mixed model coefficients
#solve(t(X) %*% ginv(V) %*% X) %*% t(X) %*% ginv(V) %*% y
# Same as the solutions from lme4
#cbind(fixef(m1),solve(t(X) %*% ginv(V) %*% X) %*% t(X) %*% ginv(V) %*% y)
# Decompose by case
#B <- solve(t(X) %*% ginv(V) %*% X) %*% t(X) %*% ginv(V) %*% diag(y)
#Groups <- rep("Others",48)
#Groups[33:35] <- "Italy"
#Groups[17:22] <- "Great Britain"
#b.parts<-aggregate(t(B),by=list(Groups),FUN="sum")
#colnames(b.parts)[2:9]<-names(fixef(m1))
#b.parts
#colSums(b.parts[,2:9])
#fixef(m1)

m1 <- lmer (gini ~ year + polint + ecoint + ecoints + gdp + trans + outflo + 
              (1|countryid),data=beck.dat)

require(HLMdiag)

cd2 <- hlm_influence(m1, level = "countryid") # At the cluster level

p1 <-ggplot(cd2,aes(x=toupper(countryid),y=cooksd)) + theme_classic() +
  geom_bar(stat="identity",width=.01,color="grey60") + geom_point() +
  labs(x="",y="Level 2 Cook's Distance")
p1 + geom_hline(yintercept=4/12,color="black",linetype="dashed")  +
  theme(axis.title = element_text(size = 16),
        axis.text = element_text(size = 15)) # Fig. 5.5


#####################   RAMBOTTI AND BREIGER (2020) RE-ANALYSIS  #####################
rm(list=ls())

#Rambotti and Breiger (2020) data
zinequality <- c(1.1244433,-0.50872159,-0.67766958,0.091982834,0.16707116,-0.2271411,-0.90293348,0.035666954,-1.33469,0.073210754,1.2558472,0.52373892,0.41110712,0.95549536,0.86163527,-1.5787259,-0.15205313,-1.2408297,-1.1657417,2.2883074)
zpoverty <- c(-0.037866674,-0.76168096,-0.67732799,0.81118757,-0.44784385,-0.60439408,-1.1635544,0.82629001,-1.1013026,-0.52556658,-0.10601201,0.48003793,0.1916173,2.4566219,0.51871502,0.40452552,-0.66222554,-0.6600154,-1.0563636,2.1151583)
zinteraction <- c(-0.56928205,-0.21050778,-0.15084223,-0.47151417,-0.5961802,-0.41923466,0.34269899,-0.50917506,0.69248253,-0.56586009,-0.64482725,-0.32402161,-0.46804377,1.4244366,-0.16090429,-1.0665343,-0.4497588,0.1494517,0.49355826,3.5040584)
zlife <- c(0.47719216,-0.15906498,0.11362062,0.56809199,0.38629928,-0.34085077,-1.8860576,0.56809199,-0.61353636,0.2954064,-0.4317506,-0.34085077,-1.5224791,0.47719216,0.022720795,2.6586561,-0.34085077,0.20451351,1.2952421,-1.4315863)
life <- c(79.2,78.5,78.8,79.3,79.1,78.3,76.6,79.3,78,79,78.2,78.3,77,79.2,78.7,81.6,78.3,78.9,80.1,77.1)
mydata <- cbind(zinequality, zpoverty, zinteraction, zlife, life)
nations <- c("AUS", "AUT", "BEL", "CAN", "CHE", "DEU", "DNK" ,"ESP" ,"FIN", "FRA", "GBR" ,"GRC" ,"IRL", "ISR", "ITA", "JPN" ,"NLD" ,"NOR", "SWE", "USA")
rownames(mydata) <- nations
mydata <- as.data.frame(mydata)

ols1 <- lm(zlife ~ zinequality - 1, data = mydata)
summary(ols1) # Table 5.6, Model 1

X1 <- model.matrix(ols1)
Y <- diag(mydata$zlife)
b_case1 <- solve(crossprod(X1)) %*% t(X1) %*% Y
round(rowSums(b_case1),2)
round(ols1$coefficients,2) # double checking: correct 

#Model 2 
## mmodel 2 ##
ols2 <- lm(zlife ~ zpoverty - 1, data = mydata)
summary(ols2) # Table 5.6, Model 2

summary(lm(zlife ~ zinequality + zpoverty -1)) # Table 5.6, Model 3
summary(lm(zlife ~ zinequality + zpoverty + zinteraction -1)) # Table 5.6, Model 4


X2 <- model.matrix(ols2)
Y <- diag(mydata$zlife)
b_case2 <- solve(crossprod(X2)) %*% t(X2) %*% Y
round(rowSums(b_case2),2)
round(ols2$coefficients,2) # double checking: correct 

colnames(b_case1) <- rownames(mydata)
colnames(b_case2) <- rownames(mydata)

intensities1 <- t(b_case1)
intensities2 <- t(b_case2)

round(colSums(intensities1),2)
round(ols1$coefficients,2) # double checking: correct 

round(colSums(intensities2),2)
round(ols2$coefficients,2) # double checking: correct 


#Rename columns
colnames(intensities1)[colnames(intensities1)=="zinequality"] <- "i_ineq_m1"

colnames(intensities2)[colnames(intensities2)=="zpoverty"] <- "i_pov_m2"

#Decompose variance
#MSE is the sum of squared errors from the model, divided by n-p, given n cases and p variables

#MSE decomp model 1
y1 = zlife
b1 = summary(ols1)$coefficient[1]
yhat1 = X1%*%b1
r1 = y1-yhat1
resid_sqrd1 = r1*r1
mse1 = (t(r1)%*%r1)/(dim(X1)[1] - dim(X1)[2])
mse1 = as.numeric(mse1)
variance1 = mse1*diag(solve(t(X1)%*%X1))
round(variance1, 8)
se1 = sqrt(variance1)
pvalue1 = summary(ols1)$coefficients[,4]

mse_j1 = resid_sqrd1/(dim(X1)[1] - dim(X1)[2])
mse_j1 = round(mse_j1,8)

var1 = matrix(0, 20, 1)
denom1 = dim(var1)[1]-dim(var1)[2]
for (i in 1:dim(var1)[1]) {
  var1[i,] = (resid_sqrd1[i]/denom1)*diag(solve(t(X1)%*%X1))
}
var1=as.data.frame(var1)
names(var1)=c("Inequality")
var_j1 = round(var1,8)

#MSE Decomp Model 2
y2 = zlife
b2 = summary(ols2)$coefficient[1]
yhat2 = X2%*%b2
r2 = y2-yhat2
resid_sqrd2 = r2*r2
mse2 = (t(r2)%*%r2)/(dim(X2)[1] - dim(X2)[2])
mse2 = as.numeric(mse2)
variance2 = mse2*diag(solve(t(X2)%*%X2))
round(variance2, 8)
se2 = sqrt(variance2)
pvalue2 = summary(ols2)$coefficients[,4]

mse_j2 = resid_sqrd2/(dim(X2)[1] - dim(X2)[2])
mse_j2 = round(mse_j2,8)

var2 = matrix(0, 20, 1)
denom2 = dim(var2)[1]-dim(var2)[2]
for (i in 1:dim(var2)[1]) {
  var2[i,] = (resid_sqrd2[i]/denom2)*diag(solve(t(X2)%*%X2))
}
var2=as.data.frame(var2)
names(var2)=c("Poverty")
var_j2 = round(var2,8)


colnames(var_j1)<- "i_ineq_var_m1"
colnames(var_j2)<- "i_pov_var_m2"

#Merge contributions into new dataset
mydataInOut <- cbind(mydata, intensities1, intensities2, var_j1, var_j2, mse_j1, mse_j2)

mydataInOut.omit1<-mydataInOut[-1,]
mydataInOut.omit2<-mydataInOut[-2,]
mydataInOut.omit3<-mydataInOut[-3,]
mydataInOut.omit4<-mydataInOut[-4,]
mydataInOut.omit5<-mydataInOut[-5,]
mydataInOut.omit6<-mydataInOut[-6,]
mydataInOut.omit7<-mydataInOut[-7,]
mydataInOut.omit8<-mydataInOut[-8,]
mydataInOut.omit9<-mydataInOut[-9,]
mydataInOut.omit10<-mydataInOut[-10,]
mydataInOut.omit11<-mydataInOut[-11,]
mydataInOut.omit12<-mydataInOut[-12,]
mydataInOut.omit13<-mydataInOut[-13,]
mydataInOut.omit14<-mydataInOut[-14,]
mydataInOut.omit15<-mydataInOut[-15,]
mydataInOut.omit16<-mydataInOut[-16,]
mydataInOut.omit17<-mydataInOut[-17,]
mydataInOut.omit18<-mydataInOut[-18,]
mydataInOut.omit19<-mydataInOut[-19,]
mydataInOut.omit20<-mydataInOut[-20,]




#########################################        
##  TABLES AND FIGURES: REPLICATION    ##
#########################################

#Table 5.6
ols1 <- lm(zlife ~ zinequality - 1, data = mydata)
ols2 <- lm(zlife ~ zpoverty - 1, data = mydata)
ols3 <- lm(zlife ~ zinequality + zpoverty - 1, data = mydata)
ols4 <- lm(zlife ~ zinequality + zpoverty + zinteraction - 1, data = mydata)

#tab_model(ols1, ols2, ols3, ols4, show.se=TRUE, show.ci=FALSE, transform = NULL, auto.label = FALSE, digits = 3)



###Figure 5.6a
require(tidyverse)
require(rioplot)
mydata <- mutate(mydata,sizes=400 * abs(mydataInOut$i_ineq_m1),
                 sizes2=400 * abs(mydataInOut$i_ineq_var_m1),
                 sizes3=40 * abs(mydataInOut$i_pov_m2),
                 sizes4=400 * abs(mydataInOut$i_pov_var_m2),
                 labelz=row.names(mydata))

p1 <- ggplot() + geom_point(data=mydata,aes(x=zinequality,y=zlife,size=sizes*10),color="black",shape=1) + 
  scale_size(guide='none') + theme_classic() + labs(x="Inequality",y="Life Expectancy",caption="Note: Size of circles is proportional to contributions to the regression coefficients.",title=" A: Regression Coefficient for Inequality") +
  geom_vline(xintercept=0,color="darkgrey") + geom_hline(yintercept=0,color="darkgrey") +
  geom_text(data=mydata,aes(x=zinequality,y=zlife,label=labelz,size=40),nudge_x=.1) + geom_smooth(data=mydata,aes(x=zinequality,y=zlife),method="lm",se=FALSE,color="black") +
  annotate("text",x=1,y=-1,label="b = - .36") +
  theme(axis.title = element_text(size = 16),
        axis.text = element_text(size = 15)) # Fig. 5.6a
require(ggrepel)
p2 <-  ggplot() + geom_point(data=mydata,aes(x=zinequality,y=zlife,size=sizes*10),color="black",shape=1) + 
  scale_size(guide='none') + theme_classic() + labs(x="Inequality",y="Life Expectancy",caption="Note: Size of circles is proportional to contributions to the regression coefficients.",title=" A: Regression Coefficient for Inequality") +
  geom_vline(xintercept=0,color="darkgrey") + geom_hline(yintercept=0,color="darkgrey") +
  geom_text_repel(data=mydata,aes(x=zinequality,y=zlife,label=labelz,size=40)) + geom_smooth(data=mydata,aes(x=zinequality,y=zlife),method="lm",se=FALSE,color="black") +
  annotate("text",x=1,y=-1,label="b = - .36") +
  theme(axis.title = element_text(size = 16),
        axis.text = element_text(size = 15)) # Fig. 5.6a
p2
require(ggpubr)
ggarrange(p1,p2)

f5.6.1<-ggplot() + geom_point(data=mydata,aes(x=zinequality,y=zlife,size=sizes),color="black",shape=1) + 
  scale_size(guide='none') + theme_classic() + labs(x="Inequality",y="Life Expectancy",title=" A: Regression Coefficient for Inequality") +
  geom_vline(xintercept=0,color="darkgrey") + geom_hline(yintercept=0,color="darkgrey") +
  geom_text(data=mydata,aes(x=zinequality,y=zlife,label=labelz,size=.8),nudge_x=.1) + geom_smooth(data=mydata,aes(x=zinequality,y=zlife),method="lm",se=FALSE,color="black") +
  annotate("text",x=1,y=-1,label="b = - .36") +
  theme(axis.title = element_text(size = 16),
        axis.text = element_text(size = 15))
f5.6.1

###Figure 5.6b
ggplot() + geom_point(data=mydata,aes(x=zinequality,y=zlife,size=sizes2),color="black",shape=1) + 
  scale_size(guide='none') + theme_classic() + labs(x="Inequality",y="Life Expectancy",caption="Note: Size of circles is proportional to variance contributions.",title=" B: Variance for Inequality") +
  geom_vline(xintercept=0,color="darkgrey") + geom_hline(yintercept=0,color="darkgrey") +
  geom_text(data=mydata,aes(x=zinequality,y=zlife,label=labelz,size=.8),nudge_x=.1) + geom_smooth(data=mydata,aes(x=zinequality,y=zlife),method="lm",se=FALSE,color="black") +
  annotate("text",x=1,y=-1,label="b = - .36") +
  theme(axis.title = element_text(size = 16),
        axis.text = element_text(size = 15)) # Fig 5.6b




#Table 5.7
mydata.omit.jpn<-mydata[-16,]
mydata.omit.dnk<-mydata[-7,]
mydata.omit.usa<-mydata[-20,]
mydata.omit.jpn.usa.dnk<-mydata[-c(7,16,20),]

ols1.2 <- lm(zlife ~ zinequality - 1, data = mydata.omit.jpn)
summary(ols1.2) # Table 5.7, Model 1
ols1.3 <- lm(zlife ~ zinequality - 1, data = mydata.omit.dnk)
summary(ols1.3) # Table 5.7, Model 2
ols1.4 <- lm(zlife ~ zinequality - 1, data = mydata.omit.usa)
summary(ols1.4)
ols1.5 <- lm(zlife ~ zinequality - 1, data = mydata.omit.jpn.usa.dnk)
summary(ols1.5)
#tab_model(ols1.2, ols1.3, ols1.4, ols1.5, show.se=TRUE, show.ci=FALSE, transform = NULL, auto.label = FALSE, digits = 3)


###Figure 5.7
cooksd.ols1<- cooks.distance(ols1)
cooksd.ols1 <- data.frame(cooksd.ols1=cooksd.ols1,names=names(cooksd.ols1))
p1 <-ggplot(cooksd.ols1,aes(x=names,y=cooksd.ols1)) + theme_classic() +
  geom_bar(stat="identity",width=.01,color="grey60") + geom_point() + theme(axis.text.x=element_blank(),axis.ticks = element_blank()) 
p1 + geom_hline(yintercept=4/18,color="black",linetype="dashed") + geom_hline(yintercept=0,color="black") +
  geom_text(data=cooksd.ols1[which(cooksd.ols1$cooksd.ols1>4/18),],aes(x=names,y=cooksd.ols1,label=names),nudge_y=.03,nudge_x=-.3) +
  labs(x="",y="Cook's D") + 
  theme(axis.title = element_text(size = 16),
        axis.text = element_text(size = 15))
#Fig. 5.7



###Figure 5.8a
ggplot() + geom_point(data=mydata,aes(x=zpoverty,y=zlife,size=sizes3),color="black",shape=1) + 
  scale_size(guide='none') +  theme_classic() + labs(x="Poverty",y="Life Expectancy",caption="Note: Size of circles is proportional to contributions to the regression coefficients.",title=" A: Regression Coefficient for Poverty") +
  geom_vline(xintercept=0,color="darkgrey") + geom_hline(yintercept=0,color="darkgrey") +
  geom_text_repel(data=mydata,aes(x=zpoverty,y=zlife,label=labelz,size=.8)) + geom_smooth(data=mydata,aes(x=zpoverty,y=zlife),method="lm",se=FALSE,color="black") +
  annotate("text",x=1.8,y=1,label="b = + .07") +
  theme(axis.title = element_text(size = 16),
        axis.text = element_text(size = 15))



###Figure 5.8b
ggplot() + geom_point(data=mydata,aes(x=zpoverty,y=zlife,size=sizes4),color="black",shape=1) + 
  scale_size(guide='none') + theme_classic() + labs(x="Poverty",y="Life Expectancy",caption="Note: Size of circles is proportional to contributions to variance contributions.",title=" B: Variance for Poverty") +
  geom_vline(xintercept=0,color="darkgrey") + geom_hline(yintercept=0,color="darkgrey") +
  geom_text_repel(data=mydata,aes(x=zpoverty,y=zlife,label=labelz,size=.8)) + geom_smooth(data=mydata,aes(x=zpoverty,y=zlife),method="lm",se=FALSE,color="black") +
  annotate("text",x=1.8,y=1,label="b = + .07") +
  theme(axis.title = element_text(size = 16),
        axis.text = element_text(size = 15))



##############        
##  GSS Example   ##
##############
rm(list=ls())
data("GSS2018")
dog<-XY[,1]
race<-XY[,2]
sex<-XY[,3]
children<-XY[,4]
married<-XY[,5]
age<-XY[,6]
income<-XY[,7]
#####################   GSS 2018 - Who has a Dog?  #####################

###Table 5.8
dog1<-glm(dog~race+sex+children+married+age+income, family="binomial")
summary(dog1) # Table 5.8
X <- cbind(1,race,sex,children,married,age,income)
###Figure 5.9
#SVD of X
svd.dog<-svd(X)
S.dog<-diag(svd.dog$d)
U.dog<-svd.dog$u
V.dog<-svd.dog$v

#Calculate WSS for k-means solutions with 2-10 clusters
kmeans.ss<-rep(0,10)
for (i in 2:10) {
  kmeans.ss[i]<-(kmeans(U.dog,i,iter.max=10000,nstart=10000)$tot.withinss)
}

#generate plot
plot(kmeans.ss[2:10], type="l")#Most evident inflection point is at 5 clusters

pdat <- data.frame(ss=kmeans.ss[2:10],clusters=paste(1:9))
ggplot(pdat,aes(x=clusters,y=ss)) + theme_classic() + geom_line(group=1) +
  geom_point() + labs(x="Number of Clusters",y="Sums of Squares") +
  theme(axis.title = element_text(size = 16),
        axis.text = element_text(size = 15)) # Fig 5.9


###Table 5.9
#Generate 5-cluster solution
kmeans5<-kmeans(U.dog,5,iter.max=10000,nstart=10000)
kmeans5<-data.frame(kmeans5$cluster)
colnames(kmeans5) = "clusters"
#Append clusters to data
Xk<-cbind(X,kmeans5)
Xk1<-Xk[which(Xk$clusters==1),]
Xk2<-Xk[which(Xk$clusters==2),]
Xk3<-Xk[which(Xk$clusters==3),]
Xk4<-Xk[which(Xk$clusters==4),]
Xk5<-Xk[which(Xk$clusters==5),]
#Generate descriptive statistics for each cluster
summary(Xk1)
summary(Xk2)
summary(Xk3)
summary(Xk4)
summary(Xk5)


###Table 5.10
#Turn the regression model inside out
X<-cbind(race,sex,children,married,age,income)
cons<-rep(1,nrow(X))
X<-cbind(cons,X)
b.cases.dog<-solve(t(X)%*%X)%*%t(X)%*%diag(dog1$linear.predictors)  
#rowSums(b.cases.dog)#check
#Aggregate contributions by cluster
b.cluster<-t(aggregate(t(b.cases.dog),by=list(Xk$clusters),FUN=sum))[-1,]
b.cluster

