require(tidyverse)
require(ggrepel)
# devtools::install_github("dmmelamed/rioplot")
require(rioplot)

rm(list=ls())
# Read in the data; see Table 2.1
nation.long <- c("Australia", "Belgium", "Canada", "Denmark", "Finland", "France", "Germany", "Ireland", "Italy", "Netherlands", "Norway", "Sweden", "Switzerland", "United Kingdom", "U.S.A.")
ISO3 <- c("AUS", "BEL", "CAN", "DNK", "FIN", "FRA", "DEU", "IRL", "ITA", "NLD", "NOR", "SWE", "CHE", "GBR", "USA")
dv <- c(11.9,6,6.5,5.9,3.7,9.8,4.3,29.4,14.3,7.3,1.7,5.8,3.8,16.8,11.7)
gdp <- c(7734.418,6258.508,7894.777,7450.465,5712.95,6938.063,6746.054,3905.857,5507.27,7390.348,6507.316,7965.854,11419.35,7982.377,11871.49)
gdp <- round(gdp, digits = 0) # For compatibility to Kenworthy.
pov <- c(23.3,26.8,22.5,26.4,11.9,36.1,15.2,39.2,30.7,22.1,9.2,23.7,12.5,29.6,21)
tran <- c(7.3,19.3,9.5,13.5,10.4,17.8,14.8,10.5,14.5,21.5,13.4,14.6,9.4,10.1,8.8)
X <- data.frame(dv,tran,pov)
Z <- data.frame(scale(X))
colnames(Z) <- paste(colnames(X),"Z",sep="")

summary(m1 <- lm(scale(dv) ~ scale(povZ) -1,data=Z)) # 2.2.1 Simple Regression
X5 <- as.matrix(scale(pov))
t2.2 <- solve(t(X5) %*% X5) %*% t(X5) %*% diag(as.vector(scale(dv)))
t2.2 <- t(t2.2); rownames(t2.2) <- ISO3
t2.2 # Table 2.2
colSums(t2.2)


m1 <- lm(dvZ~tranZ + povZ-1,data=Z)
summary(m1) # Table 2.3
m1$fitted.values;mean(m1$fitted.values);sd(m1$fitted.values) # Table 2.4

X5 <- cbind(Z$tranZ,Z$povZ)
t(solve(t(X5) %*% X5) %*% t(X5) %*% diag(m1$fitted.values)) # Table 2.5
colSums(t(solve(t(X5) %*% X5) %*% t(X5) %*% diag(m1$fitted.values))) 


s1 <- svd(X5)
round(s1$u,3) # Table 2.6a
diag(round(s1$d,3)) # Table 2.6b
round(s1$v,3) # Table 2.6c

t2.7 <- t(s1$v %*% solve(diag(s1$d)) %*% t(s1$u))
data.frame(ISO3,round(t2.7,3)) # Table 2.7

m2 <- lm(tranZ ~ povZ-1,data=Z)
tran.resid <- m2$residual 
tran.resid # column 1 of table 2.8
tran.resid/13.558 # column 2 of table 2.8
yhat <- m1$fitted.values
pdat <- data.frame(tran.resid,yhat,ISO3)

m1 <- lm(dvZ~tranZ + povZ-1,data=Z)
mat <- cbind(scale(tran),scale(pov),m1$fitted.values)
round(svd(mat)$u,3) # Table 2.9a
round(svd(mat)$u %*% diag(svd(mat)$d),3) # Table 2.9b
round(diag(svd(mat)$d),3) # Table 2.9c
round(svd(mat)$v,3) # Table 2.9d


# Fig 2.2
tran.resid <- m2$residual 
yhat <- m1$fitted.values
pdat <- data.frame(tran.resid,yhat,ISO3)
ggplot(pdat,aes(x=tran.resid,y=yhat,label=ISO3)) + theme_classic() + geom_point() +
  geom_hline(yintercept=0,color="grey60") + geom_vline(xintercept=0,color="grey60") +
  labs(x="Transfers (residualized from pre-transfer poverty)",y="Predicted Poverty") +
  geom_smooth(method="lm",se=FALSE,color="black",linewidth=.5) + 
  annotate("rect",xmin=pdat[8,1],xmax=0,ymin=0,ymax=pdat[8,2],alpha=.2) + geom_text_repel(size=6) + 
  theme(axis.title = element_text(size = 15),
        axis.text = element_text(size = 15))


# Specialized version of rio.plot to leave out the lines connecting the variables to the projection through the origin. Don't use this function otherwise (stopped updating as of 6.8.22)
data("rioplots")
rp1 <- rio.plot2(m1,include.int="no",r1=1:15,case.col = "grey60",case.names = ISO3,var.name.col = "grey25")

rp1$gg.obj + scale_x_continuous(limits=c(-2.2,2.8)) # Plot 2.3
p3 <- rp1$gg.obj + scale_x_continuous(limits=c(-2.2,2.8)) +theme(axis.title = element_text(size = 15),
                                                                 axis.text = element_text(size = 15))
p3 # Fig 2.3


trp<-project.point(rp1$col.dimensions[1,1:2],rp1$col.dimensions[3,1:2])$newpoint
pop<-project.point(rp1$col.dimensions[2,1:2],rp1$col.dimensions[3,1:2])$newpoint

# Another specialized version of rio.plot 
rp1 <-rio.plot3(m1,include.int="no",r1=1:15,case.col = "grey60",case.names = ISO3,var.name.col = "black",size=5)
rp1$gg.obj

rp1$gg.obj + scale_x_continuous(limits=c(-2.2,2.8)) +
  geom_segment(aes(x = 0, y = 0, xend = trp[1], yend = trp[2]),colour="grey60",size=2) +
  geom_segment(aes(x = 0, y = 0, xend = pop[1], yend = pop[2]),colour="black",size=2)
p4 <- rp1$gg.obj + scale_x_continuous(limits=c(-2.2,2.8)) +
  geom_segment(aes(x = 0, y = 0, xend = trp[1], yend = trp[2]),colour="grey60",size=2) +
  geom_segment(aes(x = 0, y = 0, xend = pop[1], yend = pop[2]),colour="black",size=2) +theme(axis.title = element_text(size = 15),
                                                                                             axis.text = element_text(size = 15))
p4 #Fig 2.4


w1<-project.point(rp1$row.dimensions[1,1:2],rp1$col.dimensions[3,1:2])$newpoint
w2<-project.point(rp1$row.dimensions[2,1:2],rp1$col.dimensions[3,1:2])$newpoint
w3<-project.point(rp1$row.dimensions[3,1:2],rp1$col.dimensions[3,1:2])$newpoint
w4<-project.point(rp1$row.dimensions[4,1:2],rp1$col.dimensions[3,1:2])$newpoint
w5<-project.point(rp1$row.dimensions[5,1:2],rp1$col.dimensions[3,1:2])$newpoint
w6<-project.point(rp1$row.dimensions[6,1:2],rp1$col.dimensions[3,1:2])$newpoint
w7<-project.point(rp1$row.dimensions[7,1:2],rp1$col.dimensions[3,1:2])$newpoint
w8<-project.point(rp1$row.dimensions[8,1:2],rp1$col.dimensions[3,1:2])$newpoint
w9<-project.point(rp1$row.dimensions[9,1:2],rp1$col.dimensions[3,1:2])$newpoint
w10<-project.point(rp1$row.dimensions[10,1:2],rp1$col.dimensions[3,1:2])$newpoint
w11<-project.point(rp1$row.dimensions[11,1:2],rp1$col.dimensions[3,1:2])$newpoint
w12<-project.point(rp1$row.dimensions[12,1:2],rp1$col.dimensions[3,1:2])$newpoint
w13<-project.point(rp1$row.dimensions[13,1:2],rp1$col.dimensions[3,1:2])$newpoint
w14<-project.point(rp1$row.dimensions[14,1:2],rp1$col.dimensions[3,1:2])$newpoint
w15<-project.point(rp1$row.dimensions[15,1:2],rp1$col.dimensions[3,1:2])$newpoint

rp1$gg.obj + scale_x_continuous(limits=c(-2.2,2.8)) +
  geom_segment(aes(x = 0, y = 0, xend = trp[1], yend = trp[2]),colour="grey60",size=2) +
  geom_segment(aes(x = 0, y = 0, xend = pop[1], yend = pop[2]),colour="black",size=2) +
  geom_segment(aes(x = rp1$row.dimensions[1,1], y = rp1$row.dimensions[1,2], xend = w1[1], yend = w1[2]),colour="grey30",size=.2) +
  geom_segment(aes(x = rp1$row.dimensions[2,1], y = rp1$row.dimensions[2,2], xend = w2[1], yend = w2[2]),colour="grey30",size=.2) +
  geom_segment(aes(x = rp1$row.dimensions[3,1], y = rp1$row.dimensions[3,2], xend = w3[1], yend = w3[2]),colour="grey30",size=.2) +
  geom_segment(aes(x = rp1$row.dimensions[4,1], y = rp1$row.dimensions[4,2], xend = w4[1], yend = w4[2]),colour="grey30",size=.2) +
  geom_segment(aes(x = rp1$row.dimensions[5,1], y = rp1$row.dimensions[5,2], xend = w5[1], yend = w5[2]),colour="grey30",size=.2) +
  geom_segment(aes(x = rp1$row.dimensions[6,1], y = rp1$row.dimensions[6,2], xend = w6[1], yend = w6[2]),colour="grey30",size=.2) +
  geom_segment(aes(x = rp1$row.dimensions[7,1], y = rp1$row.dimensions[7,2], xend = w7[1], yend = w7[2]),colour="grey30",size=.2) +
  geom_segment(aes(x = rp1$row.dimensions[8,1], y = rp1$row.dimensions[8,2], xend = w8[1], yend = w8[2]),colour="grey30",size=.2) +
  geom_segment(aes(x = rp1$row.dimensions[9,1], y = rp1$row.dimensions[9,2], xend = w9[1], yend = w9[2]),colour="grey30",size=.2) +
  geom_segment(aes(x = rp1$row.dimensions[10,1], y = rp1$row.dimensions[10,2], xend = w10[1], yend = w10[2]),colour="grey30",size=.2) +
  geom_segment(aes(x = rp1$row.dimensions[11,1], y = rp1$row.dimensions[11,2], xend = w11[1], yend = w11[2]),colour="grey30",size=.2) +
  geom_segment(aes(x = rp1$row.dimensions[12,1], y = rp1$row.dimensions[12,2], xend = w12[1], yend = w12[2]),colour="grey30",size=.2) +
  geom_segment(aes(x = rp1$row.dimensions[13,1], y = rp1$row.dimensions[13,2], xend = w13[1], yend = w13[2]),colour="grey30",size=.2) +
  geom_segment(aes(x = rp1$row.dimensions[14,1], y = rp1$row.dimensions[14,2], xend = w14[1], yend = w14[2]),colour="grey30",size=.2) +
  geom_segment(aes(x = rp1$row.dimensions[15,1], y = rp1$row.dimensions[15,2], xend = w15[1], yend = w15[2]),colour="grey30",size=.2)

p5 <- rp1$gg.obj + scale_x_continuous(limits=c(-2.2,2.8)) +
  geom_segment(aes(x = 0, y = 0, xend = trp[1], yend = trp[2]),colour="grey60",size=2) +
  geom_segment(aes(x = 0, y = 0, xend = pop[1], yend = pop[2]),colour="black",size=2) +
  geom_segment(aes(x = rp1$row.dimensions[1,1], y = rp1$row.dimensions[1,2], xend = w1[1], yend = w1[2]),colour="grey30",size=.2) +
  geom_segment(aes(x = rp1$row.dimensions[2,1], y = rp1$row.dimensions[2,2], xend = w2[1], yend = w2[2]),colour="grey30",size=.2) +
  geom_segment(aes(x = rp1$row.dimensions[3,1], y = rp1$row.dimensions[3,2], xend = w3[1], yend = w3[2]),colour="grey30",size=.2) +
  geom_segment(aes(x = rp1$row.dimensions[4,1], y = rp1$row.dimensions[4,2], xend = w4[1], yend = w4[2]),colour="grey30",size=.2) +
  geom_segment(aes(x = rp1$row.dimensions[5,1], y = rp1$row.dimensions[5,2], xend = w5[1], yend = w5[2]),colour="grey30",size=.2) +
  geom_segment(aes(x = rp1$row.dimensions[6,1], y = rp1$row.dimensions[6,2], xend = w6[1], yend = w6[2]),colour="grey30",size=.2) +
  geom_segment(aes(x = rp1$row.dimensions[7,1], y = rp1$row.dimensions[7,2], xend = w7[1], yend = w7[2]),colour="grey30",size=.2) +
  geom_segment(aes(x = rp1$row.dimensions[8,1], y = rp1$row.dimensions[8,2], xend = w8[1], yend = w8[2]),colour="grey30",size=.2) +
  geom_segment(aes(x = rp1$row.dimensions[9,1], y = rp1$row.dimensions[9,2], xend = w9[1], yend = w9[2]),colour="grey30",size=.2) +
  geom_segment(aes(x = rp1$row.dimensions[10,1], y = rp1$row.dimensions[10,2], xend = w10[1], yend = w10[2]),colour="grey30",size=.2) +
  geom_segment(aes(x = rp1$row.dimensions[11,1], y = rp1$row.dimensions[11,2], xend = w11[1], yend = w11[2]),colour="grey30",size=.2) +
  geom_segment(aes(x = rp1$row.dimensions[12,1], y = rp1$row.dimensions[12,2], xend = w12[1], yend = w12[2]),colour="grey30",size=.2) +
  geom_segment(aes(x = rp1$row.dimensions[13,1], y = rp1$row.dimensions[13,2], xend = w13[1], yend = w13[2]),colour="grey30",size=.2) +
  geom_segment(aes(x = rp1$row.dimensions[14,1], y = rp1$row.dimensions[14,2], xend = w14[1], yend = w14[2]),colour="grey30",size=.2) +
  geom_segment(aes(x = rp1$row.dimensions[15,1], y = rp1$row.dimensions[15,2], xend = w15[1], yend = w15[2]),colour="grey30",size=.2) + theme(axis.title = element_text(size = 15),
                                                                                                                                              axis.text = element_text(size = 15))
p5 # Fig 2.5





rp5 <- rio.plot(m1,include.int="no",r1=1:15,case.col = "grey60",case.names = ISO3,var.name.col = "grey25",exclude.vars=1:2)
rp5$gg.obj + annotate("rect",xmin=-2.3,xmax=-.83,ymin=-.5,ymax=1.25,alpha=.15,fill="black") +
  annotate("rect",xmin=-.8,xmax=2.1,ymin=-2.1,ymax=.12,alpha=.15,fill="black")  +
  annotate("rect",xmin=-.5,xmax=3,ymin=.4,ymax=1.6,alpha=.15,fill="black")
p6 <- rp5$gg.obj + annotate("rect",xmin=-2.3,xmax=-.83,ymin=-.5,ymax=1.25,alpha=.15,fill="black") +
  annotate("rect",xmin=-.8,xmax=2.1,ymin=-2.1,ymax=.12,alpha=.15,fill="black")  +
  annotate("rect",xmin=-.5,xmax=3,ymin=.4,ymax=1.6,alpha=.15,fill="black") + theme(axis.title = element_text(size = 15),
                                                                                   axis.text = element_text(size = 15))
p6 # Figure 2.6




# cosine similarities 
U <- svd(Z[,2:3])$u
UUt <- U %*% t(U)
d <- diag(UUt)
d <- sqrt(d)
cosUUt <- UUt / outer(d,d)
rownames(cosUUt) <- nation.long
colnames(cosUUt) <- nation.long
require(reshape2)
UUtD <- melt(cosUUt)
# Run Kmeans (3-cluster) on U:
# K3 <- kmeans(U, center = 3, iter.max=10000, nstart = 10000)
# This is from the result: K3$cluster
# Permutation from doing k-means clustering on matrix U:
perm3 <- c(2,4,6,9,10,12,5,7,11,13,1,3,8,14,15)
UUtD$Var1 <- factor(UUtD$Var1,levels=nation.long[perm3])
UUtD$Var2 <- factor(UUtD$Var2,levels=rev(nation.long[perm3]))

ggplot(UUtD,aes(x=Var1,y=Var2,fill=value)) + theme_classic() +
  geom_tile() + scale_fill_gradient(low="grey90",high="black",breaks=c(-.99,-.5,0,.5,.99),labels=c("-1","-.5","0",".5","1")) + labs(x="",y="",fill="Cosine\nSimilarity") + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) + 
  #theme(legend.position = "bottom") + 
  geom_segment(aes(x = 0, y = 5.5, xend = 15.5, yend = 5.5),colour="white",size=1) +
  geom_segment(aes(x = 0, y = 9.5, xend = 15.5, yend = 9.5),colour="white",size=1) +
  geom_segment(aes(x = 6.5, y = 0, xend = 6.5, yend = 15.5),colour="white",size=1) +
  geom_segment(aes(x = 10.5, y = 0, xend = 10.5, yend = 15.5),colour="white",size=1) +
  geom_segment(aes(x = 0, y = 5.5, xend = 15.5, yend = 5.5),colour="black",size=.2,linetype="dashed") +
  geom_segment(aes(x = 0, y = 9.5, xend = 15.5, yend = 9.5),colour="black",size=.2,linetype="dashed") +
  geom_segment(aes(x = 6.5, y = 0, xend = 6.5, yend = 15.5),colour="black",size=.2,linetype="dashed") +
  geom_segment(aes(x = 10.5, y = 0, xend = 10.5, yend = 15.5),colour="black",size=.2,linetype="dashed")

p7 <- ggplot(UUtD,aes(x=Var1,y=Var2,fill=value)) + theme_classic() +
  geom_tile() + scale_fill_gradient(low="grey90",high="black",breaks=c(-.99,-.5,0,.5,.99),labels=c("-1","-.5","0",".5","1")) + labs(x="",y="",fill="Cosine\nSimilarity") + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) + 
  #theme(legend.position = "bottom") + 
  geom_segment(aes(x = 0, y = 5.5, xend = 15.5, yend = 5.5),colour="white",size=1) +
  geom_segment(aes(x = 0, y = 9.5, xend = 15.5, yend = 9.5),colour="white",size=1) +
  geom_segment(aes(x = 6.5, y = 0, xend = 6.5, yend = 15.5),colour="white",size=1) +
  geom_segment(aes(x = 10.5, y = 0, xend = 10.5, yend = 15.5),colour="white",size=1) +
  geom_segment(aes(x = 0, y = 5.5, xend = 15.5, yend = 5.5),colour="black",size=.2,linetype="dashed") +
  geom_segment(aes(x = 0, y = 9.5, xend = 15.5, yend = 9.5),colour="black",size=.2,linetype="dashed") +
  geom_segment(aes(x = 6.5, y = 0, xend = 6.5, yend = 15.5),colour="black",size=.2,linetype="dashed") +
  geom_segment(aes(x = 10.5, y = 0, xend = 10.5, yend = 15.5),colour="black",size=.2,linetype="dashed") + theme(axis.title = element_text(size = 15),
                                                                                                                axis.text = element_text(size = 15))

p7 # Fig 2.7


