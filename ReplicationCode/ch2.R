
rm(list=ls())
# install rio.plot
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
 summary(lm(dvZ~tranZ + povZ-1,data=Z))
m1 <- lm(tranZ ~ povZ -1,data=Z)
tran.resid <- m1$residuals
m2 <- lm(dvZ~ tranZ + povZ -1,data=Z)
yhat = m2$fitted.values
require(tidyverse)
require(ggrepel)
pdat <- data.frame(tran.resid,yhat,ISO3)
# Fig 2.1
ggplot(pdat,aes(x=tran.resid,y=yhat,label=ISO3)) + theme_classic() + geom_point() +
  geom_hline(yintercept=0,color="grey60") + geom_vline(xintercept=0,color="grey60") +
  labs(x="Transfers (residualized from pre-transfer poverty)",y="Predicted Poverty") +
  geom_smooth(method="lm",se=FALSE,color="black",size=.5) + 
  annotate("rect",xmin=pdat[8,1],xmax=0,ymin=0,ymax=pdat[8,2],alpha=.2) + geom_text_repel() + 
  theme(axis.title = element_text(size = 15),
        axis.text = element_text(size = 15))
  

p1 <- ggplot(pdat,aes(x=tran.resid,y=yhat,label=ISO3)) + theme_classic() + geom_point() +
  geom_hline(yintercept=0,color="grey60") + geom_vline(xintercept=0,color="grey60") +
  labs(x="Transfers (residualized from pre-transfer poverty)",y="Predicted Poverty") +
  geom_smooth(method="lm",se=FALSE,color="black",size=.5) + 
  annotate("rect",xmin=pdat[8,1],xmax=0,ymin=0,ymax=pdat[8,2],alpha=.2) + geom_text_repel() + 
  theme(axis.title = element_text(size = 16),
        axis.text = element_text(size = 15))
setwd("/Users/melamed.9/Desktop/School/Research/InPrep/Regression Inside Out/RIO Book/replication.code")
ggsave(p1, file="Figure2.1.revised.eps", device=cairo_ps)
# Fig 2.2

# Specialized version of rio.plot to leave out the lines connecting the variables to the projection through the origin. Don't use this function otherwise (stopped updating as of 6.8.22)
rio.plot2<-function(m1,exclude.vars="no",r1="none",case.names="",h.just=-.2,v.just=0,case.col="blue",var.name.col="black",include.int="yes",group.cases=1,model.type="OLS"){
  require(tidyverse)
  
  X<-m1$model[,2:ncol(m1$model)]
  y<-m1$model[,1]
  u.out<-svd(X)$u
  uut.out <- u.out %*% t(u.out)
  if(include.int[1]=="yes"){
    X<-data.frame(Intercept=1,X)
    if(model.type=="OLS"){
      Xadj <- cbind(X,m1$fitted.values)}else{
        Xadj <- cbind(X,m1$linear.predictors)}
    svd2 <- svd(Xadj)
    U2 <- svd2$u %*% diag(svd2$d)
    V2 <- svd2$v
  }else{
    Xadj <- cbind(X,m1$fitted.values)
    svd2 <- svd(Xadj)
    U2 <- svd2$u %*% diag(svd2$d)
    V2 <- svd2$v}
  
  variables<-c(colnames(m1$model)[2:ncol(m1$model)],colnames(m1$model)[1])
  variables[length(variables)]<-"YHAT"
  if(include.int[1]=="yes"){variables <- c("Intercept",variables)}
  if(r1[1]!="none"){
    U2<- U2[r1,]
    U2<-data.frame(U2)
    colnames(U2)<-c("no1","no2")}
  pdat <- data.frame(V2[,1:2],variables)
  if(exclude.vars[1]!="no"){
    V2 <- V2[-exclude.vars,]
    variables <- variables[-exclude.vars]
    pdat <- data.frame(V2,variables)}
  
  if(class(V2)[1]=="numeric"){
    pdat<-data.frame(X1=pdat[1,1],X2=pdat[2,1],variables=pdat[1,2])
    V2<-t(as.matrix(V2))}
  
  project.point <-function(p1,p2){
    # This function is an adaptation of one that Ron Breiger wrote in 2014.
    # It identifies the point at which p1 intersects, at a 90 degree angle, the line between the origin and p2
    # I adapted Ron's program to get values for ggplot, rather than to use in the base R plot function
    lamda <- sum(p1*p2)
    b <- sqrt(sum(p2^2))
    slope <- p2[2] / p2[1]
    Xsq <- (lamda^2) / ((b^2) * (1 + (slope^2)))
    X1 <- sqrt(Xsq)
    Y1 <- slope*X1
    newpoint1 <- c(X1,Y1)
    dist1 <- sqrt(sum((p1 - newpoint1)^2))
    # Now try reversing sign of X, and see if distance from p1 is shorter.
    X2 <- 0 - X1
    Y2 <- slope * X2
    newpoint2 <- c(X2,Y2)
    dist2 <- sqrt(sum((p1 - newpoint2)^2))
    if (dist1 <= dist2) {newpoint <- newpoint1}else {newpoint <- newpoint2}
    out <- list(newpoint=newpoint)
    return(out)}
  l1 <- nrow(V2) -1
  if(class(V2)[1]=="matrix"){
    lineseg1<-rbind(project.point(V2[1,1:2],V2[nrow(V2),1:2])$newpoint,V2[1,1:2])
    lineseg1<-data.frame(lineseg1,1)
    colnames(lineseg1)<-c("x","y","gr")
    if(l1 > 1){
      for(i in 2:l1){
        lineseg2<-rbind(project.point(V2[i,1:2],V2[nrow(V2),1:2])$newpoint,V2[i,1:2])
        lineseg2<-data.frame(lineseg2,i)
        colnames(lineseg2)<-c("x","y","gr")
        lineseg1<-rbind(lineseg1,lineseg2)}}}
  if(r1[1]=="none"){if(class(V2)[1]=="matrix"){
    p1 <- ggplot()  + theme_classic() + labs(x="First Dimension",y="Second Dimension") +
      geom_vline(xintercept=0) + geom_hline(yintercept=0) + geom_abline(intercept=0,slope=(V2[nrow(V2),2]/V2[nrow(V2),1]))+ geom_text(data=pdat,aes(x=X1,y=X2,label=variables, hjust=h.just,vjust=v.just),color=var.name.col) + guides(color = "none") +
      #geom_line(data=lineseg1,mapping=aes(x=x,y=y,group=gr),color="grey65") + 
      coord_fixed(ratio=1)  + geom_point(data=pdat,aes(x=X1,y=X2))}else{
        p1 <- ggplot()  + theme_classic() + labs(x="First Dimension",y="Second Dimension") +
          geom_vline(xintercept=0) + geom_hline(yintercept=0) + geom_abline(intercept=0,slope=(V2[2]/V2[1]))+ geom_text(data=pdat,aes(x=X1,y=X2,label=variables, hjust=h.just,vjust=v.just),color=var.name.col) + guides(color = "none") +
          coord_fixed(ratio=1)  + geom_point(data=pdat,aes(x=X1,y=X2))}
    
  }else {if(length(group.cases)==1){if(class(V2)[1]=="matrix"){
    p1 <- ggplot()   + theme_classic() + labs(x="First Dimension",y="Second Dimension") +
      geom_vline(xintercept=0) + geom_hline(yintercept=0) + geom_abline(intercept=0,slope=(V2[nrow(V2),2]/V2[nrow(V2),1]))+ geom_text(data=pdat,aes(x=X1,y=X2,label=variables, hjust=h.just,vjust=v.just),color=var.name.col) + guides(color = "none") +
      #geom_line(data=lineseg1,mapping=aes(x=x,y=y,group=gr),color="grey65") +
      geom_point(data=U2,aes(x=no1,y=no2),color=case.col) + geom_text(data=U2,aes(x=no1,y=no2,label=case.names), hjust=h.just,vjust=v.just) + coord_fixed(ratio=1) + geom_point(data=pdat,aes(x=X1,y=X2))}else{
        p1 <- ggplot()   + theme_classic() + labs(x="First Dimension",y="Second Dimension") +
          geom_vline(xintercept=0) + geom_hline(yintercept=0) + geom_abline(intercept=0,slope=(V2[2]/V2[1]))+ geom_text(data=pdat,aes(x=X1,y=X2,label=variables, hjust=h.just,vjust=v.just),color=var.name.col) + guides(color = "none") +
          
          geom_point(data=U2,aes(x=no1,y=no2),color=case.col) + geom_text(data=U2,aes(x=no1,y=no2,label=case.names), hjust=h.just,vjust=v.just) + coord_fixed(ratio=1) + geom_point(data=pdat,aes(x=X1,y=X2))}
    
  }else {U3 <- data.frame(U2)
  U3 <- mutate(U3,agg=group.cases)
  colnames(U3)[1:2] <- c("no1","no2")
  U5 <- U3 %>% group_by(agg) %>% summarize(x=mean(no1),y=mean(no2))
  if(class(V2)[1]=="matrix"){
    p1 <- ggplot()  + theme_classic() + labs(x="First Dimension",y="Second Dimension") +
      geom_vline(xintercept=0) + geom_hline(yintercept=0) + geom_abline(intercept=0,slope=(V2[nrow(V2),2]/V2[nrow(V2),1]))+ geom_text(data=pdat,aes(x=X1,y=X2,label=variables, hjust=h.just,vjust=v.just),color=var.name.col) + guides(color = "none") +
      geom_line(data=lineseg1,mapping=aes(x=x,y=y,group=gr),color="grey65") +
      geom_point(data=U5,aes(x=x,y=y),color=case.col) + geom_text(data=U5,aes(x=x,y=y,label=agg), hjust=h.just,vjust=v.just) + coord_fixed(ratio=1) + geom_point(data=pdat,aes(x=X1,y=X2))}else{
        p1 <- ggplot()  + theme_classic() + labs(x="First Dimension",y="Second Dimension") +
          geom_vline(xintercept=0) + geom_hline(yintercept=0) + geom_abline(intercept=0,slope=(V2[2]/V2[1]))+ geom_text(data=pdat,aes(x=X1,y=X2,label=variables, hjust=h.just,vjust=v.just),color=var.name.col) + guides(color = "none") +
          
          geom_point(data=U5,aes(x=x,y=y),color=case.col) + geom_text(data=U5,aes(x=x,y=y,label=agg), hjust=h.just,vjust=v.just) + coord_fixed(ratio=1) + geom_point(data=pdat,aes(x=X1,y=X2))}
  
  }}
  
  if(model.type=="OLS"){
    re2<-resid(m1)^2
    mse <- re2/(nrow(m1$model)-m1$rank)
    
    if(include.int[1]=="yes"){
      X <- as.matrix(cbind(1,m1$model[,2:ncol(m1$model)]))}else{
        X <- as.matrix(m1$model[,2:ncol(m1$model)])}
    d.xinv<-diag(solve(t(X) %*% X))       
    case.variances <- matrix(NA,nrow=nrow(m1$model),ncol=length(m1$coefficients))
    case.variances[,1] <-mse *d.xinv[1]
    if(length(d.xinv)>1){
      for(i in 2:length(d.xinv)) {
        case.variances[,i] <-mse *d.xinv[i]}}}else{
          r <- m1$residuals
          mse<-as.numeric((t(r)%*%r)/(nrow(m1$model)-m1$rank))
          # Get each case's contribution to the MSE
          look <- rep(0, length(r))
          for (i in 1:length(r)) {
            r1<-m1$residuals[i]
            mse1<-(t(r1)%*%r1)/(nrow(m1$model)-m1$rank)
            mse1<-as.numeric(mse1)
            look[i] <- mse1}
          
          X<-as.matrix(cbind(1,m1$model[,2:ncol(m1$model)]))
          if(model.type=="logit"){
            W<-diag((m1$fitted.values*(1-m1$fitted.values)))}else if(model.type=="poisson"){
              W<-diag(m1$fitted.values)}else if(model.type=="nb"){
                W<-diag(m1$fitted.values/(1+(1/m1$theta)*m1$fitted.values))}else{
                  print("model.type should be OLS, logit, poisson, or nb.")} 
          
          cov<-solve(t(X) %*% W %*% X)
          # Weight the variance/covariance matrix by the case's contributions to the MSE
          look2 <- array(0, c(nrow(cov),ncol(cov),length(r)))
          for (k in 1:length(r)) {
            look2[,,k] <- (look[k]/mse) * cov
          }
          case.variances <- sqrt(diag(look2[,,1]))
          for(i in 2:nrow(X)){
            case.variances2 <- sqrt(diag(look2[,,i]))
            case.variances <- rbind(case.variances,case.variances2)}}
  
  colnames(case.variances) <- names(m1$coefficients)
  rownames(case.variances) <- paste(1:nrow(X))
  
  out<-list(gg.obj=p1,row.dimensions=U2[,1:2],col.dimensions=V2[,1:2],case.variances=case.variances,U=u.out,UUt=uut.out)
  return(out)}
rp1 <- rio.plot2(m2,include.int="no",r1=1:15,case.col = "grey60",case.names = ISO3,var.name.col = "grey25")

rp1$gg.obj + scale_x_continuous(limits=c(-2.2,2.8))
p2 <- rp1$gg.obj + scale_x_continuous(limits=c(-2.2,2.8)) +theme(axis.title = element_text(size = 15),
                                                                 axis.text = element_text(size = 15))
p2
setwd("/Users/melamed.9/Desktop/School/Research/InPrep/Regression Inside Out/RIO Book/replication.code")
ggsave(p2, file="Figure2.2.revised.eps", device=cairo_ps)


# Fig 2.3
project.point <-function(p1,p2){
  # This function is an adaptation of one that Ron Breiger wrote in 2014.
  # It identifies the point at which p1 intersects, at a 90 degree angle, the line between the origin and p2
  # I adapted Ron's program to get values for ggplot, rather than to use in the base R plot function
  lamda <- sum(p1*p2)
  b <- sqrt(sum(p2^2))
  slope <- p2[2] / p2[1]
  Xsq <- (lamda^2) / ((b^2) * (1 + (slope^2)))
  X1 <- sqrt(Xsq)
  Y1 <- slope*X1
  newpoint1 <- c(X1,Y1)
  dist1 <- sqrt(sum((p1 - newpoint1)^2))
  # Now try reversing sign of X, and see if distance from p1 is shorter.
  X2 <- 0 - X1
  Y2 <- slope * X2
  newpoint2 <- c(X2,Y2)
  dist2 <- sqrt(sum((p1 - newpoint2)^2))
  if (dist1 <= dist2) {newpoint <- newpoint1}else {newpoint <- newpoint2}
  out <- list(newpoint=newpoint)
  return(out)}
trp<-project.point(rp1$col.dimensions[1,1:2],rp1$col.dimensions[3,1:2])$newpoint
pop<-project.point(rp1$col.dimensions[2,1:2],rp1$col.dimensions[3,1:2])$newpoint
rp1 <-rio.plot(m2,include.int="no",r1=1:15,case.col = "grey60",case.names = ISO3,var.name.col = "red")
rp1$gg.obj + scale_x_continuous(limits=c(-2.2,2.8)) +
  geom_segment(aes(x = 0, y = 0, xend = trp[1], yend = trp[2]),colour="grey60",size=2) +
  geom_segment(aes(x = 0, y = 0, xend = pop[1], yend = pop[2]),colour="black",size=2)
p3 <- rp1$gg.obj + scale_x_continuous(limits=c(-2.2,2.8)) +
  geom_segment(aes(x = 0, y = 0, xend = trp[1], yend = trp[2]),colour="grey60",size=2) +
  geom_segment(aes(x = 0, y = 0, xend = pop[1], yend = pop[2]),colour="black",size=2) +theme(axis.title = element_text(size = 15),
                                                                                             axis.text = element_text(size = 15))
p3
ggsave(p3, file="Figure2.3.revised.eps", device=cairo_ps)


# Fig 2.4
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

p4 <- rp1$gg.obj + scale_x_continuous(limits=c(-2.2,2.8)) +
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
  
p4
ggsave(p4, file="Figure2.4.revised.eps", device=cairo_ps)




# Fig 2.5

rp5 <- rio.plot(m2,include.int="no",r1=1:15,case.col = "grey60",case.names = ISO3,var.name.col = "grey25",exclude.vars=1:2)
rp5$gg.obj + annotate("rect",xmin=-2.3,xmax=-.83,ymin=-.5,ymax=1.25,alpha=.15,fill="black") +
  annotate("rect",xmin=-.8,xmax=2.1,ymin=-2.1,ymax=.12,alpha=.15,fill="black")  +
  annotate("rect",xmin=-.5,xmax=3,ymin=.4,ymax=1.6,alpha=.15,fill="black")
p5 <- rp5$gg.obj + annotate("rect",xmin=-2.3,xmax=-.83,ymin=-.5,ymax=1.25,alpha=.15,fill="black") +
  annotate("rect",xmin=-.8,xmax=2.1,ymin=-2.1,ymax=.12,alpha=.15,fill="black")  +
  annotate("rect",xmin=-.5,xmax=3,ymin=.4,ymax=1.6,alpha=.15,fill="black") + theme(axis.title = element_text(size = 15),
                                                                                   axis.text = element_text(size = 15))
ggsave(p5, file="Figure2.5.revised.eps", device=cairo_ps)

# cosine similarities for Fig 2.6
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

p6 <- ggplot(UUtD,aes(x=Var1,y=Var2,fill=value)) + theme_classic() +
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

ggsave(p6, file="Figure2.6.revised.eps", device=cairo_ps)


