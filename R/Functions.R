rio.plot<-function(m1,exclude.vars="no",r1="none",case.names="",col.names="no",h.just=-.2,v.just=0,case.col="blue",var.name.col="black",include.int="yes",group.cases=1,model.type="OLS"){
  X1 <- NULL; X2<- NULL; agg<-NULL; gr<-NULL; no1<-NULL; no2<-NULL; x<-NULL
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
  
  if(is(V2,"numeric")){
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
  if(is(V2,"matrix")){
    lineseg1<-rbind(project.point(V2[1,1:2],V2[nrow(V2),1:2])$newpoint,V2[1,1:2])
    lineseg1<-data.frame(lineseg1,1)
    colnames(lineseg1)<-c("x","y","gr")
    if(l1 > 1){
      for(i in 2:l1){
        lineseg2<-rbind(project.point(V2[i,1:2],V2[nrow(V2),1:2])$newpoint,V2[i,1:2])
        lineseg2<-data.frame(lineseg2,i)
        colnames(lineseg2)<-c("x","y","gr")
        lineseg1<-rbind(lineseg1,lineseg2)}}}
  
  if(col.names[1]=="no"){
    
    if(r1[1]=="none"){if(is(V2,"matrix")){
      p1 <- ggplot2::ggplot()  + 
        ggplot2::theme_classic() + 
        ggplot2::labs(x="First Dimension",y="Second Dimension") +
        ggplot2::geom_vline(xintercept=0) + 
        ggplot2::geom_hline(yintercept=0) + 
        ggplot2::geom_abline(intercept=0,slope=(V2[nrow(V2),2]/V2[nrow(V2),1]))+ 
        ggplot2::geom_text(data=pdat,ggplot2::aes(x=X1,y=X2,label=variables, hjust=h.just,vjust=v.just),color=var.name.col) + 
        ggplot2::guides(color = "none") +
        ggplot2::geom_line(data=lineseg1,mapping=ggplot2::aes(x=x,y=y,group=gr),color="grey65") + 
        ggplot2::coord_fixed(ratio=1)  + 
        ggplot2::geom_point(data=pdat,ggplot2::aes(x=X1,y=X2))}else{
          p1 <- ggplot2::ggplot()  + 
            ggplot2::theme_classic() + 
            ggplot2::labs(x="First Dimension",y="Second Dimension") +
            ggplot2::geom_vline(xintercept=0) + 
            ggplot2::geom_hline(yintercept=0) + 
            ggplot2::geom_abline(intercept=0,slope=(V2[2]/V2[1]))+ 
            ggplot2::geom_text(data=pdat,ggplot2::aes(x=X1,y=X2,label=variables, hjust=h.just,vjust=v.just),color=var.name.col) + 
            ggplot2::guides(color = "none") +
            ggplot2::coord_fixed(ratio=1)  + 
            ggplot2::geom_point(data=pdat,ggplot2::aes(x=X1,y=X2))}
      
    }else {if(length(group.cases)==1){if(is(V2,"matrix")){
      p1 <- suppressMessages(ggplot2::ggplot()   + 
        ggplot2::theme_classic() + 
        ggplot2::labs(x="First Dimension",y="Second Dimension") +
        ggplot2::geom_vline(xintercept=0) + 
        ggplot2::geom_hline(yintercept=0) + 
        ggplot2::geom_abline(intercept=0,slope=(V2[nrow(V2),2]/V2[nrow(V2),1]))+ 
        ggplot2::geom_text(data=pdat,ggplot2::aes(x=X1,y=X2,label=variables, hjust=h.just,vjust=v.just),color=var.name.col) + 
        ggplot2::guides(color = "none") +
        ggplot2::geom_line(data=lineseg1,mapping=ggplot2::aes(x=x,y=y,group=gr),color="grey65") +
        ggplot2::geom_point(data=U2,ggplot2::aes(x=no1,y=no2),color=case.col) + 
        ggplot2::geom_text(data=U2,ggplot2::aes(x=no1,y=no2,label=case.names), hjust=h.just,vjust=v.just) + 
        ggplot2::coord_fixed(ratio=1) + 
        ggplot2::geom_point(data=pdat,ggplot2::aes(x=X1,y=X2)))}else{
          p1 <- suppressMessages(ggplot2::ggplot()   + 
            ggplot2::theme_classic() + 
            ggplot2::labs(x="First Dimension",y="Second Dimension") +
            ggplot2::geom_vline(xintercept=0) + 
            ggplot2::geom_hline(yintercept=0) + 
            ggplot2::geom_abline(intercept=0,slope=(V2[2]/V2[1]))+ 
            ggplot2::geom_text(data=pdat,ggplot2::aes(x=X1,y=X2,label=variables, hjust=h.just,vjust=v.just),color=var.name.col) + 
            ggplot2::guides(color = "none") +
            ggplot2::geom_point(data=U2,ggplot2::aes(x=no1,y=no2),color=case.col) + 
            ggplot2::geom_text(data=U2,ggplot2::aes(x=no1,y=no2,label=case.names), hjust=h.just,vjust=v.just) + 
            ggplot2::coord_fixed(ratio=1) + 
            ggplot2::geom_point(data=pdat,ggplot2::aes(x=X1,y=X2)))}
      
    }else {U3 <- data.frame(U2)
    U3 <- data.frame(U3,agg=group.cases)
    colnames(U3)[1:2] <- c("no1","no2")
    U5 <- data.frame(aggregate(U3$no1,by=list(U3$agg),FUN="mean"),pov=aggregate(U3$no2,by=list(U3$agg),FUN="mean")[,2])
    colnames(U5) <- c("agg","x","y")
    if(is(V2,"matrix")){
      p1 <- suppressMessages(ggplot2::ggplot()  + 
        ggplot2::theme_classic() + 
        ggplot2::labs(x="First Dimension",y="Second Dimension") +
        ggplot2::geom_vline(xintercept=0) + 
        ggplot2::geom_hline(yintercept=0) + 
        ggplot2::geom_abline(intercept=0,slope=(V2[nrow(V2),2]/V2[nrow(V2),1]))+ 
        ggplot2::geom_text(data=pdat,ggplot2::aes(x=X1,y=X2,label=variables, hjust=h.just,vjust=v.just),color=var.name.col) + 
        ggplot2::guides(color = "none") +
        ggplot2::geom_line(data=lineseg1,mapping=ggplot2::aes(x=x,y=y,group=gr),color="grey65") +
        ggplot2::geom_point(data=U5,ggplot2::aes(x=x,y=y),color=case.col) + 
        ggplot2::geom_text(data=U5,ggplot2::aes(x=x,y=y,label=agg), hjust=h.just,vjust=v.just) + 
        ggplot2::coord_fixed(ratio=1) + 
        ggplot2::geom_point(data=pdat,ggplot2::aes(x=X1,y=X2)))}else{
          p1 <- suppressMessages(ggplot2::ggplot()  + 
            ggplot2::theme_classic() + 
            ggplot2::labs(x="First Dimension",y="Second Dimension") +
            ggplot2::geom_vline(xintercept=0) + ggplot2::geom_hline(yintercept=0) + 
            ggplot2::geom_abline(intercept=0,slope=(V2[2]/V2[1]))+ 
            ggplot2::geom_text(data=pdat,ggplot2::aes(x=X1,y=X2,label=variables, hjust=h.just,vjust=v.just),color=var.name.col) + 
            ggplot2::guides(color = "none") +
            ggplot2::geom_point(data=U5,ggplot2::aes(x=x,y=y),color=case.col) + 
            ggplot2::geom_text(data=U5,ggplot2::aes(x=x,y=y,label=agg), hjust=h.just,vjust=v.just) + 
            ggplot2::coord_fixed(ratio=1) + 
            ggplot2::geom_point(data=pdat,ggplot2::aes(x=X1,y=X2)))}
    
    }}
  }else{   if(r1[1]=="none"){if(is(V2,"matrix")){
    p1 <- suppressMessages(ggplot2::ggplot()  + ggplot2::theme_classic() + 
      ggplot2::labs(x="First Dimension",y="Second Dimension") +
      ggplot2::geom_vline(xintercept=0) + ggplot2::geom_hline(yintercept=0) + 
      ggplot2::geom_abline(intercept=0,slope=(V2[nrow(V2),2]/V2[nrow(V2),1]))+ 
      ggplot2::geom_text(data=pdat,ggplot2::aes(x=X1,y=X2,label=col.names, hjust=h.just,vjust=v.just),color=var.name.col) + 
      ggplot2::guides(color = "none") +
      ggplot2::geom_line(data=lineseg1,mapping=ggplot2::aes(x=x,y=y,group=gr),color="grey65") + 
      ggplot2::coord_fixed(ratio=1)  + 
      ggplot2::geom_point(data=pdat,ggplot2::aes(x=X1,y=X2)))}else{
        p1 <- suppressMessages(ggplot2::ggplot()  + ggplot2::theme_classic() + 
          ggplot2::labs(x="First Dimension",y="Second Dimension") +
          ggplot2::geom_vline(xintercept=0) + ggplot2::geom_hline(yintercept=0) + 
          ggplot2::geom_abline(intercept=0,slope=(V2[2]/V2[1]))+ 
          ggplot2::geom_text(data=pdat,ggplot2::aes(x=X1,y=X2,label=col.names, hjust=h.just,vjust=v.just),color=var.name.col) + 
          ggplot2::guides(color = "none") +
          ggplot2::coord_fixed(ratio=1)  + 
          ggplot2::geom_point(data=pdat,ggplot2::aes(x=X1,y=X2)))}
    
  }else {if(length(group.cases)==1){if(is(V2,"matrix")){
    p1 <- suppressMessages(ggplot2::ggplot()   + ggplot2::theme_classic() + ggplot2::labs(x="First Dimension",y="Second Dimension") +
      ggplot2::geom_vline(xintercept=0) + ggplot2::geom_hline(yintercept=0) + 
      ggplot2::geom_abline(intercept=0,slope=(V2[nrow(V2),2]/V2[nrow(V2),1]))+
      ggplot2::geom_text(data=pdat,ggplot2::aes(x=X1,y=X2,label=col.names, hjust=h.just,vjust=v.just),color=var.name.col) + 
      ggplot2::guides(color = "none") +
      ggplot2::geom_line(data=lineseg1,mapping=ggplot2::aes(x=x,y=y,group=gr),color="grey65") +
      ggplot2::geom_point(data=U2,ggplot2::aes(x=no1,y=no2),color=case.col) + 
      ggplot2::geom_text(data=U2,ggplot2::aes(x=no1,y=no2,label=case.names), hjust=h.just,vjust=v.just) + 
      ggplot2::coord_fixed(ratio=1) + 
      ggplot2::geom_point(data=pdat,ggplot2::aes(x=X1,y=X2)))}else{
        p1 <- suppressMessages(ggplot2::ggplot()   + ggplot2::theme_classic() + 
          ggplot2::labs(x="First Dimension",y="Second Dimension") +
          ggplot2::geom_vline(xintercept=0) + ggplot2::geom_hline(yintercept=0) + 
          ggplot2::geom_abline(intercept=0,slope=(V2[2]/V2[1]))+ 
          ggplot2::geom_text(data=pdat,ggplot2::aes(x=X1,y=X2,label=col.names, hjust=h.just,vjust=v.just),color=var.name.col) + 
          ggplot2::guides(color = "none") +
          ggplot2::geom_point(data=U2,ggplot2::aes(x=no1,y=no2),color=case.col) + 
          ggplot2::geom_text(data=U2,ggplot2::aes(x=no1,y=no2,label=case.names), hjust=h.just,vjust=v.just) + 
          ggplot2::coord_fixed(ratio=1) + 
          ggplot2::geom_point(data=pdat,ggplot2::aes(x=X1,y=X2)))}
    
  }else {U3 <- data.frame(U2)
  U3 <- data.frame(U3,agg=group.cases)
  colnames(U3)[1:2] <- c("no1","no2")
  U5 <- data.frame(aggregate(U3$no1,by=list(U3$agg),FUN="mean"),pov=aggregate(U3$no2,by=list(U3$agg),FUN="mean")[,2])
  colnames(U5) <- c("agg","x","y")
  if(is(V2,"matrix")){
    p1 <- suppressMessages(ggplot2::ggplot()  + ggplot2::theme_classic() + 
      ggplot2::labs(x="First Dimension",y="Second Dimension") +
      ggplot2::geom_vline(xintercept=0) + 
      ggplot2::geom_hline(yintercept=0) + 
      ggplot2::geom_abline(intercept=0,slope=(V2[nrow(V2),2]/V2[nrow(V2),1]))+ 
      ggplot2::geom_text(data=pdat,ggplot2::aes(x=X1,y=X2,label=col.names, hjust=h.just,vjust=v.just),color=var.name.col) + 
      ggplot2::guides(color = "none") +
      ggplot2::geom_line(data=lineseg1,mapping=ggplot2::aes(x=x,y=y,group=gr),color="grey65") +
      ggplot2::geom_point(data=U5,ggplot2::aes(x=x,y=y),color=case.col) + 
      ggplot2::geom_text(data=U5,ggplot2::aes(x=x,y=y,label=agg), hjust=h.just,vjust=v.just) + 
      ggplot2::coord_fixed(ratio=1) + 
      ggplot2::geom_point(data=pdat,ggplot2::aes(x=X1,y=X2)))}else{
        p1 <- suppressMessages(ggplot2::ggplot()  + ggplot2::theme_classic() + 
          ggplot2::labs(x="First Dimension",y="Second Dimension") +
          ggplot2::geom_vline(xintercept=0) + 
          ggplot2::geom_hline(yintercept=0) + 
          ggplot2::geom_abline(intercept=0,slope=(V2[2]/V2[1]))+ 
          ggplot2::geom_text(data=pdat,ggplot2::aes(x=X1,y=X2,label=col.names, hjust=h.just,vjust=v.just),color=var.name.col) + 
          ggplot2::guides(color = "none") +
          ggplot2::geom_point(data=U5,ggplot2::aes(x=x,y=y),color=case.col) + 
          ggplot2::geom_text(data=U5,ggplot2::aes(x=x,y=y,label=agg), hjust=h.just,vjust=v.just) + 
          ggplot2::coord_fixed(ratio=1) + 
          ggplot2::geom_point(data=pdat,ggplot2::aes(x=X1,y=X2)))}
  
  }}}
  
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
                  message("model.type should be OLS, logit, poisson, or nb.")} 
          
          cov<-solve(t(X) %*% W %*% X)
          # Weight the variance/covariance matrix by the case's contributions to the MSE
          look2 <- array(0, c(nrow(cov),ncol(cov),length(r)))
          for (k in 1:length(r)) {
            look2[,,k] <- (look[k]/mse) * cov
          }
          case.variances <- (diag(look2[,,1]))
          for(i in 2:nrow(X)){
            case.variances2 <- diag(look2[,,i])
            case.variances <- rbind(case.variances,case.variances2)}}
  
  colnames(case.variances) <- names(m1$coefficients)
  rownames(case.variances) <- paste(1:nrow(X))
  
  out<-list(gg.obj=p1,row.dimensions=U2[,1:2],col.dimensions=V2[,1:2],case.variances=case.variances,U=u.out,UUt=uut.out)
  return(out)}

decompose.model <- function(m1,group.by=group.by,include.int="yes",model.type="OLS"){
  
  X <- as.matrix(m1$model[,2:ncol(m1$model)])
  if(include.int=="yes"){X<-cbind(Intercept=1,X)}
  
  if(model.type=="OLS"){
    # Start OLS Code
    B <- solve(t(X) %*% X) %*% t(X) %*% diag(as.numeric(m1$model[,1])) 
    ttab <- aggregate(t(B),by=list(group.by),FUN="sum")
    ttab2 <- t(ttab[,-1])
    colnames(ttab2) <- ttab[,1]
    ttab2<-cbind(ttab2,coef(m1))
    colnames(ttab2)[ncol(ttab2)]<-"Model Coefs"
    
    re2<-resid(m1)^2
    mse <- re2/(nrow(m1$model)-m1$rank)
    d.xinv<-diag(solve(t(X) %*% X))       
    case.variances <- matrix(NA,nrow=nrow(m1$model),ncol=length(m1$coefficients))
    case.variances[,1] <-mse *d.xinv[1]
    if(length(d.xinv)>1){
      for(i in 2:length(d.xinv)) {
        case.variances[,i] <-mse *d.xinv[i]}}
    
    vtab <- aggregate(case.variances,by=list(group.by),FUN="sum")
    vtab2 <- t(vtab[,-1])
    colnames(vtab2) <- vtab[,1]
    vtab2<-cbind(vtab2,diag(vcov(m1)))
    colnames(vtab2)[ncol(vtab2)]<-"Model Variances"
    rownames(vtab2) <- colnames(X)
    if(sum(round(rowSums(ttab2[,-(ncol(ttab2))]),3) != round(ttab2[,4],3))==nrow(ttab2)){warning("Things are not adding up. Did you specify the model.type correctly?")}
    out<-list(decomp.coef=ttab2,decomp.var=vtab2)
    return(out)
    # End OLS Code
  } else if(model.type=="logit"){
    
    
    # Define weight matrix and pseudovalues for GLM
    W<-diag((m1$fitted.values*(1-m1$fitted.values)))
    z<-X %*% coef(m1) + solve(W) %*% (m1$model[,1]-m1$fitted.values)
    B <- solve(t(X) %*% W %*% X)%*%t(X)%*%W%*%diag(as.numeric(z))
    ttab <- aggregate(t(B),by=list(group.by),FUN="sum")
    ttab2 <- t(ttab[,-1])
    colnames(ttab2) <- ttab[,1]
    ttab2<-cbind(ttab2,coef(m1))
    colnames(ttab2)[ncol(ttab2)]<-"Model Coefs"
    
    r <- m1$residuals
    mse<-as.numeric((t(r)%*%r)/(nrow(m1$model)-m1$rank))
    look <- rep(0, length(r))
    for (i in 1:length(r)) {
      r1<-m1$residuals[i]
      mse1<-(t(r1)%*%r1)/(nrow(m1$model)-m1$rank)
      mse1<-as.numeric(mse1)
      look[i] <- mse1}
    
    cov<-solve(t(X) %*% W %*% X)
    look2 <- array(0, c(nrow(cov),ncol(cov),length(r)))
    for (k in 1:length(r)) {
      look2[,,k] <- (look[k]/mse) * cov
    }
    case.variances <- (diag(look2[,,1]))
    for(i in 2:nrow(X)){
      case.variances2 <- diag(look2[,,i])
      case.variances <- rbind(case.variances,case.variances2)}
    vtab <- aggregate(case.variances,by=list(group.by),FUN="sum")
    vtab2 <- t(vtab[,-1])
    colnames(vtab2) <- vtab[,1]
    vtab2<-cbind(vtab2,diag(vcov(m1)))
    colnames(vtab2)[ncol(vtab2)]<-"Model Variances"
    rownames(vtab2) <- colnames(X)
    out<-list(decomp.coef=ttab2,decomp.var=vtab2)
    if(sum(round(rowSums(ttab2[,-(ncol(ttab2))]),3) != round(ttab2[,4],3))==nrow(ttab2)){warning("Things are not adding up. Did you specify the model.type correctly?")}
    return(out)
  }else if(model.type=="poisson"){
    W<-diag(m1$fitted.values)
    z<-X %*% coef(m1) + (m1$model[,1]-m1$fitted.values)/m1$fitted.values
    B <- solve(t(X) %*% W %*% X)%*%t(X)%*%W%*%diag(as.numeric(z))
    ttab <- aggregate(t(B),by=list(group.by),FUN="sum")
    ttab2 <- t(ttab[,-1])
    colnames(ttab2) <- ttab[,1]
    ttab2<-cbind(ttab2,coef(m1))
    colnames(ttab2)[ncol(ttab2)]<-"Model Coefs"
    
    r <- m1$residuals
    mse<-as.numeric((t(r)%*%r)/(nrow(m1$model)-m1$rank))
    look <- rep(0, length(r))
    for (i in 1:length(r)) {
      r1<-m1$residuals[i]
      mse1<-(t(r1)%*%r1)/(nrow(m1$model)-m1$rank)
      mse1<-as.numeric(mse1)
      look[i] <- mse1}
    
    cov<-solve(t(X) %*% W %*% X)
    look2 <- array(0, c(nrow(cov),ncol(cov),length(r)))
    for (k in 1:length(r)) {
      look2[,,k] <- (look[k]/mse) * cov
    }
    case.variances <- (diag(look2[,,1]))
    for(i in 2:nrow(X)){
      case.variances2 <- diag(look2[,,i])
      case.variances <- rbind(case.variances,case.variances2)}
    vtab <- aggregate(case.variances,by=list(group.by),FUN="sum")
    vtab2 <- t(vtab[,-1])
    colnames(vtab2) <- vtab[,1]
    vtab2<-cbind(vtab2,diag(vcov(m1)))
    colnames(vtab2)[ncol(vtab2)]<-"Model Variances"
    rownames(vtab2) <- colnames(X)
    out<-list(decomp.coef=ttab2,decomp.var=vtab2)
    if(sum(round(rowSums(ttab2[,-(ncol(ttab2))]),3) != round(ttab2[,4],3))==nrow(ttab2)){warning("Things are not adding up. Did you specify the model.type correctly?")}
    return(out)
  }else if(model.type=="nb"){
    W<-diag(m1$fitted.values/(1+(1/m1$theta)*m1$fitted.values))
    z<-X %*% coef(m1) + (m1$model[,1]-m1$fitted.values)/m1$fitted.values
    B <- solve(t(X) %*% W %*% X)%*%t(X)%*%W%*%diag(as.numeric(z))
    ttab <- aggregate(t(B),by=list(group.by),FUN="sum")
    ttab2 <- t(ttab[,-1])
    colnames(ttab2) <- ttab[,1]
    ttab2<-cbind(ttab2,coef(m1))
    colnames(ttab2)[ncol(ttab2)]<-"Model Coefs"
    
    r <- m1$residuals
    mse<-as.numeric((t(r)%*%r)/(nrow(m1$model)-m1$rank))
    look <- rep(0, length(r))
    for (i in 1:length(r)) {
      r1<-m1$residuals[i]
      mse1<-(t(r1)%*%r1)/(nrow(m1$model)-m1$rank)
      mse1<-as.numeric(mse1)
      look[i] <- mse1}
    
    cov<-solve(t(X) %*% W %*% X)
    look2 <- array(0, c(nrow(cov),ncol(cov),length(r)))
    for (k in 1:length(r)) {
      look2[,,k] <- (look[k]/mse) * cov
    }
    case.variances <- (diag(look2[,,1]))
    for(i in 2:nrow(X)){
      case.variances2 <- diag(look2[,,i])
      case.variances <- rbind(case.variances,case.variances2)}
    vtab <- aggregate(case.variances,by=list(group.by),FUN="sum")
    vtab2 <- t(vtab[,-1])
    colnames(vtab2) <- vtab[,1]
    vtab2<-cbind(vtab2,diag(vcov(m1)))
    colnames(vtab2)[ncol(vtab2)]<-"Model Variances"
    rownames(vtab2) <- colnames(X)
    out<-list(decomp.coef=ttab2,decomp.var=vtab2)
    if(sum(round(rowSums(ttab2[,-(ncol(ttab2))]),3) != round(ttab2[,4],3))==nrow(ttab2)){warning("Things are not adding up. Did you specify the model.type correctly?")}
    return(out)
  }
  else{message("model.type should be OLS, logit, poisson, or nb.")} 
  }


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


cosine <- function(x,y) {
  # This assumes that x and y are vectors of the same length.
  Sxy <- sum(x * y) # this is the sum of crossproducts
  distX <- sqrt(sum(x^2)) # This is distance of x from the origin
  distY <- sqrt(sum(y^2)) # distance of y from the origin
  out <- Sxy / (distX * distY)
  return(out)
}
