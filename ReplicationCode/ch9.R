

rm(list=ls())
nation <- c("Aul89","Bel92","Can91","Den92","Fin91","Fr89","Ger89","Ire87","It91","Neth91","Nor91","Swe92","Swi82","UK91","US91")
nation.long <- c("Australia", "Belgium", "Canada", "Denmark", "Finland", "France", "Germany", "Ireland", "Italy", "Netherlands", "Norway", "Sweden", "Switzerland", "United Kingdom", "U.S.A.")
ISO3 <- c("AUS", "BEL", "CAN", "DNK", "FIN", "FRA", "DEU", "IRL", "ITA", "NLD", "NOR", "SWE", "CHE", "GBR", "USA")
dv <- c(11.9,6,6.5,5.9,3.7,9.8,4.3,29.4,14.3,7.3,1.7,5.8,3.8,16.8,11.7)
names(dv)<-nation
gdp <- c(7734.418,6258.508,7894.777,7450.465,5712.95,6938.063,6746.054,3905.857,5507.27,7390.348,6507.316,7965.854,11419.35,7982.377,11871.49)
gdp <- round(gdp, digits = 0) # For compatibility to Kenworthy.
pov <- c(23.3,26.8,22.5,26.4,11.9,36.1,15.2,39.2,30.7,22.1,9.2,23.7,12.5,29.6,21)
tran <- c(7.3,19.3,9.5,13.5,10.4,17.8,14.8,10.5,14.5,21.5,13.4,14.6,9.4,10.1,8.8)

# NOTE: Get rid of the decimal values in "gdp" so that it matches 
#  Kenworthy's data (p. 1127 of Kenworthy 1999)
gdp <- round(gdp, digits = 0)

X <- cbind(tran, gdp, pov)
rownames(X) <- nation.long
colnames(X) <- c("tran", "gdp" ,"pov1")

present.data.df <- data.frame(nation.long, ISO3, tran, gdp, pov, dv)

# STANDARDIZE THE DATA (Z-scores):
dvZ <- as.vector(scale(dv))
names(dvZ)  <- names(dv) <- ISO3
Z <- scale(X)
colnames(Z) <- paste(colnames(X),"Z",sep="")

# Table 9.1:
table9.1 <- cbind(Z, dvZ)
rownames(table9.1) <- ISO3
table9.1 <- round(table9.1, digits = 3)

table9.1



# Use a canned program to reproduce Kenworthy's reg coefs for data in Zscore form:
m1 <- lm(dvZ ~ Z - 1)
round(coef(m1), digits = 4)

summary(m1); # Rsquare = .7733
# adjRsquare = .7166
# F(3,12) = 13.64, p = .000358
# coefs = { -.4439 [tranZ], -.1824 [gdpZ], +.7826 [pov1Z] }

svdZ <- svd(Z)
U <- svdZ$u
V <- svdZ$v
S <- diag(svdZ$d)

rownames(U) <- ISO3
rownames(V) <- colnames(Z)

# Table 9.2:

# panel (a):

round(U, digits = 3)

# panel (b):

round(S, digits = 3)

# panel (c);

round(V, digits = 3)


# Table 9.3: NOTE--What makes this difficult, is that the table
#                  reports values of (U %*% t(V)) squared --
#                  that is, each cell in Table 9.3 is the square
#                  of the corresponding cell of (U %*% t(V)) --
#                  BUT, the parentheses in Table 9.3 indicate
#                  negative values in the (not squared) 
#                  matrix U %*% t(V). (This is super-easy to
#                  do in Excel, where you can represent negative
#                  numbers with parentheses. I'm not sure how to
#                  do it in R.)

UVt <- U %*% t(V)
UUt <- U %*% t(U)
table9.3 <- cbind((UVt^2), diag(UUt))
sort.order <- order((diag(UUt)), decreasing=TRUE)
table9.3 <- table9.3[sort.order,]
table9.3 <- round(table9.3, 3)
colnames(table9.3) <- c("Transfers", "GDP", "Pre-pov", "hi")

signs9.3 <- sign(UVt)
signs9.3 <- signs9.3[sort.order,]

table9.3

signs9.3

# Now -- for the network part of the chapter -- we'll switch
#        to the Belsley-Kuh-Welsch data
country.data <- c(11.43,12.07,13.17,5.75,12.88,8.79,0.6,11.9,4.98,10.78,16.85,3.59,11.24,12.64,12.55,10.67,3.01,7.7,1.27,9,11.34,14.28,21.1,3.98,10.35,15.48,10.25,14.65,10.67,7.3,4.44,2.02,12.7,12.78,12.49,11.14,13.3,11.77,6.86,14.13,5.13,2.81,7.81,7.56,9.22,18.56,7.72,9.24,8.89,4.71,29.35,23.32,23.8,41.89,42.19,31.72,39.74,44.75,46.64,47.64,24.42,46.31,27.84,25.06,23.31,25.62,46.05,47.32,34.03,41.31,31.16,24.52,27.01,41.74,21.8,32.54,25.95,24.71,32.61,45.04,43.56,41.18,44.19,46.26,28.96,31.94,31.92,27.74,21.44,23.49,43.42,46.12,23.27,29.81,46.4,45.25,41.12,28.13,43.69,47.2,2.87,4.41,4.43,1.67,0.83,2.85,1.34,0.67,1.06,1.14,3.93,1.19,2.37,4.7,3.35,3.1,0.87,0.58,3.08,0.96,4.19,3.48,1.91,0.91,3.73,2.47,3.67,3.25,3.17,1.21,1.2,1.05,1.28,1.12,2.85,2.28,1.52,2.87,4.54,3.73,1.08,1.21,4.46,3.43,0.9,0.56,1.73,2.72,2.07,0.66,2329.68,1507.99,2108.47,189.13,728.47,2982.88,662.86,289.52,276.65,471.24,2496.53,287.77,1681.25,2213.82,2457.12,870.85,289.71,232.44,1900.1,88.94,1139.95,1390,1257.28,207.68,2449.39,601.05,2231.03,1740.7,1487.52,325.54,568.56,220.56,400.06,152.01,579.51,651.11,250.96,768.79,3299.49,2630.96,389.66,249.87,1813.93,4001.89,813.39,138.33,380.47,766.54,123.58,242.69,2.87,3.93,3.82,0.22,4.56,2.43,2.67,6.51,3.08,2.8,3.99,2.19,4.32,4.52,3.44,6.28,1.48,3.19,1.12,1.54,2.99,3.54,8.21,5.81,1.57,8.12,3.62,7.66,1.76,2.48,3.61,1.03,0.67,2,7.48,2.19,2,4.35,3.01,2.7,2.96,1.13,2.01,2.45,0.53,5.14,10.23,1.88,16.71,5.08)
country.data <- matrix(country.data, ncol=5, byrow=FALSE)
colnames(country.data) <- c("SR", "POP15", "POP75", "DPI", "ChangeDPI")
countries <- c("Australia     ", "Austria       " ,"Belgium       " ,"Bolivia       ", "Brazil        ", "Canada        ")
countries <- c(countries, "Chile         ", "China (Taiwan)", "Colombia      " ,"Costa Rica    ", "Denmark       ", "Ecuador       ")
countries <- c(countries,"Finland       ", "France        ", "Germany(F.R.) " ,"Greece        " ,"Guatemala     " ,"Honduras      ")
countries <- c(countries, "Iceland       " ,"India         " ,"Ireland       ", "Italy         ", "Japan         ", "Korea         ")
countries <- c(countries, "Luxembourg    ", "Malta         ", "Norway        " ,"Netherlands   " ,"New Zealand   ", "Nicaragua     ")
countries <- c(countries, "Panama        " ,"Paraguay      ", "Peru          ", "Philippines   " ,"Portugal      ", "South Africa  ")
countries <- c(countries, "South Rhodesia", "Spain         " ,"Sweden        " ,"Switzerland   ", "Turkey        ", "Tunisia       ")
countries <- c(countries, "United Kingdom", "United States ", "Venezuela     ", "Zambia        ", "Jamaica       ", "Uruguay       ")
countries <- c(countries, "Libya         ", "Malaysia      ")

# The model that Belsley et al fit:
mBel <- lm(SR ~ POP15 + POP75 + DPI + ChangeDPI, data=as.data.frame(country.data))

# My analysis uses their given (raw) data:
# Dependent variable:
SR <- country.data[,1]
X <- country.data[,-1]
# Add intercept:
u <- rep(1,50)
X <- cbind(u,X)

# Do SVD:
svdX <- svd(X)
U <- svdX$u
V <- svdX$v
S <- diag(svdX$d)

rownames(U) <- 1:50 # corresponding to the "countries" variable above.
rownames(V) <- c("u", "POP15", "POP75", "DPI", "ChangeDPI")

UUt <- U %*% t(U)

# From the hat matrix (UUt), make a matrix of cosines
d <- diag(UUt)
d.sqrt <- sqrt(d)
cosUUt <- UUt / outer(d.sqrt, d.sqrt)
# take a little look:
cosUUt[1:6,1:6]

# An equivalent way to get cosUUt:
DminusHalf<- diag(1/d.sqrt)
check.it.out <- DminusHalf %*% UUt %*% DminusHalf
sum(abs(check.it.out - cosUUt)) # they are the same

# Obtaining a partition of cases
data("RonsFunctions") # Load several functions written by Ronald L. Breiger (e.g., concor)
r1 <- cor(cosUUt)
b <- concor(r1, 1:50) # include all the cases
b1 <- b$b1
b2 <- b$b2
b <- concor(r1, b1)
b11 <- b$b1
b12 <- b$b2
b <- concor(r1, b2)
b21 <- b$b1
b22 <- b$b2
perm4 <- c(b11, b12, b21, b22)
sizperm4 <- c(length(b11), length(b12), length(b21), length(b22))
memb4 <-rep(4,50)
memb4[b11]<-1
memb4[b12]<-2
memb4[b21]<-3

require(reshape2)
UUtD <- melt(cosUUt)
UUtD$Var1 <- factor(UUtD$Var1,levels=1:50)
UUtD$Var2 <- factor(UUtD$Var2,levels=50:1)
require(tidyverse)
p1 <- ggplot(UUtD,aes(x=Var1,y=Var2,fill=value)) + theme_classic() +
  geom_tile() + scale_fill_gradient(limits=c(-1,1),low="grey90",high="black",breaks=c(-.99,-.5,0,.5,.99),labels=c("-1","-.5","0",".5","1")) + labs(x="",y="",fill="Cosine\nSimilarity") + 
  #scale_x_continuous(breaks=rev(perm4),labels=paste(rev(perm4))) + scale_y_continuous(breaks=perm4,labels=paste(perm4)) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) + theme(legend.position = "bottom") +   
  theme(axis.title = element_text(size = 16),
        axis.text = element_text(size = 15),
        legend.title= element_text(size=15),
        legend.text=element_text(size=14))
p1 # Fig 9.1


# Figure 9.2: Next we use the blocking found from CONCOR:
# NOTE: the function "r2blocked" is in my workspace called "concor526.RData"

result <- r2blocked(cosUUt, perm4, sizperm4)
#round(result$dens[,,1], digits = 3)

cosUUt.blocked <- result$mod[,,1]


cosUUt2 <- cosUUt[rev(perm4),rev(perm4)]
UUtD2 <- melt(cosUUt2)
UUtD2$Var1 <- factor(UUtD2$Var1,levels=(perm4))
UUtD2$Var2 <- factor(UUtD2$Var2,levels=rev(perm4))

p2 <- ggplot(UUtD2,aes(x=Var1,y=Var2,fill=value)) + theme_classic() +
  geom_tile() + 
  scale_fill_gradient(limits=c(-1,1),low="grey90",high="black",breaks=c(-.99,-.5,0,.5,.99),labels=c("-1","-.5","0",".5","1")) + theme(legend.position="bottom") +
  labs(x="",y="",fill="Cosine\nSimilarity")
p2 +   geom_segment(aes(x = 14.5, y = 0, xend = 14.5, yend = 50.5),colour="white",size=1) +
  geom_segment(aes(x = 26.5, y = 0, xend = 26.5, yend = 50.5),colour="white",size=1) +
  geom_segment(aes(x = 43.5, y = 0, xend = 43.5, yend = 50.5),colour="white",size=1) +
  geom_segment(aes(x = 0, y = 7.5, xend = 50.5, yend = 7.5),colour="white",size=1) +
  geom_segment(aes(x = 0, y = 24.5, xend = 50.5, yend = 24.5),colour="white",size=1) +
  geom_segment(aes(x = 0, y = 36.5, xend = 50.5, yend = 36.5),colour="white",size=1) +
  geom_segment(aes(x = 14.5, y = 0, xend = 14.5, yend = 50.5),colour="black",size=.3,linetype="dashed") +
  geom_segment(aes(x = 26.5, y = 0, xend = 26.5, yend = 50.5),colour="black",size=.3,linetype="dashed") +
  geom_segment(aes(x = 43.5, y = 0, xend = 43.5, yend = 50.5),colour="black",size=.3,linetype="dashed") +
  geom_segment(aes(x = 0, y = 7.5, xend = 50.5, yend = 7.5),colour="black",size=.3,linetype="dashed") +
  geom_segment(aes(x = 0, y = 24.5, xend = 50.5, yend = 24.5),colour="black",size=.3,linetype="dashed") +
  geom_segment(aes(x = 0, y = 36.5, xend = 50.5, yend = 36.5),colour="black",size=.3,linetype="dashed") 
p2 <-p2 +   geom_segment(aes(x = 14.5, y = 0, xend = 14.5, yend = 50.5),colour="white",size=1) +
  geom_segment(aes(x = 26.5, y = 0, xend = 26.5, yend = 50.5),colour="white",size=1) +
  geom_segment(aes(x = 43.5, y = 0, xend = 43.5, yend = 50.5),colour="white",size=1) +
  geom_segment(aes(x = 0, y = 7.5, xend = 50.5, yend = 7.5),colour="white",size=1) +
  geom_segment(aes(x = 0, y = 24.5, xend = 50.5, yend = 24.5),colour="white",size=1) +
  geom_segment(aes(x = 0, y = 36.5, xend = 50.5, yend = 36.5),colour="white",size=1) +
  geom_segment(aes(x = 14.5, y = 0, xend = 14.5, yend = 50.5),colour="black",size=.3,linetype="dashed") +
  geom_segment(aes(x = 26.5, y = 0, xend = 26.5, yend = 50.5),colour="black",size=.3,linetype="dashed") +
  geom_segment(aes(x = 43.5, y = 0, xend = 43.5, yend = 50.5),colour="black",size=.3,linetype="dashed") +
  geom_segment(aes(x = 0, y = 7.5, xend = 50.5, yend = 7.5),colour="black",size=.3,linetype="dashed") +
  geom_segment(aes(x = 0, y = 24.5, xend = 50.5, yend = 24.5),colour="black",size=.3,linetype="dashed") +
  geom_segment(aes(x = 0, y = 36.5, xend = 50.5, yend = 36.5),colour="black",size=.3,linetype="dashed")  +   theme(axis.title = element_text(size = 16),
                                                                                                                   legend.title= element_text(size=15),
                                                                                                                   legend.text=element_text(size=14))
# Figure 9.2






# Figure 9.3: blocked MODEL for cosUUt:
cosUUt2 <- cosUUt.blocked[rev(perm4),rev(perm4)]
cosUUt2 <- cosUUt.blocked
UUtD2 <- melt(cosUUt2)
UUtD2$Var1 <- factor(UUtD2$Var1,levels=(perm4))
UUtD2$Var2 <- factor(UUtD2$Var2,levels=rev(perm4))

p3 <- ggplot(UUtD2,aes(x=Var1,y=Var2,fill=value)) + theme_classic() +   theme(legend.position="bottom") +
  geom_tile()
p3 + scale_fill_gradient(limits=c(-1,1),low="grey90",high="black",breaks=c(-.99,-.5,0,.5,.99),labels=c("-1","-.5","0",".5","1")) + 
  labs(x="",y="",fill="Cosine\nSimilarity") + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  geom_segment(aes(x = 14.5, y = 0, xend = 14.5, yend = 50.5),colour="white",size=1) +
  geom_segment(aes(x = 26.5, y = 0, xend = 26.5, yend = 50.5),colour="white",size=1) +
  geom_segment(aes(x = 43.5, y = 0, xend = 43.5, yend = 50.5),colour="white",size=1) +
  geom_segment(aes(x = 0, y = 7.5, xend = 50.5, yend = 7.5),colour="white",size=1) +
  geom_segment(aes(x = 0, y = 24.5, xend = 50.5, yend = 24.5),colour="white",size=1) +
  geom_segment(aes(x = 0, y = 36.5, xend = 50.5, yend = 36.5),colour="white",size=1) +
  geom_segment(aes(x = 14.5, y = 0, xend = 14.5, yend = 50.5),colour="black",size=.3,linetype="dashed") +
  geom_segment(aes(x = 26.5, y = 0, xend = 26.5, yend = 50.5),colour="black",size=.3,linetype="dashed") +
  geom_segment(aes(x = 43.5, y = 0, xend = 43.5, yend = 50.5),colour="black",size=.3,linetype="dashed") +
  geom_segment(aes(x = 0, y = 7.5, xend = 50.5, yend = 7.5),colour="black",size=.3,linetype="dashed") +
  geom_segment(aes(x = 0, y = 24.5, xend = 50.5, yend = 24.5),colour="black",size=.3,linetype="dashed") +
  geom_segment(aes(x = 0, y = 36.5, xend = 50.5, yend = 36.5),colour="black",size=.3,linetype="dashed") 
# 9.3

# The reiduals matrix, cosUUt.resid

cosUUt.resid <- cosUUt - cosUUt.blocked

result2 <- r2blocked(cosUUt.resid, perm4, sizperm4)

# Figure 9.4: residuals (note that the residuals look randomly-distributed)
cosUUt2 <- cosUUt.resid[rev(perm4),rev(perm4)]
UUtD2 <- melt(cosUUt2)
UUtD2$Var1 <- factor(UUtD2$Var1,levels=(perm4))
UUtD2$Var2 <- factor(UUtD2$Var2,levels=rev(perm4))

p4 <- ggplot(UUtD2,aes(x=Var1,y=Var2,fill=value)) + theme_classic() +
  geom_tile()
p4 + scale_fill_gradient(limits=c(-1,1),low="grey90",high="black",breaks=c(-.99,-.5,0,.5,.99),labels=c("-1","-.5","0",".5","1")) + labs(x="",y="",fill="Cosine\nSimilarity") + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +   theme(legend.position = "bottom") +
  geom_segment(aes(x = 14.5, y = 0, xend = 14.5, yend = 50.5),colour="white",size=1) +
  geom_segment(aes(x = 26.5, y = 0, xend = 26.5, yend = 50.5),colour="white",size=1) +
  geom_segment(aes(x = 43.5, y = 0, xend = 43.5, yend = 50.5),colour="white",size=1) +
  geom_segment(aes(x = 0, y = 7.5, xend = 50.5, yend = 7.5),colour="white",size=1) +
  geom_segment(aes(x = 0, y = 24.5, xend = 50.5, yend = 24.5),colour="white",size=1) +
  geom_segment(aes(x = 0, y = 36.5, xend = 50.5, yend = 36.5),colour="white",size=1) +
  geom_segment(aes(x = 14.5, y = 0, xend = 14.5, yend = 50.5),colour="black",size=.3,linetype="dashed") +
  geom_segment(aes(x = 26.5, y = 0, xend = 26.5, yend = 50.5),colour="black",size=.3,linetype="dashed") +
  geom_segment(aes(x = 43.5, y = 0, xend = 43.5, yend = 50.5),colour="black",size=.3,linetype="dashed") +
  geom_segment(aes(x = 0, y = 7.5, xend = 50.5, yend = 7.5),colour="black",size=.3,linetype="dashed") +
  geom_segment(aes(x = 0, y = 24.5, xend = 50.5, yend = 24.5),colour="black",size=.3,linetype="dashed") +
  geom_segment(aes(x = 0, y = 36.5, xend = 50.5, yend = 36.5),colour="black",size=.3,linetype="dashed") 
# Figure 9.4




