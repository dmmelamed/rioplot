rm(list=ls()) # Clear the workspace
nation = c("Aul89","Bel92","Can91","Den92","Fin91","Fr89","Ger89","Ire87","It91","Neth91","Nor91","Swe92","Swi82","UK91","US91")
y = c(11.9,6,6.5,5.9,3.7,9.8,4.3,29.4,14.3,7.3,1.7,5.8,3.8,16.8,11.7)
names(y) = nation
gdp = c(7734.418,6258.508,7894.777,7450.465,5712.95,6938.063,6746.054,3905.857,5507.27,7390.348,6507.316,7965.854,11419.35,7982.377,11871.49)
pov = c(23.3,26.8,22.5,26.4,11.9,36.1,15.2,39.2,30.7,22.1,9.2,23.7,12.5,29.6,21)
tran = c(7.3,19.3,9.5,13.5,10.4,17.8,14.8,10.5,14.5,21.5,13.4,14.6,9.4,10.1,8.8)
constant = rep(1,15)




###### Table 1. regression result #################################### 
# Dep = y, Indep= tran
X = cbind(constant,tran)
rownames(X) = nation
b = solve(t(X)%*%X)%*%t(X)%*%y
yhat = X%*%b
r = y-yhat
resid_sqrd = r*r
mse = (t(r)%*%r)/(dim(X)[1] - dim(X)[2])
mse = as.numeric(mse)
variance = mse*diag(solve(t(X)%*%X))
round(variance, 8)
se = sqrt(variance)
reg = summary(lm(y~tran))
pvalue = reg$coefficients[,4]
table1 = cbind(b, se, variance, pvalue)
colnames(table1)= c('b', 'se', 'var', 'p-value')
table1 




X = cbind(constant,tran)
rownames(X) = nation

mse_j = resid_sqrd/(dim(X)[1] - dim(X)[2])
mse_j = round(mse_j,8)

var = matrix(0, 15, 2)
denom = dim(var)[1]-dim(var)[2]
for (i in 1:dim(var)[1]) {
  var[i,] = (resid_sqrd[i]/denom)*diag(solve(t(X)%*%X))
}
var=as.data.frame(var)
names(var)=c("intercept","tran")
var_j = round(var[2],8)

table2 = cbind(nation, y, tran, resid_sqrd, mse_j, var_j )
table2 = as.data.frame(table2)
names(table2) = c("Country-Year", "y", "x", "ej_t_ej", "mse_j", "var_j")
table2


table3 = cbind(nation, y, tran, gdp, pov)
table3 = as.data.frame(table3)
names(table3) = c("Country-Year", "y", "x1", "x2", "x3")
table3


X = cbind(constant,tran, gdp, pov)
rownames(X) = nation
b = solve(t(X)%*%X)%*%t(X)%*%y
yhat = X%*%b
r = y-yhat
resid_sqrd = r*r
mse = (t(r)%*%r)/(dim(X)[1] - dim(X)[2])
mse = as.numeric(mse)
variance = mse*diag(solve(t(X)%*%X))
variance = round(variance, 8)
se = sqrt(variance)
reg = summary(lm(y~tran+gdp+pov))
pvalue = reg$coefficients[,4]
pvalue = round(pvalue, 8)
table4 = cbind(b, se, variance, pvalue)
colnames(table4)= c('b', 'se', 'var', 'p-value')
table4 

