# hansen regression kink is applied for each sheet of the 4 excel files:
# DATA_results_kinkIMF.xlsx
# DATA_results_kinkUN.xlsx
# DATA_results_kinkWB.xlsx
# DATA_results_kinkWTO.xlsx


# Load in Data
library(readxl)
dataR <- read_excel("DATA_results_kinkUN.xlsx", sheet = "SWE", range = "A25:G118")
# FOR FIN : range = "A26:G118"
# FOR HUN: range = "A35:G118"
# FOR ITA: range = "A47:G118"
# FOR LUX: range = "A38:G118"
# FOR SVN: range = "A14:G118"

dataR <- data.frame(dataR)

#######################
# if no lag is included
# n = nrow(dataR)
# year = dataR[1:n,1]
# EL = dataR[1:n,3]
# IP = dataR[1:n,4]
# NG = dataR[1:n,5]
# RE = dataR[1:n,6]
# SEN = dataR[1:n,7]
#######################
# if lag is included
n = nrow(dataR)
year = dataR[1:(n-1),1]
EL  = dataR[2:n,3] 
IP = dataR[1:(n-1),4]
NG = dataR[1:(n-1),5]
RE = dataR[1:(n-1),6]
SEN = dataR[1:(n-1),7]
#######################

# # Time-Series Data Plots, Figure 1ab in paper
# windows()
# plot(year,gdp,type="l",ylab="GDP Growth Rate")
# savePlot(file="fig1a.eps",type="eps",dev.cur())
# 
# windows()
# plot(year,debt1,type="l",ylab="Debt/GDP")
# savePlot(file="fig1b.eps",type="eps",dev.cur())

# Define variables
y = EL
x = SEN
n = length(y)
z = cbind(IP,NG,RE,matrix(1,n,1))
#######################

gammas = seq(-0.027, 0.14,by=0.005) # Grid on Threshold parameter for estimation gammas = seq(0.2,0.8,by=0.005) for the global tone, gammas = seq(0.2,0.9,by=0.005) for the untone
dx = seq(-0.0764, 0.168,by=0.01) # Grid on regression function for display,dx = seq(0,0.9,by=0.1) for GLOBAL         
level = 0.9 # For confidence sets                
boot = 10000 # Number of bootstrap replications               
Ceps = c(0.5,1,2,4) # For numerical delta method bootstrap              

############################################

# Some useful functions
reg <- function(X,y) {
  X <- qr(X)
  as.matrix(qr.coef(X,y))
}

pos.part <- function(x) x*(x>0)
neg.part <- function(x) x*(x<0)

pt1 = proc.time()

# Linear Model
x0 = cbind(SEN,z) ############ CHECK HERE !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
k0 = ncol(x0)
kz = ncol(z)
x00 = solve(crossprod(x0))
bols = x00%*%crossprod(x0,y)
e0 = y - x0%*%bols
sse0 = sum(e0^2)
sigols = sse0/n
v0 = x00%*%crossprod(x0*matrix(e0,n,k0))%*%x00*(n/(n-k0))
seols = as.matrix(sqrt(diag(v0)))

# Threshold Model
grid = length(gammas)
rd = length(dx)
sse = matrix(0,grid,1)
k = kz + 3
for (j in 1:grid) {
  gamj=gammas[j]
  x1 = cbind(neg.part(x-gamj),pos.part(x-gamj),z)
  e1 = y - x1%*%reg(x1,y)
  sse[j] = sum(e1^2)
}
gi = which.min(sse)
gammahat = gammas[gi]
ssemin = sse[gi]
x1 = cbind(neg.part(x-gammahat),pos.part(x-gammahat),z)
bt = reg(x1,y)
et = y - x1%*% bt
hg = - (x<gammahat)*bt[1] - (x>gammahat)*bt[2]
x2 = cbind(x1,hg)
hg2 = crossprod(cbind((x<gammahat),(x>gammahat)),et)
xx2 = matrix(0,k,k)
xx2[1:2,k]=hg2
xx2[k,1:2]=t(hg2)
xxi = solve(crossprod(x2) + xx2)
v = xxi%*%crossprod(x2*matrix(et,n,k))%*%xxi*(n/(n-k))
betahat = rbind(bt,gammahat)
se = as.matrix(sqrt(diag(v)))
sig = sum(et^2)/n
wt = n*(sse0-ssemin)/ssemin
wg = n*(sse-ssemin)/ssemin

# # Plot Least Squares Criterion, Figure 3 in paper
# windows()
# plot(gammas,sse,type="l",ylab="Least Squares Criterion",xlab="Threshold Parameter")
# savePlot(file="fig3.eps",type="eps",dev.cur())
# 
# # Regression Estimate
G = cbind(neg.part(dx-gammahat),pos.part(dx-gammahat),matrix(1,rd,1)%*%colMeans(z))
yf = G%*%bt

# Bootstrap & Testing
waldb = matrix(0,grid,boot)
sseb  = matrix(0,grid,boot)
betab = array(0,c(grid,k-1,boot))
u = matrix(rnorm(n*boot),n,boot)
eb = matrix(e0,n,boot)*u
yb = matrix(x1%*%bt,n,boot) + matrix(et,n,boot)*u
eb0 = eb - x0%*%reg(x0,eb)
bsse0 = colSums(eb0^2)
for (j in 1:grid) {
  gamj = gammas[j]
  x2 = cbind(neg.part(x-gamj),pos.part(x-gamj),z)
  eb0 = eb - x2%*%reg(x2,eb)
  bsse = colSums(eb0^2)
  waldb[j,] = n*(bsse0-bsse)/bsse
  bb = reg(x2,yb)
  eb1 = yb - x2%*%bb
  sseb[j,] = colSums(eb1^2)
  betab[j,,] = bb
}

# Multiplier Bootstrap test for Threshold
wb = apply(waldb,2,max)
pv = mean(wb > matrix(wt,boot,1))
crit = quantile(wb,probs=level)

# Threshold Regression Estimates
gib = apply(sseb,2,which.min)
gamb = gammas[gib]

# Symmetric Percentile Confidence Interval Construction
betahatb = matrix(0,k-1,boot)
ci = matrix(0,k,2)
for (j in 1:(k-1)){
  bj = diag(betab[gib,j,])-bt[j]
  qj = quantile(abs(bj),probs=level)
  ci[j,1] = bt[j] - qj
  ci[j,2] = bt[j] + qj
  betahatb[j,] = t(bj)
}

# Confidence Interval for Threshold
sseb0 = colSums((yb - x1%*%reg(x1,yb))^2)
sseminb = apply(sseb,2,min)
wgb = n*(sseb0-sseminb)/sseminb
qa = qchisq(level,1)
wia = (wg > qa)
cga = c(gammas[which.min(wia)],gammas[grid+1-which.min(rev(wia))])
qb = quantile(wgb,probs=level)
wib = (wg > qb)
cgb = c(gammas[which.min(wib)],gammas[grid+1-which.min(rev(wib))])
ci[k,] = cgb

# Threshold Regression Confidence Intervals
mdx = t(matrix(dx,rd,boot))-gammahat
thetab = t(G%*%betahatb)
cn = length(Ceps)
yf1 = matrix(0,rd,cn)
yf2 = matrix(0,rd,cn)
for (j in 1:cn) {
  eps = Ceps[j]
  h = matrix(gamb-gammahat,boot,rd)*eps
  thetaq = thetab + ((neg.part(mdx-h)-neg.part(mdx))*bt[1] + (pos.part(mdx-h)-pos.part(mdx))*bt[2])/eps
  qf = apply(abs(thetaq),2,quantile,probs=level)
  yf1[,j] = yf - qf
  yf2[,j] = yf + qf
}

pt2 = proc.time()

sink("growth.out")
cat("Linear Model, coefficients and error variance","\n")
print(cbind(bols,seols),digits=2)
cat("Error variance","\n")
print(sigols,digits=4)


cat("Wald Test for Threshold, p-value, & critical value","\n")
print(c(wt,pv,crit,level),digits=3)
cat("Threshold Model Estimates, s.e.'s, and Bootstrap confidence intervals","\n")
print(cbind(betahat,se,ci),digits=2)
cat("Error variance","\n")
print(sig,digits=4)

cat("Bootstrap Critical value for threshold parameter interval",qb,"\n")
cat("Computation Time: replications and seconds","\n")
print(c(boot,pt2[3]-pt1[3]),digits=3)

sink()
# 
# # Plot Confidence Interval Construction for Threshold (Figure 4)
# windows()
# plot(gammas,wg,type="l",ylab="Threshold F Statistic",xlab="Threshold Parameter")
# cr1 = matrix(qa,grid,1)
# cr2 = matrix(qb,grid,1)
# lines(gammas,cr1,lty="dashed",col="blue")
# lines(gammas,cr2,lty="dotted",col="red")
# arrows(cga[1],qa,cga[1],0,lty="dashed",col="blue")
# arrows(cga[2],qa,cga[2],0,lty="dashed",col="blue")
# arrows(cgb[1],qb,cgb[1],0,lty="dotted",col="red")
# arrows(cgb[2],qb,cgb[2],0,lty="dotted",col="red")
# legend("topright",c("Bootstrap Critical Value","Asymptotic Critical Value"),lty=c("dotted","dashed"),col=c("red","blue"))
# savePlot(file="fig4.eps",type="eps",dev.cur())
# 
# # Scatter plot, regression line, and confidence intervals (Figure 2)
# windows()
# plot(SEN,EL,ylab="EL PRICE",xlab="TONE") ########## CRUCIAL!!!!!!!!!!!!!!l x and y is what? l.globaltone, globaltone untone l.untone
# lines(dx,yf)
# lines(dx,yf1[,2],lty="dashed",col="blue")
# lines(dx,yf2[,2],lty="dashed",col="blue")
# yk = colMeans(z)%*%bt[3:(2+kz)]
# points(gammahat,yk,col="red",bg="red",pch=22)
# savePlot(file="fig2.eps",type="eps",dev.cur())
# # 
# windows()
# plot(dx,yf,ylab="GDP Growth Rate",xlab="Debt/GDP",type="l",ylim=c(-8,8),yaxt="n")
# axis(2,at=c(-8,-4,0,4,8))
# lines(dx,yf1[,1],lty="dashed",col="orange")
# lines(dx,yf2[,1],lty="dashed",col="orange")
# lines(dx,yf1[,2],lty="dotted",col="red")
# lines(dx,yf2[,2],lty="dotted",col="red")
# lines(dx,yf1[,3],lty="dashed",col="blue")
# lines(dx,yf2[,3],lty="dashed",col="blue")
# lines(dx,yf1[,4],lty="dotted",col="black")
# lines(dx,yf2[,4],lty="dotted",col="black")
# points(gammahat,yk,col="red",bg="red",pch=22)
# leg1 = bquote(paste(epsilon[n]==.(Ceps[1]),n^-.5))
# leg2 = bquote(paste(epsilon[n]==.(Ceps[2]),n^-.5))
# leg3 = bquote(paste(epsilon[n]==.(Ceps[3]),n^-.5))
# leg4 = bquote(paste(epsilon[n]==.(Ceps[4]),n^-.5))
# leg_text = c(as.expression(leg1),as.expression(leg2),as.expression(leg3),as.expression(leg4))
# 
# legend("bottomleft",leg_text,lty=c("dashed","dotted","dashed","dotted"),col=c("orange","red","blue","black"))
# savePlot(file="fig7.eps",type="eps",dev.cur())
# 
# 

###########################################
# 
# write.table(betahat,"betahat2.txt")
# write.table(se,"se2.txt")
# write.table(ci,"ci2.txt")
# write.table(pv,"pv2.txt")
# write.table(crit,"crit2.txt")

betahat
se
wt
pv
