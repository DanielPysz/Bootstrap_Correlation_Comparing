########################
##  BOOTSTRAPPING... ###
########################
# data preparing 
met = "pearson"
fvectorX <- "first vector of variable X"
svectorX <- "second vector of variable X"
fX <- length(fvectorX)
sX <- length(svectorX)

fvectorY <- "first vector of variable Y"
svectorY <- "second vector of variable Y"
fY <- length(fvectorY)
sY <- length(svectorY)

variableX <- c(fvectorX, svectorX)
variableY <- c(fvectorY, svectorY)
dtboot = data.frame("VariableX" = c(fvectorX, svectorX), "VariableY" = c(fvectorY, svectorY))

test.observed = cor(fvectorX,fvectorY, method = met) - cor(svectorX,svectorY, method = met)

# let's bootstrap...
set.seed(2137)   # for reproducibility
n <- length(dtboot$VariableX)  # the number of observations to sample
B <- 10000  # the number of bootstrap samples

variable <- dtboot$VariableY # the variable we will resample from

# now, get those bootstrap samples (without loops!)
BootstrapSamplesX <- matrix( sample(variableX, size= n*B, replace=TRUE), 
                              nrow=n, ncol=B)
BootstrapSamplesY <- matrix( sample(variableY, size= n*B, replace=TRUE), 
                            nrow=n, ncol=B)

# now, calculate the means (Yc and Ym) for each of the bootstrap samples
#  (the inefficeint, but transparent way...best to start simple, and once
#   working well, then make code more efficent)
# initialize the vector to store the TEST-STATS
Boot.test.stat1 <- rep(0,B)
Boot.test.stat2 <- rep(0,B)
Boot.test.stat3 <- rep(0, B)
# run through a loop, each time calculating the bootstrap test.stat

for (i in 1:B){
  Boot.test.stat1[i] <- cor(as.vector(BootstrapSamplesX[1:fX,i]),as.vector(BootstrapSamplesY[1:fY,i]), method = met)
  Boot.test.stat2[i] <- cor(as.vector(BootstrapSamplesX[(fX+1):length(variableX),i]),as.vector(BootstrapSamplesY[(fY+1):length(variableY),i]), method = met)
  
}

Boot.observed = (Boot.test.stat1 - Boot.test.stat2)
hist(Boot.observed)
# and, let's calculate the bootstrap p-value...

# and if we ask for the mean of all of those, it treats 0=FALSE, 1=TRUE
#...calculate the p-value
mean( Boot.observed >= test.observed)

# now, recall the difference between "statistical significance" and 

# let's take a look at a density plot of all the Bootstrap test-stats, and 
# add in our Observed test stat
plot(density(Boot.observed), 
     xlab=expression( group("|", bar(Yc) - bar(Ym), "|") ) , 
     main="Bootstrap Test Stats", las=1)
abline(v=test.stat1, col="blue", lty="dotted")
text(60,0.0005, "p-value", col="blue", cex=0.7)



