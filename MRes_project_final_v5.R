# MRes Project Final  Project submitted by SAMIPAN MAJUMDER (GUID: 2354272M) on 30th August 2019
# user  system elapsed Total Elapsed time is around 10 mins for the entire code. 
# 327.51   25.03  547.23

# Loading Required Packages

listOfPackages <- list("imputeTS","segmented","xts","VIM","forecast","zoo","splines","sm","mgcv","ggplot2","dynlm",
                       "FinTS","tseries","rugarch","lubridate","leaps","doBy","dplyr","ggfortify","reshape2",
                       "gamlss","changepoint","cpm","bcp","chngpt","missForest","plot3D","rgl","kza","TSA","MASS",
                       "data.table","GGally","CCA","ggcorrplot","MVN","CCP","vars","dlnm","ecp","changepoint.np","MSGARCH")
# TSA Package NOT Available in the latest R version.
# Loading Required Packages; if PAckage is not available then it is installed from cRAN Repository.
for (i in listOfPackages){
  # cat(i)
  if(! (i %in% installed.packages())){
    install.packages(i, dependencies = TRUE)
  }
  #library(i, character.only=TRUE)
}
  #lapply(X = listOfPackages,FUN = require, character.only=TRUE)
  lapply(X = listOfPackages, FUN = function(X) {
    do.call("require", list(X)) 
  })
# }


## About Original Dataset

# Loading Original Dataset

mydata <- read.csv(file = "sat_1.csv", sep=",", header = T)
data.table::data.table(mydata)
mydata.storm <- read.csv(file = "storm_days.csv", sep=",", header = T)
data.table::data.table(mydata.storm)

# Structure of Datasets
str(mydata)
str(mydata1)

# Histogram and summary of Datasets and Plots

hist(mydata$Bx)
summary(object = mydata$Bx)
plot(mydata$Bx)

hist(mydata$By)
summary(object = mydata$By)
plot(mydata$By)

hist(mydata$Bz)
summary(object = mydata$Bz)
plot(mydata$Bz)

# Yearwise and monthwise number of orbits for Original Data set

Dataf1 <- as.data.frame(matrix(ncol=4))
#names(Dataf) <- c("Year","Month","Orbits","Number of Orbits per month")
Dataf1 <- na.omit(Dataf1)

#Dataf <- rbind(Dataf,c(2004,"","",""))

for (year in unique(mydata$year)){
  # print(year)
  Dataf1 <- rbind(Dataf1, list(year,"","",""), stringsAsFactors=FALSE) 
  for (month in unique(mydata$month)){
    Dataf1 <- rbind(Dataf1, 
                    list(year,month,list(mydata$orbit[mydata$month==month & mydata$year==year]),
                         sum(mydata$month[mydata$year==year]== month)))
  }
}
names(Dataf1) <- c("Year","Month","Orbits","Number of Orbits per month")
#Dataf1 <- as.numeric(Dataf1[,4])
str(Dataf1)
head(Dataf1)

nOrb1 <-  c()
for (year in unique(mydata$year)){
  nOrb1<- c(nOrb1,
            sum(as.integer(Dataf1$`Number of Orbits per month`[which(Dataf1$Year==year)]),na.rm = T))
}
nOrb1
mean(nOrb1)
matrix(data = c(unique(mydata$year),nOrb1),ncol = 2)

# Finding Missing values
c <- 0
miss <- c()
for ( i in 1:nrow(mydata)){
  #cat("i = ",i,"\n")
  if ((is.na(mydata[i,4]) & (is.na(mydata[i,5])) & (is.na(mydata[i,6])))) {
    c <- c+1
    miss <- c(miss,i)
  }
}
c
miss


#Checking if consecutive imputed values are same or different.

chkEqual <- function(x){
  x0 <- x[1]
  c <- 0
  for (i in 2:length(x)){
    if (x[i]==x0){c <- c+1}
    x0 <- x[i] 
  }
  return(c)
}

# impute Missing values in original data using imputeTS package.

# Copying of original data set.
mydata.imputeTS <- mydata
str(mydata.imputeTS)

#Checking distribution of missing values 
plotNA.distribution(x = mydata.imputeTS$Bx)
statsNA(mydata.imputeTS$Bx)
imputeTS::plotNA.distributionBar(x = mydata.imputeTS$Bx)
imputeTS::plotNA.gapsize(x = mydata.imputeTS$Bx,byTotalNA = T)

# For variable Bx

# Imputing using Kalman Filter
imputets.Bx <- imputeTS::na_kalman(x = mydata.imputeTS$Bx,model = "StructTS",smooth = T) 
statsNA(imputets.Bx)
plotNA.distribution(x = imputets.Bx)
plotNA.distributionBar(x = imputets.Bx)
plotNA.imputations(x.withNA = mydata.imputeTS$Bx,x.withImputations = imputets.Bx)

# For variable By

# Imputing using Kalman Filter
imputets.By <- imputeTS::na_kalman(x = mydata.imputeTS$By,model = "StructTS",smooth = T) 
statsNA(imputets.By)
plotNA.distribution(x = imputets.By)
plotNA.distributionBar(x = imputets.By)
plotNA.imputations(x.withNA = mydata.imputeTS$By,x.withImputations = imputets.By)


# For variable Bz

# Imputing using Kalman Filter
imputets.Bz <- imputeTS::na_kalman(x = mydata.imputeTS$Bz,model = "StructTS",smooth = T) 
statsNA(imputets.Bz)
plotNA.distribution(x = imputets.Bz)
plotNA.distributionBar(x = imputets.Bz)
plotNA.imputations(x.withNA = mydata.imputeTS$Bz,x.withImputations = imputets.Bz)


#Checking if consecutive imputed values are same or different.
chkEqual(mydata.imputeTS.1$imputets.Bx)
chkEqual(mydata.imputeTS.1$imputets.By)
chkEqual(mydata.imputeTS.1$imputets.Bz)

#Checking if consecutive imputed values are same or different.
chkEqual(mydata.imputeTS.1$imputets.Bx)
chkEqual(mydata.imputeTS.1$imputets.By)
chkEqual(mydata.imputeTS.1$imputets.Bz)


# Creating New data frame for the imputed values from the Impute TS Package.

mydata.imputeTS.1 <- mydata.imputeTS
str(mydata.imputeTS.1)

if (!(c("imputets.Bx","imputets.By","imputets.Bz") %in% names(mydata.imputeTS.1))){
  mydata.imputeTS.1 <- cbind(mydata.imputeTS.1,imputets.Bx,imputets.By,imputets.Bz)
}
str(mydata.imputeTS.1)

### Time-point variable verison 2 for imputeTS Dataset
### In the Dissertation Report, the New time-point variable is referred to as "New_tp".
### In the R Code, this New-time point variable is referred to as "New_tp1". Basically they
### mean the same variable, especially important during modelling purpose.

if (!("New_tp1" %in% names(mydata.imputeTS.1))){
  
  New_tp1 <- c()
  for (i in 1:nrow(mydata.imputeTS.1)) {
    New_tp1[i] <- mydata.imputeTS.1$year[i] + 
      ((mydata.imputeTS.1$orbit[i]-1)/
         sum(as.integer(Dataf1$`Number of Orbits per month`[which(Dataf1$Year==mydata.imputeTS.1$year[i])]),na.rm = T))
  }
  mydata.imputeTS.1 <- cbind(mydata.imputeTS.1, New_tp1)
}

str(mydata.imputeTS.1)
head(mydata.imputeTS.1)

#############################################################3
### Exploratory Data Analysis

# Descriptive Statistics of ImputeTS Data

cor(x = mydata.imputeTS.1)
cor(x = mydata.imputeTS.1[,c(7,8,9)],method = "spearman")
cor(x = mydata.imputeTS.1[,c(7,8,9)],method = "kendall")

pairs(x = mydata.imputeTS.1,upper.panel = NULL)

cor.test(x = mydata.imputeTS.1$imputets.Bx,y = mydata.imputeTS.1$imputets.Bx,alternative = "t",method = "p")
cor.test(x = mydata.imputeTS.1$imputets.By,y = mydata.imputeTS.1$By,alternative = "t",method = "s")
cor.test(x = mydata.imputeTS.1$imputets.By,y = mydata.imputeTS.1$imputets.By,alternative = "t",method = "k")

# Cross-Correlation function
ccf(x = mydata.imputeTS.1$imputets.Bx,y = mydata.imputeTS.1$imputets.By,type = c("correlation","covariance"),plot = T)
ccf(x = mydata.imputeTS.1$imputets.Bx,y = mydata.imputeTS.1$imputets.By,type = c("correlation","covariance"),plot = F)

# Checking No. of outliers of the Data Set using Boxplot

boxplot(mydata.imputeTS.1[,4:6])$out

par(mfrow=c(1,3))
boxplot(Bx ~ year, data = mydata.imputeTS.1)
title(main = "Box-plot of Variable Bx Year-wise",xlab = "Year")
boxplot(By ~ year, data = mydata.imputeTS.1)
title(main = "Box-plot of Variable By Year-wise",xlab = "Year")
boxplot(Bz ~ year, data = mydata.imputeTS.1)
title(main = "Box-plot of Variable Bz Year-wise",xlab = "Year")
par(mfrow=c(1,1))


######## Time - Plot of the Response Variables.

#par(mfrow=c(3,1))
#par(mar = c(1.5,1.5,1.5,1.5), oma=c(0.5,0.5,0.5,0.5))
plot(x = mydata.imputeTS.1$New_tp1, y = mydata.imputeTS.1$imputets.Bx, type='l', xaxt='n',
     xlab="Year",ylab="Bx in nano-Tesla",main="Time-plot of variable Bx",col="red")
axis(side = 1,at = c(2003,2004,2005,2006,2007,2008,2009,2010,2011,2012,2013))
#points(x = mydata.imputeTS.1$New_tp1[which(is.na(mydata3$Bx))], y = mydata.imputeTS.1$imputets.Bx[which(is.na(mydata.imputeTS.1$Bx))], col="red")
#legend("topleft",legend = c("Original","Imputed"),lty = 1,pch = 1, col=c("black","red"),cex=0.6)

plot(x = mydata.imputeTS.1$New_tp1, y = mydata.imputeTS.1$imputets.By, type='l', xaxt='n',
     xlab="Year",ylab="By in nano-Tesla",main="Time-plot of variable By",col="green")
axis(side = 1,at = c(2003,2004,2005,2006,2007,2008,2009,2010,2011,2012,2013))
#points(x = mydata.imputeTS.1$New_tp1[which(is.na(mydata3$Bx))], y = mydata.imputeTS.1$imputets.By[which(is.na(mydata.imputeTS.1$Bx))], col="red")
#legend("topleft",legend = c("Original","Imputed"),lty = 1,pch = 1, col=c("black","red"),cex=0.6)

plot(x = mydata.imputeTS.1$New_tp1, y = mydata.imputeTS.1$imputets.Bz, type='l', xaxt='n',
     xlab="Year",ylab="Value of Bz in nano-Tesla",main="Time-plot of variable Bz",col="blue")
axis(side = 1,at = c(2003,2004,2005,2006,2007,2008,2009,2010,2011,2012,2013))
#points(x = mydata.imputeTS.1$New_tp1[which(is.na(mydata3$Bx))], y = mydata.imputeTS.1$imputets.Bz[which(is.na(mydata.imputeTS.1$Bx))], col="red")
#legend("topleft",legend = c("Original","Imputed"),lty = 1,pch = 1, col=c("black","red"),cex=0.6)

#par(mfrow=c(1,1))


# Histogram with normal density curve

g <- mydata.imputeTS.1$imputets.By
h <- hist(g, breaks = 10, density = 10,
          xlab = "Accuracy", main = "Overall") 
xfit <- seq(min(g), max(g), length = 40) 
yfit <- dnorm(xfit, mean = mean(g), sd = sd(g)) 
yfit <- yfit * diff(h$mids[1:2]) * length(g) 

lines(xfit, yfit, col = "black", lwd = 2)

myhist <- hist(mydata.imputeTS.1$imputets.Bx)
multiplier <- myhist$counts / myhist$density
mydensity <- density(mydata.imputeTS.1$imputets.Bx)
mydensity$y <- mydensity$y * multiplier[1]

plot(myhist)
lines(mydensity)

myx <- seq(min(mydata.imputeTS.1$imputets.Bx), max(mydata.imputeTS.1$imputets.Bx), length.out= 100)
mymean <- mean(mydata.imputeTS.1$imputets.Bx)
mysd <- sd(mydata.imputeTS.1$imputets.Bx)

normal <- dnorm(x = myx, mean = mymean, sd = mysd)
lines(myx, normal * multiplier[1], col = "blue", lwd = 2)

sd_x <- seq(mymean - 3 * mysd, mymean + 3 * mysd, by = mysd)
sd_y <- dnorm(x = sd_x, mean = mymean, sd = mysd) * multiplier[1]

segments(x0 = sd_x, y0= 0, x1 = sd_x, y1 = sd_y, col = "firebrick4", lwd = 2)


##### Density plots of the Response Variables Bx, By and Bz
par(mfrow=c(2,3))
#par(mar = c(2,2,2,2), oma=c(4,4,0.5,0.5))
# Filled Density Plot
dx <- density(mydata.imputeTS.1$imputets.Bx)
plot(dx, main="Kernel Density of Bx")
polygon(dx, col="red", border="blue")

dy <- density(mydata.imputeTS.1$imputets.By)
plot(dy, main="Kernel Density of By")
polygon(dy, col="green", border="blue")

dz <- density(mydata.imputeTS.1$imputets.Bz)
plot(dz, main="Kernel Density of Bz")
polygon(dz, col="orange", border="blue")

par(mfrow=c(1,1))


# Scatter-plots

par(mfrow=c(1,3))
plot(x = mydata.imputeTS.1$imputets.Bx,y = mydata.imputeTS.1$imputets.By, 
     col=c("red","green"),pch=c(16,17),xlab="Bx",ylab="By",main="Scatter plot Bx vs By")
legend("topright",legend = c("Bx","By"), col=c("red","green"),pch=c(16,17),cex=0.6)

plot(x = mydata.imputeTS.1$imputets.Bx,y = mydata.imputeTS.1$imputets.Bz, 
     col=c("red","green"),pch=c(16,17),xlab="Bx",ylab="Bz",main="Scatter plot Bx vs Bz")
legend("topright",legend = c("Bx","Bz"), col=c("red","green"),pch=c(16,17),cex=0.6)

plot(x = mydata.imputeTS.1$imputets.By,y = mydata.imputeTS.1$imputets.Bz, 
     col=c("red","green"),pch=c(16,17),xlab="By",ylab="Bz",main="Scatter plot By vs Bz")
legend("topright",legend = c("By","Bz"), col=c("red","green"),pch=c(16,17),cex=0.6)
par(mfrow=c(1,1))

# Comparing the original and imputed values of ImputeTS package

par(mfrow=c(3,2))
plot(x = mydata.imputeTS.1$New_tp1,y = mydata.imputeTS.1$Bx,xlab="Year",ylab="Bx",main="Original")
axis(side = 1,at = c(2003,2005,2007,2009,2011,2013), las=2)
plot(x = mydata.imputeTS.1$New_tp1, y = mydata.imputeTS.1$imputets.Bx,xlab="Year",ylab="Bx",main="Imputed")
axis(side = 1,at = c(2003,2005,2007,2009,2011,2013), las=2)
points(x = mydata.imputeTS.1$New_tp1[which(is.na(mydata.imputeTS.1$Bx))] ,
       y = mydata.imputeTS.1$imputets.Bx[which(is.na(mydata.imputeTS.1$Bx))], col="red")

plot(x = mydata.imputeTS.1$New_tp1,y = mydata.imputeTS.1$By,xlab="Year",ylab="By", main="Original")
axis(side = 1,at = c(2003,2005,2007,2009,2011,2013), las=2)
plot(x = mydata.imputeTS.1$New_tp1, y = mydata.imputeTS.1$imputets.By, xlab="Year",ylab="By", main="Imputed")
axis(side = 1,at = c(2003,2005,2007,2009,2011,2013), las=2)
points(x = mydata.imputeTS.1$New_tp1[which(is.na(mydata.imputeTS.1$By))] ,
       y = mydata.imputeTS.1$imputets.By[which(is.na(mydata.imputeTS.1$By))], col="red")

plot(x = mydata.imputeTS.1$New_tp1,y = mydata.imputeTS.1$Bz, xlab="Year",ylab="Bz", main="Original")
axis(side = 1,at = c(2003,2005,2007,2009,2011,2013), las=2)
plot(x = mydata.imputeTS.1$New_tp1, y = mydata.imputeTS.1$imputets.Bz, xlab="Year",ylab="Bz", main="Imputed")
axis(side = 1,at = c(2003,2005,2007,2009,2011,2013), las=2)
points(x = mydata.imputeTS.1$New_tp1[which(is.na(mydata.imputeTS.1$Bz))] ,
       y = mydata.imputeTS.1$imputets.Bz[which(is.na(mydata.imputeTS.1$Bz))], col="red")

par(mfrow=c(1,1))

# Comparing the densities of Original and Imputed Data

par(mfrow=c(3,2))
plot(density(mydata.imputeTS.1$imputets.Bx),main="Imputed Data")
plot(density(mydata3$Bx,na.rm = T), main="Original data")

plot(density(mydata.imputeTS.1$imputets.By), main="Imputed data")
plot(density(mydata3$By,na.rm = T), main="Original data")
plot(density(mydata.imputeTS.1$imputets.Bz), main="Imputed data")
plot(density(mydata3$Bz, na.rm = T), main="Original data")
par(mfrow=c(1,1))


######### Seasonality Plots for the Year 2003
par(mfrow=c(1,3))

plot(x = mydata.imputeTS.1$New_tp1[mydata.imputeTS.1$year==2003], 
     y = mydata.imputeTS.1$imputets.Bx[mydata.imputeTS.1$year==2003], 
     xlab = "Year", ylab="Bx",col="red",pch=16,main="Seasonality of Bx")
lines(smooth.spline(x = mydata.imputeTS.1$New_tp1[mydata.imputeTS.1$year==2003],
                    y = (mydata.imputeTS.1$imputets.Bx[mydata.imputeTS.1$year==2003])
                    ,spar = 0.5,nknots = 20),lwd=2)

plot(x = mydata.imputeTS.1$New_tp1[mydata.imputeTS.1$year==2003], 
     y = mydata.imputeTS.1$imputets.By[mydata.imputeTS.1$year==2003], 
     xlab = "Year", ylab="By",col="green",pch=16,main="Seasonality of By")
lines(smooth.spline(x = mydata.imputeTS.1$New_tp1[mydata.imputeTS.1$year==2003],
                    y = (mydata.imputeTS.1$imputets.By[mydata.imputeTS.1$year==2003])
                    ,spar = 0.5,nknots = 20),lwd=2)

plot(x = mydata.imputeTS.1$New_tp1[mydata.imputeTS.1$year==2003], 
     y = mydata.imputeTS.1$imputets.Bz[mydata.imputeTS.1$year==2003],
     xlab = "Year", ylab="Bz",col="blue",pch=16,main="Seasonality of Bz")
lines(smooth.spline(x = mydata.imputeTS.1$New_tp1[mydata.imputeTS.1$year==2003],
                    y = (mydata.imputeTS.1$imputets.Bz[mydata.imputeTS.1$year==2003])
                    ,spar = 0.5,nknots = 20),lwd=2)

par(mfrow=c(1,1))

##################################################
#### Third Secondary Question of Interest
#####################################################
# Finding Seasonality yearwise in case of ImputeTS Package

# Iterate through 11 years

# Creating a matrix of year and corresponding row number of NA values

matyd <- matrix(data = c(mydata$year[is.na(mydata.imputeTS.1$Bx)], which(is.na(mydata.imputeTS.1$Bx))),ncol=2,byrow = F)
head(matyd)

# Forvariable Bx

par(mfrow=c(3,4))
par(mar=c(5.1,4.1,4.1,2.1))
for (year in unique(mydata.imputeTS.1$year)){
  plot(x = mydata.imputeTS.1$New_tp1[mydata.imputeTS.1$year==year], 
       y = mydata.imputeTS.1$imputets.Bx[mydata.imputeTS.1$year==year], 
       xlab = "Year", ylab="Bx", col="red",pch=16)
  lines(smooth.spline(x = mydata.imputeTS.1$New_tp1[mydata.imputeTS.1$year==year],
                      y = (mydata.imputeTS.1$imputets.Bx[mydata.imputeTS.1$year==year])
                      ,spar = 0.5,nknots = 20),lwd=2)
  # points(x = mydata.imputeTS.1$New_tp1[matyd[matyd[,1]==year,2]] ,
  #        y = mydata.imputeTS.1$imputets.Bx[matyd[matyd[,1]==year,2]], col="red")
}

par(mfrow=c(1,1))
title(main = "Seasonality plot of Bx year-wise")


# For variable By

par(mfrow=c(3,4))
par(mar=c(5.1,4.1,4.1,2.1))
for (year in unique(mydata.imputeTS.1$year)){
  plot(x = mydata.imputeTS.1$New_tp1[mydata.imputeTS.1$year==year], 
       y = mydata.imputeTS.1$imputets.By[mydata.imputeTS.1$year==year], 
       xlab="Year", ylab="By", pch=16, col="green")
  lines(smooth.spline(x = mydata.imputeTS.1$New_tp1[mydata.imputeTS.1$year==year],
                      y = (mydata.imputeTS.1$imputets.By[mydata.imputeTS.1$year==year])
                      ,spar = 0.5,nknots = 20),lwd=2)
  # points(x = mydata.imputeTS.1$New_tp1[matyd[matyd[,1]==year,2]] ,
  #        y = mydata.imputeTS.1$imputets.By[matyd[matyd[,1]==year,2]],  col="red")
}

par(mfrow=c(1,1))
title(main = "Seasonality plot of By Year-wise")

# for variable Bz 

par(mfrow=c(3,4))
par(mar=c(5.1,4.1,4.1,2.1))
for (year in unique(mydata.imputeTS.1$year)){
  plot(x = mydata.imputeTS.1$New_tp1[mydata.imputeTS.1$year==year], 
       y = mydata.imputeTS.1$imputets.Bz[mydata.imputeTS.1$year==year], 
       xlab="Year", ylab="Bz",col="blue",pch=16)
  lines(smooth.spline(x = mydata.imputeTS.1$New_tp1[mydata.imputeTS.1$year==year],
                      y = (mydata.imputeTS.1$imputets.Bz[mydata.imputeTS.1$year==year])
                      ,spar = 0.5,nknots = 20),lwd=2)
  # points(x = mydata.imputeTS.1$New_tp1[matyd[matyd[,1]==year,2]] ,
  #        y = mydata.imputeTS.1$imputets.Bz[matyd[matyd[,1]==year,2]], col="red")
}

par(mfrow=c(1,1))
title(main = "Seasonality plot of Bz Year-wise")

# Multivariate time-series plot

ts.plot(mydata.imputeTS.1[,7:9])

df1 <- data.frame(x = mydata.imputeTS.1$New_tp1, y = mydata.imputeTS.1$imputets.Bx)
df2 <- data.frame(x = mydata.imputeTS.1$New_tp1, y = mydata.imputeTS.1$imputets.By)
df3 <- data.frame(x = mydata.imputeTS.1$New_tp1, y = mydata.imputeTS.1$imputets.Bz)

ggplot(data = df1,aes(x,y))+geom_line(aes(color="Bx"))+
  geom_line(data=df2,aes(color="By"))+
  geom_line(data=df3,aes(color="Bz"))+
  labs(color="Legend text")

# 3d plots

par(mfrow=c(1,2))
plot3D::scatter3D(x = mydata.imputeTS.1$imputets.Bx, y = mydata.imputeTS.1$imputets.By, 
                  z = mydata.imputeTS.1$imputets.Bz,bty="f",pch=16)

plot3D::scatter3D(x = mydata.imputeTS.1$imputets.Bx, y = mydata.imputeTS.1$imputets.By, 
                  z = mydata.imputeTS.1$imputets.Bz,phi = 20,theta = 70,
                  surf = list( x = mesh(x = mydata.imputeTS.1$imputets.Bx, y = 1:ncol(mydata.imputeTS.1[,c(7,8,9)]))$x,
                               y = mesh(x = mydata.imputeTS.1$imputets.Bx ,y = 1:ncol(mydata.imputeTS.1[,c(7,8,9)]))$y,
                               z = as.matrix(mydata.imputeTS.1[,c(7,8,9)]),shade = 0.1),bty="f",pch=16)
par(mfrow=c(1,1))


### Standard Analysis Chapter #################
########################################################
## First Primary Question of Interest
########################################################
## Univariate Time-Series Models
################################################################

###################   Normal Linear Regression Model ####
summary(MASS::stepAIC(object = lm(imputets.Bx ~ New_tp1 + month, data = mydata.imputeTS.1),
                      scope = list(upper = ~ New_tp1*month, lower = ~1)))

### For variable Bx

summary(lm(imputets.Bx ~ New_tp1, data = mydata.imputeTS.1)) # Rsqadj 0.03652
summary(lm(imputets.Bx ~ month, data = mydata.imputeTS.1)) # Rsqadj 0.07913
summary(lm(imputets.Bx ~ New_tp1 + month, data = mydata.imputeTS.1)) #Rsqadj 0.106
summary(lm(imputets.Bx ~ New_tp1 + month + New_tp1:month, data = mydata.imputeTS.1)) # Rsqadj 0.1807

#### For variable By

summary(lm(imputets.By ~ New_tp1, data = mydata.imputeTS.1))
summary(lm(imputets.By ~ month, data = mydata.imputeTS.1))
summary(lm(imputets.By ~ New_tp1 + month, data = mydata.imputeTS.1))
summary(lm(imputets.By ~ New_tp1 + month + New_tp1:month, data = mydata.imputeTS.1))

#### for variable Bz

summary(lm(imputets.Bz ~ New_tp1, data = mydata.imputeTS.1))
summary(lm(imputets.Bz ~ month, data = mydata.imputeTS.1))
summary(lm(imputets.Bz ~ New_tp1 + month, data = mydata.imputeTS.1))
summary(lm(imputets.Bz ~ New_tp1 + month + New_tp1:month, data = mydata.imputeTS.1))

########## Using Manually All possible Normal Linear regression

predictors <- c("year","month","orbit")

## Create list of models
list.of.models <- lapply(seq_along((predictors)), function(n) {
  
  left.hand.side  <- "imputets.Bx"
  right.hand.side <- apply(X = combn(predictors, n), MARGIN = 2, paste, collapse = " + ")
  #terms.to.select <- apply(X = combn(predictors, n), MARGIN = 2, paste, collapse = " ")
  interaction.term1 <- c()
  
  if (n != 1){
    for ( i in 2:n){
      interaction.term <- apply(X = combn(predictors,i), MARGIN = 2, paste, collapse = ":")
      #cat(i," ",interaction.term,"\n")
      interaction.term1 <- c(interaction.term1, interaction.term)
      # cat(i," ",interaction.term1,"\n")
    }
    #cat(interaction.term1,"\n")
    cat(paste(interaction.term1, collapse = " + "),"\n")
    paste(left.hand.side, paste(right.hand.side, paste(interaction.term1, collapse = " + "), sep = " + "), sep = "  ~  ")
  }
  else {
    paste(left.hand.side, right.hand.side, sep = "  ~  ")
  }
  
  # if (n!=1){
  # interaction.term <- apply(X = combn(predictors, n), MARGIN = 2, paste, collapse = ":")
  # right.hand.side1 <- paste(right.hand.side, interaction.term, collapse = " + ")
  # paste(left.hand.side, right.hand.side1, sep = "  ~  ")}
  
  # include interaction term
  
  
})

#left.hand.side1 <- 
list.of.models1 <- list("1","year","month","orbit","New_tp1",
                        "year + month")

## Convert to a vector
vector.of.models <- unlist(list.of.models)
vector.of.models

list.of.fits <- lapply(vector.of.models, function(x) {
  
  formula    <- as.formula(x)
  fit        <- lm(formula, data = mydata.imputeTS.1)
  result.AIC <- extractAIC(fit)
  Adjusted_R_squared <- summary(fit)$adj.r.squared
  
  data.frame(num.predictors = result.AIC[1]-1,
             AIC            = result.AIC[2],
             model          = x, 
             Adjusted.R.squared = Adjusted_R_squared)
  #Adjusted_R_squared = fit$adj.r.squared)
})
(list.of.fits)

orderBy( ~ AIC, do.call(rbind, list.of.fits))


################# Polynomial Regression Model
#### Response Variable Bx

summary(lm(imputets.Bx ~ poly(New_tp1,2), data = mydata.imputeTS.1))
summary(lm(imputets.Bx ~ poly(month,degree = 2), data = mydata.imputeTS.1))
summary(lm(imputets.Bx ~ poly(New_tp1,degree = 2) + poly(month,degree = 2), data = mydata.imputeTS.1))
summary(lm(imputets.Bx ~ poly(New_tp1,degree = 2) + poly(month,degree = 2) + 
             poly(New_tp1,degree = 2):poly(month,degree = 2), data = mydata.imputeTS.1))

############## Polynomial Regression Models including Linear Terms and Interaction Terms

# For variable Bx

modelpoly.1 <- (lm(formula = imputets.Bx ~ poly(New_tp1,3) + poly(month,3) + New_tp1:month
                   + New_tp1:poly(month,2), data = mydata.imputeTS.1))
summary(modelpoly.1)
#par(mfrow=c(2,2))
#plot(modelpoly.1)
#par(mfrow=c(1,1))
plot(mydata.imputeTS.1$imputets.Bx, col="red",type='l',ylim = c(-250,250))
lines(fitted(modelpoly.1),col="blue",type="l")
acf(resid(modelpoly.1),plot = T)
pacf(resid(modelpoly.1),plot=T)


modelpoly.2 <- (lm(formula = imputets.Bx ~ year:month + month:orbit +
                     year:orbit + year:month:orbit + poly(year,degree = 3) + poly(x = month,degree = 3)+
                     poly(orbit,degree = 3) , data = mydata.imputeTS.1))
summary(modelpoly.2)
#par(mfrow=c(2,2))
par(mfrow=c(2,2))
plot(modelpoly.2)
par(mfrow=c(1,1))
#par(mfrow=c(1,1))
plot(mydata.imputeTS.1$imputets.Bx, col="red",type='l',ylim = c(-250,250))
lines(fitted(modelpoly.2),col="blue",type="l")
acf(resid(modelpoly.2),plot = T)
pacf(resid(modelpoly.2),plot=T)


############### Harmonic Regression

period <- mean(as.numeric(Dataf1$`Number of Orbits per month`),na.rm = T)
period
summary(lm(formula = imputets.Bx ~ month + sin(((2*2*pi)*New_tp1)/period) 
           + cos(((2*2*pi)*New_tp1)/period) + month*sin(((2*pi)*New_tp1)/period)
           + month*cos(((2*pi)*New_tp1)/period), data = mydata.imputeTS.1))

summary(lm(formula = imputets.Bx ~ New_tp1 + cos(((2*pi)*New_tp1)/12) + month + 
             month*cos(((2*pi)*New_tp1)/12), data = mydata.imputeTS.1))

modelharm1 <- (lm(formula = imputets.Bx ~ New_tp1 + month + sin(((2*pi)*New_tp1)/period) 
                  + cos(((2*pi)*New_tp1)/period) + sin(((2*2*pi)*New_tp1)/period) 
                  + cos(((2*2*pi)*New_tp1)/period) + sin(((2*3*pi)*New_tp1)/period) 
                  + cos(((2*3*pi)*New_tp1)/period) + month*sin(((2*pi)*New_tp1)/period)
                  + month*cos(((2*pi)*New_tp1)/period) + month*sin(((2*2*pi)*New_tp1)/period)
                  + month*cos(((2*2*pi)*New_tp1)/period) + month*sin(((2*3*pi)*New_tp1)/period)
                  + month*cos(((2*3*pi)*New_tp1)/period), data = mydata.imputeTS.1))

summary(modelharm1)
acf(modelharm1$residuals)
pacf(modelharm1$residuals)

########### Generalized Least Squares

# For variable Bx

modeltsgls.1 <- (gls(model = imputets.Bx ~ month + New_tp1 + month:New_tp1, data = mydata.imputeTS.1,
                     correlation = corARMA(form = ~ orbit | year,p = 1) ))
summary(modeltsgls.1)
par(mfrow=c(2,2))
plot(modeltsgls.1)
par(mfrow=c(1,1))
plot(mydata.imputeTS.1$imputets.Bx, col="red",type='l',ylim = c(-250,250))
lines(fitted(modeltsgls.1),col="blue",type="l")
acf(resid(modeltsgls.1),plot = T)
pacf(resid(modeltsgls.1),plot=T)

# GLS Model
plot(gls(model = imputets.Bx ~ month + New_tp1, data = mydata.imputeTS.1,
         correlation = corARMA(form = ~ orbit | year,p = 1) ))




################ Basis Splines Regression

acf(resid(lm(formula = imputets.Bx ~ bs(New_tp1,df = 100,degree = 4) + bs(month,df = 75), data = mydata.imputeTS.1)),demean = F, type="partial")
pacf(resid(lm(formula = imputets.Bx ~ bs(New_tp1,df = 50) + bs(month,df = 12), data = mydata.imputeTS.1)))

acf(resid(lm(formula = imputets.Bx ~ bs(New_tp1,df = 100,degree = 3) + bs(month,df = 75,degree = 3), data = mydata.imputeTS.1)),demean = F, type="partial")
pacf(resid(lm(formula = imputets.Bx ~ bs(New_tp1,df = 50,degree = 3) + bs(month,df = 12,degree = 3), data = mydata.imputeTS.1)))
plot(mydata.imputeTS.1$imputets.Bx)
lines(fitted(lm(formula = imputets.Bx ~ bs(New_tp1,df = 50,degree = 3) + bs(month,df = 12,degree = 3), data = mydata.imputeTS.1)),col="red")

summary(lm(formula = imputets.Bx ~ bs(New_tp1,df = 40,degree = 3) + bs(month,df = 40,degree=3), 
           data = mydata.imputeTS.1))
plot(mydata.imputeTS.1$imputets.Bx)
lines(fitted(lm(formula = imputets.Bx ~ bs(New_tp1,df = 40,degree = 3) + bs(month,df = 40,degree=3), 
                data = mydata.imputeTS.1)),col="red")
acf(resid(lm(formula = imputets.Bx ~ bs(New_tp1,df = 40,degree = 3) + bs(month,df = 40,degree=3), 
             data = mydata.imputeTS.1)))
pacf(resid(lm(formula = imputets.Bx ~ bs(New_tp1,df = 40,degree = 3) + bs(month,df = 40,degree=3), 
              data = mydata.imputeTS.1)))
acf(x = na.omit(resid(arma(x = resid(lm(formula = imputets.Bx ~ bs(New_tp1,df = 40,degree = 3) + bs(month,df = 40,degree=3), 
                                        data = mydata.imputeTS.1)),order = c(1,0,0)))))
pacf(x = na.omit(resid(arma(x = resid(lm(formula = imputets.Bx ~ bs(New_tp1,df = 40,degree = 3) + bs(month,df = 40,degree=3), 
                                         data = mydata.imputeTS.1)),order = c(1,0,0)))))
Box.test(x = na.omit(resid(arma(x = resid(lm(formula = imputets.Bx ~ bs(New_tp1,df = 40,degree = 3) + bs(month,df = 40,degree=3), 
                                             data = mydata.imputeTS.1)),order = c(1,0,0)))),type = "Ljung-Box")

Box.test(x = na.omit(resid(arma(x = resid(lm(formula = imputets.Bx ~ bs(New_tp1,df = 40,degree = 3) + bs(month,df = 40,degree=3), 
                                             data = mydata.imputeTS.1)),order = c(1,0,0)))),type = "Box-Pierce")

adf.test(x = na.omit(resid(arma(x = resid(lm(formula = imputets.Bx ~ bs(New_tp1,df = 40,degree = 3) + bs(month,df = 40,degree=3), 
                                             data = mydata.imputeTS.1)),order = c(1,0,0)))),alternative = "stationary",k = 1)

########### Natural cubic splines

summary(lm(formula = imputets.Bx ~ ns(New_tp1,df = 20) + ns(month,df = 11), 
           data = mydata.imputeTS.1))
plot(mydata.imputeTS.1$imputets.Bx)
lines(fitted(lm(formula = imputets.Bx ~ ns(New_tp1,df = 30) + ns(month,df = 11), 
                data = mydata.imputeTS.1)),col="red")
acf(resid(lm(formula = imputets.Bx ~ ns(New_tp1,df = 30) + ns(month,df = 11), 
             data = mydata.imputeTS.1)))
pacf(resid(lm(formula = imputets.Bx ~ ns(New_tp1,df = 30) + ns(month,df = 11), 
              data = mydata.imputeTS.1)))
acf(x = na.omit(resid(arma(x = resid(lm(formula = imputets.Bx ~ ns(New_tp1,df = 30) + ns(month,df = 11), 
                                        data = mydata.imputeTS.1)),order = c(1,0,0)))))
pacf(x = na.omit(resid(arma(x = resid(lm(formula = imputets.Bx ~ ns(New_tp1,df = 30) + ns(month,df = 11), 
                                         data = mydata.imputeTS.1)),order = c(1,0,0)))))
Box.test(x = na.omit(resid(arma(x = resid(lm(formula = imputets.Bx ~ ns(New_tp1,df = 30) + ns(month,df = 11), 
                                             data = mydata.imputeTS.1)),order = c(1,0,0)))),type = "Ljung-Box")

Box.test(x = na.omit(resid(arma(x = resid(lm(formula = imputets.Bx ~ ns(New_tp1,df = 30) + ns(month,df = 11), 
                                             data = mydata.imputeTS.1)),order = c(1,0,0)))),type = "Box-Pierce")

adf.test(x = na.omit(resid(arma(x = resid(lm(formula = imputets.Bx ~ ns(New_tp1,df = 30) + ns(month,df = 11), 
                                             data = mydata.imputeTS.1)),order = c(1,0,0)))),alternative = "stationary",k = 1)


#### Function to check whether the autocorrelation plots are within limits or not
checkAcf <- function(x){
  
  ifelse(test = all((x[-1]<=1.96/sqrt(nrow(mydata.imputeTS.1)) & x[-1]>=(-1.96/sqrt(nrow(mydata.imputeTS.1))))),
         yes = return(TRUE),no = return(FALSE))
  
}


############# Final Univariate Regression Models

#### Steps taken to arrive at FInal GAM Models

# # GAM Models
# 
# for ( i in 3:5){
#   modelgam <- gam(formula = imputets.By ~ s(year,k = i) + s(month,k = i) + s(orbit,k = i),
#                   data = mydata.imputeTS.1,method = "REML")
#   print(summary(modelgam))
#   # print(gam.check(modelgam))
# }
# 
# 
# #"cr","cs","cc"
# 
# for (b in c("cc")){
#   for ( i in 4:12){
#     modelgam <- gam(formula = imputets.By ~ s(month,k = i,bs = b) + s(New_tp1,k = i,bs = b) + 
#                       s(New_tp1,month,k = i),
#                     data = mydata.imputeTS.1)
#     #gam.check(modelgam)
#     #print(summary(modelgam))
#     #print(gam.check(modelgam))
#     print(paste("BS =",b, "K = ", i, "AIC =", modelgam$aic, "DEviance = ", summary(modelgam)$dev.expl, 
#                 "R2adj = ", summary(modelgam)$r.sq))
#   }
# }
# 
# anova(gam(formula = imputets.By ~ s(New_tp1),data = mydata.imputeTS.1),
#       gam(formula = imputets.By ~ s(month), data = mydata.imputeTS.1),
#       gam(formula = imputets.By ~ s(New_tp1) + s(month), data = mydata.imputeTS.1),
#       gam(formula = imputets.By ~ s(New_tp1) + s(month) + s(New_tp1,month),
#           data = mydata.imputeTS.1),
#       gam(formula = imputets.By ~ s(New_tp1,bs="cc"),data = mydata.imputeTS.1),
#       gam(formula = imputets.By ~ s(month,bs="cc"), data = mydata.imputeTS.1),
#       gam(formula = imputets.By ~ s(New_tp1,bs="cc") + s(month,bs="cc"), data = mydata.imputeTS.1),
#       gam(formula = imputets.By ~ s(New_tp1,bs="cc") + s(month,bs="cc") + s(New_tp1,month),
#           data = mydata.imputeTS.1),
#       gam(formula = imputets.By ~ s(New_tp1,bs="cc", k = 12),data = mydata.imputeTS.1),
#       gam(formula = imputets.By ~ s(month,bs="cc", k = 12), data = mydata.imputeTS.1),
#       gam(formula = imputets.By ~ s(New_tp1,bs="cc", k = 12) + s(month,bs="cc", k = 12), data = mydata.imputeTS.1),
#       gam(formula = imputets.By ~ s(New_tp1,bs="cc", k = 12) + s(month,bs="cc", k = 12) + s(New_tp1,month),
#           data = mydata.imputeTS.1),
#       gam(formula = imputets.By ~ s(New_tp1,bs="cc", k = 12) + s(month,bs="cc", k = 12) + te(New_tp1,month),
#           data = mydata.imputeTS.1),
#       test = "F")
# 
# # Comparing models
# 
# anova(gam(formula = imputets.By ~ 1,
#           data = mydata.imputeTS.1,method = "REML"),
#       gam(formula = imputets.By ~ month + s(New_tp1,k = 10),
#           data = mydata.imputeTS.1,method = "REML"),
#       gam(formula = imputets.By ~ s(month,k = 12,bs="cc") + New_tp1,
#           data = mydata.imputeTS.1,method = "REML"),
#       gam(formula = imputets.By ~ s(month,k = 12,bs="cc") + s(New_tp1,k = 10),
#           data = mydata.imputeTS.1,method = "REML"),
#       gam(formula = imputets.By ~ s(month,k = 12,bs="cc") + s(New_tp1,k = 10) + 
#             te(New_tp1,month,k = c(10,12)),
#           data = mydata.imputeTS.1,method = "REML"),test = "Chisq")
# 
# coef(modelgamBy)
# coef((arima(x = modelgamBy$residuals, order = c(1,0,0))))
# mean(resid(arima(x = modelgamBy$residuals, order = c(1,0,0))))




########## Final GAM Models

##### Response Bx : GAM model v1

modelgamBx <- gam(formula = imputets.Bx ~ s(month,k = 12,bs="cc") + s(New_tp1,k = 10) + 
                    te(New_tp1,month,k = c(10,12)),
                  data = mydata.imputeTS.1,method = "REML")
summary(modelgamBx)
plot(mydata.imputeTS.1$imputets.Bx)
lines(fitted.values(modelgamBx),col="red",lwd=2)
gam.check(modelgamBx)
plot(modelgamBx, pages=1, residuals = TRUE, cex=3,scheme=1)
plot(modelgamBx, pages=1, residuals = FALSE, cex=3,scheme=1)
acf(modelgamBx$residuals)
pacf(modelgamBx$residuals)
vis.gam(modelgamBx,color = "heat", theta=50, phi=40)
plot(y = modelgamBx$residuals,x = modelgamBx$fitted.values)
Box.test(x = resid(modelgamBx),type = "Ljung-Box") # Significant
Box.test(x = resid(modelgamBx), type = "Box-Pierce") # Significant
tseries::adf.test(x = resid(modelgamBx),alternative = "stationary")
shapiro.test(x = resid(modelgamBx,type = "pearson" ))
tseries::kpss.test(x = resid(modelgamBx),null = "Trend",lshort = F) #lshort T or F same result
tseries::kpss.test(x = resid(modelgamBx),null = "Level",lshort = T) #lshort T or F same result
tseries::pp.test(x = resid(modelgamBx),alternative = "stationary",lshort = T,type = "Z(alpha)")

# Choosing K
rsd <- residuals(modelgamBx)
gam(rsd ~ s(month,k = 12,bs = "cc"), gamma=1.4,data = mydata.imputeTS.1) # fine
gam(rsd ~ s(New_tp1,k = 10,bs = "cc"), gamma=1.4,data = mydata.imputeTS.1) # not fine
gam.check(gam(rsd ~ s(New_tp1,k = 60,bs="cp"), gamma=2,data = mydata.imputeTS.1,method = "REML")) # not fine
(gam(rsd ~ s(New_tp1,month,k = 100, bs="ts"), gamma=2,data = mydata.imputeTS.1)) # not fine
gam(rsd ~ te(New_tp1,month,k = c(10,12),bs = c("cc","cc")), gamma=1.4,data = mydata.imputeTS.1)


#########  Fitting AutoRegressive AR(p=1) on the residuals of GAM Model of Response Bx

Box.test(x = arima(x = modelgamBx$residuals, order = c(1,0,0))$residuals,type = "Ljung-Box") # Insignificant
Box.test(x = arima(x = modelgamBx$residuals, order = c(1,0,0))$residuals, type = "Box-Pierce") # Insignificant
tseries::adf.test(x = arima(x = modelgamBx$residuals, order = c(1,0,0))$residuals ,alternative = "stationary",k = 1)
shapiro.test(x = resid(arima(x = modelgamBx$residuals, order = c(1,0,0))))
tseries::kpss.test(x = resid(arima(x = modelgamBx$residuals, order = c(1,0,0))),null = "Trend",lshort = T) #lshort T or F same result
tseries::kpss.test(x = resid(arima(x = modelgamBx$residuals, order = c(1,0,0))),null = "Level",lshort = T) #lshort T or F same result
tseries::pp.test(x = resid(arima(x = modelgamBx$residuals, order = c(1,0,0))),alternative = "stationary",lshort = T,type = "Z(alpha)")
plot(resid(arima(x = modelgamBx$residuals, order = c(1,0,0))))
qqnorm(resid(arima(x = modelgamBy$residuals, order = c(1,0,0))))
qqline(resid(arima(x = modelgamBy$residuals, order = c(1,0,0))))

modelgamBx.res <- resid(arima(x = modelgamBx$residuals, order = c(1,0,0))) # Storing the residuals of ARMA of GAM Residuals





##### Response By : GAM model v1
modelgamBy <- gam(formula = imputets.By ~ s(month,k = 12,bs="cc") + s(New_tp1,k = 10) + 
                    te(New_tp1,month,k = c(10,12)),
                  data = mydata.imputeTS.1,method = "REML")
plot(mydata.imputeTS.1$imputets.By)
lines(fitted(modelgamBy),col="red",lwd=2)
summary(modelgamBy)
gam.check(modelgamBy)
plot(modelgamBy, pages=1, residuals = TRUE, cex=3, scheme=1)
plot(modelgamBy, pages=1, residuals = FALSE, cex=3, scheme=1)
acf(modelgamBy$residuals)
pacf(modelgamBy$residuals)
vis.gam(modelgamBy,color = "heat", theta=50, phi=40)
plot(y = modelgamBy$residuals,x = modelgamBy$fitted.values)
Box.test(x = resid(modelgamBy),type = "Ljung-Box")
Box.test(x = resid(modelgamBy), type = "Box-Pierce")
tseries::adf.test(x = resid(modelgamBy),alternative = "stationary")
shapiro.test(x = resid(modelgamBy,type = "pearson" ))
tseries::kpss.test(x = resid(modelgamBy),null = "Trend",lshort = F) #lshort T or F same result
tseries::kpss.test(x = resid(modelgamBy),null = "Level",lshort = T) #lshort T or F same result
tseries::pp.test(x = resid(modelgamBy),alternative = "stationary",lshort = T,type = "Z(alpha)")

### Choosing K
rsd <- residuals(modelgamBy)
gam(rsd ~ s(month,k = 12,bs = "cc"), gamma=1.4,data = mydata.imputeTS.1) # fine
(gam(rsd ~ s(New_tp1,k = 50,bs = "ts"), gamma=1.4,data = mydata.imputeTS.1)) # not fine
(gam(rsd ~ s(New_tp1,month,k = 100, bs="ts"), gamma=1.4,data = mydata.imputeTS.1)) # not fine


#########  Fitting AutoRegressive AR(p=1) on the residuals of GAM Model of Response By

Box.test(x = arima(x = modelgamBy$residuals, order = c(1,0,0))$residuals,type = "Ljung-Box")
Box.test(x = arima(x = modelgamBy$residuals, order = c(1,0,0))$residuals, type = "Box-Pierce")
tseries::adf.test(x = arima(x = modelgamBy$residuals, order = c(1,0,0))$residuals ,alternative = "stationary",k = 1)
acf(x = resid(arima(x = modelgamBy$residuals, order = c(1,0,0))))
pacf(x = resid(arima(x = modelgamBy$residuals, order = c(1,0,0))))
qqnorm(resid(arima(x = modelgamBy$residuals, order = c(1,0,0))))
qqline(resid(arima(x = modelgamBy$residuals, order = c(1,0,0))))
plot(resid(arima(x = modelgamBy$residuals, order = c(1,0,0))))
modelgamBy.res <- resid(arima(x = modelgamBy$residuals, order = c(1,0,0))) # Storing the residuals of ARMA of GAM Residuals
shapiro.test(x = resid(arima(x = modelgamBx$residuals, order = c(1,0,0))))
tseries::kpss.test(x = resid(arima(x = modelgamBx$residuals, order = c(1,0,0))),null = "Trend",lshort = T) #lshort T or F same result
tseries::kpss.test(x = resid(arima(x = modelgamBx$residuals, order = c(1,0,0))),null = "Level",lshort = T) #lshort T or F same result
tseries::pp.test(x = resid(arima(x = modelgamBx$residuals, order = c(1,0,0))),alternative = "stationary",lshort = T,type = "Z(alpha)")


##### Response Bz : GAM model v1

modelgamBz <- gam(formula = imputets.Bz ~ s(month,k = 12,bs="cc") + s(New_tp1,k = 10) + 
                    te(New_tp1,month,k = c(10,12)),
                  data = mydata.imputeTS.1,method = "REML")
summary(modelgamBz)
plot(mydata.imputeTS.1$imputets.Bz)
lines(fitted(modelgamBz),col="red")
gam.check(modelgamBz)
plot(modelgamBz, pages=1, residuals = TRUE, cex=3, scheme = 1)
plot(modelgamBz, pages=1, residuals = FALSE, cex=3, scheme = 1)
acf(modelgamBz$residuals) # Non stationary
pacf(modelgamBz$residuals) # Non-stationary
vis.gam(modelgamBz,color = "heat", theta=50, phi=40)
plot(y = modelgamBz$residuals,x = modelgamBz$fitted.values)
Box.test(x = resid(modelgamBz),type = "Ljung-Box") # Insignificant
Box.test(x = resid(modelgamBz), type = "Box-Pierce") # Insignificant
tseries::adf.test(x = resid(modelgamBz),alternative = "stationary")
shapiro.test(x = resid(modelgamBz,type = "pearson" ))
tseries::kpss.test(x = resid(modelgamBz),null = "Trend",lshort = F) #lshort T or F same result
tseries::kpss.test(x = resid(modelgamBz),null = "Level",lshort = T) #lshort T or F same result
tseries::pp.test(x = resid(modelgamBz),alternative = "stationary",lshort = T,type = "Z(alpha)")
qqnorm(resid(modelgamBz))
qqline(resid(modelgamBz))

####Choosing K
rsd <- residuals(modelgamBz)
gam(rsd ~ s(month,k = 12,bs = "cc"), gamma=1.4,data = mydata.imputeTS.1, method="REML") # fine
gam(rsd ~ s(New_tp1,k = 80), gamma=1.4,data = mydata.imputeTS.1,method="REML") # not fine
#gam(rsd ~ te(New_tp1,month,k = c(80,12)), gamma=1.4,data = mydata.imputeTS.1) # not fine


########  Fitting AutoRegressive AR(p=1) on the residuals of GAM Model of Response Bz
Box.test(x = arima(x = modelgamBz$residuals, order = c(1,0,0))$residuals,type = "Ljung-Box") # Insignificant
Box.test(x = arima(x = modelgamBz$residuals, order = c(1,0,0))$residuals, type = "Box-Pierce") # Insignificant
tseries::adf.test(x = arima(x = modelgamBz$residuals, order = c(1,0,0))$residuals ,alternative = "stationary",k = 1)
shapiro.test(x = resid(arima(x = modelgamBz$residuals, order = c(1,0,0))))
tseries::kpss.test(x = resid(arima(x = modelgamBz$residuals, order = c(1,0,0))),null = "Trend",lshort = T) #lshort T or F same result
tseries::kpss.test(x = resid(arima(x = modelgamBz$residuals, order = c(1,0,0))),null = "Level",lshort = T) #lshort T or F same result
tseries::pp.test(x = resid(arima(x = modelgamBz$residuals, order = c(1,0,0))),alternative = "stationary",lshort = T,type = "Z(alpha)")
plot(resid(arima(x = modelgamBz$residuals, order = c(1,0,0))))
acf(resid(arima(x = modelgamBz$residuals, order = c(1,0,0))))
pacf(resid(arima(x = modelgamBz$residuals, order = c(1,0,0))))
qqnorm(resid(arima(x = modelgamBy$residuals, order = c(1,0,0))))
qqline(resid(arima(x = modelgamBy$residuals, order = c(1,0,0))))

modelgamBz.res <- resid(arima(x = modelgamBz$residuals, order = c(1,0,0))) # Storing the residuals of ARMA of GAM Residuals


#### acf and pacf plots of Final gam Models

# par(mfrow=c(4,3))
# par(mar=c(5.1,4.1,4.1,2.1))
# acf(resid(arima(x = modelgamBx$residuals, order = c(1,0,0))))
# acf(resid(arima(x = modelgamBy$residuals, order = c(1,0,0))))
# acf(resid(arima(x = modelgamBz$residuals, order = c(1,0,0))))
# acf(resid(arima(x = modelgamBx.1$residuals, order = c(1,0,0))))
# acf(resid(arima(x = modelgamBy.1$residuals, order = c(1,0,0))))
# acf(resid(arima(x = modelgamBz.1$residuals, order = c(1,0,0))))
# 
# pacf(resid(arima(x = modelgamBx$residuals, order = c(1,0,0))))
# pacf(resid(arima(x = modelgamBy$residuals, order = c(1,0,0))))
# pacf(resid(arima(x = modelgamBz$residuals, order = c(1,0,0))))
# pacf(resid(arima(x = modelgamBx.1$residuals, order = c(1,0,0))))
# pacf(resid(arima(x = modelgamBy.1$residuals, order = c(1,0,0))))
# pacf(resid(arima(x = modelgamBz.1$residuals, order = c(1,0,0))))
# 
# par(mfrow=c(1,1))

# Residuals vs Fitted plots of GAM

par(mfrow=c(2,3))
plot(y = modelgamBx$residuals,x = modelgamBz.1$fitted.values)
plot(y = modelgamBy$residuals,x = modelgamBz.1$fitted.values)
plot(y = modelgamBz$residuals,x = modelgamBz.1$fitted.values)
plot(y = modelgamBx.1$residuals,x = modelgamBz.1$fitted.values)
plot(y = modelgamBy.1$residuals,x = modelgamBz.1$fitted.values)
plot(y = modelgamBz.1$residuals,x = modelgamBz.1$fitted.values)
par(mfrow=c(1,1))

# Plots during report

par(mfrow=c(3,1))
#par(mar = c(1.5,1.5,1.5,1.5), oma=c(4,4,0.5,0.5))

plot(mydata.imputeTS.1$imputets.Bx, main="GAM Model Variable Bx",pch=21,col="red")
lines(fitted(modelgamBx),col="black",lwd=2)
legend("topleft",legend = c("Original","Fitted"),lty = 1,col=c("red","black"),cex=0.6)

#plot(pacf(modelgamBx.res,plot=F),main="PACF plot for Bx")

plot(mydata.imputeTS.1$imputets.By, main="GAM Model Variable By",pch=21,col="seagreen")
lines(fitted(modelgamBy),col="black",lwd=2)
legend("topleft",legend = c("Original","Fitted"),lty = 1,col=c("seagreen","black"),cex=0.6)

#plot(pacf(modelgamBy.res,plot=F),main="PACF plot for By")

plot(mydata.imputeTS.1$imputets.Bz, main="GAM Model Variable Bz",pch=21,col="deepskyblue")
lines(fitted(modelgamBz),col="black",lwd=2)
legend("topleft",legend = c("Original","Fitted"),lty = 1,col=c("deepskyblue","black"),cex=0.6)

#plot(pacf(modelgamBz.res,plot=F),main="PACF plot for Bz")

par(mfrow=c(1,1))

par(mfrow=c(3,2))
plot(y = modelgamBx.res,x = fitted(modelgamBx),main ="Fitted Bx vs AR(1)Residuals")
qqnorm(modelgamBx.res)
qqline(modelgamBx.res)
plot(y = modelgamBy.res,x = fitted(modelgamBy),main ="Fitted By vs AR(1)Residuals")
qqnorm(modelgamBy.res)
qqline(modelgamBy.res)
plot(y = modelgamBz.res,x = fitted(modelgamBz),main ="Fitted Bz vs AR(1)Residuals")
qqnorm(modelgamBz.res)
qqline(modelgamBz.res)
par(mfrow=c(1,1))


plot(acf(modelgamBx.res,plot=F),main="ACF plot of Ar(1) res")
plot(acf(modelgamBy.res,plot=F),main="ACF plot of AR(1)Residuals")
plot(acf(modelgamBz.res,plot=F),main="ACF plot of AR(1)Residuals")


### Relation of variation among univariate regression models
par(mar=c(5.1, 4.1, 4.1, 8.1),xpd=TRUE)
plot(x = mydata.imputeTS.1$New_tp1, y = fitted(modelgamBx), type='l', xaxt='n',
     xlab="Year",ylab="Fitted GAM Models",main="Variation of Bx, By and Bz",col="red",lwd=2)
axis(side = 1,at = c(2003,2004,2005,2006,2007,2008,2009,2010,2011,2012,2013))
lines(x = mydata.imputeTS.1$New_tp1, y = fitted(modelgamBy),col="darkgreen",lwd=2)
lines(x = mydata.imputeTS.1$New_tp1, y = fitted(modelgamBz), col="blue",lwd=2)
legend("topright", inset=c(-0.5,-0.1), 
       legend = c("Bx","By","Bz"), 
       lty = 1, lwd=2,title="Response Variable\nof GAM Model", cex=0.7, 
       col=c("red","darkgreen","blue"))
par(xpd=FALSE)



### Standard Analysis Chapter #################
########################################################
## Second Primary Question of Interest
########################################################

#### Creating and exploring the Storm Data set

mydata.storm1 <- mydata.storm # Creating a copy of the original storm data set
str(mydata.storm1)
mydata.storm1$storm.days1 <- with(mydata.storm1, storm.days1<-lubridate::dmy(as.character(mydata.storm1$storm.days)))
class(mydata.storm1$storm.days1)
mydata.storm1$year <- with(mydata.storm1, year <- (lubridate::year(mydata.storm1$storm.days1)))
mydata.storm1$month <- with(mydata.storm1, month <- (data.table::month(mydata.storm1$storm.days1)))
mydata.storm1$day <- with(mydata.storm1, day <- lubridate::day(mydata.storm1$storm.days1))
str(mydata.storm1)
head(mydata.storm1)


###### Series of Functions and Codes to include Date in the original Data set.
### As per the "Cluster Mission Website", it takes a Satellite to complete one revolution
### of Orbit in 57 hours or in 2.375 or 2.4 days. Based on this fact, corresponding dates
## are included to each orbit with the assumption that the first record started from 
## 1st January 2003.

row <- c()
for ( i in seq(1:nrow(Dataf1))){
  #cat(i,"\n")
  if (Dataf1[i,4]==""){
    cat(i,"\n")
    row <- c(row,i)
  }
}
row

str(Dataf1)
Dataf2 <- Dataf1[-row,]
str(Dataf2)
head(Dataf2,n=100)
View(Dataf2)

checkLeapYear <- function(x){
  for ( i in 1:length(x)){
    if (!(x[i]%%4 == 0)) {
      return(FALSE)
    }
    else if (!(x[i]%%100 == 0)) {
      return(TRUE)
    }
    else if (!(x[i]%%400 == 0)) {
      return(FALSE)
    }
    else {return(TRUE)}
  }
}

lapply(unique(mydata.imputeTS.1$year), checkLeapYear)
# 2004, 2008, 2012 Leap Years

daymon <- 0
Dates <- c()
unmatched <- c()
for (i in unique(Dataf2$Year)){
  for (j in unique(Dataf2$Month)){
    if (j %in% c(1,3,5,7,8,10,12)){
      daymon <- 31 
    }
    else if (j %in% c(4,6,9,11)){
      daymon <- 30
    }
    else if (j == 2 & checkLeapYear(i)){
      daymon <- 29
    }
    else {daymon <- 28}
    # cat(i,j,Dataf2$`Number of Orbits per month`[Dataf2$Year==i & Dataf2$Month==j],
    #       length(seq(from = as.Date(paste(i,j,"01",sep = "-")), 
    #                  to = as.Date(paste(i,j,daymon,sep = "-")), 
    #                  by = 2.4)),"\n")    
    if ((i==2006 & j==12) | (i==2011 & j==10) |
        (i==2012 & j==3) | (i==2012 & j==7) | (i==2013 & j==6) | 
        (i==2003 & (j==5 | j==8 | j==10 | j==11)) | (i==2004 & (j==1 | j==7 | j==8 | j==11)) |
        (i==2005 & (j==1 | j==5 | j==6 | j==8 | j==9))){
      Dates <- c(Dates, seq(from = as.Date(paste(i,j,"01",sep = "-")), 
                            to = as.Date(paste(i,j,daymon,sep = "-")), 
                            by = 2.375))
      
      if (i==2011 & j==10){
        Dates <- c(Dates,as.Date("2011-10-31"))
      }
      else if (i==2004 & j==11){
        #Dates[year(as.Date(Dates))==i & month(as.Date(Dates))==j] <- as.Date(Dates[year(as.Date((Dates)))==i & month(as.Date((Dates)))==j])[-13]
        Dates <- Dates[-297]
      }
      else if (i == 2005 & j==6){
        #Dates[year(as.Date(Dates))==i & month(as.Date(Dates))==j] <- as.Date(Dates[year(as.Date((Dates)))==i & month(as.Date((Dates)))==j])[-13]
        #Dates <- Dates[-388]
        Dates <- Dates[-387]
      }
      else if (i == 2012 & j==7){
        #Dates[year(as.Date((Dates)))==i & month(as.Date(Dates))==j] <- as.Date(Dates[year(as.Date((Dates)))==i & month(as.Date((Dates)))==j])[-c(9:13)]
        Dates <- Dates[-c(1444, 1445, 1446, 1447)]
        #      Dates <- Dates[-1445]
      }
      cat(i,j,Dataf2$`Number of Orbits per month`[Dataf2$Year==i & Dataf2$Month==j],
          length(which(year(as.Date(Dates))==i & month(as.Date(Dates))==j)),"\n")
      
      # length(seq(from = as.Date(paste(i,j,"01",sep = "-")), 
      #            to = as.Date(paste(i,j,daymon,sep = "-")), 
      #            by = 2.375)),"\n")
      
      if (as.numeric(Dataf2$`Number of Orbits per month`[Dataf2$Year==i & Dataf2$Month==j])!=
          length(which(year(as.Date(Dates))==i & month(as.Date(Dates))==j))){
        unmatched <- c(unmatched,c(i,j)) }
    }
    else if (!(i==2007 & j==9) & !(i==2007 & j==11) & (i %in% c(2003,2004,2005,2006,2007,2008,2009,2010,2011,2012,2013))
             & !(i==2006 & j==12) & !(i==2011 & j==10) &
             !(i==2012 & j==3) & !(i==2012 & j==7) & !(i==2013 & j==6) &
             !(i==2003 & (j==5 | j==8 | j==10 | j==11)) & !(i==2004 & (j==1 | j==7 | j==8 | j==11)) &
             !(i==2005 & (j==1 | j==5 | j==6 | j==8 | j==9))){
      Dates <- c(Dates, seq(from = as.Date(paste(i,j,"01",sep = "-")), 
                            to = as.Date(paste(i,j,daymon,sep = "-")), 
                            length.out = as.numeric(Dataf2$`Number of Orbits per month`[Dataf2$Year==i & Dataf2$Month==j])))
      
      cat(i,j,Dataf2$`Number of Orbits per month`[Dataf2$Year==i & Dataf2$Month==j],
          length(seq(from = as.Date(paste(i,j,"01",sep = "-")), 
                     to = as.Date(paste(i,j,daymon,sep = "-")), 
                     length.out = as.numeric(Dataf2$`Number of Orbits per month`[Dataf2$Year==i & Dataf2$Month==j]))),"\n")  
    }
  }
}
as.Date(Dates)
tail(as.Date(Dates),n=400)
unmatched
Dates <- Dates[-1444]
length(as.Date(Dates))-nrow(mydata.imputeTS.1)
#"2004" "11"   "2005" "6"    "2011" "10"   "2012" "7" 

which(year(as.Date((Dates)))==2004 & month(as.Date((Dates)))==11)
length(which(year(as.Date((Dates)))==2004 & month(as.Date((Dates)))==11)) #13
as.numeric(Dataf2$`Number of Orbits per month`[Dataf2$Year==2004 & Dataf2$Month==11]) #12

which(year(as.Date((Dates)))==2005 & month(as.Date((Dates)))==6)
length(which(year(as.Date((Dates)))==2005 & month(as.Date((Dates)))==6)) #13
as.numeric(Dataf2$`Number of Orbits per month`[Dataf2$Year==2005 & Dataf2$Month==6]) #12

which(year(as.Date((Dates)))==2011 & month(as.Date((Dates)))==10)
length(which(year(as.Date((Dates)))==2011 & month(as.Date((Dates)))==10)) #13
as.numeric(Dataf2$`Number of Orbits per month`[Dataf2$Year==2011 & Dataf2$Month==10]) #14

which(year(as.Date((Dates)))==2012 & month(as.Date((Dates)))==7)
length(which(year(as.Date((Dates)))==2012 & month(as.Date((Dates)))==7)) #13
as.numeric(Dataf2$`Number of Orbits per month`[Dataf2$Year==2012 & Dataf2$Month==7]) #8

mydata.imputeTS.1$Dates <- with(mydata.imputeTS.1, Dates <- as.Date(Dates))
mydata.imputeTS.1$day <- with(mydata.imputeTS.1, day <- day(mydata.imputeTS.1$Dates))
View(mydata.imputeTS.1)
str(mydata.imputeTS.1)

all(mydata.imputeTS.1$year==year(mydata.imputeTS.1$Dates))
all(mydata.imputeTS.1$month==month(mydata.imputeTS.1$Dates))

mydata.imputeTS.1$Storm.Dates <- with(mydata.imputeTS.1, Storm.Dates <- rep(x = NA,nrow(mydata.imputeTS.1)))
str(mydata.imputeTS.1)

checkDates <- function(d1,d2){
  if (class(d1)!="Date" | class(d2)!="Date"){
    d1 <- as.Date(d1)
    d2 <- as.Date(d2)
  }
  if (day(d1)==day(d2) & month(d1)==month(d2) & year(d1)==year(d2)){ return(TRUE) }
  else {return(FALSE)}
}


for (i in 1:nrow(mydata.storm1)){
  cat("i = ",i,"\n")
  if (sum(mydata.storm1$storm.days1[i] %in% mydata.imputeTS.1$Dates)!=0){
    mydata.imputeTS.1$Storm.Dates[
      which(mydata.imputeTS.1$Dates %in% mydata.storm1$storm.days1[i])] <- mydata.storm1$storm.days1[i]
    cat("Using first part","\n")  
    cat("i = ",i,as.Date(mydata.imputeTS.1$Storm.Dates[which(mydata.imputeTS.1$Dates %in% mydata.storm1$storm.days1[i])]), 
        as.Date(mydata.storm1$storm.days1[i]),"\n")
  } else {
    cat("inside else i = ",i,"\n")
    for ( j in which(mydata.imputeTS.1$year==mydata.storm1$year[i] &
                     mydata.imputeTS.1$month==mydata.storm1$month[i])){
      cat("j = ",j,"\n")
      if (checkDates(mydata.imputeTS.1$Dates[j],mydata.storm1$storm.days1[i])){
        mydata.imputeTS.1$Storm.Dates[j] <- mydata.storm1$storm.days1[i]
        cat("Using Check Dates","\n")
        cat ("i = ",i," j = ",j,as.Date(mydata.imputeTS.1$Storm.Dates[j]),
             as.Date(mydata.storm1$storm.days1[i]),"\n")
        break
      }
      else if (between(x = mydata.storm1$storm.days1[i],left = mydata.imputeTS.1$Dates[j],
                       right = mydata.imputeTS.1$Dates[j+1])){
        if (abs(mydata.storm1$day[i]-mydata.imputeTS.1$day[j])<=1 & 
            mydata.storm1$month[i]==mydata.imputeTS.1$month[j] & 
            mydata.imputeTS.1$year[j]==mydata.storm1$year[i]){
          mydata.imputeTS.1$Storm.Dates[j] <- mydata.storm1$storm.days1[i]
          cat("Using first part of else-if","\n")
          cat ("i = ",i," j = ",j,as.Date(mydata.imputeTS.1$Storm.Dates[j]),
               as.Date(mydata.storm1$storm.days1[i]),"\n")
        } 
        else if (abs(mydata.imputeTS.1$day[j+1]-mydata.storm1$day[i])<=1 & 
                 mydata.storm1$month[i]==mydata.imputeTS.1$month[j+1] & 
                 mydata.imputeTS.1$year[j+1]==mydata.storm1$year[i]){
          mydata.imputeTS.1$Storm.Dates[j+1] <- mydata.storm1$storm.days1[i]  
          cat("Using second part of else-if","\n")
          cat ("i = ",i," j = ",j," j+1 = ",j+1,as.Date(mydata.imputeTS.1$Storm.Dates[j]),
               as.Date(mydata.storm1$storm.days1[i]),"\n")
        }
        
        
        break
        #cat("end of j =",j,"\n")
      }
    }
    # cat ("i = ",i," j = ",j,as.Date(mydata.imputeTS.1$Storm.Dates[j]),
    #      as.Date(mydata.storm1$storm.days1[i]),"\n")
    cat("end of i = ",i,"\n")
  }
}

#lapply(mydata.storm1$storm.days1, function(x) apply(mydata.imputeTS.1$Dates, MARGIN = c(1,2), function(y) y %in% x))  

mydata.imputeTS.1$Storm.Dates <- as.Date(mydata.imputeTS.1$Storm.Dates)
str(mydata.imputeTS.1)
sum(!is.na(as.Date(mydata.imputeTS.1$Storm.Dates)))
nrow(mydata.storm1)
mydata.imputeTS.1$Storm.Dates[which(!is.na(mydata.imputeTS.1$Storm.Dates))]
mydata.storm1$storm.days1

mydata.imputeTS.1$Dates[which(!is.na(mydata.imputeTS.1$Storm.Dates))]
which(mydata.imputeTS.1$Dates %in% mydata.storm1$storm.days1)
mydata.imputeTS.1$Dates[137]
mydata.imputeTS.1$Dates[1574]
which(mydata.storm1$storm.days1 %in% mydata.imputeTS.1$Dates)
mydata.storm1$storm.days1[7]
mydata.storm1$storm.days1[29]

# Manual Addition / Modification of Storm Dates

mydata.imputeTS.1$Storm.Dates[63] <- mydata.storm1$storm.days1[which(mydata.storm1$storm.days1=="2003-05-29")]
mydata.imputeTS.1$Storm.Dates[126] <- mydata.storm1$storm.days1[which(mydata.storm1$storm.days1=="2003-10-29")] 
mydata.imputeTS.1$Storm.Dates[127] <- mydata.storm1$storm.days1[which(mydata.storm1$storm.days1=="2003-10-30")]
mydata.imputeTS.1$Storm.Dates[128] <- mydata.storm1$storm.days1[which(mydata.storm1$storm.days1=="2003-10-31")]
mydata.imputeTS.1$Storm.Dates[137] <- mydata.storm1$storm.days1[which(mydata.storm1$storm.days1=="2003-11-20")]
mydata.imputeTS.1$Storm.Dates[138] <- mydata.storm1$storm.days1[which(mydata.storm1$storm.days1=="2003-11-21")]
mydata.imputeTS.1$Storm.Dates[257] <- mydata.storm1$storm.days1[which(mydata.storm1$storm.days1=="2004-08-30")]
mydata.imputeTS.1$Storm.Dates[258] <- mydata.storm1$storm.days1[which(mydata.storm1$storm.days1=="2004-08-31")]
mydata.imputeTS.1$Storm.Dates[288] <- mydata.storm1$storm.days1[which(mydata.storm1$storm.days1=="2004-11-08")]
mydata.imputeTS.1$Storm.Dates[289] <- mydata.storm1$storm.days1[which(mydata.storm1$storm.days1=="2004-11-09")]
mydata.imputeTS.1$Storm.Dates[290] <- mydata.storm1$storm.days1[which(mydata.storm1$storm.days1=="2004-11-10")]
mydata.imputeTS.1$Storm.Dates[412] <- mydata.storm1$storm.days1[which(mydata.storm1$storm.days1=="2005-08-31")]

# View the updated dataset and the structure of it.
head(mydata.imputeTS.1)
str(mydata.imputeTS.1)

#par("mar") # Knowing the default margins   # 5.1 4.1 4.1 2.1
#par("oma")  # Outer MArgin Area Defaults Blank Region # 0 0 0 0 
#par("usr")  # Inner Plotting Region Blank Region # 0 1 0 1



### Combining the three response variables in one single variable B

mydata.imputeTS.1$B <- with(mydata.imputeTS.1, 
                            B <- sqrt(mydata.imputeTS.1$imputets.Bx**2 + mydata.imputeTS.1$imputets.By**2 +
                                        mydata.imputeTS.1$imputets.Bz**2))
str(mydata.imputeTS.1)
plot(mydata.imputeTS.1$B)

##### Plotting the original Response Variables Bx, By , Bz and B (Overall Magnetic Field)
## and also indicate the actual geomagnetic storm locations.

### Variable Bx

par(mar=c(5.1, 4.1, 4.1, 8.1),xpd=TRUE)
plot(x = mydata.imputeTS.1$New_tp1, y = mydata.imputeTS.1$imputets.Bx, type='p', xaxt='n',
     xlab="Year",ylab="Value of Bx in nano-Tesla",main="Time-plot of variable Bx",col="red",pch=21)
axis(side = 1,at = c(2003,2004,2005,2006,2007,2008,2009,2010,2011,2012,2013))
#abline(v = mydata.imputeTS.1$New_tp1[which(!(is.na(mydata.imputeTS.1$Storm.Dates)))],col=1:33,xpd=FALSE)
segments(x0 = mydata.imputeTS.1$New_tp1[which(!(is.na(mydata.imputeTS.1$Storm.Dates)))], 
         y0= par("usr")[3], 
         x1 = mydata.imputeTS.1$New_tp1[which(!(is.na(mydata.imputeTS.1$Storm.Dates)))], 
         y1 = mydata.imputeTS.1$imputets.Bx[which(!(is.na(mydata.imputeTS.1$Storm.Dates)))], col = 1:29, lwd = 2)
# points(x = mydata.imputeTS.1$New_tp1[which(is.na(mydata3$Bx))], 
#        y = mydata.imputeTS.1$imputets.Bx[which(is.na(mydata.imputeTS.1$Bx))], col="red")
legend("topright", inset=c(-0.5,-0.3), 
       legend = mydata.imputeTS.1$Storm.Dates[which(!(is.na(mydata.imputeTS.1$Storm.Dates)))], 
       lty = 1, title="Storm Dates", col=1:29,cex=0.5)
#abline(a = 0,b = 1)
#locator()
par(xpd=FALSE)
par(mar=c(5.1, 4.1, 4.1, 2.1))

# Variable By

par(mar=c(5.1, 4.1, 4.1, 8.1),xpd=TRUE)
plot(x = mydata.imputeTS.1$New_tp1, y = mydata.imputeTS.1$imputets.By, type='p', xaxt='n',
     xlab="Year",ylab="Value of By in nano-Tesla",main="Time-plot of variable By",col="darkgreen",pch=21)
axis(side = 1,at = c(2003,2004,2005,2006,2007,2008,2009,2010,2011,2012,2013))
#abline(v = mydata.imputeTS.1$New_tp1[which(!(is.na(mydata.imputeTS.1$Storm.Dates)))])
segments(x0 = mydata.imputeTS.1$New_tp1[which(!(is.na(mydata.imputeTS.1$Storm.Dates)))], 
         y0= par("usr")[3], 
         x1 = mydata.imputeTS.1$New_tp1[which(!(is.na(mydata.imputeTS.1$Storm.Dates)))], 
         y1 = mydata.imputeTS.1$imputets.By[which(!(is.na(mydata.imputeTS.1$Storm.Dates)))], col = 1:29, lwd = 2)
# points(x = mydata.imputeTS.1$New_tp1[which(is.na(mydata3$By))], 
#        y = mydata.imputeTS.1$imputets.By[which(is.na(mydata.imputeTS.1$By))], col="red")
legend("topright", inset=c(-0.5,-0.3), 
       legend = mydata.imputeTS.1$Storm.Dates[which(!(is.na(mydata.imputeTS.1$Storm.Dates)))], 
       lty = 1, title="Storm Dates", col=1:29,cex=0.5,lwd=2)
par(xpd=FALSE)
par(mar=c(5.1, 4.1, 4.1, 2.1))

# Variable Bz

par(mar=c(5.1, 4.1, 4.1, 8.1),xpd=TRUE)
plot(x = mydata.imputeTS.1$New_tp1, y = mydata.imputeTS.1$imputets.Bz, type='p', xaxt='n',
     xlab="Year",ylab="Value of Bz in nano-Tesla",main="Time-plot of variable Bz",col="blue",pch=21)
axis(side = 1,at = c(2003,2004,2005,2006,2007,2008,2009,2010,2011,2012,2013))
segments(x0 = mydata.imputeTS.1$New_tp1[which(!(is.na(mydata.imputeTS.1$Storm.Dates)))], 
         y0= par("usr")[3], 
         x1 = mydata.imputeTS.1$New_tp1[which(!(is.na(mydata.imputeTS.1$Storm.Dates)))], 
         y1 = mydata.imputeTS.1$imputets.Bz[which(!(is.na(mydata.imputeTS.1$Storm.Dates)))], col = 1:29, lwd = 2)
legend("topright", inset=c(-0.5,-0.3), 
       legend = mydata.imputeTS.1$Storm.Dates[which(!(is.na(mydata.imputeTS.1$Storm.Dates)))], 
       lty = 1, title="Storm Dates", col=1:29,cex=0.5,lwd=2)
par(xpd=FALSE)
par(mar=c(5.1, 4.1, 4.1, 2.1))

# Variable B
par(mar=c(5.1, 4.1, 4.1, 8.1),xpd=TRUE)
plot(x = mydata.imputeTS.1$New_tp1, y = mydata.imputeTS.1$B, type='p', xaxt='n',
     xlab="Year",ylab="Value of B in nano-Tesla",main="Time-plot of variable B",col="darkmagenta",pch=21)
axis(side = 1,at = c(2003,2004,2005,2006,2007,2008,2009,2010,2011,2012,2013))
segments(x0 = mydata.imputeTS.1$New_tp1[which(!(is.na(mydata.imputeTS.1$Storm.Dates)))], 
         y0= par("usr")[3], 
         x1 = mydata.imputeTS.1$New_tp1[which(!(is.na(mydata.imputeTS.1$Storm.Dates)))], 
         y1 = mydata.imputeTS.1$B[which(!(is.na(mydata.imputeTS.1$Storm.Dates)))], col = 1:29, lwd = 2)
legend("topright", inset=c(-0.5,-0.3), 
       legend = mydata.imputeTS.1$Storm.Dates[which(!(is.na(mydata.imputeTS.1$Storm.Dates)))], 
       lty = 1, title="Storm Dates", col=1:29,cex=0.5,lwd=2)
par(xpd=FALSE)
par(mar=c(5.1, 4.1, 4.1, 2.1))


#### change point Modelling

# Using Changepoint package

### Applied on original Response Variable Bx and assummed it follows Gaussian Distribution.

# Variance Changepoints Year-wise
par(mfrow=c(3,4))
for (year in unique(mydata.imputeTS.1$year)){
  plot(cpt.var(data=mydata.imputeTS.1$imputets.Bx[mydata.imputeTS.1$year==year], 
               penalty = "SIC",  method = "PELT", test.stat="Normal", 
               class = T,param.estimates = T , minseglen=2),cpt.width=2,cpt.col="red")  
}
par(mfrow=c(1,1))

# Mean Changepoints Year-wise
par(mfrow=c(3,4))
for (year in unique(mydata.imputeTS.1$year)){
  plot(cpt.mean(data=mydata.imputeTS.1$imputets.Bx[mydata.imputeTS.1$year==year], 
                penalty = "SIC",  method = "PELT", test.stat="Normal", 
                class = T,param.estimates = T , minseglen=2),cpt.width=2,cpt.col="red")  
}
par(mfrow=c(1,1))


# Mean and Variance Changepoints Year-wise
par(mfrow=c(3,4))
for (year in unique(mydata.imputeTS.1$year)){
  plot(cpt.meanvar(data=mydata.imputeTS.1$imputets.Bx[mydata.imputeTS.1$year==year], 
                   penalty = "SIC",  method = "PELT", test.stat="Normal", 
                   class = T,param.estimates = T , minseglen=2),cpt.width=2,cpt.col="red",diagnostics=TRUE)  
}
par(mfrow=c(1,1))


### Yearwise Change points

# Original Data Bx assumming Non-Gaussian distribution making use of CUSUM Method
## and also matching with Actual Storm Dates Year-wise (for only those years in which 
## storm has occurred).

#Mean
par(mfrow=c(2,4))
for (year in unique(lubridate::year(mydata.storm1$storm.days1))){
  plot(cpt.mean(data=mydata.imputeTS.1$imputets.Bx[mydata.imputeTS.1$year==year], 
                penalty = "AIC",  method = "BinSeg", test.stat="CUSUM", Q = 5,
                class = T,param.estimates = T ),cpt.width=2,cpt.col="red",main=paste("Year: ",year))
  #axis(side = 1,at = c(2003,2004,2005,2006,2007,2008,2009,2010,2011,2012,2013))
  #abline(v = mydata.imputeTS.1$New_tp1[which(!(is.na(mydata.imputeTS.1$Storm.Dates)))])
  for (i in which(!(is.na(mydata.imputeTS.1$Storm.Dates))[mydata.imputeTS.1$year==year])){
    
    segments(x0 = i, 
             y0= par("usr")[3], 
             x1 = i, 
             y1 = mydata.imputeTS.1$imputets.Bx[mydata.imputeTS.1$year==year][i], lwd = 2, col="darkgreen")
    
  }
}

par(mfrow=c(1,1))


#Mean and Variance
par(mfrow=c(2,4))
for (year in unique(lubridate::year(mydata.storm1$storm.days1))){
  plot(cpt.meanvar(data=mydata.imputeTS.1$imputets.Bx[mydata.imputeTS.1$year==year], 
                   penalty = "AIC",  method = "BinSeg", test.stat="Normal", Q = 5,
                   class = T,param.estimates = T ),cpt.width=2,cpt.col="red",main=paste("Year: ",year))
  #axis(side = 1,at = c(2003,2004,2005,2006,2007,2008,2009,2010,2011,2012,2013))
  #abline(v = mydata.imputeTS.1$New_tp1[which(!(is.na(mydata.imputeTS.1$Storm.Dates)))])
  for (i in which(!(is.na(mydata.imputeTS.1$Storm.Dates))[mydata.imputeTS.1$year==year])){
    
    segments(x0 = i, 
             y0= par("usr")[3], 
             x1 = i, 
             y1 = mydata.imputeTS.1$imputets.Bx[mydata.imputeTS.1$year==year][i], lwd = 2, col="darkgreen")
    
  }
}

par(mfrow=c(1,1))

#### Changepoint in model residuals.

## For GAM + Ar(1) Model Residuals of Variable BX  # CUSUM # BinSeg # 30 changepoints 29 storm dates

# Mean change
plot(cpt.mean(data = modelgamBx.res,penalty = "AIC",method = "BinSeg",
              test.stat = "CUSUM",Q = 30,param.estimates = T),cpt.width=2,cpt.col="red")
for ( i in which(!(is.na(mydata.imputeTS.1$Storm.Dates)))){
  segments(x0 = i, 
           y0= par("usr")[3], 
           x1 = i, 
           y1 = modelgamBx.res[i], col = "firebrick4", lwd = 2)
}

#variance change
plot(cpt.var(data = modelgamBx.res,penalty = "AIC",method = "BinSeg",
             test.stat = "CSS",Q = 30,param.estimates = T),cpt.width=2,cpt.col="red")
for ( i in which(!(is.na(mydata.imputeTS.1$Storm.Dates)))){
  segments(x0 = i, 
           y0= par("usr")[3], 
           x1 = i, 
           y1 = modelgamBx.res[i], col = "darkgreen", lwd = 2)
}

# Mean and Variance change
plot(cpt.meanvar(data = modelgamBx.res,penalty = "AIC",method = "BinSeg",
                 test.stat = "Normal",Q = 30,param.estimates = T),cpt.width=2,cpt.col="red")
for ( i in which(!(is.na(mydata.imputeTS.1$Storm.Dates)))){
  segments(x0 = i, 
           y0= par("usr")[3], 
           x1 = i, 
           y1 = mydata.imputeTS.1$B[i], col = "firebrick4", lwd = 2)
}


## For GAM Model By  # CUSUM # BinSeg # 30 changepoints 29 storm dates

plot(cpt.mean(data = modelgamBy.res,penalty = "AIC",method = "BinSeg",
              test.stat = "CUSUM",Q = 30,param.estimates = T),cpt.width=2,cpt.col="red")
for ( i in which(!(is.na(mydata.imputeTS.1$Storm.Dates)))){
  segments(x0 = i, 
           y0= par("usr")[3], 
           x1 = i, 
           y1 = modelgamBy.res[i], col = "firebrick4", lwd = 2)
}


plot(cpt.var(data = modelgamBy.res,penalty = "AIC",method = "BinSeg",
             test.stat = "CSS",Q = 30,param.estimates = T),cpt.width=2,cpt.col="red")
for ( i in which(!(is.na(mydata.imputeTS.1$Storm.Dates)))){
  segments(x0 = i, 
           y0= par("usr")[3], 
           x1 = i, 
           y1 = modelgamBy.res[i], col = "firebrick4", lwd = 2)
}

plot(cpt.meanvar(data = modelgamBy.res,penalty = "AIC",method = "BinSeg",
                 test.stat = "Normal",Q = 30,param.estimates = T),cpt.width=2,cpt.col="red")
for ( i in which(!(is.na(mydata.imputeTS.1$Storm.Dates)))){
  segments(x0 = i, 
           y0= par("usr")[3], 
           x1 = i, 
           y1 = modelgamBy.res[i], col = "firebrick4", lwd = 2)
}

## For GAM Model Bz  # CUSUM # BinSeg # 30 changepoints 29 storm dates

plot(cpt.mean(data = modelgamBz.res,penalty = "AIC",method = "BinSeg",
              test.stat = "CUSUM",Q = 30,param.estimates = T),cpt.width=2,cpt.col="red")
for ( i in which(!(is.na(mydata.imputeTS.1$Storm.Dates)))){
  segments(x0 = i, 
           y0= par("usr")[3], 
           x1 = i, 
           y1 = modelgamBz.res[i], col = "firebrick4", lwd = 2)
}


plot(cpt.var(data = modelgamBz.res,penalty = "AIC",method = "BinSeg",
             test.stat = "CSS",Q = 30,param.estimates = T),cpt.width=2,cpt.col="red")
for ( i in which(!(is.na(mydata.imputeTS.1$Storm.Dates)))){
  segments(x0 = i, 
           y0= par("usr")[3], 
           x1 = i, 
           y1 = modelgamBz.res[i], col = "firebrick4", lwd = 2)
}

plot(cpt.meanvar(data = modelgamBz.res,penalty = "AIC",method = "BinSeg",
                 test.stat = "Normal",Q = 30,param.estimates = T),cpt.width=2,cpt.col="red")
for ( i in which(!(is.na(mydata.imputeTS.1$Storm.Dates)))){
  segments(x0 = i, 
           y0= par("usr")[3], 
           x1 = i, 
           y1 = modelgamBz.res[i], col = "firebrick4", lwd = 2)
}

modelgamB <- gam(formula = B ~ s(month,bs="cc",k = 12) + s(New_tp1, k = 10) + 
                   te(New_tp1,month,k=c(10,12)), data = mydata.imputeTS.1, method="REML")
summary(modelgamB)




### Technique to choose Penalty or Number of changepoints
# cpt.var(data = modelgamBz.res,method="PELT",penalty="CROPS",pen.value=c(5,500))
# cpts.full(cpt.var(data = modelgamBz.res,method="PELT",penalty="CROPS",pen.value=c(5,500)))
# pen.value.full(cpt.var(data = modelgamBz.res,method="PELT",penalty="CROPS",pen.value=c(5,500)))
# 
# plot(cpt.var(data = modelgamBz.res,method="PELT",penalty="CROPS",pen.value=c(5,500)),ncpts=4)
# #plot(cpt.var(data = modelgamBz.res,method="PELT",penalty="CROPS",pen.value=c(5,500)),
# #     diagnostic=T)   # 30 change points look fine


# Change point using Using Changepoint.Np Package based on Empirical distribution on Location


library(changepoint.np)

plot(changepoint.np::cpt.np(data = modelgamBx.res,method = "PELT",
                            test.stat = "empirical_distribution",class = T,nquantiles = 4*log(length(modelgamBx.res))))

plot(changepoint.np::cpt.np(data = modelgamBy.res,method = "PELT",
                            test.stat = "empirical_distribution",class = T,nquantiles = 4*log(length(modelgamBx.res))))

plot(changepoint.np::cpt.np(data = modelgamBz.res,method = "PELT",
                            test.stat = "empirical_distribution",class = T,nquantiles = 4*log(length(modelgamBx.res))))


### Advanced Analysis Chapter #################
########################################################
## First Primary Question of Interest
########################################################
## Univariate Time-Series Models
################################################################

# Code for GARCH model to capture the mild correlation in the GAM + AR(1) models

FinTS::ArchTest(x = modelgamBx.res,lags = 1,demean = F) # Presence of ARCH 

#Variance.Model: Model: “sGARCH”, “fGARCH”, “eGARCH”, “gjrGARCH”, “apARCH” and “iGARCH” and “csGARCH
# submodel If the model is “fGARCH”, valid submodels are “GARCH”, “TGARCH”,
# “AVGARCH”, “NGARCH”, “NAGARCH”, “APARCH”,“GJRGARCH” and “ALLGARCH”.

#mean.model=list(armaOrder=c(final.order[1], final.order[3]))
spec1 = ugarchspec(
  variance.model=list(model="sGARCH",garchOrder=c(1,1)),distribution.model="sged")

# Working on the GAM + AR(1) Residuals of Bx response

fit1 = tryCatch(
  ugarchfit(
    spec1,data = modelgamBx.res, solver = 'hybrid'
  ), error=function(e) e, warning=function(w) w
)

fit1
acf(fit1@fit$residuals)
pacf(fit1@fit$residuals)
#acf(fit1@fit$residuals^2)
#pacf(fit1@fit$residuals^2)
plot(fit1@fit$residuals)
plot(fit1@fit$var) # residuals^2
plot(fit1@fit$sigma^2) 
#plot(modelgamBx.res,type="l")
#lines(fit@fit$residuals,col = "red")

std.residualBx <- modelgamBx.res/fit1@fit$sigma ## Standardized Residual = AR(1) Residual/GARCH Volatility
plot(std.residualBx)
plot(std.residualBx**2)  ### Square of Standardized Residuals is equivalent to the overall variance.
#lines((modelgamBx.res))
acf(std.residualBx)
pacf(std.residualBx)
acf(std.residualBx**2) # Stationary
pacf(std.residualBx**2) # Stationary
qqnorm(std.residualBx**2)
qqnorm(std.residualBx)
qqline(std.residualBx)
shapiro.test(std.residualBx**2)

Box.test(x = std.residualBx**2,type = "Box-Pierce") # Independce
Box.test(x = std.residualBx**2,type = "Ljung-Box") # Independence
Box.test(x = fit1@fit$residuals,type="Ljung-Box")
tseries::adf.test(x = std.residualBx**2,k=1)




fit1.1 = tryCatch(
  ugarchfit(
    spec1,data = modelgamBy.res, solver = 'hybrid'
  ), error=function(e) e, warning=function(w) w
)

std.residualBy <- modelgamBy.res/fit1.1@fit$sigma
acf(std.residualBy**2) # Stationary
pacf(std.residualBy**2) # Stationary
qqnorm(std.residualBy)
qqline(std.residualBy)
Box.test(x = std.residualBy**2,type = "Box-Pierce") # Independce
Box.test(x = std.residualBy**2,type = "Ljung-Box")
tseries::adf.test(x = std.residualBy**2,k=1)

fit1.2 = tryCatch(
  ugarchfit(
    spec1,data = modelgamBz.res, solver = 'hybrid'
  ), error=function(e) e, warning=function(w) w
)

std.residualBz <- modelgamBz.res/fit1.2@fit$sigma
acf(std.residualBz**2) # Stationary
pacf(std.residualBz**2) # Stationary
qqnorm(std.residualBz)
qqline(std.residualBz)
Box.test(x = std.residualBz**2,type = "Box-Pierce") # Independce
Box.test(x = std.residualBz**2,type = "Ljung-Box")
tseries::adf.test(x = std.residualBz**2,k=1)

par(mfrow=c(2,3))
acf(std.residualBx**2) # Stationary
acf(std.residualBy**2)
acf(std.residualBz**2) 
pacf(std.residualBx**2) 
pacf(std.residualBy**2) 
pacf(std.residualBz**2) 
par(mfrow=c(1,1))


# Package MSGARCH for GARCH Models ### Markov Switching GARCH Models

# Version 1

ms.Spec <- MSGARCH::CreateSpec(
  variance.spec = list(model = c("sGARCH", "tGARCH", "eGARCH")),
  distribution.spec = list(distribution = c("snorm", "std", "sged")))
summary(ms.Spec)

ms.Fit <- MSGARCH::FitML(spec = ms.Spec, data = modelgamBx.res) # Maximum Likelihood on Ar(1) Residuals

summary(ms.Fit)
#resid(ms.Fit)

# ExtractStateFit(ms.Fit)
# ms.Fit$Inference$MatCoef
# plot(MSGARCH::Volatility(ms.Fit))
# acf(MSGARCH::Volatility(ms.Fit))
# MSGARCH::UncVol(object = ms.Fit)
# acf(modelgamBx.res/sqrt(MSGARCH::Volatility(ms.Fit)))
# pacf(modelgamBx.res/sqrt(MSGARCH::Volatility(ms.Fit)))
# acf(modelgamBx.res/sqrt(MSGARCH::UncVol(ms.Fit)))
# pacf(modelgamBx.res/sqrt(MSGARCH::UncVol(ms.Fit)))
# str(MSGARCH::State(object = ms.Fit))
# mean(modelgamBx.res)
#MSGARCH::PIT(object = ms.Fit$Inference$Hessian)

# version 2

ms.Spec1 <- MSGARCH::CreateSpec(
  variance.spec = list(model = c("eGARCH", "eGARCH", "eGARCH")),
  distribution.spec = list(distribution = c("sged", "sged", "sged")))
summary(ms.Spec)

ms.Fit1 <- MSGARCH::FitMCMC(spec = ms.Spec1, data = modelgamBx.res) # fitting using MCMC Method

summary(ms.Fit1)
#ms.Fit1$par
#ms.Fit1$accept

#ExtractStateFit(ms.Fit1)
#ms.Fit1$Inference$MatCoef
#plot(MSGARCH::Volatility(ms.Fit1))
#acf(MSGARCH::Volatility(ms.Fit1))
#MSGARCH::UncVol(object = ms.Fit1)


plot(modelgamBx.res**2)
#lines(Volatility(ms.Fit1)**2,col="red")
plot((modelgamBx.res))
#lines(Volatility(ms.Fit1),col="red")
#acf(((modelgamBx.res)-Volatility(ms.Fit1))/(MSGARCH::Volatility(ms.Fit1)))
#pacf((abs(modelgamBx.res)-Volatility(ms.Fit1))/(MSGARCH::Volatility(ms.Fit1)))
#Box.test(x = (abs(modelgamBx.res)-Volatility(ms.Fit1))/(MSGARCH::Volatility(ms.Fit1)),lag = 1)
#plot(Risk(ms.Fit1)[[1]])

std.residual <- modelgamBx.res/(MSGARCH::Volatility(ms.Fit1)) ## Standardized Residual = AR(1) Residual/GARCH Volatility
plot(std.residual**2) ### Square of Standardized Residuals is equivalent to the overall variance.
#lines((modelgamBx.res))
acf(std.residual**2)
pacf(std.residual**2)
qqnorm(std.residual**2)
qqnorm(std.residual)
qqline(std.residual)


########## Another Version of Univariate Regression Model
#### combining GAM and Auto Regressive distributed Lag Model

# Models with dlnm package

cbgam1 <- crossbasis(x = mydata.imputeTS.1$imputets.Bx,lag=c(1,2),arglag = list(fun="ps",df=5))
summary(cbgam1)

# DEFINE THE PENALTY MATRICES
cbgam1Pen <- cbPen(cbgam1)
cbgam1Pen

# RUN THE GAM MODEL AND PREDICT (TAKES ~34sec IN A 2.4 GHz PC)
system.time({
  gam1 <- gam(formula = imputets.Bx~cbgam1 +  s(month,k = 12,bs="cc") + s(New_tp1,k = 10) + 
                te(New_tp1,month,k = c(10,12)),family= gaussian,data = mydata.imputeTS.1,
              method='REML')
})

#paraPen=list(cbgam1=cbgam1Pen),
summary(gam1)
qqnorm(resid(gam1))
qqline(y = resid(gam1))
acf(resid(gam1))
pacf(resid(gam1))
Box.test(x = resid(gam1),type = "Ljung-Box")
Box.test(x = resid(gam1), type = "Box-Pierce")
tseries::adf.test(x = resid(gam1),alternative = "stationary")
tseries::kpss.test(x = resid(gam1), null = "Trend",lshort = T) #lshort T or F same result
tseries::kpss.test(x = resid(gam1), null = "Level",lshort = T) #lshort T or F same result
tseries::pp.test(x = resid(gam1), alternative = "stationary",lshort = T,type = "Z(t_alpha)")
shapiro.test(resid(gam1))
plot(mydata.imputeTS.1$imputets.Bx,type="l")
lines(fitted(gam1),col="red")
plot(x = fitted(gam1),y = resid(gam1))
plot(gam1, pages=1, residuals = TRUE, cex=3, scheme = 1)
plot(gam1, pages=1, residuals = FALSE, cex=3, scheme = 1)
plot(gam1, pages=1, residuals = FALSE, cex=3, scheme = 2)
vis.gam(gam1, plot.type="persp")
sum(gam1$residuals)
sum(residuals(gam1))
sum(resid(gam1))

############### Univariate Pure Auto Regressive distributed Lag Model
# ARDL Model
ardl1 <- dynlm(ts(imputets.By) ~ L(ts(imputets.By), 1:30) + month + New_tp1,data = mydata.imputeTS.1)
summary(ardl1)
qqnorm(rstandard(ardl1))
qqline(y = rstandard(ardl1))
acf(rstandard(ardl1))
pacf(rstandard(ardl1))
Box.test(x = rstandard(ardl1),type = "Ljung-Box")
Box.test(x = rstandard(ardl1), type = "Box-Pierce")
tseries::adf.test(x = rstandard(ardl1),alternative = "stationary")
tseries::kpss.test(x = resid(ardl1), null = "Trend",lshort = T) #lshort T or F same result
tseries::kpss.test(x = resid(ardl1), null = "Level",lshort = T) #lshort T or F same result
tseries::pp.test(x = resid(ardl1), alternative = "stationary",lshort = T,type = "Z(t_alpha)")
shapiro.test(resid(ardl1))
plot(mydata.imputeTS.1$imputets.By,type="l")
lines(fitted(ardl1),col="red")





# Multivariate Time-Series Models
#################################

# Vector Auto Regression (VAR)

#library(vars)

plot.ts(mydata.imputeTS.1[,c(7,8,9)],plot.type = "multiple")
#plot(mydata.imputeTS.1[,c(7,8,9)],plot.type = "multiple")


varmodel1 <- (vars::VAR(y = mydata.imputeTS.1[,c(7,8,9)],p = 5,exogen = mydata.imputeTS.1[,c(2,10)]))
summary(varmodel1)
vars::serial.test(x = varmodel1,lags.pt = 10,lags.bg = 5,type = "BG")
vars::normality.test(x = varmodel1,multivariate.only = F)
vars::roots(x = varmodel1,modulus = T)
#plot(fitted.values(varmodel1))
plot(mydata.imputeTS.1$imputets.Bx, type="l")
lines(fitted(varmodel1$varresult$imputets.Bx),col="red")
qqnorm(resid(varmodel1))
#plot()
acf(resid(varmodel1,type="pearson"))
pacf(resid(varmodel1))


# spec2 = ugarchspec(
#   variance.model=list(model="sGARCH",garchOrder=c(1,1)),distribution.model="sged")
# 
# fit2 <- tryCatch(
#   ugarchfit(
#     spec2,data = resid(varmodel1), solver = 'hybrid'
#   ), error=function(e) e, warning=function(w) w
# )

# fit2
# std.residual.var <- resid(varmodel1)/fit2@fit$sigma ## Standardized Residual = AR(1) Residual/GARCH Volatility
# plot(std.residual.var)
# plot(std.residual.var**2)  ### Square of Standardized Residuals is equivalent to the overall variance.
# #lines((modelgamBx.res))
# acf(std.residual.var)
# pacf(std.residual.var)
# acf(std.residual.var**2) # Stationary
# pacf(std.residual.var**2) # Stationary
# qqnorm(std.residual.var**2)
# qqnorm(std.residual.var)
# qqline(std.residual.var)
# shapiro.test(std.residual.var**2)

# Box.test(x = std.residual.var**2,type = "Box-Pierce") # Independce
# Box.test(x = std.residual.var**2,type = "Ljung-Box") # Independence
# Box.test(x = fit2@fit$residuals,type="Ljung-Box")
# tseries::adf.test(x = std.residual.var**2,k=1)


### Advanced Analysis Chapter #################
########################################################
## Second Primary Question of Interest
########################################################

#### Change point Model using cpm package

# The Mann-Whitney and Mood statistics for detecting location and scale changes respectively in sequences of random variables, where no assumptions are made about the
# distribution (Hawkins and Deng 2010; Ross et al. 2011).

# The Lepage, Kolmogorov-Smirnov, and Cramer-von-Mises statistics for detecting more
# general distributional changes where again no assumptions are made about the sequence
# distribution (Ross and Adams 2012).

### http://www.gordonjross.co.uk/cpm.pdf

# single changepoint Gaussian
cpm::detectChangePointBatch(x = mydata.imputeTS.1$imputets.Bx,cpmType = "Student",alpha = 0.05)

# Single Changepoint Non-Gaussian
cpm::detectChangePointBatch(x = mydata.imputeTS.1$imputets.Bx,cpmType = "Mann-Whitney",alpha = 0.05)

cpm::detectChangePointBatch(x = modelgamBx.res,cpmType = "Mann-Whitney",alpha = 0.05)

# Multiple change points location wise  Non-gaussian original data
cpmMean1 <- cpm::processStream(x = mydata.imputeTS.1$imputets.Bx,cpmType = "Mann-Whitney",ARL0 = 1000,startup = 20)
cpmMean1$changePoints
cpmMean1$detectionTimes

plot(modelgamBx.res)
abline(v=cpm::processStream(x = modelgamBx.res,cpmType = "Mann-Whitney",ARL0 = 1000,startup = 20)$detectionTimes)

plot(modelgamBx.res)
abline(v=cpm::processStream(x = modelgamBx.res,cpmType = "Mood",ARL0 = 2000,startup = 20)$detectionTimes)

plot(modelgamBx.res)
abline(v=cpm::processStream(x = modelgamBx.res,cpmType = "Lepage",ARL0 = 2000,startup = 20)$detectionTimes)

plot(modelgamBx.res)
abline(v=cpm::processStream(x = modelgamBx.res,cpmType = "Kolmogorov-Smirnov",ARL0 = 2000,startup = 20)$detectionTimes)

plot(modelgamBx.res)
abline(v=cpm::processStream(x = modelgamBx.res,cpmType = "Cramer-von-Mises",ARL0 = 2000,startup = 20)$detectionTimes)


# plotting the Original Data Bx, Corresponding Storm Dates and Mean Change points.
plot(mydata.imputeTS.1$imputets.Bx)
for ( i in which(!(is.na(mydata.imputeTS.1$Storm.Dates)))){
  segments(x0 = i, 
           y0= par("usr")[3], 
           x1 = i, 
           y1 = mydata.imputeTS.1$imputets.Bx[i], col = "firebrick4", lwd = 2)
}

for (i in 1:length(cpmMean1$changePoints)) {
  #abline(v=cpmMean1$changePoints[i], lty=2)
  abline(v=cpmMean1$detectionTimes[i],lty=2)
}


# Change point using Using BCP Package
# Bayesian Regression Model using Posterior Probabilities of change in Location

plot(bcp::bcp(y = mydata.imputeTS.1$imputets.Bx,burnin = 1000,mcmc = 10000,p0 = 0.005,return.mcmc = T))

plot(bcp::bcp(y = modelgamBx.res,burnin = 1000,mcmc = 10000,p0 = 0.005,return.mcmc = T))

modelbcp.Bx <- bcp::bcp(y = modelgamBx.res, burnin = 100,mcmc = 1000,return.mcmc = T)
modelbcp.Bx

plot(x = mydata.imputeTS.1$New_tp1,y = modelbcp.Bx$posterior.prob,xaxt="n")
axis(side = 1,at = c(2003,2004,2005,2006,2007,2008,2009,2010,2011,2012,2013))
segments(x0 = mydata.imputeTS.1$New_tp1[which(!(is.na(mydata.imputeTS.1$Storm.Dates)))], 
         y0= par("usr")[3], 
         x1 = mydata.imputeTS.1$New_tp1[which(!(is.na(mydata.imputeTS.1$Storm.Dates)))], 
         y1 = modelbcp.Bx$posterior.prob[which(!(is.na(mydata.imputeTS.1$Storm.Dates)))], col = 1:29, lwd = 2)

for ( i in mydata.imputeTS.1$New_tp1[which(!(is.na(mydata.imputeTS.1$Storm.Dates)))]){
  segments(x0 = i, 
           y0= par("usr")[3], 
           x1 = i, 
           y1 = modelbcp.Bx$posterior.prob[which(!(is.na(mydata.imputeTS.1$Storm.Dates)))], col = "firebrick4", lwd = 2)
}

plot(x = mydata.imputeTS.1$New_tp1, y = mydata.imputeTS.1$B, type='p', xaxt='n',
     xlab="Year",ylab="Value of B in nano-Tesla",main="Time-plot of variable B",col="darkmagenta",pch=21)
axis(side = 1,at = c(2003,2004,2005,2006,2007,2008,2009,2010,2011,2012,2013))
#abline(v = mydata.imputeTS.1$New_tp1[which(!(is.na(mydata.imputeTS.1$Storm.Dates)))])
segments(x0 = mydata.imputeTS.1$New_tp1[which(!(is.na(mydata.imputeTS.1$Storm.Dates)))], 
         y0= par("usr")[3], 
         x1 = mydata.imputeTS.1$New_tp1[which(!(is.na(mydata.imputeTS.1$Storm.Dates)))], 
         y1 = mydata.imputeTS.1$B[which(!(is.na(mydata.imputeTS.1$Storm.Dates)))], col = 1:29, lwd = 2)








