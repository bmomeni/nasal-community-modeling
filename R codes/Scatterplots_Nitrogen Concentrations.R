#Last run 04/10/2020

#Clear Workspace 
rm(list=ls())

#################################################
#Create scatterplots for GR-CC relationships 
#################################################

####################################################################################################
#C. tuberculostearicum in nitrogen concentrations 
NC_Ct <- c(0.048241667,
           0.03618125,
           0.024120833,
           0.012060417,
           0.00723625,
           0.004824167,
           0.002412083)

GR_Ct <- c(1.5164,
           1.389425,
           1.276025,
           1.093125,
           0.870075,
           0.9651,
           1.01045)

DF_Ct <- data.frame(NC_Ct, GR_Ct)
DF_Ct

#Generate a linear regression model of the two variables with the lm function 
Ct.lm <- lm(GR_Ct ~ NC_Ct + 1, data=DF_Ct)
Ct.lm2 <- lm(GR_Ct ~ 0 + NC_Ct, data=DF_Ct) #Tells the lm() to fit the line through the origin
summary(Ct.lm)
summary(Ct.lm2)

#Create scatter plot of points using plot function
plot(NC_Ct, GR_Ct,
     xlab="Nitrogen Concentration (M)",
     ylab="Growth Rate (1/hr.)",
     xlim= c(0, 49),
     ylim= c(0, 1.6),
     main="Correlation Between Growth Rate and Nitrogen Concentration (M) for C. tuberculostearicum",
     pch=16)

abline(lm(Ct.lm2))
legend("topleft", bty="n", legend= paste("R-squared= 0.671 \n p-value= 0.004"))

#abline(lm(Ct.lm))
#legend("topleft", bty="n", legend= paste("R-squared= 0.910 \n p-value= 0.00016"))

#Create scatter plot of points using plot function - without graph labels 
plot(NC_Ct, GR_Ct,
     xlab="Nitrogen Concentration (M)",
     ylab="Growth Rate (1/hr.)",
     xlim= c(0, 49),
     ylim= c(0, 1.6),
     cex= 1.5,
     pch=16)

abline(lm(Ct.lm2))
op <- par(cex= 1.5)
legend("topleft", bty="n", legend= paste("R-squared= 0.671 \n p-value= 0.004"))

###################################################################################################
#S. aureus in carbon concentrations 
CC_Sa <- c(0.097702197,
           0.073276648,
           0.048851098,
           0.024425549,
           0.01465533,
           0.00977022,
           0.00488511)

GR_Sa <- c(0.81335,
           0.740675,
           0.7548,
           0.5476,
           0.517275,
           0.363366667,
           0)

DF_Sa <- data.frame(CC_Sa, GR_Sa)
DF_Sa

#Generate a linear regression model of the two variables with the lm function 
Sa.lm <- lm(GR_Sa ~ CC_Sa + 1, data=DF_Sa)
Sa.lm2 <- lm(GR_Sa ~ 0 + CC_Sa, data=DF_Sa) #Tells the lm() to fit the line through the origin
summary(Sa.lm)
summary(Sa.lm2)

#Create scatter plot of points using plot function
plot(CC_Sa, GR_Sa,
     xlab="Carbon Concentration (M)",
     ylab="Growth Rate (1/hr.)",
     xlim= c(0, 0.1),
     ylim= c(0, 1.6),
     main="Correlation Between Growth Rate and Carbon Concentration (M) for S. aureus",
     pch=16)

abline(lm(Sa.lm2))
legend("topleft", bty="n", legend= paste("R-squared= 0.671 \n p-value= 0.004"))

#abline(lm(Sa.lm))
#legend("topleft", bty="n", legend= paste("R-squared= 0.910 \n p-value= 0.00016"))

#Create scatter plot of points using plot function - without graph labels 
plot(CC_Sa, GR_Sa,
     xlab= "Carbon Concentration (M)",
     ylab= "Growth Rate (1/hr.)",
     xlim= c(0, 0.1),
     ylim= c(0, 1.6),
     cex= 1.5,
     pch=16)

abline(lm(Sa.lm2))
op <- par(cex= 1.5)
legend("topleft", bty="n", legend= paste("R-squared= 0.671 \n p-value= 0.004"))

####################################################################################################
#S. aureus in nitrogen concentrations 
NC_Sa <- c(0.048241667,
           0.03618125,
           0.024120833,
           0.012060417,
           0.00723625,
           0.004824167,
           0.002412083)

GR_Sa <- c(0.81335,
           0.740675,
           0.7548,
           0.5476,
           0.517275,
           0.363366667,
           0)

DF_Sa <- data.frame(NC_Sa, GR_Sa)
DF_Sa

#Generate a linear regression model of the two variables with the lm function
Sa.lm <- lm(GR_Sa ~ NC_Sa + 1, data=DF_Sa)
Sa.lm2 <- lm(GR_Sa ~ 0 + NC_Sa, data=DF_Sa) #Tells the lm() to fit the line through the origin
summary(Sa.lm)
summary(Sa.lm2)

#Create scatter plot of points using plot function
plot(NC_Sa, GR_Sa,
     xlab="Nitrogen Concentration (M)",
     ylab="Growth Rate (1/hr.)",
     xlim= c(0, 49),
     ylim= c(0, 1.6),
     main="Correlation Between Growth Rate and Nitrogen Concentration (M) for S. aureus",
     pch=16)

abline(lm(Sa.lm2))
legend("topleft", bty="n", legend= paste("R-squared= 0.671 \n p-value= 0.004"))

abline(lm(Sa.lm))
legend("topleft", bty="n", legend= paste("R-squared= 0.910 \n p-value= 0.00016"))

#Create scatter plot of points using plot function - without graph labels 
plot(NC_Sa, GR_Sa,
     xlab="Nitrogen Concentration (M)",
     ylab="Growth Rate (1/hr.)",
     xlim= c(0, 49),
     ylim= c(0, 1.6),
     cex= 1.5,
     pch=16)

abline(lm(Sa.lm2))
op <- par(cex= 1.5)
legend("topleft", bty="n", legend= paste("R-squared= 0.910 \n p-value= 0.00016"))
