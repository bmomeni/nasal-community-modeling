#Last run 04/10/2020

#Clear Workspace 
rm(list=ls())

#################################################
#Create scatterplots for GR-CC relationships 
#################################################

####################################################################################################
#C. tuberculostearicum in carbon concentrations 
CC_Ct <- c(0.097702197,
           0.073276648,
           0.048851098,
           0.024425549,
           0.01465533,
           0.00977022,
           0.00488511,
           0.002442555)

GR_Ct <- c(1.5164,
           1.389425,
           1.276025,
           1.093125,
           0.870075,
           0.9651,
           1.01045,
           0.8175)

DF_Ct <- data.frame(CC_Ct, GR_Ct)
DF_Ct

#Generate a linear regression model of the two variables with the lm function 
Ct.lm <- lm(GR_Ct ~ CC_Ct + 1, data=DF_Ct)
Ct.lm2 <- lm(GR_Ct ~ 0 + CC_Ct, data=DF_Ct) #Tells the lm() to fit the line through the origin
summary(Ct.lm)
summary(Ct.lm2)

#Colors
colors <- c("deepskyblue4", "darkorchid4", "darkolivegreen", "firebrick4", "darkorange3", "deeppink2", "darkslategray", "bisque4")


#Create scatter plot of points using plot function
plot(CC_Ct, GR_Ct,
     xlab="Carbon Concentration (M)",
     ylab="Growth Rate (1/hr.)",
     xlim= c(0, 0.1),
     ylim= c(0, 1.6),
     col = colors,
     main="Correlation Between Growth Rate and Carbon Concentration (M) for C. tuberculostearicum",
     pch=16)

abline(lm(Ct.lm2))
legend("topleft", bty="n", legend= paste("R-squared= 0.671 \n p-value= 0.004"))

#abline(lm(Ct.lm))
#legend("topleft", bty="n", legend= paste("R-squared= 0.910 \n p-value= 0.00016"))

#Create scatter plot of points using plot function - without graph labels 
plot(CC_Ct, GR_Ct,
     xlab= "Carbon Concentration (M)",
     ylab= "Growth Rate (1/hr.)",
     xlim= c(0, 0.1),
     ylim= c(0, 1.6),
     cex= 1.8,
     col = colors,
     pch=16)

abline(lm(Ct.lm2))
op <- par(cex= 1.5)
legend("topleft", bty="n", legend= paste("R-squared= 0.671 \n p-value= 0.004"))

####################################################################################################
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

#Colors
colors <- c("deepskyblue4", "darkorchid4", "darkolivegreen", "firebrick4", "darkorange3", "deeppink2", "darkslategray", "bisque4")

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
     col = colors,
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
     cex= 1.8,
     col = colors,
     pch=16)

abline(lm(Sa.lm2))
op <- par(cex= 1.5)
legend("topleft", bty="n", legend= paste("R-squared= 0.671 \n p-value= 0.004"))

###########################################################################################
#S. non-aureus str 1839 in carbon concentrations 
CC_Sna1839 <- c(0.097702197,
           0.073276648,
           0.048851098,
           0.024425549,
           0.01465533,
           0.00977022,
           0.00488511)

GR_Sna1839 <- c(0.599175,
                0.57435,
                0.29115,
                0.1218,
                0.16725,
                0.3196,
                0.253466667)

DF_Sna1839 <- data.frame(CC_Sna1839, GR_Sna1839)
DF_Sna1839

#Colors
colors <- c("deepskyblue4", "darkorchid4", "darkolivegreen", "firebrick4", "darkorange3", "deeppink2", "darkslategray", "bisque4")

#Generate a linear regression model of the two variables with the lm function 
Sna1839.lm <- lm(GR_Sna1839 ~ CC_Sna1839 + 1, data=DF_Sna1839)
Sna1839.lm2 <- lm(GR_Sna1839 ~ 0 + CC_Sna1839, data=DF_Sna1839) #Tells the lm() to fit the line through the origin
summary(Sna1839.lm)
summary(Sna1839.lm2)

#Create scatter plot of points using plot function
plot(CC_Sna1839, GR_Sna1839,
     xlab="Carbon Concentration (M)",
     ylab="Growth Rate (1/hr.)",
     xlim= c(0, 0.1),
     ylim= c(0, 0.65),
     col = colors,
     main="Correlation Between Growth Rate and Carbon Concentration (M) for S. non-aureus str 1839",
     pch=16)

abline(lm(Sna1839.lm2))
legend("topleft", bty="n", legend= paste("R-squared= 0.8447 \n p-value= 7.77E-04"))

#abline(lm(Sna1839.lm))
#legend("topleft", bty="n", legend= paste("R-squared= 0.910 \n p-value= 0.00016"))

#Create scatter plot of points using plot function - without graph labels 
plot(CC_Sna1839, GR_Sna1839,
     xlab= "Carbon Concentration (M)",
     ylab= "Growth Rate (1/hr.)",
     xlim= c(0, 0.1),
     ylim= c(0, 0.65),
     cex= 1.8,
     col = colors,
     pch=16)

abline(lm(Sna1839.lm2))
op <- par(cex= 1.5)
legend("topleft", bty="n", legend= paste("R-squared= 0.8447 \n p-value= 7.77E-04"))

##########################################################################################3
##S. non-aureus str 1850 in carbon concentrations

CC_Sna1850 <- c(0.097702197,
                0.073276648,
                0.048851098,
                0.024425549,
                0.01465533,
                0.00977022,
                0.00488511)

GR_Sna1850 <- c(0.822725,
                0.47875,
                0.595275,
                0.3188,
                0.197575,
                0.30275,
                0.415366667)

DF_Sna1850 <- data.frame(CC_Sna1850, GR_Sna1850)
DF_Sna1850

#Generate a linear regression model of the two variables with the lm function 
Sna1850.lm <- lm(GR_Sna1850 ~ CC_Sna1850 + 1, data=DF_Sna1850)
Sna1850.lm2 <- lm(GR_Sna1850 ~ 0 + CC_Sna1850, data=DF_Sna1850) #Tells the lm() to fit the line through the origin
summary(Sna1850.lm)
summary(Sna1850.lm2)

#Colors
colors <- c("deepskyblue4", "darkorchid4", "darkolivegreen", "firebrick4", "darkorange3", "deeppink2", "darkslategray", "bisque4")

#Create scatter plot of points using plot function
plot(CC_Sna1850, GR_Sna1850,
     xlab="Carbon Concentration (M)",
     ylab="Growth Rate (1/hr.)",
     xlim= c(0, 0.1),
     ylim= c(0, 0.85),
     col = colors,
     main="Correlation Between Growth Rate and Carbon Concentration (M) for S. non-aureus str 1850",
     pch=16)

abline(lm(Sna1850.lm2))
legend("topleft", bty="n", legend= paste("R-squared= 0.8196 \n p-value= 0.0012"))

#abline(lm(Sna1850.lm))
#legend("topleft", bty="n", legend= paste("R-squared= 0.910 \n p-value= 0.00016"))

#Create scatter plot of points using plot function - without graph labels 
plot(CC_Sna1850, GR_Sna1850,
     xlab= "Carbon Concentration (M)",
     ylab= "Growth Rate (1/hr.)",
     xlim= c(0, 0.1),
     ylim= c(0, 0.85),
     cex= 1.8,
     col = colors,
     pch=16)

abline(lm(Sna1850.lm2))
op <- par(cex= 1.5)
legend("topleft", bty="n", legend= paste("R-squared= 0.8196 \n p-value= 0.0012"))

#######################################################################################
##S. non-aureus str 1867 in carbon concentrations

CC_Sna1867 <- c(0.097702197,
                0.073276648,
                0.048851098,
                0.024425549,
                0.01465533,
                0.00977022,
                0.00488511)

GR_Sna1867 <- c(0.9857,
                0.6512,
                0.844475,
                0.285375,
                0.307625,
                0.3798,
                0.47725)

DF_Sna1867 <- data.frame(CC_Sna1867, GR_Sna1867)
DF_Sna1867

#Generate a linear regression model of the two variables with the lm function 
Sna1867.lm <- lm(GR_Sna1867 ~ CC_Sna1867 + 1, data=DF_Sna1867)
Sna1867.lm2 <- lm(GR_Sna1867 ~ 0 + CC_Sna1867, data=DF_Sna1867) #Tells the lm() to fit the line through the origin
summary(Sna1867.lm)
summary(Sna1867.lm2)

#Colors
colors <- c("deepskyblue4", "darkorchid4", "darkolivegreen", "firebrick4", "darkorange3", "deeppink2", "darkslategray", "bisque4")

#Create scatter plot of points using plot function
plot(CC_Sna1867, GR_Sna1867,
     xlab="Carbon Concentration (M)",
     ylab="Growth Rate (1/hr.)",
     xlim= c(0, 0.1),
     ylim= c(0, 1.0),
     col = colors,
     main="Correlation Between Growth Rate and Carbon Concentration (M) for S. non-aureus str 1867",
     pch=16)

abline(lm(Sna1867.lm2))
legend("topleft", bty="n", legend= paste("R-squared= 0.8238 \n p-value= 0.0011"))

#abline(lm(Sna1867.lm))
#legend("topleft", bty="n", legend= paste("R-squared= 0.910 \n p-value= 0.00016"))

#Create scatter plot of points using plot function - without graph labels 
plot(CC_Sna1867, GR_Sna1867,
     xlab= "Carbon Concentration (M)",
     ylab= "Growth Rate (1/hr.)",
     xlim= c(0, 0.1),
     ylim= c(0, 1.0),
     cex= 1.8,
     col = colors,
     pch=16)

abline(lm(Sna1867.lm2))
op <- par(cex= 1.5)
legend("topleft", bty="n", legend= paste("R-squared= 0.8238 \n p-value= 0.0011"))