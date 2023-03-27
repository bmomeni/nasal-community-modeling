#Last run 04/10/2020

#Clear Workspace 
rm(list=ls())

#################################################
#Create scatterplots for GR-CC relationships 
#################################################
#C. tuberculostearicum 

GR_Ct <- c(0.84725,
           0.762625,
           0.6342,
           0.430675,
           0.1679)
Max_Ct <- c(0.2798,
            0.1780,
            0.1398,
            0.0855,
            0.0480)

DF_Ct <- data.frame(GR_Ct, Max_Ct)
DF_Ct

#Generate a linear regression model of the two variables with the lm function 
Ct.lm <- lm(Max_Ct ~ GR_Ct + 1, data=DF_Ct)
Ct.lm2 <- lm(Max_Ct ~ 0 + GR_Ct, data=DF_Ct) #Tells the lm() to fit the line through the origin
summary(Ct.lm)
summary(Ct.lm2)
#Adjusted R-squared:  0.9527 
#p-value: 0.0005442

#Colors
colors <- c("deepskyblue4", "darkorchid4", "darkolivegreen", "firebrick4", "darkorange2")

#Create scatter plot of points using plot function
plot(GR_Ct, Max_Ct,
     xlab="Growth Rate (1/hr.)",
     ylab="Carrying Capacity (Max OD600)",
     xlim= c(0, 0.9),
     ylim= c(0, 0.30),
     col = colors,
     main="Growth Rate vs. Carrying Capacity for C. tuberculostearicum \n Grown in THY Dilutions",
     pch=16)

abline(lm(Ct.lm2))
legend("topleft", bty="n", legend= paste("R-squared= 0.9527 \n p-value= 5.4e-04"))

#Create scatter plot of points using plot function - without graph labels 
plot(GR_Ct, Max_Ct,
     xlab= "Growth Rate (1/hr)",
     ylab= "Max OD600",
     xlim= c(0, 0.9),
     ylim= c(0, 0.30),
     cex= 1.8,
     col = colors,
     pch=16)

abline(lm(Ct.lm2))
op <- par(cex= 1.5)
legend("topleft", bty="n", legend= paste("R-squared= 0.9527  \n p-value= 5.4e-04"))


###############################################################################################################333                                        
#S. aureus 

GR_Sa <- c(1.7244,
           1.4825,
           0.0000,
           0.2940,
           0.3358)
Max_Sa <- c(0.2470,
            0.1788,
            0.1173,
            0.0470,
            0.0208)

DF_Sa <- data.frame(GR_Sa, Max_Sa)
DF_Sa

#Generate a linear regression model of the two variables with the lm function
Sa.lm <- lm(Max_Sa ~ GR_Sa+1, data=DF_Sa)
Sa.lm2 <- lm(Max_Sa ~ 0+GR_Sa, data=DF_Sa) #Tells the lm() to fit the line through the origin
summary(Sa.lm)
summary(Sa.lm2)
#Adjusted R-squared:  0.8281 
#p-value: 0.007442

#Colors
colors <- c("deepskyblue4", "darkorchid4", "darkolivegreen", "firebrick4", "darkorange2")

#Create scatter plot of points using plot funSaion
plot(GR_Sa, Max_Sa,
     xlab="Growth Rate (1/hr.)",
     ylab="Carrying Capacity (Max OD600)",
     xlim= c(0, 2.0),
     ylim= c(0, 0.25),
     col = colors,
     main="Growth Rate vs. Carrying Capacity for S. aureus \n Grown in THY Dilutions",
     pch=16)

abline(lm(Sa.lm2))
legend("topleft", bty="n", legend= paste("R-squared= 0.8281 \n p-value= 7.4e-03"))

#Create scatter plot of points using plot function - without graph labels 
plot(GR_Sa, Max_Sa,
     xlab= "Growth Rate (1/hr)",
     ylab= "Max OD600",
     xlim= c(0, 2.0),
     ylim= c(0, 0.25),
     cex= 1.8,
     col=colors,
     pch=16)

abline(lm(Sa.lm2))
op <- par(cex= 1.5)
legend("topleft", bty="n", legend= paste("R-squared= 0.8281 \n p-value= 7.4e-03"))

##############################################################################################################
#S. non-aureus str. 1839

GR_Sna1839 <- c(0.9219,
                1.0792,
                0.7006,
                0.5722,
                0.0000)
Max_Sna1839 <- c(0.0903,
                 0.0760,
                 0.0608,
                 0.0295,
                 0.0215)

DF_Sna1839 <- data.frame(GR_Sna1839, Max_Sna1839)
DF_Sna1839

#Generate a linear regression model of the two variables with the lm function 
Sna1839.lm <- lm(Max_Sna1839 ~ 1+GR_Sna1839, data=DF_Sna1839)
Sna1839.lm2 <- lm(Max_Sna1839 ~ 0+GR_Sna1839, data=DF_Sna1839) #Tells the lm() to fit the line through the origin
summary(Sna1839.lm)
summary(Sna1839.lm2)
#Adjusted R-squared:  0.9256 
#p-value: 0.001357

#Colors
colors <- c("deepskyblue4", "darkorchid4", "darkolivegreen", "firebrick4", "darkorange2")

#Create scatter plot of points using plot funSna1839ion
plot(GR_Sna1839, Max_Sna1839,
     xlab="Growth Rate (1/hr.)",
     ylab="Carrying Capacity (Max OD600)",
     xlim= c(0, 1.2),
     ylim= c(0, 0.1),
     col = colors,
     main="Growth Rate vs. Carrying Capacity for S. non-aureus str. 1839 \n Grown in THY Dilutions",
     pch=16)

abline(lm(Sna1839.lm2))
legend("topleft", bty="n", legend= paste("R-squared= 0.9256 \n p-value= 1.4e-03"))

#Create scatter plot of points using plot function - without graph labels 
plot(GR_Sna1839, Max_Sna1839,
     xlab= "Growth Rate (1/hr)",
     ylab= "Max OD600",
     xlim= c(0, 1.2),
     ylim= c(0, 0.1),
     cex= 1.8,
     col = colors,
     pch=16)

abline(lm(Sna1839.lm2))
op <- par(cex= 1.5)
legend("topleft", bty="n", legend= paste("R-squared= 0.9256 \n p-value= 1.4e-03"))


#####################################################################################################################
#S. non-aureus str. 1850 

GR_Sna1850 <- c(1.0230,
                1.0126,
                0.8624,
                0.5994,
                0.0000)
Max_Sna1850 <- c(0.1113,
                 0.0785,
                 0.0680,
                 0.0310,
                 0.0190)

DF_Sna1850 <- data.frame(GR_Sna1850, Max_Sna1850)
DF_Sna1850

#Generate a linear regression model of the two variables with the lm function 
Sna1850.lm <- lm(Max_Sna1850 ~ 1+GR_Sna1850, data=DF_Sna1850)
Sna1850.lm2 <- lm(Max_Sna1850 ~ 0+GR_Sna1850, data=DF_Sna1850) #Tells the lm() to fit the line through the origin
summary(Sna1850.lm)
summary(Sna1850.lm2)
#Adjusted R-squared:  0.9267
#p-value: 0.001316

#Colors
colors <- c("deepskyblue4", "darkorchid4", "darkolivegreen", "firebrick4", "darkorange2")

#Create scatter plot of points using plot funSna1850ion
plot(GR_Sna1850, Max_Sna1850,
     xlab="Growth Rate (1/hr.)",
     ylab="Carrying Capacity (Max OD600)",
     xlim= c(0, 1.2),
     ylim= c(0, 0.12),
     col = colors,
     main="Growth Rate vs. Carrying Capacity for S. non-aureus str. 1850 \n Grown in THY Dilutions",
     pch=16)

abline(lm(Sna1850.lm2))
legend("topleft", bty="n", legend= paste("R-squared= 0.9267 \n p-value= 1.3e-03"))


#Create scatter plot of points using plot function - without graph labels 
plot(GR_Sna1850, Max_Sna1850,
     xlab= "Growth Rate (1/hr)",
     ylab= "Max OD600",
     xlim= c(0, 1.2),
     ylim= c(0, 0.12),
     cex= 1.8,
     col = colors,
     pch=16)

abline(lm(Sna1850.lm2))
op <- par(cex= 1.5)
legend("topleft", bty="n", legend= paste("R-squared= 0.9267 \n p-value= 1.3e-03"))

################################################################################################################
#S. non-aureus str. 1867

GR_Sna1867 <- c(0.8776,
                0.9570,
                0.5774,
                0.4059,
                0.0000)
Max_Sna1867 <- c(0.0880,
                 0.0770,
                 0.0560,
                 0.0253,
                 0.0218)

DF_Sna1867 <- data.frame(GR_Sna1867, Max_Sna1867)
DF_Sna1867

#Generate a linear regression model of the two variables with the lm function
Sna1867.lm <- lm(Max_Sna1867 ~ 1+GR_Sna1867, data=DF_Sna1867)
Sna1867.lm2 <- lm(Max_Sna1867 ~ 0+GR_Sna1867, data=DF_Sna1867) #Tells the lm() to fit the line through the origin
summary(Sna1867.lm)
summary(Sna1867.lm2)
#Adjusted R-squared:  0.9457 
#p-value: 0.000717

#Colors
colors <- c("deepskyblue4", "darkorchid4", "darkolivegreen", "firebrick4", "darkorange2")

#Create scatter plot of points using plot funSna1867ion
plot(GR_Sna1867, Max_Sna1867,
     xlab="Growth Rate (1/hr.)",
     ylab="Carrying Capacity (Max OD600)",
     xlim= c(0, 1.1),
     ylim= c(0, 0.1),
     col = colors,
     main="Growth Rate vs. Carrying Capacity for S. non-aureus str. 1867 \n Grown in THY Dilutions",
     pch=16)

abline(lm(Sna1867.lm2))
legend("topleft", bty="n", legend= paste("R-squared= 0.9457 \n p-value= 7.2e-04"))


#Create scatter plot of points using plot function - without graph labels 
plot(GR_Sna1867, Max_Sna1867,
     xlab= "Growth Rate (1/hr)",
     ylab= "Max OD600",
     xlim= c(0, 1.1),
     ylim= c(0, 0.1),
     cex= 1.8,
     col = colors,
     pch=16)

abline(lm(Sna1867.lm2))
op <- par(cex= 1.5)
legend("topleft", bty="n", legend= paste("R-squared= 0.9457 \n p-value= 7.2e-04"))


#######################################################################################################################
#C. pseudodiphtheriticum 

GR_Cp <- c(0.2089,
           0.1230,
           0.0741,
           0.0427,
           0.0721)
Max_Cp <- c(0.1243,
            0.0828,
            0.0655,
            0.0445,
            0.0263)

DF_Cp <- data.frame(GR_Cp, Max_Cp)
DF_Cp

#Generate a linear regression model of the two variables with the lm function 
Cp.lm <- lm(Max_Cp ~ 1+GR_Cp, data=DF_Cp)
Cp.lm2 <- lm(Max_Cp ~ 0+GR_Cp, data=DF_Cp) #Tells the lm() to fit the line through the origin
summary(Cp.lm)
summary(Cp.lm2)
#Adjusted R-squared:  0.9526
#p-value: 0.0005472

#Colors
colors <- c("deepskyblue4", "darkorchid4", "darkolivegreen", "firebrick4", "darkorange2")

#Create scatter plot of points using plot funCpion
plot(GR_Cp, Max_Cp,
     xlab="Growth Rate (1/hr.)",
     ylab="Carrying Capacity (Max OD600)",
     xlim= c(0, 0.22),
     ylim= c(0, 0.13),
     cex= 1.5,
     col = colors,
     main="Growth Rate vs. Carrying Capacity for C. pseudodiphtheriticum \n Grown in THY Dilutions",
     pch=16)

abline(lm(Cp.lm2))
legend("topleft", bty="n", legend= paste("R-squared= 0.9526  \n p-value= 5.5e-04"))

#Create scatter plot of points using plot function - without graph labels 
plot(GR_Cp, Max_Cp,
     xlab= "Growth Rate (1/hr)",
     ylab= "Max OD600",
     xlim= c(0, 0.22),
     ylim= c(0, 0.13),
     cex= 1.8,
     col = colors,
     pch=16)

abline(lm(Cp.lm2))
op <- par(cex= 1.5)
legend("topleft", bty="n", legend= paste("R-squared= 0.9526  \n p-value= 5.5e-04"))
