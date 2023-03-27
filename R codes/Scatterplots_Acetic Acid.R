#Last run 04/10/2020

#Clear Workspace 
rm(list=ls())

#################################################
#Create scatterplots for GR-CC relationships 
#################################################
#C. tuberculostearicum 

GR_Ct <- c(0.610825,
           0.597,
           0.6611,
           0.59535,
           0.5841,
           0.61155)
Max_Ct <- c(0.369979167,
            0.315979167,
            0.320479167,
            0.314229167,
            0.280979167,
            0.272479167)

DF_Ct <- data.frame(GR_Ct, Max_Ct)
DF_Ct

#Colors
colors <- c("deepskyblue4", "darkorchid4", "darkolivegreen", "firebrick4", "darkorange3", "deeppink2")

#Generate a linear regression model of the two variables with the lm function 
Ct.lm <- lm(Max_Ct ~ GR_Ct + 1, data=DF_Ct)
Ct.lm2 <- lm(Max_Ct ~ 0 + GR_Ct, data=DF_Ct) #Tells the lm() to fit the line through the origin
summary(Ct.lm)
summary(Ct.lm2)
#Adjusted R-squared:  0.9883 
#p-value: 3.208e-06

#Create scatter plot of points using plot function
plot(GR_Ct, Max_Ct,
     xlab="Growth Rate (1/hr.)",
     ylab="Carrying Capacity (Max OD600)",
     xlim= c(0, 0.7),
     ylim= c(0, 0.4),
     col = colors,
     main="Growth Rate vs. Carrying Capacity for C. tuberculostearicum \n Grown in THY Dilutions",
     pch=16)

abline(lm(Ct.lm2))
legend("topleft", bty="n", legend= paste("R-squared= 0.9883 \n p-value= 3.208e-06"))

#Create scatter plot of points using plot function - without graph labels 
plot(GR_Ct, Max_Ct,
     xlab= "Growth Rate (1/hr)",
     ylab= "Max OD600",
     xlim= c(0, 0.7),
     ylim= c(0, 0.4),
     cex= 1.8,
     col = colors,
     pch=16)

abline(lm(Ct.lm2))
op <- par(cex= 1.5)
legend("topleft", bty="n", legend= paste("R-squared= 0.9883 \n p-value= 3.208e-06"))


###############################################################################################################333                                        
#S. aureus 

GR_Sa <- c(1.991433333,
           1.4142,
           1.24905,
           1.14605,
           1.11585,
           0.674725)
Max_Sa <- c(0.411979167,
            0.330979167,
            0.364979167,
            0.299479167,
            0.253479167,
            0.143729167)

DF_Sa <- data.frame(GR_Sa, Max_Sa)
DF_Sa

#Generate a linear regression model of the two variables with the lm function
Sa.lm <- lm(Max_Sa ~ GR_Sa+1, data=DF_Sa)
Sa.lm2 <- lm(Max_Sa ~ 0+GR_Sa, data=DF_Sa) #Tells the lm() to fit the line through the origin
summary(Sa.lm)
summary(Sa.lm2)
#Adjusted R-squared: 0.9807 
#p-value: 1.12e-05

#Colors
colors <- c("deepskyblue4", "darkorchid4", "darkolivegreen", "firebrick4", "darkorange3", "deeppink2")

#Create scatter plot of points using plot funSaion
plot(GR_Sa, Max_Sa,
     xlab="Growth Rate (1/hr.)",
     ylab="Carrying Capacity (Max OD600)",
     xlim= c(0, 2.2),
     ylim= c(0, 0.5),
     col = colors,
     main="Growth Rate vs. Carrying Capacity for S. aureus \n Grown in THY Dilutions",
     pch=16)

abline(lm(Sa.lm2))
legend("topleft", bty="n", legend= paste("R-squared= 0.9807 \n p-value= 1.12e-05"))

#Create scatter plot of points using plot function - without graph labels 
plot(GR_Sa, Max_Sa,
     xlab= "Growth Rate (1/hr)",
     ylab= "Max OD600",
     xlim= c(0, 2.2),
     ylim= c(0, 0.5),
     cex= 1.8,
     col = colors,
     pch=16)

abline(lm(Sa.lm2))
op <- par(cex= 1.5)
legend("topleft", bty="n", legend= paste("R-squared= 0.9807 \n p-value= 1.12e-05"))

##############################################################################################################
#S. non-aureus str. 1839

GR_Sna1839 <- c(1.277475,
                1.126,
                0.904275,
                0.937575,
                0.9361,
                0.895425)
Max_Sna1839 <- c(0.190729167,
                 0.404729167,
                 0.467729167,
                 0.435229167,
                 0.415229167,
                 0.398229167)

DF_Sna1839 <- data.frame(GR_Sna1839, Max_Sna1839)
DF_Sna1839

#Generate a linear regression model of the two variables with the lm function 
Sna1839.lm <- lm(Max_Sna1839 ~ 1+GR_Sna1839, data=DF_Sna1839)
Sna1839.lm2 <- lm(Max_Sna1839 ~ 0+GR_Sna1839, data=DF_Sna1839) #Tells the lm() to fit the line through the origin
summary(Sna1839.lm)
summary(Sna1839.lm2)
#Adjusted R-squared:  0.8543 
#p-value: 0.001825

#Colors
colors <- c("deepskyblue4", "darkorchid4", "darkolivegreen", "firebrick4", "darkorange3", "deeppink2")

#Create scatter plot of points using plot funSna1839ion
plot(GR_Sna1839, Max_Sna1839,
     xlab="Growth Rate (1/hr.)",
     ylab="Carrying Capacity (Max OD600)",
     xlim= c(0, 1.3),
     ylim= c(0, 0.5),
     col = colors,
     main="Growth Rate vs. Carrying Capacity for S. non-aureus str. 1839 \n Grown in THY Dilutions",
     pch=16)

abline(lm(Sna1839.lm2))
legend("topleft", bty="n", legend= paste("R-squared= 0.8543 \n p-value= 0.001825"))

#Create scatter plot of points using plot function - without graph labels 
plot(GR_Sna1839, Max_Sna1839,
     xlab= "Growth Rate (1/hr)",
     ylab= "Max OD600",
     xlim= c(0, 1.3),
     ylim= c(0, 0.5),
     cex= 1.8,
     col = colors,
     pch=16)

abline(lm(Sna1839.lm2))
op <- par(cex= 1.5)
legend("topleft", bty="n", legend= paste("R-squared= 0.8543 \n p-value= 0.001825"))


#####################################################################################################################
#S. non-aureus str. 1850 

GR_Sna1850 <- c(1.322525,
                1.0985,
                1.008125,
                0.983425,
                0.99785,
                0.9861)
Max_Sna1850 <- c(0.259229167,
                 0.432479167,
                 0.470979167,
                 0.435979167,
                 0.427729167,
                 0.422729167)

DF_Sna1850 <- data.frame(GR_Sna1850, Max_Sna1850)
DF_Sna1850

#Generate a linear regression model of the two variables with the lm function 
Sna1850.lm <- lm(Max_Sna1850 ~ 1+GR_Sna1850, data=DF_Sna1850)
Sna1850.lm2 <- lm(Max_Sna1850 ~ 0+GR_Sna1850, data=DF_Sna1850) #Tells the lm() to fit the line through the origin
summary(Sna1850.lm)
summary(Sna1850.lm2)
#Adjusted R-squared:  0.9122
#p-value: 5.05e-04

#Colors
colors <- c("deepskyblue4", "darkorchid4", "darkolivegreen", "firebrick4", "darkorange3", "deeppink2")

#Create scatter plot of points using plot funSna1850ion
plot(GR_Sna1850, Max_Sna1850,
     xlab="Growth Rate (1/hr.)",
     ylab="Carrying Capacity (Max OD600)",
     xlim= c(0, 1.4),
     ylim= c(0, 0.5),
     col = colors,
     main="Growth Rate vs. Carrying Capacity for S. non-aureus str. 1850 \n Grown in THY Dilutions",
     pch=16)

abline(lm(Sna1850.lm2))
legend("topleft", bty="n", legend= paste("R-squared= 0.9122 \n p-value= 5.05e-04"))


#Create scatter plot of points using plot function - without graph labels 
plot(GR_Sna1850, Max_Sna1850,
     xlab= "Growth Rate (1/hr)",
     ylab= "Max OD600",
     xlim= c(0, 1.4),
     ylim= c(0, 0.5),
     cex= 1.8,
     col = colors,
     pch=16)

abline(lm(Sna1850.lm2))
op <- par(cex= 1.5)
legend("topleft", bty="n", legend= paste("R-squared= 0.9122 \n p-value= 5.05e-04"))

################################################################################################################
#S. non-aureus str. 1867

GR_Sna1867 <- c(1.327066667,
                0.99385,
                0.738125,
                0.8457,
                0.90065,
                0.9121)
Max_Sna1867 <- c(0.239729167,
                 0.467979167,
                 0.487729167,
                 0.433979167,
                 0.512979167,
                 0.421979167)

DF_Sna1867 <- data.frame(GR_Sna1867, Max_Sna1867)
DF_Sna1867

#Generate a linear regression model of the two variables with the lm function
Sna1867.lm <- lm(Max_Sna1867 ~ 1+GR_Sna1867, data=DF_Sna1867)
Sna1867.lm2 <- lm(Max_Sna1867 ~ 0+GR_Sna1867, data=DF_Sna1867) #Tells the lm() to fit the line through the origin
summary(Sna1867.lm)
summary(Sna1867.lm2)
#Adjusted R-squared:  0.8313 
#p-value: 0.002655

#Colors
colors <- c("deepskyblue4", "darkorchid4", "darkolivegreen", "firebrick4", "darkorange3", "deeppink2")

#Create scatter plot of points using plot funSna1867ion
plot(GR_Sna1867, Max_Sna1867,
     xlab="Growth Rate (1/hr.)",
     ylab="Carrying Capacity (Max OD600)",
     xlim= c(0, 1.4),
     ylim= c(0, 0.55),
     col = colors,
     main="Growth Rate vs. Carrying Capacity for S. non-aureus str. 1867 \n Grown in THY Dilutions",
     pch=16)

abline(lm(Sna1867.lm2))
legend("topleft", bty="n", legend= paste("R-squared= 0.8313 \n p-value= 0.002655"))


#Create scatter plot of points using plot function - without graph labels 
plot(GR_Sna1867, Max_Sna1867,
     xlab= "Growth Rate (1/hr)",
     ylab= "Max OD600",
     xlim= c(0, 1.4),
     ylim= c(0, 0.55),
     cex= 1.8,
     col = colors,
     pch=16)

abline(lm(Sna1867.lm2))
op <- par(cex= 1.5)
legend("topleft", bty="n", legend= paste("R-squared= 0.8313 \n p-value= 0.002655"))


#######################################################################################################################
#C. pseudodiphtheriticum 

GR_Cp <- c(0,
           0.117525,
           0.048375,
           0.080225,
           0.0477,
           0)
Max_Cp <- c(0.008979167,
            0.121229167,
            0.036729167,
            0.076229167,
            0.038729167,
            0.004729167)

DF_Cp <- data.frame(GR_Cp, Max_Cp)
DF_Cp

#Generate a linear regression model of the two variables with the lm function 
Cp.lm <- lm(Max_Cp ~ 1+GR_Cp, data=DF_Cp)
Cp.lm2 <- lm(Max_Cp ~ 0+GR_Cp, data=DF_Cp) #Tells the lm() to fit the line through the origin
summary(Cp.lm)
summary(Cp.lm2)
#Adjusted R-squared:  0.9837
#p-value: 7.292e-06

#Colors
colors <- c("deepskyblue4", "darkorchid4", "darkolivegreen", "firebrick4", "darkorange3", "deeppink2")

#Create scatter plot of points using plot funCpion
plot(GR_Cp, Max_Cp,
     xlab="Growth Rate (1/hr.)",
     ylab="Carrying Capacity (Max OD600)",
     xlim= c(0, 0.15),
     ylim= c(0, 0.15),
     cex= 1.5,
     col = colors,
     main="Growth Rate vs. Carrying Capacity for C. pseudodiphtheriticum \n Grown in THY Dilutions",
     pch=16)

abline(lm(Cp.lm2))
legend("topleft", bty="n", legend= paste("R-squared= 0.9837 \n p-value= 7.292e-06"))

#Create scatter plot of points using plot function - without graph labels 
plot(GR_Cp, Max_Cp,
     xlab= "Growth Rate (1/hr)",
     ylab= "Max OD600",
     xlim= c(0, 0.15),
     ylim= c(0, 0.15),
     cex= 1.8,
     col = colors,
     pch=16)

abline(lm(Cp.lm2))
op <- par(cex= 1.5)
legend("topleft", bty="n", legend= paste("R-squared= 0.9837 \n p-value= 7.292e-06"))