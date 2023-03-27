#Last run 04/10/2020

#Clear Workspace 
rm(list=ls())

#################################################
#Create scatterplots for GR-CC relationships 
#################################################
#C. tuberculostearicum 

GR_Ct <- c(0.621375,
           0.598025,
           0.589875,
           0.57645,
           0.578625,
           0.563075)
Max_Ct <- c(0.367979167,
            0.330479167,
            0.339479167,
            0.329979167,
            0.299479167,
            0.273979167)

DF_Ct <- data.frame(GR_Ct, Max_Ct)
DF_Ct

#Generate a linear regression model of the two variables with the lm function 
Ct.lm <- lm(Max_Ct ~ GR_Ct + 1, data=DF_Ct)
Ct.lm2 <- lm(Max_Ct ~ 0 + GR_Ct, data=DF_Ct) #Tells the lm() to fit the line through the origin
summary(Ct.lm)
summary(Ct.lm2)
#Adjusted R-squared:  0.9949 
#p-value: 4.062e-07

#Colors
colors <- c("deepskyblue4", "darkorchid4", "darkolivegreen", "firebrick4", "darkorange3", "deeppink2")


#Create scatter plot of points using plot function
plot(GR_Ct, Max_Ct,
     xlab="Growth Rate (1/hr.)",
     ylab="Carrying Capacity (Max OD600)",
     xlim= c(0, 0.7),
     ylim= c(0, 0.4),
     main="Growth Rate vs. Carrying Capacity for C. tuberculostearicum \n Grown in THY Dilutions",
     pch=16)

abline(lm(Ct.lm2))
legend("topleft", bty="n", legend= paste("R-squared= 0.9949 \n p-value= 4.062e-07"))

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
legend("topleft", bty="n", legend= paste("R-squared= 0.9949 \n p-value= 4.062e-07"))


###############################################################################################################333                                        
#S. aureus 

GR_Sa <- c(2.131933333,
           1.656866667,
           1.6493,
           1.412566667,
           1.6239,
           1.4803)
Max_Sa <- c(0.517229167,
            0.418479167,
            0.443979167,
            0.389979167,
            0.362229167,
            0.340729167)

DF_Sa <- data.frame(GR_Sa, Max_Sa)
DF_Sa

#Generate a linear regression model of the two variables with the lm function
Sa.lm <- lm(Max_Sa ~ GR_Sa+1, data=DF_Sa)
Sa.lm2 <- lm(Max_Sa ~ 0+GR_Sa, data=DF_Sa) #Tells the lm() to fit the line through the origin
summary(Sa.lm)
summary(Sa.lm2)
#Adjusted R-squared: 0.9939 
#p-value: 6.379e-07

#Colors
colors <- c("deepskyblue4", "darkorchid4", "darkolivegreen", "firebrick4", "darkorange3", "deeppink2")

#Create scatter plot of points using plot funSaion
plot(GR_Sa, Max_Sa,
     xlab="Growth Rate (1/hr.)",
     ylab="Carrying Capacity (Max OD600)",
     xlim= c(0, 2.2),
     ylim= c(0, 0.6),
     main="Growth Rate vs. Carrying Capacity for S. aureus \n Grown in THY Dilutions",
     pch=16)

abline(lm(Sa.lm2))
legend("topleft", bty="n", legend= paste("R-squared= 0.9939 \n p-value= 6.379e-07"))

#Create scatter plot of points using plot function - without graph labels 
plot(GR_Sa, Max_Sa,
     xlab= "Growth Rate (1/hr)",
     ylab= "Max OD600",
     xlim= c(0, 2.2),
     ylim= c(0, 0.6),
     cex= 1.8,
     col = colors,
     pch=16)

abline(lm(Sa.lm2))
op <- par(cex= 1.5)
legend("topleft", bty="n", legend= paste("R-squared= 0.9939 \n p-value= 6.379e-07"))

##############################################################################################################
#S. non-aureus str. 1839

GR_Sna1839 <- c(1.27775,
                1.112225,
                0.95585,
                0.861125,
                0.888133333,
                0.77285)
Max_Sna1839 <- c(0.191729167,
                 0.679729167,
                 0.685729167,
                 0.745229167,
                 0.634979167,
                 0.550729167)

DF_Sna1839 <- data.frame(GR_Sna1839, Max_Sna1839)
DF_Sna1839

#Generate a linear regression model of the two variables with the lm function 
Sna1839.lm <- lm(Max_Sna1839 ~ 1+GR_Sna1839, data=DF_Sna1839)
Sna1839.lm2 <- lm(Max_Sna1839 ~ 0+GR_Sna1839, data=DF_Sna1839) #Tells the lm() to fit the line through the origin
summary(Sna1839.lm)
summary(Sna1839.lm2)
#Adjusted R-squared:  0.7837 
#p-value: 0.005023

#Colors
colors <- c("deepskyblue4", "darkorchid4", "darkolivegreen", "firebrick4", "darkorange3", "deeppink2")

#Create scatter plot of points using plot funSna1839ion
plot(GR_Sna1839, Max_Sna1839,
     xlab="Growth Rate (1/hr.)",
     ylab="Carrying Capacity (Max OD600)",
     xlim= c(0, 1.3),
     ylim= c(0, 0.8),
     main="Growth Rate vs. Carrying Capacity for S. non-aureus str. 1839 \n Grown in THY Dilutions",
     pch=16)

abline(lm(Sna1839.lm2))
legend("topleft", bty="n", legend= paste("R-squared= 0.7837 \n p-value= 0.005023"))

#Create scatter plot of points using plot function - without graph labels 
plot(GR_Sna1839, Max_Sna1839,
     xlab= "Growth Rate (1/hr)",
     ylab= "Max OD600",
     xlim= c(0, 1.3),
     ylim= c(0, 0.8),
     cex= 1.8,
     col = colors,
     pch=16)

abline(lm(Sna1839.lm2))
op <- par(cex= 1.5)
legend("topleft", bty="n", legend= paste("R-squared= 0.7837 \n p-value= 0.005023"))


#####################################################################################################################
#S. non-aureus str. 1850 

GR_Sna1850 <- c(1.3048,
                1.11235,
                0.962775,
                0.953375,
                0.965866667,
                0.809775)
Max_Sna1850 <- c(0.215729167,
                 0.644979167,
                 0.706479167,
                 0.716229167,
                 0.632979167,
                 0.4563125)

DF_Sna1850 <- data.frame(GR_Sna1850, Max_Sna1850)
DF_Sna1850

#Generate a linear regression model of the two variables with the lm function 
Sna1850.lm <- lm(Max_Sna1850 ~ 1+GR_Sna1850, data=DF_Sna1850)
Sna1850.lm2 <- lm(Max_Sna1850 ~ 0+GR_Sna1850, data=DF_Sna1850) #Tells the lm() to fit the line through the origin
summary(Sna1850.lm)
summary(Sna1850.lm2)
#Adjusted R-squared:  0.8113
#p-value: 0.003538

#Colors
colors <- c("deepskyblue4", "darkorchid4", "darkolivegreen", "firebrick4", "darkorange3", "deeppink2")

#Create scatter plot of points using plot funSna1850ion
plot(GR_Sna1850, Max_Sna1850,
     xlab="Growth Rate (1/hr.)",
     ylab="Carrying Capacity (Max OD600)",
     xlim= c(0, 1.4),
     ylim= c(0, 0.75),
     main="Growth Rate vs. Carrying Capacity for S. non-aureus str. 1850 \n Grown in THY Dilutions",
     pch=16)

abline(lm(Sna1850.lm2))
legend("topleft", bty="n", legend= paste("R-squared= 0.8113 \n p-value= 0.003538"))


#Create scatter plot of points using plot function - without graph labels 
plot(GR_Sna1850, Max_Sna1850,
     xlab= "Growth Rate (1/hr)",
     ylab= "Max OD600",
     xlim= c(0, 1.4),
     ylim= c(0, 0.75),
     cex= 1.8,
     col = colors,
     pch=16)

abline(lm(Sna1850.lm2))
op <- par(cex= 1.5)
legend("topleft", bty="n", legend= paste("R-squared= 0.8113 \n p-value= 0.003538"))

################################################################################################################
#S. non-aureus str. 1867

GR_Sna1867 <- c(1.334075,
                1.07935,
                0.970075,
                0.948075,
                0.80415,
                0.701825)
Max_Sna1867 <- c(0.199979167,
                 0.683979167,
                 0.782229167,
                 0.791729167,
                 0.753729167,
                 0.560229167)

DF_Sna1867 <- data.frame(GR_Sna1867, Max_Sna1867)
DF_Sna1867

#Generate a linear regression model of the two variables with the lm function
Sna1867.lm <- lm(Max_Sna1867 ~ 1+GR_Sna1867, data=DF_Sna1867)
Sna1867.lm2 <- lm(Max_Sna1867 ~ 0+GR_Sna1867, data=DF_Sna1867) #Tells the lm() to fit the line through the origin
summary(Sna1867.lm)
summary(Sna1867.lm2)
#Adjusted R-squared:  0.7515 
#p-value: 0.007184

#Colors
colors <- c("deepskyblue4", "darkorchid4", "darkolivegreen", "firebrick4", "darkorange3", "deeppink2")

#Create scatter plot of points using plot funSna1867ion
plot(GR_Sna1867, Max_Sna1867,
     xlab="Growth Rate (1/hr.)",
     ylab="Carrying Capacity (Max OD600)",
     xlim= c(0, 1.4),
     ylim= c(0, 0.8),
     main="Growth Rate vs. Carrying Capacity for S. non-aureus str. 1867 \n Grown in THY Dilutions",
     pch=16)

abline(lm(Sna1867.lm2))
legend("topleft", bty="n", legend= paste("R-squared= 0.8313 \n p-value= 0.002655"))


#Create scatter plot of points using plot function - without graph labels 
plot(GR_Sna1867, Max_Sna1867,
     xlab= "Growth Rate (1/hr)",
     ylab= "Max OD600",
     xlim= c(0, 1.4),
     ylim= c(0, 0.8),
     cex= 1.8,
     col = colors,
     pch=16)

abline(lm(Sna1867.lm2))
op <- par(cex= 1.5)
legend("topleft", bty="n", legend= paste("R-squared= 0.8313 \n p-value= 0.002655"))


#######################################################################################################################
#C. pseudodiphtheriticum 

GR_Cp <- c(0,
           0.147025,
           0.193375,
           0.18615,
           0.193925,
           0.17315)
Max_Cp <- c(0.009729167,
            0.113229167,
            0.112479167,
            0.110979167,
            0.096979167,
            0.085729167)

DF_Cp <- data.frame(GR_Cp, Max_Cp)
DF_Cp

#Generate a linear regression model of the two variables with the lm function 
Cp.lm <- lm(Max_Cp ~ 1+GR_Cp, data=DF_Cp)
Cp.lm2 <- lm(Max_Cp ~ 0+GR_Cp, data=DF_Cp) #Tells the lm() to fit the line through the origin
summary(Cp.lm)
summary(Cp.lm2)
#Adjusted R-squared:  0.9706
#p-value: 3.211e-05

#Colors
colors <- c("deepskyblue4", "darkorchid4", "darkolivegreen", "firebrick4", "darkorange3", "deeppink2")

#Create scatter plot of points using plot funCpion
plot(GR_Cp, Max_Cp,
     xlab="Growth Rate (1/hr.)",
     ylab="Carrying Capacity (Max OD600)",
     xlim= c(0, 0.2),
     ylim= c(0, 0.2),
     cex= 1.5,
     main="Growth Rate vs. Carrying Capacity for C. pseudodiphtheriticum \n Grown in THY Dilutions",
     pch=16)

abline(lm(Cp.lm2))
legend("topleft", bty="n", legend= paste("R-squared= 0.9706 \n p-value= 3.211e-05"))

#Create scatter plot of points using plot function - without graph labels 
plot(GR_Cp, Max_Cp,
     xlab= "Growth Rate (1/hr)",
     ylab= "Max OD600",
     xlim= c(0, 0.2),
     ylim= c(0, 0.2),
     cex= 1.8,
     col = colors,
     pch=16)

abline(lm(Cp.lm2))
op <- par(cex= 1.5)
legend("topleft", bty="n", legend= paste("R-squared= 0.9706 \n p-value= 3.211e-05"))