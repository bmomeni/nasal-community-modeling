#Last run 04/10/2020

#Clear Workspace 
rm(list=ls())

#################################################
#Create scatterplots for GR-CC relationships 
#################################################


####################################################################################################
#C. tuberculostearicum in BAAD media dilutions

GR_Ct <- c(1.5164,
           1.389425,
           1.276025,
           1.093125,
           0.870075,
           0.9651,
           1.01045,
           0.8175,
           0.46585)
Max_Ct <- c(1.206161765,
            1.039911765,
            0.824661765,
            0.450411765,
            0.407911765,
            0.313911765,
            0.202161765,
            0.166411765,
            0.152411765)

DF_Ct <- data.frame(GR_Ct, Max_Ct)
DF_Ct

#Generate a linear regression model of the two variables with the lm function 
Ct.lm <- lm(Max_Ct ~ GR_Ct + 1, data=DF_Ct)
Ct.lm2 <- lm(Max_Ct ~ 0 + GR_Ct, data=DF_Ct) #Tells the lm() to fit the line through the origin
summary(Ct.lm)
summary(Ct.lm2)

#Create scatter plot of points using plot function
plot(GR_Ct, Max_Ct,
     xlab="Growth Rate (1/hr.)",
     ylab="Carrying Capacity (Max OD600)",
     xlim= c(0, 1.6),
     ylim= c(0, 1.3),
     main="Growth Rate vs. Carrying Capacity for C. tuberculostearicum \n Grown in Glucose Concentrations",
     pch=16)

abline(lm(Ct.lm2))
legend("topleft", bty="n", legend= paste("R-squared= 0.845 \n p-value= 0.00011"))

#Create scatter plot of points using plot function - without graph labels 
plot(GR_Ct, Max_Ct,
     xlab= "Growth Rate (1/hr)",
     ylab= "Max OD600",
     xlim= c(0, 1.6),
     ylim= c(0, 1.3),
     cex= 1.7,
     pch=16)

abline(lm(Ct.lm2))
op <- par(cex= 1.5)
legend("topleft", bty="n", legend= paste("R-squared= 0.845 \n p-value= 0.00011"))

################################################################################################3
#S. aureus in BAAD media dilutions

GR_Sa <- c(0.81335,
           0.740675,
           0.7548,
           0.5476,
           0.517275,
           0.363366667,
           0,
           0.755233333,
           0)
Max_Sa <- c(0.679661765,
            0.607661765,
            0.456161765,
            0.061661765,
            0.081661765,
            0.063578431,
            0,
            0.028578431,
            0)

DF_Sa <- data.frame(GR_Sa, Max_Sa)
DF_Sa

#Generate a linear regression model of the two variables with the lm function 
Sa.lm <- lm(Max_Sa ~ GR_Sa + 1, data=DF_Sa)
Sa.lm2 <- lm(Max_Sa ~ 0 + GR_Sa, data=DF_Sa) #Tells the lm() to fit the line through the origin
summary(Sa.lm)
summary(Sa.lm2)

#Create scatter plot of points using plot function
plot(GR_Sa, Max_Sa,
     xlab="Growth Rate (1/hr.)",
     ylab="Carrying Capacity (Max OD600)",
     xlim= c(0, 0.9),
     ylim= c(0, 0.75),
     main="Growth Rate vs. Carrying Capacity for S. aureus \n Grown in Glucose Concentrations",
     pch=16)

abline(lm(Sa.lm2))
legend("topleft", bty="n", legend= paste("R-squared= 0.6287 \n p-value= 0.0038"))

#Create scatter plot of points using plot function - without graph labels 
plot(GR_Sa, Max_Sa,
     xlab= "Growth Rate (1/hr)",
     ylab= "Max OD600",
     xlim= c(0, 0.9),
     ylim= c(0, 0.75),
     cex= 1.7,
     pch=16)

abline(lm(Sa.lm2))
op <- par(cex= 1.5)
legend("topleft", bty="n", legend= paste("R-squared= 0.6287 \n p-value= 0.0038"))

##############################################################################################################
#S. non-aureus str. 1839

GR_Sna1839 <- c(0.599175,
                0.57435,
                0.29115,
                0.1218,
                0.16725,
                0.3196,
                0.253466667,
                0.49035,
                0.481)
Max_Sna1839 <- c(1.048161765,
                 0.138411765,
                 0.150661765,
                 0.044911765,
                 0.065161765,
                 0.117661765,
                 0.050578431,
                 0.024661765,
                 0.019911765)

DF_Sna1839 <- data.frame(GR_Sna1839, Max_Sna1839)
DF_Sna1839

#Generate a linear regression model of the two variables with the lm function 
Sna1839.lm <- lm(Max_Sna1839 ~ 1+GR_Sna1839, data=DF_Sna1839)
Sna1839.lm2 <- lm(Max_Sna1839 ~ 0+GR_Sna1839, data=DF_Sna1839) #Tells the lm() to fit the line through the origin
summary(Sna1839.lm)
summary(Sna1839.lm2)
#Adjusted R-squared: 0.3436 
#p-value: 0.0439

#Create scatter plot of points using plot funSna1839ion
plot(GR_Sna1839, Max_Sna1839,
     xlab="Growth Rate (1/hr.)",
     ylab="Carrying Capacity (Max OD600)",
     xlim= c(0, 0.65),
     ylim= c(0, 1.1),
     main="Growth Rate vs. Carrying Capacity for S. non-aureus str. 1839 \n Grown in THY Dilutions",
     pch=16)

abline(lm(Sna1839.lm2))
legend("topleft", bty="n", legend= paste("R-squared= 0.3436 \n p-value= 0.0439"))

#Create scatter plot of points using plot function - without graph labels 
plot(GR_Sna1839, Max_Sna1839,
     xlab= "Growth Rate (1/hr)",
     ylab= "Max OD600",
     xlim= c(0, 0.65),
     ylim= c(0, 1.1),
     cex= 1.7,
     pch=16)

abline(lm(Sna1839.lm2))
op <- par(cex= 1.5)
legend("topleft", bty="n", legend= paste("R-squared= 0.3436 \n p-value= 0.0439"))


#####################################################################################################################
#S. non-aureus str. 1850 

GR_Sna1850 <- c(0.822725,
                0.47875,
                0.595275,
                0.3188,
                0.197575,
                0.30275,
                0.415366667,
                0.00835,
                0.068225)
Max_Sna1850 <- c(1.034161765,
                 0.452911765,
                 0.236411765,
                 0.106161765,
                 0.066161765,
                 0.059411765,
                 0.032578431,
                 0.015661765,
                 0.018661765)

DF_Sna1850 <- data.frame(GR_Sna1850, Max_Sna1850)
DF_Sna1850

#Generate a linear regression model of the two variables with the lm function 
Sna1850.lm <- lm(Max_Sna1850 ~ 1+GR_Sna1850, data=DF_Sna1850)
Sna1850.lm2 <- lm(Max_Sna1850 ~ 0+GR_Sna1850, data=DF_Sna1850) #Tells the lm() to fit the line through the origin
summary(Sna1850.lm)
summary(Sna1850.lm2)
#Adjusted R-squared:  0.7023
#p-value: 0.001512

#Create scatter plot of points using plot funSna1850ion
plot(GR_Sna1850, Max_Sna1850,
     xlab="Growth Rate (1/hr.)",
     ylab="Carrying Capacity (Max OD600)",
     xlim= c(0, 0.85),
     ylim= c(0, 1.1),
     main="Growth Rate vs. Carrying Capacity for S. non-aureus str. 1850 \n Grown in THY Dilutions",
     pch=16)

abline(lm(Sna1850.lm2))
legend("topleft", bty="n", legend= paste("R-squared= 0.7023 \n p-value= 0.001512"))


#Create scatter plot of points using plot function - without graph labels 
plot(GR_Sna1850, Max_Sna1850,
     xlab= "Growth Rate (1/hr)",
     ylab= "Max OD600",
     xlim= c(0, 0.85),
     ylim= c(0, 1.1),
     cex= 1.7,
     pch=16)

abline(lm(Sna1850.lm2))
op <- par(cex= 1.5)
legend("topleft", bty="n", legend= paste("R-squared= 0.7023 \n p-value= 0.001512"))

################################################################################################################
#S. non-aureus str. 1867

GR_Sna1867 <- c(0.9857,
                0.6512,
                0.844475,
                0.285375,
                0.307625,
                0.3798,
                0.47725,
                0.33475,
                0.2827)
Max_Sna1867 <- c(1.131661765,
                 0.530911765,
                 0.347161765,
                 0.065911765,
                 0.089661765,
                 0.071411765,
                 0.076411765,
                 0.046411765,
                 0.022911765)

DF_Sna1867 <- data.frame(GR_Sna1867, Max_Sna1867)
DF_Sna1867

#Generate a linear regression model of the two variables with the lm function
Sna1867.lm <- lm(Max_Sna1867 ~ 1+GR_Sna1867, data=DF_Sna1867)
Sna1867.lm2 <- lm(Max_Sna1867 ~ 0+GR_Sna1867, data=DF_Sna1867) #Tells the lm() to fit the line through the origin
summary(Sna1867.lm)
summary(Sna1867.lm2)
#Adjusted R-squared:  0.6972 
#p-value: 0.00162

#Create scatter plot of points using plot funSna1867ion
plot(GR_Sna1867, Max_Sna1867,
     xlab="Growth Rate (1/hr.)",
     ylab="Carrying Capacity (Max OD600)",
     xlim= c(0, 1.0),
     ylim= c(0, 1.2),
     main="Growth Rate vs. Carrying Capacity for S. non-aureus str. 1867 \n Grown in THY Dilutions",
     pch=16)

abline(lm(Sna1867.lm2))
legend("topleft", bty="n", legend= paste("R-squared= 0.6972 \n p-value= 0.00162"))


#Create scatter plot of points using plot function - without graph labels 
plot(GR_Sna1867, Max_Sna1867,
     xlab= "Growth Rate (1/hr)",
     ylab= "Max OD600",
     xlim= c(0, 1.0),
     ylim= c(0, 1.2),
     cex= 1.7,
     pch=16)

abline(lm(Sna1867.lm2))
op <- par(cex= 1.5)
legend("topleft", bty="n", legend= paste("R-squared= 0.6972 \n p-value= 0.00162"))


#######################################################################################################################
####################################################################################################
#C. tuberculostearicum in BAAD media dilutions - 15% and lower

GR_Ct <- c(0.870075,
           0.9651,
           1.01045,
           0.8175,
           0.46585)
Max_Ct <- c(0.407911765,
            0.313911765,
            0.202161765,
            0.166411765,
            0.152411765)

DF_Ct <- data.frame(GR_Ct, Max_Ct)
DF_Ct

#Generate a linear regression model of the two variables with the lm function 
Ct.lm <- lm(Max_Ct ~ GR_Ct + 1, data=DF_Ct)
Ct.lm2 <- lm(Max_Ct ~ 0 + GR_Ct, data=DF_Ct) #Tells the lm() to fit the line through the origin
summary(Ct.lm)
summary(Ct.lm2)
#R-squared: 0.8643
#p-value: 0.004588

#Create scatter plot of points using plot function
plot(GR_Ct, Max_Ct,
     xlab="Growth Rate (1/hr.)",
     ylab="Carrying Capacity (Max OD600)",
     xlim= c(0, 1.1),
     ylim= c(0, 0.45),
     main="Growth Rate vs. Carrying Capacity for C. tuberculostearicum \n Grown in Glucose Concentrations",
     pch=16)

abline(lm(Ct.lm2))
legend("topleft", bty="n", legend= paste("R-squared= 0.8643 \n p-value= 0.004588"))

#Create scatter plot of points using plot function - without graph labels 
plot(GR_Ct, Max_Ct,
     xlab= "Growth Rate (1/hr)",
     ylab= "Max OD600",
     xlim= c(0, 1.1),
     ylim= c(0, 0.45),
     cex= 1.7,
     pch=16)

abline(lm(Ct.lm2))
op <- par(cex= 1.5)
legend("topleft", bty="n", legend= paste("R-squared= 0.8643 \n p-value= 0.004588"))

################################################################################################3
#S. aureus in BAAD media dilutions - 50% - 5%

GR_Sa <- c(0.7548,
           0.5476,
           0.517275,
           0.363366667,
           0)
Max_Sa <- c(0.456161765,
            0.061661765,
            0.081661765,
            0.063578431,
            0)

DF_Sa <- data.frame(GR_Sa, Max_Sa)
DF_Sa

#Generate a linear regression model of the two variables with the lm function 
Sa.lm <- lm(Max_Sa ~ GR_Sa + 1, data=DF_Sa)
Sa.lm2 <- lm(Max_Sa ~ 0 + GR_Sa, data=DF_Sa) #Tells the lm() to fit the line through the origin
summary(Sa.lm)
summary(Sa.lm2)
#R-squared: 0.620
#p-value: 0.03894

#Create scatter plot of points using plot function
plot(GR_Sa, Max_Sa,
     xlab="Growth Rate (1/hr.)",
     ylab="Carrying Capacity (Max OD600)",
     xlim= c(0, 0.8),
     ylim= c(0, 0.5),
     main="Growth Rate vs. Carrying Capacity for S. aureus \n Grown in Glucose Concentrations",
     pch=16)

abline(lm(Sa.lm2))
legend("topleft", bty="n", legend= paste("R-squared= 0.620 \n p-value= 0.03894"))

#Create scatter plot of points using plot function - without graph labels 
plot(GR_Sa, Max_Sa,
     xlab= "Growth Rate (1/hr)",
     ylab= "Max OD600",
     xlim= c(0, 0.8),
     ylim= c(0, 0.5),
     cex= 1.7,
     pch=16)

abline(lm(Sa.lm2))
op <- par(cex= 1.5)
legend("topleft", bty="n", legend= paste("R-squared= 0.620 \n p-value= 0.03894"))

##############################################################################################################
