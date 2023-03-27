#Last run 04/10/2020

#Clear Workspace 
rm(list=ls())

#################################################
#Create scatterplots for GR-CC relationships 
#################################################
#C. tuberculostearicum

GR_Ct <- c(0.5779,
           0.597,
           0.5827,
           0.5128,
           0.485125,
           0.436725,
           0.4704,
           0.438633333,
           0)
Max_Ct <- c(0.36875,
            0.34625,
            0.33325,
            0.32675,
            0.297,
            0.2885,
            0.3045,
            0.135,
            0.012)

DF_Ct <- data.frame(GR_Ct, Max_Ct)
DF_Ct

#Generate a linear regression model of the two variables with the lm function
Ct.lm <- lm(Max_Ct ~ GR_Ct + 1, data=DF_Ct)
Ct.lm2 <- lm(Max_Ct ~ 0 + GR_Ct, data=DF_Ct) #Tells the lm() to fit the line through the origin
summary(Ct.lm)
summary(Ct.lm2)
#Adjusted R-squared:  0.9721
#p-value: 1.046e-07

#Create scatter plot of points using plot function
plot(GR_Ct, Max_Ct,
     xlab="Growth Rate (1/hr.)",
     ylab="Carrying Capacity (Max OD600)",
     xlim= c(0, 0.6),
     ylim= c(0, 0.4),
     main="Growth Rate vs. Carrying Capacity for C. tuberculostearicum \n Grown in THY Dilutions",
     pch=16)

abline(lm(Ct.lm2))
legend("topleft", bty="n", legend= paste("R-squared= 0.9721 \n p-value= 1.046e-07"))

#Create scatter plot of points using plot function - without graph labels
plot(GR_Ct, Max_Ct,
     xlab= "Growth Rate (1/hr)",
     ylab= "Max OD600",
     xlim= c(0, 0.6),
     ylim= c(0, 0.4),
     cex= 1.8,
     pch=16)

abline(lm(Ct.lm2))
op <- par(cex= 1.5)
legend("topleft", bty="n", legend= paste("R-squared= 0.9721 \n p-value= 1.046e-07"))


###############################################################################################################333                                        
#S. aureus 

GR_Sa <- c(1.954625,
           0.771,
           0.76595,
           0,
           0,
           0,
           0,
           0,
           0)
Max_Sa <- c(0.43175,
            0.41,
            0.22625,
            0.00575,
            0.00675,
            0.007,
            0.00375,
            0.00875,
            0.005)

DF_Sa <- data.frame(GR_Sa, Max_Sa)
DF_Sa

#Generate a linear regression model of the two variables with the lm function
Sa.lm <- lm(Max_Sa ~ GR_Sa+1, data=DF_Sa)
Sa.lm2 <- lm(Max_Sa ~ 0+GR_Sa, data=DF_Sa) #Tells the lm() to fit the line through the origin
summary(Sa.lm)
summary(Sa.lm2)
#Adjusted R-squared: 0.860 
#p-value: 6.91e-05

#Create scatter plot of points using plot funSaion
plot(GR_Sa, Max_Sa,
     xlab="Growth Rate (1/hr.)",
     ylab="Carrying Capacity (Max OD600)",
     xlim= c(0, 2.2),
     ylim= c(0, 0.5),
     main="Growth Rate vs. Carrying Capacity for S. aureus \n Grown in THY Dilutions",
     pch=16)

abline(lm(Sa.lm2))
legend("topleft", bty="n", legend= paste("R-squared= 0.860 \n p-value= 6.91e-05"))

#Create scatter plot of points using plot function - without graph labels 
plot(GR_Sa, Max_Sa,
     xlab= "Growth Rate (1/hr)",
     ylab= "Max OD600",
     xlim= c(0, 2.2),
     ylim= c(0, 0.5),
     cex= 1.8,
     pch=16)

abline(lm(Sa.lm2))
op <- par(cex= 1.5)
legend("topleft", bty="n", legend= paste("R-squared= 0.860 \n p-value= 6.91e-05"))

##############################################################################################################
#S. non-aureus str. 1839

GR_Sna1839 <- c(1.196475,
                0.9139,
                0.3139,
                0,
                0,
                0,
                0,
                0,
                0)
Max_Sna1839 <- c(0.161,
                 0.132,
                 0.13225,
                 0.0045,
                 0.00625,
                 0.0055,
                 0.00325,
                 0.00425,
                 0.0055)

DF_Sna1839 <- data.frame(GR_Sna1839, Max_Sna1839)
DF_Sna1839

#Generate a linear regression model of the two variables with the lm function 
Sna1839.lm <- lm(Max_Sna1839 ~ 1+GR_Sna1839, data=DF_Sna1839)
Sna1839.lm2 <- lm(Max_Sna1839 ~ 0+GR_Sna1839, data=DF_Sna1839) #Tells the lm() to fit the line through the origin
summary(Sna1839.lm)
summary(Sna1839.lm2)
#Adjusted R-squared:  0.8567
#p-value: 7.6e-05

#Create scatter plot of points using plot funSna1839ion
plot(GR_Sna1839, Max_Sna1839,
     xlab="Growth Rate (1/hr.)",
     ylab="Carrying Capacity (Max OD600)",
     xlim= c(0, 1.4),
     ylim= c(0, 0.25),
     main="Growth Rate vs. Carrying Capacity for S. non-aureus str. 1839 \n Grown in THY Dilutions",
     pch=16)

abline(lm(Sna1839.lm2))
legend("topleft", bty="n", legend= paste("R-squared= 0.8567 \n p-value= 7.6e-05"))

#Create scatter plot of points using plot function - without graph labels 
plot(GR_Sna1839, Max_Sna1839,
     xlab= "Growth Rate (1/hr)",
     ylab= "Max OD600",
     xlim= c(0, 1.4),
     ylim= c(0, 0.25),
     cex= 1.8,
     pch=16)

abline(lm(Sna1839.lm2))
op <- par(cex= 1.5)
legend("topleft", bty="n", legend= paste("R-squared= 0.8567 \n p-value= 7.6e-05"))


#####################################################################################################################
#S. non-aureus str. 1850 

GR_Sna1850 <- c(1.2302,
                0.501875,
                0,
                0,
                0,
                0,
                0,
                0,
                0)
Max_Sna1850 <- c(0.1875,
                 0.18525,
                 0.004,
                 0.00475,
                 0.007,
                 0.00675,
                 0.00575,
                 0.00625,
                 0.0055)

DF_Sna1850 <- data.frame(GR_Sna1850, Max_Sna1850)
DF_Sna1850

#Generate a linear regression model of the two variables with the lm function 
Sna1850.lm <- lm(Max_Sna1850 ~ 1+GR_Sna1850, data=DF_Sna1850)
Sna1850.lm2 <- lm(Max_Sna1850 ~ 0+GR_Sna1850, data=DF_Sna1850) #Tells the lm() to fit the line through the origin
summary(Sna1850.lm)
summary(Sna1850.lm2)
#Adjusted R-squared:  0.8325
#p-value: 1.43e-04

#Create scatter plot of points using plot funSna1850ion
plot(GR_Sna1850, Max_Sna1850,
     xlab="Growth Rate (1/hr.)",
     ylab="Carrying Capacity (Max OD600)",
     xlim= c(0, 1.3),
     ylim= c(0, 0.2),
     main="Growth Rate vs. Carrying Capacity for S. non-aureus str. 1850 \n Grown in THY Dilutions",
     pch=16)

abline(lm(Sna1850.lm2))
legend("topleft", bty="n", legend= paste("R-squared= 0.8325 \n p-value= 1.43e-04"))


#Create scatter plot of points using plot function - without graph labels 
plot(GR_Sna1850, Max_Sna1850,
     xlab= "Growth Rate (1/hr)",
     ylab= "Max OD600",
     xlim= c(0, 1.3),
     ylim= c(0, 0.2),
     cex= 1.8,
     pch=16)

abline(lm(Sna1850.lm2))
op <- par(cex= 1.5)
legend("topleft", bty="n", legend= paste("R-squared= 0.8325 \n p-value= 1.43e-04"))

################################################################################################################
#S. non-aureus str. 1867

GR_Sna1867 <- c(1.2241,
                0.909775,
                0.414125,
                0,
                0,
                0,
                0,
                0,
                0)
Max_Sna1867 <- c(0.1825,
                 0.16,
                 0.13325,
                 0.006,
                 0.0095,
                 0.00875,
                 0.0055,
                 0.00575,
                 0.00625)

DF_Sna1867 <- data.frame(GR_Sna1867, Max_Sna1867)
DF_Sna1867

#Generate a linear regression model of the two variables with the lm function
Sna1867.lm <- lm(Max_Sna1867 ~ 1+GR_Sna1867, data=DF_Sna1867)
Sna1867.lm2 <- lm(Max_Sna1867 ~ 0+GR_Sna1867, data=DF_Sna1867) #Tells the lm() to fit the line through the origin
summary(Sna1867.lm)
summary(Sna1867.lm2)
#Adjusted R-squared:  0.9278 
#p-value: 4.76e-06

#Create scatter plot of points using plot funSna1867ion
plot(GR_Sna1867, Max_Sna1867,
     xlab="Growth Rate (1/hr.)",
     ylab="Carrying Capacity (Max OD600)",
     xlim= c(0, 1.3),
     ylim= c(0, 0.2),
     main="Growth Rate vs. Carrying Capacity for S. non-aureus str. 1867 \n Grown in THY Dilutions",
     pch=16)

abline(lm(Sna1867.lm2))
legend("topleft", bty="n", legend= paste("R-squared= 0.9278 \n p-value= 4.76e-06"))


#Create scatter plot of points using plot function - without graph labels 
plot(GR_Sna1867, Max_Sna1867,
     xlab= "Growth Rate (1/hr)",
     ylab= "Max OD600",
     xlim= c(0, 1.3),
     ylim= c(0, 0.2),
     cex= 1.8,
     pch=16)

abline(lm(Sna1867.lm2))
op <- par(cex= 1.5)
legend("topleft", bty="n", legend= paste("R-squared= 0.9278 \n p-value= 4.76e-06"))


#######################################################################################################################
#C. pseudodiphtheriticum

GR_Cp <- c(0,
           0.0849,
           0.06125,
           0.2752,
           0.4951,
           0.4856,
           0.34105,
           0.227625,
           0.30045)
Max_Cp <- c(0.004666667,
            0.028916667,
            0.089666667,
            0.113166667,
            0.180666667,
            0.163416667,
            0.105166667,
            0.130916667,
            0.163416667)

DF_Cp <- data.frame(GR_Cp, Max_Cp)
DF_Cp

#Generate a linear regression model of the two variables with the lm function
Cp.lm <- lm(Max_Cp ~ 1+GR_Cp, data=DF_Cp)
Cp.lm2 <- lm(Max_Cp ~ 0+GR_Cp, data=DF_Cp) #Tells the lm() to fit the line through the origin
summary(Cp.lm)
summary(Cp.lm2)
#Adjusted R-squared:  0.9179
#p-value: 7.982e-06

#Create scatter plot of points using plot funCpion
plot(GR_Cp, Max_Cp,
     xlab="Growth Rate (1/hr.)",
     ylab="Carrying Capacity (Max OD600)",
     xlim= c(0, 0.55),
     ylim= c(0, 0.2),
     cex= 1.5,
     main="Growth Rate vs. Carrying Capacity for C. pseudodiphtheriticum \n Grown in THY Dilutions",
     pch=16)

abline(lm(Cp.lm2))
legend("topleft", bty="n", legend= paste("R-squared= 0.9179 \n p-value= 7.982e-06"))

#Create scatter plot of points using plot function - without graph labels
plot(GR_Cp, Max_Cp,
     xlab= "Growth Rate (1/hr)",
     ylab= "Max OD600",
     xlim= c(0, 0.55),
     ylim= c(0, 0.2),
     cex= 1.8,
     pch=16)

abline(lm(Cp.lm2))
op <- par(cex= 1.5)
# legend("topleft", bty="n", legend= paste("R-squared= 0.9179 \n p-value= 7.982e-06"))