#Last run 04/10/2020

#Clear Workspace 
rm(list=ls())

#################################################
#Create scatterplots for GR-CC relationships 
#################################################
#C. tuberculostearicum

GR_Ct <- c(0.036225,
           0.0778,
           0.099575,
           0.246925,
           0.016175,
           0.175225)
Max_Ct <- c(0.023958333,
            0.051208333,
            0.040208333,
            0.110208333,
            0.024458333,
            0.041458333)

DF_Ct <- data.frame(GR_Ct, Max_Ct)
DF_Ct

#Generate a linear regression model of the two variables with the lm function
Ct.lm <- lm(Max_Ct ~ GR_Ct + 1, data=DF_Ct)
Ct.lm2 <- lm(Max_Ct ~ 0 + GR_Ct, data=DF_Ct) #Tells the lm() to fit the line through the origin
summary(Ct.lm)
summary(Ct.lm2)
#Adjusted R-squared:  0.890
#p-value: 8.94e-04

#Create scatter plot of points using plot function
plot(GR_Ct, Max_Ct,
     xlab="Growth Rate (1/hr.)",
     ylab="Carrying Capacity (Max OD600)",
     xlim= c(0, 0.2),
     ylim= c(0, 0.2),
     main="Growth Rate vs. Carrying Capacity for C. tuberculostearicum \n Grown in THY Dilutions",
     pch=16)

abline(lm(Ct.lm2))
legend("topleft", bty="n", legend= paste("R-squared= 0.890 \n p-value= 8.94e-04"))

#Create scatter plot of points using plot function - without graph labels
plot(GR_Ct, Max_Ct,
     xlab= "Growth Rate (1/hr)",
     ylab= "Max OD600",
     xlim= c(0, 0.2),
     ylim= c(0, 0.2),
     cex= 1.9,
     pch=16)

abline(lm(Ct.lm2))
op <- par(cex= 1.5)
legend("topleft", bty="n", legend= paste("R-squared= 0.890 \n p-value= 8.94e-04"))


###############################################################################################################333                                        
#S. aureus 

GR_Sa <- c(0.095,
           0.1546,
           0.196,
           0.337833333,
           0.286675,
           0.192725)
Max_Sa <- c(0.048958333,
            0.080708333,
            0.056708333,
            0.036875,
            0.036458333,
            0.049958333)

DF_Sa <- data.frame(GR_Sa, Max_Sa)
DF_Sa

#Generate a linear regression model of the two variables with the lm function
Sa.lm <- lm(Max_Sa ~ GR_Sa+1, data=DF_Sa)
Sa.lm2 <- lm(Max_Sa ~ 0+GR_Sa, data=DF_Sa) #Tells the lm() to fit the line through the origin
summary(Sa.lm)
summary(Sa.lm2)
#Adjusted R-squared: 0.6402 
#p-value: 0.0189

#Create scatter plot of points using plot function
plot(GR_Sa, Max_Sa,
     xlab="Growth Rate (1/hr.)",
     ylab="Carrying Capacity (Max OD600)",
     xlim= c(0, 0.4),
     ylim= c(0, 0.1),
     main="Growth Rate vs. Carrying Capacity for S. aureus \n Grown in THY Dilutions",
     pch=16)

abline(lm(Sa.lm2))
legend("topleft", bty="n", legend= paste("R-squared= 0.6402 \n p-value= 0.0189"))

#Create scatter plot of points using plot function - without graph labels 
plot(GR_Sa, Max_Sa,
     xlab= "Growth Rate (1/hr)",
     ylab= "Max OD600",
     xlim= c(0, 0.4),
     ylim= c(0, 0.1),
     cex= 1.8,
     pch=16)

abline(lm(Sa.lm2))
op <- par(cex= 1.5)
legend("topleft", bty="n", legend= paste("R-squared= 0.6402 \n p-value= 0.0189"))

##############################################################################################################
#S. non-aureus str. 1839

GR_Sna1839 <- c(0.358025,
                0.198575,
                0.308066667,
                0.270025,
                0.070875,
                0.197775)
Max_Sna1839 <- c(0.045708333,
                 0.060958333,
                 0.041208333,
                 0.039208333,
                 0.024208333,
                 0.036708333)

DF_Sna1839 <- data.frame(GR_Sna1839, Max_Sna1839)
DF_Sna1839

#Generate a linear regression model of the two variables with the lm function 
Sna1839.lm <- lm(Max_Sna1839 ~ 1+GR_Sna1839, data=DF_Sna1839)
Sna1839.lm2 <- lm(Max_Sna1839 ~ 0+GR_Sna1839, data=DF_Sna1839) #Tells the lm() to fit the line through the origin
summary(Sna1839.lm)
summary(Sna1839.lm2)
#Adjusted R-squared:  0.8624
#p-value: 0.001578

#Create scatter plot of points using plot funSna1839ion
plot(GR_Sna1839, Max_Sna1839,
     xlab="Growth Rate (1/hr.)",
     ylab="Carrying Capacity (Max OD600)",
     xlim= c(0, 0.4),
     ylim= c(0, 0.1),
     main="Growth Rate vs. Carrying Capacity for S. non-aureus str. 1839 \n Grown in THY Dilutions",
     pch=16)

abline(lm(Sna1839.lm2))
legend("topleft", bty="n", legend= paste("R-squared= 0.8624 \n p-value= 0.001578"))

#Create scatter plot of points using plot function - without graph labels 
plot(GR_Sna1839, Max_Sna1839,
     xlab= "Growth Rate (1/hr)",
     ylab= "Max OD600",
     xlim= c(0, 0.4),
     ylim= c(0, 0.1),
     cex= 1.8,
     pch=16)

abline(lm(Sna1839.lm2))
op <- par(cex= 1.5)
legend("topleft", bty="n", legend= paste("R-squared= 0.8624 \n p-value= 0.001578"))


#####################################################################################################################
#S. non-aureus str. 1850 

GR_Sna1850 <- c(0.369375,
                0.114133333,
                0.27855,
                0.24665,
                0.02245,
                0.22595)
Max_Sna1850 <- c(0.045958333,
                 0.056875,
                 0.040208333,
                 0.042708333,
                 0.026208333,
                 0.040208333)

DF_Sna1850 <- data.frame(GR_Sna1850, Max_Sna1850)
DF_Sna1850

#Generate a linear regression model of the two variables with the lm function 
Sna1850.lm <- lm(Max_Sna1850 ~ 1+GR_Sna1850, data=DF_Sna1850)
Sna1850.lm2 <- lm(Max_Sna1850 ~ 0+GR_Sna1850, data=DF_Sna1850) #Tells the lm() to fit the line through the origin
summary(Sna1850.lm)
summary(Sna1850.lm2)
#Adjusted R-squared:  0.7598
#p-value: 6.582e-03

#Create scatter plot of points using plot funSna1850ion
plot(GR_Sna1850, Max_Sna1850,
     xlab="Growth Rate (1/hr.)",
     ylab="Carrying Capacity (Max OD600)",
     xlim= c(0, 0.4),
     ylim= c(0, 0.1),
     main="Growth Rate vs. Carrying Capacity for S. non-aureus str. 1850 \n Grown in THY Dilutions",
     pch=16)

abline(lm(Sna1850.lm2))
legend("topleft", bty="n", legend= paste("R-squared= 0.7598 \n p-value= 6.582e-03"))


#Create scatter plot of points using plot function - without graph labels 
plot(GR_Sna1850, Max_Sna1850,
     xlab= "Growth Rate (1/hr)",
     ylab= "Max OD600",
     xlim= c(0, 0.4),
     ylim= c(0, 0.1),
     cex= 1.8,
     pch=16)

abline(lm(Sna1850.lm2))
op <- par(cex= 1.5)
legend("topleft", bty="n", legend= paste("R-squared= 0.7598 \n p-value= 6.582e-03"))

################################################################################################################
#S. non-aureus str. 1867

GR_Sna1867 <- c(0.30945,
                0.21615,
                0.257275,
                0.2683,
                0.035675,
                0.1659)
Max_Sna1867 <- c(0.040208333,
                 0.047458333,
                 0.034708333,
                 0.037208333,
                 0.023708333,
                 0.040458333)

DF_Sna1867 <- data.frame(GR_Sna1867, Max_Sna1867)
DF_Sna1867

#Generate a linear regression model of the two variables with the lm function
Sna1867.lm <- lm(Max_Sna1867 ~ 1+GR_Sna1867, data=DF_Sna1867)
Sna1867.lm2 <- lm(Max_Sna1867 ~ 0+GR_Sna1867, data=DF_Sna1867) #Tells the lm() to fit the line through the origin
summary(Sna1867.lm)
summary(Sna1867.lm2)
#Adjusted R-squared:  0.883 
#p-value: 0.001045

#Create scatter plot of points using plot funSna1867ion
plot(GR_Sna1867, Max_Sna1867,
     xlab="Growth Rate (1/hr.)",
     ylab="Carrying Capacity (Max OD600)",
     xlim= c(0, 1.3),
     ylim= c(0, 0.2),
     main="Growth Rate vs. Carrying Capacity for S. non-aureus str. 1867 \n Grown in THY Dilutions",
     pch=16)

abline(lm(Sna1867.lm2))
legend("topleft", bty="n", legend= paste("R-squared= 0.883 \n p-value= 0.001045"))


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
legend("topleft", bty="n", legend= paste("R-squared= 0.883 \n p-value= 0.001045"))


#######################################################################################################################
#C. pseudodiphtheriticum

GR_Cp <- c(0.0285,
           0.028,
           0.012625,
           0.0051,
           0.0163,
           0.059275)
Max_Cp <- c(0.021958333,
            0.026458333,
            0.015458333,
            0.013208333,
            0.012708333,
            0.029458333)

DF_Cp <- data.frame(GR_Cp, Max_Cp)
DF_Cp

#Generate a linear regression model of the two variables with the lm function
Cp.lm <- lm(Max_Cp ~ 1+GR_Cp, data=DF_Cp)
Cp.lm2 <- lm(Max_Cp ~ 0+GR_Cp, data=DF_Cp) #Tells the lm() to fit the line through the origin
summary(Cp.lm)
summary(Cp.lm2)
#Adjusted R-squared:  0.8551
#p-value: 0.001801

#Create scatter plot of points using plot funCpion
plot(GR_Cp, Max_Cp,
     xlab="Growth Rate (1/hr.)",
     ylab="Carrying Capacity (Max OD600)",
     xlim= c(0, 0.05),
     ylim= c(0, 0.05),
     cex= 1.5,
     main="Growth Rate vs. Carrying Capacity for C. pseudodiphtheriticum \n Grown in THY Dilutions",
     pch=16)

abline(lm(Cp.lm2))
legend("topleft", bty="n", legend= paste("R-squared= 0.8551 \n p-value= 0.001801"))

#Create scatter plot of points using plot function - without graph labels
plot(GR_Cp, Max_Cp,
     xlab= "Growth Rate (1/hr)",
     ylab= "Max OD600",
     xlim= c(0, 0.05),
     ylim= c(0, 0.05),
     cex= 1.8,
     pch=16)

abline(lm(Cp.lm2))
op <- par(cex= 1.5)
legend("topleft", bty="n", legend= paste("R-squared= 0.8551 \n p-value= 0.001801"))