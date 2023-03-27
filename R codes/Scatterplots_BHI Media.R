#Last run 04/10/2020

#Clear Workspace 
rm(list=ls())

#################################################
#Create scatterplots for GR-CC relationships
#From Data Verificiation_Compiled Dataset
#################################################
# #C. tuberculostearicum

GR_Ct <- c(0.727425,
           0.6421,
           0.296,
           0.201425)
Max_Ct <- c(0.2975,
            0.1535,
            0.1235,
            0.02775)

DF_Ct <- data.frame(GR_Ct, Max_Ct)
DF_Ct

#Generate a linear regression model of the two variables with the lm function
Ct.lm <- lm(Max_Ct ~ GR_Ct + 1, data=DF_Ct)
Ct.lm2 <- lm(Max_Ct ~ 0 + GR_Ct, data=DF_Ct) #Tells the lm() to fit the line through the origin
summary(Ct.lm)
summary(Ct.lm2)
#Adjusted R-squared:  0.9078
#p-value: 0.0079

#Colors 
colors <- c("deepskyblue4", "darkorchid4", "darkolivegreen", "firebrick4")

#Create scatter plot of points using plot function
plot(GR_Ct, Max_Ct,
     xlab="Growth Rate (1/hr.)",
     ylab="Carrying Capacity (Max OD600)",
     xlim= c(0, 0.8),
     ylim= c(0, 0.3),
     col = colors,
     main="Growth Rate vs. Carrying Capacity for C. tuberculostearicum \n Grown in THY Dilutions",
     pch=16)

abline(lm(Ct.lm2))
legend("topleft", bty="n", legend= paste("R-squared= 0.9078 \n p-value= 0.0079"))

#Create scatter plot of points using plot function - without graph labels
plot(GR_Ct, Max_Ct,
     xlab= "Growth Rate (1/hr)",
     ylab= "Max OD600",
     xlim= c(0, 0.8),
     ylim= c(0, 0.3),
     cex= 1.5,
     col = colors,
     pch=16)

abline(lm(Ct.lm2))
op <- par(cex= 1.5)
legend("topleft", bty="n", legend= paste("R-squared= 0.9078 \n p-value= 0.0079"))
# 
# 
# ###############################################################################################################333
#S. aureus

GR_Sa <- c(2.283025,
           2.021075,
           1.19315,
           0)
Max_Sa <- c(0.36775,
            0.20975,
            0.10925,
            0.02325)

DF_Sa <- data.frame(GR_Sa, Max_Sa)
DF_Sa

#Generate a linear regression model of the two variables with the lm function
Sa.lm <- lm(Max_Sa ~ GR_Sa+1, data=DF_Sa)
Sa.lm2 <- lm(Max_Sa ~ 0+GR_Sa, data=DF_Sa) #Tells the lm() to fit the line through the origin
summary(Sa.lm)
summary(Sa.lm2)
#Adjusted R-squared:  0.9271
#p-value: 0.0055

#Colors
colors <- c("deepskyblue4", "darkorchid4", "darkolivegreen", "firebrick4")

#Create scatter plot of points using plot function
plot(GR_Sa, Max_Sa,
     xlab="Growth Rate (1/hr.)",
     ylab="Carrying Capacity (Max OD600)",
     xlim= c(0, 2.3),
     ylim= c(0, 0.4),
     col = colors,
     main="Growth Rate vs. Carrying Capacity for S. aureus \n Grown in THY Dilutions",
     pch=16)

abline(lm(Sa.lm2))
legend("topleft", bty="n", legend= paste("R-squared= 0.9271 \n p-value= 0.0055"))

#Create scatter plot of points using plot function - without graph labels
plot(GR_Sa, Max_Sa,
     xlab= "Growth Rate (1/hr)",
     ylab= "Max OD600",
     xlim= c(0, 2.3),
     ylim= c(0, 0.4),
     col = colors,
     cex= 1.8,
     pch=16)

abline(lm(Sa.lm2))
op <- par(cex= 1.5)
legend("topleft", bty="n", legend= paste("R-squared= 0.9271 \n p-value= 0.0055"))

##############################################################################################################
#S. non-aureus str. 1839

GR_Sna1839 <- c(1.1359,
                0.816,
                0.1445,
                0)
Max_Sna1839 <- c(0.1635,
                 0.131,
                 0.06975,
                 0.01125)

DF_Sna1839 <- data.frame(GR_Sna1839, Max_Sna1839)
DF_Sna1839

#Generate a linear regression model of the two variables with the lm function
Sna1839.lm <- lm(Max_Sna1839 ~ 1+GR_Sna1839, data=DF_Sna1839)
Sna1839.lm2 <- lm(Max_Sna1839 ~ 0+GR_Sna1839, data=DF_Sna1839) #Tells the lm() to fit the line through the origin
summary(Sna1839.lm)
summary(Sna1839.lm2)
#Adjusted R-squared:  0.9307
#p-value: 0.0051

#Colors
colors <- c("deepskyblue4", "darkorchid4", "darkolivegreen", "firebrick4")

#Create scatter plot of points using plot funSna1839ion
plot(GR_Sna1839, Max_Sna1839,
     xlab="Growth Rate (1/hr.)",
     ylab="Carrying Capacity (Max OD600)",
     xlim= c(0, 1.2),
     ylim= c(0, 0.2),
     col = colors,
     main="Growth Rate vs. Carrying Capacity for S. non-aureus str. 1839 \n Grown in THY Dilutions",
     pch=16)

abline(lm(Sna1839.lm2))
legend("topleft", bty="n", legend= paste("R-squared= 0.9307 \n p-value= 0.0051"))

#Create scatter plot of points using plot function - without graph labels
plot(GR_Sna1839, Max_Sna1839,
     xlab= "Growth Rate (1/hr)",
     ylab= "Max OD600",
     xlim= c(0, 1.2),
     ylim= c(0, 0.2),
     col = colors,
     cex= 1.8,
     pch=16)

abline(lm(Sna1839.lm2))
op <- par(cex= 1.5)
legend("topleft", bty="n", legend= paste("R-squared= 0.9307 \n p-value= 0.0051"))


####################################################################################################################
#S. non-aureus str. 1850

GR_Sna1850 <- c(1.231575,
                0.925425,
                0.2452,
                0.01295)
Max_Sna1850 <- c(0.183,
                 0.13725,
                 0.09725,
                 0.0095)

DF_Sna1850 <- data.frame(GR_Sna1850, Max_Sna1850)
DF_Sna1850

#Generate a linear regression model of the two variables with the lm function
Sna1850.lm <- lm(Max_Sna1850 ~ 1+GR_Sna1850, data=DF_Sna1850)
Sna1850.lm2 <- lm(Max_Sna1850 ~ 0+GR_Sna1850, data=DF_Sna1850) #Tells the lm() to fit the line through the origin
summary(Sna1850.lm)
summary(Sna1850.lm2)
#Adjusted R-squared:  0.9210
#p-value: 0.0062

#Colors
colors <- c("deepskyblue4", "darkorchid4", "darkolivegreen", "firebrick4")

#Create scatter plot of points using plot function
plot(GR_Sna1850, Max_Sna1850,
     xlab="Growth Rate (1/hr.)",
     ylab="Carrying Capacity (Max OD600)",
     xlim= c(0, 1.3),
     ylim= c(0, 0.2),
     col = colors,
     main="Growth Rate vs. Carrying Capacity for S. non-aureus str. 1850 \n Grown in THY Dilutions",
     pch=16)

abline(lm(Sna1850.lm2))
legend("topleft", bty="n", legend= paste("R-squared= 0.9210 \n p-value= 0.0062"))


#Create scatter plot of points using plot function - without graph labels
plot(GR_Sna1850, Max_Sna1850,
     xlab= "Growth Rate (1/hr)",
     ylab= "Max OD600",
     xlim= c(0, 1.3),
     ylim= c(0, 0.2),
     col = colors,
     cex= 1.8,
     pch=16)

abline(lm(Sna1850.lm2))
op <- par(cex= 1.5)
legend("topleft", bty="n", legend= paste("R-squared= 0.9210 \n p-value= 0.0062"))

# ################################################################################################################
#S. non-aureus str. 1867

GR_Sna1867 <- c(1.1501,
                0.840875,
                1.074833333,
                0.19855)
Max_Sna1867 <- c(0.24475,
                 0.12875,
                 0.042,
                 0.023)

DF_Sna1867 <- data.frame(GR_Sna1867, Max_Sna1867)
DF_Sna1867

#Generate a linear regression model of the two variables with the lm function
Sna1867.lm <- lm(Max_Sna1867 ~ 1+GR_Sna1867, data=DF_Sna1867)
Sna1867.lm2 <- lm(Max_Sna1867 ~ 0+GR_Sna1867, data=DF_Sna1867) #Tells the lm() to fit the line through the origin
summary(Sna1867.lm)
summary(Sna1867.lm2)
#Adjusted R-squared:  0.6804
#p-value: 0.054

#Colors
colors <- c("deepskyblue4", "darkorchid4", "darkolivegreen", "firebrick4")

#Create scatter plot of points using plot funSna1867ion
plot(GR_Sna1867, Max_Sna1867,
     xlab="Growth Rate (1/hr.)",
     ylab="Carrying Capacity (Max OD600)",
     xlim= c(0, 1.2),
     ylim= c(0, 0.3),
     col = colors,
     main="Growth Rate vs. Carrying Capacity for S. non-aureus str. 1867 \n Grown in THY Dilutions",
     pch=16)

abline(lm(Sna1867.lm2))
legend("topleft", bty="n", legend= paste("R-squared= 0.6804 \n p-value= 0.054"))


#Create scatter plot of points using plot function - without graph labels
plot(GR_Sna1867, Max_Sna1867,
     xlab= "Growth Rate (1/hr)",
     ylab= "Max OD600",
     xlim= c(0, 1.2),
     ylim= c(0, 0.3),
     cex= 1.8,
     col = colors,
     pch=16)

abline(lm(Sna1867.lm2))
op <- par(cex= 1.5)
legend("topleft", bty="n", legend= paste("R-squared= 0.6804 \n p-value= 0.054"))
 
 
# ######################################################################################################################
#C. pseudodiphtheriticum

GR_Cp <- c(0.465075,
           0,
           0.3339,
           0)
Max_Cp <- c(0.21675,
            0.092,
            0.0795,
            0.068)

DF_Cp <- data.frame(GR_Cp, Max_Cp)
DF_Cp

#Generate a linear regression model of the two variables with the lm function
Cp.lm <- lm(Max_Cp ~ 1+GR_Cp, data=DF_Cp)
Cp.lm2 <- lm(Max_Cp ~ 0+GR_Cp, data=DF_Cp) #Tells the lm() to fit the line through the origin
summary(Cp.lm)
summary(Cp.lm2)
#Adjusted R-squared:  0.6604
#p-value: 0.059

#Colors
colors <- c("deepskyblue4", "darkorchid4", "darkolivegreen", "firebrick4")

#Create scatter plot of points using plot funCpion
plot(GR_Cp, Max_Cp,
     xlab="Growth Rate (1/hr.)",
     ylab="Carrying Capacity (Max OD600)",
     xlim= c(0, 0.5),
     ylim= c(0, 0.25),
     cex= 1.5,
     col = colors,
     main="Growth Rate vs. Carrying Capacity for C. pseudodiphtheriticum \n Grown in THY Dilutions",
     pch=16)

abline(lm(Cp.lm2))
legend("topleft", bty="n", legend= paste("R-squared= 0.6604 \n p-value= 0.059"))

#Create scatter plot of points using plot function - without graph labels
plot(GR_Cp, Max_Cp,
     xlab= "Growth Rate (1/hr)",
     ylab= "Max OD600",
     xlim= c(0, 0.5),
     ylim= c(0, 0.25),
     cex= 1.8,
     col = colors,
     pch=16)

abline(lm(Cp.lm2))
op <- par(cex= 1.5)
legend("topleft", bty="n", legend= paste("R-squared= 0.9526  \n p-value= 5.5e-04"))
