#Last run 04/10/2020

#Clear Workspace 
rm(list=ls())

#################################################
#Create scatterplots for GR-CC relationships
# From 'DATA VERIFICATION_COMPILED' data
#################################################
#C. tuberculostearicum

GR_Ct <- c(0.89495,
           0.8637,
           0.89135,
           0.86425)
Max_Ct <- c(1.75875,
            1.29725,
            0.858,
            0.53225)

DF_Ct <- data.frame(GR_Ct, Max_Ct)
DF_Ct

#Generate a linear regression model of the two variables with the lm function
Ct.lm <- lm(Max_Ct ~ GR_Ct + 1, data=DF_Ct)
Ct.lm2 <- lm(Max_Ct ~ 0 + GR_Ct, data=DF_Ct) #Tells the lm() to fit the line through the origin
summary(Ct.lm)
summary(Ct.lm2)
#Adjusted R-squared:  0.8109
#p-value: 0.024

#Colors
colors <- c("deepskyblue4", "darkorchid4", "darkolivegreen", "firebrick4")

#Create scatter plot of points using plot function
plot(GR_Ct, Max_Ct,
     xlab="Growth Rate (1/hr.)",
     ylab="Carrying Capacity (Max OD600)",
     xlim= c(0, 0.9),
     ylim= c(0, 1.8),
     col = colors,
     main="Growth Rate vs. Carrying Capacity for C. tuberculostearicum \n Grown in THY Dilutions",
     pch=16)

abline(lm(Ct.lm2))
legend("topleft", bty="n", legend= paste("R-squared= 0.8109 \n p-value= 0.024"))

#Create scatter plot of points using plot function - without graph labels
plot(GR_Ct, Max_Ct,
     xlab= "Growth Rate (1/hr)",
     ylab= "Max OD600",
     xlim= c(0, 0.9),
     ylim= c(0, 1.8),
     cex= 1.8,
     col = colors,
     pch=16)

abline(lm(Ct.lm2))
op <- par(cex= 1.5)
legend("topleft", bty="n", legend= paste("R-squared= 0.8109 \n p-value= 0.024"))


###############################################################################################################333                                        
#S. aureus 

GR_Sa <- c(2.020125,
           2.020125,
           2.1861,
           2.183775)
Max_Sa <- c(1.815,
            1.5375,
            0.99,
            0.63575)

DF_Sa <- data.frame(GR_Sa, Max_Sa)
DF_Sa

#Generate a linear regression model of the two variables with the lm function
Sa.lm <- lm(Max_Sa ~ GR_Sa+1, data=DF_Sa)
Sa.lm2 <- lm(Max_Sa ~ 0+GR_Sa, data=DF_Sa) #Tells the lm() to fit the line through the origin
summary(Sa.lm)
summary(Sa.lm2)
#Adjusted R-squared:  0.8064
#p-value: 0.025

#Colors
colors <- c("deepskyblue4", "darkorchid4", "darkolivegreen", "firebrick4")

#Create scatter plot of points using plot funSaion
plot(GR_Sa, Max_Sa,
     xlab="Growth Rate (1/hr.)",
     ylab="Carrying Capacity (Max OD600)",
     xlim= c(0, 2.25),
     ylim= c(0, 1.9),
     col = colors,
     main="Growth Rate vs. Carrying Capacity for S. aureus \n Grown in THY Dilutions",
     pch=16)

abline(lm(Sa.lm2))
legend("topleft", bty="n", legend= paste("R-squared= 0.8064 \n p-value= 0.025"))

#Create scatter plot of points using plot function - without graph labels 
plot(GR_Sa, Max_Sa,
     xlab= "Growth Rate (1/hr)",
     ylab= "Max OD600",
     xlim= c(0, 2.25),
     ylim= c(0, 1.9),
     cex= 1.8,
     col = colors,
     pch=16)

abline(lm(Sa.lm2))
op <- par(cex= 1.5)
legend("topleft", bty="n", legend= paste("R-squared= 0.8064 \n p-value= 0.025"))

##############################################################################################################
#S. non-aureus str. 1839

GR_Sna1839 <- c(1.4166,
                1.725075,
                1.61245,
                1.336775)
Max_Sna1839 <- c(1.6295,
                 1.35475,
                 0.67275,
                 0.339)

DF_Sna1839 <- data.frame(GR_Sna1839, Max_Sna1839)
DF_Sna1839

#Generate a linear regression model of the two variables with the lm function 
Sna1839.lm <- lm(Max_Sna1839 ~ 1+GR_Sna1839, data=DF_Sna1839)
Sna1839.lm2 <- lm(Max_Sna1839 ~ 0+GR_Sna1839, data=DF_Sna1839) #Tells the lm() to fit the line through the origin
summary(Sna1839.lm)
summary(Sna1839.lm2)
#Adjusted R-squared:  0.7423 
#p-value: 0.038

#Colors
colors <- c("deepskyblue4", "darkorchid4", "darkolivegreen", "firebrick4")

#Create scatter plot of points using plot funSna1839ion
plot(GR_Sna1839, Max_Sna1839,
     xlab="Growth Rate (1/hr.)",
     ylab="Carrying Capacity (Max OD600)",
     xlim= c(0, 1.8),
     ylim= c(0, 1.7),
     col = colors,
     main="Growth Rate vs. Carrying Capacity for S. non-aureus str. 1839 \n Grown in THY Dilutions",
     pch=16)

abline(lm(Sna1839.lm2))
legend("topleft", bty="n", legend= paste("R-squared= 0.7423 \n p-value= 0.038"))

#Create scatter plot of points using plot function - without graph labels 
plot(GR_Sna1839, Max_Sna1839,
     xlab= "Growth Rate (1/hr)",
     ylab= "Max OD600",
     xlim= c(0, 1.8),
     ylim= c(0, 1.7),
     cex= 1.8,
     col = colors,
     pch=16)

abline(lm(Sna1839.lm2))
op <- par(cex= 1.5)
legend("topleft", bty="n", legend= paste("R-squared= 0.7423 \n p-value= 0.038"))


#####################################################################################################################
#S. non-aureus str. 1850 

GR_Sna1850 <- c(1.704675,
                1.88905,
                1.611575,
                1.409875)
Max_Sna1850 <- c(1.6035,
                 1.34825,
                 0.722,
                 0.32625)

DF_Sna1850 <- data.frame(GR_Sna1850, Max_Sna1850)
DF_Sna1850

#Generate a linear regression model of the two variables with the lm function 
Sna1850.lm <- lm(Max_Sna1850 ~ 1+GR_Sna1850, data=DF_Sna1850)
Sna1850.lm2 <- lm(Max_Sna1850 ~ 0+GR_Sna1850, data=DF_Sna1850) #Tells the lm() to fit the line through the origin
summary(Sna1850.lm)
summary(Sna1850.lm2)
#Adjusted R-squared:  0.8121
#p-value: 0.025

#Colors
colors <- c("deepskyblue4", "darkorchid4", "darkolivegreen", "firebrick4")

#Create scatter plot of points using plot function
plot(GR_Sna1850, Max_Sna1850,
     xlab="Growth Rate (1/hr.)",
     ylab="Carrying Capacity (Max OD600)",
     xlim= c(0, 1.95),
     ylim= c(0, 1.7),
     col = colors,
     main="Growth Rate vs. Carrying Capacity for S. non-aureus str. 1850 \n Grown in THY Dilutions",
     pch=16)

abline(lm(Sna1850.lm2))
legend("topleft", bty="n", legend= paste("R-squared= 0.8121 \n p-value= 0.025"))


#Create scatter plot of points using plot function - without graph labels 
plot(GR_Sna1850, Max_Sna1850,
     xlab= "Growth Rate (1/hr)",
     ylab= "Max OD600",
     xlim= c(0, 1.95),
     ylim= c(0, 1.7),
     cex= 1.8,
     col = colors,
     pch=16)

abline(lm(Sna1850.lm2))
op <- par(cex= 1.5)
legend("topleft", bty="n", legend= paste("R-squared= 0.8121 \n p-value= 0.025"))

################################################################################################################
#HAVE NOT CHANGED DATA FROM HERE DOWN - CHANGE TO 'DATA VERIFICATION_COMPILED' DATA
#S. non-aureus str. 1867

GR_Sna1867 <- c(1.57855,
                1.696975,
                1.59195,
                1.3573)
Max_Sna1867 <- c(1.62925,
                 1.36425,
                 0.7215,
                 0.36375)

DF_Sna1867 <- data.frame(GR_Sna1867, Max_Sna1867)
DF_Sna1867

#Generate a linear regression model of the two variables with the lm function
Sna1867.lm <- lm(Max_Sna1867 ~ 1+GR_Sna1867, data=DF_Sna1867)
Sna1867.lm2 <- lm(Max_Sna1867 ~ 0+GR_Sna1867, data=DF_Sna1867) #Tells the lm() to fit the line through the origin
summary(Sna1867.lm)
summary(Sna1867.lm2)
#Adjusted R-squared:  0.7949
#p-value: 0.027

#Colors
colors <- c("deepskyblue4", "darkorchid4", "darkolivegreen", "firebrick4")

#Create scatter plot of points using plot function
plot(GR_Sna1867, Max_Sna1867,
     xlab="Growth Rate (1/hr.)",
     ylab="Carrying Capacity (Max OD600)",
     xlim= c(0, 1.75),
     ylim= c(0, 1.7),
     col = colors,
     main="Growth Rate vs. Carrying Capacity for S. non-aureus str. 1867 \n Grown in THY Dilutions",
     pch=16)

abline(lm(Sna1867.lm2))
legend("topleft", bty="n", legend= paste("R-squared= 0.7949 \n p-value= 0.027"))


#Create scatter plot of points using plot function - without graph labels 
plot(GR_Sna1867, Max_Sna1867,
     xlab= "Growth Rate (1/hr)",
     ylab= "Max OD600",
     xlim= c(0, 1.75),
     ylim= c(0, 1.7),
     cex= 1.8,
     col = colors, 
     pch=16)

abline(lm(Sna1867.lm2))
op <- par(cex= 1.5)
legend("topleft", bty="n", legend= paste("R-squared= 0.7949 \n p-value= 0.027"))


#######################################################################################################################
#C. pseudodiphtheriticum 

GR_Cp <- c(0.850575,
           0.711225,
           0.617725,
           0)
Max_Cp <- c(1.761,
            1.38675,
            0.656,
            0.096)

DF_Cp <- data.frame(GR_Cp, Max_Cp)
DF_Cp

#Generate a linear regression model of the two variables with the lm function
Cp.lm <- lm(Max_Cp ~ 1+GR_Cp, data=DF_Cp)
Cp.lm2 <- lm(Max_Cp ~ 0+GR_Cp, data=DF_Cp) #Tells the lm() to fit the line through the origin
summary(Cp.lm)
summary(Cp.lm2)
#Adjusted R-squared:  0.9314
#p-value: 0.005

#Colors
colors <- c("deepskyblue4", "darkorchid4", "darkolivegreen", "firebrick4")

#Create scatter plot of points using plot funCpion
plot(GR_Cp, Max_Cp,
     xlab="Growth Rate (1/hr.)",
     ylab="Carrying Capacity (Max OD600)",
     xlim= c(0, 0.9),
     ylim= c(0, 1.8),
     cex= 1.5,
     col = colors,
     main="Growth Rate vs. Carrying Capacity for C. pseudodiphtheriticum \n Grown in THY Dilutions",
     pch=16)

abline(lm(Cp.lm2))
legend("topleft", bty="n", legend= paste("R-squared= 0.9314  \n p-value= 0.005"))

#Create scatter plot of points using plot function - without graph labels
plot(GR_Cp, Max_Cp,
     xlab= "Growth Rate (1/hr)",
     ylab= "Max OD600",
     xlim= c(0, 0.9),
     ylim= c(0, 1.8),
     cex= 1.8,
     col = colors,
     pch=16)

abline(lm(Cp.lm2))
op <- par(cex= 1.5)
legend("topleft", bty="n", legend= paste("R-squared= 0.9314  \n p-value= 0.005"))
