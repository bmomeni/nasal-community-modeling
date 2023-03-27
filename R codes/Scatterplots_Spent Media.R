#Last run 04/10/2020

#Clear Workspace 
rm(list=ls())

#################################################
#Create scatterplots for GR-CC relationships 
#################################################
#C. tuberculostearicum 

GR_Ct <- c(0.976475,
           0.1099,
           0.3584,
           0,
           0.90835,
           0.872,
           0.52)
Max_Ct <- c(0.708916667,
            0.202166667,
            0.027916667,
            0.003416667,
            0.573916667,
            0.712,
            0.402166667)

DF_Ct <- data.frame(GR_Ct, Max_Ct)
DF_Ct

#Generate a linear regression model of the two variables with the lm function 
Ct.lm <- lm(Max_Ct ~ GR_Ct + 1, data=DF_Ct)
Ct.lm2 <- lm(Max_Ct ~ 0 + GR_Ct, data=DF_Ct) #Tells the lm() to fit the line through the origin
summary(Ct.lm)
summary(Ct.lm2)

#Colors
colors <- c("deepskyblue4", "darkorchid4", "darkolivegreen", "firebrick4", "darkorange3", "darkslategray", "bisque4")

#Create scatter plot of points using plot function
plot(GR_Ct, Max_Ct,
     xlab="Growth Rate (1/hr.)",
     ylab="Carrying Capacity (Max OD600)",
     main="Growth Rate vs. Carrying Capacity for C. tuberculostearicum \n Grown in Nasal Microbiota Spent Media",
     pch=16)

abline(lm(Ct.lm2))
legend("topleft", bty="n", legend= paste("R-squared= 0.9383 \n p-value= 4.73e-05"))

#Create scatter plot of points using plot function - without graph labels 
plot(GR_Ct, Max_Ct,
     xlab= "Growth Rate (1/hr)",
     ylab= "Max OD600",
     cex= 1.8,
     col = colors,
     pch=16)

abline(lm(Ct.lm2))
op <- par(cex= 1.5)
legend("topleft", bty="n", legend= paste("R-squared= 0.9383 \n p-value= 4.73e-05"))


###############################################################################################################333                                        
#S. aureus 

GR_Sa <- c(2.574233333,
           0.515925,
           0.246725,
           1.189575,
           2.978825,
           2.236975,
           0.59775)
Max_Sa <- c(0.652666667,
            0.174666667,
            0.182916667,
            0.177166667,
            0.470416667,
            0.429416667,
            0.155166667)

DF_Sa <- data.frame(GR_Sa, Max_Sa)
DF_Sa

#Generate a linear regression model of the two variables with the lm function
Sa.lm <- lm(Max_Sa ~ GR_Sa+1, data=DF_Sa)
Sa.lm2 <- lm(Max_Sa ~ 0+GR_Sa, data=DF_Sa) #Tells the lm() to fit the line through the origin
summary(Sa.lm)
summary(Sa.lm2)

#Colors
colors <- c("deepskyblue4", "darkorchid4", "darkolivegreen", "firebrick4", "darkorange3", "darkslategray", "bisque4")

#Create scatter plot of points using plot funSaion
plot(GR_Sa, Max_Sa,
     xlab="Growth Rate (1/hr.)",
     ylab="Carrying Capacity (Max OD600)",
     xlim= c(0, 3),
     ylim= c(0, 0.7),
     main="Growth Rate vs. Carrying Capacity for S. aureus \n Grown in Nasal Microbiota Spent Media",
     pch=16)

abline(lm(Sa.lm2))
legend("topleft", bty="n", legend= paste("R-squared= 0.9229 \n p-value= 9.26e-05"))

#Create scatter plot of points using plot function - without graph labels 
plot(GR_Sa, Max_Sa,
     xlab= "Growth Rate (1/hr)",
     ylab= "Max OD600",
     xlim= c(0, 3),
     ylim= c(0, 0.7),
     cex= 1.8,
     col = colors,
     pch=16)

abline(lm(Sa.lm2))
op <- par(cex= 1.5)
legend("topleft", bty="n", legend= paste("R-squared= 0.9229  \n p-value= 9.26e-05"))

##############################################################################################################
#S. non-aureus str. 1839

GR_Sna1839 <- c(1.453766667,
           0.087075,
           0.249,
           0.1827,
           1.725125,
           0.94125,
           0)
Max_Sna1839 <- c(0.249416667,
            0.044666667,
            0.021166667,
            0.033166667,
            0.240166667,
            0.194666667,
            0.006416667)

DF_Sna1839 <- data.frame(GR_Sna1839, Max_Sna1839)
DF_Sna1839

#Generate a linear regression model of the two variables with the lm function
Sna1839.lm <- lm(Max_Sna1839 ~ 1+GR_Sna1839, data=DF_Sna1839)
Sna1839.lm2 <- lm(Max_Sna1839 ~ 0+GR_Sna1839, data=DF_Sna1839) #Tells the lm() to fit the line through the origin
summary(Sna1839.lm)
summary(Sna1839.lm2)

#Colors
colors <- c("deepskyblue4", "darkorchid4", "darkolivegreen", "firebrick4", "darkorange3", "darkslategray", "bisque4")

#Create scatter plot of points using plot funSna1839ion
plot(GR_Sna1839, Max_Sna1839,
     xlab="Growth Rate (1/hr.)",
     ylab="Carrying Capacity (Max OD600)",
     main="Growth Rate vs. Carrying Capacity for S. non-aureus str. 1839 \n Grown in Nasal Microbiota Spent Media",
     pch=16)

abline(lm(Sna1839.lm2))
legend("topleft", bty="n", legend= paste("R-squared= 0.9649 \n p-value= 8.63e-06"))

#Create scatter plot of points using plot function - without graph labels 
plot(GR_Sna1839, Max_Sna1839,
     xlab= "Growth Rate (1/hr)",
     ylab= "Max OD600",
     cex= 1.8,
     col = colors,
     pch=16)

abline(lm(Sna1839.lm2))
op <- par(cex= 1.5)
legend("topleft", bty="n", legend= paste("R-squared= 0.9649  \n p-value= 8.63e-06 "))


#####################################################################################################################
#S. non-aureus str. 1850 

GR_Sna1850 <- c(1.427825,
                0.084575,
                0.116725,
                0.3351,
                1.65325,
                1.07925,
                0)
Max_Sna1850 <- c(0.207166667,
                 0.018916667,
                 0.017666667,
                 0.030666667,
                 0.253916667,
                 0.111166667,
                 0.001166667)

DF_Sna1850 <- data.frame(GR_Sna1850, Max_Sna1850)
DF_Sna1850

#Generate a linear regression model of the two variables with the lm function
Sna1850.lm <- lm(Max_Sna1850 ~ 1+GR_Sna1850, data=DF_Sna1850)
Sna1850.lm2 <- lm(Max_Sna1850 ~ 0+GR_Sna1850, data=DF_Sna1850) #Tells the lm() to fit the line through the origin
summary(Sna1850.lm)
summary(Sna1850.lm2)

#Colors
colors <- c("deepskyblue4", "darkorchid4", "darkolivegreen", "firebrick4", "darkorange3", "darkslategray", "bisque4")

#Create scatter plot of points using plot funSna1850ion
plot(GR_Sna1850, Max_Sna1850,
     xlab="Growth Rate (1/hr.)",
     ylab="Carrying Capacity (Max OD600)",
     main="Growth Rate vs. Carrying Capacity for S. non-aureus str. 1850 \n Grown in Nasal Microbiota Spent Media",
     pch=16)

abline(lm(Sna1850.lm2))
legend("topleft", bty="n", legend= paste("R-squared= 0.9763 \n p-value= 2.65e-06"))


#Create scatter plot of points using plot function - without graph labels 
plot(GR_Sna1850, Max_Sna1850,
     xlab= "Growth Rate (1/hr)",
     ylab= "Max OD600",
     cex= 1.8,
     col = colors,
     pch=16)

abline(lm(Sna1850.lm2))
op <- par(cex= 1.5)
legend("topleft", bty="n", legend= paste("R-squared= 0.9763   \n p-value= 2.65e-06 "))

################################################################################################################
#S. non-aureus str. 1867

GR_Sna1867 <- c(1.50865,
                0.058775,
                0.004575,
                0.3279,
                1.739633333,
                1.005675,
                0)
Max_Sna1867 <- c(0.265416667,
                 0.014666667,
                 0.011166667,
                 0.029416667,
                 0.273916667,
                 0.156166667,
                 0.000916667)

DF_Sna1867 <- data.frame(GR_Sna1867, Max_Sna1867)
DF_Sna1867

#Generate a linear regression model of the two variables with the lm function 
Sna1867.lm <- lm(Max_Sna1867 ~ 1+GR_Sna1867, data=DF_Sna1867)
Sna1867.lm2 <- lm(Max_Sna1867 ~ 0+GR_Sna1867, data=DF_Sna1867) #Tells the lm() to fit the line through the origin
summary(Sna1867.lm)
summary(Sna1867.lm2)

#Colors
colors <- c("deepskyblue4", "darkorchid4", "darkolivegreen", "firebrick4", "darkorange3", "darkslategray", "bisque4")

#Create scatter plot of points using plot funSna1867ion
plot(GR_Sna1867, Max_Sna1867,
     xlab="Growth Rate (1/hr.)",
     ylab="Carrying Capacity (Max OD600)",
     main="Growth Rate vs. Carrying Capacity for S. non-aureus str. 1867 \n Grown in Nasal Microbiota Spent Media",
     pch=16)

abline(lm(Sna1867.lm2))
legend("topleft", bty="n", legend= paste("R-squared= 0.9915 \n p-value= 1.21e-07"))


#Create scatter plot of points using plot function - without graph labels 
plot(GR_Sna1867, Max_Sna1867,
     xlab= "Growth Rate (1/hr)",
     ylab= "Max OD600",
     cex= 1.8,
     col = colors,
     pch=16)

abline(lm(Sna1867.lm2))
op <- par(cex= 1.5)
legend("topleft", bty="n", legend= paste("R-squared= 0.9915   \n p-value= 1.21e-07 "))


#######################################################################################################################
#C. pseudodiphtheriticum 

GR_Cp <- c(0.291825,
           0,
           0.022075,
           0.138666667,
           0.172025,
           0.2022,
           0.0796)
Max_Cp <- c(0.286166667,
            0.002166667,
            0.014416667,
            0.074666667,
            0.306166667,
            1.102166667,
            0.053916667)

DF_Cp <- data.frame(GR_Cp, Max_Cp)
DF_Cp

#Generate a linear regression model of the two variables with the lm function
Cp.lm <- lm(Max_Cp ~ 1+GR_Cp, data=DF_Cp)
Cp.lm2 <- lm(Max_Cp ~ 0+GR_Cp, data=DF_Cp) #Tells the lm() to fit the line through the origin
summary(Cp.lm)
summary(Cp.lm2)

#Colors
colors <- c("deepskyblue4", "darkorchid4", "darkolivegreen", "firebrick4", "darkorange3", "darkslategray", "bisque4")

#Create scatter plot of points using plot funCpion
plot(GR_Cp, Max_Cp,
     xlab="Growth Rate (1/hr.)",
     ylab="Carrying Capacity (Max OD600)",
     main="Growth Rate vs. Carrying Capacity for C. pseudodiphtheriticum \n Grown in Nasal Microbiota Spent Media",
     pch=16)

abline(lm(Cp.lm2))
legend("topleft", bty="n", legend= paste("R-squared= 0.4753 \n p-value= 0.035"))

#Create scatter plot of points using plot function - without graph labels 
plot(GR_Cp, Max_Cp,
     xlab= "Growth Rate (1/hr)",
     ylab= "Max OD600",
     cex= 1.8,
     col = colors,
     pch=16)

abline(lm(Cp.lm2))
op <- par(cex= 1.5)
legend("topleft", bty="n", legend= paste("R-squared= 0.4753   \n p-value= 0.035"))
