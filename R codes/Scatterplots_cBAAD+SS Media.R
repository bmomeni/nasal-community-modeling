#Last run 04/10/2020

#Clear Workspace 
rm(list=ls())

#################################################
#Create scatterplots for GR-CC relationships 
#################################################

####################################################################################################
#C. tuberculostearicum in cBAAD+SS dilutions

GR_Ct <- c(0.418875,
           0.481875,
           0.558425,
           0.66295,
           0.663075,
           0.63545)

Max_Ct <- c(0.630666667,
            0.60775,
            0.54575,
            0.371,
            0.27375,
            0.23325)

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
     xlim= c(0, 0.7),
     ylim= c(0, 0.7),
     main="Growth Rate vs. Carrying Capacity for C. tuberculostearicum \n Grown in Complex Carbon Concentrations",
     pch=16)

abline(lm(Ct.lm2))
legend("topleft", bty="n", legend= paste("R-squared= 0.730 \n p-value= 0.009"))

#Create scatter plot of points using plot function - without graph labels 
plot(GR_Ct, Max_Ct,
     xlab= "Growth Rate (1/hr)",
     ylab= "Max OD600",
     xlim= c(0, 0.7),
     ylim= c(0, 0.7),
     cex= 1.5,
     pch=16)

#abline(lm(Ct.lm2))
#op <- par(cex= 1.5)
#legend("topleft", bty="n", legend= paste("R-squared= 0.730 \n p-value= 0.009"))

################################################################################################3
#S. aureus in cBAAD+SS dilutions

GR_Sa <- c(1.102125,
           1.088575,
           1.116875,
           1.081475,
           1.0499,
           1.1184)

Max_Sa <- c(1.19375,
            0.85425,
            0.44175,
            0.20975,
            0.1375,
            0.15)

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
     xlim= c(0, 1.25),
     ylim= c(0, 1.25),
     main="Growth Rate vs. Carrying Capacity for S. aureus \n Grown in Complex Carbon Concentrations",
     pch=16)

abline(lm(Sa.lm2))
legend("topleft", bty="n", legend= paste("R-squared= 0.538 \n p-value= 0.0375"))

#Create scatter plot of points using plot function - without graph labels 
plot(GR_Sa, Max_Sa,
     xlab= "Growth Rate (1/hr)",
     ylab= "Max OD600",
     xlim= c(0, 1.25),
     ylim= c(0, 1.25),
     cex= 1.5,
     pch=16)

#abline(lm(Sa.lm2))
#op <- par(cex= 1.5)
#legend("topleft", bty="n", legend= paste("R-squared= 0.538 \n p-value= 0.0375"))

####################################################################################################