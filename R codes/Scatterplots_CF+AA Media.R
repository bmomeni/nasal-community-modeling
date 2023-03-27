#Last run 04/10/2020

#Clear Workspace 
rm(list=ls())

#################################################
#Create scatterplots for GR-CC relationships 
#################################################

####################################################################################################
#C. tuberculostearicum in CF + AA dilutions

GR_Ct <- c(0.4733,
           0.650175,
           0.819325,
           0.9336,
           1.44235,
           1.001425,
           1.576725,
           1.162133333,
           1.276025)
Max_Ct <- c(0.272694444,
            0.317944444,
            0.553194444,
            0.816444444,
            0.801194444,
            0.738694444,
            0.618444444,
            0.448694444,
            0.316444444)

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
     xlim= c(0, 1.65),
     ylim= c(0, 0.83),
     main="Growth Rate vs. Carrying Capacity for C. tuberculostearicum \n Grown in Amino Acid Concentrations",
     pch=16)

abline(lm(Ct.lm2))
legend("topleft", bty="n", legend= paste("R-squared= 0.867 \n p-value= 5.62e-05"))

#Create scatter plot of points using plot function - without graph labels 
plot(GR_Ct, Max_Ct,
     xlab= "Growth Rate (1/hr)",
     ylab= "Max OD600",
     xlim= c(0, 1.65),
     ylim= c(0, 0.83),
     cex= 1.5,
     pch=16)

abline(lm(Ct.lm2))
op <- par(cex= 1.5)
legend("topleft", bty="n", legend= paste("R-squared= 0.867 \n p-value= 5.62e-05"))

################################################################################################3
#S. aureus in CF + AA dilutions

GR_Sa <- c(0.032025,
           0.05245,
           0.042675,
           0.173975,
           0.069375,
           0.03725,
           0.010525,
           0.101325,
           0.1208)
Max_Sa <- c(0.014194444,
            0.036194444,
            0.025194444,
            0.037444444,
            0.027944444,
            0.021694444,
            0.016694444,
            0.033694444,
            0.077444444)

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
     xlim= c(0, 0.19),
     ylim= c(0, 0.085),
     main="Growth Rate vs. Carrying Capacity for S. aureus \n Grown in Complex Amino Acid Concentrations",
     pch=16)

abline(lm(Sa.lm2))
legend("topleft", bty="n", legend= paste("R-squared= 0.7781 \n p-value= 0.00045"))

#Create scatter plot of points using plot function - without graph labels 
plot(GR_Sa, Max_Sa,
     xlab= "Growth Rate (1/hr)",
     ylab= "Max OD600",
     xlim= c(0, 0.19),
     ylim= c(0, 0.085),
     cex= 1.5,
     pch=16)

abline(lm(Sa.lm2))
op <- par(cex= 1.5)
legend("topleft", bty="n", legend= paste("R-squared= 0.7781 \n p-value= 0.00045"))

####################################################################################################