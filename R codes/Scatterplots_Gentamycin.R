#Last run 04/10/2020

#Clear Workspace 
rm(list=ls())

#################################################
#Create scatterplots for GR-CC relationships 
#################################################
# #C. tuberculostearicum 
# 
# GR_Ct <- c(0,
#            0,
#            0,
#            0.128025,
#            0.5942,
#            0.7467,
#            0.777525,
#            0.813525,
#            0.8183)
# Max_Ct <- c(0,
#             0,
#             0,
#             0.112333333,
#             0.301416667,
#             0.486666667,
#             0.550416667,
#             0.611666667,
#             0.591166667)
# 
# DF_Ct <- data.frame(GR_Ct, Max_Ct)
# DF_Ct
# 
# #Generate a linear regression model of the two variables with the lm function 
# Ct.lm <- lm(Max_Ct ~ GR_Ct + 1, data=DF_Ct)
# Ct.lm2 <- lm(Max_Ct ~ 0 + GR_Ct, data=DF_Ct) #Tells the lm() to fit the line through the origin
# summary(Ct.lm)
# summary(Ct.lm2)
# #Adjusted R-squared:  0.9864 
# #p-value: 5.952e-09
# 
# #Create scatter plot of points using plot function
# plot(GR_Ct, Max_Ct,
#      xlab="Growth Rate (1/hr.)",
#      ylab="Carrying Capacity (Max OD600)",
#      xlim= c(0, 0.9),
#      ylim= c(0, 0.65),
#      main="Growth Rate vs. Carrying Capacity for C. tuberculostearicum \n Grown in THY Dilutions",
#      pch=16)
# 
# abline(lm(Ct.lm2))
# legend("topleft", bty="n", legend= paste("R-squared= 0.9864 \n p-value= 5.952e-09"))
# 
# #Create scatter plot of points using plot function - without graph labels 
# plot(GR_Ct, Max_Ct,
#      xlab= "Growth Rate (1/hr)",
#      ylab= "Max OD600",
#      xlim= c(0, 0.9),
#      ylim= c(0, 0.65),
#      cex= 1.8,
#      pch=16)
# 
# abline(lm(Ct.lm2))
# op <- par(cex= 1.5)
# legend("topleft", bty="n", legend= paste("R-squared= 0.9864 \n p-value= 5.952e-09"))


###############################################################################################################333                                        
#S. aureus 

GR_Sa <- c(1.946225,
           1.6369,
           0.981725,
           1.123,
           0.487675,
           0.037425,
           0.005175,
           0.006975,
           0.02295)
Max_Sa <- c(0.39325,
            0.379,
            0.39325,
            0.36525,
            0.3125,
            0.05925,
            0.016,
            0.0155,
            0.015)

DF_Sa <- data.frame(GR_Sa, Max_Sa)
DF_Sa

#Generate a linear regression model of the two variables with the lm function
Sa.lm <- lm(Max_Sa ~ GR_Sa+1, data=DF_Sa)
Sa.lm2 <- lm(Max_Sa ~ 0+GR_Sa, data=DF_Sa) #Tells the lm() to fit the line through the origin
summary(Sa.lm)
summary(Sa.lm2)
#Adjusted R-squared: 0.9064 
#p-value: 1.353e-05

#Create scatter plot of points using plot funSaion
plot(GR_Sa, Max_Sa,
     xlab="Growth Rate (1/hr.)",
     ylab="Carrying Capacity (Max OD600)",
     xlim= c(0, 2.2),
     ylim= c(0, 0.45),
     main="Growth Rate vs. Carrying Capacity for S. aureus \n Grown in THY Dilutions",
     pch=16)

abline(lm(Sa.lm2))
legend("topleft", bty="n", legend= paste("R-squared= 0.9064 \n p-value= 1.353e-05"))

#Create scatter plot of points using plot function - without graph labels 
plot(GR_Sa, Max_Sa,
     xlab= "Growth Rate (1/hr)",
     ylab= "Max OD600",
     xlim= c(0, 2.2),
     ylim= c(0, 0.45),
     cex= 1.8,
     pch=16)

abline(lm(Sa.lm2))
op <- par(cex= 1.5)
legend("topleft", bty="n", legend= paste("R-squared= 0.9064 \n p-value= 1.353e-05"))

##############################################################################################################
#S. non-aureus str. 1839

GR_Sna1839 <- c(1.06845,
                1.1066,
                1.327725,
                1.187875,
                1.352575,
                1.376575,
                1.22965,
                1.162675,
                0.912375)
Max_Sna1839 <- c(0.239916667,
                 0.134416667,
                 0.144166667,
                 0.168416667,
                 0.235666667,
                 0.186666667,
                 0.125416667,
                 0.096666667,
                 0.121666667)

DF_Sna1839 <- data.frame(GR_Sna1839, Max_Sna1839)
DF_Sna1839

#Generate a linear regression model of the two variables with the lm function 
Sna1839.lm <- lm(Max_Sna1839 ~ 1+GR_Sna1839, data=DF_Sna1839)
Sna1839.lm2 <- lm(Max_Sna1839 ~ 0+GR_Sna1839, data=DF_Sna1839) #Tells the lm() to fit the line through the origin
summary(Sna1839.lm)
summary(Sna1839.lm2)
#Adjusted R-squared:  0.9172
#p-value: 8.279e-06

#Create scatter plot of points using plot funSna1839ion
plot(GR_Sna1839, Max_Sna1839,
     xlab="Growth Rate (1/hr.)",
     ylab="Carrying Capacity (Max OD600)",
     xlim= c(0, 1.4),
     ylim= c(0, 0.25),
     main="Growth Rate vs. Carrying Capacity for S. non-aureus str. 1839 \n Grown in THY Dilutions",
     pch=16)

abline(lm(Sna1839.lm2))
legend("topleft", bty="n", legend= paste("R-squared= 0.9172 \n p-value= 8.279e-06"))

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
legend("topleft", bty="n", legend= paste("R-squared= 0.9172 \n p-value= 8.279e-06"))


#####################################################################################################################
#S. non-aureus str. 1850 

GR_Sna1850 <- c(1.0928,
                1.14775,
                1.322075,
                1.446975,
                1.464275,
                1.481625,
                1.400375,
                1.295125,
                0.9751)
Max_Sna1850 <- c(0.237416667,
                 0.157666667,
                 0.134416667,
                 0.264916667,
                 0.265916667,
                 0.220666667,
                 0.159916667,
                 0.132941667,
                 0.120166667)

DF_Sna1850 <- data.frame(GR_Sna1850, Max_Sna1850)
DF_Sna1850

#Generate a linear regression model of the two variables with the lm function 
Sna1850.lm <- lm(Max_Sna1850 ~ 1+GR_Sna1850, data=DF_Sna1850)
Sna1850.lm2 <- lm(Max_Sna1850 ~ 0+GR_Sna1850, data=DF_Sna1850) #Tells the lm() to fit the line through the origin
summary(Sna1850.lm)
summary(Sna1850.lm2)
#Adjusted R-squared:  0.9323
#p-value: 3.665e-06

#Create scatter plot of points using plot funSna1850ion
plot(GR_Sna1850, Max_Sna1850,
     xlab="Growth Rate (1/hr.)",
     ylab="Carrying Capacity (Max OD600)",
     xlim= c(0, 1.5),
     ylim= c(0, 0.35),
     main="Growth Rate vs. Carrying Capacity for S. non-aureus str. 1850 \n Grown in THY Dilutions",
     pch=16)

abline(lm(Sna1850.lm2))
legend("topleft", bty="n", legend= paste("R-squared= 0.9323 \n p-value= 3.665e-06"))


#Create scatter plot of points using plot function - without graph labels 
plot(GR_Sna1850, Max_Sna1850,
     xlab= "Growth Rate (1/hr)",
     ylab= "Max OD600",
     xlim= c(0, 1.5),
     ylim= c(0, 0.35),
     cex= 1.8,
     pch=16)

abline(lm(Sna1850.lm2))
op <- par(cex= 1.5)
legend("topleft", bty="n", legend= paste("R-squared= 0.9323 \n p-value= 3.665e-06"))

################################################################################################################
#S. non-aureus str. 1867

GR_Sna1867 <- c(1.11455,
                1.2146,
                1.350025,
                1.44755,
                1.4582,
                1.474875,
                1.32915,
                1.292675,
                0.836225)
Max_Sna1867 <- c(0.261416667,
                 0.185916667,
                 0.156666667,
                 0.234916667,
                 0.260666667,
                 0.207416667,
                 0.183916667,
                 0.133916667,
                 0.125166667)

DF_Sna1867 <- data.frame(GR_Sna1867, Max_Sna1867)
DF_Sna1867

#Generate a linear regression model of the two variables with the lm function
Sna1867.lm <- lm(Max_Sna1867 ~ 1+GR_Sna1867, data=DF_Sna1867)
Sna1867.lm2 <- lm(Max_Sna1867 ~ 0+GR_Sna1867, data=DF_Sna1867) #Tells the lm() to fit the line through the origin
summary(Sna1867.lm)
summary(Sna1867.lm2)
#Adjusted R-squared:  0.9468 
#p-value: 1.397e-06

#Create scatter plot of points using plot funSna1867ion
plot(GR_Sna1867, Max_Sna1867,
     xlab="Growth Rate (1/hr.)",
     ylab="Carrying Capacity (Max OD600)",
     xlim= c(0, 1.55),
     ylim= c(0, 0.35),
     main="Growth Rate vs. Carrying Capacity for S. non-aureus str. 1867 \n Grown in THY Dilutions",
     pch=16)

abline(lm(Sna1867.lm2))
legend("topleft", bty="n", legend= paste("R-squared= 0.9468 \n p-value= 1.397e-06"))


#Create scatter plot of points using plot function - without graph labels 
plot(GR_Sna1867, Max_Sna1867,
     xlab= "Growth Rate (1/hr)",
     ylab= "Max OD600",
     xlim= c(0, 1.55),
     ylim= c(0, 0.35),
     cex= 1.8,
     pch=16)

abline(lm(Sna1867.lm2))
op <- par(cex= 1.5)
legend("topleft", bty="n", legend= paste("R-squared= 0.9468 \n p-value= 1.397e-065"))


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
legend("topleft", bty="n", legend= paste("R-squared= 0.9179 \n p-value= 7.982e-06"))