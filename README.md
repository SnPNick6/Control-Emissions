# Control-Emissions
# Graduate School R Project on Nuclear Power Generation 
# This is a project I made in graduate school. It was a semester long project in R. 
# I am attempting to expand my knowledge in Data Science.

# Nuclear Power Project
# ==============================================================================
# 
# Variable List - Based on Total Nuclear Production States
varlist.1 = c("Argentina","Armenia","Belgium","Brazil","Bulgaria","Canada",
              "China","Czech Republic","Finland","France","Germany",
              "Hungary","India","Iran","Japan","Korea,Republic of",
              "Republic of Korea",
              "Lithuania","Mexico","Netherlands", "Other Asia","Pakistan",
              "Romania","Russian Federation","Slovakia","Slovenia",
              "South Africa", "Spain","Sweden","Switzerland","Ukraine",
              "United Kingdom","United States")
varlist.1

varlist.2 = c("2010") # The year under observation in cross-sectional analysis.
varlist.2

varlist.3 = c("France") # To be used for time-series analysis.


# Variable Acronyms
# TNP = Total Nuclear Production
# EI = Energy Intensity
# Region = Geographic Location
# SFI = State Fragility Index
# GDPk = Gross Domestic Product Per Capita
# POL = Polity Score
# CO2 = Carbon Dioxide Emissions per 1000 metric tons
# EGEN = Percent of Nuclear Electric Generation Annually
# POP = Total Population
# COAL = Percent of coal used for electrical production


#
# Variables
# Main Independent Variable
# Total Nuclear Production - Electricity = TNP
# Source:  UN Data
# 
myData = read.csv("Nuclear.csv", header = T) # Upload nuclear data
myData = myData[,-c(2,4,6)]               # Clean Data
colnames(myData) = c("Country","Year","TNP") # Create column names

summary(myData)                              # Gather Basic Statistics



myData = subset(myData, myData$Year %in% varlist.2)  # Use var.list.2
                                                     # on data
myData = subset(myData, myData$Country %in%varlist.1) # Use var.list.1
                                                      # on data
sd(myData$TNP, na.rm = FALSE)                #Calculate Standard Deviation
#
# Energy Intensity (Energy Consumption/GDP) = EI 
# Source:  Energy Information Agency
#
myData1 = read.csv("Energy Intensity.csv",       # Upload EI data
                   stringsAsFactors=F,  # Don't allow string to factor conversion
                   na.strings = "NA",
                   header= F)            # Data has no headers)
myData1 = myData1[-c(1:4),-c(2)]
summary(myData1)
colnames(myData1) = c("Country", "1990","1991","1992","1993","1994","1995",
                      "1996","1997","1998","1999","2000","2001","2002",
                      "2003","2004","2005","2006","2007",
                      "2008","2009","2010","2011")
myData1 = myData1[,-c(2:22)]             # Clean Data
myData1 = subset(myData1, myData1$Country %in%varlist.1)
t(myData1)                              # View data transposed
colnames(myData1) = c("Country","EI")   # Create Column Names
sd(myData1$EI, na.rm = FALSE)           # Standard Deviation


# State Fragility Index = SFI
# Independent Variable
# This dataset also includes a region categorical variable that will be useful
# in distinguishing different regions of the countries under observation.
# Source:  Center for Systemic Peace
myData2 = read.csv("SFI.csv", header = T)       # Upload SFI data
myData2 = myData2[,(2:5)]                       # Clean Data   
colnames(myData2) = c("Country","Year","Region","SFI") # create Column Names

# Use lists to observe necessary states

myData2 = subset(myData2, myData2$Year %in% varlist.2)
myData2 = subset(myData2, myData2$Country %in%varlist.1)


summary(myData2)
sd(myData2$SFI, na.rm = FALSE)

# Economic Development = GDPk 
# Source:  UN Data
# This variable will be necessary as a control during the time-series analysis
myData3 = read.csv("GDPk.csv", header = T)
myData3 = myData3[,c(1,2,4)]
colnames(myData3) = c("Country","Year","GDPk")
myData3 = subset(myData3, myData3$Year %in% varlist.2)
myData3 = subset(myData3, myData3$Country %in%varlist.1)

summary(myData3)
sd(myData3$GDPk, na.rm = FALSE)

# Polity = POL
# This is a measure from 10 to -10 with 10 being fully democratic and -10 being
# fully autocratic.
# Source:  Center for Systemic Peace
myData4 = read.csv("POL.csv",
                   stringsAsFactors=F,  # Don't allow string to factor conversion
                   na.strings = ",",   # Note this wierd missing data notation
                   header= T)           # Data has headers))
myData4 = myData4[,c(4,5,10)]          # Clean Data
colnames(myData4) = c("Country","Year","POL") # Create Column Names

# Use lists to observe the necessary states

myData4 = subset(myData4, myData4$Year %in% varlist.2)
myData4 = subset(myData4, myData4$Country %in% varlist.1)

summary(myData4)
sd(myData4$POL, na.rm = FALSE)             # Calculate Standard Deviation


# Carbon Dioxide Emissions, thousand metric tons = CO2
# This is the dependent variable of the study.
# Source:  UN Data
myData5 = read.csv("CO2.csv", header = T)   # Upload CO2 data
colnames(myData5) = c("Country","Year","CO2") # Create Column Names
summary(myData5)

# Use lists to observe necessary states

myData5 = subset(myData5, myData5$Year %in% varlist.2)   # Eliminate unnecessary 
myData5 = subset(myData5, myData5$Country %in% varlist.1)# observations.
sd(myData5$CO2, na.rm = FALSE)


# This data came from ended up being unncessary for the project so it was eliminated
# after the merge. It is the % of nuclear energy emitted per year.
# Source:  Energy Information Agency
myData6 = read.csv("EGEN.csv",          # Upload EGEN data
                   stringsAsFactors=F,  # Don't allow string to factor conversion
                   na.strings = "NA",
                   header= T)            # Data has headers)
myData6 = myData6[-c(32:67),]            # Clean Data
myData6 = myData6[,c(1,32)]
colnames(myData6) = c("Country","EGEN")  # Create Column Names

# Population - Total Population of the State = POP
# Population holds an important relationship with CO2 emissions so it is included
# in the study.
# Source:  UN Data
myData7 = read.csv("POP.csv",          # Upload POPd data
                   stringsAsFactors=F,  # Don't allow string to factor conversion
                   na.strings = "NA",
                   header= T)            # Data has headers)
colnames(myData7) = c("Country","Year", "POP")  # Create Column Names

# Use list for necessary observations
myData7 = subset(myData7, myData7$Year %in% varlist.2)
myData7 = subset(myData7, myData7$Country %in% varlist.1)
sd(myData7$POP, na.rm = FALSE)
summary(myData7)

# It is predicted that coal will have the opposite effect with CO2 compared to the
# TNP variable. It is measured as the % of electricity production of coal sources.
# Source U.N. Data
myData8 = read.csv("COAL.csv",          # UPload COAL data
                   stringsAsFactors=F,  # Don't allow string to factor conversion
                   na.strings = "NA",
                   header= F)            # Data has no headers) )
myData8 = myData8[,c(1,55)]
myData8 = myData8[-c(1:3),]
colnames(myData8) = c("Country","COAL")
myData8 = subset(myData8, myData8$Country %in% varlist.1)
sd(myData8$COAL,na.rm = FALSE)
summary(myData8)

# Merge the Data

newData = merge(myData, myData1,      # The two datasets to merge
                by.x="Country", by.y="Country", # The name of the variables to match
                all.x=F, all.y=F)           # By Which observations to keep

newData1 = merge(newData, myData2,        
                 by.x="Country", by.y="Country",  
                 all.x=F, all.y=F)                  

newData3 = merge(newData1, myData3,       
                 by.x="Country", by.y="Country", 
                 all.x=F, all.y=F)                  

newData4 = merge(newData3, myData4,       
                 by.x="Country", by.y="Country", 
                 all.x=F, all.y=F)

newData5 = merge(newData4, myData5,       
                 by.x="Country", by.y="Country", 
                 all.x=F, all.y=F)

newData6 = merge(newData5, myData6,
                 by.x="Country",by.y="Country",
                 all.x=F,all.y=F)

newData7 = merge(newData6, myData7,
                 by.x="Country",by.y="Country",
                 all.x=F,all.y=F)

Energy = merge(newData7, myData8,       
                 by.x="Country", by.y="Country", 
                 all.x=F, all.y=F)



remove(myData,myData1,myData2,myData3,myData4,myData5,myData6,myData7,
       myData8,newData,newData1,newData3,newData4,
       newData5,newData6,newData7)  # Remove data after merging

# Summarize Data

Energy = Energy[,-c(2,5,8,10,12,14,15)]  # Eliminate unnecessary observations
 
summary(Energy)         # Summary of full data set
dim(Energy)             # Dimensions of full dataset

# =============================================================================
# Regression Modeling
# =============================================================================


# This regression modeling is done looking at a sample size of 25
# states.
# This is a regular OLS model regression
# The model is first used to test the relationship of all variables with CO2.

Model1 = lm(CO2 ~ TNP + Region + SFI + POP+ POL + EI + COAL, 
            data = Energy)
summary(Model1)                 # Summary of Model 1
plot(Model1)
     
step(Model1)                    # Use step regression

str(Model1)

# Calculate new regression based on the step function.

Energy$lEI = log(Energy$EI)               # Create logarithmic variable
Energy$TNP2 = (Energy$TNP/1000)           # Divide by 1000. More beneficial
                                          # for graphs.
Model2 = lm(CO2 ~ TNP2 + Region + EI + COAL +POP,
            data = Energy)                # Create Second Model with 
                                          # dropped variables.
summary(Model2)                 # Summary of Model 2
plot(Model2)                    # Plot of Model 2

# The next plot looks at the fitted residuals of Model 2. This will be 
# useful in validating the residuals of the model.

plot(fitted(Model2), residuals(Model2),
     xlab = "Fitted Values", ylab = "Residuals", main = "Fitted Values of Model 2")
abline(h=0, lty=2)
lines(smooth.spline(fitted(Model2), residuals(Model2))) # Create a line going through
                                                        # the center
text(Model2$resid, labels=Energy$Country, cex=0.5) # Label the Country Names


plot(Energy$Region,Energy$CO2,                # Plot Region and CO2
     main = "Regions with Nuclear Reactors",  # Give Title
     xlab = "Region",                         # Label x-axis
     ylab = "CO2 Emissions")                  # Label y-axis
text(Energy$Region, Energy$CO2, labels=Energy$Country, cex=0.6)
# Label the Country Names


# Plot relationship between CO2 emissions and Nuclear Electricity generation
# for the 2010.

plot(Energy$TNP2,Energy$CO2,
     main="Nuclear Electricity & CO2",         # Create labels
     xlab="Nuclear Electricity Generation",
     ylab="CO2 Emissions",
     col="red", pch=19, cex=1,lty="solid",lwd=2)
text(Energy$TNP2, Energy$CO2, labels=Energy$Country, cex=0.7) # Label Country Names
abline(lm(Energy$CO2~Energy$TNP2),col="black")            # Graph slope
legend(x=14,                          # Create Legend
       y=700,
       legend = c("TNP2"),
       lty=1,
       col=c("black")
)
lm(formula = Energy$CO2 ~ Energy$TNP2) # Formula of relationship
text(x = 600,                           # x-axes
     y = 170,                         # y-axes
     "y=.01x+5.8")                     # Insert text to axes coordinates



# Create histogram with density plot to further validate Model 2.

hist(Model2$residuals,
     main = "Histogram of Model 2 Residuals", # Create Labels
     xlab = "Residuals",
     ylab = "Frequency")
m<-mean(Model2$residuals)                     # Calculate mean of resid
std<-sqrt(var(Model2$residuals))              # Calculate std of resid
hist(Model2$residuals, density=.00000000005, breaks=10, prob=TRUE, 
     xlab="Residuals", ylim=c(0,0.5),   # Create limitations
     main="Model 2 Histogram")
curve(dnorm(x, mean=m, sd=std),         # Create normal distribution curve
      col="darkblue", lwd=2, add=TRUE, yaxt="n") # Make dark blue

     
hist(rstandard(Model2))            # Model standardized residuals
hist(rstandard(Model2), breaks=20) # A little finer grained 

# Plot Population with Carbon Dioxide Emissions

# Transform POPulation to logarithmic function

Energy$lPOP = log(Energy$POP)

plot(Energy$lPOP,Energy$CO2, 
     main="Log(POP) & CO2",               # Create labels
     xlab="Population",
     ylab="CO2 Emissions",
     col="red",pch=19, cex=1,lty="solid",lwd=2)       # Make color red
abline(lm(Energy$CO2~Energy$lPOP),col="black") # Create abline
legend(x=19,                         # Create legend
       y=13,
       legend = c("POP"),
       lty=1,
       col=c("black")                # Make color black
)
lm(formula = Energy$CO2 ~ Energy$SFI) # Calculate formula
text(x = 16,                     # Insert formula into plot
     y = 14,
     "y=-0.5x+8.5")
text(Energy$lPOP, Energy$CO2, labels=Energy$Country, cex=0.7) # Label Country Names

# Plot the statistical relationship between CO2 and COAL.

plot(Energy$COAL,Energy$CO2,        # Plot the relationship between COAL and CO2
     main="CO2 & COAL",             # Create Labels
     xlab="Electricity Production from Coal",
     ylab="CO2 Emissions",
     col = "red",pch=19, cex=1,lty="solid",lwd=2)
abline(lm(Energy$CO2~Energy$COAL),col="black") # Draw abline
legend (x=10,                                  # Create legend
        y=85,
        legend = c("COAL"),
        lty=1,
        col=c("black")          # Make black
        )
lm(formula = Energy$CO2 ~ Energy$COAL) # Calculate slope formula
text(x = 80,                           # Insert formula into plot
     y = 12,
     "y=0.05x+5.6")
text(Energy$COAL, Energy$CO2, labels=Energy$Country, cex=0.7)
# Label Country Names

# Combine Total nuclear production with region to examine the variation. A log of 
# TNP2 is necessary because of France and the United States.
Energy$lTNP2 = log(Energy$TNP2)    # Calculate log of TNP2

library(ggplot2); qplot(Energy$lTNP2*Energy$Region    # Install ggplot2
                        , Energy$CO2, data=Energy, colour=Energy$Region,
                        main = "CO2 Emissions by Region and Level of TNP",
                        xlab = "log(TNP2)",
                        ylab = "CO2 Emissions",
                        cex=0.7,
                        label(Energy$Country)) # Label Country Names
text(Energy$lTNP2*Energy$Region,Energy$CO2, labels=Energy$Region, cex=0.7,pos=2)

# Construct dichotomous model

mean(Energy$CO2)            # Take mean of CO2
median(Energy$CO2)          # Take median of CO2
Energy$CO2d=                 # Create a new dichotomous Energy Variable
  ifelse(Energy$CO2>6.9,1,0) # Create dichotmous variable
Model3 = lm(CO2d ~ TNP2 + Region + POP + EI + COAL, # Model 3
            data = Energy)
summary(Model3)             # Summary of Model 3

plot(Energy$EI,Energy$CO2d,        # Plot EI and CO2d
     main = "High CO2 v. Low CO2", # Labels
     xlab = "Energy Intensity",
     ylab = "Dichotomous CO2")
text(Energy$CO2d,Energy$EI, labels=Energy$Country, cex=0.7) # Label Country Names


# Create a Logit Model with Dichotomous Dependent Variable

Model4=glm(                           # Set up glm model
  Energy$CO2d~Energy$TNP2+
    Energy$Region+Energy$POP+
    Energy$EI+Energy$COAL,
  family = binomial(link="logit"),
  na.action= na.omit)

summary(Model4)                     # Summary of the Model

hist(Model4$fitted)                  # Create fitted histogram of 
                                     # Model 4

qplot(Energy$CO2d,Energy$COAL,      # Plot relationship of CO2d and COAL
     main="COAL & CO2",               # Create labels
     xlab="COAL Usage",
     ylab="High or Low CO2 Emitted",
     col="red",pch=19, cex=1,lty="solid",lwd=2)
text(Energy$COAL, Energy$CO2d,        # Label Country Names
     labels=Energy$Country, cex=0.7)


# Create a Poisson regression for a count-based dependent variable. This is to
# show the use of the model. The data does not contain any significant count
# variables.

Model5=glm(                           # Set up glm model
  Energy$CO2~Energy$TNP2+
    Energy$Region+Energy$EI+
    Energy$POP+Energy$COAL,
  family = poisson,
  data = Energy)
summary(Model5)
plot(Model5$fitted.values,Model5$residuals) # Plot fitted residuals of
                                            # Model 5
plot(Model5)


# Construct a binomial model for a dependent variable that is measured as a 
# proportion. The binomial is created by dividing CO2 by GDPk.

Energy$CO2b = (Energy$CO2/Energy$GDPk)     # Create proportional CO2 variable
Model6=glm(CO2b~TNP2+Region+EI+POP+COAL,
           family=binomial, data=Energy)
summary(Model6)                    # Summary of Model 6
plot(Model6, ask=F)                # Plot Model 6
step(Model6)


# Construct a Pairs plot to show relationship between data

Energy1 = Energy[,c(1,3,4,8,9,10,14)]  # Include necessary variables.

# Create a pairs plot with regression lines and correlation matrix.

panel.lm=function(x, y, ...){          # First a function for regression lines
  points(x, y, ...)                  # Plot all of the points
  abline(lm(y~x),col="blue")         # Add the lines
}                                      # End function

panel.cor=function(x,y, ...){          # Now a function to add correlations
  usr=par("usr")                     # Save the current usr parameters
  on.exit(par(usr))                  # Revert to usr pars on function exit
  par(usr = c(0, 1, 0, 1))           # Set par to 0-1 coordinate system
  r=round(cor(x,y,                   # Calculate correlations to 2 digits
              use="pairwise.complete.obs"),2)  #   using pairwise controls for NAs
  text(0.5,0.5,r, cex=2)             # Paste correlation using 01 coordinates
}                                      # End function

pairs(formula= ~CO2+TNP2+POP+Region+EI+COAL, # Create a pairs plot
      data=Energy,
      lower.panel=panel.lm,
      upper.panel=panel.cor,
      main = "Pairs Plot")                 # Labels


# Graphical Analysis of the remaining variables
hist(Energy$TNP2)
hist(Energy$SFI)
hist(Energy$POL)
hist(Energy$CO2)
hist(Energy$lEI)

remove(Energy,Energy1)

# Create a time series analysis. The same variables are used as the
# initial dataset.

myData = read.csv("Nuclear.csv", header = T) # Upload nuclear data
myData = myData[,-c(2,4,6)]                  # Clean Data
colnames(myData) = c("Country","Year","TNP") # Create column names

summary(myData)                              # Gather Basic Statistics

myData = subset(myData, myData$Country %in%varlist.1) # Use var.list.1
# on data
sd(myData$TNP, na.rm = FALSE)                #Calculate Standard Deviation
myData = subset(myData, myData$Country %in% varlist.3)

# Economic Development = GDPk
myData3 = read.csv("GDPk.csv", header = T)
myData3 = myData3[,c(1,2,4)]
colnames(myData3) = c("Country","Year","GDPk")

myData3 = subset(myData3, myData3$Country %in%varlist.1)

summary(myData3)
sd(myData3$GDPk, na.rm = FALSE)
myData3 = subset(myData3, myData3$Country %in% varlist.3)

# Carbon Dioxide Emissions, thousand metric tons = CO2
myData5 = read.csv("CO2.csv", header = T)   # Upload CO2 data
colnames(myData5) = c("Country","Year","CO2") # Create Column Names
summary(myData5)

# Use lists to observe necessary states

myData5 = subset(myData5, myData5$Country %in% varlist.1)
sd(myData5$CO2, na.rm = FALSE)
myData5 = subset(myData5, myData5$Country %in% varlist.3)

# Population - Total Population of the State = POP
myData7 = read.csv("POP.csv",          # Upload POPd data
                   stringsAsFactors=F,  # Don't allow string to factor conversion
                   na.strings = "NA",
                   header= T)            # Data has headers)
colnames(myData7) = c("Country","Year", "POP")  # Create Column Names
myData7 = subset(myData7, myData7$Country %in% varlist.3)

# Merge the Data to create a TS merge to create a Model of France. Their use of 
# nuclear energy is higher than any other state in the world. Because their use
# of nuclear energy is high, they have more of a chance to cause a decline on the CO2
# variable.

newData = merge(myData, myData3,      # The two datasets to merge
                by.x="Year", by.y="Year",
                all.x=F, all.y=F)           # By Which observations to keep

newData1 = merge(newData, myData7,
                 by.x="Year", by.y="Year",
                 all.x=F, all.y=F)


NUK = merge(newData1, myData5,       
                 by.x="Year", by.y="Year", 
                 all.x=F, all.y=F)

install.packages("tseries")    # Install Time Series
library(tseries)
NUK = NUK[,-c(4,6,8)]          # Eliminate unnecessary observations 


# Transform the Year Data to as.POSIXct.

head(NUK$Year)

as.character(NUK$Year)
NUK$Year = as.POSIXct(strptime(NUK$Year, format="%Y"))
NUK$Year=as.POSIXct(NUK$Year)       # Create as.POSIX year data

layout(matrix(c(0,1,2,3,0),        # Layout matrix for 3 plots
              ncol=1),             #   all in one column
       heights=c(.25,1,1,1,.5))    # Set plot heights
layout.show(3)                     # Show layout
par(mai=c(0,.5,0,.25))             # Set margins of plots

plot.ts(NUK$Year,NUK$CO2,
        type="l", xaxt="n",           #   with no x axis
        xlab="", ylab="") 
my.usr = par("usr")                    # Remember coordinate system
par(usr=c(0,1,0,1))                    # Set coordinates to (0,1) space
text(x=.15,y=.15,                      # Overlay a label
     label="CO2 Emissions")

plot.ts(NUK$Year,NUK$TNP,
        type="l", xaxt="n",            #   with no x axis
        xlab="", ylab="")              #   and no x or y labels
par(usr=c(0,1,0,1))                    # Set coordinates to (0,1) space
text(x=.15,y=.75,                      # Overlay a label
     label="Total Nuclear Production")

plot.ts(NUK$Year,NUK$GDPk,
        type="l", xaxt="n",                  #   with no x axis
        xlab="", ylab="")                    #   and no x or y labels
par(usr=c(0,1,0,1))                    # Set coordinates to (0,1) space
text(x=.15,y=.75,                      # Overlay a label
     label="GDPk")

par(usr=my.usr)                       # Reset usr coordinates to years

axis.POSIXct(side=1,                           # Add an x axis with dates
          x=NUK$Year,                       # 
          at=seq(min(NUK$Year),             # Put dates at each month from
                 max(NUK$Year),             #   min to max date 
                 by="year"),                #   by month
          format="%Y",                      # Format yy-mm
          las=2)                            # Rotate labels perpendicular to axis

par(mfrow=c(1,1))                          # Change layout to default
old.par <- par(mar = c(5, 4, 4, 2))
old.par

hist(NUK$CO2)                          # Gain an understanding of
                                       # the distributions.
hist(NUK$GDPk)
hist(NUK$TNP)

mod1 = lm(CO2 ~ TNP + GDPk, data = NUK) # Regression of mod1
summary(mod1)                           # Summary of mod1
step(mod1)
plot(mod1)                              # Plot mod1


# The adf.test is used to test for trending. Each of the variables
# below shows trending.

adf.test(NUK$CO2[!is.na(NUK$CO2)])
adf.test(NUK$TNP[!is.na(NUK$TNP)]
)
adf.test(NUK$GDPk[!is.na(NUK$GDPk)])

# -------------------------------------------------------------------------------
NUK$dTNP = c(NA,                     # Here we difference the TNP measure
              diff(NUK$TNP,lag=1))   # Adding a blank first observation
NUK$dTNP = as.double(             # Moving average of differenced TNP
  filter(NUK$dTNP,
         rep(1/4,4),                          # Moving average of 4 periods
         sides=1))                            # Looking backwards

adf.test(                             # Check stationarity of differenced
  NUK$dTNP[!is.na(NUK$dTNP)])  #   moving average
# -------------------------------------------------------------------------------



NUK$dGDPk = c(NA,                     # Here we difference the GDPk measure
              diff(NUK$GDPk,lag=1))   # Adding a blank first observation
NUK$dGDPk = as.double(               # Moving average of differenced GDPk
  filter(NUK$dGDPk,
         rep(1/4,4),                  # Moving average of 4 periods
         sides=1))                    # Looking backwards

adf.test(                             # Check stationarity of differenced
  NUK$dGDPk[!is.na(NUK$dGDPk)])       # moving average

# ---------------------------------------------------------------------------------
NUK$dCO2 = c(NA,
             diff(NUK$CO2,lag=1))
NUK$dCO2 = as.double(
  filter(NUK$dCO2,
          rep(1/4,4),
          sides = 1))
adf.test(
  NUK$dCO2[!is.na(NUK$dCO2)])

# Test the transformed variables based on the differences.

mod3 = lm(dCO2 ~ dTNP + dGDPk, data = NUK)
summary(mod3)
# These models are still found to not be significant. 
# They will have to go through another lag to make the
# model more stationary.


# Transform the TNP variable so there is not a trend


adf.test(NUK$dTNP[!is.na(NUK$dTNP)])     # Note the original difference

NUK$tTNP = (NUK$dTNP/NUK$dPOP)

adf.test(NUK$tTNP[!is.na(NUK$tTNP)])


NUK$ddTNP = c(NA,                     # Here we difference the dTNP measure
             diff(NUK$dTNP,lag=1))                # Adding a blank first observation
NUK$ddTNP = as.double(             # Moving average of differenced dTNP
  filter(NUK$ddTNP,
         rep(1/4,4),                          # Moving average of 4 periods
         sides=1))                            # Looking backwards
as.numeric(NUK$ddTNP)

adf.test(                             # Check stationarity of differenced
  NUK$ddTNP[!is.na(NUK$ddTNP)])       # moving average
# With this test done, we can have some confidence in the significance of TNP.

# Next we try to get stationarity for the CO2 variable

NUK$tCO2 = (NUK$CO2/NUK$GDPk)

NUK$dCO2 = c(NA,
               diff(NUK$tCO2,lag=1))
NUK$dCO2 = as.double(
  filter(NUK$dCO2,
         rep(1/4,4),
         sides = 1))

adf.test(
  NUK$dCO2[!is.na(NUK$dCO2)]
)

NUK$ddCO2 = c(NA,
             diff(NUK$dCO2,lag=1))
NUK$ddCO2 = as.double(
  filter(NUK$ddCO2,
         rep(1/4,4),
         sides = 1))

adf.test(
  NUK$ddCO2[!is.na(NUK$ddCO2)]
)

# This also shows a stationary variable. 
# The p-value for ddTNP is approximately .05 and the p-value for ddCO2p which uses
# an economic control.

layout(matrix(c(0,1,2,0),        # Layout matrix for 2 plots
              ncol=1),                             #   all in one column
       heights=c(.25,1,1,.5))         # Set plot heights
layout.show(2)                         # Show layout
par(mai=c(0,.5,0,.25))                 # Set margins of plots

plot.ts(NUK$Year,NUK$ddCO2,
        type="l", xaxt="n",                  #   with no x axis
        xlab="", ylab="") 
my.usr = par("usr")                    # Remember coordinate system
par(usr=c(0,1,0,1))                    # Set coordinates to (0,1) space
text(x=.15,y=.15,                      # Overlay a label
     label="CO2 Emissions")

plot.ts(NUK$Year,NUK$ddTNP,
        type="l", xaxt="n",                  #   with no x axis
        xlab="", ylab="")                    #   and no x or y labels
par(usr=c(0,1,0,1))                    # Set coordinates to (0,1) space
text(x=.15,y=.75,                      # Overlay a label
     label="Total Nuclear Production")


par(usr=my.usr)                       # Reset usr coordinates to years

axis.POSIXct(side=1,                           # Add an x axis with dates
             x=NUK$Year,                       # 
             at=seq(min(NUK$Year),             # Put dates at each month from
                    max(NUK$Year),             #   min to max date 
                    by="year"),                #   by month
             format="%Y",                      # Format yy-mm
             las=2)  

mod4 = lm(NUK$ddCO2~NUK$ddTNP, data = NUK)
summary(mod4)
# The model is found to be not significant; however, the amount of trending
# left after multiple lags has decreased drastically.
