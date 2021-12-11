library(tidyverse)
library(dplyr)

#Deliverable 1
### Perform Multiple Regression on MechaCar dataset

# Read in the csv file.
mecha_data <- read.csv("MechaCar_mpg.csv",stringsAsFactors = F,check.names = F)

# Perform a linear regression module 
mecha_lm <- lm(mpg ~ vehicle_length + vehicle_weight + spoiler_angle + ground_clearance + AWD,data=mecha_data)

# Determine the p-value and r-squared of the linear regression module.
summary(mecha_lm)


#Deliverable 2
#read Suspension_coil.csv file
Suspension_Coil <- read.csv(file='Sus.csv',check.names=F,stringsAsFactors = F)

#create total_summary dataframe
total_summary <- Suspension_Coil %>% group_by(Manufacturing_Lot)%>% 

# lot_summary dataframe
summarize(Mean=mean(PSI),Median=median(PSI),Variance=var(PSI),SD=sd(PSI),.groups='keep')

#Deliverable 3
# t.test function
t.test(Suspension_Coil$PSI,mu = 1500)
# t.test No.1
t.test(subset(Suspension_Coil,Manufacturing_Lot=="Lot1")$PSI,mu = 1500)
# t.test No.2
t.test(subset(Suspension_Coil,Manufacturing_Lot=="Lot2")$PSI,mu = 1500)
# t.test No.3
t.test(subset(Suspension_Coil,Manufacturing_Lot=="Lot3")$PSI,mu = 1500)

