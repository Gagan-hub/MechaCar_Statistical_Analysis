#Deliverable 1
#Load the dplyr package
library(dplyr)
table1<- read.csv(file='MechaCar_mpg.csv',check.names=F,stringsAsFactors = F) #Import and read csv file
lm(mpg ~ vehicle_length+ vehicle_weight + spoiler_angle+ground_clearance + AWD,data=table1) #generate multiple linear regression model
summary( lm(mpg ~ vehicle_length+ vehicle_weight + spoiler_angle+ground_clearance + AWD,data=table1)) ## find the p-value and r-squared

#deliverable 2
table2<- read.csv(file='Suspension_Coil.csv',check.names=F,stringsAsFactors = F) #Import and read csv file
#Create a total_summary dataframe using the summarize() function
#to get the mean, median, variance, and standard deviation of the PSI column
total_summary <- table2 %>% summarize(Mean=mean(PSI),Median=(PSI),Variance=var(PSI),SD=sd(PSI))

#Create a lot_summary dataframe using groupby() and summarize() functions 
#to group each lot by the mean, median, variance, and standard deviation of the PSI column
lot_summary <-table2 %>% group_by(Manufacturing_Lot)%>% summarize(Mean=mean(PSI),Median=(PSI),Variance=var(PSI),SD=sd(PSI))



#Deliverable 3

#t-test to determine consistency in sample means
t.test(table2$PSI, mu=1500)

#t-tests for individual lots
t.test(subset(table2, Manufacturing_Lot=="Lot1")$PSI, mu=1500)
t.test(subset(table2, Manufacturing_Lot=="Lot2")$PSI, mu=1500)
t.test(subset(table2, Manufacturing_Lot=="Lot3")$PSI, mu=1500)
