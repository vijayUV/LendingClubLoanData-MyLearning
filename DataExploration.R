rm(list=ls())
gc()
##setwd("/Volumes/NO NAME/Documents/Course/kaggle/lending-club-loan-data")
setwd("D:/Study/Big Data Analytics/MyWork/DataAnalytics/LendingClubLoanData/Data")
dir()
##.libPaths("H:/Documents/Course/Libraries")
library(readr)
library(dplyr)
library(ggplot2)   # For plotting functions
library(ggthemes)	 # For a variety of plot-themes
library(gridExtra) # Arranging ggplots in a grid
library(lattice)
library(latticeExtra)
library(vcd)
library(choroplethrMaps, quietly = TRUE)
library("zipcode")
library("maps")
library(openintro) #for plots
options(scipen=999)

loan <- read_csv("loan.csv", n_max=100000)
View(loan)
str(loan)
loan$Nterm <- as.factor(loan$term)
class(loan$Nterm)
loan$Ngrade <- as.factor(loan$grade)
loan$Ngrade <- as.factor(loan$sub_grade)
loan$Nemp_title <- as.factor(loan$emp_title)
loan$Nhome_ownership <-as.factor(loan$home_ownership)
loan$Nverification_status <- as.factor(loan$verification_status)
loan$Nloan_status <- as.factor(loan$loan_status)
loan$Nint_rate <- as.factor(cut(loan$int_rate, breaks = c(0,10,15,20,30), labels = c("0% - 10%","10% - 15%","15% - 20%","20%- 30%")))
loan$Nemp_length <- as.factor(loan$emp_length)
loan$Nannual_inc <- as.factor(cut(loan$annual_inc, breaks = c(0,10000,25000,50000,75000,1000000000), labels = c("0-10K","10K - 25K","25K-50K","50K-75K","75K and above")))
loan$Npurpose <- as.factor(loan$purpose)
loan$Ntitle <- as.factor(loan$title)
loan$Naddr_state <- as.factor(loan$addr_state)
loan$Ndti_fact <- as.factor(cut(loan$dti, 5, labels = c("dti1","dti2","dti3","dti4","dti5")))
loan$Ntotal_acc <-as.factor(loan$total_acc)
loan$Ntotal_accC <-as.factor(cut(loan$total_acc, 5, labels = c("cl1","cl2","cl3","cl4","cl5")))
loan$Npymnt_plan <- as.factor(loan$pymnt_plan)
##########Ploting
ggplot(loan, aes(x=loan$Nint_rate))+labs(x="Interest-rate Range", y = "Count")+geom_bar(fill="#06F573")+ggtitle("loans per Interest rate")

ggplot(loan, aes(x=loan$Nint_rate, y= loan$loan_amnt))+labs(x="Interest-rate Range", y = "Amount")+geom_bar(fill="#06B7F5",stat="identity")+ggtitle("Loan Amounts per Interest rate")
table1 <- table(loan$Nannual_inc)
labels <- c(row.names(loan$Nannual_inc))
piepercent <- round(100*table1/sum(table1),1)
piepercent <- paste(piepercent,"%",sep = "")
pie(table1, labels = piepercent, main = "Loans per Annual Income",col=rainbow(length(table1)))
legend("topright", c("0-10K","10K - 25K","25K-50K","50K-75K","75K and above"), cex = 0.8,
       fill = rainbow(length(table1)))
ggplot(loan, aes(x=loan$Npurpose, fill=loan$Nint_rate))+geom_bar()+coord_flip()+ggtitle("Loans per Purpose")
ggplot(loan)+geom_density(aes(x=loan$Nhome_ownership, fill=loan$Nhome_ownership))+labs(x="Home Ownership", y="Density")+guides(fill=guide_legend(title="Ownership"))+ggtitle("Density per Home Ownership")
##Heatplot
ggplot(loan, aes(x=loan$Nint_rate,y=loan$Ngrade, fill=loan$Nannual_inc))+geom_raster()+labs(x="InterestRate Range",y="Grade")+guides(fill=guide_legend(title = "Annual Income"))+ggtitle("Grades per interest rates and Annual Income")

table1 <- table(loan$Nint_rate,loan$Nannual_inc)

mosaic(~Nint_rate+Nterm, 
       data=loan, 
       gp=shading_max,
       split_vertical=T)

ggplot(loan, aes(x=loan$Nverification_status, y=loan$loan_amnt, fill=loan$verification_status))+geom_boxplot()+labs(x="Verification Status", y="Loan Amount", title= "Box plot of verification status wise loan amount" )+guides(fill=(guide_legend(title = "Status")))

ggplot(loan, aes(Nverification_status, ..count..,fill=loan$Npurpose ) ) +
  geom_bar(stat = "count", size = 2) + 
  coord_flip()+
  facet_grid( Nloan_status ~ . )

ggplot(loan, aes(x = factor(""), fill = purpose) ) +  geom_bar() + coord_polar(theta = "y") +
  scale_x_discrete("")+
  facet_grid(loan$grade ~ .)

ggplot(loan,aes(loan$Nemp_length, loan$loan_amnt))+geom_jitter()+facet_grid(loan$Nannual_inc ~ .)
###########Ploting Playground

all_states<- map_data("state")
all_states$reg <- state2abbr(all_states$region)
loan$reg = loan$addr_state
lnmpdata = data.frame(loan_amnt=loan$loan_amnt,reg = loan$reg)
head(lnmpdata)
mapdata = merge(all_states,lnmpdata, by="reg")

ggplot(data = mapdata) + 
  geom_polygon(aes(x = long,y=lat, fill = loan$loan_amnt, group=group), color = "white") + 
  coord_fixed(1.3) +
  guides(fill=FALSE)  # do this to leave off the color legend


ggplot(loan,aes(loan$Nemp_length, loan$loan_amnt))+geom_jitter()+facet_grid(loan$Nannual_inc ~ .)



ggplot(loan,aes(x=loan$Nemp_length)) + geom_line(stat="density") + geom_histogram(fill="blue", colour="black",bins = 50) + expand_limits(y=0) + facet_grid( grade ~ term ) + theme_solarized()
data(zipcode)
loan$newZip <- clean.zipcodes(loan$zip_code)
x <-merge(loan,loan$newZip)
head(x)







table2 = table()




ggplot(loan,aes(x=Nint_rate)) + geom_histogram(fill="blue", colour="black",bins = 50) + expand_limits(y=0) + facet_grid( grade ~ term ) + theme_solarized()


newZip
