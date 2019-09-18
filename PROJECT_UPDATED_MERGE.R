################################################
# IST687, Final Project
#
# Student name: Ishita Joshi,  Leah Singer, Wei Mu, Xin Sun, Zhenlei Liu
# Date due: April 25
# Date submitted: April 25

# Attribution statement: (choose the one statement that is true)
# We did this homework with help from the book and the 
# professor and these Internet sources: <provide the urls>
# https://ggplot2.tidyverse.org/reference/
# rattle()
# and other R documents

# Run these three functions to get a clean test of homework code
dev.off() # Clear the graph window
cat('\014') # Clear the console
rm(list = ls()) # Clear all user objects from the environment!!!

# Set working directory 
# Change to the folder containing your homework data files
setwd("~/IntroDataScience/")

# Your homework specific code goes below here

if(!requireNamespace("ggplot2")) install.packages("ggplot2")
if(!requireNamespace("dplyr")) install.packages("dplyr")
if(!requireNamespace("rpart")) install.packages("rpart")
if(!requireNamespace("rattle")) install.packages("rattle")
if(!requireNamespace("ROSE")) install.packages("ROSE")
if(!requireNamespace("arules")) install.packages("arules")
if(!requireNamespace("arulesViz")) install.packages("arulesViz")
library(dplyr)
library(ggplot2)
library(rpart)
library(rattle)
library(ROSE)
library(arules)
library(arulesViz)

df <- read.table("spring19survey.csv", sep = ',', header = TRUE)
str(df)

# There are 190720 complete cases, 4133 cases contain "NA" answers. 
sum(complete.cases(df))

# Average satisfaction between cancelled or not
df %>% group_by(Flight.cancelled) %>% summarise(mean(Satisfaction, na.rm = T))

# A table which pivots into incomplete cases and cancelled cases
df_whole_addcomplete <- df
df_whole_addcomplete$ifNA <- as.factor(!complete.cases(df_whole_addcomplete))
df_whole_addcomplete %>% group_by(ifNA) %>% count(Flight.cancelled==levels(Flight.cancelled)[2]) #Flight Cancelled

# drop incomplete cases
df <- df[complete.cases(df),]

# Examine satisfaction score
hist(df$Satisfaction)
table(df$Satisfaction)

# >=4 as Satisfied, <4 as not satisfied
df_addif <- df
df_addif$ifSatisfied <- as.factor(ifelse(df$Satisfaction>=4, "Sat", "NotSat"))
# balanced between two categories
ggplot(df_addif, aes(ifSatisfied)) + geom_bar()

# =5 as very satisfied, <5 as not very satisfied
df_addif$ifSatisfied <- as.factor(ifelse(df$Satisfaction==5, "VerySat", "NotVerySat"))
# extremely unbalanced
ggplot(df_addif, aes(ifSatisfied)) + geom_bar()

# For now we define >=4 as Satisfied, <4 as not satisfied
df_addif$ifSatisfied <- as.factor(ifelse(df$Satisfaction>=4, "Sat", "NotSat"))

# Examine Partner companies, 14 in total
summary(df_addif$Partner.Code)
str(df_addif)
# Very different number of responses
ggplot(df_addif, aes(x=Partner.Code, fill=ifSatisfied)) + geom_bar()
df_addif$TFSatisfied <- as.factor(ifelse(df_addif$Satisfaction>=4, TRUE, FALSE))
str(df_addif)
# Satisfied percent for companies
# result: pretty much the same between different partner companies, we can combine them together
group_by(df_addif, Partner.Code) %>% 
  summarise(Satisfied.Percent = sum(TFSatisfied==TRUE)/n()) %>%
  ggplot(aes(x=reorder(Partner.Code, Satisfied.Percent), y=Satisfied.Percent)) + 
  geom_bar(stat = 'identity') + 
  xlab("Partner.Code")

# convert partner code to name
Partner.Coversion.Table <- group_by(df_addif, Partner.Code) %>% 
  summarise(Partner.Name[1])
Partner.Coversion.Table

# average raw satisfaction score of partner companies, almost the same
group_by(df_addif, Partner.Code) %>% 
  summarise(Satisfaction.Score = sum(Satisfaction)/n()) %>%
  ggplot(aes(x=reorder(Partner.Code, Satisfaction.Score), y=Satisfaction.Score)) + 
  geom_bar(stat = 'identity') + 
  xlab("Partner.Code")

# Decision Tree
# Split into training and test data
drop.cols <- c('Day.of.Month', 'Year.of.First.Flight', 'Flight.date', 'Partner.Code', 'Partner.Name', 'Orgin.City', 'Origin.State', 'Destination.City', 'Destination.State')
df_cleared <- df %>% select(-drop.cols)
df_cleared$ifSatisfied <- as.factor(ifelse(df_cleared$Satisfaction>=4, "Sat", "NotSat"))
df_cleared <- df_cleared %>% select(-Satisfaction)
str(df_cleared)

## Perform sampling on the dataset
set.seed(42)
sample.size <- round(0.70*nrow(df_cleared))
indexes <- sample(1:nrow(df_cleared),size = sample.size,replace = F)
training.data <- df_cleared[indexes,]
testing.data  <- df_cleared[-indexes,]
str(testing.data)

mytree <- rpart(ifSatisfied ~.,
                data = df_cleared, 
                parms = list(split= 'gini'), 
                method = "class", control=rpart.control(minsplit=2, minbucket = 1, cp = 0.01))

# Complexity parameter
plotcp(mytree)
# Plot the tree
fancyRpartPlot(mytree)

# Try expanding the tree
mytree2 <- rpart(ifSatisfied ~.,
                data = df_cleared, 
                parms = list(split= 'gini'), 
                method = "class", control=rpart.control(minsplit=2, minbucket = 1, cp = 0.005))
# Complexity parameter, not much difference when cp<=0.01
plotcp(mytree2)
# Plot the tree
fancyRpartPlot(mytree2)

# Test on testing dataset for cp=0.01 tree
predictions <- predict(mytree,testing.data, type = "class")
# ROC: 0.78
roc.curve(testing.data$ifSatisfied, predictions)

# Verify the conclusion that personal travelers are not satisfied using descriptive statistics
df_temp <- df_cleared %>% 
  group_by(Type.of.Travel, ifSatisfied) %>% 
  count() %>% 
  mutate(flag = as.integer(ifSatisfied)*2-3) %>%
  mutate(Response = n*flag)
ggplot(df_temp, aes(x=Type.of.Travel, y=Response, fill=ifSatisfied)) +
  geom_bar(stat = 'identity') +
  ylab("Number of Responses")

# PCA
# Note some of the following code is merged from rattle log
str(df)
drop.cols <- c('Day.of.Month', 'Year.of.First.Flight', 'Flight.date', 'Partner.Code', 'Partner.Name', 'Orgin.City', 'Origin.State', 'Destination.City', 'Destination.State')
df_whole <- df %>% select(-drop.cols)
str(df_whole)

numeric_attr   <- c("Age", "Price.Sensitivity", "Flights.Per.Year",
               "Loyalty", "Total.Freq.Flyer.Accts",
               "Shopping.Amount.at.Airport",
               "Eating.and.Drinking.at.Airport",
               "Scheduled.Departure.Hour",
               "Departure.Delay.in.Minutes",
               "Arrival.Delay.in.Minutes",
               "Flight.time.in.minutes", "Flight.Distance")
set.seed(42)
nobs <- nrow(df_whole)
train <- sample(nobs, 0.7*nobs)
pc <- prcomp(na.omit(df_whole[train, numeric_attr]), scale=TRUE, center=TRUE, tol=0)
# Show the output of the analysis.
pc

# Summarise the importance of the components found.
summary(pc)

# Display a plot showing the relative importance of the components.
plot(pc, main="")
title(main="Principal Components Importance",
      sub=paste("Rattle", format(Sys.time(), "%Y-%b-%d %H:%M:%S"), Sys.info()["user"]))
axis(1, at=seq(0.7, ncol(pc$rotation)*1.2, 1.2), labels=colnames(pc$rotation), lty=0)


# gender and age analysis
g <- ggplot(data = df, aes(Gender))
# Number of responses from different genders in the entire survey. Note that responses from female customers are more than male responses.
g + geom_bar()
# If we define Satisfaction >= 4 as "satisfied", otherwise "not satisfied"
df$ifSatisfied <- as.factor(ifelse(df$Satisfaction>=4, "Yes", "No"))
# Stacked bar plot of customer satisfaction and gender
# Note the difference between genders. Male customer seems to be more likely to give higher satisfaction score.
g <- ggplot(data = df, aes(Gender))
g + geom_bar(aes(fill=ifSatisfied))
# Show the stacked histogram of number of customer responses versus ages and gender 
ggplot(df, aes(Age, fill = Gender)) + geom_histogram(binwidth = 5)
# If we define Age < 30 as "Young", 30 <= Age < 60 as "Middle" age, Age >= 60 as "old" age
df$ageRange <- ifelse(df$Age < 30, "Young",
                      ifelse(df$Age < 60, "Middle", "Old"))
df$ageRange <- as.factor(df$ageRange)
# 34527 Young age responses, 111744 Middle age responses, 44449 Old age responses
summary(df$ageRange)

# Stacked bar plot of number of customer responses versus age and gender
ggplot(data = df, aes(ageRange)) + geom_bar(aes(fill = Gender))

# Stacked bar plot of customer satisfaction and age
# Note the difference between different age range. Old customers(Age >=60) are likely to give lower satisfaction score,
# while Middle age customers are more likely to give higher satisfaction score.
ggplot(data = df, aes(ageRange)) + geom_bar(aes(fill = ifSatisfied)) 


# Linear regression
checkage<- lm(Satisfaction ~ Age + I(Gender), data = df)
# Note to Ishita: if you write as Satisfaction ~ Age + Gender rather than I(rather), it gives the same result.
# It is a dummy variable.

summary(checkage)
# The coefficient of Age is negative and the coefficient of I(Male) is positive.
# The overall regression is significant with p value < 2.2e-16. Both Age and I(Male) are significant with p<2e-16. 
# So the interpretation is:
# With other factors constant, if the customer has a higher age, the customer gives lower satisfaction score.
# If everything else is the same, male customers gives higher satisfaction score.
# In general, the conclusion is the same as conclusions from the visualized graphs above.


# ===================================================================================================================
# ===================================================================================================================
# ===================================================================================================================
# Part 2 of the code from a different group member
# remove everything first but keep the library
rm(list=ls())

# ===================================================================================================================

df <- read.csv("spring19survey.csv") # read from csv file
str(df) 
summary(df)

# Preparing of the data for futher analysis
df <- df[-which(is.na(df$Satisfaction)),] # remove NA value in stisfaction


# Flight per year 
df.flightlot <- df[df$Flights.Per.Year>17,]
df.flightlot2 <- df[df$Flights.Per.Year>29,]

# Price sensitivity
summary(df$Price.Sensitivity)
df.price <- df[df$Price.Sensitivity > 2,]
dim(df.price)
summary(df.price$Satisfaction)

# Departure Delay 
summary(df$Departure.Delay.in.Minutes)
df.D.delay <- df[df$Departure.Delay.in.Minutes>60,]
dim(df.D.delay)
summary(df.D.delay$Departure.Delay.in.Minutes)

# Arrival Delay
summary(df$Arrival.Delay.in.Minutes)
df.A.delay <- df[df$Arrival.Delay.in.Minutes>480,]
dim(df.A.delay)
summary(df.A.delay$Arrival.Delay.in.Minutes)

# Airline Status
summary(df$Airline.Status)
Airline.Blue <- filter(df, Airline.Status == "Blue")
Airline.Silver <- filter(df, Airline.Status == "Silver")
Airline.Gold <- filter(df, Airline.Status == "Gold")
Airline.Platinum <- filter(df, Airline.Status == "Platinum")

# Data visualization

# function for ploting
plot_pie <- function(df,title){
  cust5 <- filter(df, Satisfaction == 5)
  cust4 <- filter(df, Satisfaction == 4)
  cust3 <- filter(df, Satisfaction == 3)
  cust2 <- filter(df, Satisfaction == 2)
  cust1 <- filter(df, Satisfaction == 1)
  
  t1 <- c(dim(cust5)[1],dim(cust4)[1],dim(cust3)[1],dim(cust2)[1],dim(cust1)[1])
  t1 <- t1/sum(t1)
  print(t1)
  p1 <- pie(t1,label = paste(round(t1*100),"%"), main = title, col = rainbow(length(t1)))
  legend("topright",c("Satisfaction = 5", "Satisfaction = 4","Satisfaction = 3",
                      "Satisfaction = 2","Satisfaction = 1"),cex = 0.8, fill = rainbow(length(t1)) )
  return(p1)
}

plot_hist <- function(df){
  g1 <- ggplot(df)+ aes(x=Satisfaction)+geom_histogram(binwidth=0.5)
  # +ggtitle("Hist of Satisfaction")
  return(g1)
}






p <- plot_pie(Airline.Blue,"Satisfaction Distribution of Blue")
p <- plot_pie(Airline.Silver,"Satisfaction Distribution of Sliver")
p <- plot_pie(Airline.Gold,"Satisfaction Distribution of Gold")
p <- plot_pie(Airline.Platinum,"Satisfaction Distribution of Plantinum")

# Associate rules
# Age, arrive delay, airline status, type of travel, price sensitivity, flight per year
summary(df)
rate1to5 <- function(vec){
  level <- replicate(length(vec), "Average")
  level[vec > 3] <- "High"
  level[vec < 3] <- "Low"
  return(level)
}

ratenum <- function(vec){
  q <- quantile(vec, c(0.4, 0.6))
  level <- replicate(length(vec), "Average") # num = (0.4,0.6) -> average
  level[vec <= q[1]] <- "Low" # num = [0,0.4] -> low
  level[vec > q[2]] <- "High" # num = [0.6,1] -> high
  return(level)
}

ratearrive <- function(vec){
  level <- replicate(length(vec), "Delay over 5 min")
  level[vec < 5] <- "Delay less than 5 min"
  return(level)
}

create_df_arule <- function(df){
  Satisfaction <- df$Satisfaction
  Age <- df$Age
  Airlinestatus <- df$Airline.Status
  Arrivedelay <- df$Arrival.Delay.in.Minutes
  Typeoftravel <- df$Type.of.Travel
  Price <- df$Price.Sensitivity
  Flightsperyear <- df$Flights.Per.Year
  df.arules <- data.frame(Satisfaction,Age,Airlinestatus,Arrivedelay,Typeoftravel,Price,Flightsperyear)
  
  df.arules$Satisfaction <- rate1to5(df.arules$Satisfaction)
  table(df.arules$Satisfaction)
  df.arules$Price <- rate1to5(df.arules$Price)
  table(df.arules$Price)
  df.arules$Flightsperyear <- ratenum(df.arules$Flightsperyear)
  table(df.arules$Flightsperyear)
  df.arules$Age <- ratenum(df.arules$Age)
  table(df.arules$Age)
  df.arules$Arrivedelay <- ratearrive(df.arules$Arrivedelay)
  
  return(df.arules)
}


df.arules <- create_df_arule(df)
summary(df.arules)
str(df.arules)

rulesets <- as(df.arules,"transactions")

itemFrequency(rulesets)
itemFrequencyPlot(rulesets)
r1 <- apriori(rulesets,
              parameter = list(support=.1, confidence = .5),
              appearance = list(default = "lhs",rhs =("Satisfaction=Low")))
summary(r1)
inspect(r1)
plot(r1)

# 
# lhs                               rhs                  support confidence     lift count
# [1] {Typeoftravel=Personal Travel} => {Satisfaction=Low} 0.1582189  0.5129635 2.515812 30825
# [2] {Age=High,                                                                              
# Typeoftravel=Personal Travel} => {Satisfaction=Low} 0.1019479  0.5355371 2.626523 19862
# [3] {Airlinestatus=Blue,                                                                    
# Typeoftravel=Personal Travel} => {Satisfaction=Low} 0.1435391  0.6013203 2.949155 27965
# [4] {Typeoftravel=Personal Travel,                                                          
# Price=Low}                    => {Satisfaction=Low} 0.1545336  0.5124421 2.513255 30107
# [5] {Age=High,                                                                              
# Typeoftravel=Personal Travel,                                                          
# Price=Low}                    => {Satisfaction=Low} 0.1000693  0.5354573 2.626132 19496
# [6] {Airlinestatus=Blue,                                                                    
# Typeoftravel=Personal Travel,                                                          
# Price=Low}                    => {Satisfaction=Low} 0.1400334  0.6020523 2.952745 27282
# 

# Plot overall distribution

p1 <- plot_pie(df,"Overall Satisfaction Distribution")
p2 <- plot_hist(df)
p2

# Plot > mean distribution
p3 <- plot_pie(df.flightlot,"Satifaction Distribution for Flight >17/year")
p4 <- plot_hist(df.flightlot)
p4

# Plot >75% distribution
p5 <- plot_pie(df.flightlot2,"Satifaction Distribution for Flight >29/year")
p6 <- plot_hist(df.flightlot2)
p6


lm.model <- lm(df.A.delay$Satisfaction~df.A.delay$Arrival.Delay.in.Minutes, data = df.A.delay)
summary(lm.model)
plot(df.A.delay$Arrival.Delay.in.Minutes,df.A.delay$Satisfaction)

df.delayover5 <- filter(df, Arrival.Delay.greater.5.Mins == "yes")

df.arules <- create_df_arule(df.delayover5)
summary(df.arules)
str(df.arules)

rulesets <- as(df.arules,"transactions")

itemFrequency(rulesets)
itemFrequencyPlot(rulesets)
r1 <- apriori(rulesets,
              parameter = list(support=.1, confidence = .5),
              appearance = list(default = "lhs",rhs =("Satisfaction=Low")))
summary(r1)
inspect(r1)
plot(r1)
