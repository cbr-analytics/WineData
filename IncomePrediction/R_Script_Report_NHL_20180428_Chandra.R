## ----setup, include=FALSE------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)
options(width=200)

## ----eval=FALSE, tidy=TRUE, tidy.opts=list(width.cutoff=60)--------------
## #install.packages("plyr")
## #install.packages("stringr")
## #install.packages("magrittr")
## #install.packages("scatterplot3d")
## 
## library(tidyverse)
## library(plyr)
## library(magrittr)
## library(stringr)
## 
## library(scatterplot3d)

## ---- tidy=TRUE, tidy.opts=list(width.cutoff=60)-------------------------
setwd("../JB125")


## ---- tidy=TRUE, tidy.opts=list(width.cutoff=60)-------------------------
train.df <- read.csv("./train.csv", header = TRUE)
colnames(train.df)  # Check column names
head(train.df, 5)

## ---- tidy=TRUE, tidy.opts=list(width.cutoff=60)-------------------------
test.df <- read.csv("./test.csv", header = TRUE)
colnames(test.df)     # Check column names
head(test.df, 5)

## ---- tidy=TRUE, tidy.opts=list(width.cutoff=60)-------------------------
dim.data.frame(train.df)
dim.data.frame(test.df)


## ---- tidy=TRUE, tidy.opts=list(width.cutoff=60)-------------------------

#test_x$TrainTest <- "test"
#train_y$TrainTest <- "train"

train_data <- train.df[,-c(1)]
dim(train_data)
test_data <- test.df
dim(test_data)

#test <-cbind(test_y, test_x)
all_data <- rbind(train_data, test_data)

#######
colnames(all_data)
dim(all_data)


## ---- tidy=TRUE, tidy.opts=list(width.cutoff=60)-------------------------

all_missing_list = colnames(all_data)[colSums(is.na(all_data)) > 0]
print(all_missing_list)


## ---- tidy=TRUE, tidy.opts=list(width.cutoff=60)-------------------------
library(plyr)
#fill the Pr.St column with 'INT' for international players
all_data$Pr.St = mapvalues(all_data$Pr.St, from = "", to="INT")


## ----eval = TRUE, tidy=TRUE, tidy.opts=list(width.cutoff=60)-------------
#Make team boolean columns
#get the unique list of team acronymns
teams = c()    # A list
for( i in levels(all_data$Team)){
  x = strsplit(i, "/") # Split the string and store the values as list in "x"
  #print(x)
  for(y in x){
    teams = c(teams, y) # Combine all the values of x in a list "teams"
  #  print(y)
  #  print(teams)
    
  }
}
teams = unique(teams)  # assign unique entires to list teams 
print(teams)

# add columns with the team names as the header and 0 as values
for(j in teams){
  all_data[,j] = 0 # Assign inital values 0 to each new column created in the loop
  print(j)
  
}
head(all_data,5) # Check the new columns created. 
print(all_data$team)
#iterate through and record the teams for each player
for(i in 1:length(all_data$Team)){
  teams_of_person = strsplit(as.character(all_data$Team[i]), "/")[[1]]
  print(teams_of_person)
  for(x in teams_of_person){
    all_data[,x][i] = 1	     # Assign value 1 for each column_team with which player is associated
    #print("hello")
  }
}
print(head(all_data))

## ---- tidy=TRUE, tidy.opts=list(width.cutoff=60)-------------------------
#Make position boolean columns
pos = c()
for( i in levels(all_data$Position)){
	x = strsplit(i, "/")
	for(y in x){
		pos = c(pos, y)
	}
}
pos = unique(pos)
print(pos)

# add columns with the pos names as the header and 0 as values
for(position in pos){
	all_data[,position] = 0
}

#iterate through and record the position(s) for each player
for(i in 1:length(all_data$Position)){
	pos_of_person = strsplit(as.character(all_data$Position[i]), "/")[[1]]
	for(x in pos_of_person){
		all_data[,x][i] = 1	
	}
}
print(head(all_data))

## ---- tidy=TRUE, tidy.opts=list(width.cutoff=60)-------------------------
#turn the born column into 
# an age column 
# 3 integer columns year:month:date
library(stringr)


# Objective: standardize year, month and day and create separate columns for each.
bday_parts = str_split_fixed(all_data$Born, "-", 3)

#adjust year column to account for missing digits
birth_year = c()  # A list created for storing players' year of birth
for(year in bday_parts[,1]){   # Read from the first columns of bday_parts
  if(as.numeric(year) < 10){      ## It is two digit year, so payers born in 21st centry must be younger
    yr = paste("20", year, sep="") # Players born in 21st century 
    birth_year = c(birth_year, yr)   # Store the new values in birth_year
  }else{
    yr = paste("19",year, sep="")  # If player are not born in 21st century append 19 before the year. 
    birth_year = c(birth_year, yr)
  }
}

all_data$birth_year <- as.numeric(birth_year)       # Create separate column for YEAR & add to all_data
all_data$birth_month <- as.numeric(bday_parts[,2])  # Create separate column for MONTH & add to all_data
all_data$birth_day <- as.numeric(bday_parts[,3])   # Create separate column for DAY & add to all_data
head(all_data)


## ---- tidy=TRUE, tidy.opts=list(width.cutoff=60)-------------------------
# split Cntry and Nat to boolean columns

birth_country = levels(all_data$Cntry)
# add columns with the country of birth options
# note the Estonia for Uncle Leo
for(country in birth_country){
	c = paste("born", country, sep="_")

	all_data[,c] = 0
}

#iterate through and record the birth country of each player
for(i in 1:length(all_data$Cntry)){
	birth_country = all_data$Cntry[i]
	c = paste("born", birth_country, sep="_")
	all_data[,c][i] = 1	
}


nationality = levels(all_data$Nat)
for(country in nationality){
	c = paste("nation", country, sep="_")
	all_data[,c] = 0
}

#iterate through and record the birth country of each player
for(i in 1:length(all_data$Nat)){
	nationality = all_data$Nat[i]
	c = paste("nation", nationality, sep="_")
	all_data[,c][i] = 1	
}

head(all_data)

## ---- tidy=TRUE, tidy.opts=list(width.cutoff=60)-------------------------
all_data$undrafted = is.na(all_data$DftRd)

#fill median values
#loop through the dataframe, filling each column with the median of 
#the existing values for the entire dataset
#where are there still missing values?

all_missing_list =  colnames(all_data)[colSums(is.na(all_data)) > 0]

length(all_missing_list) == 0    # Flag to check NA values
#if above true all values are imputed!

for( i in 1:length(all_missing_list)){
	#get the global median
	median_all <- median(all_data[,all_missing_list[i]], na.rm =TRUE) # Neglect NA when calculating #+ median
	print(median_all)
	#imput the missing values with the column's median
	all_data[,all_missing_list[i]][is.na(all_data[,all_missing_list[i]])] <- median_all
}

all_missing_list <- colnames(all_data)[colSums(is.na(all_data))]

length(all_missing_list) == 0   # Flag to check NA values

## ---- tidy=TRUE, tidy.opts=list(width.cutoff=60)-------------------------
hist(all_data$Ht,
     main="Histogram for NHL players' Height", 
     xlab="Height", 
     #border="blue", 
     col="green")

## ---- tidy=TRUE, tidy.opts=list(width.cutoff=60)-------------------------
hist(all_data$Wt,
     main="Histogram for NHL players' Weight", 
     xlab="Weight", 
     #border="blue", 
     col="green")

## ---- tidy=TRUE, tidy.opts=list(width.cutoff=60)-------------------------
hist(all_data$Wt,
     main="Histogram for NHL players' Salary", 
     xlab="Salary", 
     #border="blue", 
     col="green")

## ---- tidy=TRUE, tidy.opts=list(width.cutoff=60)-------------------------
table(all_data$birth_year)

## ---- tidy=TRUE, tidy.opts=list(width.cutoff=60)-------------------------
hist(all_data$birth_year, breaks=28, 
     col="skyblue", xlab='Year of birth', 
     main='Distribution of NHL players by birth year (2016/2017 season)\nA.K.A. Jaromir Jagr the ageless one')

## ---- tidy=TRUE, tidy.opts=list(width.cutoff=60)-------------------------
summary(train.df$Salary)
hist(train.df$Salary, breaks=52,col="salmon", xlab='Salary', 
     ylab = "Number of players", main='NHL Salary Distribution: 2016/2017')

## ---- tidy=TRUE, tidy.opts=list(width.cutoff=60)-------------------------
plot(train.df$G, train.df$Salary, xlab = "No. Goals", pch =20, ylab = 'Money Earned')

abline(lm(train.df$Salary ~ train.df$G), col = 'blue')

## ---- tidy=TRUE, tidy.opts=list(width.cutoff=60)-------------------------
plot(train.df$G, train.df$Salary, pch=20, xlab='goals scored', ylab='money earned', main="Who are the outliers?")
abline(lm(train.df$Salary ~ train.df$G), col="red")
text(train.df$G, train.df$Salary, labels=train.df$Last.Name, cex=0.7, pos = 3)

## ---- tidy=TRUE, tidy.opts=list(width.cutoff=60)-------------------------
#train.df2$Salary <- train.df$Salary
#train.df3 <- cbind(train.df[,1], all_data[1:612,]) 
train.df2 <- all_data[c(1:612),]


train.final <- all_data[c(1:612),]
train.final$Salary <- train.df$Salary

test.final <- all_data[c(613:874),]



## ---- tidy=TRUE, tidy.opts=list(width.cutoff=60)-------------------------
pairs(~ Born+ +  Ht + Wt +DftRd + 
        Position+Team+GP+G , data = train.df2,  main="Simple Scatterplot Matrix")

str(train.final)
str(test.final)

## ---- tidy=TRUE, tidy.opts=list(width.cutoff=60)-------------------------
colnames(train.final)

## ---- tidy=TRUE, tidy.opts=list(width.cutoff=60)-------------------------
cor(train.final$Salary, train.final$birth_year)


## ---- tidy=TRUE, tidy.opts=list(width.cutoff=60)-------------------------
boxplot( train.final$birth_year, data = train.final)


## ---- tidy=TRUE, tidy.opts=list(width.cutoff=60)-------------------------
summary(train.final$birth_year)
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
##    1972    1987    1991    1990    1993    1998
# From above we have 
Q1 <- 1987
Q3 <- 1993
IQR <- Q3-Q1

new_value_birth_year <- Q1 - (1.5 * IQR)
new_value_birth_year


## ---- tidy=TRUE, tidy.opts=list(width.cutoff=60)-------------------------
for(i in 1:nrow(train.final)){
  if(train.final$birth_year[i] < new_value_birth_year )
    train.final$birth_year[i] <- new_value_birth_year
}

## ---- tidy=TRUE, tidy.opts=list(width.cutoff=60)-------------------------
boxplot( train.final$birth_year, data = train.final)


## ---- tidy=TRUE, tidy.opts=list(width.cutoff=60)-------------------------
train.final <- train.final[,-c(1,2,12,13,14,15)]
test.final <- test.final[,-c(1,2,12,13,14,15)]

train.final <- train.final[1:612,]
test.final <- train.final[613:874,]
dim(train.final)
dim(test.final)



## ---- tidy=TRUE, tidy.opts=list(width.cutoff=60)-------------------------
# Normalize Height (Ht)

mean_ht <- mean(train.final$Ht) # Store mean value of the columns
std_ht <- sd(train.final$Ht) # Store the standard deviation of the columns
mean_ht
std_ht
for(i in 1:nrow(train.final)){        # A for loop to compute the normalized value of each row element of given column
 train.final$Ht_n[i] <-(train.final$Ht[i] - mean(train.final$Ht)) / sd(train.final$Ht)
}
head(train.final$Ht_n)

#mean(train.final$Ht_n)


## ---- tidy=TRUE, tidy.opts=list(width.cutoff=60)-------------------------
# Normalize Weight (Wt)

mean_wt <- mean(train.final$Wt) # Store mean value of the columns
std_wt <- sd(train.final$Wt) # Store the standard deviation of the columns
mean_wt
std_wt
for(i in 1:nrow(train.final)){        # A for loop to compute the normalized value of each row element of given column
 train.final$Wt_n[i] <-(train.final$Wt[i] - mean(train.final$Wt)) / sd(train.final$Wt)
}
head(train.final$Wt_n)

#mean(train.final$Ht_n)


## ---- tidy=TRUE, tidy.opts=list(width.cutoff=60)-------------------------
# Normalize Weight (Wt)
mean_birth_year <- mean(train.final$birth_year) # Store mean value of the columns
std_birth_year <- sd(train.final$birth_year) # Store the standard deviation of the columns
mean_birth_year
std_birth_year
for(i in 1:nrow(train.final)){        # A for loop to compute the normalized value of each row element of given column
 train.final$birth_year_n[i] <-(train.final$birth_year[i] - mean(train.final$birth_year)) / sd(train.final$birth_year)
}
head(train.final$birth_year_n)

#mean(train.final$Ht_n)


## ---- tidy=TRUE, tidy.opts=list(width.cutoff=60)-------------------------
lm_model <- lm(Salary ~Ht_n+Wt_n+Ovrl+G+birth_year_n, data = train.final)
lm_model
summary(lm_model)

## ---- tidy=TRUE, tidy.opts=list(width.cutoff=60)-------------------------
predicted_salary <- predict(lm_model, train.final)
#predicted_salary

## ---- tidy=TRUE, tidy.opts=list(width.cutoff=60)-------------------------
actual_preds <- data.frame(cbind(actuals = train.final$Salary, predicteds = predicted_salary))
corrleation_accuracy <- cor(actual_preds)
corrleation_accuracy

head(actual_preds)

## ---- tidy=TRUE, tidy.opts=list(width.cutoff=60)-------------------------
#train.final <- train.final[,-c(1,2,12,13,14,15)]
#test.final <- test.final[,-c(1,2,12,13,14,15)]

data = train.final  # Store the train data before spliting

split_indexes <- sample(1:nrow(data), size = 0.2*nrow(data))

test <- data[split_indexes,]
train <- data[-split_indexes,]

dim(train)
dim(test)



## ---- tidy=TRUE, tidy.opts=list(width.cutoff=60)-------------------------
lm_model <- lm(Salary ~Ht_n+Wt_n+Ovrl+G+birth_year_n, data = train)
lm_model
summary(lm_model)

## ---- tidy=TRUE, tidy.opts=list(width.cutoff=60)-------------------------
predicted_salary <- predict(lm_model, test)


## ---- tidy=TRUE, tidy.opts=list(width.cutoff=60)-------------------------
actual_preds <- data.frame(cbind(actuals = test$Salary, predicteds = predicted_salary))
corrleation_accuracy <- cor(actual_preds)
corrleation_accuracy

head(actual_preds)

## ---- tidy=TRUE, tidy.opts=list(width.cutoff=60)-------------------------


## ---- tidy=TRUE, tidy.opts=list(width.cutoff=60)-------------------------


