 # install.packages("plyr")
 # install.packages("stringr")
 # install.packages("magrittr")
 # install.packages("scatterplot3d")
# ---
#   title: "NHL_prediction_kaggle"
# author: "Chandra Bhushan Roy"
# date: "April 14, 2018"
# output: html_document
# ---
  



## R Markdown

#This is an R Markdown document to predict the salary of National Hockey League players. For more details on using R Markdown see <http://rmarkdown.rstudio.com>.


# install.packages("plyr")
# install.packages("stringr")
# install.packages("magrittr")
# install.packages("scatterplot3d")

library(tidyverse) 
library(plyr)
library(stringr)
library(magrittr)
library(scatterplot3d)
## Set working directory in R

setwd("D:/2018/Upwork/JB125")

train.df <- read.csv("./train.csv", header = TRUE)
colnames(train.df)  # Check column names
head(train.df, 5)




test.df <- read.csv("./test.csv", header = TRUE)
colnames(test.df)     # Check column names
head(test.df, 5)


## Data Cleaning

dim.data.frame(train.df)
dim.data.frame(test.df)



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

###### Missing values
all_missing_list = colnames(all_data)[colSums(is.na(all_data)) > 0]


### Imputation Starts here
which(is.na(all_data$Pr.St))

#fill the Pr.St column with 'INT' for international players
all_data$Pr.St = mapvalues(all_data$Pr.St, from = "", to="INT")


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


# Split and make columns for each position a player plays for. Some players play for multiple
# positons. 

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


#turn the born column into 
# an age column 
# 3 integer columns year:month:date

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

all_data$birth_year = as.numeric(birth_year)       # Create separate column for YEAR & add to all_data
all_data$birth_month = as.numeric(bday_parts[,2])  # Create separate column for MONTH & add to all_data
all_data$birth_day = as.numeric(bday_parts[,3])   # Create separate column for DAY & add to all_data
head(all_data)




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
## Missing values
count(all_missing_list)

all_data$undrafted = is.na(all_data$DftRd)

#fill median values
#loop through the dataframe, filling each column with the median of 
#the existing values for the entire dataset
#where are there still missing values?

all_missing_list =  colnames(all_data)[colSums(is.na(all_data)) > 0]
length(all_missing_list) == 0
#if above true all values are imputed!

for( i in 1:length(all_missing_list)){
  #get the global median
  median_all = median(all_data[,all_missing_list[i]], na.rm =TRUE)
  #imput the missing values with the column's median
  all_data[,all_missing_list[i]][is.na(all_data[,all_missing_list[i]])] = median_all
}


all_missing_list =  colnames(all_data)[colSums(is.na(all_data)) > 0]

length(all_missing_list) == 0

cor(train.df$City, train.df$Salary)



train.df2 <- all_data[c(1:612),]


train.final <- all_data[c(1:612),]
train.final$Salary <- train.df$Salary

test.final <- all_data[c(613:874),]


# EDA

plot(train.df$G, train.df$Salary, xlab = "No. Goals", pch =20, ylab = 'Money Earned')

abline(lm(train.df$Salary ~ train.df$G))
colnames(train.final)
