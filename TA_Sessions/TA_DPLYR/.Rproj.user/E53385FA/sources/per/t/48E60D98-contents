# Introduction to DPLYR

# A.1 Working with data in R

# ### A.1.1 What is a Data Frame? 
# - Think of it as an excel sheet with data
# - In many cases:
#   - Rows are observations (e.g. people, households, countries, time)
# - Columns are variables (e.g. GDP, life expectancy)
# 
# ### A.1.2 Why don't we just use excel if Data Frames have similar structures?
# - For many reasons:
#   - We can easily copy dataframes and mainpulate
# - We have access to multiple dataframes with only one line of code
# 
# ### A.1.3 What are the other advantages of using R and dataframes?
# - We can be lazy and use the thousands of free libraries to easily:
#   - Easily manipulate data (Today's topic)
#     - Download data directly from the internet
#     - Viusualize our data (graphing etc.)
#     - Build models (Regression, Machine learning, Neural Networks)
#     
#     
# - You have all used libraries before, perhaps without knowing it!
#     - This is done in R in two steps: 
#         1. install.packages("Package name") Downloads package
#         2. library(Package name) Imports package

### A.1.4 What is the best way to manipulate Data Frames?
# #### In R: DPLYR!
# In python: Pandas

# 1. Basics

## 1.1 Find file [path](https://automatetheboringstuff.com/2e/chapter9/)

# ![title](Images/File_path.png)

path = getwd() #Sets current working directory
print(paste("The current working directory is: ", getwd(), sep=""))

## 1.2 Import files
### 1.2.1 From: current working directory

#### 1.2.1.1 Lets import that file from an internal data set!

data("mtcars") #Download internal data set: cars data

write.csv(mtcars, "cars.csv") #Name the dataset "cars.csv" and save to current working directory

#### 1.2.1.2 Now import "cars.csv" which is now in our current directory

# read.csv("cars.csv")
df = read.csv("cars.csv") #We often name our dataframes df_NAME

### 1.2.2  From: not in our current directory

# We need to know the file path of the file!!

#### 1.2.2.1 Create new directory (folder) and place new data set

dir.create('New_directory') #Creates new folder

setwd("New_directory") #Changes working directory

data("USArrests") #Imports internal data set

write.csv(USArrests, "arrests.csv") #Name the dataset "arrests.csv" and save to current working directory

setwd(path) #Returns to our original file path!!!

getwd() #Checks our path which is not in the folder we just created

#### 1.2.2.2 Import "arrests.csv" from the "New_directory" folder

df_arrests = read.csv("New_directory/arrests.csv")

head(df_arrests, 2)

# 2. DPLYR

# We will use cars data set we named df

## 2.1 Basics

### 2.1.1 Download and import dplyr package

#install.packages("dplyr") #Already installed!
library(dplyr)

### 2.1.2 Convert Data Frame into tibble (DPLYR's special Data Frame)

dp_df = as_tibble(df)

### 2.2 Select columns

#We can list all the column names from a Data Frame
colnames(dp_df)

### 2.2.1 Select one column

dp_df %>% #Dataframe
  select(X)%>% #Select column named X
  head(3) #Shows only first 3 obs

### 2.2.2 Select multiple columns

dp_df %>% #Dataframe
  select(cyl, disp, hp, wt)%>% #Select column named X
  head(2) #Shows only first 2 obs

### 2.2.3 QUESTION: Select columns and create new Data Frame



### 2.2.4 Pull: similar to select expect it only extracts values

dp_df %>% #Dataframe
  pull(var = cyl)

## 2.3 Filter 

head(dp_df,2)

### 2.3.1 Filter by 1 condition

dp_df %>% 
  filter(mpg>22)%>% #filter all observations over 25 mpg
  head(5)

### 2.3.2 Filter by 2 conditions

dp_df %>% 
  filter(mpg>22 & mpg<35) %>% head(5)

### 2.3.3 Filter by gear not equal to 4

dp_df %>% 
  filter(gear!=4)%>% head(2)

### 2.3.4 QUESTION: Filter by vs=1



## 2.4 Arrange

### 2.4.1 Arrange by column (Ascending)

dp_df %>% 
  arrange(mpg)%>% 
  head(3)

### 2.4.2 Arrange  (Descending)

dp_df %>% 
  arrange(desc(mpg))%>% 
  head(3)

## 2.5 Mutate (Create new column)

### 2.5.1 Mutate

dp_df %>% 
  mutate(mpg_10 = mpg/10) %>% head(2)

# #### 2.5.1.1 Mutate: Various functions
# 1. Cumulative sum: cumsum(X)
# 2. Lag: lag(X)
# 3. percent_rank(X)

dp_df %>% 
  mutate(cumulative_wt = cumsum(wt), #cumulative
         lag_wt = lag(wt), #Opposite function: lead()
         Percent_wt = percent_rank(wt)
  ) %>% head(2)

### 2.5.2 Transmute
# - Same as mutate expect it drops other columns

dp_df %>% 
  transmute(mpg_10 = mpg/10)%>% head(2)

# ### 2.5.3 Mutate_all
# 
# - Mutate all selected columns by certain function
# - This example shows log
# - funs(log(.))

dp_df[, 2:5]%>% 
  mutate_all(funs(log(.)))%>%
  head(3)

### 2.5.4 Mutate_at

# Mutates only at specified columns

dp_df%>% 
  mutate_at(vars(mpg, disp, hp), funs(log(.)))%>% 
  head(2)

## 2.6 Summarise

dp_df %>% 
  summarise(mean_mpg = mean(mpg),
            median_mpg = median(mpg),
            sum_mpg = sum(mpg),
            count_mpg = n(),
            first_obs = first(mpg),
            last_obs = last(mpg),
            variance = var(mpg),
            sd = sd(mpg))

## 2.7 Groupby with summarise

dp_df %>% 
  group_by(gear) %>% 
  summarise(mpg_by_gear = mean(mpg))

# 3. Other import DPLYR functions

## 3.1 Count

### 3.1.1 Count number of observations by gear

dp_df %>% 
  count(gear)

## 3.2 Distinct

dp_df %>% 
  distinct(X)%>% head(3)

## 3.3 Sample data

# Used for train and test split in ML and NN

### 3.3.1 Sample 50% of the data

dp_df %>% 
  sample_frac(.5)%>% head(3)

### 3.3.2 Sample 10 observations

dp_df %>% 
  sample_n(10)%>% head(3)

## 3.4 Slice
# Pick out rows by observation number

dp_df %>% 
  slice(10:12)

## 3.5 add_count

dp_df %>% 
  add_count()%>% 
  head(3)