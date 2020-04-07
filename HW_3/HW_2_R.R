# PLCY 782
# Empirical Exercise 3: Due date on ELMS April 13th, 2020 7PM
# The Informal Sector
# The objective of this exercise is to study the informal sector using the Mexican Family Life Survey. Using the 2002 data round, please write a program (in whichever statistical package you prefer) to answer the following questions:
#   1. What is the labor market participation of men and women aged 16 to 65? 
#   - 1.1 Provide labor market participation rates by 5 year age ranges for men and women.
# 
# 
# 2. Construct at least two different definitions of the informal sector to answer the following questions.
# - 2.1 What proportion of men and women who work are in the informal sector?
#   - 2.2 What are the average hours worked of men and women in the informal sector versus the formal sector?
#   - 2.3 Compare total earnings, average earnings per hour and median earnings per hour in the formal and informal sector for men and women.
# - 2.4 TB24 asks broadly about occupation using this question repeat c) above. 
# - 2.5 Are there occupations where earnings in the informal sector are close to or higher than the formal sector?
#   
library(tidyverse)
library(foreign)
library(dplyr)

# A. Import data


# ## A.1.1 Import labor participation data "c_ls.dta"
# - used in Q.1

df_labor =  read.dta("hh02dta_bc/c_ls.dta")
df_labor = as_tibble(df_labor )
#str(df_labor)
head(df_labor,3)

## A.1.2 Import income data "iiia_tb"

df_income = read.dta("hh02dta_b3a/iiia_tb.dta")
df_income = as_tibble(df_income)
#str(df_income)
head(df_income,3)

# Homework Questions

# Q.1 Labor market participation
## Q.1.1 What is the labor market participation of men and women aged 16 to 65
### 1.1.1 Filter ages 16-65

df_labor$Worked_dummy = plyr::mapvalues(df_labor$ls12,from= c(3,1), to=c(0,1))
ages_16to65 <- df_labor[which(df_labor$ls02_2>15 & df_labor$ls02_2<66),]
head(ages_16to65,3)

unique(ages_16to65$Worked_dummy)

### 1.1.2 Filter men and women

ages_16to65$gender = plyr::mapvalues(ages_16to65$ls04,from= c(1,3), to=c ("Male","Female"))

ages_16to65 %>%
  group_by(gender)%>%
  select(Worked_dummy)%>%
  na.omit()%>%
  summarise(Labor_force_participation = 100*mean(Worked_dummy))

## Q1.2 Provide labor market participation rates by 5 year age ranges for men and women.
### 1.2.1 We cannot use simple groupby function to pull age range
ages_16to65%>%
  group_by(ls02_2)%>%
  select(Worked_dummy)%>%
  na.omit()%>%
  summarise(Labor_force_participation = 100*mean(Worked_dummy))%>%head(4)

# ### 1.2.2 Use pd.cut
# - First sort age values 
# - Create age cut off values
# - ages_16to65["ls02_2"].sort_values().values
# - Use pd.cut()

#### 1.2.2.1 Create age cut offs

age_cutoffs= c(seq(15, 55, by = 5), 65)
age_cutoffs

#### 1.2.2.2 Create age groups

age_group = cut(ages_16to65$ls02_2, age_cutoffs, include.lowest = TRUE)

#### 1.2.2.3 Groupby mean of worked dummy

ages_16to65$age_groups = age_group
ages_16to65 %>%
  group_by(age_groups)%>%
  select(Worked_dummy)%>%
  na.omit()%>%
  summarise(ans1_2 = 100*mean(Worked_dummy))

# Q.2. Construct at least two different definitions of the informal sector to answer the following questions.
# 
# - 2.1 What proportion of men and women who work are in the informal sector?
#   - 2.2 What are the average hours worked of men and women in the informal sector versus the formal sector?
#   - 2.3 Compare total earnings, average earnings per hour and median earnings per hour in the formal and informal sector for men and women.
# - 2.4 TB24 asks broadly about occupation using this question repeat c) above. 

# ## 2.1 What proportion of men and women who work are in the informal sector?
# - 11 Profesionist 304 
# - 12 Technicians 230 
# - 13 Education workers 370 
# - 14 Arts, performance 63 
# - 21 Employees and 87 directors of the public, private and social sectors
# - 41 Agricultural, cattle 2097 activities, foresting, hunting and fishing workers
# - 51 Chiefs, supervisors 74 
# - 52 Manufacturing 1877 craftsmen and workers
# - 53 Operators of fixed 251 machinery of continuous movement and equipment in the process of industrial production
# - 54 Assistants, laborers and similar in the process of artisan and industrial manufacture and in activities of repair and maintenance 608 
# - 55 Conductors and assistants of conductors of movable machinery and means of transport 394 
# - 61 Department chiefs, coordinators and supervisors of the administrative activities and services. 161 
# - 62 Workers in the support of the administrative activities  735 
# - 71 Retailers, employees in commerce and sales agents 1,726 
# - 72 Street sales and services workers233 
# - 81 Workers in personal establishments 762 
# - 82 Workers in domestic services 444 
# - 83 Workers in services of protection, monitoring and armed forces 248
# - 99 Other workers with 29 occupations noncclassified previously, insufficiently specified and not specified

### 2.1.1 Merge df_income and ages_16to65
ages_16to65$male_dummy = plyr::mapvalues(ages_16to65$ls04,from= c(1,3), to=c (1,0))
ages_16to65_small = ages_16to65 %>%select(folio, ls, male_dummy, Worked_dummy)
df_income_small = df_income %>% select(folio, ls,tb24_26p_cmo,tb44p_2, tb35a_2)
merged = merge(df_income_small, ages_16to65_small, by=c("ls","folio"), all=FALSE)
str(merged)


#### 2.1.1.1 Create informal dummy

merged = merged %>% 
  na.omit() %>% 
  mutate(informal_dummy = as.numeric(tb24_26p_cmo==41 | tb24_26p_cmo==72 | tb24_26p_cmo==81 | tb24_26p_cmo==82))

### 2.1.2 Workforce participation of men in women in the informal sector

merged %>% 
  select(tb24_26p_cmo, male_dummy, Worked_dummy)%>% 
  na.omit() %>% 
  filter(tb24_26p_cmo==41 | tb24_26p_cmo==72 | tb24_26p_cmo==81 | tb24_26p_cmo==82) %>% #Filtered by informal
  group_by(male_dummy)%>%
  summarise(Pct_working = mean(Worked_dummy*100))

### 2.1.3 Answer proportion of men and women who work are in the informal sector

merged %>% 
  na.omit() %>% 
  group_by(male_dummy)%>%
  summarise(Informal_sector_participation = mean(informal_dummy)*100)

## 2.2 What are the average hours worked of men and women in the informal sector versus the formal sector?


### 2.2.1 Show histogram



### 2.2.2 Answer

merged %>% 
  na.omit() %>% 
  filter(tb24_26p_cmo==41 | tb24_26p_cmo==72 | tb24_26p_cmo==81 | tb24_26p_cmo==82) %>% #Filtered by informal
  group_by(male_dummy)%>%
  summarise(Pct_working = mean(tb44p_2))

merged %>% 
  na.omit() %>% 
  filter(tb24_26p_cmo!=41 | tb24_26p_cmo!=72 | tb24_26p_cmo!=81 | tb24_26p_cmo!=82) %>% #Filtered by formal
  group_by(male_dummy)%>%
  summarise(Pct_working = mean(tb44p_2))

merged %>% 
  na.omit() %>% 
  group_by(male_dummy, informal_dummy)%>%
  summarise(Informal_sector_participation =  mean(tb44p_2))

## 2.3 Compare total earnings, average earnings per hour and median earnings per hour in the formal and informal sector for men and women.

merged$Monthly_hours = merged$tb44p_2*4.3
merged$Hourly_wage = merged$tb35a_2/merged$Monthly_hours

merged %>% 
  na.omit() %>% 
  filter(tb24_26p_cmo==41 | tb24_26p_cmo==72 | tb24_26p_cmo==81 | tb24_26p_cmo==82) %>% #Filtered by informal
  group_by(male_dummy)%>%
  summarise(Avg_hourly_wage = mean(Hourly_wage),
            Median_hourly_wage = median(Hourly_wage))

merged %>% 
  na.omit() %>% 
  filter(tb24_26p_cmo!=41 | tb24_26p_cmo!=72 | tb24_26p_cmo!=81 | tb24_26p_cmo!=82) %>% #Filtered by formal
  group_by(male_dummy)%>%
  summarise(Avg_hourly_wage = mean(Hourly_wage),
            Median_hourly_wage = median(Hourly_wage))

merged %>% 
  na.omit() %>% 
  group_by(male_dummy, informal_dummy)%>%
  summarise(Avg_hourly_wage = mean(Hourly_wage),
            Median_hourly_wage = median(Hourly_wage))

## 2.5 Are there occupations where earnings in the informal sector are close to or higher than the formal sector?

merged %>% 
  na.omit() %>% 
  group_by(informal_dummy)%>%
  summarise(Avg_hourly_wage = mean(Hourly_wage))

