library(foreign)
library(dplyr)

# A. Import data
## A.1.1 Import labor participation data "c_ls.dta"
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

### 1.1.1 Create labor dummy

#Dummy creation method 1:
#Worked_my is the new variable--plyr::mapvalues is the maping function--
#df_labor$Worked_dummy = plyr::mapvalues(df_labor$ls12,from= c(3,1), to=c(0,1)) 
#Dummy creation method 2:
df_labor = df_labor %>% mutate(Worked_dummy = recode(ls12, "3"=0, "1"=1)) 
#Check work
df_labor %>% select(Worked_dummy,ls12)

###1.1.2 Drop ages not in 16 to 65 age range
#Method 1:
#ages_16to65 <- df_labor[which(df_labor$ls02_2>15 & df_labor$ls02_2<66),]
#Method 2:
ages_16to65  = df_labor %>% filter(ls02_2>15  & ls02_2<66)
ages_16to65 %>% select(ls02_2)

### 1.1.3 Create gender dummy
#Method 1:
ages_16to65= ages_16to65 %>% mutate(gender = recode(ls04, "3"="Female", "1"="Male"))
#Method 2:
#ages_16to65$gender = plyr::mapvalues(ages_16to65$ls04,from= c(1,3), to=c ("Male","Female"))
#Check work
ages_16to65 %>% select(gender, ls04)


#QUESTION 1.1 Answer:
ages_16to65 %>%
  group_by(gender)%>% #Groupby compares genders
  select(Worked_dummy)%>% #Labor force participation
  na.omit()%>%
  summarise(Answer_112 = mean(Worked_dummy))


## Q1.2 Provide labor market participation rates by 5 year age ranges for men and women.
### 1.2.1 We cannot use simple groupby function to pull age range
ages_16to65%>%
  group_by(ls02_2)%>%
  select(Worked_dummy)%>%
  na.omit()%>%
  summarise(Labor_force_participation = 100*mean(Worked_dummy))

# ### 1.2.2 Use cut function
# - First sort age values 
# - Create age cut off values
# - ages_16to65["ls02_2"].sort_values().values
# - Use pd.cut()

#### 1.2.2.1 Create age cut offs
age_cutoffs= c(seq(15, 55, by = 5), 65)
age_cutoffs

#### 1.2.2.2 Create age groups
#Method 1
#ages_16to65$age_groups = cut(ages_16to65$ls02_2, age_cutoffs, include.lowest = TRUE)
#Method 2
ages_16to65 = ages_16to65 %>% mutate(age_groups = cut(ls02_2, age_cutoffs, include.lowest = TRUE))
#Check work
ages_16to65 %>% select(age_groups, ls02_2)

#QUESTION 1.2 Answer:
#### 1.2.2.3 Groupby mean of worked dummy
ages_16to65 %>%
  group_by(age_groups)%>%
  select(Worked_dummy)%>%
  na.omit()%>%
  summarise(ans1_2 = 100*mean(Worked_dummy))

# Q.2. Construct at least two different definitions of the informal sector to answer the following questions.
# - 2.1 What proportion of men and women who work are in the informal sector?
# - 2.2 What are the average hours worked of men and women in the informal sector versus the formal sector?
# - 2.3 Compare total earnings, average earnings per hour and median earnings per hour in the formal and informal sector for men and women.
# - 2.4 TB24 asks broadly about occupation using this question repeat c) above. 

### 2.1.1 create male dummy
#Create male dummy
#Method 1:
#ages_16to65$male_dummy = plyr::mapvalues(ages_16to65$ls04,from= c(1,3), to=c (1,0))
#Method 2:
ages_16to65= ages_16to65 %>% mutate(male_dummy = recode(ls04, "3"=0, "1"=1))
#Check work
ages_16to65 %>% select(male_dummy, ls04,gender)

### 2.1.2 Merge two data sets
#This code is so we can merge the two data frames
ages_16to65_small = ages_16to65 %>%select(folio, ls, male_dummy, Worked_dummy)
df_income_small = df_income %>% select(folio, ls,tb24_26p_cmo,tb44p_2, tb35a_2)

#Merge the data
merged = merge(df_income_small, ages_16to65_small, by=c("ls","folio"), all=FALSE)
str(merged)


#### 2.1.1.1 Create informal dummy
#Method 1.
#merged$informal_dummy = as.numeric(tb24_26p_cmo==41 | tb24_26p_cmo==72 | tb24_26p_cmo==81 | tb24_26p_cmo==82)
#Method 2.
merged = merged %>% 
  na.omit() %>% 
  mutate(informal_dummy1 = as.numeric(tb24_26p_cmo==41| tb24_26p_cmo==72 | tb24_26p_cmo==81 | tb24_26p_cmo==82))

merged = merged %>% 
  na.omit() %>% 
  mutate(informal_dummy2 = as.numeric(tb24_26p_cmo==41| tb24_26p_cmo==72 | tb24_26p_cmo==81 | tb24_26p_cmo==82))

#Check work
merged %>% select(informal_dummy, tb24_26p_cmo)

#Answer: 2.1 What proportion of men and women who work are in the informal sector?
merged %>% 
  select(male_dummy) %>% 
  na.omit() %>% 
  group_by(male_dummy)%>%
  summarise(Informal_sector_participation = mean(informal_dummy)*100)

#Answer: 2.1 OTHER METHOD
#Workforce participation of men in women in the informal sector
#Method 2: Using filter(Informal Jobs)
# merged %>% 
#   select(tb24_26p_cmo, male_dummy, Worked_dummy)%>% 
#   na.omit() %>% 
#   filter(tb24_26p_cmo==41 | tb24_26p_cmo==72 | tb24_26p_cmo==81 | tb24_26p_cmo==82) %>% #Filtered by informal
#   group_by(male_dummy)%>%
#   summarise(Pct_working = mean(Worked_dummy*100))

# merged %>% 
#   select(tb24_26p_cmo, male_dummy, Worked_dummy)%>% 
#   na.omit() %>% 
#   filter(tb24_26p_cmo!=41 | tb24_26p_cmo!=72 | tb24_26p_cmo!=81 | tb24_26p_cmo!=82) %>% #Filtered by informal
#   group_by(male_dummy)%>%
#   summarise(Pct_working = mean(Worked_dummy*100))

## 2.2 What are the average hours worked of men and women in the informal sector versus the formal sector?
#ANSWER: 2.2
merged %>% 
  na.omit() %>% 
  group_by(male_dummy, informal_dummy)%>%
  summarise(Avg_hours_worked =  mean(tb44p_2))

##Answer: 2.2 OTHER METHOD
# merged %>%
#   na.omit() %>%
#   filter(tb24_26p_cmo==41 | tb24_26p_cmo==72 | tb24_26p_cmo==81 | tb24_26p_cmo==82) %>% #Filtered by informal
#   group_by(male_dummy)%>%
#   summarise(Pct_working = mean(tb44p_2))
# 
# merged %>%
#   na.omit() %>%
#   filter(tb24_26p_cmo!=41 | tb24_26p_cmo!=72 | tb24_26p_cmo!=81 | tb24_26p_cmo!=82) %>% #Filtered by formal
#   group_by(male_dummy)%>%
#   summarise(Pct_working = mean(tb44p_2))

## 2.3 Compare total earnings, average earnings per hour and median earnings per hour in the formal and informal sector for men and women.


merged = merged %>% mutate(Monthly_hours = tb44p_2*4.3)
merged = merged %>% mutate(Hourly_wage  = tb35a_2/Monthly_hours)

#ANSWER: 2.2
merged %>% 
  na.omit() %>% 
  group_by(male_dummy, informal_dummy)%>%
  summarise(Avg_hourly_wage = mean(Hourly_wage),
            Median_hourly_wage = median(Hourly_wage))


##Answer: 2.3 OTHER METHOD
# merged %>% 
#   na.omit() %>% 
#   filter(tb24_26p_cmo==41 | tb24_26p_cmo==72 | tb24_26p_cmo==81 | tb24_26p_cmo==82) %>% #Filtered by informal
#   group_by(male_dummy)%>%
#   summarise(Avg_hourly_wage = mean(Hourly_wage),
#             Median_hourly_wage = median(Hourly_wage))
# 
# merged %>% 
#   na.omit() %>% 
#   filter(tb24_26p_cmo!=41 | tb24_26p_cmo!=72 | tb24_26p_cmo!=81 | tb24_26p_cmo!=82) %>% #Filtered by formal
#   group_by(male_dummy)%>%
#   summarise(Avg_hourly_wage = mean(Hourly_wage),
#             Median_hourly_wage = median(Hourly_wage))



## 2.5 Are there occupations where earnings in the informal sector are close to or higher than the formal sector?
merged %>% 
  na.omit() %>% 
  group_by(tb24_26p_cmo)%>%
  summarise(Avg_hourly_wage = mean(Hourly_wage))

