# under COVID19 which reports individual level data for patients in Mexico who have been tested for covid19. 
#While the variable labels are in Spanish the catalog and definitions provided have all been translated to English and thus you should be able to understand the variables and codes.


#A.1 Import libraries
library(foreign)
library(dplyr)
library(readstata13)
library(readxl)

#A.2 Import data
### A.2.1
df = read.dta13("COVID19_Mexico_13.04.2020_version1.dta")
str(df)

#A.3 Rename vars
df = df %>% rename(Test_result = resultado,
              death_date = FECHA_DEF,
              date=FECHA_ACTUALIZACION
              ) 

#A.4 Map variables
df = df %>% mutate(gender = recode(sexo, "1" = "female", "2"='male'),
              confirmed_dummy = recode(Test_result, '1'= 1, "2" = 0),
              death_dummy = as.numeric(death_date!="9999-99-99"),
              Testing = recode(Test_result, "1"="Positive", "2"='Negative', '3'= 'Result pending')
              )

#Questions
#1.1 How many individuals have been tested for covid19? 
df %>% select(Testing) %>% summarise(Total_tested= n())

#1.2 What proportion have tested positive? 
df %>% 
  select(confirmed_dummy) %>% 
  na.omit() %>%  
  summarise(Percent_tested_positive = mean(confirmed_dummy)*100)

# 1.3 By gender? 
df %>% 
  group_by(gender) %>% 
  select(confirmed_dummy) %>% 
  na.omit() %>%  
  summarise(Percent_tested_positive = mean(confirmed_dummy)*100)

#1.4 Has the proportion testing positive changed over time?
unique(df$date)

# 2. What proportion of individuals testing positive have passed away? 
df %>% 
  group_by(gender) %>% 
  select(death_dummy) %>% 
  na.omit() %>%  
  summarise(Mortality_rate = mean(death_dummy )*100)

# 3. Analyze the probability of passing away by pre existing conditions and age/gender using a regression analysis. 
#Provide an interpretation of the variables which are predictive of death for those testing positive.
df = df %>% mutate(female_dummy = recode(sexo, "1" = 1, "2"=0))

#3.1 Regression by age and gender

reg1 = lm(death_dummy*100 ~ female_dummy + edad, data=df)
print(summary(reg1))

##3.2 Regression by age gender and pre-existing conditions

### 3.2.1 Create dummy variables for pre-existing condition variables

df = df %>% mutate(diabetes_dummy = recode(diabetes, '1'= 1, "2" = 0, "98"=0),
              asma_dummy = recode(asma, '1'= 1, "2" = 0, "98"=0),
              inmusupr_dummy = recode(inmusupr, '1'= 1, "2" = 0, "98"=0),
              hipertension_dummy = recode(hipertension, '1'= 1, "2" = 0, "98"=0),
              obesidad_dummy = recode(obesidad, '1'= 1, "2" = 0, "98"=0),
              RENAL_CRONICA_dummy = recode(RENAL_CRONICA, '1'= 1, "2" = 0, "98"=0) )

### 3.2.2 Regression
reg2 = lm(death_dummy ~ female_dummy + diabetes_dummy + asma_dummy+inmusupr_dummy+ hipertension_dummy+obesidad_dummy+RENAL_CRONICA_dummy, data=df)
print(summary(reg2))





