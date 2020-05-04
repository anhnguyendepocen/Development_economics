*A.1 Import libraries


*A.2 Import data
**A.2.1
use "COVID19_Mexico_13.04.2020_version1.dta", clear

describe

**A.2.1 Import english names
*english_names_df = read_xlsx("Descriptores_English.xlsx")
*var_description = read_xlsx("Catalogos_English.xlsx")

rename resultado Test_result
rename sexo gender
rename FECHA_DEF death_date
rename FECHA_ACTUALIZACION date

**A.4 Map variables
gen female_dummy = 0
replace female_dummy=1 if gender==1

gen confirmed_dummy = 0
replace confirmed_dummy = 1 if Test_result == 1

gen death_dummy = 0
replace death_dummy=1 if death_date!="9999-99-99"


*Questions
*1.1 How many individuals have been tested for covid19? 
count

*1.2 What proportion have tested positive? 
tabstat confirmed_dummy

*1.3 By gender? 
tabstat confirmed_dummy, by(female_dummy)

*1.4 Has the proportion testing positive changed over time?


* 2. What proportion of individuals testing positive have passed away? 
tabstat death_dummy if confirmed_dummy==1
**2.1 By gender
tabstat death_dummy if confirmed_dummy==1, by(female_dummy)



*# 3. Analyze the probability of passing away by pre existing conditions and age/gender using a regression analysis. 

** 3.1 Regression by age and gender
reg death_dummy female_dummy edad

** 3.2 Regression by age gender and pre-existing conditions

*** 3.2.1 Create dummy variables for pre-existing condition variables
local dummies "diabetes asma inmusupr hipertension obesidad RENAL_CRONICA"
foreach x in `dummies' {
	generate `x'_dummy = 0
	replace `x'_dummy=1 if `x'==1
}

*** 3.2.2 Regression
reg death_dummy female_dummy diabetes_dummy asma_dummy inmusupr_dummy hipertension_dummy obesidad_dummy RENAL_CRONICA_dummy, robust

