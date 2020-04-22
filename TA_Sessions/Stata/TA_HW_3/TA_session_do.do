*Import
sysuse auto.dta, clear
describe
codebook foreign

* Filter age
tab mpg
drop if mpg<13 | mpg>35

*1. What is the labor market participation of men and women aged 16 to 65? 
tabstat mpg, by(foreign)

** 1.1 Provide labor market participation rates by 5 year age ranges for men and women.
egen weightgroup = cut(weight), at(2000, 2500, 3000, 3500, 4000)
list weightgroup weight in 1/10 //Check to see if done correctly
tabstat mpg, by(weightgroup)

*Q.2. Construct at least two different definitions of the informal sector to answer the following questions.
//For Q.2 you must use the "Or" condition with the "|" symbol
//drop if tb24_26p_cmo==.
//gen informal_dummy = 0
//replace informal_dummy=1 if tb24_26p_cmo==41 | tb24_26p_cmo==72 | ETC...
gen expensive = 0
replace expensive =1 if price>8000 
tabstat expensive, by (weightgroup)

**2.2 What are the average hours worked of men and women in the informal sector versus the formal sector?
tab weightgroup expensive, sum(mpg) means

*2.3 Compare in the formal and informal sector for men and women for:
*1. total earnings

*2. average earnings per hour
gen mpk = mpg*1.6
tab weightgroup expensive, sum(mpk) means 

*3. median earnings per hour
table weightgroup expensive, contents(median mpk) //Median
table weightgroup expensive, contents(mean mpk median mpk) //Same as above 2 commands but combined into one table

* 2.5 Are there occupations where earnings in the informal sector are close to or higher than the formal sector?
tabulate mpg, sum(gear_ratio)






