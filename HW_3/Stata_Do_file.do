*A.1.1 Import labor participation data "c_ls.dta"
use "hh02dta_bc/c_ls.dta", clear

* A.1.2 Import income data "iiia_tb"
merge m:m ls folio using "hh02dta_b3a/iiia_tb.dta"

*Homework Questions
*Q.1 Labor market participation
*What is the labor market participation of men and women aged 16 to 65
**1.1.1 Filter ages 16-65¶

*Create worked dummy
drop if ls12==.
gen worked_dummy = 0
replace worked_dummy = 1 if ls12==1
la de worked_dummy 0 "Not-working" 1 "Working"
**Shows worked_dummy
list worked_dummy ls12 in 1/10

*Drops ages other than 16-65
keep if ls02_2>15 & ls02_2<66


**1.1.2 Filter men and women
*Create male_dummy
gen male_dummy = 0
replace male_dummy =1 if ls04==1
la de male_dummy 0 "Female" 1 "Male"
list male_dummy ls04 in 1/5 //Check validity

*Drops na
drop if worked_dummy==.|male_dummy==.

**1.1.3 ANSWER: labor market participation of men and women
tabstat worked_dummy, by(male_dummy) //Female, Male, Overall

tab male_dummy worked_dummy, row nofreq //2nd column is the same as above


*Q1.2 Provide labor market participation rates by 5 year age ranges for men and women.
**1.2.1 We cannot use simple groupby function to pull age range
**1.2.2.2 Create age groups
*We can only show labor force participation by every age group (ls02_2)
tabstat worked_dummy, by(ls02_2)

*As a result we need to create five year age groups
egen agegroup = cut(ls02_2), at(15, 20, 25, 30, 35, 40, 45, 50, 55, 60, 65)
**Shows new varaible created from ls02_2
list agegroup ls02_2 in 1/10


**1.2.2.3 Groupby mean of worked dummy
**1.2.3 Answer:  5 year age ranges for men and women.
tabstat worked_dummy, by(agegroup)
tab male_dummy agegroup, sum(worked_dummy) means


*Q.2. Construct at least two different definitions of the informal sector to answer the following questions.
*Drops na
drop if tb24_26p_cmo==.

*Create informal dummy
gen informal_dummy = 0
replace informal_dummy=1 if tb24_26p_cmo==41 | tb24_26p_cmo==72 | tb24_26p_cmo==81 | tb24_26p_cmo==82 
*Shows new informa_dummy created from tb24_26p_cmo
list informal_dummy tb24_26p_cmo in 1/10 

*2.1 What proportion of men and women who work are in the informal sector?
tabstat informal_dummy, by(male_dummy)

**2.2 What are the average hours worked of men and women in the informal sector versus the formal sector?
twoway (histogram tb44p_2 if male_dummy==1, color(green)) (histogram tb44p_2 if male_dummy==0), legend(order(1 "Male" 2 "Female" ))

***2.2.2 Answer
tab male_dummy informal_dummy, sum(tb44p_2) means

*2.3 Compare in the formal and informal sector for men and women for:
*1. total earnings
*2. average earnings per hour
*3. median earnings per hour
*** 2.3.1 Calculate "month hours" and "hourly wage"

*tb44p_2 is a weekly variable that we convert to monthly
gen monthly_hours = tb44p_2*4.3
*tb35a_2 is monthly wage
gen Hourly_wage = tb35a_2/monthly_hours 

*2.3.3 Answer:
** 2.3.3.1 Total earnings
tab male_dummy informal_dummy, sum(tb35a_2) means


** 2.3.3.2 Avg. & Median earnings per hour
tab male_dummy informal_dummy, sum(Hourly_wage) means






