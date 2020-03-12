* A.2.1 Import from folder
* A.2.1.1 Import weekly variables data "i_cs"
use "hh02dta_b1\i_cs.dta", clear

* A.2.1.2 Select weekly consumption variables
* A.2.1.2.1 Convert weekly varibles to monthly vars
local weeklyvar "cs02a_12 cs02a_22 cs02a_32 cs02a_42 cs02a_52 cs02a_62 cs02a_72 cs02a_82"
foreach x in `weeklyvar' {
generate ics_monthly_`x' = `x' * 4.3
}

* A.2.2 Monthly variables
local 1month_var "cs16a_2 cs16b_2 cs16c_2 cs16d_2 cs16e_2 cs16f_2 cs16g_2 cs16h_2 cs16i_2"
foreach x in `1month_var' {
generate ics_monthly_`x' = `x' 
}

* A.2.3 3-Month variables
* A.2.3.1 Import weekly variables data "i_cs1"
merge m:m folio using "hh02dta_b1\i_cs1.dta"

* A.2.1.3.1 Convert 3-month varibles to monthly vars
local 3month_var "cs22a_2 cs22b_2 cs22c_2 cs22d_2 cs22e_2 cs22f_2 cs22g_2 cs22h_2"

foreach x in `3month_var' {
generate ics1_monthly_`x' = `x' /3
}

* Q.1. 
* 1.1.1 Calculate total consumer spending
egen total_cons = rowtotal(ics1_monthly_cs22a_2 ics1_monthly_cs22b_2 ics1_monthly_cs22c_2 ics1_monthly_cs22d_2 ics1_monthly_cs22e_2 ics1_monthly_cs22f_2 ics1_monthly_cs22g_2 ics1_monthly_cs22h_2 ics_monthly_cs02a_12 ics_monthly_cs02a_22 ics_monthly_cs02a_32 ics_monthly_cs02a_42 ics_monthly_cs02a_52 ics_monthly_cs02a_62 ics_monthly_cs02a_72 ics_monthly_cs02a_82 ics_monthly_cs16a_2 ics_monthly_cs16b_2 ics_monthly_cs16c_2 ics_monthly_cs16d_2 ics_monthly_cs16e_2 ics_monthly_cs16f_2 ics_monthly_cs16g_2 ics_monthly_cs16h_2 ics_monthly_cs16i_2)

* 1.1.1.1 Basic stats: total consumer spending
sum total_cons

* 1.1.1.2 Graph: total consumer spending
egen total_cons_sd =std(total_cons)
histogram total_cons if total_cons_sd<3, bin(10)


* Q.1.2 Per capita consumption Total/house size
* Q.1.2.1 Avg. house size
* 1.2.1.1 Import house size data set "c_ls"
drop _merge
merge m:m folio using "hh02dta_bc\c_ls.dta"


*1.2.1.2 Count family members "ls" in each household "folio"
egen family_members = count(ls), by(folio)
histogram family_members, bin(10) title(Family members) xtitle(# of family members) 


* 1.3 Per capita consumption (Total/house size)
gen percap_consum = total_cons / family_members
egen percap_consum_sd = std(percap_consum)
histogram percap_consum  if percap_consum_sd <3, bin(100)

* Q.2
* Q2.1 Headcount
gen poverty_line = 500
gen p0 =percap_consum <poverty_line  //Poor households dummy
la var p0 "poverty incidence"
la de p0 0 "non-poor" 1 "poor"
sum p0
local p0_tot=r(mean)
display `p0_tot'


*Check work
list folio ls percap_consum p0 in 1/10

*2.2 Avg. poverty gap
gen p1=(poverty_line-percap_consum)*p0  //calculate the poverty gap for each household
la var p1 "poverty gap"
gen p1z= p1/poverty_line

sum p1z
local p1z_tot=r(mean)
display `p1z_tot'

* 2.3 Avg. poverty gap squared
gen p2=(poverty_line-percap_consum)^2*p0
la var p2 "poverty gap squared"
gen p2z=p2/(poverty_line^2)

sum p2z
local p2z_tot=r(mean)
display `p2z_tot'

* 3.1. Import residence data from "c_portad"
drop _merge
merge m:m folio using "hh02dta_bc\c_portad.dta"

* 3.2 Show poverty by area of residence
tab estrato p0, row

* Q.4. 
* 4.1 Calculate cumulative sum for population and consumption
sort percap_consum
gen rank_percap_consum  =_n
cumul percap_consum, generate(cum_percap_consum)
egen total_percap_consum =total(percap_consum)
sort percap_consum


xtile q5=percap_consum, n(5)  //create the variable that indicates the quintiles
sum percap_consum
scalar percap_consum_sum=r(sum)
 forvalues i = 1/5 {
	quietly sum percap_consum if q5==`i'
	scalar percap_consum_`i'_sum=r(sum)
	scalar percap_consum_`i'_share=percap_consum_`i'_sum/percap_consum_sum
	}
scalar list  percap_consum_1_share  percap_consum_2_share  percap_consum_3_share  percap_consum_4_share  percap_consum_5_share


gen cum_total_conspc=0
replace cum_total_conspc=  percap_consum if _n==1
local N=_N
forvalues k=2/`N' {
	quietly replace cum_total_conspc = percap_consum+cum_total_conspc[_n-1] if _n==`k'
	}
gen cuml_conspc=cum_total_conspc/total_percap_consum  //create the variable for the cumulative consumption share

gen pop_share = rank_percap_consum/35677 

* 4.1.1 Plot:
line cuml_conspc pop_share, title("lorenz curve") xtitle("cum. % of households") ytitle("cum. % of consumption per capita")

* 4.2 Gini coefficient calculation

correlate percap_consum cuml_conspc, covariance
scalar conspc_cov=r(cov_12) //Calculate the covariance
su percap_consum
scalar conspc_mean=r(mean)  //Calculate the mean
scalar gini=2*conspc_cov/conspc_mean   //Gini coefficient
display gini

*Export data to csv

*export delimited folio  ls  family_members percap_consum  p0 p1 p2 estrato cum_percap_consum cum_total_conspc cuml_conspc pop_share using "\\Client\H$\Desktop\HW2_Stata\Results_stata.csv", nolabel replace
