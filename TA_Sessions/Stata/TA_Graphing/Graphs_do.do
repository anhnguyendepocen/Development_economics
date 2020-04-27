clear all
import delimited Covid_TS_global.csv



* 1. Scatter
*Only keep most recent data
list country date confirmed if date == "2020-04-22"
keep if date == "2020-04-22"

** 1.1 Scatter plot: basic
twoway scatter deaths confirmed 

*** 1.1.1 Title, xtitle, ytitle
twoway scatter deaths confirmed, ytitle(y title) xtitle(x title) title(Cool title)

** 1.2 Scatter plot: show countries
twoway scatter deaths confirmed, mlabel(country)

*** 1.2.1 Scatter plot with regression line
twoway scatter deaths confirmed || lfit  deaths confirmed 

* 1.3 Using logs
gen deaths_ln = ln(deaths)
gen confirmed_ln = ln(confirmed)

** 1.3.1 Scatter using logs
twoway scatter deaths_ln  confirmed_ln, mlabel(country) ytitle(Deaths--Log) xtitle(Confirmed--Log) title(Graphing logs)


* 2. Bar 
*** 2.1.1 Re-import data and set to date 
import delimited "Covid_TS_global.csv", clear 
keep if country=="US"

*** 2.1.2 Convert date string to time variable
gen date2 = date(date, "YMD")  //1. Convert to time variable

**** 2.1.2.1 Change time type
format date2 %tdCCYY.NN.DD

*Other date formats include:
*format date2 %tdMonth dd, CCYY
*format date2  %tdnn/dd/YY

**** 2.1.2.2 Use tsset 
tsset date2 //Use tsset to set date

*** 2.1.3 Plot US deaths
twoway tsline deaths , recast(bar) ytitle(Date) xtitle(Deaths) title(U.S. Deaths)



* 3. Line
import delimited "Covid_TS_global.csv", clear 

** 3.1 Encode country variable
encode country, generate(country_n)

** 3.2 Encode date variable
gen date2 = date(date, "YMD")  //1. Convert to time variable
format date2 %tdCCYY.NN.DD //2. Convert to time variable
xtset country_n date2

*** 3.3.1 Line chart with two y-axis
twoway (line deaths date2 if country=="US", lwidth(thick)) (line deaths date2 if country=="Spain"), legend(label(1 "US") label(2 "Spain"))


* 4. Fixed effects regression
*** 4.1.1 Import again
import delimited "Covid_TS_global.csv", clear 
*** 4.1.2 Encode country
encode country, generate(country_n)
*** 4.1.3 Encode date
gen date2 = date(date, "YMD")  //1. Convert to time variable
format date2 %td //2. Convert to time variable

*** 4.2 Set panel data: xtset Area/Country/State Time
xtset country_n date2

gen deaths_ln = ln(deaths)
gen confirmed_ln = ln(confirmed)

reg deaths_ln confirmed_ln, robust
xtreg deaths_ln confirmed_ln, fe robust cluster(country_n)



** 5. Saving graphs
** 5.1 Re-import data
import delimited Covid_TS_global.csv,clear
keep if date == "2020-04-22"
twoway scatter deaths confirmed, ytitle(y title) xtitle(x title) title(Cool title)

** 5.2 Save
graph save Graph "Scatter", replace
graph export "Scatter.png", as(png) name("Graph") replace

