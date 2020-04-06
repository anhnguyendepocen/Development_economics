
use "hh02dta_bc/c_ls.dta", clear
merge m:m folio ls using "hh02dta_bc/c_portad.dta"

*A.2 Examine data
list in 1/5

*A.2.2 Show information on variables
describe

*A.3 Drop ages other than 5-14
keep if ls02_2>4 & ls02_2<15

*A.3.1 Replace non-attendance number 3 to 0
replace ls16=0 if ls16==3


*Q.1
*What proportion of children between the ages of 5 and 14 are enrolled in school?
tab ls02_2 ls16, row nofreq

*Q.2
*How does the proportion enrolled in school differ by gender?
sort ls04
by ls04: tab ls16

*Q.3
*How does the proportion enrolled in school differ by size of zone of residence?
tab estrato ls16, row

*Q.5
*What proportion of children are working outside the home?
tab ls12 ls16, row
bysort ls04: tab ls12 ls16, row
bysort estrato: tab ls12 ls16, row
