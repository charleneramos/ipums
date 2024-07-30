*---------------------------------------------------------------------------------
* Objective: Generate sex, race, ethnicity, educational attainment, 
* and employment status variables
*
* Charlene Ramos
* July 11, 2024
*---------------------------------------------------------------------------------


cd // set working directory
use cps_00005.dta, clear // load data
desc // describe data from cps_00005.dta (Figure 1)

label list SEX // list variables categories defined by BLS (Figure 2)
label list RACE 
label list HISPAN
label list EDUC
label list EMPSTAT

* convert sex into dummy variable
* 0 = male, 1 = female
gen sex_new = .
replace sex_new = 0 if sex==1
replace sex_new = 1 if sex==2

* create new race variable (3 categories)
* 1 = white non-hispanic, 2 = black non-hispanic, 3 = hispanic
gen race_new = .
replace race_new = 1 if (race==100 & hispan==0)
replace race_new = 2 if (race==200 & hispan==0)
replace race_new = 3 if (hispan==100  | hispan==102 | hispan==103  | hispan==104 | hispan==108 | hispan==109 | hispan==200 | hispan==300 | hispan==400 | hispan==500 | hispan==600 | hispan==610 | hispan==611 | hispan==612)

* create new educ variable (3 levels)
* 1 = hs or less, 2 = some college, 3 = bachelor's & higher 
gen educ_new = .
replace educ_new = 1 if (educ==0 | educ==1 | educ==2 | educ==10 | educ==11 | educ==12 | educ==13 | educ==14 | educ==20 | educ==21 | educ==22 | educ==30 | educ==31 | educ==32 | educ==40 | educ==50 | educ==60 | educ==70 | educ==71 | educ==72 | educ==73)
replace educ_new = 2 if (educ==80 | educ==81 | educ==90 | educ==91 | educ==92 | educ==100)
replace educ_new = 3 if (educ==110 | educ==111 | educ==120 | educ==121 | educ==122 | educ==123 | educ==124 | educ==125)

* replace educ_new = 4 if (educ==73) // hs graduates only test

* create new empstat variable (3 subpopulations)
* 1 = employed, 2 = unemployed, 3 = not in labor force 
gen empstat_new = .
replace empstat_new = 1 if (empstat==10 | empstat==12)
replace empstat_new = 2 if (empstat==20 | empstat==21 | empstat==22)
replace empstat_new = 3 if (empstat==30 | empstat==31 | empstat==32 | empstat==33 | empstat==34 | empstat==35 | empstat==36)

* aggregate rows using provided weights 
collapse (sum) wtfinl, by(empstat_new sex_new race_new educ_new year month)

* export to excel for continuation in R
export excel using ipums.xlsx, firstrow(variables) replace
* export excel using ipums_copy.xlsx, firstrow(variables) replace // hs graduates only test
