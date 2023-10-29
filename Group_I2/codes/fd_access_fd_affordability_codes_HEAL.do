cd "/Users/dk/Library/CloudStorage/Dropbox/2023 Local Food Economics Data Visualization Challenge/data"
**********************Food Accessibility*********************************************************
clear
import delimited "/Users/dk/Library/CloudStorage/Dropbox/2023 Local Food Economics Data Visualization Challenge/data/general_dtc_all_2017.csv", stringcols(1) 

*cd "/Users/dk/Library/CloudStorage/Dropbox/2023 Local Food Economics Data Visualization Challenge/data/dta"

*save datageneral_dtc_CA_2017.dta


*clear
*import delimited "/Users/dk/Library/CloudStorage/Dropbox/2023 Local Food Economics Data Visualization Challenge/data/general_dtc_TX_2017.csv", stringcols(1) 
*save datageneral_dtc_TX_2017.dta


*append using datageneral_dtc_CA_2017.dta
**Change fips code adding leading zero 
gen fips_new = fips
replace fips_new = "0"+fips if strlen(fips_new) <5
save datageneral_dtc_all_2017.dta, replace
**Merge with County population 
clear
import delimited "/Users/dk/Library/CloudStorage/Dropbox/2023 Local Food Economics Data Visualization Challenge/data/ACS/ACS_county_population_2017/ACSDT5Y2017.B01003-Data.csv",varnames(2)

gen fips_new = substr(geography,-5,.) 
save pop2017.dta,replace


merge 1:1 fips_new using datageneral_dtc_all_2017
keep if _merge==3
drop _merge

**
rename estimatetotal pop1000
rename value fmkts
gen state = substr(fips_new, 1,2)
gen fdacc = fmkts/pop1000
replace fdacc = fdacc*1000

bysort state: sum fdacc

**Unweighted standardization
sum fdacc
gen fdacc_index = (fdacc - `r(min)') / (`r(max)'-`r(min)')
sum fdacc fdacc_index

*State average number of farms (DTC)
bysort state_name: sum fdacc_index
sum fdacc_index
sum fdacc  if state_name =="California" //Wihout considering population: 8.36
sum fdacc  if state_name =="Texas" //Wihout considering population: 54.41

**************************Food affordability*************************************************
clear

keep geography geographicareaname estimatehouseholdsmedianincomedo 
gen fips_new = substr(geography,-5,.) 
gen state = substr(fips_new, 1,2)
gen year =2017
save medinc_county_2017

**
clear 
import delimited "/Users/dk/Library/CloudStorage/Dropbox/2023 Local Food Economics Data Visualization Challenge/data/foodsales.csv", clear 
rename state state_name
save foodsales.dta, replace

use medinc_county_2017, clear

gen state_name= substr(geographicareaname, index(geographicareaname, ", ") +2, .)

save medinc_county_2017, replace

use medinc_county_2017, clear
merge m:1 state_name year using foodsales

keep if _merge==3
drop _merge

gen fdaff = totalnominalfoodsalespercapita/estimatehouseholdsmedianincomedo
sum fdaff if state_name =="California"
sum fdaff if state_name =="Texas"


**Unweighted standardization
sum fdaff
gen fdaff_index = (fdaff - `r(min)') / (`r(max)'-`r(min)')
sum fdaff fdaff_index

*State average
bysort state_name: sum fdaff_index
