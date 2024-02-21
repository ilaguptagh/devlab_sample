*-------------------------------------------------------------------------------
* Udry - Assignment 5
* Feb 2023
* Ila Gupta and Irena Petryk
*-------------------------------------------------------------------------------

clear
global data "/Users/ilagupta/Documents/NU/Economics/Udry/Assignment5"

*---------------------------- I. CLEANING WAVE 3 -------------------------------

/* Calculating annual consumption value of COFFEE AND ALCOHOL. According to 
Appendix 7 in Basic Information document for 2015, we must: */

* Keep observations for coffee and alcohol 
use "$data/sect7b_plantingw3.dta", clear
keep if item_cd == 120 | item_cd == 160 | item_cd == 161 | item_cd == 162 | ///
		item_cd == 163 | item_cd == 164
drop if s7bq3b == 11 | s7bq3b == 121 												// w4 doesn't have conversion factors for these units (removes 7 obs)

/* For households that reported a recent purchase, generate unit prices after 
manually normalizing to a common unit of measurement based on conversion factors 
from w4 (note: using conversion factors from food_conv_w3.dta, 2 GRAMS, 
3 LITERS, 4 CENTRILETERS, 120 PACKET/SACHET SMALL) */
foreach x in 2 3 5 6 7 {
	tab s7bq`x'a item_cd
}

foreach x in 2 3 5 6 7 {
	replace s7bq`x'a = s7bq`x'a * 0.001 if s7bq`x'b == 2
	replace s7bq`x'a = s7bq`x'a * 0.01 if s7bq`x'b == 4
	replace s7bq`x'a = s7bq`x'a * 0.005 if s7bq`x'b == 120
}

foreach x in 2 3 5 6 7 {
	tab s7bq`x'b
}

replace s7bq3a = . if s7bq3b == 14 													// dropping recent purchase for 1 obs in unconvertable units (14. TIN)
replace s7bq5a = . if s7bq5b == 60 													// dropping recent purchance for 1 obs in unconvertable units (60. KOBIOWU)
gen unit_price = s7bq4 / s7bq3a 						 							// hh naira expenditure / quantity purchased by hh

/* For hh that did not report a recent purchase but still consumed the good, 
generate unit prices using the median values for specific food items at the 
smallest strata (e.g., enumeration area, local government area, state, etc.) as 
long as the number of values per strata is no less than 30 */
foreach x in ea lga state zone {
	egen count_`x' = count(unit_price), by(`x' item_cd)
	egen median_price_`x' = median(unit_price), by(`x' item_cd)
	replace unit_price = median_price_`x' if s7bq2a != . & unit_price == . & count_`x' >= 30
}

egen median_price = median(unit_price), by(item_cd)
replace unit_price = median_price if s7bq2a != . & unit_price == .
label var median_price "W3: median unit price by smallest strata"

/* Calculate total consumed of each item by summing consumption from purchases, 
self-production, and gifts. If values for each source are missing, use total 
consumption. If total consumption is greater than value of source components, 
use total consumption. */
gen food_sum = s7bq5a + s7bq6a + s7bq7a 
replace food_sum = s7bq2a if food_sum == . | s7bq2a > food_sum
label var food_sum "W3: sum of consumption from purchases, self-pdctn, gifts"

/* Determine value of food consumption at the level of food item, annualize, and 
create variables for annual consumption of each good */
gen food_exp = food_sum * unit_price
   label var food_exp "W3: total food expenditure"
gen food_item = food_exp * 52.14
   label var food_item "W3: annual food expenditure"
gen coffee_temp = food_item if item_cd == 120
gen beer_temp = food_item if item_cd == 160
gen wine_temp = food_item if item_cd == 161
gen pito_temp = food_item if item_cd == 162
gen gin_temp = food_item if item_cd == 163
gen alc_other_temp = food_item if item_cd == 164

* Attaching all food consumption expenditure values to each obs for a given hh
foreach x in coffee beer wine pito gin alc_other {
	egen `x' = max(`x'_temp), by(hhid)
	replace `x' = 0 if `x' == .
}

* Summing alcohol consumption 
egen alc = rowtotal(beer wine pito gin alc_other)
   label var alc "W3: total annual alcohol consumption expenditure"
keep hhid coffee alc

* Keeping one observation per hh
duplicates drop hhid, force
save foodw3, replace

* Calculating annual value of consumption of TOBACCO AND CLOTHING.
* Tobacco
use "$data/sect8a_plantingw3.dta", clear

replace s8q2 = s8q2 * 52.14 
gen tobacco_temp = s8q2 if item_cd == 101 
egen tobacco = max(tobacco_temp), by(hhid) 
   label var tobacco "W3: total annual tobacco consumption expenditure"
replace tobacco = 0 if tobacco == . 

duplicates drop hhid, force
save tobaccow3, replace

* Clothing
use "$data/sect8c_plantingw3.dta", clear

replace s8q6 = s8q6 * 2
gen mtailor_temp = s8q6 if item_cd == 407 
gen ftailor_temp = s8q6 if item_cd == 409
gen mready_temp = s8q6 if item_cd == 408
gen fready_temp = s8q6 if item_cd == 410
gen mshoes_temp = s8q6 if item_cd == 414
gen fshoes_temp = s8q6 if item_cd == 416

foreach x in mtailor ftailor mready fready mshoes fshoes {
	egen `x' = max(`x'_temp), by(hhid)
	replace `x' = 0 if `x' == .
}

egen clothing = rowtotal(mtailor ftailor mready fready mshoes fshoes)
   label var clothing "W3: total annual clothing consumption expenditure"
   
duplicates drop hhid, force
merge 1:1 hhid using tobacco

keep hhid tobacco clothing
save nonfoodw3, replace

* Calculating annual value of consumption of ALCOHOL OUTSIDE HH
use "$data/sect7a_plantingw3.dta", clear

replace s7aq2 = s7aq2 * 52.14 
gen alc_out_temp = s7aq2 if item_cd == 9
egen alc_out = max(alc_out_temp), by(hhid)
replace alc_out = 0 if alc_out == .

duplicates drop hhid, force
keep hhid alc_out
save mealsw3, replace

* Determining number of individuals and children per hh
use "$data/sect1_plantingw3.dta", clear

gen boy = 0 
foreach x in 3 4 5 6 8 {
	replace boy = 1 if s1q2 == 1 & s1q6 < 14 & s1q3 == `x'
}
gen girl = 0
foreach x in 3 4 5 6 8 {
	replace girl = 1 if s1q2 == 2 & s1q6 < 14 & s1q3 == `x'
}

egen boys = sum(boy), by(hhid)
   label var boys "W3: number of boys (age < 14) in hh"
egen girls = sum(girl), by(hhid)
   label var boys "W3: number of girls (age < 14) in hh"
   
keep hhid boys girls
duplicates drop hhid, force

* Merging with expenditure data, calculating per-capita household expenditure, and merging with hunger data
foreach x in food3 nonfood3 meals3 "$data/cons_agg_wave3_visit1.dta" {
	merge 1:1 hhid using `x'
	keep if _merge == 3
	drop _merge
}
egen alcohol = rowtotal(alc alc_out)
keep hhid totcons coffee alcohol tobacco clothing hhsize boys girls
gen wave = 3
save w3, replace

*---------------------------- II. CLEANING WAVE 4 ------------------------------

* Recreating consumption aggregate for Wave 4 (re-using some of Irena's code from Assignment 2). We'll start with food expenditure. According to Appendix 7 in the Basic Information document for 2015, we must:

* For households that reported a recent purchase, generate unit prices after normalizing various units of measurement to the common unit of measurement.
use "$data/sect7b_plantingw4.dta", clear
gen unit_price = s7bq10 / (s7bq9a * s7bq9_cvn)

/* For households that did not report a recent purchase but still consumed the 
good, generate unit prices using the median values for specific food items at 
the smallest strata (e.g., enumeration area, local government area, state, etc.) 
as long as the number of values per strata is no less than 30 */
foreach x in ea lga state zone {
	egen count_`x' = count(unit_price), by(`x' item_cd)
	egen median_price_`x' = median(unit_price), by(`x' item_cd)
	replace unit_price = median_price_`x' if s7bq2a != . & unit_price == . & count_`x' >= 30
}

egen median_price = median(unit_price), by(item_cd)
replace unit_price = median_price if s7bq2a != . & unit_price == .

*-------------------------------- IIa. HOUSING ---------------------------------

/* Calculate total consumed of each item by summing consumption from purchases, 
self-production, and gifts. If values for each source are missing, use total 
consumption. If total consumption is greater than value of source components, 
use total consumption. Normalize to common unit of measurement. */
gen food_sum = s7bq5a + s7bq6a + s7bq7a
replace food_sum = s7bq2a if food_sum == . | s7bq2a > food_sum
gen food_q = food_sum * s7bq2_cvn

* Determine value of food cons at the level of food item, annualize, & aggregate by hh
gen food_exp = food_q * unit_price
gen food_item = food_exp * 52.14
egen food = sum(food_item), by(hhid)

* Generating variables for value of annual consumption of adult goods
gen coffee_temp = food_item if item_cd == 120
gen beer_temp = food_item if item_cd == 160
gen wine_temp = food_item if item_cd == 161
gen pito_temp = food_item if item_cd == 162
gen gin_temp = food_item if item_cd == 163
gen alc_other_temp = food_item if item_cd == 164
foreach x in coffee beer wine pito gin alc_other {
	egen `x' = max(`x'_temp), by(hhid)
	replace `x' = 0 if `x' == .
}

egen alc = rowtotal(beer wine pito gin alc_other)
   label var alc "W4: total annual alcohol consumption expenditure"
keep hhid food coffee alc
duplicates drop hhid, force
save food4

*---------------------------- IIb. NON-FOOD EXPENDITURE ------------------------	// annualize and aggregate on the household level

* For 7 day recall:
use "$data/sect8a_plantingw4.dta", clear
replace s8q2 = s8q2 * 52.14 
egen exp_a = sum(s8q2), by(hhid)

* Generating variable for value of annual consumption of tobacco
gen tobacco_temp = s8q2 if item_cd == 101 
egen tobacco = max(tobacco_temp), by(hhid) 
   label var tobacco "W4: total annual tobacco consumption expenditure"
replace tobacco = 0 if tobacco == . 
duplicates drop hhid, force
save exp_a

* For 30 day recall:
use "$data/sect8b_plantingw4.dta", clear
drop if item_cd == 326 | item_cd == 329 											// dropping large expenses as outlined in appendix
replace s8q4 = s8q4 * 12.17
egen exp_b = sum(s8q4), by(hhid)
duplicates drop hhid, force
save exp_b

* For 6 month recall: 
use "$data/sect8c_plantingw4.dta", clear
replace s8q6 = s8q6 * 2
egen exp_c = sum(s8q6), by(hhid)

* Generating variable for value of annual consumption of adult clothing
gen mtailor_temp = s8q6 if item_cd == 407 
gen ftailor_temp = s8q6 if item_cd == 409
gen mready_temp = s8q6 if item_cd == 408
gen fready_temp = s8q6 if item_cd == 410
gen mshoes_temp = s8q6 if item_cd == 414
gen fshoes_temp = s8q6 if item_cd == 416
foreach x in mtailor ftailor mready fready mshoes fshoes {
	egen `x' = max(`x'_temp), by(hhid)
	replace `x' = 0 if `x' == .
}

egen clothing = rowtotal(mtailor ftailor mready fready mshoes fshoes)
   label var clothing "W4: total annual clothing consumption expenditure"
duplicates drop hhid, force
save exp_c

* Merging all non-food expenditure data and calculating total non-food expenditure
foreach x in a b {
	merge 1:1 hhid using exp_`x'
	keep if _merge == 3
	drop _merge
} 
gen nonfood = exp_a + exp_b + exp_c

keep hhid nonfood tobacco clothing
save nonfoodw4

* Meals away from home. Annualize and aggregate on the household level
use "$data/sect7a_plantingw4.dta", clear
replace s7aq2 = s7aq2 * 52.14 
egen meals = sum(s7aq2), by(hhid)
   label var meals "W4: total annual meals out of hh consumption expenditure"

* Generating variable for value of annual consumption of alcohol outside the home
gen alc_out_temp = s7aq2 if item_cd == 9
egen alc_out = max(alc_out_temp), by(hhid)
   label var alc_out "W4: total annual alcohol out of hh consumption expenditure"
replace alc_out = 0 if alc_out == .

duplicates drop hhid, force
keep hhid meals alc_out
save mealsw4

/* (HOUSING) Appendix 7 states that a hedonic regression model is applied to 
estimate/predict rent. The dependent variable is actual rent paid, and the 
independent variables are location, number of rooms, material of roof, material 
of floor, material of wall, and amenities/utilities (toilet, bathroom type, 
water source, electricity connection, etc.) */

* Normalizing rent to common unit of time
use "$data/sect11_plantingw4.dta", clear
gen rent = s11q4a
replace rent = s11q4a * 12 if s11q4b == 1

/* Setting up dummy variables for location, material of roof, material of floor, 
material of wall, and amenities/utilities (leaving out dummy variables for 
"other" categories) */
keep hhid rent sector state s11q6 s11q7 s11q8 s11q9 s11q40 s11q33b s11q36 s11q47
gen urban = sector
replace urban = 0 if sector == 2
gen elec = s11q47
replace elec = 0 if s11q47 == 2
foreach x in state s11q6 s11q7 s11q8 s11q40 s11q33b s11q36 {
	tab `x', gen(`x')
}
drop s11q69 s11q710 s11q86 s11q408 s11q33b17 s11q3612

* Running the regression and predicting values using results
quietly reg rent sector state1-state36 s11q9 s11q61-s11q67 s11q71-s11q78 ///
            s11q81-s11q85 s11q401-s11q406 elec s11q33b1-s11q33b15 s11q361-s11q3611
predict rent_p
drop if rent_p == . 																// 4 observations dropped
keep hhid rent_p
save housing4

* Determining number of individuals and children per household
use "$data/sect1_plantingw4.dta", clear
egen hhsize = max(indiv), by(hhid)
gen boy = 0 
foreach x in 3 4 5 6 8 {
	replace boy = 1 if s1q2 == 1 & s1q6 < 14 & s1q3 == `x'
}
gen girl = 0
foreach x in 3 4 5 6 8 {
	replace girl = 1 if s1q2 == 2 & s1q6 < 14 & s1q3 == `x'
}
egen boys = sum(boy), by(hhid)
egen girls = sum(girl), by(hhid)
keep hhid hhsize boys girls
duplicates drop hhid, force

* Merging with expenditure data calculating per-capita household expenditure
foreach x in food4 nonfood4 meals4 housing4 {
	merge 1:1 hhid using `x'
	keep if _merge == 3
	drop _merge
}
gen totcons = (food + nonfood + meals + rent_p) / hhsize
egen alcohol = rowtotal(alc alc_out)
keep hhid totcons coffee alcohol tobacco clothing hhsize boys girls
gen wave = 4
save 4

*--------------------------- III. APPENDING W3-W4 ------------------------------
append using 3
duplicates tag hhid, gen(dup)
gen dup3 = 0
replace dup3 = 1 if dup == 1 & wave == 3


* Generating log variables
gen cons = totcons * hhsize
drop if cons < clothing | cons < coffee | cons < tobacco | cons < alcohol
gen lnexp = ln(cons)
gen lncloth = ln(clothing)
gen lncoffee = ln(coffee)
gen lntob = ln(tobacco)
gen lnalc = ln(alcohol)
save final

*------------------------------- IV. ANALYSIS ----------------------------------

* Testing whether coefficients for boys and girls differ 
foreach x in lncloth lncoffee lntob lnalc {
	reg `x' lnexp hhsize boys girls if dup3 != 1
	estimates store `x'
	outreg2 using `x'.tex
	test boys = girls
}

* Testing for substitution effect 
suest lncloth lncoffee lntob lnalc
test [lncloth_mean]boys = [lncoffee_mean]boys = [lntob_mean]boys = [lnalc_mean]boys
test [lncloth_mean]girls = [lncoffee_mean]girls = [lntob_mean]girls = [lnalc_mean]girls

*---------------------------------- V. GRAPHS  ---------------------------------
gen pctcoffee = (coffee)/(totcons)
gen pcttobacco = (tobacco)/(totcons)
gen pctalcohol = (alcohol)/(totcons)

preserve
drop if boys>6
graph bar pctclothing pctalcohol pcttobacco pctcoffee, over(boys) //
stack title("Consumption of Adult Goods by Number of Boys in Household") //
ytitle("Percent of Total Consumption", margin(0 4 0 2)) //
blabel(bar, format(%9.4f) position(inside) color(white) size(vsmall)) //
legend(label(1 "clothing") label(2 "alcohol") label(3 "tobacco") label(4 "coffee")) //
saving("graph1", replace) 
restore

preserve
drop if girls>6
graph bar pctclothing pctalcohol pcttobacco pctcoffee, over(girls) //
stack title("Consumption of Adult Goods by Number of Girls in Household") //
ytitle("Percent of Total Consumption", margin(0 4 0 2)) //
blabel(bar, format(%9.4f) position(inside) color(white) size(vsmall)) //
legend(label(1 "clothing") label(2 "alcohol") label(3 "tobacco") label(4 "coffee")) //
saving("graph2", replace) 
restore

