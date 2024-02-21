*-------------------------------------------------------------------------------
* Date			   January 18, 2023
* Author		   Ila Gupta

* Task 			   Stata Coding Sample

* Datasets		   treatment_status.csv
*				   endline.dta
*			   	   baseline_controls.dta
*-------------------------------------------------------------------------------

*----------------------------------- SET-UP ------------------------------------
	clear all
	
	log using EGC_StataTest, replace												// start log file
	
	ssc install estout																// install Stata packages 
	ssc install outreg2

	global main 	"--/Downloads/EGC_Stata_Test_2022" 								// set pathways
	global raw 		"$main/raw"
	global clean 	"$main/clean"
	global output 	"$main/output"
	global tex		"$output/tex"
	cd "$main"

*-------------------------------- DATA PREPARATION -----------------------------
 
* load endline data
	use "$raw/endline.dta", clear
	
    label var hhid "Household ID"													
	label var group_id "Group ID"
	label var totformalborrow_24 "Total formal borrowing (INR), past 24 mos."
	label var totinformalborrow_24 "Total informal borrowing (INR), past 24 mos."
	label var hhinc "Household income (INR), last 30 days"
	label var hhnomembers "Number of household members"
	label var survey_round "Survey round"
	
* recoding hh debt
	foreach var of varlist totformalborrow_24 totinformalborrow_24 hhinc {
		sum `var'
		tab `var', missing
		list hhid if `var'=="."
		list hhid if hhinc =="Refuse to answer"
	}
	
		/* note- totformalborrow_24 has 4 missing obs, 1,221 "None"
			     totinformalborrow_24 has 4 missing obs, 1,539 "None"
				 hhinc has 4 "Refuse to answer", 244 "None"
				 HHIDs for missing & refuse to answer: 289, 1507, 2922, 3269 */
*/
	
	foreach var of varlist totformalborrow_24 totinformalborrow_24 hhinc {
		replace `var'="." if `var'=="Refuse to answer"
		replace `var'="0" if `var'=="None"
		destring `var', replace
	}
	
* describe financial status of the households in this sample
// produce summary table 
	eststo clear																												
	estpost sum totformalborrow_24 totinformalborrow_24 hhinc hhnomembers
	esttab using "$tex/summary_table.tex", replace ///
	label cells("mean(label(Mean)) sd(label(S.D.)) min(label(Min)) max(label(Max))") ///
	title("Descriptive Statistics on Household Financial Status")
		
// bar graph: borrowing by household income percentile
	xtile hhinc_pctile = hhinc, nq(4)												// var for hh income percentile
	gen totborrow = totformalborrow_24 + totinformalborrow_24						// var for total borrowing
	gen pct_formalborrow = totformalborrow_24/totborrow								// var for % of formal borrowing
	
// producing bar graph
	graph bar pct_formalborrow, over(hhinc_pctile) ///								
		title("Formal Borrowing by Household Income Percentile") ///
		ytitle("% Formal Borrowing of Total Borrowing") ///
		blabel(bar, size(vsmall))  bargap(-20)
	graph export "$output/formalborrowing.jpg", replace
		
	drop hhinc_pctile totborrow pct_formalborrow
	   
* top code-debt and income to 3 s.d. above mean 
	foreach var of varlist totformalborrow_24 totinformalborrow_24 hhinc {
		gen `var'_tc = `var'
		gen tc = r(mean) + (3 * r(sd))
		replace `var'_tc = tc if `var' > tc
		drop tc
	}

* labeling top-coded variables
	label var totformalborrow_24_tc ///
		"Total formal borrowing, top-coded 3 s.d. above mean"
	label var totinformalborrow_24_tc ///
		"Total informal borrowing, top-coded 3 s.d. above mean"
	label var hhinc_tc ///
		"Household income, top-coded 3 s.d. above mean"

* reason for top-coding
		   
* create var for total borrowed as sum of formal and informal borrowing
	gen totborrow_24 = totformalborrow_24 + totinformalborrow_24
	label var totborrow_24 "Total borrowing (INR), past 24 mos."

* merge endline data w/ treatment_status.csv to assign hh treatment status
	save "$clean/endline_clean.dta", replace										
	import delimited "$raw/treatment_status.csv", clear								

	label var pair_id "Pair ID"													
	label var group_id "Service Group ID"
	label var treated "Treated"
	
	sort pair_id group_id															
	
	save "$clean/treatment_status_clean.dta", replace								
	merge m:m group_id using "$clean/endline_clean.dta"								
	drop _merge	
	* note- all 4,160 observations are matched

* create a dummy variable for households below poverty line (26.995 INR/day)
	gen inc_percap = (hhinc_tc/30) / hhnomembers
	
	gen poverty = inc_percap
		replace poverty = 1 if inc_percap < 26.995 & inc_percap != .
		replace poverty = 0 if inc_percap > 26.995 & inc_percap != .
		
	/* tab poverty
	note- 4 missing values follow from missing hhinc obs */
	
	drop inc_percap
	
	label var poverty "Household below poverty line (26.995 INR/day) [1=below poverty line]"

	  
* merge working data with baseline controls dataset
	save "$clean/endline_treatment_clean.dta", replace				 

	use "$raw/baseline_controls.dta", clear											// import baseline controls 
	
	label var group_id "Service Group ID"											
	label var hhnomembers "Number of household members"
	
	save "$clean/baseline_controls_clean.dta", replace								
	
// merge baseline controls with previous working dataset
	isid hhid																		// checking for hhid uniqueness
	
	merge 1:1 hhid using "$clean/endline_treatment_clean.dta"
			
	drop if _merge == 1
	
	/* note- We  drop 264 observations here from the master (baseline controls) 
	dataset since these observations were not recorded as part of control or 
	treatment groups. To be clear, for this set of observations, pair_id, 
	survey_round, and treated variables are missing; hence, we will not move 
	forward with them in the analysis. We do not drop if _merge == 2, since for
	these observations, we are missing baseline demographics, but not 
	treatment/control results. */

	save "$clean/merged_final.dta", replace											
	
*--------------------------------- ANALYSIS ------------------------------------

/* Using the given data, we can begin to assess the impact of the bank program 
   on household income and poverty persistence in rural South India. Motivated 
   by existing literature "Do Rural Banks Matter?" (Burgess & Pande, 2003) and 
   "Bank Financing in India" (Banerjee, Cole, & Duflo, 2003), we can posit that 
   local bank expansion in treated areas corresponds to reduction of poverty on 
   average when the program is specifically instituted to support rural 
   households (as in this RCT design). By this hypothesis, we might expect 
   higher household incomes on average as local bank lending may contribute to 
   growth in local commerce, mobilization of credit, and savings opportunities. 
   Moreover, households will be less likely to fall below the poverty line of 
   27 INR. */
		   
*  t-tests for treatment/control 
	global hhcontrols hhnomembers gender_hoh age_hoh educyears_hoh ///
	hhnomembers_above18 hhreg_muslim hhcaste_fc
					  
	eststo clear
	estpost ttest $hhcontrols, by(treated) unequal
	esttab .
	esttab using "$output/ttests.tex", replace
   
* OLS regress household income on treatment dummy 
	reg hhinc treated i.pair_id, vce(cluster pair_id)
	outreg2 using "$output/reg_results.tex", keep(hhinc treated) replace

*  gen log income and re-run the previous specification on treatment 
	gen log_inc = log(hhinc)
	reg log_inc treated i.pair_id, vce(cluster pair_id)
	outreg2 using "$output/log_reg_results.tex", keep(log_inc treated) replace

* re-run including additional household-level controls
	reg log_inc treated i.pair_id $hhcontrols, vce(cluster pair_id)
	outreg2 using "$output/log_controls_reg_results.tex", keep(log_inc ///
	treated $hhcontrols) replace	
	
/* generating bar chart summarizing average borrowed amount for each income 
   quartile, by treatment group */
	xtile hhinc_tc_pctile = hhinc_tc, nq(4)
	
	gen totborrow_control = totborrow_24 if treated == 0
	gen totborrow_treat = totborrow_24 if treated == 1
	
	graph bar (mean) totborrow_control totborrow_treat, ///
		over(hhinc_tc_pctile, gap(10)) ///
		title("Average Borrowed Amount by Household Income Quartile") ///
		ytitle("Average Borrowed (INR)") ///
		legend(label(1 "Control") label(2 "Treatment")) ///
		note("Source: Yale University, Economic Growth Center", size(vsmall)) ///
		blabel(bar, size(vsmall) format(%9.0f) position(outside))

	graph export "$output/final_bar.png", as(png) replace							

	log close																		// close log file

*-------------------------------------------------------------------------------
