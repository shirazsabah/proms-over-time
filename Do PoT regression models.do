***** Knees

*******************************************************************************
 ***********************      Variable prep      *****************************
*******************************************************************************

clear
cls
cd "/Users/shirazsabah/Library/Mobile Documents/com~apple~CloudDocs/Revision TKR/NPROMS/PROMS Temporal Trends/TT"

use nproms
encode year, gen(yr)
tab proc yr
*Note that early years do not have this indicator
keep if proc==2 & revisionflag==0

*Adds numbers to all value labels
*numlabel, add

drop if yr==3

*Create group for <60y & >80y
*Recode age suppressed to missing
gen age=.
replace age=1 if ageband =="40 to 49"
replace age=1 if ageband =="50 to 59"
replace age=2 if ageband =="60 to 69"
replace age=3 if ageband =="70 to 79"
replace age=4 if ageband =="80 to 89"
replace age=4 if ageband =="90 to 120"
replace age=. if ageband =="9"

lab de age_lbl ///
1 "<60" ///
2 "60-69" ///
3 "70-79" ///
4 "80+"
label values age age_lbl

*Recode preopqdisability so it matches 0/1 coding of all other binary variables
recode preopqdisability 2=0

egen comorb = rowtotal(arthritis heartdisease highbp stroke circulation lungdisease diabetes kidneydisease nervoussystem liverdisease cancer depression)

replace comorb=3 if comorb>=3

tab comorb

egen complication = anycount(postopqallergy postopqwound postopqbleeding postopqfurthersurgery postopqreadmitted postopqurine), values(1)
tab complication, mi


*Number of complications per person
foreach var of varlist postopqallergy postopqbleeding postopqwound postopqurine postopqfurthersurgery postopqreadmitted {
	replace `var'=0 if `var'==2
}

egen numcomps = rsum2(postopqallergy postopqbleeding postopqwound postopqurine postopqfurthersurgery postopqreadmitted), allmiss

tab numcomps, mi

gen comp01 = numcomps
replace comp01 = 1 if comp01>0 & comp01!=.
tab comp01, mi

*MIC-adj 8 for primary THR, 7 for primary TKR
gen mic=.
replace mic =0 if coks<7
replace mic =1 if coks>=7 & coks!=.
tab mic, mi

*Satisfaction
gen satisf =.
replace satisf =0 if postopqsatisfaction >3 & postopqsatisfaction!=.
replace satisf =1 if postopqsatisfaction <4

*Success
gen success =.
replace success =0 if postopqsucess >2 & postopqsucess!=.
replace success =1 if postopqsucess <3

save "kneett.dta", replace

*******************************************************************************
 ********************      Variables explained      **************************
*******************************************************************************

/*The pre-operative patient factors explored were: 
(i) age (<60/ 60-69/ 70-79/ ≥80);
age

(ii) gender (male/ female); 
gender

(iii) year of surgery (2013-4/ 2014-5/ 2015-6/ 2016-7/2017-8/ 2018-9/2019-20); 
yr

(iv) symptom period (<1 year/ 1-5 years/ 6-10 years/ >10 years); 
preopqsymptomperiod

(v) living arrangements (with partner, spouse, family or friends/ alone/ nursing home or facility/ other); 
preopqlivingarrangements

(vi) perceived disability (yes/ no); 
preopqdisability

(vii) self-reported comorbidities -arthritis, cancer, circulation, depression, diabetes, heart disease, high blood pressure, kidney disease, liver disease, lung disease, nervous system disorder, stroke (yes/ no, for each comorbidity); 
heartdisease highbp stroke circulation lungdisease diabetes kidneydisease nervoussystem liverdisease cancer arthritis depression

(viii) total number of comorbidities; 
comorb

(ix) each EQ5D utility dimension; 
preopqmobility preopqselfcare preopqactivity preopqdiscomfort preopqanxiety

(x) baseline OKS
krpreopqscore

*/


*******************************************************************************
 ********************     Assessing correlation     **************************
*******************************************************************************

/*
**Binary
global bin "gender preopqdisability heartdisease highbp stroke circulation lungdisease diabetes kidneydisease nervoussystem liverdisease cancer arthritis depression"

**Ordinal/Categorical
global ord "age yr preopqsymptomperiod preopqlivingarrangements comorb preopqmobility preopqselfcare preopqactivity preopqdiscomfort preopqanxiety"

global binord "gender preopqdisability heartdisease highbp stroke circulation lungdisease diabetes kidneydisease nervoussystem liverdisease cancer arthritis depression age yr preopqsymptomperiod preopqlivingarrangements comorb preopqmobility preopqselfcare preopqactivity preopqdiscomfort preopqanxiety"

** All missing data is coded as .
* codebook $bin $ord krpreopqscore

*******************************************************************************
*Binary v Binary
*rows, columns, preset content

mat drop _all
matrix bb = J(14,14,.)
matrix colnames bb= $bin
matrix rownames bb= $bin

local j = 0
foreach x of varlist $bin {
local j = `j'+ 1
local k = 1
		foreach y of varlist $bin {
		qui tab `x' `y', V
		matrix bb[`j',`k'] = (r(CramersV))
		local k = `k' + 1
		}
}

// list the matrix
mat list bb

// get original column names of matrix
local names : colfullnames bb

// get original row names of matrix (and row count)
local rownames : rowfullnames bb
local c : word count `rownames'

// make original names legal variable names
local newnames
foreach name of local names {
    local newnames `newnames' `=strtoname("`name'")'
}

// rename columns of matrix
matrix colnames bb = `newnames'

// convert matrix to dataset
frame create bb
frame change bb
svmat bb, names(col)

// add matrix row names to dataset
gen rownames = ""
forvalues i = 1/`c' {
    replace rownames = "`:word `i' of `rownames''" in `i'
}

// list
order rownames

*******************************************************************************
*Binary/Ord v Ordinal
*Quicker to correlate everything with everything

frame change default
cls
qui spearman $binord, matrix
matrix S = r(Rho)
frame create sp
frame change sp
svmat S, names(col)

*******************************************************************************
*Binary/Ord v Continuous

cls
frame change default
foreach var of varlist $binord { 
	pwcorr `var' krpreopqscore
}

*******************************************************************************
*Look at screen output --> No correlations >0.7

*Look at frame bb --> No correlations >0.7
frame change bb
foreach var of varlist $bin {
	replace `var'=. if `var'>-0.7 & `var'<0.7
}

*Look at frame sp --> No correlations >0.7
frame change sp
foreach var of varlist $binord {
	replace `var'=. if `var'>-0.7 & `var'<0.7
}

frame change default
frame drop bb
frame drop sp
cls

*/


*******************************************************************************
 ********************         Case-mix models        **************************
*******************************************************************************

clear
cls
use kneett

*POST score
eststo m1, title(Post-operative OHS): regress krpostopqscore yr krpreopqscore i.gender i.preopqdisability i.heartdisease i.highbp i.stroke i.circulation i.lungdisease i.diabetes i.kidneydisease i.nervoussystem i.liverdisease i.cancer i.arthritis i.depression i.age i.preopqsymptomperiod i.preopqlivingarrangements i.comorb, baselevels
	
*MIC
*Stata has two commands for logistic regression, logit and logistic. The main difference between the two is that the former displays the coefficients and the latter displays the odds ratios

eststo m2, title(Minimal important change): logit mic yr krpreopqscore i.gender i.preopqdisability i.heartdisease i.highbp i.stroke i.circulation i.lungdisease i.diabetes i.kidneydisease i.nervoussystem i.liverdisease i.cancer i.arthritis i.depression i.age i.preopqsymptomperiod i.preopqlivingarrangements i.comorb, or baselevels

*EQ-5D
eststo m3, title(Post-operative EQ-5D): regress postopqeq5dindex yr preopqeq5dindex i.gender i.preopqdisability i.heartdisease i.highbp i.stroke i.circulation i.lungdisease i.diabetes i.kidneydisease i.nervoussystem i.liverdisease i.cancer i.arthritis i.depression i.age i.preopqsymptomperiod i.preopqlivingarrangements i.comorb, baselevels

*Satisfaction (yes/no)
eststo m4, title(Satisfaction): logit satisf yr i.gender i.preopqdisability i.heartdisease i.highbp i.stroke i.circulation i.lungdisease i.diabetes i.kidneydisease i.nervoussystem i.liverdisease i.cancer i.arthritis i.depression i.age i.preopqsymptomperiod i.preopqlivingarrangements i.comorb, or baselevels

*Success (yes/no)
eststo m5, title(Success): logit success yr i.gender i.preopqdisability i.heartdisease i.highbp i.stroke i.circulation i.lungdisease i.diabetes i.kidneydisease i.nervoussystem i.liverdisease i.cancer i.arthritis i.depression i.age i.preopqsymptomperiod i.preopqlivingarrangements i.comorb, or baselevels

*Complications (yes/no)
eststo m6, title(Complications): logit comp01 yr i.gender i.preopqdisability i.heartdisease i.highbp i.stroke i.circulation i.lungdisease i.diabetes i.kidneydisease i.nervoussystem i.liverdisease i.cancer i.arthritis i.depression i.age i.preopqsymptomperiod i.preopqlivingarrangements i.comorb, or baselevels


esttab m1 m3 using kr1.csv, plain replace varwidth(20) label cells(b(fmt(3)) ci(fmt(3) par) p(fmt(3) par)) ///
keep(yr) ///
collabels(none) nonumbers mtitles("Post-operative OKS" "Post-operative EQ-5D") ///
coeflabels( ///
yr "Year" ///
) ///
compress se ar2 nostar ///
eqlabels(" " " ") ///
addnotes("95% confidence intervals in brackets; p-values in parentheses.")
	
esttab m2 m4 m5 m6 using kr2.csv, plain replace varwidth(20) label cells(b(fmt(3)) ci(fmt(3) par) p(fmt(3) par)) eform ///
keep(yr) ///
collabels(none) nonumbers mtitles("Minimal Important Change" "Satisfaction" "Success" "Complications") ///
coeflabels( ///
yr "Year" ///
) ///
compress se ar2 nostar ///
eqlabels(" " " ") ///
addnotes("95% confidence intervals in brackets; p-values in parentheses.")







*******************************************************************************

***** Hips

*******************************************************************************
 ***********************      Variable prep      *****************************
*******************************************************************************

estimates clear
clear
cls
cd "/Users/shirazsabah/Library/Mobile Documents/com~apple~CloudDocs/Revision TKR/NPROMS/PROMS Temporal Trends/TT"

use nproms
encode year, gen(yr)
tab proc yr
keep if proc==1 & revisionflag==0
drop if yr==3

*Create group for <60y & >80y
*Recode age suppressed to missing
gen age=.
replace age=1 if ageband =="40 to 49"
replace age=1 if ageband =="50 to 59"
replace age=2 if ageband =="60 to 69"
replace age=3 if ageband =="70 to 79"
replace age=4 if ageband =="80 to 89"
replace age=4 if ageband =="90 to 120"
replace age=. if ageband =="9"

lab de age_lbl ///
1 "<60" ///
2 "60-69" ///
3 "70-79" ///
4 "80+"
label values age age_lbl

*Recode preopqdisability so it matches 0/1 coding of all other binary variables
recode preopqdisability 2=0

egen comorb = rowtotal(arthritis heartdisease highbp stroke circulation lungdisease diabetes kidneydisease nervoussystem liverdisease cancer depression)

replace comorb=3 if comorb>=3

tab comorb

egen complication = anycount(postopqallergy postopqwound postopqbleeding postopqfurthersurgery postopqreadmitted postopqurine), values(1)
tab complication

*Number of complications per person
foreach var of varlist postopqallergy postopqbleeding postopqwound postopqurine postopqfurthersurgery postopqreadmitted {
	replace `var'=0 if `var'==2
}

egen numcomps = rsum2(postopqallergy postopqbleeding postopqwound postopqurine postopqfurthersurgery postopqreadmitted), allmiss

tab numcomps, mi

gen comp01 = numcomps
replace comp01 = 1 if comp01>0 & comp01!=.
tab comp01, mi

*MIC-adj 8 for primary THR, 7 for primary TKR
gen mic=.
replace mic =0 if cohs<8
replace mic =1 if cohs>=8 & cohs!=.
tab mic, mi

*Satisfaction
gen satisf =.
replace satisf =0 if postopqsatisfaction >3 & postopqsatisfaction!=.
replace satisf =1 if postopqsatisfaction <4

*Success
gen success =.
replace success =0 if postopqsucess >2 & postopqsucess!=.
replace success =1 if postopqsucess <3

save "hiptt.dta", replace

*******************************************************************************
 ********************      Variables explained      **************************
*******************************************************************************

/*The pre-operative patient factors explored were: 
(i) age (<60/ 60-69/ 70-79/ ≥80);
age

(ii) gender (male/ female); 
gender

(iii) year of surgery (2013-4/ 2014-5/ 2015-6/ 2016-7/2017-8/ 2018-9/2019-20); 
yr

(iv) symptom period (<1 year/ 1-5 years/ 6-10 years/ >10 years); 
preopqsymptomperiod

(v) living arrangements (with partner, spouse, family or friends/ alone/ nursing home or facility/ other); 
preopqlivingarrangements

(vi) perceived disability (yes/ no); 
preopqdisability

(vii) self-reported comorbidities -arthritis, cancer, circulation, depression, diabetes, heart disease, high blood pressure, kidney disease, liver disease, lung disease, nervous system disorder, stroke (yes/ no, for each comorbidity); 
heartdisease highbp stroke circulation lungdisease diabetes kidneydisease nervoussystem liverdisease cancer arthritis depression

(viii) total number of comorbidities; 
comorb

(ix) each EQ5D utility dimension; 
preopqmobility preopqselfcare preopqactivity preopqdiscomfort preopqanxiety

(x) baseline OHS
hrpreopqscore

*/


*******************************************************************************
 ********************     Assessing correlation     **************************
*******************************************************************************

/*

**Binary
global bin "gender preopqdisability heartdisease highbp stroke circulation lungdisease diabetes kidneydisease nervoussystem liverdisease cancer arthritis depression"

**Ordinal/Categorical
global ord "age yr preopqsymptomperiod preopqlivingarrangements comorb preopqmobility preopqselfcare preopqactivity preopqdiscomfort preopqanxiety"

global binord "gender preopqdisability heartdisease highbp stroke circulation lungdisease diabetes kidneydisease nervoussystem liverdisease cancer arthritis depression age yr preopqsymptomperiod preopqlivingarrangements comorb preopqmobility preopqselfcare preopqactivity preopqdiscomfort preopqanxiety"

** All missing data is coded as .
* codebook $bin $ord hrpreopqscore

*******************************************************************************
*Binary v Binary
*rows, columns, preset content

mat drop _all
matrix bb = J(14,14,.)
matrix colnames bb= $bin
matrix rownames bb= $bin

local j = 0
foreach x of varlist $bin {
local j = `j'+ 1
local k = 1
		foreach y of varlist $bin {
		qui tab `x' `y', V
		matrix bb[`j',`k'] = (r(CramersV))
		local k = `k' + 1
		}
}

// list the matrix
mat list bb

// get original column names of matrix
local names : colfullnames bb

// get original row names of matrix (and row count)
local rownames : rowfullnames bb
local c : word count `rownames'

// make original names legal variable names
local newnames
foreach name of local names {
    local newnames `newnames' `=strtoname("`name'")'
}

// rename columns of matrix
matrix colnames bb = `newnames'

// convert matrix to dataset
frame create bb
frame change bb
svmat bb, names(col)

// add matrix row names to dataset
gen rownames = ""
forvalues i = 1/`c' {
    replace rownames = "`:word `i' of `rownames''" in `i'
}

// list
order rownames

*******************************************************************************
*Binary/Ord v Ordinal
*Quicker to correlate everything with everything

frame change default
cls
qui spearman $binord, matrix
matrix S = r(Rho)
frame create sp
frame change sp
svmat S, names(col)

*******************************************************************************
*Binary/Ord v Continuous

cls
frame change default
foreach var of varlist $binord { 
	pwcorr `var' hrpreopqscore
}

*******************************************************************************
*Look at screen output --> No correlations >0.7

*Look at frame bb --> No correlations >0.7
frame change bb
foreach var of varlist $bin {
	replace `var'=. if `var'>-0.7 & `var'<0.7
}

*Look at frame sp --> No correlations >0.7
frame change sp
foreach var of varlist $binord {
	replace `var'=. if `var'>-0.7 & `var'<0.7
}

frame change default
frame drop bb
frame drop sp
cls

*/

*******************************************************************************
 ********************         Case-mix model        **************************
*******************************************************************************

clear
cls
use hiptt

*POST score
eststo m1, title(Post-operative OHS): regress hrpostopqscore yr hrpreopqscore i.gender i.preopqdisability i.heartdisease i.highbp i.stroke i.circulation i.lungdisease i.diabetes i.kidneydisease i.nervoussystem i.liverdisease i.cancer i.arthritis i.depression i.age i.preopqsymptomperiod i.preopqlivingarrangements i.comorb, baselevels

*MIC
eststo m2, title(Minimal important change): logit mic yr hrpreopqscore i.gender i.preopqdisability i.heartdisease i.highbp i.stroke i.circulation i.lungdisease i.diabetes i.kidneydisease i.nervoussystem i.liverdisease i.cancer i.arthritis i.depression i.age i.preopqsymptomperiod i.preopqlivingarrangements i.comorb, or baselevels

*EQ-5D
eststo m3, title(Post-operative EQ-5D): regress postopqeq5dindex yr preopqeq5dindex i.gender i.preopqdisability i.heartdisease i.highbp i.stroke i.circulation i.lungdisease i.diabetes i.kidneydisease i.nervoussystem i.liverdisease i.cancer i.arthritis i.depression i.age i.preopqsymptomperiod i.preopqlivingarrangements i.comorb, baselevels

*Satisfaction (yes/no)
eststo m4, title(Satisfaction): logit satisf yr i.gender i.preopqdisability i.heartdisease i.highbp i.stroke i.circulation i.lungdisease i.diabetes i.kidneydisease i.nervoussystem i.liverdisease i.cancer i.arthritis i.depression i.age i.preopqsymptomperiod i.preopqlivingarrangements i.comorb, or baselevels


*Success (yes/no)
eststo m5, title(Success): logit success yr i.gender i.preopqdisability i.heartdisease i.highbp i.stroke i.circulation i.lungdisease i.diabetes i.kidneydisease i.nervoussystem i.liverdisease i.cancer i.arthritis i.depression i.age i.preopqsymptomperiod i.preopqlivingarrangements i.comorb, or baselevels
	
*Complications (yes/no)
eststo m6, title(Complications): logit comp01 yr i.gender i.preopqdisability i.heartdisease i.highbp i.stroke i.circulation i.lungdisease i.diabetes i.kidneydisease i.nervoussystem i.liverdisease i.cancer i.arthritis i.depression i.age i.preopqsymptomperiod i.preopqlivingarrangements i.comorb, or baselevels


esttab m1 m3 using hr1.csv, plain replace varwidth(20) label cells(b(fmt(3)) ci(fmt(3) par) p(fmt(3) par)) ///
keep(yr) ///
collabels(none) nonumbers mtitles("Post-operative OHS" "Post-operative EQ-5D") ///
coeflabels( ///
yr "Year" ///
) ///
compress se ar2 nostar ///
eqlabels(" " " ") ///
addnotes("95% confidence intervals in brackets; p-values in parentheses.")
	
esttab m2 m4 m5 m6 using hr2.csv, plain replace varwidth(20) label cells(b(fmt(3)) ci(fmt(3) par) p(fmt(3) par)) eform ///
keep(yr) ///
collabels(none) nonumbers mtitles("Minimal Important Change" "Satisfaction" "Success" "Complications") ///
coeflabels( ///
yr "Year" ///
) ///
compress se ar2 nostar ///
eqlabels(" " " ") ///
addnotes("95% confidence intervals in brackets; p-values in parentheses.")
