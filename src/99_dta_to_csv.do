set seed 42
use "C:\Users\sprenmax\Seafile\Social-Media-Page\Data_with_printing_rules_20220818.dta", clear
keep if useable=="1 Yes"
gen r = runiform()
sort r
drop r
keep if _n<=1000 // use subset for testing
export delimited ///
	using "C:/Users/sprenmax/Seafile/projects/matched-social-media-profile/testdata_2022-08-18.csv" ///
	, replace quote
