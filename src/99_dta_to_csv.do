set seed 42
use "/home/max/Seafile/Social-Media-Page/Data_with_printing_rules_20220907.dta", clear

// selection with usable input
keep if useable=="1 Yes"

// additional cleaning (will be passed on to Ferdi)
rename homestate homestate_ger
rename incomebrackets incomebracket

// csv
export delimited ///
	using "/home/max/Seafile/Projects/matched-social-media-profile/data/testdata_2022-09-07.csv" ///
	, replace quote
