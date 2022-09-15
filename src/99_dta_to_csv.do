set seed 42
use "/home/max/Seafile/Social-Media-Page/Data_with_printing_rules_20220912.dta", clear

// selection with usable input
keep if useable=="1 Yes"

// csv
export delimited ///
	using "/home/max/Seafile/Projects/matched-social-media-profile/data/testdata_2022-09-12.csv" ///
	, replace quote
