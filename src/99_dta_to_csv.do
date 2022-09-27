set seed 42
use "/home/max/Seafile/Social-Media-Page/Data_with_printing_rules_20220926.dta", clear

// csv
export delimited ///
	using "/home/max/Seafile/Projects/matched-social-media-profile/data/Data_with_printing_rules_20220926.csv" ///
	, replace quote
