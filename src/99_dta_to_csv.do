set seed 42
use "/home/max/Seafile/Social-Media-Page/Data_with_printing_rules_20221012.dta", clear

// csv
export delimited ///
	using "/home/max/Seafile/Projects/matched-social-media-profile/data/Data_with_printing_rules_20221012.csv" ///
	, replace quote
