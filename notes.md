# Notes

## Matching (see 02_match.R for details)
- There are some differences in the implementation compared to the Baliettia Paper :
    - Number of dimensions (how does this affect similarity cutoffs?)
    - Categories in dimensions (and associated weights)
    - Some weights are not super sensible to me
    - Not always clear what exactly was passed on to determine the weights, esp. when multiple answers to open items are possible
- Cleaning and fuzzy matching
    - Would be best to manually clean open answers, but might be cumbersome
    - Fuzzy matching is implemented and allows for a max. distance specification (= max. no. of deletions, insertions, replacements), but these distances have to be carefully tested
- Generally:
    - The implementation needs to be carefully tested
    - One issue is the string encoding

## Profile
- The documentation of the Baliettia Paper says that just matched info was displayed in the profiles. Is that sensible? Is the header info shown irrespective of match?
- What belongs in header?
- Do we want to use alternative signals (e.g. showing gender via different avatars)?
- Gender terms?
- Is external JS/CSS possible? -> FontAwesome Icons (could also be implemented via links to external svgs, or downloaded)
