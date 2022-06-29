################################################################################
#
#   EXPORT SOCIAL MEDIA PROFILES
#
################################################################################


### FUNCTIONS ###

exporter <- function(
        df,
        idcol="lfdn", # person id, numeric
        path_dir,
        keepunmatchedinfo=c("age", "initials", "gender", "currentstate") # vector of variables displayed in header even if unmatched
        ) {
    '
    - Wrapper function
    - Input: df input, multiple rows with matches per person
    - Output: vector with profile urls matching passed df length
    '

    # Step 1: keep best(?) matches for each person
    '
    What is the approach here? Also very different persons? Then the selection
    rule should be which.min() and the matcher needs to return the values of all
    variables irrespective of being matched or not.
    '
    df_match_best <- df %>%
        group_by_at(idcol) %>% slice(which.max(match_simscore))

    # for each person and match, export match profile and add file name/url to df
    matched_set <- colnames(df_match_best)[grepl("match_", colnames(df_match_best))]
    df_match_best <- df_match_best[,c(idcol, matched_set)]
    df_match_best <- df_match_best %>%
        pmap_df(function(...) {
            df_match_row <- tibble(...) # info to use
            path_file <- paste0(path_dir, df_match_row[1,idcol], ".html")
            export_values(df_match_row, path_file, keepunmatchedinfo)
            return(list(profile_url = path_file)) # return url
            })
    }

export_values <- function(df_match_row, path_file, keepunmatchedinfo) {
    '
    - In matchparams: define combination rules to keep short!
    - Omit categories without matches
    - CAUTION: Also refers to matchvarparams, but the variable names are
      prefixed with "match_" (matched values) or "match_profileheader" (can
      contain unmatched values, displayed in profile header). These prefixes
      have to be stripped when used as keys! (see below)
    '
    # use template document in /src
    template <- paste(readLines(paste0(wd,"/src/profile_template.html")))

    # drop everything from row that is NA
    df_match_row <- df_match_row[colSums(!is.na(df_match_row)) > 0]

    ### gather header content
    '
    - Initials:
      - Hard coded!
      - If missing: use random character
    '
    content_header <- ""
    # initials
    content_header <- paste0(
        content_header,
        "<h1>",
        ifelse("match_profileheader_initials" %in% colnames(df_match_row),
            df_match_row$match_profileheader_initials,
            sample(LETTERS, 1)),
        ".</h1>")
    # other header info
    header_set <- lapply(
        keepunmatchedinfo[!keepunmatchedinfo %in% "initials"],
        function(x) paste0("match_profileheader_", x)
        )
    header_set <- intersect(header_set, colnames(df_match_row))
    header_set_keys <- lapply(header_set, function(x) gsub("match_profileheader_", "", x))
    header_print <- mapply(
        function(col, key) {
            value <- df_match_row[col]
            printer <- matchvarparams$get(key)$get("print")
            printer(value=value, header=TRUE)$print
            },
        unlist(header_set),
        unlist(header_set_keys)
        )
    content_header <- paste(
        content_header,
        '<p class="quote">',
        paste(header_print, collapse=", "),
        "</p>",
        sep="\n"
        )

    ### gather main content (exclude what is in header)
    '
    - Makes use of specific print functions given in matchvarparams. These
      functions can be extended at will to accomodate specific combinations, etc.
    '
    content_main <- ""
    content_set <- colnames(df_match_row)[
        !grepl(paste0("lfdn|", paste(
            lapply(c("lfdn", "simscore", keepunmatchedinfo), function(x) paste0("_", x)),
            collapse="|"
            )),
        colnames(df_match_row))
        ] # grep: make sure patterns do not include other variables!
    content_main_grouped <- list()
    content_main_tracker <- c("match_totlanguage") # items already printed; also used to exclude items
    for (col in content_set) {
        # loop over elements, but keep them together in groups in list
        # skip over already printed items (possible when combined)
        key <- gsub("match_", "", col)
        if (!col %in% content_main_tracker) {
            group <- matchvarparams$get(key)$get("group")
            if (!group$id %in% names(content_main_grouped)) {
                # wrapper
                content_main_grouped[group$id] <- paste0(
                    '<div id="',
                    group$id,
                    '" class="info-group">',
                    '<div class="info-group-head">',
                    '<h2><span class="info-group-icon fa-solid ',
                    group$icon,
                    '"></span>',
                    group$label,
                    '</h2></div><ul class="info-items">'
                    )
            }
            # write list elements in wrapper
            value <- df_match_row[col]
            printer <- matchvarparams$get(key)$get("print")
            printed <- printer(df=df_match_row, value=value)
            content_main_grouped[group$id] <- paste(
                content_main_grouped[group$id],
                printed$print, # pass df for combinations
                sep="\n"
                )
            content_main_tracker <- c(content_main_tracker, col, printed$track)
            }
        }
    content_main_grouped <- lapply(
        content_main_grouped, function(x) paste(x, "</ul></div>", sep="\n")
        )
    content_main <- paste(content_main_grouped, collapse="\n")

    ### insert into template and write everything
    content <- gsub("--HEADER--", content_header, template)
    content <- gsub("--MAIN--", content_main, content)
    stri_write_lines(content, path_file)
}

### RUN ###
profiles <- exporter(
    df=df_merged,
    path_dir=paste0(wd,"/profiles/"), # where to store profiles
    keepunmatchedinfo=c("initials", "gender", "age", "currentstate") # used in header
    )
profiles
