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
        keepunmatchedinfo # vector of variables displayed in header even if unmatched
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
            export_values(df_match_row, path_file)
            return(list(profile_url = path_file)) # return url
            })
    }

export_values <- function(df_match_row, path_file) {
    '
    - Define combination rules to keep short (makes only sense within categories)
    - Omit categories without matches
    - Return values signales that everything was processed and file is not empty
    '
    # use template document in /src
    template <- paste(readLines(paste0(wd,"/src/profile_template.html")))
    # file to be written
    output_file <- file(path_file)
    # gather header content
    header_set <- colnames(df_match_row)[
        grepl("match_profileheader_", colnames(df_match_row))
        ]
    message(header_set)
    content_header <- ""
    # gather main content
    content_set <- colnames(df_match_row)[
        !grepl(paste(keepunmatchedinfo, collapse="|"), colnames(df_match_row))
        ]
    message(content_set)
    content_main <- ""
    # for (col in colnames(match_info)) {
    #     matchvarparams$get(matchvar)$get("split", FALSE)
    # }
    # insert into template and write everything
    writeLines(paste(template), output_file)
    # close connection
    close(output_file)

}

### RUN ###
test <- exporter(
    df=df_merged,
    path_dir=paste0(wd,"/profiles/"), # where to store profiles
    keepunmatchedinfo=c("age", "initials", "gender", "currentstate") # header info
    )
test
