################################################################################
#
#   EXPORT SOCIAL MEDIA PROFILES
#
################################################################################

### PARAMETERS ###
source("src/02_matchparams.R")

### FUNCTIONS ###

exporter <- function(
        df,
        matchparams,
        idcol="lfdn", # person id, NUMERIC (change as.numeric to as.character if not)
        export_indicator, # dummy indicating which observations to be exported
        path_dir,
        valuesNA
        ) {
    '
    - Wrapper function
    - Input df:
      - With indicator "profile_match_export" (bool)
      - Enumerated print strings for the respective printing blocks (=HTML divs
        of the item groups, e.g. family). These have to be generated beforehand,
        meaning that the cleaning/combining complexity is no longer part of the
        matchvarparams dict (all print functions removed)
    - Output df: df with pids and profile urls for the matched and exported profile
    '

    # for each person and match, export match profile and add file name/url to df;
    # fetch info via match_id (match_lfdn), use only print strings and id info
    print_set <- colnames(df)[
        grepl(
            paste(c("header", unlist(matchparams$keys())), collapse="[0-9]+|"),
            colnames(df)
            )
        ]
    df <- df[, c(idcol, paste0("match_", idcol), export_indicator, print_set)]
    df_match_export <- df[df[export_indicator]==1,] # those to be exported
    df_match_export <- df_match_export %>%
        pmap_df(function(...) {
            # person for which we export a matched profile
            df_row <- tibble(...)
            # person matched
            match_id <- as.numeric(df_row[1, paste0("match_", idcol)])
            df_match_row <- df[df[idcol]==match_id,]
            df_match_row <- df_match_row[1,]
            # file path
            path_file <- paste0(path_dir, df_row[1,idcol], ".html")
            # export profile
            export_values(
                df_match_row,
                idcol=idcol,
                path_file,
                matchparams=matchparams,
                valuesNA=valuesNA)
            # return url (only thing stored, to be merged to df)
            return(data.frame(idcol = df_row[1,idcol], match_profile_url = path_file))
            })
    }

export_values <- function(df_match_row, idcol, path_file, matchparams, valuesNA) {
    '
    - References are mostly hard-coded (e.g. header1 for intials). Bear in mind
      when changing the references.
    '
    # use template document in /src
    template <- paste(readLines(paste0(wd,"/src/profile_template.html")))

    # drop everything from row that is NA according to our spec
    df_match_row <- df_match_row[colSums(!is.na(df_match_row)) > 0]
    df_match_row <- df_match_row[,which(!(df_match_row[1,] %in% valuesNA))]

    ### gather header content
    content_header <- ""
    # initials (random char if empty)
    content_header <- paste0(
        content_header,
        "<h1>",
        ifelse("header1" %in% colnames(df_match_row),
            df_match_row["header1"],
            sample(LETTERS, 1)),
        ".</h1>")
    # other header info
    content_header <- paste(
        content_header,
        '<p class="quote">',
        ifelse("header2" %in% colnames(df_match_row),
            df_match_row["header2"],
            ""),
        "</p>",
        sep="\n"
        )

    ### gather main content (exclude what is in header)

    # for keys { for keys not in header_set {PRINT}}
    content_main <- list() # gather group content in list, collapse later
    for (group in unlist(matchparams$keys())) {
        # check if not empty
        item_set <- colnames(df_match_row)[
            grepl(paste0(group, "[0-9]+"), colnames(df_match_row))
            ]
        if (length(item_set)>0) {
            # randomly select print subset
            item_set <- select_subset(group, matchparams, item_set)
            # container start
            content_main[group] <- paste0(
                '<div id="',
                group,
                '" class="info-group">',
                '<div class="info-group-head">',
                '<h2><span class="info-group-icon fa-solid ',
                matchparams$get(group)$get("hl")$icon,
                '"></span>',
                matchparams$get(group)$get("hl")$label,
                '</h2></div><ul class="info-items">'
                )
            # fill with available item info
            for (item in item_set) {
                content_main[group] <- paste(
                    content_main[group],
                    "<li>",
                    as.character(df_match_row[1, item]),
                    "</li>",
                    sep="\n"
                    )
            }
            # container end
            content_main[group] <- paste(
                content_main[group],
                "</ul></div>",
                sep="\n"
                )
            }
        }
    content_main <- paste(content_main, collapse="\n")

    ### insert into template and write everything
    content <- gsub("--HEADER--", content_header, template)
    content <- gsub("--MAIN--", content_main, content)
    stri_write_lines(content, path_file)
}

select_subset <- function(group, matchparams, item_set) {
    '
    This function takes a set of available items per item group and returns
    a random selection which will be printed in the profile.
    '
    printsubset <- matchparams$get(group)$get("printsubset")
    if (length(printsubset$subgroups)>1) {
        item_set <- intersect(
            item_set,
            unlist(sample(printsubset$subgroups, printsubset$max))
        )
    } else {
        item_set <- sample(item_set, printsubset$max)
        }
    return(item_set)
    }


### RUN ########################################################################
'
You need to provide a export_indicator=dummy indicating if a profile should be
exported for the match or not. Dummy created in 03_match.R (match_profile_export).
'

# import matched data
df <- read_feather(paste0(wd, "/data/post_match.feather"))

# export
profiles <- exporter(
    df=df,
    matchparams=matchparams,
    export_indicator="match_profile_export",
    path_dir=paste0(wd,"/profiles/"), # where to store the profiles
    valuesNA=c("-99", "-66", ".", "", "NA", NA) # print string values no to export
    )

# merge back to df
df <- merge(df, profiles, by="lfdn", all=TRUE)

# save (might be HUGE if match no. is not restricted!)
write_feather(df, paste0(wd, "/data/post_export.feather"))
