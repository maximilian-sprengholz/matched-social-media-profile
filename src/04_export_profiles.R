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
      - With export indicator (bool), currently "match_profile_export"
      - Enumerated print strings for the respective printing blocks (=HTML divs
        of the item groups, e.g. family). These have to be generated beforehand,
        meaning that the cleaning/combining complexity is no longer part of the
        matchvarparams dict (all print functions removed)
    - Output df: df with pids and profile urls for the matched and exported profile
    '

    # for each person and match, export match profile and add file name/url to df;
    # fetch info via match_id (match_lfdn), use only print strings and id info
    df_match_export <- df[
        df[export_indicator] == 1 & !is.na(df[export_indicator])
        ,
        ] # those to be exported
    df_match_export <- df_match_export %>%
        pmap_df(function(...) {
            # person for which we export a matched profile
            df_row <- tibble(...)
            # person matched
            match_id <- as.numeric(df_row[1, paste0("match_", idcol)])
            df_match_row <- df[df[idcol] == match_id, ]
            df_match_row <- df_match_row[1, ]
            # file path
            path_file <- paste0(path_dir, df_row[1, idcol], ".html")
            # export profile; save sum of simscores of printed items
            profile_simscore <- export_values(
                df_row,
                df_match_row,
                idcol,
                path_file,
                matchparams,
                valuesNA)
            # return url (only thing stored, to be merged to df)
            return(data.frame(
                idcol = df_row[1, idcol],
                match_profile_url = path_file,
                match_profile_simscore = profile_simscore
                ))
            })
    }

export_values <- function(df_row, df_match_row, idcol, path_file, matchparams, valuesNA) {
    '
    - returns the sum of simscores of all printed items in each profile
    - References are mostly hard-coded (e.g. header1 for intials). Bear in mind
      when changing the references.
    '
    # use template document in /src
    template <- paste(readLines(paste0(wd,"/src/profile_template.html")))

    # drop everything from row that is NA according to our spec
    df_match_row <- df_match_row[colSums(!is.na(df_match_row)) > 0]
    df_match_row <- df_match_row[, which(!(df_match_row[1, ] %in% valuesNA))]

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
        sep = "\n"
        )

    ### gather main content (exclude what is in header)

    # for keys { for keys not in header_set {PRINT}}
    content_main <- list() # gather group content in list, collapse later
    profile_simscore <- 0
    for (group in unlist(matchparams$keys())) {
        # get all print strings (e.g. demography1, 2, ...); check if not empty
        item_set <- sort(colnames(df_match_row)[
            grepl(paste0(group, "[0-9]+"), colnames(df_match_row))
            ])
        if (length(item_set) > 0) {
            # randomly select print subset; sum up corresponding simscore (returned)
            item_selection <- select_subset(group, matchparams, item_set, df_row)
            item_set <- item_selection$items
            profile_simscore <- profile_simscore + item_selection$simscore
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
                    sep = "\n"
                    )
            }
            # container end
            content_main[group] <- paste(
                content_main[group],
                "</ul></div>",
                sep = "\n"
                )
            }
        }
    content_main <- paste(content_main, collapse = "\n")

    ### insert into template and write everything
    content <- gsub("--HEADER--", content_header, template)
    content <- gsub("--MAIN--", content_main, content)
    stri_write_lines(content, path_file)

    # return simscore sum of all printed items
    return(profile_simscore)
}

select_subset <- function(group, matchparams, item_set, df_row) {
    '
    This function selects items for print based on their (combined) similarity
    score and the type of match (minimum vs. maximum similarity) from the passed 
    item set which contains all non-empty print strings per group (e.g. demography).
    '

    # get params
    printsubset <- matchparams$get(group)$get("printsubset")

    # create df with print items that is sortable by simscore sums
    # check if items are grouped further (thingsilike, requires double unlist)
    if (length(printsubset$subgroups) > 1) {
        # select groups as rows
        df_item_set <- data.frame(item = names(printsubset$subgroups))
        # sum over groups x match var scores per print item
        df_item_set$simscore <- sapply(df_item_set$item, function(item) {
            item_scores <- intersect(
                paste0(unlist(printsubset$map[unlist(printsubset$subgroups[item])]), "_score"),
                colnames(df_row)
                )
            if (!all(is.na(item_scores))) {
                simscore <- rowSums(df_row[, item_scores], na.rm = TRUE)
            } else {
                simscore <- NA
                }
            return(simscore)
            })
    } else {
        # select print items as rows
        df_item_set <- data.frame(item = item_set)
        # sum over match var scores per print item
        df_item_set$simscore <- sapply(df_item_set$item, function(item) {
            item_scores <- intersect(
                paste0(unlist(printsubset$map[item]), "_score"),
                colnames(df_row)
                )
            if (!all(is.na(item_scores))) {
                simscore <- rowSums(df_row[, item_scores], na.rm = TRUE)
            } else {
                simscore <- NA
                }
            return(simscore)
            })
        }

    # sort by score, random order when scores are equal
    # select x best/worst matches from items (if there is room for selection)
    if (df_row[1, "match_nonpolsim"] == 1) {
        # best -> decreasing
        df_item_set <- df_item_set[
            order(
                -df_item_set$simscore,
                sample(nrow(df_item_set), nrow(df_item_set)),
                na.last = TRUE
                )
            ,
            ]
    } else {
        # worst -> increasing
        df_item_set <- df_item_set[
            order(
                df_item_set$simscore, 
                sample(nrow(df_item_set), nrow(df_item_set)),
                na.last = TRUE
                )
            ,
            ]
        }
    if (nrow(df_item_set) >= printsubset$max) df_item_set <- df_item_set[1:printsubset$max,]

    # simscore sum of printed items (might differ from total simscore)
    simscore <- colSums(df_item_set["simscore"], na.rm = TRUE)

    # subselection of item_set to be printed (expand groups if necessary)
    if (length(printsubset$subgroups) > 1) {
        item_set <- intersect(item_set, unlist(printsubset$subgroups[unlist(df_item_set["item"])]))
    } else {
        item_set <- intersect(item_set, unlist(df_item_set["item"]))
        }

    # return
    item_selection <- list(items = item_set, simscore = simscore)
    return(item_selection)
    }

### RUN ########################################################################
'
You need to provide an export_indicator=dummy indicating if a profile should be
exported for the match or not (in this case: match_profile_export).
'

# import matched data
df <- read_feather(paste0(wd, "/data/post_match.feather"))

# export
profiles <- exporter(
    df = df,
    matchparams = matchparams,
    export_indicator = "match_profile_export",
    path_dir = paste0(wd,"/profiles/"), # where to store the profiles
    valuesNA = c("-99", "-66", ".", "", "NA", NA) # print string values no to export
    )

# merge back to df
df <- merge(df, profiles, by = "lfdn", all = TRUE)

# save (might be HUGE if match no. is not restricted!)
write_feather(df, paste0(wd, "/data/post_export.feather"))