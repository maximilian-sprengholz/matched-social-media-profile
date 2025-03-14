################################################################################
#
#   EXPORT SOCIAL MEDIA PROFILES
#
################################################################################

### PARAMETERS ###
set.seed(42)
options(scipen = 99)
source("src/02_matchparams.R", encoding = "UTF-8")


### FUNCTIONS ###

exporter <- function(
        df,
        idcol, # person ids, numeric
        matchparams, # dict containing all matching parameters
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

    # keep track in terminal
    nsample <- nrow(df)
    message(paste0("\nExporter started for n=", nsample, " Profiles."))

    with_progress({

        # track progress
        p <- progressor(steps = nsample)

        # for each person and match, export match profile and add file name/url to df;
        # fetch info via match_id (match_c_0116), use only print strings and id info
        df_match_export <- df %>%
            future_pmap_dfr(function(...) {
                # person for which we export a matched profile
                df_row <- tibble(...)
                # person matched
                match_id <- as.numeric(df_row[1, paste0("match_", idcol)])
                df_match_row <- df[df[idcol] == match_id, ]
                df_match_row <- df_match_row[1, ]
                # file path
                path_file <- paste0(path_dir, df_row[1, idcol], ".html")
                # export profile; returns printed items and their simscore sum
                profile <- export_values(
                    df_row,
                    df_match_row,
                    idcol,
                    path_file,
                    matchparams,
                    valuesNA)
                # create profile df
                df_profiles <- data.frame(
                    df_row[1, idcol],
                    match_id,
                    path_file,
                    profile$simscore
                    )
                colnames(df_profiles) <- c(
                    idcol,
                    paste0("match_", idcol),
                    "match_profile_url",
                    "match_profile_simscore"
                    )
                # add info which items have been printed
                for (group in unlist(matchparams$keys())) {
                    printsubset <- matchparams$get(group)$get("printsubset")
                    for (item in names(printsubset$map)) {
                            df_profiles[paste0("match_profile_", item)] <- ifelse(
                                item %in% profile$items,
                                1,
                                0)
                        }
                    }
                # update progress and return
                p()
                return(df_profiles)
                },
                .options = furrr_options(seed = TRUE)
                )
        })
    return(df_match_export)
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

    # gather profile items and scores
    profile <- list(items = c(), simscore = 0) # gather printed items and scores

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
    # add score
    if ("initials_score" %in% colnames(df_row)) {
        profile$simscore <- profile$simscore + df_row[1, "initials_score"]
        }
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
    # add score
    for (matchvar in c("gender", "age", "agegroup", "currentstate")) {
        if (paste0(matchvar, "_score") %in% colnames(df_row)) {
            profile$simscore <- profile$simscore + df_row[1, paste0(matchvar, "_score")]
            }
        }

    ### gather main content (exclude what is in header)

    # for keys { for keys not in header_set {PRINT}}
    content_main <- list() # gather group content in list, collapse later
    for (group in unlist(matchparams$keys())) {
        # get all print strings (e.g. demography1, 2, ...); check if not empty
        item_set <- sort(colnames(df_match_row)[
            grepl(paste0(group, "[0-9]+"), colnames(df_match_row))
            ])
        if (length(item_set) > 0) {
            # randomly select print subset; sum up corresponding simscore (returned)
            item_selection <- select_subset(group, matchparams, item_set, df_row)
            item_set <- item_selection$items
            profile$items <- c(profile$items, item_set)
            profile$simscore <- profile$simscore + item_selection$simscore
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

    # return printed items and their simscore sum
    return(profile)
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
    if (df_row[1, "match_group"] == "Different opinion, same characteristics"
            | df_row[1, "match_group"] == "Same opinion, same characteristics") {
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
    if (nrow(df_item_set) >= printsubset$max) df_item_set <- df_item_set[1:printsubset$max, ]

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

# delete old files
docs <- list.files(path = paste0(wd, "/profiles"), pattern = "*.html")
if (length(docs) > 0) {
    invisible(file.remove(paste0(wd, "/profiles/", docs)))
    print(paste0("Cleaning up before export: ", length(docs), " profiles removed."))
    }

# import matched data
df <- read_rds(paste0(wd, "/data/post_match.rds"))
df_exportable <- df %>% filter(match_profile_export == 1) # those to be exported

# set-up multisession
ncores <- detectCores()
plan(multisession, workers = ncores)

# export
tic()
profiles <- exporter(
    df = df_exportable,
    idcol = "c_0116",
    matchparams = matchparams,
    path_dir = paste0(wd, "/profiles/"), # where to store the profiles
    valuesNA = c("-99", "-66", ".", "", "NA", NA) # print string values not to export
    )
toc()

# merge back to df
df <- merge(df, profiles, by = c("c_0116", "match_c_0116"), all.x = TRUE, all.y = FALSE)

### export match correspondence table
df_matches <- df %>%
    filter(match_profile_export == 1) %>%
    select(c(c_0116, match_c_0116, match_profile_url)) %>%
    mutate(match_profile_url = gsub(wd, "", match_profile_url))
write.csv(df_matches, paste0(wd, "/data/match_id_correspondence.csv"), row.names = FALSE)

# save
write_rds(df, paste0(wd, "/data/post_export.rds"))
colnames(df) <- sapply(colnames(df), function(str) {
    strtrim(str, 32)
    })
write_dta(
  df %>% select(!any_of(c("datetime", "date_of_last_access"))) %>%
    replace(is.na(.), "."),
  paste0(wd, "/data/post_export.dta")
  )
