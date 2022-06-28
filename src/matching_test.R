df <- mtcars[1:10,]
# print(df)
# set.seed(42)
# df[sample(nrow(df)),]
#

# compare person row to all other person rows
row_p1 <- df[1,]
df_p2 <- df[-1,]

# specify which variables to match excatly and which fuzzy
varset_exact <- c()
varset_fuzzy <- c()

# match
test <- df_p2 %>%
  pmap_dfr(function(...) {
    
    # defaults
    simlow <- 300
    simhigh <- 600
    simscore <- 0 # start value
    
    # all variables of the matching set
    row_p2 <- tibble(...)
    
    # match = compare rows and add match scores
    row_matched <- pmap_dfr(
      list(row_p1, row_p2, names(row_p2)),
      function(p1, p2, name) {
        # exact vs. fuzzy matching
        if (name %in% varset_fuzzy) {
          # how to do? implement rudimentary version
          match <- p1==p2
        } else {
          match <- p1==p2
        }
        # exact matching
        if (match == TRUE) {
            # score
            simscore <<- simscore + 1 #match_score(name)
            # return matched value
            p2
          } else {
            NA
          }
        }
      )
    
    # return only if similarity high or low
    if (simscore<simlow | simscore>simhigh) {
      # add similarity score
      row_matched$simscore <- simscore
      # add id of p1 to merge later
      #row_matched$lfdn <- row_p1$lfdn
      #row_matched$match_lfdn <- row_p2$lfdn
      row_matched
    }

  })

test

paste0(getwd(), '/')