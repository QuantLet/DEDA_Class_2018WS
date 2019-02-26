# Clear all variables
rm(list = ls(all = TRUE))
graphics.off()

# Install and load packages
libraries = c("tm")
lapply(libraries, function(x) if (!(x %in% installed.packages())) {
  install.packages(x)
})
lapply(libraries, library, quietly = TRUE, character.only = TRUE)


(obj.names = load("yaml_DTM.RData", .GlobalEnv))

# summary of the DocumentTermMatrix containing the keywords from the YAML meta info in Quantlets
DTM


## Find associations in a document-term or term-document matrix.

# corlimit: a numeric vector (of the same length as terms; recycled otherwise) for the
# (inclusive) lower correlation limits of each term in the range from zero to one.

# The output is a named list. Each list component is named after a term in terms and contains a named numeric vector.
# Each vector holds matching terms from DTM and their rounded correlations satisfying the inclusive lower correlation limit of corlimit.

findAssocs(DTM, terms = "black-scholes", corlimit = 0.6)

findAssocs(DTM, "black-scholes", corlimit = 0.3)

findAssocs(DTM, "normal", corlimit = 0.3)

findAssocs(DTM, "normal-distribution", corlimit = 0.2)

findAssocs(DTM, "financial", corlimit = 0.3)

findAssocs(DTM, "text-mining", corlimit = 0.3)

findAssocs(DTM, "copula", corlimit = 0.3)


# Multiple associations: term vectors matched against DTM
findAssocs(DTM, c("text-mining", "data-mining", "black-scholes", "distribution"), corlimit = c(0.3, 0.4, 0.5, 0.2) )

findAssocs(DTM, c("time-series", "chart", "dax", "asset", "correlation"), corlimit = 0.2 )
