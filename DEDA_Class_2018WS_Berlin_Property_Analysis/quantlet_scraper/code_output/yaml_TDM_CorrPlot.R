# Clear all variables
rm(list = ls(all = TRUE))
graphics.off()

# Install and load packages
libraries = c("tm")
lapply(libraries, function(x) if (!(x %in% installed.packages())) {
  install.packages(x)
})
lapply(libraries, library, quietly = TRUE, character.only = TRUE)

## Rgraphviz must be installed for the first time,
## afterwards it is automatically loaded via tm library
# source("http://bioconductor.org/biocLite.R")
# biocLite("Rgraphviz")
# library(Rgraphviz)


(obj.names = load("yaml_DTM.RData", .GlobalEnv))

# summary of the DocumentTermMatrix containing the keywords from the YAML meta info in Quantlets
DTM

# Find frequent terms in a document-term or term-document matrix with the lower frequency bound 'lowfreq'
findFreqTerms(DTM, lowfreq = 70)

# http://artax.karlin.mff.cuni.cz/r-help/library/tm/html/plot.html
## Visualize correlations between terms of a term-document matrix. 
# The correlation plots below use the most frequent words (above the given frequency bound 'lowfreq') from the DTM as the nodes
# and include the links among words with a correlation above the given threshold 'corThreshold'

plot(DTM, terms = findFreqTerms(DTM, lowfreq = 100), corThreshold = 0.5)

plot(DTM, terms = findFreqTerms(DTM, lowfreq = 100), corThreshold = 0.2)

plot(DTM, terms = findFreqTerms(DTM, lowfreq = 100), corThreshold = 0.2, weighting = TRUE)

plot(DTM, terms = findFreqTerms(DTM, lowfreq = 70),  corThreshold = 0.1)

plot(DTM, terms = findFreqTerms(DTM, lowfreq = 70),  corThreshold = 0.1, weighting = TRUE)

plot(DTM, terms = findFreqTerms(DTM, lowfreq = 60),  corThreshold = 0.2, weighting = TRUE)

plot(DTM, terms = findFreqTerms(DTM, lowfreq = 50),  corThreshold = 0.2)

plot(DTM, terms = findFreqTerms(DTM, lowfreq = 50),  corThreshold = 0.1, weighting = TRUE)

plot(DTM, terms = findFreqTerms(DTM, lowfreq = 10),  corThreshold = 0.2)
