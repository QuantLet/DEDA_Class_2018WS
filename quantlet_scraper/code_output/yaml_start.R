# Clear all variables
rm(list = ls(all = TRUE))
graphics.off()


### STEP 0: install packages

## needed only for package installation or update

# The aim of 'devtools' is to make package development easier by providing R functions that simplify common tasks.
libraries = c("devtools")
lapply(libraries, function(x) if (!(x %in% installed.packages())) {
  install.packages(x)
})
lapply(libraries, library, quietly = TRUE, character.only = TRUE)


## The devtools package provides install_github() that enables installing packages from GitHub.

# This command installs the yamldebugger package directly from GitHub,
# namely: https://github.com/lborke/yamldebugger
devtools::install_github("lborke/yamldebugger")


### STEP 1: once the yamldebugger package was installed you can start from this step everytime you want to debug new Quantlets

## load the package every time you want to use 'yamldebugger'
library(yamldebugger)

# Check if a keyword is in the Quantnet list
"plot" %in% allKeywords

# Check if several keywords are in the Quantnet list
c("time-series", "stochastic integration", "chi-square-test") %in% allKeywords


# start help pages for the main 4 functions of the yamldebugger package

help(yaml.debugger.init)

help(yaml.debugger.get.qnames)

help(yaml.debugger.run)

help(yaml.debugger.summary)
