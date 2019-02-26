# ------------------------------------------------------
# Name of QuantLet: DMFchord
# ------------------------------------------------------ 
# Published in: Third Party Funding (Drittmittel)
# ------------------------------------------------------
# Description: Plots a chord-diagram of the number of co-authorships 
#              by researchers aggregated w.r.t. HU-Units
# ------------------------------------------------------
# Keywords: plot, chord, adjacency matrix, visualization
# ------------------------------------------------------
# See also: DMFsankey, DMFvarx, DMFtsmtpe
# ------------------------------------------------------
# Author: Alona Zharova
# ------------------------------------------------------
# Submitted: Marius Sterling, Alona Zharova 20180106
# ------------------------------------------------------
# Datafile: DMFchord.csv
# ------------------------------------------------------
# Input:  a (n x n)-matrix with the number of co-authorships,
#         additionally a matrix specifiing the colors
# ------------------------------------------------------
# Output:
# ------------------------------------------------------
# Example:
# ------------------------------------------------------


# Close windows and clear variables
graphics.off()
rm(list = ls(all = TRUE))

# installing the chorddiag-package from github and laoding it
devtools::install_github("mattflor/chorddiag")
library(chorddiag)

# loading data
tab = NULL
tab$data = as.matrix(read.csv("DMFchord.csv"))
tab$color = read.csv("DMFchordCol.csv")

# plot of all co-authorships
chorddiag(
  tab$data, 
  groupnamePadding = 40,
  groupnameFontsize = 25,
  showTicks = T,
  groupColors = tab$color$color
)

# plot without co-authorships within HU-Units
chorddiag(
  tab$data-diag(diag(tab$data)), 
  groupnamePadding = 40,
  groupnameFontsize = 25,
  groupColors = tab$color$color
)
