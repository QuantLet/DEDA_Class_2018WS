# This script creates heatmaps of the missing values and the cases and 
# variables that are removed in all three data sets.

library(foreign)

# Replace values where respondent didn't give an informative answer
SubNA1 &lt;- function(x){ 
  # Args:
  # x: variable
  
  x &lt;- gsub("8", NA, x)
  x &lt;- gsub("9", NA, x)
  x
}

# Replace values where respondent didn't give an informative answer
SubNA2 &lt;- function(x){
  # Args:
  # x: variable
  
  x &lt;- gsub("7", NA, x)
  x &lt;- gsub("8", NA, x)
  x &lt;- gsub("9", NA, x)
  x
}

year08 &lt;- read.spss("knightfoundation2008sotcdata.por",
                    use.value.labels = FALSE,
                    to.data.frame = TRUE,
                    use.missings = TRUE)
year09 &lt;- read.spss("knightfoundation2009sotcdata.por",
                    use.value.labels = FALSE,
                    to.data.frame = TRUE,
                    use.missings = TRUE)
year10 &lt;- read.spss("knightfoundation2010sotcdata.por",
                    use.value.labels = FALSE,
                    to.data.frame = TRUE,
                    use.missings = TRUE)

colnames(year08) &lt;- tolower(colnames(year08))
colnames(year09) &lt;- tolower(colnames(year09))
colnames(year10) &lt;- tolower(colnames(year10))

# Rename variables with the same name as in the 2008 data
colnames(year09)[which(colnames(year09) == "q4_1_1")] &lt;- "q4_1"
colnames(year09)[which(colnames(year09) == "ca")] &lt;- "cce"
colnames(year09)[which(colnames(year09) == "cagrp")] &lt;- "ccegrp"
colnames(year09)[which(colnames(year09) == "ca_attac")] &lt;- "cce_enga"

colnames(year10)[which(colnames(year10) == "cca")] &lt;- "cce"
colnames(year10)[which(colnames(year10) == "ccagrp")] &lt;- "ccegrp"
colnames(year10)[which(colnames(year10) == "cca_atta")] &lt;- "cce_enga"
colnames(year10)[which(colnames(year10) == "caseid")] &lt;- "case"

# These variables can have values 8 and/or 9 indicating a missing value
vars &lt;- c("qs4", "qs5", "qs5_2", "q5", "q6a", "q8a", "q8b",
          "q8c", "q8d", "q8e", "q8f", "q8g", "q9", "q10", "q10a",
          "q13", "q14", "q15", "q15aa", "q15ab", "q17", "q18", "q19",
          "q20", "q21", "q22a", "q22b", "q22c", "q22d", "q22e",
          "q22f", "q22g", "q22h", "q22i", "q22_a", "q23", "q24",
          "q24a", "q25", "q26", "qd4", "qd6", "qd7", "qd8", "qd10",
          "qd13", "qd13a", "qd13b", "qd13c") 

# These variables can have values 7, 8 and/or 9 indicating a missing value
vars2 &lt;- c("qce1", "qce2", "q3a", "q3b", "q3c", "q6", "q7a", "q7b",
           "q7c", "q7d", "q7e", "q7f", "q7g", "q7h", "q7i",
           "q7j", "q7k", "q7l", "q7m", "q7n", "q7o", "q7p",
           "q16a", "q16b", "q16c", "q16d", "q28", "q29", "q30",
           "q31a", "q31b", "q31c", "q31d", "q31e", "q31f", "q31g",
           "q31h") 

# Pick out variables with values indicating missingness
d1vars &lt;- vars[vars %in% colnames(year08)]
d1vars2 &lt;- vars2[vars2 %in% colnames(year08)]
d2vars &lt;- vars[vars %in% colnames(year09)]
d2vars2 &lt;- vars2[vars2 %in% colnames(year09)]
d3vars &lt;- vars[vars %in% colnames(year10)]
d3vars2 &lt;- vars2[vars2 %in% colnames(year10)]

# Substitute values indicating missing with a missing value
year08[, d1vars] &lt;- apply(year08[, d1vars], 2, SubNA1)
year08[, d1vars2] &lt;- apply(year08[, d1vars2], 2, SubNA2)
year09[, d2vars] &lt;- apply(year09[, d2vars], 2, SubNA1)
year09[, d2vars2] &lt;- apply(year09[, d2vars2], 2, SubNA2)
year10[, d3vars] &lt;- apply(year10[, d3vars], 2, SubNA1)
year10[, d3vars2] &lt;- apply(year10[, d3vars2], 2, SubNA2)

# Substitute values indicating missing with a missing value
year08[, "q4_1"] &lt;- gsub("3", NA, gsub("2", NA, year08[, "q4_1"]))
year08[, "q11"] &lt;- gsub("98", NA, gsub("99", NA, year08[, "q11"]))
year08[, "qd3"] &lt;- gsub("98", NA, gsub("99", NA, year08[, "qd3"]))
year08[, "qd9"] &lt;- gsub("98", NA, gsub("99", NA, year08[, "qd9"]))
year08[, "qd111"] &lt;- gsub("^2$", NA, gsub("^3$", NA, year08[, "qd111"]))
year08[, "qd3"]  &lt;- as.numeric(gsub(" ", NA, year08[, "qd3"]))

year09[, "q11"] &lt;- gsub("98", NA, gsub("99", NA, year09[, "q11"]))
year09[, "qd9"] &lt;- gsub("98", NA, gsub("99", NA, year09[, "qd9"]))
year09[, "qd111"] &lt;- gsub("^2$", NA, gsub("^3$", NA, year09[, "qd111"]))

year10[, "q4_1"] &lt;- gsub("3", NA, gsub("2", NA, year10[, "q4_1"]))
year10[, "q11"] &lt;- gsub("98", NA, gsub("99", NA, year10[, "q11"]))
year10[, "qd9"] &lt;- gsub("98", NA, gsub("99", NA, year10[, "qd9"]))
year10[, "qd111"] &lt;- gsub("^2$", NA, gsub("^3$", NA, year10[, "qd111"]))

# Pick out common variable in all three data sets
common0809 &lt;- colnames(year08)[colnames(year08) %in% colnames(year09)]
commonAll &lt;- common0809[common0809 %in% colnames(year10)]

# Pick out variable not common in all three data sets
leftOver08 &lt;- colnames(year08)[!colnames(year08) %in% commonAll]
leftOver09 &lt;- colnames(year09)[!colnames(year09) %in% commonAll]
leftOver10 &lt;- colnames(year10)[!colnames(year10) %in% commonAll]

# Rearrange data so that common variables come first 
year08 &lt;- year08[, c(commonAll, leftOver08)]
year09 &lt;- year09[, c(commonAll, leftOver09)]
year10 &lt;- year10[, c(commonAll, leftOver10)]

# List variables not useful in our analyses
takeOutVars &lt;- c(# redundant variables
                 "qs3_02", "qs3_03", "qs3_04", "qs3_05", "qs3_06", 
                 "qs3_07", "qs3_08", "qs3_09", "qs3_10", "qs3_11", 
                 "qs3_12", "qs3_13", "qs3_14", "qs3_15", "qs3_16", 
                 "qs3_17", "qs3_18", "qs3_19", "qs3_20", "qs3_21", 
                 "qs3_22", "qs3_23", "qs3_24", "qs3_25", "qs3_26", 
                 "qs3", "qs5", "qs5_2",
                 # not useful variables
                 "case", "caseid", "intro1",                                
                 # Get rid of variables that make up our dependent variable
                 "loyalty", "loyalty_", "passion", "passion_",
                 "qce1", "qce2", "q6a", "q3a", "q3b", 
                 # Don't need community attachment variables that's not the 3 
                 # level variable
                 "cce_enga", "cce",
                 # remove index variables since we're using it's derived vars
                 "basic_se", "leadersh", "educatio", "safety", 
                 "aestheti", "economy", "social_o", "communit", 
                 "involvem", "openness", "social_c", "domains",
                 # Remove 3-level variables and keep the 5-level ones
                 "qce1r", "qce2r", "q3ar", "q3br", "q3cr", "q5r", 
                 "q6r", "q6ar", "q7ar", "q7br", "q7cr", "q7dr", 
                 "q7er", "q7fr", "q7gr", "q7hr", "q7ir", "q7jr", 
                 "q7kr", "q7lr", "q7mr", "q8ar", "q8br", "q8cr", 
                 "q8dr", "q8er", "q8fr", "q9r", "q10r", "q13r", 
                 "q14r", "q15r", "q15aar", "q15abr", "q16ar", 
                 "q16br", "q16cr", "q16dr", "q17r", "q18r", "q19r", 
                 "q22ar", "q22br", "q22cr", "q22dr", "q23r", "q24r", 
                 "q25r", "q26r",
                 # Completely missing in '09 and '10 data
                 "q7j", "q12", "qd3", 
                 # All missing values in year'10
                 "q16a", "q16b", "q16c", "q16d", "q17", 
                 # Remove variables from '09 data not found in other data sets
                 "qd111r", "qd112r", "qd113r", "q28r", "q29r", "q4_2_1",
                 "q30r", "lei_inde", "thriving", "struggli", "sufferin", 
                 "emotiona", "q1a", "q1b", "q4_1_1", "qd2a", "q4_1_2", "q4_3_1", 
                 "q4_1_3", "q4_2_2", "q4_2_3", "q4_3_2", "q4_3_3",
                 # Get rid of '10 vars not found in '08 and '09 data
                 "qn1a", "qn1b", "q7n", "q7o", "q7p", "q8g", "q10a", "qd13a", 
                 "q22e", "q22f", "q22g", "q22h", "q22i", "q22_a", 
                 "q24a", "q27", "q28", "q29", "q30", "q31a", "q31b", 
                 "q31c", "q31d", "q31e", "q31f", "q31g", "q31h", 
                 "qd13b", "qd13c", "q15vera", "q15verb", "q15veras", 
                 "q15verbs", "totaln", "lei", "race_gro", "q1r", "q2r", 
                 "q7nr", "q7or", "q7pr", "q8gr", "q10ar", "q22er", "q22fr", 
                 "q22gr", "q22hr", "q22ir", "q22_ar", "q24ar", "gender", 
                 # Too many missing values in all 3 data sets
                 "qs3a", "q4_1", "qd5a", "qd5b", "qd5c", "q13", "q15", "q14",
                 "q4_2", "q4_3", "qd112", "qd113", "qd12",
                 # Same as qd3 in '08, '09, '10  data sets
                 "qd3a")

# Variables removed are represented as a 3 and variables not removed are 
# labeled with a 2
varsRemoved1 &lt;- as.numeric(colnames(year08) %in% takeOutVars) + 2 
varsRemoved2 &lt;- as.numeric(colnames(year09) %in% takeOutVars) + 2
varsRemoved3 &lt;- as.numeric(colnames(year10) %in% takeOutVars) + 2

# Replicate the vector representing variables removed 500 times so that this 
# shows up on the plot
varsRemoved1 &lt;- matrix(rep(varsRemoved1, 500), byrow = TRUE, nrow = 500)
varsRemoved2 &lt;- matrix(rep(varsRemoved2, 500), byrow = TRUE, nrow = 500)
varsRemoved3 &lt;- matrix(rep(varsRemoved3, 500), byrow = TRUE, nrow = 500)

# Subset data with variables common in all three data sets
commonVars08 &lt;- year08[, !colnames(year08) %in% takeOutVars]
commonVars09 &lt;- year09[, !colnames(year09) %in% takeOutVars]
commonVars10 &lt;- year10[, !colnames(year10) %in% takeOutVars]

# Create vector representing case removed: 2 - not removed 3 - removed
caseRemoved1 &lt;- c(2, 3)[as.numeric(apply(is.na(commonVars08), 1, sum) &gt; 1) + 1]
caseRemoved2 &lt;- c(2, 3)[as.numeric(apply(is.na(commonVars09), 1, sum) &gt; 1) + 1]
caseRemoved3 &lt;- c(2, 3)[as.numeric(apply(is.na(commonVars10), 1, sum) &gt; 1) + 1]

# Create missing values at last 500 rows where the vectors representing 
# variables removed are
caseRemoved1 &lt;- c(caseRemoved1, rep(NA, 500))
caseRemoved2 &lt;- c(caseRemoved2, rep(NA, 500))
caseRemoved3 &lt;- c(caseRemoved3, rep(NA, 500))

# Replicate the vector representing cases removed 10 times so that this 
# shows up on the plot
caseRemoved1 &lt;- matrix(rep(caseRemoved1, 10), nrow = length(caseRemoved1))
caseRemoved2 &lt;- matrix(rep(caseRemoved2, 10), nrow = length(caseRemoved2))
caseRemoved3 &lt;- matrix(rep(caseRemoved3, 10), nrow = length(caseRemoved3))

# Rearrange data so that the image being plotted looks like a data set with
# variables as columns and cases as rows
remove08 &lt;- t(cbind(rbind(apply(is.na(year08), 2, as.numeric), varsRemoved1), 
                    caseRemoved1))[, (nrow(year08) + 500):1]
remove09 &lt;- t(cbind(rbind(apply(is.na(year09), 2, as.numeric), varsRemoved2), 
                    caseRemoved2))[, (nrow(year09) + 500):1]
remove10 &lt;- t(cbind(rbind(apply(is.na(year10), 2, as.numeric), varsRemoved3), 
                    caseRemoved3))[, (nrow(year10) + 500):1]

colors &lt;- c("red", "#FFFDD0", "black", "yellow")

png("Missing2008.png",
    width = 800)
par(xpd = TRUE, mar = c(4.5, 4.5, 2.5, 12) + 0.1)
image(remove08, 
      axes = FALSE,
      main = "Year 2008",
      xlab = "Variables",
      ylab = "Cases",
      col = colors,
      cex.lab = 2,
      cex.main = 2.5)
mtext("1",
      side = 1,
      at = 0.001,
      cex = 1.4)
mtext("179",
      side = 1,
      at = 0.95,
      cex = 1.4)
mtext("13822",
      side = 2,
      at = 0.04,
      las = 1,
      cex = 1.4)
mtext("1",
      side = 2,
      at = 1,
      las = 1,
      cex = 1.4)
legend(1.05, 1,
       col = colors[c(2, 1, 4, 3)],
       pch = 15,
       legend = c("Missing", "Not Missing", "Removed", "Not Removed"),
       cex = 1.4)
dev.off()

png("Missing2009.png",
    width = 800)
par(xpd = TRUE, mar = c(4.5, 4.5, 2.5, 12) + 0.1)
image(remove09, 
      axes = FALSE,
      main = "Year 2009",
      xlab = "Variables",
      ylab = "Cases",
      col = colors,
      cex.lab = 2,
      cex.main = 2.5)
mtext("1",
      side = 1,
      at = 0.001,
      cex = 1.4)
mtext("195",
      side = 1,
      at = 0.95,
      cex = 1.4)
mtext("13728",
      side = 2,
      at = 0.04,
      las = 1,
      cex = 1.4)
mtext("1",
      side = 2,
      at = 1,
      las = 1,
      cex = 1.4)
legend(1.05, 1,
       col = colors[c(2, 1, 4, 3)],
       pch = 15,
       legend = c("Missing", "Not Missing", "Removed", "Not Removed"),
       cex = 1.4)
dev.off()

png("Missing2010.png",
    width = 800)
par(xpd = TRUE, mar = c(4.5, 4.5, 2.5, 12) + 0.1)
image(remove10, 
      axes = FALSE,
      main = "Year 2010",
      xlab = "Variables",
      ylab = "Cases",
      col = colors,
      cex.lab = 2,
      cex.main = 2.5)
mtext("1",
      side = 1,
      at = 0.001,
      cex = 1.4)
mtext("229",
      side = 1,
      at = 0.96,
      cex = 1.4)
mtext("20271",
      side = 2,
      at = 0.04,
      las = 1,
      cex = 1.4)
mtext("1",
      side = 2,
      at = 1,
      las = 1,
      cex = 1.4)
legend(1.05, 1,
       col = colors[c(2, 1, 4, 3)],
       pch = 15,
       legend = c("Missing", "Not Missing", "Removed", "Not Removed"),
       cex = 1.4)
dev.off()
