# This script (only needs to be ran once) goes through the data cleaning steps 
# and outputs our final data sets.

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

# Rename variables so they match variable names in the 2008 data set
colnames(year09)[which(colnames(year09) == "q4_1_1")] &lt;- "q4_1"
colnames(year09)[which(colnames(year09) == "ca")] &lt;- "cce"
colnames(year09)[which(colnames(year09) == "cagrp")] &lt;- "ccegrp"
colnames(year09)[which(colnames(year09) == "ca_attac")] &lt;- "cce_enga"

colnames(year10)[which(colnames(year10) == "cca")] &lt;- "cce"
colnames(year10)[which(colnames(year10) == "ccagrp")] &lt;- "ccegrp"
colnames(year10)[which(colnames(year10) == "cca_atta")] &lt;- "cce_enga"

# Choose the variables we're interested in analyzing
year08 &lt;- year08[, !colnames(year08) %in% takeOutVars]
year09 &lt;- year09[, !colnames(year09) %in% takeOutVars]
year10 &lt;- year10[, !colnames(year10) %in% takeOutVars]

# Order variables as in the 2008 data to make life easier
year09 &lt;- year09[, colnames(year08)]
year10 &lt;- year10[, colnames(year08)]

# 8 and 9 are considered as missing values
vars &lt;- c("qs4",   "q5",    "q8a",   "q8b",   "q8c",   "q8d",   "q8e",  
          "q8f",   "q9",    "q10",   "q15aa", "q15ab", "q18",   "q19",  
          "q20",   "q21",   "q22a",  "q22b",  "q22c",  "q22d",  "q23",  
          "q24",   "q25",   "q26",   "qd4",   "qd6",   "qd7",   "qd8",  
          "qd10",  "qd13") 
# 7, 8, and 9 are considered as missing values
vars2 &lt;- c("q3c", "q6",  "q7a", "q7b", "q7c", "q7d", "q7e", "q7f",
           "q7g", "q7h", "q7i", "q7k", "q7l", "q7m")

year08[, vars] &lt;- apply(year08[, vars], 2, SubNA1)
year08[, vars2] &lt;- apply(year08[, vars2], 2, SubNA2)
year09[, vars] &lt;- apply(year09[, vars], 2, SubNA1)
year09[, vars2] &lt;- apply(year09[, vars2], 2, SubNA2)
year10[, vars] &lt;- apply(year10[, vars], 2, SubNA1)
year10[, vars2] &lt;- apply(year10[, vars2], 2, SubNA2)

year08$q11 &lt;- gsub("98", NA, gsub("99", NA, year08$q11))
year08$qd9 &lt;- gsub("98", NA, gsub("99", NA, year08$qd9))
year08$qd111 &lt;- gsub("^2$", NA, gsub("^3$", NA, year08$qd111))

year09$qd111 &lt;- gsub("^2$", NA, gsub("^3$", NA, year09$qd111)) 

year10$q11 &lt;- gsub("98", NA, gsub("99", NA, year10$q11))
year10$qd9 &lt;- gsub("98", NA, gsub("99", NA, year10$qd9))
year10$qd111 &lt;- gsub("^2$", NA, gsub("^3$", NA, year10$qd111)) 

write.csv(year08, 
          "FinalData08.csv", 
          row.names = FALSE)
write.csv(year09, 
          "FinalData09.csv", 
          row.names = FALSE)
write.csv(year10, 
          "FinalData10.csv", 
          row.names = FALSE)
