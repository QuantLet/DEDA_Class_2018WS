# This script runs PCA to find how the index variables are derived.

data &lt;- read.spss("knightfoundation2008sotcdata.por",
                 use.value.labels = FALSE,
                 to.data.frame = TRUE,
                 use.missings = FALSE)

colnames(data) &lt;- tolower(colnames(data))

vars &lt;- c("q3ar",     "q3br",     "q3cr",     "q5r",      "q6r",     
          "q6ar",     "q7ar",     "q7br",     "q7cr",     "q7dr",    
          "q7er",     "q7fr",     "q7gr",     "q7hr",     "q7ir",    
          "q7kr",     "q7lr",     "q7mr",     "q8ar",     "q8br",    
          "q8cr",     "q8dr",     "q8er",     "q8fr",     "q9r",     
          "q10r",     "q13r",     "q14r",     "q15r",     "q15aar",  
          "q15abr",   "q18r",     "q19r",     "q22ar",    "q22br",   
          "q22cr",    "q22dr",    "q23r",     "q24r",     "q25r",    
          "q26r",     "qce1r",    "qce2r",    "q7jr",     "q16ar",   
          "q16br",    "q16cr",    "q16dr",    "q17r",     "weight",  
          "projwt",   "urban_gr", "city",     "loyalty",  "passion", 
          "basic_se", "leadersh", "educatio", "safety",   "aestheti",
          "economy",  "social_o", "communit", "involvem", "openness",
          "social_c", "domains",  "cce")

pcdata &lt;- data[, vars]

pcdata &lt;- na.omit(pcdata)
pc &lt;- prcomp(pcdata, scale = TRUE, retx = TRUE)

# Look at the last 12 eigenvalues that are zero. There must be variables 
# that are linearly dependent of other variables.
plot(pc$sd^2, type = "b", ylim = c(0, max(pc$sd^2)), main = "Scree Plot", 
     xlab = "Principal Components", ylab = "Eigenvalues", cex.lab = 1.3, 
     cex.main = 2, cex.axis = 1.2)

col &lt;- ncol(pc$rotation)

# Correlated variables are at the end. Look for values large in magnitude and
# rearrange the values that give you a sum (or average) of a larger value. There
# may be more than one relationship.
round(pc$rotation[, (col - 11):col], 2)

# Verify the relationship by looking at correlations
cor(data$social_c, # Social Capital
    data$q23r + data$q24r + data$q25r + data$q26r, 
    use = "pairwise.complete.obs")
cor(data$safety, # Safety
    data$q18r + data$q19r, 
    use = "pairwise.complete.obs")
cor(data$aestheti, # Aesthetics
    data$q7ar + data$q7br, 
    use = "pairwise.complete.obs")
cor(data$educatio, # Education
    data$q7fr + data$q7gr, 
    use = "pairwise.complete.obs")
cor(data$involvem, # Civic Involvement
    data$q22ar + data$q22br + data$q22cr + data$q22dr, 
    use = "pairwise.complete.obs")
cor(data$leadersh, # Leadership
    data$q7lr + data$q15abr, 
    use = "pairwise.complete.obs")
cor(data$social_o, # Social Offerings
    data$q7hr + data$q7ir + data$q7mr, 
    use = "pairwise.complete.obs") 
cor(data$basic_se, # Basic Services
    data$q7cr + data$q7dr + data$q7kr, 
    use = "pairwise.complete.obs") 
cor(data$openness, # Openess
    data$q8ar + data$q8br + data$q8cr + data$q8dr + data$q8er + data$q8fr, 
    use = "pairwise.complete.obs") 
cor(data$economy, #Economy
    data$q7er + data$q9r + data$q10r + data$q14r + data$q15r + data$q15aar, 
    use = "pairwise.complete.obs") 
cor(data$communit, # Community Offerings
    data$basic_se + data$aestheti + data$safety + data$economy + data$social_o + 
    data$educatio + data$leadersh, 
    use = "pairwise.complete.obs")
cor(data$domains, # Community Domains
    data$communit + data$involvem + data$openness + data$social_c, 
    use = "pairwise.complete.obs") 

### Doesn't use variables that end in r
cor(data$passion, # Community Passion
    data$q3a + data$q3b, 
    use = "pairwise.complete.obs")
cor(data$loyalty, # Community Loyalty
    data$qce1 + data$qce2 + data$q6a, 
    use = "pairwise.complete.obs")
cor(data$cce, # Community Attachment
    data$loyalty + data$passion,
    use = "pairwise.complete.obs")
