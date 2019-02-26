# Libraries
library(dplyr)
library(tidyr)
library(foreign)

# Functions
## Inputs Dataframe and returns same Dataframe with UPPERCASE variable names
upper = function(df) {
  names(df) = toupper(names(df))
  df
}

# Read in data
## read in health examination data
files_all = list.files(pattern = "_ALL")
for (file in files_all) {
  td = as.data.frame(read.spss(paste0(file), reencode = "UTF-8"))
  td_name = substr(file, 0, 8)
  
  assign(td_name, td)
  print(td_name)
}

# Analysis
## Computes BMI categorization for the Nutrition Survey KNHANES datasets from
## 1998 and 2015 using the following four categorizations:
## Underweight, Normal Range, Overweight, Obese
upper(HN98_ALL) %&gt;%
  group_by(ID) %&gt;%
  summarise(BMI = (HE_WT/(HE_HT/100)^2)) %&gt;%
  filter(!is.na(BMI)) %&gt;%
  mutate(BMI_CAT = ifelse(BMI &lt; 18.5, "Underweight", 
                          ifelse((BMI &gt;= 25 &amp; BMI &lt; 30), "Overweight", 
                                 ifelse(BMI &gt;= 30, "Obese", "Normal range")))) %&gt;%
  group_by(BMI_CAT) %&gt;%
  summarise(N = n()) %&gt;%
  mutate(RELFREQ = N/sum(N)) 

upper(HN15_ALL) %&gt;%
  group_by(ID) %&gt;%
  summarise(BMI = (HE_WT/(HE_HT/100)^2)) %&gt;%
  filter(!is.na(BMI)) %&gt;%
  mutate(BMI_CAT = ifelse(BMI &lt; 18.5, "Underweight", 
                          ifelse((BMI &gt;= 25 &amp; BMI &lt; 30), "Overweight", 
                                 ifelse(BMI &gt;= 30, "Obese", "Normal range")))) %&gt;%
  group_by(BMI_CAT) %&gt;%
  summarise(N = n()) %&gt;%
  mutate(RELFREQ = N/sum(N))


# t-test to compare changes in body mass index from 1998 -&gt; 2015
BMI98 = upper(HN98_ALL) %&gt;%
  group_by(ID) %&gt;%
  summarise(BMI = (HE_WT/(HE_HT/100)^2)) %&gt;%
  select(BMI) %&gt;%
  unlist(.)

BMI15 = upper(HN15_ALL) %&gt;%
  group_by(ID) %&gt;%
  summarise(BMI = (HE_WT/(HE_HT/100)^2)) %&gt;%
  select(BMI) %&gt;%
  unlist(.)

var.test(BMI98, BMI15) 
# p value &lt; 0.01 -&gt; reject null hypothesis: variance is not equal
t.test(BMI98, BMI15, var.equal = F)
# p value &lt; 0.01 -&gt; reject null hypothesis -&gt; means are signif. different
