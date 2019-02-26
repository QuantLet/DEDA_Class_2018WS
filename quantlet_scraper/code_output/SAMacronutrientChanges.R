# Libraries
library(foreign)
library(dplyr)
library(tidyr)

# Functions
## Inputs Dataframe and returns same Dataframe with UPPERCASE variable names
upper = function(df) {
  names(df) = toupper(names(df))
  df
}

# Read in data
## read in 24 h recall examination files
files = list.files(pattern = "_24RC")
for (file in files) {
  td = as.data.frame(read.spss(paste0(file)), stringsAsFactors = F)
  td_name = substr(file, 0, 9)
  
  assign(td_name, td)
}

# Analysis
## t-tests to compare intake of total consumed kilocalories, fats, proteins,
## carbohydrates in 1998 and 2015 KNHANES nutrition survey datasets
### TOTAL ENERGY INTAKE
en98 = upper(HN98_24RC) %&gt;%
  group_by(ID) %&gt;%
  summarise(SUM_FAT = sum(NF_EN)) %&gt;%
  select(SUM_FAT) %&gt;%
  unlist(.)

en15 = upper(HN15_24RC) %&gt;%
  group_by(ID) %&gt;%
  summarise(SUM_FAT = sum(NF_EN)) %&gt;%
  select(SUM_FAT) %&gt;%
  unlist(.)

var.test(en98, en15)
t.test(en98, en15, var.equal = F)

### FAT INTAKE
fat98 = upper(HN98_24RC) %&gt;%
  group_by(ID) %&gt;%
  summarise(SUM_FAT = sum(NF_FAT*9)) %&gt;%
  select(SUM_FAT) %&gt;%
  unlist(.)

fat15 = upper(HN15_24RC) %&gt;%
  group_by(ID) %&gt;%
  summarise(SUM_FAT = sum(NF_FAT*9)) %&gt;%
  select(SUM_FAT) %&gt;%
  unlist(.)

var.test(fat98, fat15)
t.test(fat98, fat15, var.equal = F)

### PROTEIN INTAKE
PROT98 = upper(HN98_24RC) %&gt;%
  group_by(ID) %&gt;%
  summarise(SUM_PROT = sum(NF_PROT*4)) %&gt;%
  select(SUM_PROT) %&gt;%
  unlist(.)

PROT15 = upper(HN15_24RC) %&gt;%
  group_by(ID) %&gt;%
  summarise(SUM_PROT = sum(NF_PROT*4)) %&gt;%
  select(SUM_PROT) %&gt;%
  unlist(.)

var.test(PROT98, PROT15)
t.test(PROT98, PROT15, var.equal = F)


### CARBOHYDRATE INTAKE
CHO98 = upper(HN98_24RC) %&gt;%
  group_by(ID) %&gt;%
  summarise(SUM_CHO = sum(NF_CHO*4)) %&gt;%
  select(SUM_CHO) %&gt;%
  unlist(.)

CHO15 = upper(HN15_24RC) %&gt;%
  group_by(ID) %&gt;%
  summarise(SUM_CHO = sum(NF_CHO*4)) %&gt;%
  select(SUM_CHO) %&gt;%
  unlist(.)

var.test(CHO98, CHO15)
# p value = 0.2313 &gt; 0.01 -&gt; null hyp. not rejected: variances equal
t.test(CHO98, CHO15, var.equal = T)
# -&gt; p value = 0.00139 &lt; 0.01 -&gt; null hyp rejected: means not equal
