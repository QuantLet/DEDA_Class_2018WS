# Libraries
library(foreign)
library(survey)
library(dplyr)
library(tidyr)

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
## Application of survey weights
### 1998 
#### Compute BMI, Education Categories, Agegroups
HN98_ALL$BMI         = HN98_ALL$HE_WT/(HN98_ALL$HE_HT/100)^2
HN98_ALL$EDUC_GROUP  = ifelse(HN98_ALL$educ %in% c(4:7), "&gt;=High school diploma", "
<high %="" diploma")="" hn98_all="HN98_ALL" school="">
 %
  mutate(AGEGROUP = ifelse((age &gt; 6 &amp; age &lt; 13), "7-12 y", 
                    ifelse((age &gt; 12 &amp; age &lt; 19), "13-18 y",
                    ifelse((age &gt; 18 &amp; age &lt; 40), "19-39 y",
                    ifelse((age &gt; 39 &amp; age &lt; 60), "40-59 y", 
                    ifelse(age &gt; 59, "60+ y",
                    ifelse((age &lt; 7 &amp; age &gt; 1), "2-6 y", 
                    "&lt;2y")))))))

#### initialize survey
svy98 = svydesign(ids = ~kstrata, data = HN98_ALL, weights = ~wt_itv)

#### Age
svymean(~age, design = svy98)

#### Sex
prop.table(svytable(~sex, design = svy98))

#### Agegroup
prop.table(svytable(~AGEGROUP, design = svy98))

#### bmi
svymean(~BMI, design = svy98, na.rm = T)

#### bmi by agegroup
svyttest(BMI ~ AGEGROUP, svy98)

#### bmi by sex
svymean(~BMI, design = subset(svy98, sex == "1"), na.rm = T)
svymean(~BMI, design = subset(svy98, sex == "2"), na.rm = T)
svyttest(BMI ~ sex, svy98)

#### bmi by EDUCGROUP
prop.table(svytable(~EDUC_GROUP, design = svy98))
svymean(~BMI, design = subset(svy98, EDUC_GROUP == "&gt;=High school diploma"), na.rm = T)
svymean(~BMI, design = subset(svy98, EDUC_GROUP == "
 <high "="" ###="" ####="" %in%="" 2015="" agegroups="" bmi,="" c(4:7),="" categories,="" compute="" diploma"),="" educ_group,="" education="" hn15_all$bmi="HN15_ALL$HE_wt/(HN15_ALL$HE_ht/100)^2" hn15_all$educ_group="ifelse(HN15_ALL$educ" na.rm="T)" school="" svy98)="" svyttest(bmi="" ~="">
  =High school diploma", "
  <high %="" diploma")="" hn15_all="HN15_ALL" school="">
   %
  mutate(AGEGROUP = ifelse((age &gt; 6 &amp; age &lt; 13), "7-12 y", 
                    ifelse((age &gt; 12 &amp; age &lt; 19), "13-18 y",
                    ifelse((age &gt; 18 &amp; age &lt; 40), "19-39 y",
                    ifelse((age &gt; 39 &amp; age &lt; 60), "40-59 y", 
                    ifelse(age &gt; 59, "60+ y",
                    ifelse((age &lt; 7 &amp; age &gt; 1), "2-6 y", 
                    "&lt;2y")))))))

#### initialize survey with survey weights
svy15 = svydesign(ids = ~kstrata, data = HN15_ALL, weights = ~wt_hs)

#### Age
svymean(~age, design = svy15)

#### Sex
prop.table(svytable(~sex, design = svy15))

#### Agegroup
prop.table(svytable(~AGEGROUP, design = svy15))

#### bmi
svymean(~BMI, design = svy15, na.rm = T)

#### educgroup
prop.table(svytable(~EDUC_GROUP, design = svy15))

#### bmi by agegroup
svyttest(BMI ~ AGEGROUP, svy15)

#### bmi by sex
svymean(~BMI, design = subset(svy15, sex == "1"), na.rm = T)
svymean(~BMI, design = subset(svy15, sex == "2"), na.rm = T)
svyttest(BMI ~ sex, svy15)

#### bmi by EDUCGROUP
svymean(~BMI, design = subset(svy15, EDUC_GROUP == "&gt;=High school diploma"), na.rm = T)
svymean(~BMI, design = subset(svy15, EDUC_GROUP == "&lt;High school diploma"), na.rm = T)
svyttest(BMI ~ EDUC_GROUP, svy15)
  </high>
 </high>
</high>