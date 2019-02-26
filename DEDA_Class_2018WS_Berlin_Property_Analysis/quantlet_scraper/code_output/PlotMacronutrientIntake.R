# Libraries
library(foreign)
library(ggplot2)
library(dplyr)
library(scales)

# Functions
## Inputs KNHANES Nutrient Survey Dataset and returns summarizes Protein, Fat,
## Carbohydrate consumption as well as the total calorie intake.
baseNutrientsSummary = function(df, groupby = NULL) {
  if (is.null(groupby)) {
    out = upper(df) %&gt;%
      summarise(NF_EN = mean(NF_EN, na.rm = T),
                NF_PROT = mean(NF_PROT, na.rm = T),
                NF_FAT = mean(NF_FAT, na.rm = T),
                NF_CHO = mean(NF_CHO, na.rm = T))
  } else {
    out = upper(df) %&gt;%
      group_by_(groupby) %&gt;%
      summarise(NF_EN = mean(NF_EN, na.rm = T),
                NF_PROT = mean(NF_PROT, na.rm = T),
                NF_FAT = mean(NF_FAT, na.rm = T),
                NF_CHO = mean(NF_CHO, na.rm = T))
  }
  return(out)
}

## Inputs Dataset and returns same Dataset with uppercase variable names.
upper = function(df) {
  names(df) = toupper(names(df))
  df
}

## Inputs Dataset and returns same Dataset with lowercase variable names.
lower = function (df) {
  names(df) = tolower(names(df))
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

# Summarizes each years dataset by study participant for further analysis
for (file in files) {
  td_name = substr(file, 0, 9)
  df = get(td_name)
  
  df = lower(df)
  
  df2 = df %&gt;%
    group_by(id) %&gt;%
    summarise(NF_EN = sum(nf_en, na.rm = T), 
              NF_PROT = sum(nf_prot, na.rm = T),
              NF_FAT = sum(nf_fat, na.rm = T),
              NF_CHO = sum(nf_cho, na.rm = T), 
              region = unique(region), 
              town_t = unique(town_t), 
              psu = unique(psu), 
              sex = unique(sex), 
              age = unique(age),
              incm = unique(incm)) %&gt;%
    mutate(AGEGROUP = ifelse(age&lt;18, "u18", 
                             ifelse(age&lt;25, "u25", 
                                    ifelse(age&lt;65, "u65", "65+"))))
  
  td_name2 = substr(td_name, 3, 4)
  if (td_name2 == "98") {
    td_name2 = paste0("HN_19", td_name2)
  } else {
    td_name2 = paste0("HN_20", td_name2)
  }
  assign(td_name2, df2)
}

# Analysis
## Reads every KNHANES Nutrition Survey dataset from 1998 - 2015 and
## applies baseNutrientsSummary function (see functions section above)
ordervec = as.character(c(1998, 2001, 2005, 2007, 2008, 2009, 2010:2015))
df_macros = data.frame()
for (vec in ordervec) {
  print(vec)
  df = get(paste0("HN_", vec))
  sampsize = length(unique(df$id))
  print(sampsize)
  
  df2 = baseNutrientsSummary(df = df)
  df2$YEAR = vec
  df_macros = rbind(df_macros, df2)
}

## Some data transformation:
## YEAR variable is converted into date object
## PROT, FAT, CHO intake in kcal is calculated
df_m2 = df_macros %&gt;%
  group_by(YEAR)  %&gt;%
  summarise(NF_EN = mean(NF_EN, na.rm = T),
            NF_PROT = mean(NF_PROT, na.rm = T)*4,
            NF_FAT = mean(NF_FAT, na.rm = T)*9,
            NF_CHO = mean(NF_CHO, na.rm = T)*4) %&gt;%
  mutate(YEAR = as.Date(YEAR, format = "%Y")) %&gt;%
  gather(MACRONUTRIENT, GRAMS_DAILY, NF_PROT:NF_CHO)



## Plots Area Chart from 1998 - 2015 in kcal by Macronutrients
df_m2 %&gt;%
  mutate(MACRONUTRIENT = ifelse(MACRONUTRIENT == "NF_PROT", "Proteins",
                         ifelse(MACRONUTRIENT == "NF_FAT", "Fats",
                         ifelse(MACRONUTRIENT == "NF_CHO", "Carbohydrates", "")))) %&gt;%
  ggplot(., aes(x = YEAR, 
                y = GRAMS_DAILY, 
                group = MACRONUTRIENT, 
                fill = MACRONUTRIENT)) +
  geom_area() +
  scale_x_date(breaks = c(as.Date(c("1998-01-05", "2001-01-05", "2005-01-05")), 
                          seq(as.Date("2007-01-05"), as.Date("2015-01-05"), 
                              by = "1 years")),
               labels = date_format("%y")) +
  labs(title = "Daily Energy Intake by Macronutrients from 1998 until 2015",
       x = "Year",
       y = "Energy Intake in kcal") +
  theme_bw()
