# Libraries
library(foreign)
library(dplyr)
library(tidyr)

# Functions
## Inputs KNHANES nutrition survey dataset and seperates into existing
## food groups + seperates Alcohol, Kimchi, White Rice, Coffee, Bread into
## new food groups
FoodGroupAdd = function(df = NULL, year = NULL, agefilter = NULL) {
  df_fg = upper(df) %&gt;%
    mutate(FOOD_GROUP = as.numeric(substr(N_FCODE, 1, 2))) 
  
  if(!is.null(agefilter)) {
    df_fg = df_fg %&gt;%
      filter(AGE &gt; agefilter)
  }
  
  df_fg_wname = merge(df_fg, foodgroup_db, 
                       by.x = "FOOD_GROUP", 
                       by.y = "ID") %&gt;%
    mutate(NAME = as.character(NAME))
  
  
  if (year == 98) {
    alc = as.factor(c(15031:15062))
    kimchi = paste0("0", c(6045:6057))
    whiterice = paste0("0", c(1157:1167))
    coffee = as.character(c(15012:15018))
    bread = paste0("0", c(1049:1071))
    df_fg_wname$NAME[trim(df_fg_wname$N_FCODE) %in% alc] = "Alcohol"
    df_fg_wname$NAME[trim(df_fg_wname$N_FCODE) %in% kimchi] = "Kimchi"
    df_fg_wname$NAME[trim(df_fg_wname$N_FCODE) %in% whiterice] = "White Rice"
    df_fg_wname$NAME[trim(df_fg_wname$N_FCODE) %in% coffee] = "Coffee"
    df_fg_wname$NAME[trim(df_fg_wname$N_FCODE) %in% bread] = "Bread"
  } else if (year == 14) {
    alc = as.character(c(15026:15060))
    kimchi = paste0("0", c(6057:6070))
    whiterice = paste0("0", c(1173:1182))
    coffee = as.character(c(15083:15088))
    bread = paste0("0", c(1053:1076))
    df_fg_wname$NAME[trim(df_fg_wname$N_FCODE) %in% alc] = "Alcohol"
    df_fg_wname$NAME[trim(df_fg_wname$N_FCODE) %in% kimchi] = "Kimchi"
    df_fg_wname$NAME[trim(df_fg_wname$N_FCODE) %in% whiterice] = "White Rice"
    df_fg_wname$NAME[trim(df_fg_wname$N_FCODE) %in% coffee] = "Coffee"
    df_fg_wname$NAME[trim(df_fg_wname$N_FCODE) %in% bread] = "Bread"
  }
  return(df_fg_wname)
}

## Summarizes the kcal intake by foodgroups for each individual first and 
## for the entire population second.
FoodGroupRank = function(df = NULL, year = NULL) {
  df1 = FoodGroupAdd(df, year) %&gt;%
    group_by(ID, FOOD_GROUP, NAME) %&gt;%
    summarise(DAILY_INTAKE_KCAL = sum(NF_EN))
  
  dfreturn = df1 %&gt;%
    group_by(FOOD_GROUP, NAME) %&gt;%
    summarise(DailYIntGram = sum(DAILY_INTAKE_KCAL)/length(unique(df1$ID))) %&gt;%
    arrange(desc(DailYIntGram))
  
  return(dfreturn)
}

## Inputs Dataframe and returns same Dataframe with UPPERCASE variable names
upper = function(df) {
  names(df) = toupper(names(df))
  df
}

## trim function removes unnecessary blank spaces
trim = function(x) gsub("^\\s+|\\s+$", "", x)

# Read in data
## read in 24 h recall examination files
files = list.files(pattern = "_24RC")
for (file in files) {
  td = as.data.frame(read.spss(paste0(file)), stringsAsFactors = F)
  td_name = substr(file, 0, 9)
  
  assign(td_name, td)
}

# Analysis
## Creates lookup table for foodgroups
foodgroup_db = data.frame(
  ID = 1:18,
  NAME = c("Grains", "Potatoes and Starch", "Sugars", "Legumes", 
           "Seeds and Nuts", "Other Vegetables", "Mushrooms", 
           "Fruits", "Meats", "Eggs", "Fish", "Seeweads", 
           "Milk and Dairy Products", "Fat and Oils", 
           "Beverages", "Seasonings", "Processed Foods", "Others"))

## Applies FoodGroupRank function on KNHANES Nutrition Survey datasets from 
## 1998 and 2015
foodgroup98 = FoodGroupRank(df = HN98_24RC, year = 98)
foodgroup15 = FoodGroupRank(df = HN15_24RC, year = 14)
