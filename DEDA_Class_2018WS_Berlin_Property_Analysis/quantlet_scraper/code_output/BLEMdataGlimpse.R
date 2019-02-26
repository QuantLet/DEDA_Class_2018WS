## Save data glimpse of energy smart meter recordings
## Author: Michael Kostmann


# Load packages
packages = c("data.table",
             "lubridate",
             "tidyverse",
             "tibbletime")
invisible(lapply(packages, library, character.only = TRUE))

# Function for easy string pasting
"%&amp;%" = function(x, y) {paste(x, y, sep = "")}


# Specify datasets to load
dataset_ids    = c("consumer/consumer-00000056",
                   "prosumer/producer-00000089")

# Loop over datasets specified in datasets_ids
for(i in dataset_ids) {
    
    # Load raw data from csv-file
    raw_data      = fread(input     = "../data/"%&amp;%i%&amp;%".csv",
                          header    = T,
                          sep       = ',',
                          integer64 = "numeric")
    
    raw_data$time = as_datetime(raw_data$time/1000, tz = "CET")
    
    # Keep only timestamp, energy, and energyOut
    data = raw_data %&gt;%
        select(time,
               energy,
               energyOut) %&gt;%
        as_tbl_time(index = time)
    
    # Save glimpse as csv-file: Specify time interval
    from = "2017-09-20 12:18:00"
    to   = "2017-09-20 12:33:00"
    
    data %&gt;%
        filter_time(from ~ to) %&gt;%
        write.csv(substring(i, 10, 26)%&amp;%"_glimpse.csv")
    
}


## end of file##
