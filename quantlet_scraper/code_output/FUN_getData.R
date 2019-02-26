## Function: getData
## Author: Michael Kostmann

## Args:     path   - path of the directory that contains the datasets of
##                    consumers or prosumers that should be read in
##           data   - Set to "all" to load all datasets in directory or to
##                    "single" to load only single dataset specified in "id".
##                    If "single" is set, argument "id" must be given.
##           id     - specifies consumer or prosumer dataset that should be
##                    read in if "single" is specified in "data". Must be of
##                    the form "consumer-00000001" or "producer-00000001".
##           return - which values to return in the data frame. Arguments are
##                    "consumption" (first-order difference of energy),
##                    "energy", "energyOut", and "production" (first-order
##                    difference of energyOut)
##           
## Returns:  df     - time tibble with the values specified in the return arg
##                    of one or all consumer(s)/prosumer(s) in one column



getData = function(path, data = "all", id = NULL, return = "consumption") {
    
    # Load packages
    packages = c("data.table", "tidyverse", "tidyquant", "tibbletime")
    invisible(lapply(packages, library, character.only = TRUE))
    
    # Function for easy string pasting
    "%&amp;%" = function(x, y) {paste(x, y, sep = "")}
    
    # Supress start of browser by error handler
    stop_nobrowser = function(x) {
        opt = options()
        options(error = NULL)
        on.exit(options(opt))
        stop(x, call. = FALSE)
    }
    
    # file.exists-function that is robust to macOS/windows OS differences
    robust.file.exists &lt;- function(x) { 
        if (.Platform$OS == "windows" &amp;&amp; grepl("[/\\]$", x)) { 
            file.exists(dirname(x)) 
        } else file.exists(x) 
    } 
    
    # Check wether "path" and "id" are correctly specified
    if(!robust.file.exists(path)) {
        stop_nobrowser("Path not specified correctly: "
                       %&amp;%path%&amp;%" does not exist.")}
    
    if(data == "single"){
        if(!robust.file.exists(path%&amp;%id%&amp;%".csv")) {
            stop_nobrowser("id not specified correctly: "
                           %&amp;%"File "%&amp;%path%&amp;%id%&amp;%".csv does not exist")
        }
    }
    
    
    
    # List all consumer/prosumer datasets in directory specified by "path" and
    # write into "files" or write dataset specified by "id" in to "files"
    files   = switch(data,
                      "all"    = list.files(path = path, pattern = "*.csv"),
                      "single" = id%&amp;%".csv"
                      )
    
    # Generate variable to name columns in data frame that is output
    id      = if(grepl("consumer", path)) {
                   gsub(".*?([0]{5})([0-9]{3}).*", "c\\2", files)
               } else {
                   if(grepl("prosumer", path)) {
                       gsub(".*?([0]{5})([0-9]{3}).*", "p\\2", files)
                   } else {
                       stop_nobrowser("Path must contain 'consumer' or 'prosumer'")
                   }
               }
    
    # Check wether path is correct
    if(!robust.file.exists(path%&amp;%files[1])) {stop_nobrowser("Path should end with '/'")}
    
    # Read in first dataset listed in "files"
    df      = fread(path%&amp;%files[1],
                     header    = T,
                     sep       = ',',
                     integer64 = "numeric")
    
    # Change unix millisecond timestamp into date format
    df$time = as_datetime(df$time/1000, tz = "CET")
    
    # Format data and keep only relevant variables specified by "return"
    df      = switch(return,
                     "consumption" = df %&gt;%
                         mutate(!!(id[1]%&amp;%"_cons") :=
                                    c(NA, diff(energy,
                                               lag = 1)*10^-10)) %&gt;%
                         select(time, !!(id[1]%&amp;%"_cons")) %&gt;%
                         slice(-1),
                     
                     "energy"      = df %&gt;%
                         mutate(!!(id[1]%&amp;%"_energy") :=
                                    energy*10^-10) %&gt;%
                         select(time, !!(id[1]%&amp;%"_energy")),
                     
                     "energyOut"   = df %&gt;%
                         mutate(!!(id[1]%&amp;%"_energyOut") :=
                                    energyOut*10^-10) %&gt;%
                         select(time, !!(id[1]%&amp;%"_energyOut")),
                     
                     "production"  = df %&gt;%
                         mutate(!!(id[1]%&amp;%"_prod") :=
                                    c(NA, diff(energyOut,
                                               lag = 1)*10^-10)) %&gt;%
                         select(time, !!(id[1]%&amp;%"_prod")) %&gt;%
                         slice(-1)
    ) %&gt;%
        as_tbl_time(time)
    
    
    # Read in all further datasets specified in files following the same
    # logic as above if "data" was set to "all"
    
    if(length(files) &gt; 1) {
        
        # Initialize progress bar
        pb = txtProgressBar(min = 0, max = length(files), style = 3)
        
        for(i in 2:length(files)){
            
            # Read in and format data
            x = fread(path%&amp;%files[i],
                       header    = T,
                       sep       = ',',
                       integer64 = "numeric")
            
            x$time = as_datetime(x$time/1000, tz = "CET")
            
            x = switch(return,
                        "consumption" = x %&gt;%
                            mutate(!!(id[i]%&amp;%"_cons") :=
                                       c(NA, diff(energy,
                                                  lag = 1)*10^-10)) %&gt;%
                            select(!!(id[i]%&amp;%"_cons")) %&gt;%
                            slice(-1),
                        
                        "energy"      = x %&gt;%
                            mutate(!!(id[i]%&amp;%"_energy") :=
                                       energy*10^-10) %&gt;%
                            select(!!(id[i]%&amp;%"_energy")),
                        
                        "energyOut"   = x %&gt;%
                            mutate(!!(id[i]%&amp;%"_energyOut") :=
                                       energyOut*10^-10) %&gt;%
                            select(!!(id[i]%&amp;%"_energyOut")),
                        
                        "production"  = x %&gt;%
                            mutate(!!(id[i]%&amp;%"_prod") :=
                                       c(NA, diff(energyOut,
                                                  lag = 1)*10^-10)) %&gt;%
                            select(!!(id[i]%&amp;%"_prod")) %&gt;%
                            slice(-1)
            )
            
            # Bind existing "df" with next read in dataset "x" if there is no
            # missing data in "x"
            
            switch(return,
                   "consumption" = nrow &lt;- 175200,
                   "energy"      = nrow &lt;- 175201,
                   "energyOut"   = nrow &lt;- 175201,
                   "production"  = nrow &lt;- 175200)
            
            if(nrow(x) == nrow) {
                df = list(df, x) %&gt;%
                      bind_cols() %&gt;%
                      as_tbl_time(index = time)
            
                setTxtProgressBar(pb, i)
                
            } else {
                warning(paste(files[i]%&amp;%" has missing data and was not added",
                        "to returned tibble time frame", sep = " "), call. = FALSE)
            }
            }
        close(pb)   
    }
    
    return(df)
}
