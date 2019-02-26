# This script create a map of the locations of the communities and then
# creates a dotchart of the samples size of all the communities before and
# after removing cases with missing values.

library(ggmap)
library(maps)

data1 &lt;- read.csv("FinalData08.csv")
data2 &lt;- read.csv("FinalData09.csv")
data3 &lt;- read.csv("FinalData10.csv")

# Use these data sets later to created a dotchart of the changes in sample 
# sizes before and after data cleaning.

cleanData1 &lt;- na.omit(data1)
cleanData2 &lt;- na.omit(data2)
cleanData3 &lt;- na.omit(data3)

cities &lt;- c("Aberdeen, SD",      "Akron, OH",         "Biloxi, MS",       
            "Boulder, CO",       "Bradenton, FL",     "Charlotte, NC",    
            "Columbia, SC",      "Columbus, GA",      "Detroit, MI",      
            "Duluth, MN",        "Fort Wayne, IN",    "Gary, IN",         
            "Grand Forks, ND",   "Lexington, KY",     "Long Beach, CA",   
            "Macon, GA",         "Miami, FL",         "Milledgeville, GA",
            "Myrtle Beach, SC",  "Palm Beach, FL",    "Philadelphia, PA", 
            "San Jose, CA",      "St. Paul, MN",      "State College, PA",
            "Tallahassee, FL",   "Wichita, KS")

city.state &lt;- strsplit(as.character(cities), ", ")

# Create a city vector and state vector so we can use the vectors to get the
# the city and state name in the right format to compare with the us.cities
# data set
n &lt;- length(cities)
state &lt;- rep(NA, n)
city &lt;- rep(NA, n)
for (i in 1:n) {
  city[i] &lt;- city.state[[i]][1]
  state[i] &lt;- city.state[[i]][2]
}

data1$state &lt;- state[data1$qsb]
data1$city &lt;- city[data1$qsb]

# Format need to compare the city state names with the us.cities data set
cityState &lt;- paste(data1$city, data1$state, sep = " ")

# Obtain some of the latitudes of longitudes of the communities from the 
# us.cities data set
data(us.cities)
location &lt;- us.cities[us.cities$name %in% cityState, ]
location &lt;- location[, c("name", "lat", "long")]

# Determine which communities are not found in the us.cities data set and 
# obtain the location of those communities from the web
unique(cityState[!(cityState %in% us.cities$name)])

location &lt;- rbind(location,
             c("Aberdeen SD", 45.46, -98.49),
             c("Palm Beach FL", 26.72, -80.05),
             c("St. Paul MN", 44.94, -93.09),
             c("Myrtle Beach SC", 33.69, -78.89),
             c("Milledgeville GA", 33.08, -83.23))

# Manipulate the data to get in the right format for the qmap function
location &lt;- location[order(location$name), ]
location$city &lt;- city
location$lat &lt;- as.numeric(location$lat)
location$long &lt;- as.numeric(location$long)

lat &lt;- location$lat
long &lt;- location$long

# List of communities that we don't need to manually adjust the name on the map
include &lt;- c(1, 4, 5, 6, 7, 9, 10, 13, 14, 15, 22, 25, 26)
  
png("map.png",
    width = 800,
    height = 800)
print(qmap("united states", zoom = 4, maptype = "roadmap", source = "google", 
           extent = "panel", color = "bw") +
      labs(x = "Longitude", y = "Latitude") + 
      geom_point(data = data.frame(cbind(long, lat)), aes(x = long, y = lat), 
                 colour = "red", size = 5) +
      geom_text(data = location[include, ], aes(x = long, y = lat, label = name, 
                                              vjust = 0, hjust = -0.1), 
                colour = "red", size = 6, fontface = "bold") +
      theme(axis.text = element_text(size = 15),
            axis.title = element_text(size = 18)) +
      annotate("text", x = location[11, ]$long - 2.5, 
               y = location[11, ]$lat - 0.5,
               label = location[11, ]$name, #Fort Wayne
               colour = "red", size = 6, fontface = "bold") +
      annotate("text", x = location[12, ]$long - 3, 
               y = location[12, ]$lat + 0.3,
               label = location[12, ]$name, # Gary
               colour = "red", size = 6, fontface = "bold") +
      annotate("text", x = location[23, ]$long - 0, 
               y = location[23, ]$lat - 0.5,
               label = location[23, ]$name, #St. Paul
               colour = "red", size = 6, fontface = "bold") +
      annotate("text", x = location[21, ]$long - 0, 
               y = location[21, ]$lat - 0.5,
               label = location[21, ]$name, #Philadelphia
               colour = "red", size = 6, fontface = "bold") +
      annotate("text", x = location[24, ]$long + 5.5, 
               y = location[24, ]$lat - 0,
               label = location[24, ]$name, #State College
               colour = "red", size = 6, fontface = "bold") +
      annotate("text", x = location[2, ]$long + 3.5, 
               y = location[2, ]$lat + 0.6,
               label = location[2, ]$name, #Akron
               colour = "red", size = 6, fontface = "bold") +
      annotate("text", x = location[3, ]$long - 3.5, 
               y = location[3, ]$lat + 0.3,
               label = location[3, ]$name, # Biloxi
               colour = "red", size = 6, fontface = "bold") +
      annotate("text", x = location[20, ]$long + 5.5, 
               y = location[20, ]$lat - 0,
               label = location[20, ]$name, #Palm Beach
               colour = "red", size = 6, fontface = "bold") +
      annotate("text", x = location[17, ]$long + 0, 
               y = location[17, ]$lat - 0.8,
               label = location[17, ]$name, #Miami
               colour = "red", size = 6, fontface = "bold") +
      annotate("text", x = location[8, ]$long - 5, 
               y = location[8, ]$lat + 0.3,
               label = location[8, ]$name, # Columbus
               colour = "red", size = 6, fontface = "bold") +
      annotate("text", x = location[16, ]$long + 3.5, 
               y = location[16, ]$lat - 0.5,
               label = location[16, ]$name, #Macon
               colour = "red", size = 6, fontface = "bold") +
      annotate("text", x = location[18, ]$long - 3.8, 
               y = location[18, ]$lat + 0.8,
               label = location[18, ]$name, # Milledgeville
               colour = "red", size = 6, fontface = "bold") +
      annotate("text", x = location[19, ]$long + 5.6, 
               y = location[19, ]$lat - 0.2,
               label = location[19, ]$name, #Myrtle Beach
               colour = "red", size = 6, fontface = "bold"))
dev.off()

###############################################################################
# Create the dotchart of the communities sample size before and after data
# cleaning
###############################################################################

# Obtain the sample size of the communities before data cleaning for each year
nPeople &lt;- as.vector(t(cbind(as.vector(table(data1$qsb)),
                             as.vector(table(data2$qsb)),
                             as.vector(table(data3$qsb)))))

# Use the variable year to identify which year the sample size came from
Year &lt;- rep(c("2008", "2009", "2010"), times = 26)

# Create data set of the sample sizes for each community and year before 
# data cleaning
location &lt;- cbind(name = rep(location$name, each = 3),
                  city = rep(location$city, each = 3),
                  lat = rep(location$lat, each = 3), 
                  long = rep(location$long, each = 3), 
                  nPeople, 
                  Year)
location &lt;- data.frame(location)
location$lat &lt;- as.numeric(as.character(location$lat))
location$long &lt;- as.numeric(as.character(location$long))
location$nPeople &lt;- as.numeric(as.character(location$nPeople))


# Obtain the sample size of each community at each year after cases with 
# missing values have been omitted
cnpeople = as.vector(t(cbind(as.vector(table(cities[cleanData1$qsb])), 
                             as.vector(table(cities[cleanData2$qsb])), 
                             as.vector(table(cities[cleanData3$qsb])))))

# Use the city, state format as the labels for the dotchart
location$name = rep(cities, each = 3)

# Use the replicated location data for the cleaned data sample size
sampleSize = rbind(location, location)

# Use the cleaned variable to distinguish the data before and after cleaning
sampleSize$Cleaned = c(rep(FALSE, 78), rep(TRUE, 78))

# Replace the replicated sample sizes with the correct sample size for the 
# cleaned data
sampleSize$nPeople[sampleSize$Cleaned] = cnpeople

# Order the communties on the dotchart in such a way that it shows up 
# alphabetically 
sampleSize[, 1] = factor(sampleSize[, 1], levels = cities[26:1])

png("SampleSizes.png",
    width = 600,
    height = 500)
print(ggplot(sampleSize, 
       aes(x = nPeople, y = name, colour = Year, 
           shape = Year, size = Cleaned)) +
      scale_size_manual(values = c(3, 6)) +
      geom_point(alpha = 0.5) +
      labs(title = "Sample Size per Year") +
      scale_colour_brewer(palette = "Set2") + 
      labs(x = "Sample Size", y = "Community")+ 
      scale_x_continuous(limits = c(0, 2000)) + 
      guides(colour = guide_legend(override.aes = list(size = 4))))
dev.off()
