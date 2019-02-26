# Clear all variables
rm(list = ls(all = TRUE))
graphics.off()

# Install and load packages
libraries = c("RGoogleAnalytics", "httpuv", "xtable", "knitr")
lapply(libraries, function(x) if (!(x %in% installed.packages())) {
  install.packages(x)
})
lapply(libraries, library, quietly = TRUE, character.only = TRUE)

# setwd("C:/R/GoA/QNet")

oauth_token = Auth(client.id = "XXX", client.secret = "YYY")

GetProfiles(oauth_token)

s_date = "2013-11-16"
e_date = "2016-11-22"

###########################################################################
# Most downloaded Quantlets
###########################################################################
query.list.Qlet = Init( start.date = s_date,
						end.date = e_date,
						dimensions = "ga:eventLabel",
						metrics = "ga:totalEvents",
						filters = "ga:eventCategory==QNetShow",
						sort = "-ga:totalEvents",
						max.results = 2000,
						table.id = "ga:78690351")

ga.query = QueryBuilder(query.list.Qlet)
ga.df 	 = GetReportData(ga.query, oauth_token)

# Parameters for the length and the column names of the output
samplesize	= 8
columnnames = c("Quantlet", "Downloads")

colnames(ga.df) = columnnames

## Part I
# output as data frame, top downloaded Quantlets as defined by samplesize
ga.df[1:samplesize,]


## Part II
# read Qlet-descriptions
qn_descript = read.csv2("quantlet_description_list.csv", col.names = c("Quantlet", "Description"))
qn_descript$Description = paste(qn_descript$Quantlet, qn_descript$Description)

qn_top_frm = ga.df[1:samplesize,]
qn_top_described = merge(qn_descript, qn_top_frm, by = "Quantlet")[-1]

# Check if the description list contains all top Qlets
if (nrow(qn_top_described) &lt; nrow(qn_top_frm)) {
    print("Warning: There is no description for the following Quantlets:")
    missing_quantlets = qn_top_frm$Quantlet[!(qn_top_frm$Quantlet %in% qn_descript$Quantlet)]
    print(as.character(missing_quantlets))
}

# Sort and rename
qn_top_described = qn_top_described[order(qn_top_described$Downloads, decreasing = TRUE), ]
names(qn_top_described) = columnnames

qn_top_described

# convert to Latex output
print(xtable(qn_top_described, align = "|r|l|r|", digits = 0), include.rownames = FALSE)

# convert to Markdown format
( k_t = kable(qn_top_described, row.names = F) )
writeLines(k_t, con = "TopDownloads.md")
