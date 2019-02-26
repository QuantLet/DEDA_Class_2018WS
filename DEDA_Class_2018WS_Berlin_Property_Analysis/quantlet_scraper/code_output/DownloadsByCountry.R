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
# Quantlet downloads by country
###########################################################################
query.list.Country = Init(	start.date = s_date,
							end.date = e_date,
							dimensions = "ga:country",
							metrics = "ga:totalEvents",
							filters = "ga:eventCategory==QNetShow",
							sort = "-ga:totalEvents",
							max.results = 1000,
							table.id = "ga:78690351")

ga.query = QueryBuilder(query.list.Country)
ga.df	 = GetReportData(ga.query, oauth_token)

# Parameters for the length and the column names of the output
samplesize  = 10
columnnames = c("Country", "Downloads")
colnames(ga.df) = columnnames
qn_top_frm = ga.df[1:samplesize,]

# output as data frame, top countries as defined by samplesize
qn_top_frm

# convert to Latex output
print(xtable(qn_top_frm, align = "|r|l|r|", digits = 0), include.rownames = F)

# convert to Markdown format
( k_t = kable(qn_top_frm, row.names = F) )
writeLines(k_t, con = "DownloadsByCountry.md")
