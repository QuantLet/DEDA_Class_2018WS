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

s_date = "2016-11-01"
e_date = "2017-02-14"

###########################################################################
# QNet2 top search queries
###########################################################################
query.list.search = Init(	start.date = s_date,
							end.date = e_date,
							dimensions = "ga:eventLabel",
							metrics = "ga:totalEvents",
							filters = "ga:eventCategory==QNet2Visu;ga:eventAction==search",
							sort = "-ga:totalEvents",
							max.results = 1000,
							table.id = "ga:134092861")

ga.query = QueryBuilder(query.list.search)
ga.df	 = GetReportData(ga.query, oauth_token)

# Parameters for the length and the column names of the output
samplesize	= 100
columnnames = c("SearchQuery", "frequency")
colnames(ga.df) = columnnames
qn_top_search_frm = ga.df[1:samplesize,]

# output as data frame, top search queries as defined by samplesize
qn_top_search_frm

# trimmed output omitting search queries with less than 3 characters
qn_top_search_frm_trimmed = qn_top_search_frm[nchar(qn_top_search_frm$SearchQuery) &gt;= 3,]


# convert to Latex output
print(xtable(qn_top_search_frm_trimmed, align = "|r|l|r|", digits = 0), include.rownames = F)

# convert to Markdown format
( k_t = kable(qn_top_search_frm_trimmed, row.names = F) )
writeLines(k_t, con = "QTopSearch_trimmed.md")
