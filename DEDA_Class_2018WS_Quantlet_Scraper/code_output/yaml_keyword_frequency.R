# Clear all variables
rm(list = ls(all = TRUE))
graphics.off()

# Install and load packages
libraries = c("tm", "ggplot2")
lapply(libraries, function(x) if (!(x %in% installed.packages())) {
  install.packages(x)
})
lapply(libraries, library, quietly = TRUE, character.only = TRUE)


(obj.names = load("yaml_DTM.RData", .GlobalEnv))

# summary of the DocumentTermMatrix containing the keywords from the YAML meta info in Quantlets
DTM

# transform DocumentTermMatrix to a standard matrix (in TDM format)
m_a = as.matrix(as.TermDocumentMatrix(DTM))
# calculate and sort term frequencies
freq = sort(rowSums(m_a), decreasing=TRUE)
# put everything in a dataframe
wf = data.frame(word = names(freq), freq = freq)


# Plot word distribution via ggplot : Implementation of the Grammar of Graphics in R

freq_min = 70
# words with frequency below freq_min will not be plotted
p &lt;- ggplot(subset(wf, freq &gt;= freq_min), aes(word, freq)) + geom_bar(stat="identity") + theme(axis.text.x=element_text(angle=45, hjust=1))
p

# all words sorted in descending order
ggplot(wf, aes(x = reorder(word, -freq), y = freq)) +
geom_bar(stat="identity") + theme(axis.text.x=element_text(angle=45, hjust=1))

# top 100 words wrt. the frequency
ggplot(wf[1:100,], aes(x = reorder(word, -freq), y = freq)) +
geom_bar(stat="identity") + theme(axis.text.x=element_text(angle=45, hjust=1))
