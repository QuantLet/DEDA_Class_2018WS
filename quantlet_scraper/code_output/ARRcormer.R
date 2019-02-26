# clear istory
rm(list = ls(all = TRUE))
graphics.off()
# settings
font = "Times"
res  = 300

# activating required packages, if they are not installed they first get
# installed
libraries = c("corrplot")
lapply(libraries, function(x) if (!(x %in% installed.packages())) {install.packages(x)})
lapply(libraries, library, quietly = TRUE, character.only = TRUE)

# data input and selection of all score values (no rankings)
data  = read.csv2("ARRdata.csv", sep = ";", dec = ",", header = T, stringsAsFactors = FALSE)
data  = data[!is.na(data$hb_commonscore) &amp; !is.na(data$rp_author) &amp; !is.na(data$gs_author),]
data2 = data[, grepl(pattern = "hb_age_today|hb_commonscore", x = colnames(data)) | 
  (grepl(pattern = "rp_", x = colnames(data)) &amp; grepl(pattern = "score", x = colnames(data))) | 
  (grepl(pattern = "gs_total_cites|gs_h_index|gs_i_index", x = colnames(data)))]
x = data2
colnames(x) = gsub(pattern = "_score", replacement = "", x = colnames(x))

# computing the correlation matrix
mcor = cor(x, method = c("pearson"), use = "pairwise.complete.obs")  # 'pearson', 'kendall', 'spearman'

# plot of the correlation matrix
png(file = "ARRcormer.png", width = 10, height = 10, units="in",res=res, family = font, pointsize = 7)
corrplot(mcor, order = "AOE", method = "color", type = "upper", tl.col = "black", 
  cl.cex = 1.5, tl.cex = 1.5, addCoef.col = "black", addCoef.cex = 1, addCoefasPercent = TRUE, 
  p.mat = 1 - abs(mcor), sig.level = 0.75, insig = "blank")
dev.off()
# type=upper: upper triangular matrix!  order: 'orignial'= as is in the matrix,
# order='AOE' for the angular order of the eigenvectors.
