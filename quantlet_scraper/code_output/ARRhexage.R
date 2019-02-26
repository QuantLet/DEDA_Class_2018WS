# Close windows and clear variables
graphics.off()
rm(list = ls(all = TRUE))

# Install Packages
libraries = c("hexbin")
lapply(libraries, function(x) if (!(x %in% installed.packages())) {install.packages(x)})
lapply(libraries, library, quietly = TRUE, character.only = TRUE)

# setting (font, size etc.) for paper or presentation output, depending on choice: pub="paper" or pub="presentation"
font                 = "serif" # Font Times
label.size.main_axis = 3  # size of label symbols
label.size.support   = 2  # size of axes label symbols
col.from             = 0.2  # shading from this percentage on (number between 0 and 1)
res                  = 300 # resolution of pictures in ppi

# data input and selection of overall scores of HB, RePEc and GS and age
data     = read.csv2("ARRdata.csv",sep=";",dec=",",header = T,stringsAsFactors = FALSE)
data1    = data[!is.na(data$age_combined),]
subagehb = na.exclude(data1[order(data1$hb_commonscore, decreasing = T), 
                            grep(pattern = "age_combined|hb_commonscore",colnames(data1))])
subagerp = na.exclude(data1[order(as.numeric(data1$rp_average_rank_score), decreasing = F), 
                            grep(pattern = "age_combined|rp_average_rank_score", colnames(data1))])
subagegs = na.exclude(data1[order(data1$gs_total_cites, decreasing = T), 
                            grep(pattern = "age_combined|gs_total_cites",colnames(data1))])
m           = 600
len         = min(dim(subagehb)[1],dim(subagegs)[1],dim(subagerp)[1])
if (m&lt;=len)len=m
# selecting data
ageshb      = subagehb[1:len, "age_combined"] 
agesrp      = subagerp[1:len, "age_combined"]
agesgs      = subagegs[1:len, "age_combined"]
hb          = subagehb[1:len, "hb_commonscore"]
rp          = as.numeric(subagerp[1:len, "rp_average_rank_score"])
gs          = subagegs[1:len, "gs_total_cites"]      

# setting colors 
max      = 255
az_red   = rgb(138, 15, 20,alpha=0.999999 *max,maxColorValue = max)
az_green = rgb(  0, 87, 44,alpha=0.999999 *max,maxColorValue = max)
az_blue  = rgb(  0, 55,108,alpha=0.999999 *max,maxColorValue = max)

png(file = "ARRhexagehb.png", width = 7, height = 6, units = "in", res = res, family = font)
hexbinplot(hb ~ ageshb, xlab = list(label = "Age", cex = label.size.main_axis), xlim = c(27.5,92.5),
           ylab = list(label = "HB", cex = label.size.main_axis), style = "colorscale", 
           border = TRUE, aspect = 1, trans = sqrt, inv = function(ages) ages^2,
           scales = list(cex = label.size.support), cex.labels = label.size.support, 
           cex.title = label.size.support, colramp = function(n) {
             rgb(0, 87, 44, alpha = seq(from = col.from, to = 0.999999, length = n) * 
                   max, maxColorValue = max)
           }, colorcut = c(0, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1))
# style: string specifying the style of hexagon plot, see 'grid.hexagons' for the
# possibilities!  border=TRUE: frame around the hexagons!  trans: specifying a
# transformation for the counts!  inv: the inverse transformation of trans!
# scales: the inverse transformation of trans!  coloramp: color of hexagon
# depends on count it represents as greater counts as darker!  colorcut: setting
# number of different colored hexagons by setting intervals!
dev.off()

png(file = "ARRhexagerp.png", width = 7, height = 6, units = "in", res = res, family = font)
hexbinplot(-rp/100 ~ agesrp, xlab = list(label = "Age", cex = label.size.main_axis), 
           xlim = c(27.5, 92.5), ylab = list(label = "RP", cex = label.size.main_axis,ylim = c(8,0)),
           scales = list(cex = label.size.support, y = list(at = seq(0, -6, -2),labels = seq(0, 6, 2))),
           style = "colorscale", border = TRUE, aspect = 1,trans = sqrt, inv = function(ages) ages^2,
           cex.labels = label.size.support, cex.title = label.size.support,colramp = function(n) {
             rgb(0, 55, 108, alpha = seq(from = col.from, to = 1, length = n) * max, maxColorValue = max)
           }, colorcut = c(0, 0.2, 0.4, 0.6, 0.8, 1))
axis(2, at = -c(2:0) * 200, labels = c(2:0) * 200)
dev.off()

png(file = "ARRhexagegs.png", width = 6, height = 6, units = "in", res = res, family = font)
hexbinplot(gs/10000 ~ agesgs, xlab = list(label = "Age", cex = label.size.main_axis), 
           xlim = c(27.5, 92.5), ylab = list(label = "GS", cex = label.size.main_axis), 
           style = "colorscale", border = TRUE, aspect = 1, trans = sqrt, inv = function(ages) ages^2, 
           cex.labels = label.size.support, cex.title = label.size.support, scales = list(cex = label.size.support), 
           colramp = function(n) {
             rgb(138, 15, 20, alpha = seq(from = col.from, to = 0.999999, length = n) * 
                   max, maxColorValue = max)
           }, colorcut = c(0, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1))
dev.off()
