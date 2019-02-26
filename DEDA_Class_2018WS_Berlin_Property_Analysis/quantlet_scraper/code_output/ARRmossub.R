# clear cache and close windows
rm(list=ls(all=TRUE))
graphics.off()

# librariers
libraries = c("vcd","xtable")
lapply(libraries,function(x)if(!(x %in% installed.packages())){install.packages(x)})
lapply(libraries,library,quietly=TRUE,character.only=TRUE)

# color settings
max      = 180
az_red   = rgb(138, 15, 20, alpha = 0.6 * max, maxColorValue = max)
az_green = rgb(0, 87, 44, alpha = 0.65 * max, maxColorValue = max)
az_blue  = rgb(0, 55, 108, alpha = 0.8 * max, maxColorValue = max)

# setting
font           = "serif" # font Times
fill_color     = c(az_red,az_green,az_blue)
cex.lab        = 2.5
cex.axis       = 2
cex.main       = 3.25
fontsize_big   = 36
fontsize_small = 22
plotsize       = c(16,8)
res            = 300

# data input and selection
data = read.csv2("ARRdata.csv",sep=";",dec=",",header = T,stringsAsFactors = FALSE)
hb   = na.exclude(data[,grep(pattern = "hb_commonscore|subject_fields|age_combined",x = colnames(data))])
rp   = na.exclude(data[,grep(pattern = "rp_rank|subject_fields|age_combined",x = colnames(data))])
gs   = na.exclude(data[,grep(pattern = "gs_total_cites|subject_fields|age_combined",x = colnames(data))])
gs   = gs[order(gs$gs_total_cites,decreasing = T),]

m    = 448
len  = min(dim(hb)[1],dim(rp)[1],dim(gs)[1])
if (m
<len)len #="" (i="" =="" codes="" computing="" count="" different="" for="" gs="" hb="table(hb[order(hb$hb_commonscore,decreasing" hb,="" i,="" i]="ifelse(test" in="" jel="" length(tmp.hb)="" list(names(rp),="" m="" names(gs))))="" names(hb),="" of="" reduce(union,="" rp="table(rp[1:len,]$subject_fields)" rp,="" t),][1:len,]$subject_fields)="" tab="data.frame()" tab["hb_count",="" table="" tmp.hb="grep(pattern" x="names(hb))" {="">
 0, yes = hb[tmp.hb], no = 0)
  tmp.rp = grep(pattern = i, x = names(rp))
  tab["rp_count", i] = ifelse(test = length(tmp.rp) &gt; 0, yes = rp[tmp.rp], no = 0)
  tmp.gs = grep(pattern = i, x = names(gs))
  tab["gs_count", i] = ifelse(test = length(tmp.gs) &gt; 0, yes = gs[tmp.gs], no = 0)
}
tab[, "Total"] = apply(X = tab, MARGIN = 1, FUN = sum)

# preparing data for Plot
tab2              = tab 
tab2              = as.data.frame(t(tab2))
tab4              = tab2[-dim(tab2)[1],]
tab2[,"JELcodes"] = rownames(tab2)
for (i in 1:3) {
  name = substr(x = colnames(tab2)[i], start = 1, stop = 2)
  tmp = cbind(tab2[, c(i, 4)], Ranking = toupper(name))
  colnames(tmp)[1] = "Freq"
  assign(tolower(paste0(name, "2")), value = tmp)
}

tab3 = rbind(gs2, hb2, rp2)
# creating mosaic plot
png(file = "ARRmossub.png", width = plotsize[1], height = plotsize[2], units = "in", 
    res = res, family = font)
  mosaic(~JELcodes + Ranking, data = tab3[!grepl(pattern = "Total", x = tab3$JELcodes),],
         gp = gpar(fill = matrix(rep(fill_color, times = dim(tab2)[1]), ncol = 3, byrow = T)), 
         shade = TRUE, direction = c("v", "h"), spacing = spacing_highlighting(start = unit(1, "lines")), 
         labeling_args = list(gp_labels = gpar(fontsize = fontsize_small, fontface = 1, fontfamily = font), 
                              gp_varnames = gpar(fontsize = fontsize_big, fontface = 1, fontfamily = font)), 
         keep_aspect_ratio = FALSE, margins = unit(8, "lines"))
  # gp: coloring so that all HB, GS and RP are colored the same
dev.off()

# creating histogram for each ranking (HB, RP, GS)
png(file = "ARRhissub.png", width = plotsize[1], height = plotsize[2], units = "in", 
    res = res, family = font)
  par(mfrow = c(3, 1), las = 1, mar = c(3.1, 5.5, 1, 1))
  tab4 = tab4[, order(colnames(tab4))]
  for (i in 1:3) {
    name = substr(x = colnames(tab4)[i], start = 1, stop = 2)
    barplot(height = tab4[, i], names.arg = rownames(tab4), ylab = toupper(name), 
            col = fill_color[i], cex.names = cex.lab, cex.lab = cex.main, space = 0.05, 
            axes = F)
    ax = c(0:4) * 20
    axis(2, at = ax, labels = ax, cex.axis = cex.axis)
    box()
  }
dev.off()
</len)len>