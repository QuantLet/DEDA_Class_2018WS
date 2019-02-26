# clear cache and close windows
rm(list=ls(all=TRUE))
graphics.off()

# install required packages
libraries = c("KernSmooth","misc3d")
lapply(libraries, function(x) if (!(x %in% installed.packages())) {install.packages(x)})
lapply(libraries, library, quietly = TRUE, character.only = TRUE)

# image settings
res  = 300
# data input
data = read.csv2("ARRdata.csv",sep=";",dec=",",header = T,stringsAsFactors = FALSE)
data = data[!is.na(data$hb_commonscore)&amp;!is.na(data$rp_author)&amp;!is.na(data$gs_author),]
# Computing Kernel
d    = kde3d(data$hb_commonscore, data$rp_rank, data$gs_total_cites, n = 30)

png(file = "ARRdenmer3d.png", width = 7, height = 7, units = "in", res = res)
par(mar = c(1, 1, 1, 1))
contour3d(d$d, level = c(max(d$d[10, 10, ]) * 0.001, max(d$d[10, 10, ]) * 0.1, 
                         max(d$d[10, 10, ]) * 0.75), fill = c(FALSE, FALSE, TRUE),
          col.mesh = c("green", "red", "blue"), engine = "standard", 
          screen = list(z = 40, x = -95, y = -45), scale = TRUE)
# level: levels at which to construct contour surfaces fill: filled surfaces
# col.mesh: color to use for the surfaces/ wire frame
box(lwd = 1)
dev.off()
