## clear history
rm(list = ls(all = TRUE))
graphics.off()

## install and load packages
libraries = c("QRM")
lapply(libraries, function(x) if (!(x %in% installed.packages())) {
  install.packages(x)
})
lapply(libraries, library, quietly = TRUE, character.only = TRUE)

par(mfrow = c(1, 2))
plot(rcopula.clayton(500, 3, 2), pch = 19, ann = F, cex = 0.6, cex.axis = 0.6)
plot(rcopula.frank(500, 6, 2), pch = 19, ann = F, cex = 0.6, cex.axis = 0.6)
