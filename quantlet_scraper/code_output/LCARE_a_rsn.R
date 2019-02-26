# clear history
rm(list = ls(all = TRUE))
graphics.off()

# install and load packages
libraries = c("sn")
lapply(libraries, function(x) if (!(x %in% installed.packages())) {
  install.packages(x)
})
lapply(libraries, library, quietly = TRUE, character.only = TRUE)

# define matrix and save as datasets
V      = 1000
er_005 = matrix(0, 250, V)
for (i in 1:V) {
  er_005[, i] = as.matrix(rsn(n = 250, xi = 0, omega = sqrt(5e-05), alpha = 2))
}
write.table(er_005, "er_005.dat", row.names = F, col.names = F)
write.table(er_005, "er_005.txt", row.names = F, col.names = F)

er_001 = matrix(0, 250, V)
for (i in 1:V) {
  er_001[, i] = as.matrix(rsn(n = 250, xi = 0, omega = sqrt(4e-04), alpha = 4))
}
write.table(er_001, "er_001.dat", row.names = F, col.names = F)
write.table(er_001, "er_001.txt", row.names = F, col.names = F)
