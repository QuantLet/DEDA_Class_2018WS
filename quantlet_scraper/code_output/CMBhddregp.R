# clear variables and close windows
rm(list = ls(all = TRUE))
graphics.off()

# set working directory
setwd("")
options(stringsAsFactors = FALSE)

hdd.l  = readRDS("hddreg.rds")
reg.lm = hdd.l$reg.lm
hdd.df = hdd.l$hdd.df

#pdf("CMBhddregp.pdf", width = 6.5, height = 2.8)

par(mar = c(3, 4.8, 2.5, 4.8))

plot(hdd.df$date, 
     log(hdd.df$per.GB),
     pch  = 16,
     cex  = 0.6,
     col  = adjustcolor("black", alpha.f = 0.15),
     bty  = 'l',
     xaxt = "n",
     yaxt = "n",
     xlab = "",
     ylab = "")

lines(hdd.df$date, 
      predict(reg.lm),
      lwd = 3, 
      lty = 1,
      col = "blue")

box(lwd = 2, 
    bty = 'l', 
    col = "black")

mtext("Price per GB", 
      side = 3,
      line = 0.9, 
      cex  = 1, 
      col  = "black",
      adj  = -0.2)

# axis tick labels
x.from = as.Date("1980-01-01")
x.to   = as.Date("2010-01-01")
x.at   = seq(from=x.from, to=x.to, by="5 years")
x.lab  = format(x.at, '%Y')

axis(side     = 1,
     at       = x.at,
     labels   = x.lab,
     tick     = FALSE, 
     cex.lab  = 0.8, 
     line     = -0.7, 
     col.axis = "black")

y.pos = c(0.05,  3,  150,  8000,  440000)
y.lab = formatC(y.pos,
                big.mark      =',',             
                formatC       ='f',
                drop0trailing =TRUE)

y.at  = log(y.pos)

axis(side     = 2, 
     at       = y.at,
     labels   = y.lab,
     tick     = FALSE, 
     cex.lab  = 0.8, 
     line     = -0.8, 
     col.axis = "black",
     las      = 1)

usr  = par("usr")
segments(x.at, 
         usr[3], 
         x.at,
         usr[3] - ((usr[4] - usr[3]) * 0.005), 
         lwd  = 4,
         lend = 2,
         col  = "white", 
         xpd  = TRUE)

segments(usr[1]+10, 
         y.at,
         usr[1] - ((usr[2] - usr[1]) * 0.005), 
         y.at,
         lwd  = 4,
         lend = 2,
         col  = "white", 
         xpd  = TRUE)

#dev.off()
