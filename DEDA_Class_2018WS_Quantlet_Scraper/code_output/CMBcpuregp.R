# clear variables and close windows
rm(list = ls(all = TRUE))
graphics.off()

# set working directory
setwd('')

cpu.l  = readRDS('cpureg.rds')
reg.lm = cpu.l$reg.lm
cpu.df = cpu.l$cpu.df

#pdf('CMBcpuregp.pdf', width = 6.5, height = 2.8)

par(mar = c(3, 5.5, 2.5, 4.8))

plot(cpu.df$Date, 
     log10(cpu.df$Transistors),
     pch  = 16,
     cex  = 0.6,
     col  = adjustcolor('black', alpha.f = 0.15),
     bty  = 'l',
     xaxt = 'n',
     yaxt = 'n',
     xlab = '',
     ylab = '')

lines(cpu.df$Date, 
      predict(reg.lm),
      lwd = 3, 
      lty = 1,
      col = 'blue')

box(lwd = 2, 
    bty = 'l', 
    col = 'black')

mtext('Transistor count', 
      side = 3,
      line = 0.9, 
      cex  = 1, 
      col  = 'black',
      adj  = -0.2)

x.at = seq(1970, 2010, 10)

axis(side     = 1,
     at       = x.at,
     tick     = FALSE, 
     cex.lab  = 0.8, 
     line     = -0.7, 
     col.axis = 'black')

# y axis tick positions and formatted labels
y.at  = seq(4, 10)
y.lab = 10 ** y.at / 1000
y.lab = format(y.lab,
               big.mark   = ',',
               scientific = FALSE,
               trim       = TRUE)

y.lab = paste(y.lab, 'k', sep='')

axis(side     = 2, 
     at       = y.at,
     labels   = y.lab,
     tick     = FALSE, 
     cex.lab  = 0.8, 
     line     = -0.8, 
     col.axis = 'black',
     las      = 1)

usr = par('usr')

segments(x.at, 
         usr[3], 
         x.at,
         usr[3] - ((usr[4] - usr[3]) * 0.005), 
         lwd  = 4,
         lend = 2,
         col  = 'white', 
         xpd  = TRUE)

segments(usr[1], 
         y.at,
         usr[1] - ((usr[2] - usr[1]) * 0.005), 
         y.at,
         lwd  = 4, 
         lend = 2,
         col  = 'white', 
         xpd  = TRUE)

#dev.off()
