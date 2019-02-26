## clear history
rm(list = ls(all = TRUE))
graphics.off()

## Zeichne die verallg. Inf fuer B(5,0.4)
par(mfrow = c(1, 2))
p = 0.4
n = 5

curve(pbinom(x, n, p), from = -1, to = 6, type = "s", ylab = expression(), 
      lwd = 3, col = "black")
title("Verteilungsfunktion")

curve(qbinom(x, n, p), from = 0, to = 1, type = "s", ylab = expression(), 
      xlab = expression(u), lwd = 3, col = "black")
title("Pseudoinverse")
