# clear variables and close windows
rm(list = ls(all = TRUE))
graphics.off()

# install and load packages
libraries = c("rpanel", "fBasics")
lapply(libraries, function(x) if (!(x %in% installed.packages())) {
install.packages(x)
})
lapply(libraries, library, quietly = TRUE, character.only = TRUE)

e1 = new.env(parent = baseenv())
assign("L", c(2, 0, 1, 0), envir = e1)

density.draw.alpha = function(panel) {
    panel$beta = e1$L[2]
    panel$gamma = e1$L[3]
    panel$delta = e1$L[4]
    density.draw(panel)
}

density.draw.beta = function(panel) {
    panel$alpha = e1$L[1]
    panel$gamma = e1$L[3]
    panel$delta = e1$L[4]
    density.draw(panel)
}

density.draw.gamma = function(panel) {
    panel$beta = e1$L[2]
    panel$alpha = e1$L[1]
    panel$delta = e1$L[4]
    density.draw(panel)
}

density.draw.delta = function(panel) {
    panel$beta = e1$L[2]
    panel$gamma = e1$L[3]
    panel$alpha = e1$L[1]
    density.draw(panel)
}

density.draw = function(panel) {
    e1$L[1] = panel$alpha
    e1$L[2] = panel$beta
    e1$L[3] = panel$gamma
    e1$L[4] = panel$delta
    density.draw.exact(panel)
}

density.draw.exact = function(panel) {
    main.s = paste("alpha = ", panel$alpha, "| beta = ", panel$beta, "| gamma = ", 
        panel$gamma, "| delta =", panel$delta)
    x = seq(-10, 10, 0.2)
    y = dstable(x, alpha = panel$alpha, beta = panel$beta, gamma = panel$gamma, 
        delta = panel$delta, pm = panel$parametrization)
    
    nf = layout(matrix(c(1, 2), 2, 1, byrow = TRUE), c(1, 1), c(0.5, 0.5), TRUE)
    par(mai = (c(-0.05, 0.7, 0.3, 0) + 0.1))
    plot(x, y, type = "l", xlim = c(-5, 5), ylim = c(0, 0.6), ylab = "f(x)", xlab = "x", 
        lwd = 3, col = "blue3", main = main.s)
    abline(h = seq(0, 0.6, 0.1), v = seq(-5, 5, 1), lty = "dotted")
    
    par(mai = (c(0.7, 0.7, -0.05, 0) + 0.1))
    plot(x, log(y), type = "l", xlim = c(-10, 10), ylim = c(-10, 0), ylab = "log(f(x))", 
        xlab = "x", lwd = 3, col = "blue3")
    abline(h = seq(-10, 0, 2), v = seq(-10, 10, 2), lty = "dotted")
    panel
}
# this function defines at which values the densities are evaluated,
#  if an exact distribution is selected in the panel
density.draw.exact.rb = function(panel) {
    if (panel$exact.dist.type == "gaussian") {
        panel$alpha = 2
        panel$beta = 0
        panel$gamma = 1
        panel$delta = 0
        density.draw.exact(panel)
    } else if (panel$exact.dist.type == "couchy") {
        panel$alpha = 1
        panel$beta = 0
        panel$gamma = 1
        panel$delta = 0
        density.draw.exact(panel)
    } else if (panel$exact.dist.type == "levy") {
        panel$alpha = 0.5
        panel$beta = 0.9999
        panel$gamma = 1
        panel$delta = 1
        density.draw.exact(panel)
    }
}

# the panel is defined, i.e. the adjustable parameter 
# for example the different distributions
panel = rp.control(size = c(320, 350), title = "Stable Distributions")
rp.slider(panel, alpha, 0.01, 2, log = FALSE, action = density.draw.alpha, showvalue = TRUE, 
    initval = 2, resolution = 0.1, pos = c(10, 0, 300, 80))
rp.slider(panel, beta, -1, 1, log = FALSE, action = density.draw.beta, showvalue = TRUE, 
    initval = 0, resolution = 0.1, pos = c(10, 60, 300, 80))
rp.slider(panel, gamma, 0.5, 1.5, log = FALSE, action = density.draw.gamma, showvalue = TRUE, 
    initval = 1, resolution = 0.2, pos = c(10, 120, 300, 80))
rp.slider(panel, delta, -2, 2, log = FALSE, action = density.draw.delta, showvalue = TRUE, 
    initval = 0, resolution = 0.2, pos = c(10, 180, 300, 80))
rp.radiogroup(panel, parametrization, c(0, 1, 2), labels = c("S0", "S1", "S2"), 
    action = density.draw, title = "Parametrization", initval = 0, pos = c(10, 
        240, 145, 100))
rp.radiogroup(panel, exact.dist.type, c("gaussian", "cauchy", "levy"), action = density.draw.exact.rb, 
    title = "Exact Stable Distributions", initval = "gaussian", pos = c(165, 240, 
        145, 100))
