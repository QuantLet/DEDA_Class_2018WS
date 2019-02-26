# clear variables and close windows
rm(list = ls(all = TRUE))
graphics.off()

# install and load packages
libraries = c("copula", "fGarch", "fBasics")
lapply(libraries, function(x) if (!(x %in% installed.packages())) {
install.packages(x)
})
lapply(libraries, library, quietly = TRUE, character.only = TRUE)

# load data
S = read.table("Representative_Data.txt", skip = 1)
date.time = read.table("Stocks_Series.txt")[, 1]

# parameter settings
simul = 1000
steps = 2500
n.start = 1
window = 250
sel.pairs = c(3, 4)
# sel.pairs = c(2, 3, 4)
w = c(1, 1)
# w = c(1, 1, 1)
quantiles.points = c(5, 1, 0.5, 0.1)/100

X = S[-1, ]
for (i in 1:length(S[1, ])) X[, i] = log(S[-1, i]/S[-length(S[, i]), i])

 # helpful functions 
gumbel.tau2theta = function(tau) {
    1/(1 - tau)
}
clayton.tau2theta = function(tau) {
    2 * tau/(1 - tau)
}
normal.tau2theta = function(tau) {
    sin(tau * pi/2)
}

theta2tau = function(theta) {
    1 - 1/theta
}
tau2theta = function(tau) {
    1/(1 - tau)
}
phi = function(x, theta) {
    exp(-x^{
        1/theta
    })
}
phi_m1 = function(x, theta) {
    (-log(x))^theta
}
cop2d = function(x1, x2, theta) {
    phi(phi_m1(x1, theta) + phi_m1(x2, theta), theta)
}

renditen.copula.fun = function(copula.name, tau, params.margins) {
    gumbel.cop = gumbelCopula(gumbel.tau2theta(tau))
    clayton.cop = claytonCopula(clayton.tau2theta(tau))
    normal.cop = normalCopula(normal.tau2theta(tau))
    if (copula.name == "gumbel") 
        mvdc(gumbel.cop, c("norm", "norm"), params.margins) else if (copula.name == "clayton") 
        mvdc(clayton.cop, c("norm", "norm"), params.margins) else if (copula.name == "normal") 
        mvdc(normal.cop, c("norm", "norm"), params.margins)
}

# HAC Simulation 
simul.g.HAC = function(theta0, theta1, n) {
    # theta0 = 1.5
    dims = c(2, 1)
    theta = c(theta1, theta0 + 0.01)
    f0_gumbel = function(n, Ltheta0, Ltheta1) {
        alpha = Ltheta0/Ltheta1
        as.vector(rstable(n, alpha, 1, cos(alpha * pi/2)^(1/alpha), 0, 1))   
    }
    AC = function(n, Ltheta0, Ltheta1, dim, LV0) {
        V1 = f0_gumbel(n, Ltheta0, Ltheta1)
        X = matrix(runif(dim * n), n, dim)
        exp(-(-log(X)/V1)^(Ltheta0/Ltheta1))
    }   
    HACg = function(n) {
        V0 = f0_gumbel(n, 1, theta0)
        X = rep(n, 0)
        for (i in 1:length(dims)) X = cbind(X, AC(n, theta0, theta[i], dims[i], 
            V0))
        phi(-log(X)/V0, theta0)
    }
    t.hat = function(n) {
        sample = HACg(n)
        # dim(sample)
        pairs(qnorm(sample), pch = 19)
        # theta.matr = tau2theta(cor(sample, method = 'kendall'))
        # z12 = cop2d(sample[,1],sample[,2],theta.matr[1,2]) 
        # z34 = cop2d(sample[,3],sample[,4],theta.matr[3,4])
        sample
    }
    HACg(n)
}

VaR = function(x, start, end, bHAC = FALSE) {
    S.part = S[start:end, ]
    X.part = X[start:(end - 1), ]
    eps = X.part
    sigma.t = eps
    params = matrix(0, 4, dim(S.part)[2])
    for (i in 1:length(S.part[1, ])) {
        fit = garchFit(~garch(1, 1), data = X.part[, i], trace = F)
        params[i, ] = fit@fit$coef
        eps[, i] = fit@residuals
        sigma.t[, i] = fit@sigma.t
    }
    eps = eps/sigma.t
    means = mean(eps)
    sds = sd(eps)
    if (length(sel.pairs) == 2) {
        tau = cor(eps[, sel.pairs], method = "kendall")[1, 2]
        params.margins = list(list(mean = means[sel.pairs[1]], sd = sds[sel.pairs[1]]), 
            list(mean = means[sel.pairs[2]], sd = sds[sel.pairs[2]]))
        eps.copula.g = renditen.copula.fun("gumbel", tau, params.margins)
        eps.copula.c = renditen.copula.fun("clayton", tau, params.margins)
        eps.copula.n = renditen.copula.fun("normal", tau, params.margins)
        eps.copula.g.r = rmvdc(eps.copula.g, n = simul)
        eps.copula.c.r = rmvdc(eps.copula.c, n = simul)
        eps.copula.n.r = rmvdc(eps.copula.n, n = simul)
    } else {
        if (bHAC) {
            f.eps = eps
            for (i in 1:dim(eps)[2]) f.eps[, i] = ecdf(eps[, i])(eps[, i])  # making margins uniform                                                   
            tau = cor(f.eps[, sel.pairs], method = "kendall")  # calculating kendall tau matrix                                                    
            tau.largest = max(tau[upper.tri(tau)])  # largest tau which corresponds to the first group in HAC                                     
            
            if (which(tau[upper.tri(tau)] == tau.largest) == 1) {
                first.paar = c(sel.pairs[1], sel.pairs[2])
            }
            if (which(tau[upper.tri(tau)] == tau.largest) == 2) {
                first.paar = c(sel.pairs[1], sel.pairs[3])
            }
            if (which(tau[upper.tri(tau)] == tau.largest) == 3) {
                first.paar = c(sel.pairs[2], sel.pairs[3])
            }
            rest = sel.pairs[!(sel.pairs %in% first.paar)]  # which variable is left after joining first two                                      
            tau.small = cor(cop2d(f.eps[, first.paar[1]], f.eps[, first.paar[2]], 
                tau.largest), f.eps[, rest], method = "kendall")  # tau of the copula on the highest level
            if (tau.small &gt; tau.largest) 
                tau.small = tau.largest - 0.01  # if tau of the subcopula is smaller than tau on the highest level, put them almost equal       
            eps.HAC.simul = simul.g.HAC(gumbel.tau2theta(tau.small), gumbel.tau2theta(tau.largest), 
                n = simul)  # simulate HAC                                           
            in.hac = c(first.paar, rest)
            new.order = c(which(in.hac == sel.pairs[1]), which(in.hac == sel.pairs[2]), 
                which(in.hac == sel.pairs[3]))  # going back to the order of the variable sel.pairs
            eps.HAC.simul = eps.HAC.simul[, new.order]
            for (i in 1:length(sel.pairs)) eps.HAC.simul[, i] = qnorm(eps.HAC.simul[, 
                i], means[sel.pairs[i]], sds[sel.pairs[i]])  # setting back margins                      
        } else {
            # # estimate simple Archimedean copula, by ML
            f.eps = eps
            for (i in 1:dim(eps)[2]) f.eps[, i] = rank(eps[, i])/(length(eps[, 
                i]) + 1)  # making margins uniform, based on Ranks                                             
            gumbel.cop = gumbelCopula(1.5, dim = 3)
            fit.ml = fitCopula(f.eps[, sel.pairs], gumbel.cop, method = "mpl")@estimate
            params.margins.3 = list(list(mean = means[sel.pairs[1]], sd = sds[sel.pairs[1]]), 
                list(mean = means[sel.pairs[2]], sd = sds[sel.pairs[2]]), list(mean = means[sel.pairs[3]], 
                  sd = sds[sel.pairs[3]]))
            gumbel.cop.3 = gumbelCopula(fit.ml, dim = 3)
            eps.HAC.simul = rmvdc(mvdc(gumbel.cop.3, c("norm", "norm", "norm"), 
                params.margins.3), n = simul)
        }
    }
    L.g = 0
    L.c = 0
    L.n = 0
    L.hac = 0
    S.fin = S.part[dim(S.part)[1], sel.pairs]
    h = sqrt(params[sel.pairs, 2] + params[sel.pairs, 3] * sigma.t[dim(sigma.t)[1], 
        sel.pairs]^2 * eps[dim(eps)[1], sel.pairs]^2 + params[sel.pairs, 4] * sigma.t[dim(sigma.t)[1], 
        sel.pairs]^2)
    for (i in 1:simul) {
        if (length(sel.pairs) == 2) {
            y.g = params[sel.pairs, 1] + h * eps.copula.g.r[i, ]
            y.c = params[sel.pairs, 1] + h * eps.copula.c.r[i, ]
            y.n = params[sel.pairs, 1] + h * eps.copula.n.r[i, ]
            L.g = c(L.g, sum(w * S.fin * (exp(y.g) - 1)))
            L.c = c(L.c, sum(w * S.fin * (exp(y.c) - 1)))
            L.n = c(L.n, sum(w * S.fin * (exp(y.n) - 1)))
        } else {
            y.hac = params[sel.pairs, 1] + h * eps.HAC.simul[i, ]
            L.hac = c(L.hac, sum(w * S.fin * (exp(y.hac) - 1)))
        }
    }
    if (length(sel.pairs) == 2) {
        L.g = L.g[-1]
        L.c = L.c[-1]
        L.n = L.n[-1]
    } else {
        L.hac = L.hac[-1]
    }
    if (length(sel.pairs) == 2) {
        quants.g = quantile(L.g, quantiles.points)
        quants.c = quantile(L.c, quantiles.points)
        quants.n = quantile(L.n, quantiles.points)
        print(c(start, as.vector(quants.g)))
        print(c(start, as.vector(quants.c)))
        print(c(start, as.vector(quants.n)))
        as.vector(cbind(quants.g, quants.c, quants.n))
    } else {
        quants.hac = quantile(L.hac, quantiles.points)
        print(c(start, as.vector(quants.hac)))
        as.vector(quants.hac)
    }
}

moving.VaR.PL = function(steps, window, bHAC = FALSE) {
    if (length(sel.pairs) == 2) 
        VaR.set = rep(0, length(quantiles.points) * 3) else VaR.set = rep(0, length(quantiles.points))
    for (i in 1:steps) VaR.set = rbind(VaR.set, VaR(S, i, (i + window - 1), bHAC))
    VaR.set = VaR.set[-1, ]
    if (!is.vector(VaR.set)) {
        if (length(sel.pairs) == 2) {
            # # setting columns names
            a = rep(c("g_", "c_", "n_"), each = length(quantiles.points))
            b = rep(quantiles.points, 3)
            for (i in 1:length(a)) b[i] = paste(a[i], b[i], sep = "")
            colnames(VaR.set) = b
        } else {
            colnames(VaR.set) = quantiles.points
        }
    }
    L.real = w * S[-dim(S)[1], sel.pairs] * (exp(X[, sel.pairs]) - 1)
    if (length(sel.pairs) == 2) 
        L.real.left = (L.real[, 1] + L.real[, 2])[-(1:window)] else L.real.left = (L.real[, 1] + L.real[, 2] + L.real[, 3])[-(1:window)]
    L.real.left = L.real.left[1:steps]
    data.time.part = as.integer(format(as.Date(as.vector(date.time[window:(window + 
        steps)]), "%d.%m.%Y"), "%Y"))
    if (is.vector(VaR.set)) 
        c(VaR.set, L.real.left, data.time.part) else cbind(VaR.set, L.real.left, data.time.part)
}

plot.PL.var = function(copula, quantile.one, dataset, bHAC = FALSE) {
    PL = as.vector(dataset[, dim(dataset)[2] - 1])
    dataset.date.ind = which(c(1, diff(dataset[, dim(dataset)[2]])) == 1)
    first.date = dataset[, dim(dataset)[2]][1]
    dataset.date.labels = first.date:(first.date + length(dataset.date.ind) - 1)
    if (length(sel.pairs) == 2) {
        q = which(quantiles.points == quantile.one)
        if (copula == "gumbel") {
            copula.name = "Gumbel"
        }
        if (copula == "clayton") {
            q = length(quantiles.points) * 1 + q
            copula.name = "Clayton"
        }
        if (copula == "normal") {
            q = length(quantiles.points) * 2 + q
            copula.name = "Normal"
        }
    } else {
        q = which(quantiles.points == quantile.one)
        if (bHAC) 
            copula.name = "HAC_Gumbel" else copula.name = "Gumbel_3D"
    } 
    VaR.v = as.vector(dataset[, q])
    # already plotting PL and VaR
    pdf(paste("PL_VaR_", copula.name, "_", quantile.one, ".pdf", sep = ""), width = 16, 
        height = 8)
    plot(VaR.v, col = "yellow3", type = "l", ylim = c(min(dataset[, c(q, dim(dataset)[2] - 
        1)]), max(dataset[, c(q, dim(dataset)[2] - 1)])), lwd = 2, xlab = "time", 
        ylab = "P&amp;L", main = paste("VaR -", copula.name, "Copula"), axes = F, frame = T, 
        cex.main = 2, cex.lab = 1.5)
    if (length(PL[PL &gt; VaR.v]) != 0) 
        points(which(PL == pmax(PL, VaR.v)), PL[PL &gt; VaR.v], col = "black", pch = 19, 
            cex = 0.5)
    if (length(PL[PL &lt; VaR.v]) != 0) 
        text(which(PL == pmin(PL, VaR.v)), PL[PL &lt; VaR.v], "+", col = "red3", pch = 19, 
            cex = 1.2)
    axis(1, dataset.date.ind, dataset.date.labels, cex.axis = 1.5)
    y.labels = c(round(seq(min(dataset[, c(q, dim(dataset)[2] - 1)]), max(dataset[, 
        c(q, dim(dataset)[2] - 1)]), length = 7) * 100)/100, 0)
    axis(2, y.labels, y.labels, cex.axis = 1.5)
    dev.off()
}

moving.win = moving.VaR.PL(steps, window, bHAC = F)  # moving window
write.table(moving.win, "bivar.pl.col", row.names = F)

for (i in quantiles.points) {
    plot.PL.var("gumbel", i, moving.win)
    plot.PL.var("clayton", i, moving.win)
    plot.PL.var("normal", i, moving.win)
}
