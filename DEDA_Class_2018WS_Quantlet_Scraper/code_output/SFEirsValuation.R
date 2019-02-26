##### SFM1 - Project -&gt; Compare the three different ways to value an IRS

# clear all variables
rm(list = ls(all = TRUE))
graphics.off()

# install and load packages
libraries = c("gridExtra", "grid", "gtable")
lapply(libraries, function(x) if (!(x %in% installed.packages())) {
    install.packages(x)
})
lapply(libraries, library, quietly = TRUE, character.only = TRUE)

######## 1. Input Parameters

Rfix     = 0.075  # Fix Rate
# Maturities and the corresponding yields
Maturity = c(0.0, 0.5, 1.0, 1.5, 2.0)
Yields   = c(0, 0.06185567, 0.06382979, 0.06593407, 0.07471264)  # Yields corresponding to the maturities

y        = data.frame(Maturity, Yields)  # zero-bond-yield-curve

P        = 10  # Principal

######## 2. Valuation

######## 2.1. Helper Functions

# Value of a fixed Coupon Bond
Bfix = function(yields, Coupon_Anually, P = 1) {

    D                  = sapply(1:(nrow(yields)), function(x) (yields[x, 2] * yields[x, 1] + 1)^(-1))  # Discountfactors
    PayDiff            = c(0, diff(yields[, 1]))  # Time between Paymemnts
    BondPrice          = (D %*% (PayDiff * Coupon_Anually) + D[length(D)]) * P  #Final Price of the Bond
    NPVs               = P * D * (PayDiff * Coupon_Anually)  # Net Present Value of the single coupon payments
    NPVs[length(NPVs)] = NPVs[length(NPVs)] + D[length(D)] * P  # Net Present Value of all payments
    
    return(list(BondPrice = BondPrice, PV = NPVs))
}

# Value of a Forward Rate Agreement Si&lt;Ti and both are the times of the yield
FRA = function(Rfix, yields, Si, Ti, P = 1) {
    
    DSi = ((yields[, 2][yields[, 1] == Si] * Si) + 1)^(-1)  # Discountfactor corresponding to Si
    DTi = ((yields[, 2][yields[, 1] == Ti] * Ti) + 1)^(-1)  # Discountfactor corresponding to Ti
    FRA = (DTi * (Ti - Si) * Rfix + DTi - DSi) * P  # Price of the FRA
    
    return(FRA)
}

# Simple compounding Forward Rates implied by the Yield Curve Si&lt;Ti and both are
# the times of the yield
ForwardRates = function(yields, Si, Ti) {
    
    DSi = ((yields[, 2][yields[, 1] == Si] * Si) + 1)^(-1)  # Discountfactor corresponding to Si
    DTi = ((yields[, 2][yields[, 1] == Ti] * Ti) + 1)^(-1)  # Discountfactor corresponding to Ti
    FR  = (1/(Ti - Si)) * ((DSi/DTi) - 1)  # Forward Rate for time Si to Ti
    
    return(FR)
}

######## 2.2. Valuation in Terms of Bond Prizes

VSwapB   = as.numeric(Bfix(y, Rfix, P)$BondPrice - P)

######## 2.3. Valuation in Terms of FRA Prizes

VSwapFRA = sum(unlist(lapply(1:(nrow(y) - 1), function(x) FRA(Rfix, y, y[x, 1], y[x + 1, 1], P))))

######## 2.4. Valuation in Terms of discounted Cash Flows

DCF    = P * c(0, as.numeric((sapply(2:(nrow(y)), function(x) (y[x, 2] * y[x, 1] + 1)^(-1)) * 
         diff(y[, 1])) * (Rfix - sapply(1:(nrow(y) - 1), function(x) ForwardRates(y, y[x, 1], 
         y[x + 1, 1])))))
VSwapR = sum(DCF)

######## 3. Results

Results = data.frame(`Maturity` = c(format(round(Maturity,2), nsmall = 1), "Results:"), 
                     `Fixed Bond Cash Flows` = round(c(Bfix(y, Rfix, P)$PV, Bfix(y, Rfix, P)$BondPrice), 5),
                     `Floating Rate Note` = c(P, rep("", length(Maturity) - 1), P), 
                     `Bond based Value` = c(rep("", length(Maturity)), round(Bfix(y, Rfix, P)$BondPrice - P, 5)), 
                     `FRA Value` = round(c(0, unlist(lapply(1:(nrow(y) - 1), function(x) FRA(Rfix, y, y[x, 1], y[x + 1, 1], P))), VSwapFRA), 5), 
                     DCF = round(c(DCF, sum(DCF)), 5))


g          = tableGrob(head(Results, n = nrow(Results)), theme = ttheme_minimal(), rows = NULL)

separators = replicate(ncol(g) - 1, segmentsGrob(x1 = unit(0, "npc"), gp = gpar(lty = 2)), simplify = FALSE)

# add vertical lines on the left side of columns (after 2nd)
g = gtable::gtable_add_grob(g, grobs = separators, t = 1, b = nrow(g), l = seq_len(ncol(g) - 1) + 1)

g = gtable_add_grob(g, grobs = segmentsGrob(x0 = unit(0, "npc"), y0 = unit(0, "npc"), 
    x1 = unit(1, "npc"), y1 = unit(0, "npc"), gp = gpar(lwd = 2)), t = 1, b = nrow(g) - 1, l = 1, r = ncol(g))

g = gtable_add_grob(g, grobs = segmentsGrob(x0 = unit(0, "npc"), y0 = unit(0, "npc"), 
    x1 = unit(1, "npc"), y1 = unit(0, "npc"), gp = gpar(lwd = 2)), t = 1, b = 1, l = 1, r = ncol(g))

grid.draw(g)
