rm(list = ls(all = TRUE))
graphics.off()

# setwd('C:/...')

# install.packages('MASS')
library(MASS)

u0 &lt;- 5 * 10^10  # initial capital of insurance company (in USD)
n &lt;- 70
u1 &lt;- c(seq(0, by = u0/200, length.out = 21), seq(u0/10 + u0/200, by = (u0 - u0/10 - u0/200)/48, length.out = 49))
theta1 &lt;- 0.3  # relative safety loading
dparameters1 &lt;- list(c(0.0584, 0.9416), c(3.59e-10, 7.5088e-09))  # weights (first vector) and exponential parameters (second vector)

# produces the exact ruin probability in infinite time for insurance collective risk model with mixture of 2 exponentials
# distribution claims
ruinmix2exps &lt;- function(u, theta, dparameters) {
    # u: initial capital for risk process theta: security loading in insurance collective risk model dparameters: list, composed
    # of 2 vectors containing the parameters of loss distribution, exponential parameters (first vector) and weights (second
    # vector)
    p1 &lt;- dparameters[[1]]  # exponential parameters
    p2 &lt;- dparameters[[2]]  # weights
    p &lt;- p1[1]/p2[1]/(p1[1]/p2[1] + (1 - p1[1])/p2[2])
    psii &lt;- p2[1] * (1 - p) + p2[2] * p
    r1 &lt;- (psii + theta * sum(p2) - sqrt((psii + theta * sum(p2))^2 - 4 * prod(p2) * theta * (1 + theta)))/(2 * (1 + theta))
    r2 &lt;- (psii + theta * sum(p2) + sqrt((psii + theta * sum(p2))^2 - 4 * prod(p2) * theta * (1 + theta)))/(2 * (1 + theta))
    y &lt;- 1/((1 + theta) * (r2 - r1)) * ((psii - r1) * exp(-r1 * u) + (r2 - psii) * exp(-r2 * u))  # ruin probability using the Laplace transform inversion
    return(y)
}

# the exact ruin probability in infinite time
psi1 &lt;- ruinmix2exps(u1, theta1, dparameters1)

# returns the k-th moment (up to fourth) of the mixture of 2 exponentials distribution claims
moments &lt;- function(k, dparameters) {
    # k: order of moment to calculate dparameters: list, composed of 2 vectors containing the parameters of loss distribution,
    # weights (first vector) and exponential parameters (second vector)
    p1 &lt;- dparameters[[1]]  # weights
    p2 &lt;- dparameters[[2]]  # exponential parameters
    if (k == 1) {
        mk &lt;- sum(p1/p2)
    } else {
        if (k == 2) {
            mk &lt;- 2 * sum(p1/p2^2)
        } else {
            if (k == 3) {
                mk &lt;- 6 * sum(p1/p2^3)
            } else {
                if (k == 4) {
                  mk &lt;- 24 * sum(p1/p2^4)
                }
            }
        }
    }
    return(mk)  # k-th raw moment of the mixture of 2 exponentials distribution claims 
}

# returns the moment generating function or its k-th derivative (up to third) for mixture of 2 exponentials distribution
# claims
mgfs &lt;- function(x, k, dparameters) {
    # x: scalar, n x 1 vector or m x n matrix, argument of the moment generating function k: scalar, integer, 0 =&lt; k &lt;= 3, order
    # of the derivative dparameters: list, composed of 2 vectors containing the parameters of the loss distribution, weights
    # (first vector) and exponential parameters (second vector)
    p1 &lt;- dparameters[[1]]  # weights
    p2 &lt;- dparameters[[2]]  # exponential parameters
    if (k == 0) {
        y &lt;- sum((p1 * p2)/(p2 - t(x)))
    } else {
        if (k == 1) {
            y &lt;- sum((p1 * p2)/(p2 - t(x))^2)
        } else {
            if (k == 2) {
                y &lt;- 2 * sum((p1 * p2)/(p2 - t(x))^3)
            } else {
                if (k == 3) {
                  y &lt;- 6 * sum((p1 * p2)/(p2 - t(x))^4)
                }
            }
        }
    }
    return(y)
}

m &lt;- moments(1, dparameters1)  # 1st raw moment
m2 &lt;- moments(2, dparameters1)  # 2nd raw moment
m3 &lt;- moments(3, dparameters1)  # 3nd raw moment
m4 &lt;- moments(4, dparameters1)  # 4th raw moment

# returns the adjustment coefficient R for mixture of 2 exponentials distribution claims
adjR &lt;- function(theta, dparameters) {
    # theta: security loading in insurance collective risk model dparameters: list, composed of 2 vectors containing the
    # parameters of the loss distribution, weights (first vector) and exponential parameters (second vector)
    p1 &lt;- dparameters[[1]]  # weights
    p2 &lt;- dparameters[[2]]  # exponential parameters
    R0 &lt;- min(p2)
    R0 &lt;- c(12 * theta * m/(3 * m2 + sqrt(9 * m2^2 + 24 * m * m3 * theta)), R0)
    R0 &lt;- min(R0)
    r &lt;- R0
    err = 1
    while (err &gt; 1e-09) {
        D1 &lt;- 1 + (1 + theta) * m * r - mgfs(r, 0, dparameters1)
        D2 &lt;- (1 + theta) * m - mgfs(r, 1, dparameters1)
        err &lt;- r
        r &lt;- r - D1/D2
        err &lt;- abs(err - r)/r
        R &lt;- r
    }
    return(R)  # adjustment coefficient R 
}

R &lt;- adjR(theta1, dparameters1)  # adjustment coefficient R
mgfprim &lt;- mgfs(R, 1, dparameters1)  # moment generating function 

C &lt;- (theta1 * m)/(mgfprim - m * (1 + theta1))

# the exact ruin probability in infinite time
psi1 &lt;- ruinmix2exps(u1, theta1, dparameters1)

# the Cramer-Lundberg approximation
psi2 &lt;- C * exp(-R * u1)

# the exponential approximation
psi3 &lt;- exp(-1 - (2 * m * theta1 * u1 - m2)/sqrt(m2^2 + 4 * theta1 * m * m3/3))

# the Lundberg approximation
psi4 &lt;- (1 + (theta1 * u1 - m2/m/2) * (4 * theta1 * m^2 * m3/(3 * m2^3))) * exp(-(2 * m * theta1 * u1)/m2)

delta1 &lt;- (2 * m * theta1)/(m2 + ((4 * m * m3/3/m2) - m2) * theta1)  # scale parameter of gamma distribution function
delta2 &lt;- (1 + theta1)/(1 + ((4 * m * m3/3/m2^2) - 1) * theta1)  # shape parameter of gamma distribution function

# the Beekman-Bowers approximation
psi5 &lt;- (1 - pgamma(u1, shape = delta2, scale = 1/delta1))/(1 + theta1)

# the Renyi approximation
psi6 &lt;- exp(-(2 * m * theta1 * u1)/(m2 * (1 + theta1)))/(1 + theta1)

# parameters of the De Vylder approximation
delta &lt;- 3 * m2/m3
beta &lt;- 9 * m2^3/(2 * (1 + theta1) * m * m3^2)
p &lt;- 3 * m2^2/(2 * (1 + theta1) * m * m3) - 1/(1 + theta1) + 1

# the De Vylder approximation
psi7 &lt;- (beta/(p * delta)) * exp(-(delta - beta/p) * u1)

if (m2 * m4 &lt; 3/2 * m3^2) {
    thetanew &lt;- (theta1 * m * (2 * m3^2 - m2 * m4)/m2^2/m3)
    mnew &lt;- ((3 * m3^2 - 2 * m2 * m4)/m2/m3)
    m2new &lt;- ((m2 * m4 - 2 * m3^2) * (2 * m2 * m4 - 3 * m3^2)/m3^2/m2^2)
} else {
    thetanew &lt;- 1/2 * theta1/m2^2 * m * (m3 + m2 * m)
    mnew &lt;- m
    m2new &lt;- 1/2/m2 * m * (m3 + m2 * m)
}

p1 &lt;- mnew^2/(m2new - mnew^2)
p2 &lt;- mnew/(m2new - mnew^2)
dparametersnew &lt;- list(p1, p2)  # gamma parameters

# returns the k-th moment (up to fourth) of the mixture of 2 gamma distribution claims
momentsgam &lt;- function(k, dparameters) {
    # k: order of moment to calculate dparameters: list of scalars, parameters of gamma distribution
    p1 &lt;- dparameters[[1]]
    p2 &lt;- dparameters[[2]]
    if (k == 1) {
        mk &lt;- p1/p2
    } else {
        if (k == 2) {
            mk &lt;- (p1^2 + p1)/(p2^2)
        } else {
            if (k == 3) {
                mk &lt;- (p1^3 + 3 * p1^2 + 2 * p1)/(p2^3)
            } else {
                if (k == 4) {
                  mk &lt;- p1 * (p1 + 1) * (p1 + 2) * (p1 + 3)/(p2^4)
                }
            }
        }
    }
    return(mk)  # k-th raw moment of gamma distribution claims 
}

# returns the moment generating function or its k-th derivative (up to third) for gamma distribution claims
mgfsg &lt;- function(x, k, dparameters) {
    # x: scalar, argument of the moment generating function k: scalar, integer, 0 =&lt; k &lt;= 3, order of the derivative
    # dparameters: list of scalars, parameters of gamma distribution
    p1 &lt;- dparameters[[1]]
    p2 &lt;- dparameters[[2]]
    if (k == 0) {
        y &lt;- p2^p1/((p2 - x)^p1)
    } else {
        if (k == 1) {
            y &lt;- (p1/p2) * (p2/(p2 - x))^(p1 + 1)
        } else {
            if (k == 2) {
                y &lt;- p1 * (p1 + 1) * p2^p1/(p2 - x)^(p1 + 2)
            } else {
                if (k == 3) {
                  y &lt;- p1 * (p1 + 1) * (p1 + 2) * p2^p1/(p2 - x)^(p1 + 3)
                }
            }
        }
    }
    return(y)
}

# moments for gamma distribution claims
mg &lt;- momentsgam(1, dparametersnew)  # 1st raw moment
mg2 &lt;- momentsgam(2, dparametersnew)  # 2nd raw moment
mg3 &lt;- momentsgam(3, dparametersnew)  # 3nd raw moment

# returns the adjustment coefficient R for gamma distribution claims
adjR &lt;- function(theta, dparameters) {
    # theta: security loading in insurance collective risk model dparameters: list of scalars, parameters of gamma distribution
    p1 &lt;- dparameters[[1]]
    p2 &lt;- dparameters[[2]]
    R0 &lt;- 0.99999999 * p2
    R0 &lt;- c(12 * theta * mg/(3 * mg2 + sqrt(9 * mg2^2 + 24 * mg * mg3 * theta)), R0)
    R0 &lt;- min(R0)
    r &lt;- R0
    err &lt;- 1
    while (err &gt; 1e-09) {
        D1 &lt;- 1 + (1 + theta) * mg * r - mgfsg(r, 0, dparameters)
        D2 &lt;- (1 + theta) * mg - mgfsg(r, 1, dparameters)
        err &lt;- r
        r &lt;- r - D1/D2
        err &lt;- abs(err - r)/r
        R &lt;- r
    }
    return(R)  # adjustment coefficient R 
}

R &lt;- adjR(thetanew, dparametersnew)  # adjustment coefficient R for gamma distribution claims
mgfprim &lt;- mgfsg(R, 1, dparametersnew)  # moment generating function for gamma distribution claims 

C &lt;- (thetanew * mg)/(mgfprim - mg * (1 + thetanew))
Cram &lt;- C * exp(-R * u1)  # the Cramer-Lundberg approximation for gamma claims 

u1 &lt;- u1 * p2/p1
b &lt;- 1/p1
n &lt;- length(u1)

# the function to be integrated
exactgamint &lt;- function(x) {
    j &lt;- 1
    while (j &lt; n + 1) {
        uj &lt;- u1[j]
        L &lt;- x^(1/b) * exp(-(x + 1) * uj/b)
        M &lt;- (x^(1/b) * (1 + (1 + thetanew) * (x + 1)/b) - cos(pi/b))^2 + sin(pi/b)^2
        j &lt;- j + 1
    }
    y &lt;- L/M
    return(y)
}

# integrates exactgamint function using the Simpson's method
d &lt;- area(exactgamint, 0, 0.001)
d &lt;- d + area(exactgamint, 0.001, 1)
d &lt;- rbind(1, d)

err &lt;- 1e-05
int &lt;- matrix(1, n)
j &lt;- 1
while (j &lt; n + 1) {
    i &lt;- 2
    while (abs((d[i - 1] - d[i])/d[i]) &gt; err) {
        v &lt;- area(exactgamint, i - 1, i)
        d &lt;- rbind(d, (v + d[i]))
        i &lt;- i + 1
    }
    endd &lt;- length(d)
    int[j] &lt;- d[endd]
    j &lt;- j + 1
}

# the 4-moment gamma De Vylder approximation
psi8 &lt;- Cram + as.vector((thetanew * sin(pi/b)/pi/b) * int)

u1 &lt;- c(seq(0, by = u0/200, length.out = 21), seq(u0/10 + u0/200, by = (u0 - u0/10 - u0/200)/48, length.out = 49))

# the heavy traffic approximation
psi9 &lt;- exp((-2 * theta1 * m * u1)/m2)

pa &lt;- matrix(dparameters1[[1]])  # 2 x 1 matrix of weights
pb &lt;- matrix(dparameters1[[2]])  # 2 x 1 matrix of exponential parameters

paa &lt;- matrix(pa, nrow = 2, ncol = length(u1))
pbb &lt;- matrix(pb, nrow = 2, ncol = length(u1))

d &lt;- rowSums(t(paa/pbb * exp(-pb %*% u1)))
c &lt;- matrix(m, nrow = length(d)) - d

# the light traffic approximation
psi10 &lt;- (m - c)/(1 + theta1)/m

u2 &lt;- (1 - 1/(1 + theta1)) * u1  # capital for the light traffic approximation

paa &lt;- matrix(pa, nrow = 2, ncol = length(u2))
pbb &lt;- matrix(pb, nrow = 2, ncol = length(u2))

d &lt;- rowSums(t(paa/pbb * exp(-pb %*% u2)))
c &lt;- matrix(m, nrow = length(d)) - d

psil &lt;- (m - c)/(1 + theta1)/m  # the new light traffic approximation

# the heavy-light traffic approximation
psi11 &lt;- (1/(1 + theta1)^2) * psi9 + (1 - 1/(1 + theta1)) * psil

u1 &lt;- u1/10^9

# the relative errors of the approximations
err2 &lt;- (psi2 - psi1)/psi1
err3 &lt;- (psi3 - psi1)/psi1
err4 &lt;- (psi4 - psi1)/psi1
err5 &lt;- (psi5 - psi1)/psi1
err6 &lt;- (psi6 - psi1)/psi1
err7 &lt;- (psi7 - psi1)/psi1
err8 &lt;- (psi8 - psi1)/psi1
err9 &lt;- (psi9 - psi1)/psi1
err10 &lt;- (psi10 - psi1)/psi1
err11 &lt;- (psi11 - psi1)/psi1

plot(u1, psi1, type = "l", col = "red", ylim = c(0, 0.801), lwd = 3, main = "", xlab = "u  (USD billion)", ylab = expression(psi(u)), 
    cex.axis = 1.6, cex.lab = 1.6)
dev.new()
plot(u1, err2, type = "l", col = "blue", ylim = c(-0.301, 0.301), lwd = 3, main = "", xlab = "u  (USD billion)", ylab = expression((psi[i](u) - 
    psi(u))/psi(u)), cex.axis = 1.6, cex.lab = 1.6)
lines(u1, err3, col = "brown", lwd = 3, lty = 4)
lines(u1, err5, col = "red", lwd = 3, lty = 3)
lines(u1, err7, col = "black", lwd = 3, lty = 2)
lines(u1, err8, col = "darkgreen", lwd = 3, lty = 5)
dev.new()
plot(u1, err4, type = "l", lty = 4, col = "red", ylim = c(-1.01, 1.01), lwd = 3, main = "", xlab = "u  (USD billion)", ylab = expression((psi[i](u) - 
    psi(u))/psi(u)), cex.axis = 1.6, cex.lab = 1.6)
lines(u1, err6, col = "blue", lwd = 3, lty = 3)
lines(u1, err9, col = "magenta", lwd = 3)
lines(u1, err10, col = "darkgreen", lwd = 3, lty = 5)
lines(u1, err11, col = "brown", lwd = 3, lty = 2)
