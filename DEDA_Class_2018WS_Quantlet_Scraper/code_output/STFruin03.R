rm(list = ls(all = TRUE))
# setwd('C:/...')

# install.packages('MASS')
library(MASS)

# returns the k-th moment (up to fourth) of the mixture of 2 gamma distribution claims
moments &lt;- function(k, dparameters) {
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
mgfs &lt;- function(x, k, dparameters) {
    # x: scalar, n x 1 vector or m x n matrix, argument of the moment generating function k: scalar, integer, 0 =&lt; k &lt;= 3, order
    # of the derivative dparameters: list of scalars, parameters of gamma distribution
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

u1 &lt;- c(0, 10^9, 2 * 10^9, 3 * 10^9, 4 * 10^9, 5 * 10^9)  # initial capital of insurance company (in USD)
theta1 &lt;- 0.3  # relative safety loading
dparameters1 &lt;- list(0.9185, 6.1662e-09)  # gamma parameters

m &lt;- moments(1, dparameters1)  # 1st raw moment
m2 &lt;- moments(2, dparameters1)  # 2nd raw moment
m3 &lt;- moments(3, dparameters1)  # 3nd raw moment

# returns the adjustment coefficient R for gamma distribution claims
adjR &lt;- function(theta, dparameters) {
    # theta: security loading in insurance collective risk model dparameters: list of scalars, parameters of gamma distribution
    p1 &lt;- dparameters[[1]]
    p2 &lt;- dparameters[[2]]
    R0 &lt;- 0.99999999 * p2
    R0 &lt;- c(12 * theta * m/(3 * m2 + sqrt(9 * m2^2 + 24 * m * m3 * theta)), R0)
    R0 &lt;- min(R0)
    r &lt;- R0
    err &lt;- 1
    while (err &gt; 1e-09) {
        D1 &lt;- 1 + (1 + theta) * m * r - mgfs(r, 0, dparameters)
        D2 &lt;- (1 + theta) * m - mgfs(r, 1, dparameters)
        err &lt;- r
        r &lt;- r - D1/D2
        err &lt;- abs(err - r)/r
        R &lt;- r
    }
    return(R)  # adjustment coefficient R 
}

R &lt;- adjR(theta1, dparameters1)  # adjustment coefficient R for gamma distribution claims
mgfprim &lt;- mgfs(R, 1, dparameters1)  # moment generating function for gamma distribution claims 

C &lt;- (theta1 * m)/(mgfprim - m * (1 + theta1))
Cram &lt;- C * exp(-R * u1)  # the Cramer-Lundberg approximation for gamma claims 

p1 &lt;- dparameters1[[1]]
p2 &lt;- dparameters1[[2]]
u1 &lt;- u1 * p2/p1
b &lt;- 1/p1
n &lt;- length(u1)

# the function to be integrated
exactgamint &lt;- function(x) {
    j &lt;- 1
    while (j &lt; n + 1) {
        uj &lt;- u1[j]
        L &lt;- x^(1/b) * exp(-(x + 1) * uj/b)
        M &lt;- (x^(1/b) * (1 + (1 + theta1) * (x + 1)/b) - cos(pi/b))^2 + sin(pi/b)^2
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

# the ruin probability for gamma claims with alpha=0.9185, beta=6.1662e-09 and theta=0.3 (u in USD)
psi &lt;- Cram + as.vector((theta1 * sin(pi/b)/pi/b) * int)
psi
