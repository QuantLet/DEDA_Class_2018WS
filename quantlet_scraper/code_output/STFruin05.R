rm(list = ls(all = TRUE))
# setwd('C:/...')

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

u &lt;- c(0, 10^9, 5 * 10^9, 10^10, 2 * 10^10, 5 * 10^10)  # initial capital of insurance company (in USD)
theta1 &lt;- 0.3  # relative safety loading
dparameters1 &lt;- list(c(0.0584, 0.9416), c(3.59e-10, 7.5088e-09))  # weights (first vector) and exponential parameters (second vector)

m &lt;- moments(1, dparameters1)  # 1st raw moment
m2 &lt;- moments(2, dparameters1)  # 2nd raw moment
m3 &lt;- moments(3, dparameters1)  # 3nd raw moment

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

R &lt;- adjR(theta1, dparameters1)  # adjustment coefficient R for mixture of 2 exponentials distribution claims
mgfprim &lt;- mgfs(R, 1, dparameters1)  # moment generating function for mixture of 2 exponentials distribution claims 

C &lt;- (theta1 * m)/(mgfprim - m * (1 + theta1))

# the Cramer-Lundberg approximation for mixture of 2 exponentials claims with \fbeta1=3.5900e-10, beta2=7.5088e-09,
# alpha=0.0584 and theta=0.3 (u in USD)
psi &lt;- C * exp(-R * u)
psi
