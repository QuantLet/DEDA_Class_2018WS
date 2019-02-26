rm(list = ls(all = TRUE))
# setwd('C:/...')

install.packages("abind")
library(abind)

# generates a homogeneous Poisson process with intensity lambda
simHPP &lt;- function(lambda, T, N) {
    # lambda: scalar, intensity of the Poisson process T: scalar, time horizon N: scalar, number of trajectories
    EN &lt;- rpois(N, lambda * T)
    y &lt;- matrix(T, nrow = 2 * max(EN) + 2, ncol = N) * matrix(1, nrow = 2 * max(EN) + 2, ncol = N)
    yy &lt;- abind(y, matrix(1, nrow = 2 * max(EN) + 2, ncol = N) * EN, along = 3)
    i = 1
    while (i &lt;= N) {
        if (EN[i] &gt; 0) {
            yy[1:(2 * EN[i] + 1), i, 1] &lt;- c(0, rep(sort(T * runif(EN[i])), each = 2))
        } else {
            yy[1, i, 1] = 0
        }
        yy[1:(2 * EN[i] + 2), i, 2] &lt;- c(0, floor((1:(2 * EN[i]))/2), EN[i])
        i = i + 1
    }
    return(yy)
}

# generates a non-homogeneous Poisson process with intensity lambda
simNHPP &lt;- function(lambda, parlambda, T, N) {
    # lambda: scalar, intensity function, sine function (lambda=0), linear function (lambda=1) or sine square function
    # (lambda=2) parlambda: n x 1 vector, parameters of the intensity function lambda (n=2 for lambda=1, n=3 otherwise) T:
    # scalar, time horizon N: scalar, number of trajectories
    a &lt;- parlambda[1]
    b &lt;- parlambda[2]
    if (lambda == 0) {
        c &lt;- parlambda[3]
        JM &lt;- simHPP(a + b, T, N)
    } else {
        if (lambda == 1) {
            JM &lt;- simHPP(a + b * T, T, N)
        } else {
            if (lambda == 3) {
                JM &lt;- simHPP(a + b * T, T, N)
            }
        }
    }
    rjm &lt;- nrow(JM)
    yy &lt;- abind(matrix(T, nrow = rjm, ncol = N), matrix(0, nrow = rjm, ncol = N), along = 3)
    i = 1
    maxEN = 0
    while (i &lt;= N) {
        pom &lt;- JM[, i, 1][JM[, i, 1] &lt; T]
        pom &lt;- pom[2 * (1:(length(pom)/2))]
        R &lt;- runif(NROW(pom))
        if (lambda == 0) {
            lambdat &lt;- (a + b * sin(2 * pi * (pom + c)))/(a + b)
        } else {
            if (lambda == 1) {
                lambdat &lt;- (a + b * pom)/(a + b * T)
            } else {
                if (lambda == 3) {
                  lambdat &lt;- (a + b * sin(2 * pi * (pom + c))^2)/(a + b)
                }
            }
        }
        pom &lt;- pom[R &lt; lambdat]
        EN &lt;- NROW(pom)
        maxEN &lt;- max(maxEN, EN)
        yy[1:(2 * EN + 1), i, 1] &lt;- c(0, rep(pom, each = 2))
        yy[2:(2 * EN), i, 2] &lt;- c(floor((1:(2 * EN - 1))/2))
        yy[(2 * EN + 1):rjm, i, 2] &lt;- matrix(EN, nrow = rjm - 2 * EN, ncol = 1)
        i = i + 1
    }
    yy &lt;- yy[1:(2 * maxEN + 2), , ]
    return(yy)
}

# generates aggregate loss process driven by the non-homogeneous Poisson process for lognormal distribution
simNHPPALP &lt;- function(lambda, parlambda, params, T, N) {
    # lambda: scalar, intensity function, sine function (lambda=0), linear function (lambda=1) or sine square function
    # (lambda=2) parlambda: n x 1 vector, parameters of the intensity function lambda (n=2 for lambda=1, n=3 otherwise) params:
    # n x 1 vector, parameters of the lognormal distribution # T: scalar, time horizon N: scalar, number of trajectories
    if (N == 1) {
        poisproc &lt;- simNHPP(lambda, parlambda, T, N)
        poisproc &lt;- abind(matrix(poisproc[, 1]), matrix(poisproc[, 2]), along = 3)
    } else {
        poisproc &lt;- simNHPP(lambda, parlambda, T, N)
    }
    rpp &lt;- nrow(poisproc)
    cpp &lt;- ncol(poisproc)
    losses &lt;- matrix(0, nrow = rpp, ncol = cpp)
    i = 1
    while (i &lt;= N) {
        aux &lt;- min(which(poisproc[, i, 1] == T))
        if (aux &gt; 2) {
            laux &lt;- cumsum(rlnorm(aux/2 - 1, params[1], params[2]))
            losses[3:aux, i] &lt;- rep(laux, each = 2)
        } else {
            if (aux &lt; rpp) {
                losses[(aux + 1):rpp, i] &lt;- laux[NROW(laux)] * matrix(1, nrow = rpp - aux)
            } else {
                losses[, i] &lt;- matrix(0, nrow = rpp)
            }
        }
        i = i + 1
    }
    if (N == 1) {
        y &lt;- abind(poisproc[, , 1], losses)
    } else {
        y &lt;- abind(poisproc[, , 1], losses, along = 3)
    }
    return(y)
}

set.seed(2)

lambda1 &lt;- 0  # intensity 
parlambda1 &lt;- c(35.32, 2.32 * 2 * pi, -0.2)  # parameters of intensity function
params1 &lt;- c(18.3806, 1.1052)  # parameters of the lognormal distribution
T1 &lt;- 10  # time
N1 &lt;- 1  # trajectories
N2 &lt;- 100

y1 &lt;- simNHPPALP(lambda1, parlambda1, params1, T1, N1)  # aggregate loss process for lognormal distribution
y1[, 2] &lt;- y1[, 2]/1e+09

x &lt;- read.table("ncl.dat")

t &lt;- rep(x[, 2], each = 2)
t1 &lt;- c(0, t, T1)
PCS &lt;- cumsum(x[, 3])/1e+09
PCS1 &lt;- rep(PCS, each = 2)
PCS1 &lt;- c(0, 0, PCS1)
z &lt;- cbind(t1, PCS1)

# mean of aggregate loss process
t2 &lt;- (0:(100 * T1))/100
RP &lt;- exp(params1[1] + params1[2]^2/2) * (parlambda1[1] * t2 - parlambda1[2]/2/pi * (cos(2 * pi * (t2 + parlambda1[3])) - cos(2 * 
    pi * parlambda1[3])))
me &lt;- cbind(t2, RP/1e+09)

# computes quantiles of trajectories
quantilelines &lt;- function(data, step, perc) {
    # data: n x m x 2 array, data, where n is the length of trajectories and m the number of trajectories step: scalar, time
    # interval between points at which the quantiles are computed perc: s x 1 vector, orders of quantiles
    N &lt;- ncol(data)
    R &lt;- nrow(data)
    begin &lt;- data[1, 1, 1]
    theend &lt;- data[R, 1, 1]
    numofpoints &lt;- (theend - begin)/step + 1
    vecstep &lt;- seq(begin, theend, step)
    y &lt;- matrix(0, nrow = 1, ncol = NROW(perc))
    i = 1
    while (i &lt;= numofpoints) {
        j = 1
        vecval = 0
        while (j &lt;= N) {
            aux1 = data[, j, 1]
            aux2 = data[, j, 2]
            pos = sum(aux1 &lt;= vecstep[i])
            if (pos &lt; R) {
                vecval &lt;- c(vecval, aux2[pos] + (vecstep[i] - aux1[pos]) * (aux2[pos + 1] - aux2[pos])/(aux1[pos + 1] - aux1[pos]))
            } else {
                vecval &lt;- c(vecval, aux2[pos])
            }
            j = j + 1
        }
        y &lt;- rbind(y, quantile(vecval[2:(N + 1)], perc))
        i = i + 1
    }
    y = cbind(vecstep, y[2:(numofpoints + 1), ])
}

step1 &lt;- 0.05  #time
perc1 &lt;- c(0.05, 0.95)  #quantiles

# quantiles of trajectories
qq1 &lt;- quantilelines(simNHPPALP(lambda1, parlambda1, params1, T1, N2), step1, perc1[1])  # 0.05 quantile of trajectories
qq2 &lt;- quantilelines(simNHPPALP(lambda1, parlambda1, params1, T1, N2), step1, perc1[2])  # 0.95 quantile of trajectories

qq1 &lt;- cbind(qq1[, 1], qq1[, 2]/1e+09)
qq2 &lt;- cbind(qq2[, 1], qq2[, 2]/1e+09)

plot(y1, type = "l", col = "blue", ylim = c(0, 120), xlab = "Years", ylab = "Aggregate loss process (USD billion)", cex.lab = 1.4, 
    cex.axis = 1.4, lwd = 2)
lines(me, col = "red", lty = 2, lwd = 2)
lines(z, col = "green", lwd = 3)
lines(qq1[1:200, ], col = "brown", lty = 3, lwd = 2)
lines(qq2[1:200, ], col = "brown", lty = 3, lwd = 2)
abline(h = 60, lwd = 3)
