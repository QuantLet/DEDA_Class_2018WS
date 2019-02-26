# clear history
rm(list = ls(all = TRUE))
graphics.off()

n     = 100
x     = numeric(n)              #create empty vector to hold result
x[1]  = 0                       #set first entry to 0 (is already 0)

for (i in 2:n) {
    e    = sample(c(-1,1), 1)
    x[i] = x[i-1] + e           #draw the others (2nd onwards) using the conditional distribution 
}

plot(x, type="o", pch=16)       #visualize the results
