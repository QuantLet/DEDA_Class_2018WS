# clear history
rm(list = ls(all = TRUE))
graphics.off()

x = 1                         #set initial value
for (i in 1:50) {             #repeat 
    old.x = x                 #store old value
    x     = 1 + 1/x           #update x
    if (abs(old.x-x)&lt;10e-10)  #check for convergence
        break
}

x                             #print result
(1+sqrt(5)) / 2               #compare to desired answer
