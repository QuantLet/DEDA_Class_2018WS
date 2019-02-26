# required packages
library(class)
library(mclust)
library(MASS)
library(kernlab)

# read in the data after doing dimension reduction
faces1 = read.table("newfaces1.txt")

# split the data in test and trainings set (first nine pictures = train, last picture = test)
id = rep(1:40, each = 10)
faces.data.frame = data.frame(cbind(id = id, faces1))

testid = seq(10, 400, by = 10)
train.faces = faces.data.frame[-testid, ]
test.faces = faces.data.frame[testid, ]


# FLDA model
flda = lda(id ~ ., train.faces)
# prediction
pl = predict(flda, test.faces)$class

# accuracy measurement
table(pl)
diag(table(pl, id[testid]))
classError(pl, id[testid])
# calculation of running time
ptm = proc.time()
pl = predict(flda, test.faces)$class
proc.time() - ptm
