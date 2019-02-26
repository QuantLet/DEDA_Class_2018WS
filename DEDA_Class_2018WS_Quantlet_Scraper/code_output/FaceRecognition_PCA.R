# read in the data
faces = read.table("newfaces1.txt")
id &lt;- rep(1:40, each = 10)
faces.data.frame &lt;- data.frame(cbind(id = id, faces))

# data splitting
id &lt;- rep(1:40, each = 10)
testid &lt;- seq(10, 400, by = 10)

train.faces &lt;- faces.data.frame[-testid, ]
test.faces &lt;- faces.data.frame[testid, ]

# since train.faces is 360x4094 matrix, it is not efficient to directly compute the eignvalues and eignvecters
# for a size of 4096*4096 matrix but fortunately,
xc &lt;- scale(train.faces[, -1], scale = FALSE)
A &lt;- t(xc)/sqrt(360 - 1)  # 4096x360
# thus , the covariance matrix is A%*%t(A)
A.egn &lt;- eigen(t(A) %*% A)  # 360x360
# thus, the covariance matrix is A%*%t(A)
pc &lt;- A %*% A.egn$vectors
pc &lt;- apply(pc, 2, function(i) i/sqrt(sum(i * i)))  # normalize the pc
n &lt;- 80
sum(A.egn$value[1:n])/sum(A.egn$value)
# 92.08%
pcs &lt;- pc[, 1:n]
yt &lt;- xc %*% pcs  # pc scores for training data

ft &lt;- data.frame(cbind(id[-testid], yt))  #training set
# test group
xv &lt;- as.matrix(test.faces[, -1])
xvs &lt;- scale(xv, scale = FALSE)
yv &lt;- xvs %*% pcs  # pc scores for testing data
fv &lt;- data.frame(cbind(id[testid], yv))  #test set
