# Clear all variables
rm(list = ls(all = TRUE))
graphics.off()

# Install and load packages
libraries = c("lsa", "ggplot2", "plyr")
lapply(libraries, function(x) if (!(x %in% installed.packages())) {
  install.packages(x)
})
lapply(libraries, library, quietly = TRUE, character.only = TRUE)


(obj.names = load("TDM_Q3D3LSA.RData", .GlobalEnv))


# LSA space creation

system.time( LSA_space &lt;- lsa(m_a) )
summary(LSA_space)

# Tests and experiments with SVD
U   = LSA_space$tk
V   = LSA_space$dk
sv  = LSA_space$sk

dim(m_a)
dim(U)
dim(V)
length(sv)

# Check that the rows of the right matrix of SVD (rows of t(V) = Docs Coefficients to a PC) are normalized to 1
rowSums(t(V)^2)

# Check that columnns of U are normalized to 1
colSums(U^2)


#------------------------------------------------
# Plot the highest singular values having a total sum of 50%: a default parametrization given by LSA_space &lt;- lsa(m_a)
#------------------------------------------------

# Effect of the singular values i.e. their weights

x_axis = 1:length(LSA_space$sk)

#qp &lt;- qplot(x_axis, LSA_space$sk, xlab = "", ylab = "singular values")

qp &lt;- qplot(x_axis, LSA_space$sk, xlab = "The highest singular values having a total sum of 50%", ylab = "singular values")
qp + theme(axis.text = element_text(size = 17), axis.title = element_text(size = 20))


#------------------------------------------------
# TDK - Histogram of the matrix values in TDM (basis VSM)
#------------------------------------------------

# convert the TDM to a vector
m_v = as.vector(m_a)

summary(m_v)

# Sparsity of TDM (basis VSM)
length(m_v[m_v &gt; 0])
length(m_v)
length(m_v[m_v &gt; 0]) / length(m_v)
length(m_v[m_v == 0]) / length(m_v)


# collect all data in a data frame for ggplot representation
datTDM &lt;- data.frame(
	Matrices = factor(rep(c("tdm"), each = length(m_v))),
	Values = c(m_v)
)

cdatTDM &lt;- ddply(datTDM, "Matrices", summarise, Values.mean=mean(Values), Values.median=median(Values))
cdatTDM

# ggplot
dev.new(width = 16, height = 13)

ggplot(datTDM, aes(x = Values, fill = Matrices)) + coord_cartesian(xlim = c(0,1), ylim = c(0,20)) + 
	geom_histogram(binwidth = .0001, alpha = .9, position = "identity", colour = "red") + 
	geom_vline(data = cdatTDM, aes(xintercept = Values.mean), linetype = "dashed", size = 2, colour = "darkred") +
	theme(axis.text = element_text(size = 35), axis.title = element_text(size = 40)) +
	theme(legend.title = element_text(size = 20, face = "bold"), legend.text = element_text(size=20))
