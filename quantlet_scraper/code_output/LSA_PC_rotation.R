# Clear all variables
rm(list = ls(all = TRUE))
graphics.off()

# Install and load packages
libraries = c("lsa")
lapply(libraries, function(x) if (!(x %in% installed.packages())) {
  install.packages(x)
})
lapply(libraries, library, quietly = TRUE, character.only = TRUE)


(obj.names = load("TDM_Q3D3LSA.RData", .GlobalEnv))


# LSA space creation

system.time( LSA_space &lt;- lsa(m_a) )
summary(LSA_space)

# ------------------------------------------------
# LSA Extended / Interpretation
# ------------------------------------------------

LSA.PC.rotation = function(LSA_space, auto_rotation = TRUE, n = 10, top_terms = 5, weight_round = 2){
	pc_labels = c()
	sv_weights = c()
	
	U   = LSA_space$tk
	sv  = LSA_space$sk

	for (i in 1:n) {
		PC = U[,i]
		
		if (auto_rotation) {
			if (sum(PC[PC &gt; 0]) + sum(PC[PC &lt; 0]) &gt;= 0) {
				res = PC[PC &gt; 0]
			} else {
				res = -PC[PC &lt; 0]
			}
		} else {
			res = PC[PC &gt; 0]
		}
		
		res = sort(res, decreasing = TRUE)[1:top_terms] * sv[i]
		res = round(res, weight_round)
		
		SV_weight = round(sv[i], weight_round)
		
		pc_labels = c(pc_labels, list(res))
		sv_weights = c(sv_weights, SV_weight)
	}

	#return(pc_labels)
	return(list(PC_labels = pc_labels, SV_weights = sv_weights))
}


LSA_PC = LSA.PC.rotation(LSA_space)

LSA_PC = LSA.PC.rotation(LSA_space, n = 8)

LSA_PC = LSA.PC.rotation(LSA_space, auto_rotation = F)


str(LSA_PC)

LSA_PC$PC_labels
LSA_PC$SV_weights
