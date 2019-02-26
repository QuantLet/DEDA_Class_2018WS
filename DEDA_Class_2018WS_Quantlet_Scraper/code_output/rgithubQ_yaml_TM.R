library(rgithubQ)
library(yamldebugger)
library(TManalyzerQ)

# MD output
library(knitr)

# setwd(workdir)

# GitHub's user authorization
ctx = interactive.login("XXX", "YYY")

q_search = 'Quantlet Published Description Keywords Author filename:"metainfo.txt"'

# github search 1
spec_search_term = "yaml user:Quantlet user:lborke user:b2net"
sr = search.code(paste(spec_search_term, q_search), per_page = 20)
# github search 2
spec_search_term = "black scholes user:Quantlet"
sr = search.code(paste(spec_search_term, q_search), per_page = 100)

sr$content$total_count
length(sr$content$items)

# yaml_list = yaml.parser.light(sr, print_item = TRUE)
yaml_list = yaml.parser.light(sr, print_item = FALSE, error_to_results = FALSE)

# Meta Configuration I
t_vec = yaml.list.extract(yaml_list, weight = c(q=1, d=1, k=1, p=1))
# Meta Configuration III: smart
t_vec = yaml.list.extract(yaml_list, weight = c(d=6, k=10, sa=3, a=4, df=5, e=4))

# github search 1
system.time( tm_list &lt;- tm.create.models(t_vec, lsa_dim = 2) )
# github search 2
system.time( tm_list &lt;- tm.create.models(t_vec, lsa_dim = 6) )

# queries for github search 1
query = c("document term matrix", "lsa", "Text Mining")
# queries for github search 2
query = c("european option", "implied volatility", "black scholes")

### show available proximities from the proxy package (is loaded within the TManalyzerQ package)
library(proxy)
summary(pr_DB)

## TM-IR section
# transform the queries into the TM models
query.tm.folded = query.tm.fold_in(query, tm_list, tf_weight = "nnc")
# apply the cosine similarity as default
q_tdm_sim.tm_res = q_tdm_sim.tm.list(query.tm.folded)
# some other similarities
# q_tdm_sim.tm_res = q_tdm_sim.tm.list(query.tm.folded, sim_meth = "Chi-squared")
# q_tdm_sim.tm_res = q_tdm_sim.tm.list(query.tm.folded, sim_meth = "eJaccard")

# 3 threshold levels for IR
q_ir_list = query.similar.doc.inspect(q_tdm_sim.tm_res, sim_threshold = 0.8)
(m1 = q_ir_list$retrieved_m)
q_ir_list = query.similar.doc.inspect(q_tdm_sim.tm_res, sim_threshold = 0.7)
(m2 = q_ir_list$retrieved_m)
q_ir_list = query.similar.doc.inspect(q_tdm_sim.tm_res, sim_threshold = 0.6)
(m3 = q_ir_list$retrieved_m)

# github search 1
colnames(m1) = colnames(m2) = colnames(m3) = c("BVSM", "GVSM(TT)", "LSA", "LSA2")
# github search 2
colnames(m1) = colnames(m2) = colnames(m3) = c("BVSM", "GVSM(TT)", "LSA", "LSA6")

# full output
( m_full = cbind(m1, m2, m3) )

# queries as TDM representation
query.tm.folded$q_tdm

## retrieved Qs for each query
# as list
q_ir_list$query_tm_list
# sorted by TM models and similarities
q_ir_list$query_tm_text


## MD output
( k_t = kable(m_full) )

writeLines(k_t, con = "rgithubQ_TM_search_1.md")

writeLines(k_t, con = "rgithubQ_TM_search_2.md")
