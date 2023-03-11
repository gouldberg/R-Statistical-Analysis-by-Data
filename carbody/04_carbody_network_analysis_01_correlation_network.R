
setwd("C:\\Users\\kswad\\OneDrive\\デスクトップ\\技術力強化_統計解析\\51_解析スクリプト\\carbody")

packages <- c("dplyr")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  car body coating process data
# ------------------------------------------------------------------------------

dat <- read.csv("carbody_dat.txt", header = TRUE, skip = 2, sep = "\t")


str(dat)


car::some(dat)




# ----------
dat_s <- data.frame(scale(dat))


head(dat_s)




# ------------------------------------------------------------------------------
# Correlation network by qgraph:  simple pearson correlation network
# ------------------------------------------------------------------------------


dat_s2 <- dat_s %>% dplyr::select(-centmaku, -makuwid)


cormat <- cor(dat_ss)


library("qgraph")



# correnation network (no directed)
# minimum = 0.2:  we set the lower absolute correlation threshold to 0.2 (i.e. small-medium effect size)
# Edges with negative correaltions are colored differently

cornet <- qgraph(cormat, layout = "spring", minimum = 0.2,
                 graph = "cor",
                 color = c("white", "gray"), labels = colnames(dat_s2))



# ----------
cornet$Edgelist


ed <- cornet$graphAttributes$Nodes$names



# ----------
# show edge weitht between from - to
df <- data.frame(bind_rows(cornet$Edgelist))

df$from <- ed[match(df$from, 1:length(ed))]

df$to <- ed[match(df$to, 1:length(ed))]


head(df)


df %>% filter(from == "toryotemp", to == "temp")

df %>% filter(from == "toryotemp", to == "tochaku")

df %>% filter(from == "fukituke", to == "tochaku")




# ----------
# degree centrality (strength):  number of edges connected with a node
centralityPlot(cornet)





# ------------------------------------------------------------------------------
# Correlation network by qgraph:  partial correlation
#   - Instead of using simple Pearson correlations, we use partial correlations
#     A partial correlation involving two nodes controls for the influence of all the remaining nodes in the network
#     Thus, the inferred edges are more reflective of direct influence among nodes
# ------------------------------------------------------------------------------

# set lower minimum to 0.1
# graph = "pcor"

pcornet <- qgraph(cormat, layout = "spring", minimum = 0.1,
                    graph = "pcor",
                    color = c("white", "gray"), labels = colnames(dat_s2))



# ----------
ed_p <- pcornet$graphAttributes$Nodes$names



# ----------
# show edge weitht between from - to
df_p <- data.frame(bind_rows(pcornet$Edgelist))

df_p$from <- ed_p[match(df_p$from, 1:length(ed_p))]

df_p$to <- ed_p[match(df_p$to, 1:length(ed_p))]


head(df_p)


df %>% filter(from == "toryotemp", to == "temp")
df_p %>% filter(from == "toryotemp", to == "temp")


df %>% filter(from == "toryotemp", to == "tochaku")
df_p %>% filter(from == "toryotemp", to == "tochaku")


df %>% filter(from == "fukituke", to == "tochaku")
df_p %>% filter(from == "fukituke", to == "tochaku")



# ----------
# degree centrality (strength):  number of edges connected with a node
centralityPlot(pcornet)




# ------------------------------------------------------------------------------
# Correlation network by qgraph:  graphical lasso
#   - Instead of using a somewhat arbitrary correlation threshold or a Bonferroni corrected significance level,
#     a graphical lasso can be considered.
#     The idea of the graphical lasso is to shrink low correaltion to zero such that they disappear from the graph.
# ------------------------------------------------------------------------------


#glassonet <- qgraph(cormat, layout = "spring", sampleSize = nrow(dat_s2),
#                 graph = "glasso",
#                 color = c("white", "gray"), labels = colnames(dat_s2),
#                 lambda.min.ratio = 0.5)



# if we do not set any tuning parameter, producing errors (not estimated) ....
# gamma = 0:  setting to zero will cause regular BIC to be used
glassonet <- qgraph(cormat, layout = "spring", sampleSize = nrow(dat_s2),
                    graph = "glasso",
                    color = c("white", "gray"), labels = colnames(dat_s2),
                    gamma = 0)



# ----------
ed_g <- glassonet$graphAttributes$Nodes$names



# ----------
# show edge weitht between from - to
df_g <- data.frame(bind_rows(glassonet$Edgelist))

df_g$from <- ed[match(df_g$from, 1:length(ed_g))]

df_g$to <- ed[match(df_g$to, 1:length(ed_g))]


head(df_g)


df %>% filter(from == "fukituke", to == "tochaku")

df_p %>% filter(from == "fukituke", to == "tochaku")

df_g %>% filter(from == "fukituke", to == "tochaku")




# ----------
# degree centrality (strength):  number of edges connected with a node
centralityPlot(glassonet)




