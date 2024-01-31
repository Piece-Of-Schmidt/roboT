require(tosca)

# set wd
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# load functions
source("0 functions.R")


corpus = filterDate(corpus, "2001-01-01", "2024-01-01") # falls nur ein bestimmter Zeitabschnitt gewuenscht ist
corpus = clean_complete(corpus) # fuehrt ein Standar-Cleaning fuer den Korpus durch


# -------------------------------------------------------------------------
# teilautomatisches cleanen
# -------------------------------------------------------------------------


require(reshape2)
require(ggplot2)


# check resources ---------------------------------------------------------

# show sources:
table(corpus$meta$resource)

# plot sources:
plotSources(corpus, alpha=0.6, span=0.5)

# Quellen beibehalten:
# quellen_list = c() # hier einen Vektor anlegen, der alle Quellennamen beinhaltet, die behalten werden sollen
corpus = filterID(corpus, corpus$meta$id[corpus$meta$resource %in% quellen_list])



# check titles ------------------------------------------------------------

most_frquent_titles = table(corpus$meta$title)[1:20]
most_frquent_titles

pattern = "Johnson" # Begriffe (Patternsuche), durch "|" getrennt
filterTitles(corpus, pattern, print=T)

# Doks ausschließen, bei denen <pattern> im Title vorkommt
corpus = filterTitles(corpus, pattern)



# sonst... ----------------------------------------------------------------


