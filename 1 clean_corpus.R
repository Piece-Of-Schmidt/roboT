require(tosca)

# set wd
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# load functions
source("0 functions.R")

corpus = readRDS("example_corpus.rds")
corpus

# filter by date
corpus = filterDate(corpus, "1975-01-01", "1995-01-01") # falls nur ein bestimmter Zeitabschnitt gewuenscht ist

# clean corpus automatically
corpus = clean_complete(corpus) # fuehrt ein Standard-Cleaning fuer den Korpus durch


# -------------------------------------------------------------------------
# teilautomatisches cleanen
# -------------------------------------------------------------------------


# check resources ---------------------------------------------------------

# show sources:
table(corpus$meta$resource)

# plot sources:
plotSources(corpus, unit="year", alpha=0.6, smooth=F)

# Quellen beibehalten:
quellen_list = c("wikipedia") # hier einen Vektor anlegen, der alle Quellennamen beinhaltet, die behalten werden sollen
corpus = filterID(corpus, corpus$meta$id[corpus$meta$resource %in% quellen_list])



# check titles ------------------------------------------------------------

most_frquent_titles = table(corpus$meta$title)[1:20]
most_frquent_titles

pattern = "Life" # Begriffe (Patternsuche), durch "|" getrennt
filterTitles(corpus, pattern, print=T)

# Doks ausschließen, bei denen <pattern> im Title vorkommt
corpus = filterTitles(corpus, pattern)



# sonst... ----------------------------------------------------------------


