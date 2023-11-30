require(tosca)

# set wd
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# falls nur ein bestimmter Zeitabschnitt gewuenscht ist
corpus = filterDate(corpus, "2001-01-01", "2024-01-01") 


# -------------------------------------------------------------------------
# automatisches cleanen
# -------------------------------------------------------------------------

hbweltsz = F
# wenn der Korpus der HB-Welt-SZ-Korpus ist, den Param auf T setzen

utf8 = T
# kann sehr lange dauern

check_first_chars = 120
# relevant fuer filterDups_leads. Auf Basis wie vieler Characters sollen die Dokumente verglichen werden?

min_nchar = 750
# Massgabe: Jeder Text soll mindestens eine halbe Normseite lange
# eine Normseite: 30 Zeilen*60 Anschlaege/chars = 1800 Chars. Von der VG Wort vereinfacht als Normseite bezeichnet: 1500 Zeichen
# Halbe Normseite: 750 Chars

cutoff = 0.99
max_nchar = quantile(nchar(corpus$text), cutoff)

# run script
source("clean_blind.R")



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
quellen_list = c()
corpus = filterID(corpus, corpus$meta$id[corpus$meta$resource %in% quellen_list])



# check titles ------------------------------------------------------------

most_frquent_titles = table(corpus$meta$title)[1:20]
most_frquent_titles

pattern = "Johnson" # Begriffe (Patternsuche), durch "|" getrennt
filterTitles(corpus, pattern, print=T)

# Doks ausschließen, bei denen <pattern> im Title vorkommt
corpus = filterTitles(corpus, pattern)



# sonst... ----------------------------------------------------------------


