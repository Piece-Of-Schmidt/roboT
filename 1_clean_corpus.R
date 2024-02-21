require(tosca)

# set wd
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# load functions
source("0_functions.R")

corpus = readRDS("data/example_corpus.rds")
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

most_frquent_titles = sort(table(corpus$meta$title), decreasing=T)
most_frquent_titles[1:20]
# -> zeigt an, welches die haeufigsten Dokumenten-Titel sind. Oft Rubriken, manchmal Leserbriefe etc.
# -> Oft ist es ratsam, solche Dokumente von der Analyse auszuschliessen

pattern = "Life|Death" # Begriffe (Patternsuche), durch "|" ("oder") getrennt
filterTitles(corpus, pattern, print=T)

# Doks ausschließen, bei denen <pattern> im Title vorkommt
corpus_without_Life_or_Death = filterTitles(corpus, pattern)



# sonst... ----------------------------------------------------------------



# -------------------------------------------------------------------------
# Corpus auf Artikel einschraenken, in denen ein bestimmtes Wort/Pattern vorkommt
# -------------------------------------------------------------------------


# Auf Grundlage des Vorkommens bestimmter Worte
corpus = filterWord(corpus, "America") # -> Wichtig: per Default fuehrt die Funktion sog. Pattern Searches aus. D.h., es werden auch solche Dokumente gefunden, in denen (Beispiel hier) "American Dream" vorkommt 
corpus_America = filterWord(corpus, "\\America\\b") # -> Um das zu umgehen: "\\b" stellt sicher, dass "America" zu beiden Seiten von einer Word Boundary (Leer- oder Satzzeichen) umgeben ist

# Die Funktion filterWord() versteht zu einem gewissen Teil regular expressions
# Es sind also auch Ausdruecke wie der Folgende moeglich:
corpus_America = filterWord(corpus, "\\b(America|USA|United States)\\b", ignore.case=T)

# -> "|" bedeutet "oder", "\\b" bedeutet "word boundary"
# -> "ignore.case=T" stellt sicher, dass Gross- und Kleinschreibung ignoriert werden. Wenn das nicht gewuenscht ist, Parameter auf F (FALSE) setzen
# -> Wichtig: "ignore.case=T" transformiert alle Texte automatisch in Kleinbuchstaben. Wenn das vermieden werden soll, ist der folgende Code zu empfehlen:

mask = filterWord(corpus, "\\b(America|USA|United States)\\b", ignore.case=T, out="bin") # -> out="bin" gibt keinen Korpus, sondern einen booleschen Vektor zurueck
relevant_ids = names(corpus$text)[mask]
corpus_America = filterID(corpus, relevant_ids)

# Fuer weitere Moeglichkeiten der Korpus-Einschraenkung (inkl. Einstellung, dass einzelne Worte mehrfach vorkommen sollen):
# Siehe Koppers et al. (2021) tosca Vignette


# -------------------------------------------------------------------------
# Corpus speichern
# -------------------------------------------------------------------------

obj_to_save = corpus_America # hier auf das Objekt verweisen, das lokal gespeichert werden soll
saveRDS(obj_to_save, "data/corpus_edited.rds")
