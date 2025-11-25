
# -------------------------------------------------------------------------
# preparation
# -------------------------------------------------------------------------

# Paket laden
library(tosca)
library(roboT)

# Datensatz einladen
corp = example_corpus



# -------------------------------------------------------------------------
# simple Korpus-Analysen
# -------------------------------------------------------------------------

# Anz. Artikel im Zeitverlauf
plotScot(corp)

# Anz. Artikel im Zeitverlauf, in denen ein best. Suchwort vorkommt
worte = c("America", "World", "Love")
plotFreq(corp, wordlist = worte)



# -------------------------------------------------------------------------
# Korpus fuer LDA vorbereiten
# -------------------------------------------------------------------------

# Korpus tokenisieren
stopwords = stop_words_en # stop_words_de fuer deutsche Liste, etc.
tokenizedCorpus = cleanTexts(object=corp, sw=stopwords)

# kurze Dokumente entfernen
min_length = 10 # mind. 10 Worte
tokenizedCorpus = filterCount(tokenizedCorpus, count = min_length)

saveRDS(tokenizedCorpus, "tokenizedCorpus.rds") # -> Objekt speichern. Das brauchen wir spaeter noch bei der Auswertung

# Vokabular extrahieren
wordlist = makeWordlist(tokenizedCorpus$text)
vocab = wordlist$words

# optional: rare words rauswerfen
min_occurence = 4
vocab = wordlist$words[wordlist$wordtable >= min_occurence]

# Texte in numerische Repraesentation ueberfuehren
docs = LDAprep(tokenizedCorpus$text, vocab)
saveRDS(docs, "docs.rds") # -> Objekt speichern. Das brauchen wir spaeter noch bei der Auswertung



# -------------------------------------------------------------------------
# LDA rechnen und auswerten
# -------------------------------------------------------------------------

K_WERTE = c(8, 10) # <- probieren Sie hier verschiedene K-Werte aus, z.B. K_WERTE = c(4, 6, 8) oder K_WERTE = seq(10, 20, 2), oder K_WERTE = seq(10, 60, 10), ...

# define seed for LDA
seed = 123 # arbitrary choice

# select sample dates for top texts per quarter <- an Korpus-Größe (min-Date, max-Date) anpassen
if (FALSE){ # set to TRUE and select relevant dates, if you wish to generate top texts per month/quarter/year only for selected dates
  rel_dates = as.Date(c("2001-04-01", "2005-06-01", "2010-07-01", "2013-10-01", "2016-05-01", "2019-06-01", "2022-08-01", "2025-01-01")) # arbitrary selection
} else rel_dates = NULL

# loop through all k values
for(k in K_WERTE){
  
  message("Generate results for K=", k)

  # create subfolder
  if(! dir.exists(paste0("K",k))) dir.create(paste0("K",k))
  
  # save LDA-result to R environment
  result = LDAgen(docs, K=k, vocab, folder = paste0("K",k,"/Ergebnis"), seed=seed)
  assign(paste0("result_K",k), result)
  
  # save LDA-result locally
  saveRDS(result, sprintf("K%d/result_K%d_seed%d.rds", k, k, seed))
  
  # create Top Words
  lda_getTopWords(result, file = sprintf("K%d/TopWords_K%d_seed%d", k, k, seed))
  
  # create Top Texts
  lda_getTopTexts(corp, result, ldaID = names(docs), file = sprintf("K%d/TopTexts_K%d_seed%d", k, k, seed))

  # extract top words per quarter 
  lda_getTopWordsPerUnit(corpus = corp, ldaresult = result, docs = docs, unit = "year", file = sprintf("K%d/TopWordsPerQuarter_k%s_seed%s", k, k, seed))

  # extract top texts per quarter -> most relevant docs per quarter per topic
  lda_getTopTextsPerUnit(corpus = corp, ldaresult = result, ldaID = names(docs), unit = "year", groupby = "topic", select_dates = rel_dates, foldername = sprintf("K%d/TopTextsPerQuarter", k))

  # plot topics
  plotTopic(object = tokenizedCorpus, ldaresult = result, ldaID = names(docs), rel = FALSE, curves = "both", smooth = 0.1, unit = "quarter", legend = "topleft", pages = TRUE, file = sprintf("K%d/topics_over_time_K%d_seed%d.pdf", k, k, seed))

}; message("files are stored in: ", getwd())



# -------------------------------------------------------------------------
# LDA auswerten (Auswahl)
# -------------------------------------------------------------------------


# Weitere Funktionen fuer die Auswertung: Siehe Koppers et al. (2021) tosca Vignette


# print TopWords along with Doc Titles (e.g. for K=8)
get_tw_and_titles(corpus = corp, ldaresult = result_K8, ldaID = names(docs), n = 20)


# um die Plots lokal zu speichern (e.g. for K=8)
plot = plotTopic(object = tokenizedCorpus, ldaresult = result_K8, ldaID = names(docs),
                 select = c(3,4), rel = TRUE, curves = "both", smooth = 0.1, legend = "topleft")
write.csv2(plot, "plot.csv")



# -------------------------------------------------------------------------
# LDA-Ergebnis nach Quelle und Jahr zerlegen
# -------------------------------------------------------------------------

library(lubridate)

# Dokument-IDs aus dem LDA-Objekt holen
doc_ids = names(docs)

# Metadaten der entsprechenden Dokumente aus dem Korpus holen
# (so stellen wir sicher, dass Reihenfolge zu doc_ids passt)
doc_meta = corp$meta[match(doc_ids, corp$meta$id), ]

# Quelle und Jahr bestimmen
doc_sources = doc_meta$resource                    # hier "moviepilot" und "wikipedia" # auch andere Gruppierungen sind möglich!
doc_years   = floor_date(doc_meta$date, "year")    # z.B. 1980-01-01, 1976-10-08, ...

# Lookup-Tabelle bauen: welche ID gehört zu welcher Quelle / welchem Jahr?
lookup = build_lookup(
  ids    = doc_ids,
  groups = doc_sources,
  dates  = doc_years
); head(lookup) # Vorschau

# LDA-Ergebnis zerlegen:
# Wie stark ist jedes Topic pro Jahr und pro Quelle vertreten?
twpg = topic_weights_by_group(
  document_topic_matrix = result_K8$document_sums,
  lookup_dict           = lookup,
  plot_by               = "topic",
  span                  = 0.5,
  out                   = "long" # oder "wide"
)

# Vorschau
head(twpg)



# -------------------------------------------------------------------------
# R-Umgebung Speichern
# -------------------------------------------------------------------------

'
Das Skript speichert bereits die wichtigsten Objekte.
Es ist damit moeglich, an spaeterer und anderer Stelle mit den bisherigen Ergebnissen weiter zu arbeiten.
Dafuer muessen lediglich die erforderlichen Objekte eingeladen werden. Je nach Funktion sind das
- der Korpus ("corp")
- der tokenisierte Korpus ("tokenizedCorpus")
- das LDA-Ergebnis ("result_K8", "result_K20", etc.)
- die numerische repraesentation des Korpus ("docs")
- die ldaIDs ("ldaID" bzw. die Elementnamen der docs-Liste: "names(docs)")

Um darueber hinaus die gesamte R-Umgebung zu speichern und "alles auf einmal" wieder einladen zu koennen, kann die folgende Funktion verwendet werden.
Der Speichervorgang sowie das spaetere Einladen koennen jedoch sehr lange dauern.
Ausserdem werden auf diese Weise auch unnoetige Objekte mit gespeichert.
'

# save everything
current_time = gsub(":","-",format(Sys.time(), "%X"))
save.image(paste0("analysis_", current_time, ".RData"))





