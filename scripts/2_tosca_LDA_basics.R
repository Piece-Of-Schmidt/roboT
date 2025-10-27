
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
# LDA rechnen
# -------------------------------------------------------------------------

K_WERTE = c(8,10) # <- probieren Sie hier verschiedene K-Werte aus

for(k in K_WERTE){
  
  message("Generate results for K=", k)

  # define seed
  seed = 123
  
  # create subfolder
  if(! dir.exists(paste0("K",k))) dir.create(paste0("K",k))
  
  # save LDA-result to R environment
  result = LDAgen(docs, K=k, vocab, folder = paste0("K",k,"/Ergebnis"), seed = seed)
  assign(paste0("result_K",k), result)
  
  # save LDA-result locally
  saveRDS(result, sprintf("K%d/result_K%d_seed%d.rds", k, k, seed))
  
  # create Top Words
  lda_getTopWords(result, file = sprintf("K%d/TopWords_K%d_seed%d.rds", k, k, seed))
  
  # create Top Texts
  lda_getTopTexts(corp, result, ldaID = names(docs), file = sprintf("K%d/TopTexts_K%d_seed%d.rds", k, k, seed))
  
}



# -------------------------------------------------------------------------
# LDA auswerten
# -------------------------------------------------------------------------


# Weitere Funktionen fuer die Auswertung: Siehe Koppers et al. (2021) tosca Vignette


# print TopWords along with Doc Titles
get_tw_and_titles(corpus = corp, ldaresult = result_K8, ldaID = names(docs), topic = 8, n = 20)


# Beispiel: PlotTopic:
plotTopic(object = tokenizedCorpus, ldaresult = result_K8, ldaID = names(docs),
          select = c(3,4), rel = TRUE, curves = "both", smooth = 0.1, legend = "topleft")

# um die Plots lokal zu speichern:
plot = plotTopic(object = tokenizedCorpus, ldaresult = result_K8, ldaID = names(docs),
                 select = c(3,4), rel = TRUE, curves = "both", smooth = 0.1, legend = "topleft")
write.csv2(plot, "plot.csv")


# Top Texts per unit (hier: quarter) - pro Quartal die relevantesten Texte pro Topic
K_value = 8
result = get(paste0("result_K", K_value))
lda_getTopTextsPerUnit(corpus = corp, ldaresult = result, ldaID = names(docs), unit = "quarter", groupby="topic", foldername = paste0("K", K_value, "/TopTextsPerUnit"))

# Top Words per unit (hier: quarter) - pro Quartal die relevantesten Worte pro Topic
K_value = 8
result = get(paste0("result_K", K_value))
lda_getTopWordsPerUnit(corpus = corp, ldaresult = result, docs = docs, unit = "quarter", groupby="topic", file = paste0("K", K_value, "/TopWordsPerUnit"))




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


