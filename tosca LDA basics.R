

# -------------------------------------------------------------------------
# preparation
# -------------------------------------------------------------------------

# ggf. working directory festlegen
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# aktiviere erweierte Funktionen
source("erweiterte Funktionen.R")

# packages installieren und einladen
if(! require("tosca", character.only = T, quietly = T)) install.packages("tosca")
if(! require("writexl", character.only = T, quietly = T)) install.packages("writexl")

library(tosca)
library(writexl)

# Datensatz einladen
corp = readRDS()


# -------------------------------------------------------------------------
# simple Korpus-Analysen
# -------------------------------------------------------------------------

# Anz. Artikel im Zeitverlauf
plotScot(corp)

# Anz. Artikel im Zeitverlauf, in denen ein best. Suchwort vorkommt
worte = c("Trump", "Krim", "Merkel")
plotFreq(corp, wordlist = worte)



# -------------------------------------------------------------------------
# Korpus einschraenken
# -------------------------------------------------------------------------

# Auf Grundlage des Erscheinungsdatums der Artikel 
corp = filterDate(corp, s.date="2010-01-01", e.date="2020-12-31")

# Auf Grundlage des Vorkommens bestimmter Worte
ezb = filterWord(corp, "EZB")

# -> die Funktion filterWord() versteht (zu einem gewissen Teil) regular expressions
# Es sind also auch Ausdruecke wie der Folgende moeglich:
texte_ueber_parteien = filterWord(corp, "\\bCDU\\b|\\bSPD\\b|\\bFDP\\b|Gr(ü|ue)ne(n)?\\b", ignore.case=T)

# -> "|" bedeutet "oder", "\\b" bedeutet "word boundary", "?" bedeutet "kann vorkommen, muss aber nicht", "ignore.case=T" stellt sicher, dass Gross- und Kleinschreibung ignoriert werden 
# -> ignore.case=T transformiert alle Texte automatisch in Kleinbuchstaben. Wenn du das umgehen willst, verwenden stattdessen den folgenden Code:
texte_ueber_parteien_bool = filterWord(corp, "\\bCDU\\b|\\bSPD\\b|\\bFDP\\b|Gr(ü|ue)ne(n)?\\b", out="bin")
texte_ueber_parteien_ids = names(corp$text)[bool]
texte_ueber_parteien = filterID(corp, texte_ueber_parteien_ids)



# -------------------------------------------------------------------------
# Korpus fuer LDA vorbereiten
# -------------------------------------------------------------------------

# Korpus tokenisieren - verwende alternative (deutsche) Stopword-Liste
stopwords = readLines("stop_words_de.txt")
tokenizedCorpus = cleanTexts(object=corp, sw=stopwords)
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

K_WERTE = c(8,10) # <- probiere hier verschiedene K-Werte aus

for(k in K_WERTE){
  
  message("Generate results for K=", k)
  
  # create subfolder
  if(! dir.exists(paste0("K",k))) dir.create(paste0("K",k))
  
  # save LDA-result to R environment
  result = LDAgen(docs, K=k, vocab, folder = paste0("K",k,"/Ergebnis"), seed = 123)
  assign(paste0("result_K",k), result)
  
  # save LDA-result locally
  saveRDS(result, paste0("K",k,"/result_K",k, ".rds"))
  
  # create Top Words
  lda_getTopWords(result, file = paste0("K",k,"/TopWords_K",k))
  
  # create Top Texts
  lda_getTopTexts(corp, result, ldaID = names(docs), file = paste0("K",k,"/TopTexts_K",k))
  
}

# save everything
save.image(paste0("analysis_", gsub(":","-",format(Sys.time(), "%X")), ".RData"))



# -------------------------------------------------------------------------
# LDA auswerten
# -------------------------------------------------------------------------

# Weitere Funktionen fuer die Auswertung: Siehe Koppers et al. (2020) tosca Vignette


# Beispiel: PlotTopic:
plotTopic(object = tokenizedCorpus, ldaresult = result_K10, ldaID = names(docs),
          select = c(3,4), rel = TRUE, curves = "both", smooth = 0.1, legend = "topleft")

# um die Plots lokal zu speichern:
plot = plotTopic(object = tokenizedCorpus, ldaresult = result_K10, ldaID = names(docs),
                 select = c(3,4), rel = TRUE, curves = "both", smooth = 0.1, legend = "topleft")
write.csv2(plot, "plot.csv")


# Top Texts per unit (hier: quarter) - pro Quartal die relevantesten Texte pro Topic
topTextsPerUnit(corp, result_K8, unit="quarter", foldername = "TopTextsPerUnit")

# Top Words per unit (hier: quarter) - pro Quartal die relevantesten Worte pro Topic
topWordsPerUnit(corp, result_K8, docs, unit="quarter", file="TopWordsPerUnit")


