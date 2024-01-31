

# -------------------------------------------------------------------------
# preparation
# -------------------------------------------------------------------------

# ggf. working directory festlegen
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# packages installieren und einladen
if(! require("tosca", character.only = T, quietly = T)) install.packages("tosca")
if(! require("writexl", character.only = T, quietly = T)) install.packages("writexl")

# aktiviere erweierte Funktionen
source("erweiterte Funktionen.R")

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
corp = filterWord(corp, "EZB") # -> Wichtig: per Default fuehrt die Funktion sog. Pattern Searches aus. D.h., es werden auch solche Dokumente gefunden, in denen (Beispiel hier) "OEZBECK" vorkommt 
corp = filterWord(corp, "\\bEZB\\b") # -> Um das zu umgehen: "\\b" stellt sicher, dass "EZB" zu beiden Seiten von einer Word Boundary (Leer- oder Satzzeichen) umgeben ist

# Die Funktion filterWord() versteht zu einem gewissen Teil regular expressions
# Es sind also auch Ausdruecke wie der Folgende moeglich:
corp = filterWord(corp, "\\b(CDU|SPD|FDP|Gr(ü|ue)ne(n)?)\\b", ignore.case=T)

# -> "|" bedeutet "oder", "\\b" bedeutet "word boundary", "?" bedeutet "kann vorkommen, muss aber nicht"
# -> "ignore.case=T" stellt sicher, dass Gross- und Kleinschreibung ignoriert werden. Wenn das nicht gewuenscht ist, Parameter auf F (FALSE) setzen

# -> Wichtig: "ignore.case=T" transformiert alle Texte automatisch in Kleinbuchstaben. Wenn das vermieden werden soll, ist der folgende Code zu empfehlen:

mask = filterWord(corp, "\\b(CDU|SPD|FDP|Gr(ü|ue)ne(n)?)\\b", ignore.case=T, out="bin") # -> out="bin" gibt keinen Korpus, sondern einen booleschen Vektor zurueck
relevant_ids = names(corp$text)[mask]
corp = filterID(corp, relevant_ids)

# Fuer weitere Moeglichkeiten der Korpus-Einschraenkung (inkl. Einstellung, dass einzelne Worte mehrfach vorkommen sollen):
# Siehe Koppers et al. (2021) tosca Vignette



# -------------------------------------------------------------------------
# Korpus fuer LDA vorbereiten
# -------------------------------------------------------------------------

# Korpus tokenisieren - verwende alternative (deutsche) Stopword-Liste
stopwords = readLines("stopword_lists/stop_words_de.txt")
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



# -------------------------------------------------------------------------
# LDA auswerten
# -------------------------------------------------------------------------


# Weitere Funktionen fuer die Auswertung: Siehe Koppers et al. (2021) tosca Vignette


# print TopWords along with Doc Titles
get_tw_and_titles(corpus = corp, ldaresult = result_K8, ldaID = names(docs), topic = 1, n = 20)


# Beispiel: PlotTopic:
plotTopic(object = tokenizedCorpus, ldaresult = result_K8, ldaID = names(docs),
          select = c(3,4), rel = TRUE, curves = "both", smooth = 0.1, legend = "topleft")

# um die Plots lokal zu speichern:
plot = plotTopic(object = tokenizedCorpus, ldaresult = result_K8, ldaID = names(docs),
                 select = c(3,4), rel = TRUE, curves = "both", smooth = 0.1, legend = "topleft")
write.csv2(plot, "plot.csv")


# Top Texts per unit (hier: quarter) - pro Quartal die relevantesten Texte pro Topic
topTextsPerUnit(corpus = corp, ldaresult = result_K8, ldaID = names(docs), unit = "quarter", foldername = "TopTextsPerUnit")

# Top Words per unit (hier: quarter) - pro Quartal die relevantesten Worte pro Topic
topWordsPerUnit(corpus = corp, ldaresult = result_K8, docs = docs, unit = "quarter", file = "TopWordsPerUnit")



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

