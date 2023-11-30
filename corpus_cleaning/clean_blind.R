

# load functions
source("functions.R")
before = length(corpus$text)

# -------------------------------------------------------------------------
# rein strukturelle Vorverarbeitung
# -------------------------------------------------------------------------


# Shorten meta data
{
  a = Sys.time()
  cat("Shorten meta data")
  corpus$metamult = NULL
  corpus$meta = corpus$meta[,c("id","date","title","resource")]
  diff = difftime(Sys.time(), a)
  cat(" | Done. Time for computation:", round(diff,3), attr(diff, "unit"), "\n\n")
}

# Reorder meta files
{
  a = Sys.time()
  cat("Reorder meta files")
  corpus$meta = corpus$meta[order(corpus$meta$date),]
  corpus$text = corpus$text[match(corpus$meta$id, names(corpus$text))]
  corpus$meta = corpus$meta[corpus$meta$id %in% names(corpus$text),]
  diff = difftime(Sys.time(), a)
  cat(" | Done. Time for computation:", round(diff,3), attr(diff, "unit"), "\n\n")
}

# Titles aufraeumen: Mehrfache Leerzeichen raus
{
  cat("Mehrfache Leerzeichen in Titles loeschen")
  a = Sys.time()
  corpus$meta$title = gsub("\\s+"," ",corpus$meta$title)
  diff = difftime(Sys.time(), a)
  cat(" | Done. Time for computation:", round(diff,3), attr(diff, "unit"), "\n\n")
}

# Titles aufraeumen: Weirde Anfuehrungszeichen ersetzen
{
  cat("Weirde Anfuehrungszeichen ersetzen")
  a = Sys.time()
  corpus$meta$title = gsub("&quot;","'",corpus$meta$title)
  diff = difftime(Sys.time(), a)
  cat(" | Done. Time for computation:", round(diff,3), attr(diff, "unit"), "\n\n")
}

# Restrict corpus to long texts
{
  a = Sys.time()
  before = nrow(corpus$meta)
  cat("Restrict corpus to long texts (min_length =", min_nchar, "\b)")
  mask = nchar(corpus$text) >= min_nchar
  corpus = filterID(corpus, names(mask)[mask])
  diff = difftime(Sys.time(), a)
  cat(" | Done. Time for computation:", round(diff,3), attr(diff, "unit"), "\n")
  cat(sprintf("Kept %d out of %d articles | %d removed (%.2f%%)\n\n",
              nrow(corpus$meta), before, before - nrow(corpus$meta), 100 * (before - nrow(corpus$meta)) / before))
  
}

# remove too long texts
{
  a = Sys.time()
  before = nrow(corpus$meta)
  cat("Restrict corpus to short texts (max_length =", max_nchar, "\b)")
  mask = nchar(corpus$text) <= max_nchar
  corpus = filterID(corpus, names(mask)[mask])
  cat(" | Done. Time for computation:", round(diff,3), attr(diff, "unit"), "\n")
  cat(sprintf("Kept %d out of %d articles | %d removed (%.2f%%)\n\n",
              nrow(corpus$meta), before, before - nrow(corpus$meta), 100 * (before - nrow(corpus$meta)) / before))
}

# Redefine resources
if(hbweltsz){
  cat("Redefine resources")
  a = Sys.time()
  corpus$meta$source_indicator[!is.na(corpus$meta$source)] = "HB"
  corpus$meta$source_indicator[!is.na(corpus$meta$resource)] = "Welt"
  corpus$meta$source_indicator[!is.na(corpus$meta$AnzChar)] = "SZ"
  corpus$meta$resource = corpus$meta$source_indicator
  diff = difftime(Sys.time(), a)
  cat(" | Done. Time for computation:", round(diff,3), attr(diff, "unit"), "\n\n")
}

# Convert texts and titles to utf8
if(utf8){
  a = Sys.time()
  cat("Convert texts and titles to utf8")
  corpus$text = lapply(corpus$text, utf8::as_utf8)
  corpus$meta$title = sapply(corpus$meta$title, utf8::as_utf8)
  diff = difftime(Sys.time(), a)
  cat(" | Done. Time for computation:", round(diff,3), attr(diff, "unit"), "\n\n")
}



# -------------------------------------------------------------------------
# Duplikate raus
# -------------------------------------------------------------------------

# Select unique text elements by ID
{
  a = Sys.time()
  before = nrow(corpus$meta)
  cat("Select unique text elements by ID")
  ids = unique(names(corpus$text))
  corpus$text = corpus$text[ids]
  corpus$meta = corpus$meta[match(ids,corpus$meta$id),]
  diff = difftime(Sys.time(), a)
  cat(" | Done. Time for computation:", round(diff,3), attr(diff, "unit"), "\n")
  cat(sprintf("Kept %d out of %d articles | %d removed (%.2f%%)\n\n",
              nrow(corpus$meta), before, before - nrow(corpus$meta), 100 * (before - nrow(corpus$meta)) / before))
  
}

# duplist
{
  a = Sys.time()
  before = nrow(corpus$meta)
  duplist = duplist(corpus)
  corpus = filterID(corpus, duplist$notDuplicatedTexts)
  diff = difftime(Sys.time(), a)
  cat("\nDone. Time for computation:", round(diff,3), attr(diff, "unit"), "\n")
  cat(sprintf("Kept %d out of %d articles | %d removed (%.2f%%)\n\n",
              nrow(corpus$meta), before, before - nrow(corpus$meta), 100 * (before - nrow(corpus$meta)) / before))
  
}

# Filter Leads: Texte raus, die in den ersten X Zeichen uebereinstimmen
{
  a = Sys.time()
  cat("Filter Leads: ")
  corpus = filterDups_leads(corpus, check_first_chars, "week")
  diff = difftime(Sys.time(), a)
  cat("Done. Time for computation:", round(diff,3), attr(diff, "unit"), "\n\n")
}

# Filter Titles: Alle Texte raus, die am selben Tag erschienen sind und denselben Title haben
{
  a = Sys.time()
  cat("Filter Titles: ")
  corpus = filterDups_titles(corpus, "day")
  diff = difftime(Sys.time(), a)
  cat("Done. Time for computation:", round(diff,3), attr(diff, "unit"), "\n\n")
}


after = length(corpus$text)
message(sprintf("Complete.\nKept %d out of %d articles | %d removed (%.2f%%)\n",
            after, before, before - after, 100 * (before - after) / before))
