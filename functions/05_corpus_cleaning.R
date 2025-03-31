# remove texts based on specific title, like "Leserbriefe" or "Stocks today"
filterTitles = function(corpus, titles, pattern=F, ignore.case=F, print=F, invert=T, out="text"){
  
  before = nrow(corpus$meta)
  
  # find dups
  if(pattern){
    mask = tosca::filterWord(text=corpus$meta$title, titles, ignore.case=ignore.case, out="bin")
  }else mask = corpus$meta$title %in% titles
  
  if(print){ print(corpus$meta$title[mask]); cat("\n") }
  
  # invert mask
  if(invert) mask = !mask
  
  # print
  cat(sprintf("Kept %d out of %d articles | %d removed (%.2f%%)\n",
              sum(mask), before, before - sum(mask), 100 * (before - sum(mask)) / before))
  
  # return
  if(out=="bin") return(mask) else return(tosca::filterID(corpus, corpus$meta$id[mask]))
  
}


#' Erweiterung von tosca::filterCounts(): erlaubt sowohl relative als auch absolute Werte, sowohl obere als auch untere Grenzen
filterWordCounts = function(corpus, lower_thresh=0, upper_thresh=1){
  
  lens = lengths(corpus$text) # expects tokenized corpus
  
  if(lower_thresh < 1) lower_thresh = quantile(lens, lower_thresh)
  if(upper_thresh <= 1) upper_thresh = quantile(lens, upper_thresh)
  
  # return restricted corpus
  mask = lens >= lower_thresh & lens <= upper_thresh
  return(filterID(corpus, names(mask)[mask]))
  
}


# removes text duplicates based on a text's title
filterDups_titles = function(corpus, unit = "day", ignore.case=F, message=T) {
  
  # Sicherstellen, dass das Eingabeformat korrekt ist
  # if (!unit %in% c("all", "days", "months", "years")) stop('unit must be one of c("all", "days", "months", "years")')
  
  before = nrow(corpus$meta)
  
  # corpus sortieren
  corpus$meta = corpus$meta[order(corpus$meta$date),]
  corpus$text = corpus$text[match(corpus$meta$id, names(corpus$text))]
  
  if(any(is.na(corpus$meta$date))){
    message("NAs found in meta data. Corpus is restricted to non-NA cases.")
    corpus = filterID(corpus, corpus$meta$id[!is.na(corpus$meta$date)])
  }
  
  # Erstellung der Datumsgruppen
  date_chunks = if (unit == "all") "2000-01-01" else unique(lubridate::floor_date(corpus$meta$date, unit))
  floor_dates = if (unit == "all") rep("2000-01-01", nrow(corpus$meta)) else lubridate::floor_date(corpus$meta$date, unit)
  
  # controle case
  if(ignore.case) corpus$meta$title = tolower(corpus$meta$title)
  
  # find dups
  dups = unlist(sapply(date_chunks, function(chunk) {
    mask = floor_dates == chunk
    duplicated(corpus$meta$title[mask])
  }, simplify = T))
  
  # print
  if(message){
    cat(sprintf("Kept %d out of %d articles | %d removed (%.2f%%)\n",
                sum(!dups), before, before - sum(!dups), 100 * (before - sum(!dups)) / before))
  }
  
  # return
  return(tosca::filterID(corpus, corpus$meta$id[!dups]))
}


# removes text duplicates based on first X characters of a text
filterDups_leads = function(corpus, checkFirstChars = 120, unit = "day", ignore.case=F, message=T) {
  
  # if (!is.list(corpus) || !'text' %in% names(corpus) || !'meta' %in% names(corpus)) {
  #   stop("Invalid corpus format")
  # }
  
  before = nrow(corpus$meta)
  
  # Vorbereitung der Daten
  leads = substr(corpus$text, 1, checkFirstChars)
  ids = names(corpus$text)
  
  # controle case
  if(ignore.case) leads = tolower(leads)
  
  if (unit != "all") {
    dates = corpus$meta$date[match(ids, corpus$meta$id)]
    
    # Erstellen eines Datenrahmens
    df = data.frame(leads, ids, dates)
    
    # Konvertieren Sie Datumsangaben je nach Einheit
    df$dates = lubridate::floor_date(df$dates, unit)
    
    # find dups
    ids = df %>% 
      arrange(dates) %>% 
      group_by(dates) %>% 
      filter(!duplicated(leads)) %>% 
      pull(ids)
  } else {
    ids = ids[!duplicated(leads)]
  }
  
  # print
  if(message){
    cat(sprintf("Kept %d out of %d articles | %d removed (%.2f%%)\n",
                length(ids), before, before - length(ids), 100 * (before - length(ids)) / before))
    
  }
  return(tosca::filterID(corpus, id = ids, filtermeta = TRUE))
}