require(dplyr)
require(tosca)
require(lubridate)
require(utf8)


filterTitles = function(corpus, pattern, ignore.case=F, print=F){
  
  before = nrow(corpus$meta)
  
  # find dups
  mask = !tosca::filterWord(corpus$meta$title, pattern, ignore.case=ignore.case, out="bin")
  
  if(print) print(corpus$meta$title[!mask])
  
  # print
  cat(sprintf("Kept %d out of %d articles | %d removed (%.2f%%)\n",
              sum(mask), before, before - sum(mask), 100 * (before - sum(mask)) / before))
  
  # return
  invisible(tosca::filterID(corpus, corpus$meta$id[mask]))
}


filterDups_titles = function(corpus, unit = "day") {
  
  # Sicherstellen, dass das Eingabeformat korrekt ist
  # if (!unit %in% c("all", "days", "months", "years")) stop('unit must be one of c("all", "days", "months", "years")')
  
  before = nrow(corpus$meta)
  
  # Erstellung der Datumsgruppen
  date_chunks = if (unit == "all") "2000-01-01" else unique(lubridate::floor_date(corpus$meta$date, unit))
  floor_dates = if (unit == "all") rep("2000-01-01", nrow(corpus$meta)) else lubridate::floor_date(corpus$meta$date, unit)
  
  # find dups
  dups = unlist(sapply(date_chunks, function(chunk) {
    mask = floor_dates == chunk
    duplicated(corpus$meta$title[mask])
  }, simplify = T))
  
  # print
  cat(sprintf("Kept %d out of %d articles | %d removed (%.2f%%)\n",
              sum(!dups), before, before - sum(!dups), 100 * (before - sum(!dups)) / before))
  
  # return
  invisible(tosca::filterID(corpus, corpus$meta$id[!dups]))
  
}


filterDups_leads = function(corpus, checkFirstChars = 120, unit = "day") {
  
  # if (!is.list(corpus) || !'text' %in% names(corpus) || !'meta' %in% names(corpus)) {
  #   stop("Invalid corpus format")
  # }
  
  before = nrow(corpus$meta)
  
  # Vorbereitung der Daten
  leads = substr(corpus$text, 1, checkFirstChars)
  ids = names(corpus$text)
  
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
  cat(sprintf("Kept %d out of %d articles | %d removed (%.2f%%)\n",
              length(ids), before, before - length(ids), 100 * (before - length(ids)) / before))
  
  invisible(tosca::filterID(corpus, id = ids, filtermeta = TRUE))
}



# Plot Sources
plotSources = function(corpus, ..., span=0.1) {
  
  # count
  data = corpus$meta %>%
    mutate(date = floor_date(date, unit)) %>%
    count(date, resource)
  
  # generate plot
  print(ggplot(data, aes(date, n, fill=resource))+
    geom_area(...)+
    geom_smooth(span=span, se=F, alpha=0.8, color="grey2")+ 
    theme_classic())
  
  invisible(data)
  
}



# Beispiele
# filterDups_leads(welt, 20, "year")
# filterDups_titles(welt, "all")
# filterTitles(corpus, "Merkel")
