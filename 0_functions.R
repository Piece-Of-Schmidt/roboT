# install and/or load packages

packages = c("lubridate","tosca","writexl", "dplyr", "utf8", "ggplot2", "deeplr")
for(package in packages){
  
  load_package = T
  if(!require(package, quietly=T, character.only=T)){
    
    cat("Package", package, "wird benoetigt, ist aber nicht installiert. Soll es installiert werden?\n0: nein\n1: ja")
    install = as.logical(as.numeric(readline("Print 0 or 1:  ")))
    
    if(install) install.packages(package, dependencies = TRUE) else load_package = F
  }
  if(load_package){ library(package, character.only=TRUE) }
}

# -------------------------------------------------------------------------
# KORPUS CLEANING
# -------------------------------------------------------------------------


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


filterDups_titles = function(corpus, unit = "day", message=T) {
  
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


filterDups_leads = function(corpus, checkFirstChars = 120, unit = "day", message=T) {
  
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
  if(message){
    cat(sprintf("Kept %d out of %d articles | %d removed (%.2f%%)\n",
                length(ids), before, before - length(ids), 100 * (before - length(ids)) / before))
    
  }
  
  return(tosca::filterID(corpus, id = ids, filtermeta = TRUE))
}


# Plot Sources
plotSources = function(corpus, unit="month", area_alpha=0.6, area_position="identity", ..., smooth=T, span=0.1) {
  
  # count
  data = corpus$meta %>%
    mutate(date = floor_date(date, unit)) %>%
    count(date, resource)
  
  # generate plot
  if(smooth){
    p = ggplot(data, aes(date, n, fill=resource))+
      geom_area(alpha=area_alpha, position=area_position,...)+
      geom_smooth(span=span, se=F, alpha=0.8, color="grey2")+ 
      theme_classic()
  }else{
    p = ggplot(data, aes(date, n, fill=resource))+
      geom_area(alpha=area_alpha, position=area_position,...)+
      theme_classic()
  }
  print(p)
  
  invisible(data)
}


clean_complete = function(corpus,
                          shorten_meta=T,
                          remove_na_dates=T,
                          remove_na_sources=T,
                          remove_na_titles=T,
                          sort_meta=T,
                          min_text_length = 750,
                          max_text_length = 0.99,
                          check_dups_id=T,
                          check_dups_duplist=T,
                          check_dups_textleads=120,
                          check_dups_titles=T,
                          check_dups_textleads_unit="week",
                          check_dups_titles_unit="day",
                          clean_titles=T,
                          utf8=F,
                          clean_memory=F,
                          hbweltsz=F
){
  
  before_whole = length(corpus$text)
  if(!is.null(min_text_length) || !is.null(max_text_length)){
    a = Sys.time()
    cat("Calculate text lengths...")
    text_chars = nchar(corpus$text)
    diff = difftime(Sys.time(), a)
    cat(" Done. Time for computation:", round(diff,3), attr(diff, "unit"), "\n")
  }
  max_text_length = if(max_text_length<=1) quantile(text_chars, max_text_length) else max_text_length

  # if corp == "HBWeltSZ": Redefine resources
  if(hbweltsz){
    cat("Redefine resources...")
    a = Sys.time()
    corpus$meta$source_indicator[!is.na(corpus$meta$source)] = "HB"
    corpus$meta$source_indicator[!is.na(corpus$meta$resource)] = "Welt"
    corpus$meta$source_indicator[!is.na(corpus$meta$AnzChar)] = "SZ"
    corpus$meta$resource = corpus$meta$source_indicator
    diff = difftime(Sys.time(), a)
    cat(" Done. Time for computation:", round(diff,3), attr(diff, "unit"), "\n")
    if(clean_memory) gc()
  }
  
  # Shorten meta data
  if(shorten_meta){
    a = Sys.time()
    cat("Shorten meta data...")
    corpus$metamult = NULL
    corpus$meta = corpus$meta[, names(corpus$meta) %in% c("id","date","title","resource")]
    diff = difftime(Sys.time(), a)
    cat(" Done. Time for computation:", round(diff,3), attr(diff, "unit"), "\n")
    if(clean_memory) gc()
  }

  # remove texts with missing date info
  if(remove_na_dates){
    a = Sys.time()
    before = nrow(corpus$meta)
    cat("Remove docs with missing date info...")
    corpus = filterID(corpus, corpus$meta$id[!is.na(corpus$meta$date)])
    diff = difftime(Sys.time(), a)
    cat(" Done. Time for computation:", round(diff,3), attr(diff, "unit"))
    cat(sprintf(" | Kept %d out of %d articles / %d removed (%.2f%%)\n",
                nrow(corpus$meta), before, before - nrow(corpus$meta), 100 * (before - nrow(corpus$meta)) / before))
    if(clean_memory) gc()
  }

  # remove texts with missing resource info
  if(remove_na_sources){
    a = Sys.time()
    before = nrow(corpus$meta)
    cat("Remove docs with missing resource info...")
    corpus = filterID(corpus, corpus$meta$id[!is.na(corpus$meta$resource)])
    diff = difftime(Sys.time(), a)
    cat(" Done. Time for computation:", round(diff,3), attr(diff, "unit"))
    cat(sprintf(" | Kept %d out of %d articles / %d removed (%.2f%%)\n",
                nrow(corpus$meta), before, before - nrow(corpus$meta), 100 * (before - nrow(corpus$meta)) / before))
    if(clean_memory) gc()
  }

  # remove texts with missing title info
  if(remove_na_titles){
    a = Sys.time()
    before = nrow(corpus$meta)
    cat("Remove docs with missing title info...")
    corpus = filterID(corpus, corpus$meta$id[!is.na(corpus$meta$title)])
    diff = difftime(Sys.time(), a)
    cat(" Done. Time for computation:", round(diff,3), attr(diff, "unit"))
    cat(sprintf(" | Kept %d out of %d articles / %d removed (%.2f%%)\n",
                nrow(corpus$meta), before, before - nrow(corpus$meta), 100 * (before - nrow(corpus$meta)) / before))
    if(clean_memory) gc()
  }
  
  # Reorder meta files
  if(sort_meta){
    a = Sys.time()
    cat("Reorder meta files...")
    corpus$meta = corpus$meta[order(corpus$meta$date),]
    corpus$text = corpus$text[match(corpus$meta$id, names(corpus$text))]
    corpus$meta = corpus$meta[corpus$meta$id %in% names(corpus$text),]
    diff = difftime(Sys.time(), a)
    cat(" Done. Time for computation:", round(diff,3), attr(diff, "unit"), "\n")
    if(clean_memory) gc()
  }
  
  # remove too long texts
  if(!is.null(max_text_length)){
    a = Sys.time()
    before = nrow(corpus$meta)
    cat("Restrict corpus to short texts (max_length =", max_text_length, "\b)...")
    mask = text_chars <= max_text_length
    text_chars = text_chars[mask]
    corpus = filterID(corpus, names(text_chars))
    cat(" Done. Time for computation:", round(diff,3), attr(diff, "unit"))
    cat(sprintf(" | Kept %d out of %d articles / %d removed (%.2f%%)\n",
                nrow(corpus$meta), before, before - nrow(corpus$meta), 100 * (before - nrow(corpus$meta)) / before))
    if(clean_memory) gc()
  }
  
  # Restrict corpus to long texts
  if(!is.null(min_text_length)){
    a = Sys.time()
    before = nrow(corpus$meta)
    cat("Restrict corpus to long texts (min_length =", min_text_length, "\b)...")
    mask = text_chars >= min_text_length
    text_chars = text_chars[mask]
    corpus = filterID(corpus, names(text_chars))
    diff = difftime(Sys.time(), a)
    cat(" Done. Time for computation:", round(diff,3), attr(diff, "unit"))
    cat(sprintf(" | Kept %d out of %d articles / %d removed (%.2f%%)\n",
                nrow(corpus$meta), before, before - nrow(corpus$meta), 100 * (before - nrow(corpus$meta)) / before))
    if(clean_memory) gc()
  }
  
  # Convert texts and titles to utf8
  if(utf8){
    a = Sys.time()
    cat("Convert texts and titles to utf8...")
    corpus$text = lapply(corpus$text, utf8::as_utf8)
    corpus$meta$title = sapply(corpus$meta$title, utf8::as_utf8)
    diff = difftime(Sys.time(), a)
    cat(" Done. Time for computation:", round(diff,3), attr(diff, "unit"), "\n")
    if(clean_memory) gc()
  }
  
  # -------------------------------------------------------------------------
  # Duplikate raus
  # -------------------------------------------------------------------------
  
  # Select unique text elements by ID
  if(check_dups_id){
    a = Sys.time()
    before = nrow(corpus$meta)
    cat("Select unique text elements by ID...")
    ids = unique(names(corpus$text))
    corpus$text = corpus$text[ids]
    corpus$meta = corpus$meta[match(ids,corpus$meta$id),]
    diff = difftime(Sys.time(), a)
    cat(" Done. Time for computation:", round(diff,3), attr(diff, "unit"))
    cat(sprintf("| Kept %d out of %d articles / %d removed (%.2f%%)\n",
                nrow(corpus$meta), before, before - nrow(corpus$meta), 100 * (before - nrow(corpus$meta)) / before))
    if(clean_memory) gc()
    
  }
  
  # duplist
  if(check_dups_duplist){
    a = Sys.time()
    before = nrow(corpus$meta)
    cat("Perform tosca::duplist() operation...")
    invisible(capture.output(duplist <- suppressMessages(duplist(corpus))))
    corpus = filterID(corpus, duplist$uniqueTexts)
    diff = difftime(Sys.time(), a)
    cat(" Done. Time for computation:", round(diff,3), attr(diff, "unit"))
    cat(sprintf(" | Kept %d out of %d articles / %d removed (%.2f%%)\n",
                nrow(corpus$meta), before, before - nrow(corpus$meta), 100 * (before - nrow(corpus$meta)) / before))
    if(clean_memory) gc()
    
  }
  
  # Filter Leads: Texte raus, die in den ersten X Zeichen uebereinstimmen
  if(!is.null(check_dups_textleads)){
    a = Sys.time()
    before = nrow(corpus$meta)
    cat("Filter Leads...")
    corpus = filterDups_leads(corpus, check_dups_textleads, check_dups_textleads_unit, message=F)
    diff = difftime(Sys.time(), a)
    cat(" Done. Time for computation:", round(diff,3), attr(diff, "unit"))
    cat(sprintf(" | Kept %d out of %d articles / %d removed (%.2f%%)\n",
                nrow(corpus$meta), before, before - nrow(corpus$meta), 100 * (before - nrow(corpus$meta)) / before))
    if(clean_memory) gc()
  }
  
  # Filter Titles: Alle Texte raus, die am selben Tag erschienen sind und denselben Title haben
  if(check_dups_titles){
    a = Sys.time()
    before = nrow(corpus$meta)
    cat("Filter Titles...")
    corpus = filterDups_titles(corpus, check_dups_titles_unit, message=F)
    diff = difftime(Sys.time(), a)
    cat(" Done. Time for computation:", round(diff,3), attr(diff, "unit"))
    cat(sprintf(" | Kept %d out of %d articles / %d removed (%.2f%%)\n",
                nrow(corpus$meta), before, before - nrow(corpus$meta), 100 * (before - nrow(corpus$meta)) / before))
    if(clean_memory) gc()
  }
  
  # Titles aufraeumen: Mehrfache Leerzeichen raus
  if(clean_titles){
    cat("Clean title strings...")
    a = Sys.time()
    corpus$meta$title = gsub("\\s+"," ",corpus$meta$title)
    corpus$meta$title = gsub("&quot;","'",corpus$meta$title)
    diff = difftime(Sys.time(), a)
    cat(" Done. Time for computation:", round(diff,3), attr(diff, "unit"), "\n\n")
    if(clean_memory) gc()
  }
  
  after = length(corpus$text)
  message(sprintf("Complete.\nKept %d out of %d articles / %d removed (%.2f%%)\n",
                  after, before_whole, before_whole - after, 100 * (before_whole - after) / before_whole))
  
  return(corpus)
}


# -------------------------------------------------------------------------
# LDA AUSWERTUNG
# -------------------------------------------------------------------------


#' Erweiterung von tosca::filterCounts(): erlaubt sowohl relative als auch absolute Werte, sowohl obere als auch untere Grenzen
filterWordCounts = function(corpus, lower_thresh=0, upper_thresh=1){
  
  lens = lengths(corpus$text)
  
  if(lower_thresh < 1) lower_thresh = quantile(lens, lower_thresh)
  if(upper_thresh <= 1) upper_thresh = quantile(lens, upper_thresh)
  
  # return restricted corpus
  mask = lens >= lower_thresh & lens <= upper_thresh
  return(filterID(corpus, names(mask)[mask]))
  
}

get_tw_and_titles = function(corpus, ldaresult, ldaID, topic = 1, n = 20){
  
  # get titles
  titles = topTexts(ldaresult, ldaID, limit=n)
  titles = corpus$meta$title[match(titles, corpus$meta$id)]
  titles = matrix(titles, nrow=n)[,topic]
  
  # get topwords
  topwords = topWords(ldaresult[["topics"]], n)[,topic]
  
  # print result
  sep = paste(strrep(" ", max(nchar(topwords)) - nchar(topwords)), "| ")
  
  out = paste(topwords, sep , titles)
  print("TopWords | Doc Titles")
  print(out, width = max(nchar(out)))
}


#'lda_getTopTexts
#'
#'Speichert die Top-Texte (n=100) einer LDA und einem Excel-Sheet und legt sie in einem entsprechenden Ordner auf dem Rechner ab. Fuehrt die tosca-Funktionen "topTexts()" und "showTexts()" durch. Erweitert den Standard-tosca-Output um zwei weitere Spalten ("topic_relevance" und "source")
#'
#'@param corpus Ausgangskorpus als meta-Datei. Sollte im Vorhinein um Duplikate bereinigt werden
#'@param ldaresult Objekt, das die tosca-Funktion "LDAgen()" generiert
#'@param ldaID IDs von Texten, die beruecksichtigt werdeb sollen. Default: Alle Texte
#'@param nTopTexts Menge an TopTexts, die generiert wird
#'@param file Dateiname
#'
lda_getTopTexts = function(corpus, ldaresult, ldaID, nTopTexts=50, file="topTexts",
                           translate=F, max_text_length=32000, source_lang=NULL, deepl_key=NULL){
  
  # safety belt
  if(missing("ldaID")){ldaID = names(corpus$text); warning("Missing ldaID. IDs of the corpus text are used as proxies.\nTrue IDs may differ!\nPlease check the generated top texts.")}
  if(missing("corpus") || missing(ldaresult)) stop("Insert correct arguments for corpus and ldaresult")
  if(translate && is.null(deepl_key)) stop("Insert valid deepl authentification key to use translation")
  
  # is the passed object a textmeta object?
  corp = is.textmeta(corpus)
  
  # convert to textmeta object, even if object is just a list of texts
  if(!corp) corpus = as.textmeta(corpus)
  
  # generate data frame of topTexts
  tt = tosca::topTexts(ldaresult, ldaID, nTopTexts)
  tt = tosca::showTexts(corpus, tt)
  
  # shorten (and translate) texts
  tt = lapply(tt, function(topic){
    topic$text = substr(topic$text, 0, max_text_length)
    if(translate){
      topic$title = toGerman2(topic$title, source_lang=source_lang, auth_key=deepl_key)
      topic$text = toGerman2(topic$text, source_lang=source_lang, auth_key=deepl_key)
    }
    topic
  })
  
  # get theta values of LDA result
  docs_per_topic = ldaresult$document_sums/rowSums(ldaresult$document_sums)
  docs_per_topic = apply(docs_per_topic, 2, function(x) x/sum(x))
  
  # add theta values to data frame
  proms = apply(docs_per_topic, 1, function(x) round(sort(x,decreasing = T)[1:nTopTexts],2))
  for(i in seq(tt)){
    tt[[i]][, "topic_relevance"] = proms[,i]
    tt[[i]] = tt[[i]][, c(1,2,5,3,4)] }
  
  # add resource to data frame
  if("resource" %in% names(corpus$meta)){
    tt = lapply(tt, function(x){
      mask = match(x[,"id"], corpus$meta$id)
      x[, "source"] = corpus$meta$resource[mask]
      x[, c(1,2,3,6,4,5)] }) }
  
  # remove empty cols in case object is not a textmeta obj
  if(!corp) tt = lapply(tt, function(topic) topic[,c(1,3,5)])
  
  # save locally
  filename = paste0(sub(".xlsx","",file), ".xlsx")
  if(!is.null(file)) writexl::write_xlsx(tt, filename)
  
  invisible(tt)
}


#'lda_getTopWords
#'
#'Generiert ein Excel-Sheet mit den topwords einer LDA und legt dieses lokal auf dem rechner ab.
#'
#'@param ldaresult Objekt, das die tosca-Funktion "LDAgen()" generiert
#'@param numWords Anzahl der topwords pro Topic
#'@param file Dateiname, unter dem das Excel-Sheet gespeichert werden soll
#'
lda_getTopWords = function(ldaresult, numWords=50, file="topwords",
                           translate=F, source_lang=NULL, deepl_key=NULL){
  
  if(translate && is.null(deepl_key)) stop("Insert valid deepl authentification key to use translation")
  
  # create data frame
  topwords = tosca::topWords(ldaresult$topics, numWords)
  if(translate){
    topwords = toGerman2(topwords, source_lang=source_lang, auth_key=deepl_key)
    topwords = matrix(topwords, ncol = nrow(ldaresult$topics))
  }
  rel = round(rowSums(ldaresult$topics) / sum(ldaresult$topics), 3)
  topwords = as.data.frame(rbind(rel,topwords))
  colnames(topwords) = paste("Topic",1:ncol(topwords))
  rownames(topwords) = NULL
  
  # save locally
  filename = paste0(sub(".xlsx","",file),".xlsx")
  if(!is.null(file)) writexl::write_xlsx(topwords, filename)
  
  invisible(topwords)
}


#'topTextsPerUnit
#'
#'Errechnet die TopTexts pro Monat/Bimonth/Quartal/Halbjahr/Jahr basierend auf einem tosca-LDA-Objekt und speichert sie (falls gewuenscht) lokal
#'
#'@param corpus Textkorpus
#'@param ldaresult Objekt, das die tosca-Funktion "LDAgen()" generiert
#'@param unit month, bimonth. quarter, halfyear oder year
#'@param nTopTexts Anzahl der toptexte, die pro Topic und Periode generiert werden soll
#'@param tnames (optional) desired topic names
#'@param foldername name of folder in which the top texts are saved in. If NULL (default), texts are not saved locally
#'
topTextsPerUnit = function(corpus, ldaresult, ldaID, unit="quarter", nTopTexts=20, tnames=paste0("Topic", 1:K, ".", tosca::topWords(ldaresult$topics)), foldername=NULL,
                           translate=F, max_text_length=32000, source_lang=NULL, deepl_key=NULL){
  
  # safety belt
  if(missing("corpus") | missing(ldaresult)|!robot::is.textmeta(corpus)) stop("Insert correct arguments for corpus, ldaresult and topic")
  if(missing("ldaID")){ldaID = names(corpus$text); warning("Missing ldaID. IDs of the corpus text are used as proxies.\nTrue IDs may differ!\nPlease check the generated top texts.")}
  if(!is.null(foldername)) dir.create(foldername)
  if(translate && is.null(deepl_key)) stop("Insert valid deepl authentification key to use translation")
  
  # get params
  K = nrow(ldaresult$topics)
  doc = ldaresult$document_sums
  
  # just to be sure: reorder meta so it matches the order of the ldaIDs
  corpus$meta = corpus$meta[match(ldaID, corpus$meta$id), ]
  
  # create date chunks
  floor_dates = lubridate::floor_date(corpus$meta$date, unit)
  chunks = unique(floor_dates)
  
  # for every date chunk do the following
  progress.initialize(chunks)
  out = lapply(chunks, function(chunk){
    
    # find all docs from that period
    mask = floor_dates == chunk
    
    # normalize values
    docs_per_topic = apply(doc[, mask], 2, function(x) x/sum(x))
    
    # for every topic (row) do the following
    temp = apply(docs_per_topic, 1, function(topic){
      
      # reorder theta values (decreasing=T)
      proms = order(topic, decreasing = T)
      relevant_docs = seq(min(nTopTexts, length(proms)))
      proms = proms[relevant_docs]
      theta_vals = topic[proms]
      
      # get corresponding text IDs
      ids = ldaID[mask][proms]
      ids = ids[!is.na(ids)]
      
      if(!is.null(foldername)){
        
        # get text IDs
        texts = showTexts(corpus, ids)
        
        # add theta value
        texts[, "topic_relevance"] = round(theta_vals[relevant_docs], 2)
        
        # add resource
        if("resource" %in% names(corpus$meta)){
          mask = match(texts[,"id"], corpus$meta$id)
          texts[,"source"] = corpus$meta$resource[mask]
          texts = texts[,c(1,2,6,5,3,4)]} else texts = texts[,c(1,2,5,3,4)]
        
        # shorten (and translate) texts
        texts$text = substr(texts$text, 0, max_text_length)
        if(translate){
          texts$title = toGerman2(texts$title, source_lang=source_lang, auth_key=deepl_key)
          texts$text = toGerman2(texts$text, source_lang=source_lang, auth_key=deepl_key)
        }
        
        # return 
        texts
      }else ids
    })
    
    # save locally
    if(!is.null(foldername)){
      names(temp) = tnames
      filename = paste0(foldername,"/",chunk,".xlsx")
      writexl::write_xlsx(temp, filename)
    }
    
    # show progress and return
    progress.indicate()
    temp
    
  })
  
  # only return ids if no foldername was provided
  if(is.null(foldername)){
    
    # rearrange list
    out = lapply(1:K, function(k){
      sapply(1:length(chunks), function(t) out[[t]][,k])
    })
    
    # add names
    out = lapply(out, function(x){ colnames(x) = as.character(chunks); x })
    names(out) = tnames
  }
  
  invisible(out)
}
  


#'topWordsPerUnit
#'
#'Erzeugt ein Objekt, dass die Topwords pro Monat/Bimonth/Quartal/Halbjahr/Jahr ausgibt
#'
#'@param corpus Textkorpus
#'@param ldaresult Objekt, das die tosca-Funktion "LDAgen()" generiert
#'@param docs Objekt, das die Funktion LDAprep() generiert
#'@param unit month, bimonth. quarter, halfyear oder year
#'@param numWords Anzahl der topwords pro Topic
#'@param tnames (optional) desired topic names
#'@param values Wenn TRUE, werden zu den topwords selbst auch die zugehoerigen Werte ausgegeben
#'@param file Dateiname, unter dem das Excel-Sheet gespeichert werden soll
#'
topWordsPerUnit = function(corpus, ldaresult, docs, unit="quarter", numWords=50, tnames=NULL, values=T, file=NULL){
  
  # safety belt
  if(missing("corpus") || missing("ldaresult") || missing("docs")) stop("Insert arguments for corpus, ldaresult, and docs")
  tw = ldaresult$topics
  
  # get params
  K = nrow(tw)
  if(is.null(tnames)) tnames = paste0("Topic", 1:K, ".", tosca::topWords(tw))
  assignments = ldaresult$assignments
  vocab = colnames(tw)
  ldaID = names(docs)
  
  # just to be sure: reorder meta so it matches the order of the ldaIDs
  corpus$meta = corpus$meta[match(ldaID, corpus$meta$id), ]
  
  # create date chunks
  floor_dates = lubridate::floor_date(corpus$meta$date, unit)
  chunks = unique(floor_dates)
  
  # for every date chunk do the following
  topicsq = lapply(chunks, function(x){
    
    tmp = table(factor(unlist(assignments[floor_dates == x])+1, levels = 1:K),
                factor(unlist(lapply(docs[floor_dates == x], function(y) y[1,]))+1, levels = seq(vocab)))
    tmp = matrix(as.integer(tmp), nrow = K)
    colnames(tmp) = vocab
    tmp
  })
  
  topwordsq = lapply(topicsq, topWords, numWords = numWords, values = T)
  names(topwordsq)=chunks
  
  out = lapply(1:K, function(k) sapply(seq(topwordsq), function(t) topwordsq[[t]][[1]][,k]))
  out = lapply(out, function(x){ colnames(x)=as.character(chunks); x})
  names(out) = tnames
  
  # generate values that belong to every top word
  if(values) out = list(words=out, vals=lapply(1:K, function(k) sapply(seq(topwordsq), function(t) topwordsq[[t]][[1]][,k])))
  
  # save locally
  if(!is.null(file)){
    if(values) words=out[[1]] else words=out
    
    # transform to dataframe
    words = lapply(words, as.data.frame)
    
    filename = paste0(sub(".xlsx","",file),".xlsx")
    writexl::write_xlsx(words, filename)
  }
  
  invisible(out)
}

decompose_lda = function(document_topic_matrix, lookup_dict, select=1:nrow(document_topic_matrix), unit="month", tnames=paste0("topic", select), plot=F, out="melt"){
  
  # LOOKUP-DICT: create floor_dates
  colnames(lookup_dict) = c("id", "date", "group")
  lookup_dict$date = lubridate::floor_date(lookup_dict$date, unit=unit)
  
  # DTM: create rownames & colnames and restrict DTM to selected topics
  colnames(document_topic_matrix) = lookup_dict$id
  document_topic_matrix = document_topic_matrix[select, , drop=F]
  rownames(document_topic_matrix) = tnames
  
  # DTM: create long format
  long_format = reshape2::melt(document_topic_matrix)
  colnames(long_format) = c("topic", "id", "count")
  
  # DTM: merge with LOOKUP-DICT
  merged_data = long_format %>%
    left_join(lookup_dict, by = "id") %>%
    group_by(date, group, topic) %>%
    summarise(doc_count = sum(count))
  
  # FINAL: create plot
  if(plot){
    print(ggplot2::ggplot(merged_data, aes(x = date, y = doc_count, color = group)) +
            geom_smooth(se=F, span=0.1) +
            facet_wrap(~ topic) +
            theme_minimal() +
            labs(x = "Monat", y = "Anzahl Dokumente", color = "Quelle"))
  }
  
  result = if(out == "melt") merged_data else dcast(merged_data, date + group ~ topic, value.var = "doc_count")
  
  return(result)
  
}



# perform multiple LDAs with different settings
multipleLDAs = function(docs, vocab, ..., func = "LDAgen", runs="all", seed=1337, savelogs = T){
  
  onlyonedoc = length(unique(lengths(docs))) != 1
  onlyonevoc = !is.list(vocab)
  
  stopifnot(onlyonedoc == onlyonevoc)
  
  model = if(onlyonedoc) 1 else seq(docs)
  
  lda_vars = list(model=model, ...)
  
  # build grid
  grid = expand.grid(lda_vars)
  
  # take sample
  if(runs != "all"){
    set.seed(seed)
    runs = min(nrow(grid), runs)
    grid = grid[sample(1:nrow(grid), runs), ]
  }
  
  cat("Model params (", runs, "):\n", sep="")
  print(grid)
  
  out = lapply(seq(nrow(grid)), function(idx){
    
    args = grid[idx,]
    cat("\nCalculate LDA: ", paste(names(args), args, sep=": ", collapse = ", "))
    
    d = if(onlyonedoc) docs else docs[[unlist(args[1])]]
    v = if(onlyonevoc) vocab else vocab[[unlist(args[1])]]
    
    args = as.list(args[,-1])
    args$documents = d
    args$vocab = v
    
    do.call(func, args = args)
    
  })
  
  model_ids = paste0("run", 1:nrow(grid))
  names(out) = model_ids
  
  if(savelogs){
    grid = data.frame(id = model_ids, grid)
    write.csv(grid, "model_logs.csv")
  }
  
  invisible(out)
  
}

# -------------------------------------------------------------------------
# TEXT ANALYSIS FUNCS
# -------------------------------------------------------------------------

#' Erzeugt Samples basierend auf einem Text-Input. Input muss als Liste von Textsammlungen uebergeben werden.
#' Funktion erlaubt den Einsatz verschiedener Speicher-Funktionen. Ermoeglicht damit den flexiblen Einsatz von
#' write.csv(), write.csv2() für Windows-Files oder write.table(). 
sample_texts = function(texts, sample_size=100, seed=1337, filenames=NULL, call="write.csv", ...){
  
  # build texts
  lapply(seq(texts), function(idx){
    
    cat("Process subcorp ", idx, "/", length(texts), "\n", sep="")
    
    set.seed(seed)
    s_texts = sample(texts[[idx]], size=sample_size)
    
    if(!is.null(filenames)){
      filename = paste0(sub("[.]csv","",filenames[idx]), ".csv")
      args = list(..., x = s_texts, file=filename)
      do.call(call, args = args)
    }
    s_texts
  })
}


highlight = function(text, pattern, color=31){
  if(grepl("\\|",pattern)) pattern = strsplit(pattern,"|",fixed=T)[[1]]
  if(length(text)>1) text = paste(text, collapse = "\n\n")
  for(pat in pattern) text = gsub(pat,paste0("\033[0;",color,"m",pat,"\033[0m"), text)
  return(cat(text))
}


get_context = function(texts, pattern, windowsize=30, seperator=NULL, ignore.case=F, perl=F, offset=T){
  
  out = sapply(texts, function(text){
    
    # find target word
    match = regexpr(pattern, text, ignore.case=ignore.case, perl=perl)
    
    # if target word is found do:
    if(match[1]!=-1){
      
      # tokenize text
      words = gregexpr("\\b\\w+\\b", text)[[1]]
      
      # only keep words around target word
      keep = words[words > match-windowsize & words < match+windowsize]
      
      first = keep[1] # first word to keep
      last = keep[length(keep)] # last word to keep
      
      # add word length to last word index so it is part of the context
      last = last + attr(words, "match.length")[match(last, words)]
      
      # actually restrict context to rel context and remove whitespace
      keep = gsub("\\s$","",substr(text, first, last))
      
      # highlight target word
      if(!is.null(seperator)) sub(pattern, paste(seperator, pattern, seperator), keep) else keep
      
    }else ""
    
  }, USE.NAMES = F)
  
  if(offset){
    # center texts
    offsets = regexpr(pattern, out)
    center = max(offsets)
    
    # return
    out = paste0(strrep(" ", center-offsets), out)
  }
  return(out)
  
}



# -------------------------------------------------------------------------
# helper funcs
# -------------------------------------------------------------------------


#'as.textmeta
#'
#'transforms a named vector or a named list to a tosca::textmeta-file
#'
#'@param named_object Textfile to convert
#'@return textmeta-Objekt
#'@export
#'
as.textmeta = function(named_object){
  if(is.null(names(named_object))) names(named_object) = paste0("text_",seq(length(named_object)))
  
  if(!is.list(named_object)) named_object = as.list(named_object)
  return(
    tosca::textmeta(text = named_object,
                    meta = data.frame(id=names(named_object),
                                    date="",
                                    title="",
                                    fake="")
    )
  )
}


#'is.textmeta
#'
#'Testet, ob ein Objekt ein Textmeta-Objekt (im Sinne von tosca) ist
#'
#'@param obj Das Objekt, das getestet werden soll
#'@return TRUE oder FALSE
#'@examples is.textmeta(HB)
#'@export
#'
is.textmeta = function(obj){
  is.list(obj) && all(c("meta","text") %in% names(obj))
}



#'progress.initialize
#'
#'initializes progress bar
#'@param runner object that the loop is performed on
#'
progress.initialize = function(runner, track_time=F){
  
  # create new environment
  progress.environment <<- new.env()
  
  # save params to new environment
  local(runner <- runner, env=progress.environment) # running steps
  local(width <- round((2/3)*getOption("width")) +
          round((2/3)*getOption("width"))%%2,
        env=progress.environment) # width of prgress bar (even number forced)
  local(scaling <- width/length(runner), env=progress.environment) # scalar for each iteration
  local(index_to_indicate_progress <- 1, env=progress.environment) # start index
  
  invisible(TRUE)
}


#'progress.indicate
#'
#'plots progress bar
#'
progress.indicate = function(){
  
  # load params from environment
  runner = local(runner, env=progress.environment)
  width = local(width, env=progress.environment)
  scaling = local(scaling, env=progress.environment)
  index_to_indicate_progress = local(index_to_indicate_progress, env=progress.environment)
  track_time = local(track_time, env=progress.environment)
  
  # calculate progress
  progress = paste(floor(100*index_to_indicate_progress/length(runner)), "%  ")
  
  # print progress bar
  cat("\r",
      strrep("=", round(index_to_indicate_progress * scaling)),
      strrep(" ", round(width - index_to_indicate_progress*scaling)),
      " | ",
      progress, sep="")
  
  # update running parameter
  local(index_to_indicate_progress <- index_to_indicate_progress+1, env=progress.environment)
  
  # if done: reset running variable
  if(local(index_to_indicate_progress, env=progress.environment) == length(runner)+1){
    local(index_to_indicate_progress <- 1, env=progress.environment)
    cat("\n")
  }
  
  # return invisibly
  invisible(TRUE)
}
