# easy function that gives quick overview over a selected topic
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
  if(missing("corpus") | missing(ldaresult)|!is.textmeta(corpus)) stop("Insert correct arguments for corpus, ldaresult and topic")
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
    # progress.indicate()
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


# perform multiple LDAs with different settings
multipleLDAs = function(docs, vocab, ..., func="LDAgen", runs="all", seed=1337, savelogs=T){
  
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