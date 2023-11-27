# load packages
require(lubridate, quietly = T)
require(tosca, quietly = T)
require(writexl, quietly = T)


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
lda_getTopTexts = function(corpus, ldaresult, ldaID, nTopTexts=50, file="topTexts"){
  
  # safety belt
  if(missing("ldaID")){ldaID = names(corpus$text); warning("Missing ldaID. IDs of the corpus text are used as proxies.\nTrue IDs may differ!\nPlease check the generated top texts.")}
  if(missing("corpus") || missing(ldaresult)) stop("Insert correct arguments for corpus and ldaresult")

  # is the passed object a textmeta object?
  corp = is.textmeta(corpus)
  
  # convert to textmeta object, even if object is just a list of texts
  if(!corp) corpus = as.textmeta(corpus)
  
  # generate data frame of topTexts
  tt = tosca::topTexts(ldaresult, ldaID, nTopTexts)
  tt = tosca::showTexts(corpus, tt)
  
  # shorten texts
  tt = lapply(tt, function(topic){ topic$text = substr(topic$text, 0, 32000); topic })
  
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
lda_getTopWords = function(ldaresult, numWords=50, file="topwords"){
  
  # create data frame
  topwords = tosca::topWords(ldaresult$topics, numWords)
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
topTextsPerUnit = function(corpus, ldaresult, ldaID, unit="quarter", nTopTexts=20, tnames=NULL, foldername=NULL){
  
  # safety belt
  if(missing("corpus") | missing(ldaresult)|!robot::is.textmeta(corpus)) stop("Insert correct arguments for corpus, ldaresult and topic")
  if(missing("ldaID")){ldaID = names(corpus$text); warning("Missing ldaID. IDs of the corpus text are used as proxies.\nTrue IDs may differ!\nPlease check the generated top texts.")}
  if(is.null(tnames)) tnames = paste0("Topic", 1:K, ".", tosca::topWords(ldaresult$topics))
  if(!is.null(foldername)) dir.create(foldername)

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
      proms = order(topic, decreasing = T)[seq(nTopTexts)]
      theta_vals = topic[proms]
      
      # get corresponding text IDs
      ids = ldaID[mask][proms]
      ids = ids[!is.na(ids)]
      
      if(!is.null(foldername)){
        
        # get text IDs
        texts = showTexts(corpus, ids)
        
        # add theta value
        texts[, "topic_relevance"] = round(theta_vals[seq(nTopTexts)], 2)
        
        # add resource
        if("resource" %in% names(corpus$meta)){
          mask = match(texts[,"id"], corpus$meta$id)
          texts[,"source"] = corpus$meta$resource[mask]
          texts = texts[,c(1,2,6,5,3,4)]} else texts = texts[,c(1,2,5,3,4)]
        
        # shorten texts
        texts$text = substr(texts$text, 0, 32000)
        
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
  if(is.null(tnames)) tnames = paste0("Topic", 1:K, ".", tosca::topWords(tw))
  
  # get params
  K = nrow(tw)
  assignments = ldaresult$assignments
  vocab = colnames(tw)
  
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
