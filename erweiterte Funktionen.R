
# packages installieren und einladen
if(! require("tosca", character.only = T, quietly = T)) install.packages("tosca")
if(! require("writexl", character.only = T, quietly = T)) install.packages("writexl")
#'lda_getTopTexts
#'
#'Generiert die Top-Texte (n=100) einer LDA und speichert sie in einem entsprechenden Ordner auf dem Rechner ab. Fuehrt die tosca-Funktionen "topTexts()" und "showTexts()" durch
#'
#'@param corpus Ausgangskorpus als meta-Datei. Sollte im Vorhinein um Duplikate bereinigt werden
#'@param ldaresult Objekt, das die tosca-Funktion "LDAgen()" generiert
#'@param ldaID IDs von Texten, die beruecksichtigt werdeb sollen. Default: Alle Texte
#'@param nTopTexts Menge an TopTexts, die generiert wird
#'@param file Dateiname
#'
lda_getTopTexts = function(corpus, ldaresult, ldaID, nTopTexts=50, file="topTexts"){
  
  corp=T
  if(!is.textmeta(corpus)){corpus = as.textmeta(corpus); corp=FALSE}
  if(missing("ldaID")){ldaID = names(corpus$text); warning("Missing ldaID. IDs of the corpus text are used as proxies.\nTrue IDs may differ!\nPlease check the generated top texts.")}
  if(missing("corpus")|missing(ldaresult)) stop("Insert correct arguments for corpus and ldaresult")
  if(!require("writexl", character.only = T, quietly = T)){
    install = as.logical(as.numeric(readline("Package 'writexl' is not installed but required. Shall it be installed now? (NO: 0, YES: 1)  ")))
    if(install) install.packages("writexl") else break
  }
  
  require(writexl, quietly = T)
  require(tosca, quietly = T)
  
  # generate data frame of topTexts
  tt = topTexts(ldaresult, ldaID, nTopTexts)
  tt = showTexts(corpus, tt)
  
  # add share of most prominent topic per tt to data frame
  docs_per_topic = ldaresult$document_sums/rowSums(ldaresult$document_sums)
  docs_per_topic = apply(docs_per_topic, 2, function(x) x/sum(x))
  proms = apply(docs_per_topic, 1, function(x) round(sort(x,decreasing = T)[1:nTopTexts],2))
  for(i in 1:length(tt)){tt[[i]][,"topic_relevance"] = proms[,i]; tt[[i]] = tt[[i]][,c(1,2,5,3,4)]}
  
  # add resource to data frame
  if("resource" %in% names(corpus$meta)){
    tt = lapply(tt, function(x){
      x[,"source"] = corpus$meta$resource[match(x[,"id"],corpus$meta$id)]
      x[,c(1,2,3,6,4,5)]})}
  if(!corp) tt = lapply(tt, function(topic) topic[,c(1,3,5)])
  
  # save locally
  if(!is.null(file)) writexl::write_xlsx(tt, paste0(sub(".xlsx","",file),".xlsx"))
  
  invisible(tt)
}


#'lda_getTopWords
#'
#'Generiert ein Excel-Sheet mit den topwords einer LDA
#'
#'@param ldaresult Objekt, das die tosca-Funktion "LDAgen()" generiert
#'@param numWords Anzahl der topwords pro Topic
#'@param file Dateiname, unter dem das Excel-Sheet gespeichert werden soll
#'
lda_getTopWords = function(ldaresult, numWords=50, file="topwords"){
  
  if(!require("writexl", character.only = T, quietly = T)){
    install = as.logical(as.numeric(readline("Package 'writexl' is not installed but required. Shall it be installed now? (NO: 0, YES: 1)  ")))
    if(install) install.packages("writexl") else break
  }
  require(tosca, quietly = T)
  require(writexl, quietly = T)
  
  topwords = topWords(ldaresult$topics, numWords)
  rel = round(rowSums(ldaresult$topics)/sum(ldaresult$topics),3)
  topwords = as.data.frame(rbind(rel,topwords))
  colnames(topwords) = paste("Topic",1:ncol(topwords))
  rownames(topwords) = NULL
  
  # save locally
  if(!is.null(file)) writexl::write_xlsx(topwords, paste0(sub(".xlsx","",file),".xlsx"))
  
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
topTextsPerUnit = function(corpus, ldaresult, ldaID, unit="quarter", nTopTexts=20, tnames=NULL, foldername=NULL, s.date=min(corpus$meta$date, na.rm=T), e.date=max(corpus$meta$date, na.rm=T)){
  
  if(missing("corpus") | missing(ldaresult)|!robot::is.textmeta(corpus)) stop("Insert correct arguments for corpus, ldaresult and topic")
  require(tosca, quietly = T)
  require(lubridate, quietly = T)
  
  # get params
  K = nrow(ldaresult$topics)
  if(missing("ldaID")){ldaID = names(corpus$text); warning("Missing ldaID. IDs of the corpus text are used as proxies.\nTrue IDs may differ!\nPlease check the generated top texts.")}
  doc = ldaresult$document_sums
  colnames(doc) = as.character(corpus$meta$date[match(ldaID,corpus$meta$id)])
  if(is.null(tnames)) tnames = paste0("Topic",1:K,".",topWords(ldaresult$topics))
  if(!is.null(foldername)) dir.create(foldername)
  
  # create date chunks
  q = c(month=1,months=1,bimonth=2,bimonths=2,quarter=3,quarters=3,year=12,years=12)[unit]
  chunks = seq.Date(s.date, e.date, unit)
  
  progress.initialize(chunks)
  out = lapply(chunks, function(chunk){
    
    chunk = as.Date(chunk)
    currSpan = chunk < as.Date(colnames(doc)) & as.Date(colnames(doc)) < chunk+months(q)
    temp = doc[, currSpan]
    docs_per_topic = apply(temp, 2, function(x) x/sum(x))
    
    temp = apply(docs_per_topic, 1, function(x){
      
      # get most prominent texts
      proms = order(x,decreasing = T)[1:nTopTexts]
      ids = ldaID[currSpan][proms]
      ids = ids[!is.na(ids)]
      
      if(!is.null(foldername)){
        
        # get text IDs
        texts = showTexts(corpus, ids)
        
        # add theta value
        texts[,"topic_relevance"] = round(sort(x,decreasing = T)[1:nTopTexts],2)
        
        # add resource
        if("resource" %in% names(corpus$meta)){
          texts[,"source"] = corpus$meta$resource[match(texts[,"id"],corpus$meta$id)]
          texts = texts[,c(1,2,6,5,3,4)]} else texts = texts[,c(1,2,5,3,4)]
        
        # shorten texts
        texts$text = substr(texts$text, 0, 32000)
        
        # return 
        texts
        
      }else ids

    })
    names(temp) = tnames
    
    # save locally
    if(!is.null(foldername)) writexl::write_xlsx(temp, paste0(foldername,"/",chunk,".xlsx"))
    
    progress.indicate()
    temp
  })
  
  if(is.null(foldername)){
    out = lapply(1:K, function(k){
      sapply(1:length(chunks), function(t) out[[t]][,k])
    })
    out = lapply(out, function(x){colnames(x)=as.character(chunks); x})
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
#'@param s.date optional: Start-Datum, Wenn nur ein Teil der TopTexte von Interesse ist
#'@param e.date optional: End-Datum, Wenn nur ein Teil der TopTexte von Interesse ist
#'@param saveRAW Wenn TRUE werden die in der Analyse erzeugten Zwischenergebnisse ins Environment geladen
#'@param file Dateiname, unter dem das Excel-Sheet gespeichert werden soll
#'
topWordsPerUnit = function(corpus, ldaresult, docs, unit="quarter", numWords=50, tnames=NULL, values=T, s.date=NULL, e.date=NULL, saveRAW=F, file=NULL){
  
  if(missing("corpus") || missing("ldaresult") || missing("docs")) stop("Insert arguments for corpus, ldaresult, and docs")
  
  if(!is.null(file) && !require("writexl", character.only = T, quietly = T)){
    install = as.logical(as.numeric(readline("Package 'writexl' is not installed but required. Shall it be installed now? (NO: 0, YES: 1)  ")))
    if(install) install.packages("writexl") else break
  }
  library(lubridate)
  library(tosca)
  require(writexl, quietly = T)
  
  K = nrow(ldaresult$topics)
  assignments = ldaresult$assignments
  vocab = colnames(ldaresult$topics)
  if(is.null(tnames)) tnames = paste0("Topic",1:K,".",topWords(ldaresult$topics)) else tnames = tnames
  
  date_chunks = lubridate::floor_date(corpus$meta$date[match(names(docs), corpus$meta$id)], unit)
  chunks = unique(date_chunks); min = chunks[1]; max = tail(chunks,1)
  if(!is.null(s.date)) min = floor_date(as.Date(s.date),unit); if(!is.null(e.date)) max = floor_date(as.Date(e.date),unit)
  chunks = chunks[chunks >= min & chunks <= max]
  
  topicsq = lapply(chunks, function(x){
    tmp = table(factor(unlist(assignments[date_chunks == x])+1, levels = 1:K),
                factor(unlist(lapply(docs[date_chunks == x], function(y) y[1,]))+1, levels = seq(length(vocab))))
    tmp = matrix(as.integer(tmp), nrow = K)
    colnames(tmp) = vocab
    tmp
  })
  if(saveRAW) names(topicsq)=chunks; ttm <<- topicsq
  topwordsq = lapply(topicsq, topWords, numWords = numWords, values = T)
  names(topwordsq)=chunks
  
  out = lapply(1:K, function(k) sapply(seq(topwordsq), function(t) topwordsq[[t]][[1]][,k]))
  out = lapply(out, function(x){ colnames(x)=as.character(chunks); x})
  names(out) = tnames
  if(values) out = list(words=out, vals=lapply(1:K, function(k) sapply(seq(topwordsq), function(t) topwordsq[[t]][[1]][,k])))
  
  if(!is.null(file)){
    
    # save locally
    if(values) words=out[[1]] else words=out
    words = lapply(words, as.data.frame)
    writexl::write_xlsx(words, paste0(sub(".xlsx","",file),".xlsx"))
    
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
    tosca::textmeta(text=named_object,
                    meta=data.frame(id=names(named_object),
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
  is.list(obj) & all(c("meta","text") %in% names(obj))
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

