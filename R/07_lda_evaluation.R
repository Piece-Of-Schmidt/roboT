#' Show top words and top titles for an LDA topic
#'
#' Prints a quick overview of the top words and the most representative document titles
#' for a given topic from an LDA result.
#'
#' @param corpus A `textmeta` object.
#' @param ldaresult Result object from `tosca::LDAgen()`.
#' @param ldaID Document IDs used in the LDA model.
#' @param n Number of top words and documents to show (default: 20).
#' @param elect Integer indices of the topics to inspect. If NULL (default), print all topics.
#' @return Printed output only; nothing returned.
#' @export
#'
#' @examples
#' # get_tw_and_titles(corpus, ldaresult, ldaID, topic = 3)
get_tw_and_titles = function(corpus, ldaresult, ldaID, n = 20, select = NULL){
  
  # get titles
  titles = tosca::topTexts(ldaresult, ldaID, limit=n)
  titles = corpus$meta$title[match(titles, corpus$meta$id)]
  
  # get topwords
  topwords = tosca::topWords(ldaresult[["topics"]], n)
  if (!is.null(select)) topwords = topwords[,select, drop=F]
  
  # print result
  lapply(seq(ncol(topwords)), function(idx){
    
    curr_topwords = topwords[,idx]
    curr_titles = matrix(titles, nrow=n)[,idx]
    width = round(max(nchar(curr_topwords)+4)/getOption("width"), 2)
    
    cat("\n\n")
    print_dataframe(data.frame(curr_topwords, curr_titles), line_char = NULL, col_prop = width, sep = " |  ")
  })
  
  # return topwords
  invisible(topwords)
}


#' Export top documents per LDA topic to Excel
#'
#' For each topic, retrieves the most representative documents and optionally translates
#' them via DeepL. Outputs enhanced data with topic relevance and source, and writes them
#' to a local Excel file.
#'
#' @param corpus A `textmeta` object or compatible list.
#' @param ldaresult LDA result from `tosca::LDAgen()`.
#' @param ldaID Vector of document IDs used in the model.
#' @param nTopTexts Number of top documents per topic (default: 50).
#' @param file Base filename for Excel export (default: `"topTexts"`).
#' @param translate Logical. Should text and titles be translated using DeepL?
#' @param max_text_length Maximum length of exported text strings (default: 32000).
#' @param source_lang Source language code (e.g., `"EN"`).
#' @param deepl_key DeepL API authentication key.
#' @return Invisibly returns a list of data frames with top texts per topic.
#' @export
#'
#' @examples
#' # lda_getTopTexts(corpus, ldaresult, ldaID, nTopTexts = 20)
lda_getTopTexts = function(corpus, ldaresult, ldaID, nTopTexts=50, file="topTexts",
                           translate=F, max_text_length=32000, source_lang=NULL, deepl_key=NULL){

  # safety belt
  if(missing("ldaID")){ldaID = names(corpus$text); warning("Missing ldaID. IDs of the corpus text are used as proxies.\nTrue IDs may differ!\nPlease check the generated top texts.")}
  if(missing("corpus") || missing(ldaresult)) stop("Insert correct arguments for corpus and ldaresult")
  if(translate && is.null(deepl_key)) stop("Insert valid deepl authentification key to use translation")

  # is the passed object a textmeta object?
  corp = is.textmeta(corpus)

  # convert to textmeta object, even if object is just a list of texts
  if(!corp) corpus = vec.as.textmeta(corpus)

  # generate data frame of topTexts
  tt = tosca::topTexts(ldaresult, ldaID, nTopTexts)
  tt = tosca::showTexts(corpus, tt)

  # shorten (and translate) texts
  tt = lapply(tt, function(topic){
    topic$text = substr(topic$text, 0, max_text_length)
    if(translate){
      topic$title = deeplr::toGerman2(topic$title, source_lang=source_lang, auth_key=deepl_key)
      topic$text = deeplr::toGerman2(topic$text, source_lang=source_lang, auth_key=deepl_key)
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


#' Export top words per LDA topic to Excel
#'
#' Retrieves the most frequent words for each topic from an LDA result, optionally
#' translates them, and saves the result to Excel or CSV.
#'
#' @param ldaresult LDA result from `tosca::LDAgen()`.
#' @param numWords Number of top words per topic (default: 50).
#' @param file Output file name (e.g., `"topwords.xlsx"` or `"topwords.csv"`).
#' @param translate Logical. Should words be translated using DeepL?
#' @param source_lang Source language code.
#' @param deepl_key DeepL API authentication key.
#' @return Invisibly returns a data frame of top words.
#' @export
#'
#' @examples
#' # lda_getTopWords(ldaresult, numWords = 30, file = "words.xlsx")
lda_getTopWords = function(ldaresult, numWords=50, file="topwords",
                           translate=F, source_lang=NULL, deepl_key=NULL){

  if(translate && is.null(deepl_key)) stop("Insert valid deepl authentification key to use translation")

  # create data frame
  topwords = tosca::topWords(ldaresult$topics, numWords)
  if(translate){
    topwords = deeplr::toGerman2(topwords, source_lang=source_lang, auth_key=deepl_key)
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


#' Export top documents per topic and time period
#'
#' Groups documents by time (e.g., month or quarter), then extracts and optionally saves
#' the most representative texts per topic and time unit.
#'
#' @param corpus A `textmeta` object.
#' @param ldaresult LDA result from `tosca::LDAgen()`.
#' @param ldaID Document IDs used in the model.
#' @param unit Time unit (`"month"`, `"bimonth"`, `"quarter"`, `"halfyear"`, `"year"`).
#' @param nTopTexts Number of top texts per topic and unit.
#' @param tnames Optional custom topic names.
#' @param foldername Output folder name. If `NULL`, no export is performed.
#' @param translate Logical. Translate texts and titles?
#' @param max_text_length Max. length of exported text.
#' @param source_lang Source language code.
#' @param deepl_key DeepL API key.
#' @return Invisibly returns a list of top texts or IDs per topic and time chunk.
#' @export
#'
#' @examples
#' # topTextsPerUnit(corpus, ldaresult, ldaID, unit = "quarter", foldername = "toptexts/")
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
        texts = tosca::showTexts(corpus, ids)

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
          texts$title = deeplr::toGerman2(texts$title, source_lang=source_lang, auth_key=deepl_key)
          texts$text = deeplr::toGerman2(texts$text, source_lang=source_lang, auth_key=deepl_key)
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


#' Compute top words per topic and time period
#'
#' Aggregates token assignments per time slice and extracts top words for each topic and period.
#' Optionally saves results to file and includes word weights.
#'
#' @param corpus A `textmeta` object.
#' @param ldaresult LDA result from `tosca::LDAgen()`.
#' @param docs Document-term list as used in `LDAprep()`.
#' @param unit Time unit for aggregation (e.g., `"quarter"`).
#' @param numWords Number of top words per topic (default: 50).
#' @param min_docs_per_chunk Number of words a chunks has to contain to be considered.
#' @param tnames Optional topic names.
#' @param values Logical. Should word weights be included in output?
#' @param file File path for saving (`.xlsx` or `.csv`).
#' @param verbose Print progress?
#' @return A list of top words (and optionally weights) per topic and time slice.
#' @importFrom utils write.csv
#' @export
#'
#' @examples
#' # topWordsPerUnit(corpus, ldaresult, docs, unit = "quarter", file = "tw.xlsx")
topWordsPerUnit = function(corpus, ldaresult, docs, unit="quarter", numWords=50, min_docs_per_chunk=50, tnames=NULL, values=F, file=NULL, verbose=T){
  
  # safety belt
  if(missing("corpus") || missing("ldaresult") || missing("docs")) stop("Insert arguments for corpus, ldaresult, and docs")
  stopifnot(is.textmeta(corpus))
  
  tw = ldaresult$topics
  
  # get params
  K = nrow(tw)
  if(is.null(tnames)) tnames = paste0("Topic", 1:K, ".", tosca::topWords(tw))
  assignments = ldaresult$assignments
  vocab = colnames(tw)
  ldaID = names(docs)
  
  # just to be sure: reorder meta so it matches the order of the ldaIDs
  corpus$meta = corpus$meta[match(ldaID, corpus$meta$id), ]
  corpus$meta = corpus$meta[order(corpus$meta$date), ]
  
  # create date chunks
  floor_dates = lubridate::floor_date(corpus$meta$date, unit)
  chunks = unique(floor_dates)
  chunks = chunks[table(floor_dates) > min_docs_per_chunk]
  
  # for every date chunk do the following
  topicsq = lapply(chunks, function(x){
    if(verbose) cat("\rcalculate top words for chunk", as.character(x))
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
  if(values){
    vals = lapply(1:K, function(k){ sapply(seq(topwordsq), function(t) topwordsq[[t]][[2]][,k])})
    vals = lapply(vals, \(v){colnames(v) = as.character(chunks); v} )
    out = list(words=out, vals=vals)
  }
  
  # save locally
  if(!is.null(file)){
    
    if(values){
      words=out[[1]]
      vals=out[[2]]
    } else words=out
    
    # transform to dataframe
    words = lapply(words, as.data.frame)
    
    # filename = paste0(sub(".xlsx","",file),".xlsx")
    if(grepl("csv$", file)){
      
      basename = sub("[.]csv$", "", file)
      for(topic_idx in seq_along(words)) write.csv(words[[topic_idx]], paste0(basename, "_topic", topic_idx, ".csv"))
      if(values) for(topic_idx in seq_along(words)) write.csv(vals[[topic_idx]], paste0(basename, "_topic", topic_idx, "_values", ".csv"))
      
      
    }else{
      
      basename = sub("[.]xlsx$", "", file)
      writexl::write_xlsx(words, paste0(basename, ".xlsx"))
      if(values) writexl::write_xlsx(vals, paste0(basename, "_values.xlsx"))
      
    }
  }
  
  invisible(out)
}
