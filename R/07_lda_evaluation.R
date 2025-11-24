#' Show top words and top titles for an LDA topic
#'
#' Prints a quick overview of the top words and the most representative document titles
#' for a given topic from an LDA result.
#'
#' @param corpus A `textmeta` object.
#' @param ldaresult Result object from `tosca::LDAgen()`.
#' @param ldaID Document IDs used in the LDA model.
#' @param n Number of top words and documents to show (default: 20).
#' @param select Integer indices of the topics to inspect. If NULL (default), print all topics.
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
lda_getTopTexts = function(corpus, ldaresult, ldaID, nTopTexts=50, file="topTexts", select_ids=NULL,
                           translate=F, max_text_length=32000, source_lang=NULL, deepl_key=NULL){

  # safety belt
  if(missing("ldaID")){ldaID = names(corpus$text); warning("Missing ldaID. IDs of the corpus text are used as proxies.\nTrue IDs may differ!\nPlease check the generated top texts.")}
  if(missing("corpus") || missing(ldaresult)) stop("Insert correct arguments for corpus and ldaresult")
  if(translate && is.null(deepl_key)) stop("Insert valid deepl authentification key to use translation")

  # is the passed object a textmeta object?
  corp = is.textmeta(corpus)

  # convert to textmeta object, even if object is just a list of texts
  if(!corp) corpus = vec.as.textmeta(corpus)

  # select relevant documents
  if(!is.null(select_ids)) ldaresult$document_sums = ldaresult$document_sums[, match(select_ids, ldaID)]

  # generate data frame of topTexts. Since NA cases can occure: identify 10x top texts and only return top nTopTexts docs
  effect_nTopTexts = min(nTopTexts*10, ncol(ldaresult$document_sums))
  tt = tosca::topTexts(ldaresult, ldaID, effect_nTopTexts)
  tt = apply(tt, 2, \(col) na.omit(col)[1:nTopTexts])
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
#' # lda_getTopWordsPerUnit(corpus, ldaresult, docs, unit = "quarter", file = "tw.xlsx")
lda_getTopWordsPerUnit = function(corpus, ldaresult, docs, unit="quarter", numWords=50, min_docs_per_chunk=1, tnames=NULL, values=F, file=NULL, verbose=T){
  
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
  if(verbose) cat("\n")
  
  topwordsq = lapply(topicsq, tosca::topWords, numWords = numWords, values = T)
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


# top texts per quarter ---------------------------------------------------

.safe_name = function(x, maxlen = 80) {
  x = gsub("[^[:alnum:]/_\\-\\.]+", "_", x)
  x = gsub("_+", "_", x)
  substr(x, 1, maxlen)
}

.build_text_table = function(corpus, ids, theta_vals, max_text_length, translate, source_lang, deepl_key) {
  
  if (length(ids) == 0) return(data.frame())
  texts = tosca::showTexts(corpus, ids)
  
  # add theta value (topic relevance)
  texts[,"topic_relevance"] = round(theta_vals[seq_len(nrow(texts))], 2)
  
  # add resource column if present
  if ("resource" %in% names(corpus$meta)) {
    m = match(texts[,"id"], corpus$meta$id)
    texts[,"source"] = corpus$meta$resource[m]
    # reorder: id, date, source, topic_relevance, title, text
    texts = texts[, c("id","date","source","topic_relevance","title","text")]
  } else {
    # reorder: id, date, topic_relevance, title, text
    texts = texts[, c("id","date","topic_relevance","title","text")]
  }
  
  # shorten (& optionally translate)
  texts$text = substr(texts$text, 0, max_text_length)
  if (isTRUE(translate)) {
    texts$title = deeplr::toGerman2(texts$title, source_lang = source_lang, auth_key = deepl_key)
    texts$text  = deeplr::toGerman2(texts$text,  source_lang = source_lang, auth_key = deepl_key)
  }
  texts
}

# core workers

toptextsperunit_groupbydate = function(corpus, doc, ldaID, select_topics, chunks, floor_dates,
                                       tnames, nTopTexts, foldername, translate,
                                       max_text_length, source_lang, deepl_key,
                                       verbose = TRUE) {
  
  out = lapply(chunks, function(chunk) {
    
    if (verbose) cat("\rcalculate top texts for chunk", as.character(chunk))
    
    mask = floor_dates == chunk
    if (!any(mask)) {
      # for each Topic: return empty element
      empty_el = if (is.null(foldername)) character(0) else data.frame()
      temp = rep(list(empty_el), length(select_topics))
      if (!is.null(foldername)) {
        names(temp) = tnames
        file_chunk = paste0(.safe_name(foldername), "/", .safe_name(as.character(chunk)), ".xlsx")
        suppressWarnings(dir.create(foldername, showWarnings = FALSE, recursive = TRUE))
        writexl::write_xlsx(temp, path = file_chunk)
      }
      if (verbose) cat("\n")
      return(temp)
    }
    
    # normalize per document (columns)
    docs_per_topic = apply(doc[select_topics, mask, drop = FALSE], 2, function(x) {
      s = sum(x)
      if (s == 0) rep(0, length(x)) else x / s
    })
    
    # for each topic (row) pick top docs in that chunk
    temp = apply(docs_per_topic, 1, function(topic_vec) {
      
      proms = order(topic_vec, decreasing = TRUE)
      if (length(proms) == 0) return(if (is.null(foldername)) character(0) else data.frame())
      
      take = seq_len(min(nTopTexts, length(proms)))
      proms = proms[take]
      theta_vals = topic_vec[proms]
      
      ids = ldaID[mask][proms]
      ids = ids[!is.na(ids)]
      
      if (!is.null(foldername)) {
        .build_text_table(corpus, ids, theta_vals, max_text_length, translate, source_lang, deepl_key)
      } else {
        ids
      }
    })
    if(verbose) cat("\n")
    
    # write 1 xlsx per CHUNK
    if (!is.null(foldername)) {
      names(temp) = tnames
      file_chunk = paste0(.safe_name(foldername), "/", .safe_name(as.character(chunk)), ".xlsx")
      suppressWarnings(dir.create(foldername, showWarnings = FALSE, recursive = TRUE))
      writexl::write_xlsx(temp, path = file_chunk)
    }
    temp
  })
  
  names(out) = as.character(chunks)
  out
}

toptextsperunit_groupbytopic = function(corpus, doc, ldaID, select_topics, chunks, floor_dates,
                                        tnames, nTopTexts, foldername, translate,
                                        max_text_length, source_lang, deepl_key,
                                        verbose = TRUE) {
  
  if (is.null(foldername))
    stop("groupby='topic' ist nur sinnvoll mit 'foldername' (wir schreiben 1 Datei pro Topic).")
  
  suppressWarnings(dir.create(foldername, showWarnings = FALSE, recursive = TRUE))
  
  # Precompute per-chunk normalized matrices so wir normalisieren nur einmal pro Chunk
  norm_by_chunk = lapply(chunks, function(chunk) {
    mask = floor_dates == chunk
    if (!any(mask)) return(NULL)
    apply(doc[select_topics, mask, drop = FALSE], 2, function(x){
      s = sum(x); if (s == 0) rep(0, length(x)) else x / s
    })
  })
  names(norm_by_chunk) = as.character(chunks)
  
  # iterate topics
  invisible(lapply(select_topics, function(k) {
    
    if (isTRUE(verbose)) cat("\rassemble workbook for topic", k, "of", length(select_topics))
    
    # collect one sheet per chunk for this topic
    sheets = lapply(seq_along(chunks), function(i) {
      M = norm_by_chunk[[i]]
      if (is.null(M)) return(data.frame())   # chunk without docs
      
      topic_vec = M[which(select_topics == k), , drop = TRUE]       # relevance of this topic across docs in chunk
      proms = order(topic_vec, decreasing = TRUE)
      if (length(proms) == 0) return(data.frame())
      
      take = seq_len(min(nTopTexts, length(proms)))
      proms = proms[take]
      theta_vals = topic_vec[proms]
      
      mask = floor_dates == chunks[i]
      ids  = ldaID[mask][proms]
      ids  = ids[!is.na(ids)]
      
      .build_text_table(corpus, ids, theta_vals, max_text_length, translate, source_lang, deepl_key)
    })
    
    names(sheets) = vapply(as.character(chunks), function(x) substr(.safe_name(x, 31), 1, 31), "")
    # Empty workbooks are still written with empty sheets
    topic_name = if (length(tnames) >= k) tnames[k] else paste0("Topic_", k)
    file_topic = file.path(.safe_name(foldername),
                           paste0(.safe_name(topic_name), ".xlsx"))
    
    writexl::write_xlsx(sheets, path = file_topic)
    NULL
  }))
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
#' @param tnames Optional custom topic names (used also for file names with groupby="topic").
#' @param groupby "date" (default: 1 Datei pro Zeit-Chunk) oder "topic" (neu: 1 Datei pro Topic).
#' @param foldername Output folder name. If `NULL`, no export is performed.
#' @param translate Logical. Translate texts and titles?
#' @param max_text_length Max. length of exported text.
#' @param source_lang Source language code.
#' @param deepl_key DeepL API key.
#' @param verbose Print progress?
#' @return Invisibly returns a nested list (IDs if no `foldername`; when exporting, returns (invisibly) the in-memory structure).
#' @export
lda_getTopTextsPerUnit = function(corpus, ldaresult, ldaID,
                                  unit = "quarter",
                                  nTopTexts = 20,
                                  tnames = NULL,
                                  groupby = c("date", "topic"),
                                  select_topics = 1:nrow(ldaresult$topics),
                                  select_dates = NULL,
                                  foldername = NULL,
                                  translate = FALSE,
                                  max_text_length = 32000,
                                  source_lang = NULL,
                                  deepl_key = NULL,
                                  verbose = TRUE) {
  
  # safety belt
  if (missing(corpus) || missing(ldaresult) || !tosca::is.textmeta(corpus))
    stop("Insert correct arguments for corpus, ldaresult and topic")
  if (missing(ldaID)) {
    ldaID = names(corpus$text)
    warning("Missing ldaID. IDs of the corpus text are used as proxies.\nTrue IDs may differ!\nPlease check the generated top texts.")
  }
  if (!is.null(foldername) && !dir.exists(foldername)) dir.create(foldername, recursive = TRUE)
  if (isTRUE(translate) && is.null(deepl_key))
    stop("Insert valid deepl authentification key to use translation")
  
  groupby = match.arg(groupby)
  
  # params from ldaresult
  doc = ldaresult$document_sums  # assumed: rows = topics, cols = docs
  
  # default topic names
  if (is.null(tnames)) {
    twords = tosca::topWords(ldaresult$topics)
    tnames = paste0("Topic", select_topics, ".", twords[select_topics])
  } else if (length(tnames) == K_total) {
    tnames = tnames[select_topics]
  } else if (length(tnames) != length(select_topics)) {
    stop("Length of tnames must be either K (all topics) or length(select_topics).")
  }
  
  # align meta to ldaID order (very important)
  corpus$meta = corpus$meta[match(ldaID, corpus$meta$id), ]
  
  # date chunks
  floor_dates = lubridate::floor_date(corpus$meta$date, unit)
  chunks = if(!is.null(select_dates)) unique(lubridate::floor_date(select_dates, unit)) else unique(floor_dates)
  
  # branch
  if (groupby == "date") {
    out = toptextsperunit_groupbydate(
      corpus, doc, ldaID, select_topics, chunks, floor_dates,
      tnames, nTopTexts, foldername, translate,
      max_text_length, source_lang, deepl_key,
      verbose = verbose
    )
    
    # if no export: return IDs rearranged as in original API (topics × chunks)
    if (is.null(foldername)) {
      K = length(select_topics)
      reshaped = lapply(seq_len(K), function(k) {
        sapply(seq_along(chunks), function(ti) out[[ti]][[k]])
      })
      reshaped = lapply(reshaped, function(x) { colnames(x) = as.character(chunks); x })
      names(reshaped) = tnames
      return(invisible(reshaped))
    } else {
      return(invisible(out))
    }
    
  } else { # groupby == "topic"
    toptextsperunit_groupbytopic(
      corpus, doc, ldaID, select_topics, chunks, floor_dates,
      tnames, nTopTexts, foldername, translate,
      max_text_length, source_lang, deepl_key,
      verbose = verbose
    )
    return(invisible(NULL))
  }
}



# topic cosines ---------------------------------------------------

#' get relevant data to print quarter-to-quarter cosine similarities of topics
#'
#' @param rollinglda_out RollingLDA output object containing topic assignments, vocabulary, and document dates.
#' @param pm.backend Parallelization backend to be used for cosine similarity calculation (e.g., `"socket"` or `"multicore"`).
#' @param ncpus Number of CPU cores to use for parallel computation.
#'
#' @return Returns a list with relevant data for plotting topic similarities, including:
#' \itemize{
#'   \item \code{xquarter}: vector of quarter timestamps,
#'   \item \code{sims}: list of cosine similarity matrices per topic,
#'   \item \code{valq}: matrix of adjacent-quarter similarities,
#'   \item \code{valq_first}: similarity values of each quarter to the first quarter,
#'   \item \code{valq_last}: similarity values of each quarter to the last quarter.
#' }
#' @export
get_simdata = function(rollinglda_out, pm.backend = "socket", ncpus = 1){
  
  # get RollingLDA data
  nks = getK(getLDA(rollinglda_out))
  assignments = getAssignments(getLDA(rollinglda_out))
  docs = getDocs(rollinglda_out)
  vocab = getVocab(rollinglda_out)
  dates = getDates(rollinglda_out)
  
  # extract quarters
  quarters = lubridate::floor_date(dates, "quarter")
  xquarter = sort(unique(quarters))
  nquarter = length(xquarter)
  
  # get topic assignments per quarter
  topicsq = lapply(xquarter, function(x){
    tmp = table(factor(unlist(assignments[quarters == x])+1, levels = 1:nks), 
                factor(unlist(lapply(docs[quarters == x], function(y) y[1,]))+1,
                       levels = seq_len(length(vocab))))
    tmp = matrix(as.integer(tmp), nrow = nks)
    colnames(tmp) = vocab
    tmp
  })
  
  topics = lapply(1:nks, function(k){
    tmp = sapply(topicsq, function(x) x[k,])
    colnames(tmp) = paste0("Q", 1:nquarter)
    tmp
  })
  
  # calculate simils
  sims = lapply(1:nks, function(k){
    ldaPrototype::cosineTopics(topics[[k]], pm.backend = pm.backend, ncpus = ncpus)
  })
  
  valq = sapply(sims, function(x) c(NA, x$sims[cbind(2:nquarter,2:nquarter-1)]))
  valq_first = sapply(sims, function(x) x$sims[,1])
  valq_last = sapply(sims, function(x) x$sims[nquarter,])
  
  # return
  return(list(xquarter=xquarter, sims=sims, valq=valq, valq_first=valq_first, valq_last=valq_last))
  
}


#' Print quarter-to-quarter cosine similarities of topics
#'
#' @param simdata Result of get_simdata() function.
#' @param row Number of rows pro plot grid.
#' @param col Number of columns pro plot grid.
#' @param labels Topic names.
#' @param title Plot title.
#' @param xlab xlab of plot.
#' @param ylab ylab of plot.
#' @param filename Name of file. If `NULL`, no export is performed.
#' @param width Width of plot.
#' @param height Height of plot.
#' @return Invisibly returns plot file.
#' @export
print_sims = function(simdata, nrow, ncol, labels=NULL, title="", xlab="", ylab="", filename=NULL, width=8, height=10){
  
  xquarter = simdata$xquarter
  valq = simdata$valq
  valq_first = simdata$valq_first
  valq_last = simdata$valq_last
  topics = 1:length(simdata[["sims"]])
  
  if(is.null(labels)) labels = paste0("Topic",topics)
  xmin = min(xquarter)
  cosine_quarterly = ggmatrix(lapply(topics, function(i){
    ggplot() + geom_line(aes(x = xquarter, y = valq[,i]), col = "darkgrey") + ylim(c(0,1)) +
      geom_line(aes(x = xquarter, y = valq_first[,i], col = "green")) +
      geom_line(aes(x = xquarter, y = valq_last[,i], col = "red")) +
      geom_line(aes(x = xquarter, y = valq[,i])) +
      annotate("text", x = xmin, y = 0.05, label = labels[i], hjust = 0, vjust = 0)
  }), nrow = nrow, ncol = ncol, ylab = ylab, xlab = xlab, title = title)
  
  # print
  if(!is.null(filename)){
    pdf(paste0(sub("([.]\\w{3})$","",filename), ".pdf"), width = width, height = height)
    print(cosine_quarterly)
    dev.off()
  }
  
  # return
  print(cosine_quarterly)
  invisible(cosine_quarterly)

}


# decompose LDA result ---------------------------------------------------


#' Build a lookup table for LDA decomposition
#'
#' Creates a standardized lookup table linking document IDs to groups
#' (e.g., labels, clusters, categories) and optionally dates.  
#' The lookup table is required by `decompose_lda()` to aggregate topic
#' contributions by metadata.
#'
#' You can either pass a data frame containing the relevant columns
#' (`id`, `group`, and optionally `date`), or you can supply vectors
#' (`ids`, `groups`, `dates`) directly.
#'
#' @param ids Character or numeric vector of document IDs.  
#'   Required if `frame` is not provided.
#' @param groups Vector specifying group membership for each document  
#'   (e.g., class labels, newspapers, time periods).
#' @param dates Optional vector of dates for each document  
#'   (used to aggregate topics over time).
#' @param frame Optional data frame containing columns `id`, `group`,
#'   and optionally `date`. If provided, it overrides `ids`, `groups`,
#'   and `dates`.
#'
#' @return A data frame with columns:
#'   \describe{
#'     \item{id}{Document ID}
#'     \item{group}{Group label}
#'     \item{date}{Optional document date}
#'   }
#'
#' @examples
#' # Using separate vectors
#' lookup <- build_lookup(
#'   ids = c("d1", "d2", "d3"),
#'   groups = c("A", "B", "A"),
#'   dates = as.Date(c("2020-01-01", "2020-01-02", "2020-01-03"))
#' )
#'
#' # Using an existing data frame
#' df <- data.frame(
#'   id = 1:3,
#'   group = c("x", "y", "x"),
#'   date = Sys.Date() + 0:2
#' )
#' lookup2 <- build_lookup(frame = df)
#'
#' @export
build_lookup = function(ids = NULL,
                        groups = NULL,
                        dates = NULL,
                        frame = NULL) {
  
  # Fall 1: data.frame wird übergeben
  if (!is.null(frame)) {
    rel_columns = intersect(colnames(frame), c("id", "group", "date"))
    if (!all(c("id", "group") %in% rel_columns)) {
      stop("If 'frame' is used, it must contain at least 'id' and 'group' columns.")
    }
    out = frame[, rel_columns]
    
  } else {
    # Fall 2: ids / groups (und evtl. dates) werden direkt übergeben
    if (is.null(ids) || is.null(groups)) {
      stop("Either 'frame' or both 'ids' and 'groups' must be supplied.")
    }
    if (!is.null(dates) &&
        !all(length(ids) == length(groups), length(ids) == length(dates))) {
      stop("'ids', 'groups' and 'dates' (if given) must have the same length.")
    }
    out = data.frame(
      id    = ids,
      group = groups,
      date  = if (!is.null(dates)) dates else NA
    )
  }
  
  return(out)
}


#' Decompose an LDA topic-document matrix by groups and dates
#'
#' Takes a document-topic matrix (rows = topics, columns = documents),
#' merges it with document metadata (e.g., groups and dates),
#' and aggregates topic weights for each combination of
#' `date × group × topic`.  
#'
#' This function can return either a long-format table or a wide-format
#' table, and it optionally plots topic trends over time.
#'
#' You can supply any aggregation function via `fun`, such as
#' `sum`, `mean`, `median`, `sd`, or a custom function.
#'
#' @param document_topic_matrix Matrix with topics in rows and documents in columns.
#' @param lookup_dict Data frame created via `build_lookup()` containing
#'   columns `id`, `group`, and `date`.
#' @param select Integer vector specifying which topics (rows) to include.
#' @param tnames Optional custom topic names (must be same length as `select`).
#' @param fun Aggregation function applied to topic weights within each
#'   `date × group × topic` combination.  
#'   Can be `sum`, `mean`, `"sum"`, `"mean"`, or any function like
#'   `function(x) max(x) - min(x)`.
#' @param plot_by Character: one of `"group"`, `"topic"`, or `"none"`.  
#'   Determines which variable is used as color in the plot.
#' @param out `"long"` or `"wide"`.  
#'   `"long"` returns a tidy data frame;  
#'   `"wide"` returns one column per topic.
#' @param span Smoothing parameter for the LOESS smoothing line.
#'
#' @return Either:
#' \describe{
#'   \item{long}{A long-format data frame with columns `date`, `group`,
#'     `topic`, and aggregated `doc_count`.}
#'   \item{wide}{A wide-format data frame with one column per topic.}
#' }
#'
#' If `plot_by != "none"`, a ggplot2 trend plot is also displayed.
#'
#' @examples
#' # Suppose dtm is a topic × document matrix from an LDA model
#' # and lookup is created with build_lookup().
#'
#' # Long-format summary using mean aggregation
#' res <- decompose_lda(dtm, lookup, fun = mean, out = "long")
#'
#' # Wide-format summary using sum aggregation
#' res2 <- decompose_lda(dtm, lookup, fun = sum, out = "wide")
#'
#' # Custom aggregation (e.g., max-minus-min)
#' res3 <- decompose_lda(dtm, lookup, fun = function(x) max(x) - min(x))
#'
#' @export
decompose_lda = function(document_topic_matrix,
                         lookup_dict,
                         select = 1:nrow(document_topic_matrix),
                         tnames = paste0("topic", select),
                         fun = "sum",
                         plot_by = c("group", "topic", "none"),
                         out = c("long", "wide"),
                         span = 0.1) {
  
  out     = match.arg(out)
  plot_by = match.arg(plot_by)
  fun     = match.fun(fun)
  
  # Checks
  if (!"id" %in% colnames(lookup_dict)) {
    stop("'lookup_dict' must contain a column 'id'.")
  }
  if (!all(c("group", "date") %in% colnames(lookup_dict))) {
    stop("'lookup_dict' must contain 'group' and 'date' columns.")
  }
  if (ncol(document_topic_matrix) != nrow(lookup_dict)) {
    stop("Number of columns in 'document_topic_matrix' must match rows of 'lookup_dict'.")
  }
  if (length(select) != length(tnames)) {
    stop("'select' and 'tnames' must have the same length.")
  }
  
  # DTM: Topics auswählen
  document_topic_matrix = document_topic_matrix[select, , drop = FALSE]
  rownames(document_topic_matrix) = tnames
  
  # DTM: IDs als Spaltennamen
  colnames(document_topic_matrix) = lookup_dict$id
  
  # Long format
  long_format = document_topic_matrix |>
    as.data.frame() |>
    tibble::rownames_to_column("topic") |>
    tidyr::pivot_longer(
      cols      = -topic,
      names_to  = "id",
      values_to = "count"
    )
  
  # Merge mit Lookup
  merged_data = long_format |>
    dplyr::left_join(lookup_dict, by = "id") |>
    dplyr::group_by(date, group, topic) |>
    dplyr::summarise(doc_count = fun(count), .groups = "drop")
  
  # Plot
  if (plot_by != "none") {
    facet_var = if (plot_by == "group") "topic" else "group"
    
    p = ggplot2::ggplot(
      merged_data,
      ggplot2::aes(x = date,
                   y = doc_count,
                   colour = .data[[facet_var]])
    ) +
      ggplot2::geom_smooth(se = FALSE, span = span) +
      ggplot2::facet_wrap(stats::as.formula(paste("~", plot_by))) +
      ggplot2::theme_minimal() +
      ggplot2::labs(x = "date", y = "topic weight", colour = facet_var)
    
    print(p)
  }
  
  # Output-Format
  if (out == "long") {
    return(merged_data)
  } else {
    wide = tidyr::pivot_wider(
      merged_data,
      id_cols     = c(date, group),
      names_from  = topic,
      values_from = doc_count,
      values_fill = 0
    )
    return(wide)
  }
}
