#' Aggregate LDA topic scores by metadata group and time
#'
#' Decomposes a document-topic matrix by grouping documents by a meta variable
#' (e.g., source or region) and aggregating over time (e.g., monthly).
#'
#' @param document_topic_matrix A matrix of topic scores (topics × documents).
#' @param lookup_dict A data frame with three columns: `id`, `date`, and `group`.
#' @param select Vector of topic indices to include (default: all).
#' @param unit Time unit for aggregation, passed to `lubridate::floor_date()` (e.g., `"month"`).
#' @param tnames Optional names for selected topics.
#' @param plot Logical. If `TRUE`, plots the result using `ggplot2`.
#' @param out Format of output: `"melt"` (long format) or `"wide"` (default: `"melt"`).
#' @return A data frame of aggregated topic counts by date, group, and topic.
#' @importFrom dplyr left_join group_by summarise
#' @importFrom magrittr %>%
#' @export
#'
#' @examples
#' # decompose_lda(theta, lookup_dict, unit = "month", plot = TRUE)
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
    print(ggplot2::ggplot(merged_data, ggplot2::aes(x = date, y = doc_count, color = group)) +
            ggplot2::geom_smooth(se=F, span=0.1) +
            ggplot2::facet_wrap(~ topic) +
            ggplot2::theme_minimal() +
            ggplot2::labs(x = "Monat", y = "Anzahl Dokumente", color = "Quelle"))
  }

  result = if(out == "melt") merged_data else reshape2::dcast(merged_data, date + group ~ topic, value.var = "doc_count")

  return(result)
}


#' Run multiple LDA models with varying parameters
#'
#' Automates repeated LDA estimation across a grid of parameter combinations and/or corpora.
#' Optionally saves parameter log and model outputs to disk.
#'
#' @param ... Named arguments to be passed to `LDAgen()`, `RollingLDA()` or `updateRollingLDA()` (e.g., `x`, `texts`, `docs`, `K`, `alpha`, `beta`). Arguments must meet requirements of respective function, see ?LDAgen or ?RollingLDA for details.
#' @param func Function name to use for LDA generation (default: `"LDAgen()"`, also works with `RollingLDA()` and `updateRollingLDA()`).
#' @param data_vars Names of parameters that are passed through `...` that contain the data to be analysed (e.g. `docs` and `vocab` for LDAgen or `texts` and `dates` for RollingLDA).
#' @param select Integer vector specifying which parameter constellations (rows of the parameter grid) to run.
#' If `NULL`, all combinations in the grid are executed. Useful for resuming incomplete runs or testing specific setups.
#' @param filename Name (prefix) of model outputs (e.g. filename=`lda` results in `lda1`, `lda2`, etc.).
#' @param savelogs Logical. If True, save parameter combinations to `"model_logs.csv"`.
#' @param save_on_every_iteration Logical. If True, every model output is saved seperately after a successful iteration.
#' @param verbose Logical. If True, prints information while processing.
#' @param calculate Logical. If False, only generates and returns grid of parameters without calculation.
#' @return A list of LDA result objects, named `run1`, `run2`, ...
#' @importFrom utils write.csv
#' @export
#'
#' @examples
#' # multipleLDAs(docs, vocab, K = seq(10, 50, 10), alpha = c(0.1, 1), seed = c(100, 200))

multipleLDAs = function(..., func="LDAgen", data_vars=c("x", "texts", "dates", "docs", "vocab", "vocabLDA"), select=NULL, filename="lda", savelogs=T, save_on_every_iteration=T, verbose=F, calculate=T){
  
  numerics = c("vocab.abs", "K", "alpha", "eta", "seeds", "n", "vocab.rel", "vocab.fallback", "doc.abs", "memory.fallback")
  
  # init grid
  lda_vars = list(...)
  core_data = names(lda_vars) %in% data_vars
  grid = expand.grid(lda_vars[!core_data], stringsAsFactors=F)
  runs = nrow(grid)
  model_ids = paste0(filename, 1:runs)
  
  if(verbose){
    cat("Model params (", runs, " runs):\n", sep="")
    print(grid)
    cat("\n")
  }
  
  if(savelogs){
    savegrid = data.frame(id = model_ids, grid)
    write.csv(savegrid, "model_logs.csv")
  }
  
  if(calculate){

    rel_cases = if(is.null(select)) seq(runs) else select
    out = lapply(rel_cases, function(idx){
      
      args = as.list(grid[idx, ])
      
      if(verbose) message("\nCalculate LDA: ", paste(names(args), args, sep=": ", collapse = ", "), sep=" ")
      
      # build all params
      modelargs = append(lda_vars[core_data], unlist(args))
      modelargs[names(modelargs) %in% numerics] = as.numeric(modelargs[names(modelargs) %in% numerics])
      
      # call func
      res = do.call(func, args = modelargs)
      res$hyperparams = args
      
      if (save_on_every_iteration) saveRDS(res, paste0(model_ids[idx], ".rds"))
      res
      
    })
    names(out) = model_ids
    
    return(invisible(out))
    
  } else return(grid)  

}


# EXPERIMENTAL:
get_rolling_at_time = function(obj, date){
  
  # get individual items ----------------------------------------------------
  
  # get names of rel documents (stem)
  rel_dates = obj$dates[obj$dates <= date] 
  rel_documents = names(rel_dates)
  
  # get relevant vocab (stem)
  rel_vocab_idx = lapply(obj$docs[rel_documents], \(d) d[1,]) |>
    unlist() |>
    unique()
  
  # check vocab length
  length(rel_vocab) == obj$chunks[1, n.vocab]
  
  # get words
  rel_vocab = obj$vocab[rel_vocab_idx+1]
  
  # get assignments
  rel_assignments = obj$lda$assignments[seq_along(rel_documents)]
  
  # get topicXtopic assignments - and reorder so it matches old topic matrix
  rel_topics = topic_word_matrix(obj$docs, obj$vocab, obj$lda$assignments, select_tokens=rel_vocab, as_dfm=F)
  words_in_order = intersect(colnames(obj$lda$topics), colnames(rel_topics))
  rel_topics = rel_topics[, words_in_order]
  
  # check
  ncol(rel_topics) == obj$chunks[1, n.vocab]
  all(colnames(rel_topics) == colnames(obj$lda$topics)[1:obj$chunks[1, n.vocab]])
  
  # get documents_sums - and check
  rel_doc_sums = obj$lda$document_sums[, seq_along(rel_documents)]
  ncol(rel_doc_sums) == obj$chunks[1, n]
  
  # get docs
  rel_docs = obj$docs[rel_documents]
  
  # build chunks
  rel_chunks = obj$chunks[1,]
  
  
  # rebuild rolling object --------------------------------------------------
  
  rolling_new = obj
  
  rolling_new$lda$assignments = rel_assignments
  rolling_new$lda$topics = rel_topics
  rolling_new$lda$document_sums = rel_doc_sums
  
  rolling_new$docs = rel_docs
  rolling_new$dates = rel_dates
  rolling_new$vocab = words_in_order
  rolling_new$chunks = rel_chunks
  
  # return
  return(rolling_new)
  
}
