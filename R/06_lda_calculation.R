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
#' Optionally saves parameter log to disk.
#'
#' @param docs Either a single document list or a list of such objects (as in `LDAprep()`).
#' @param vocab Either a single vocabulary or a list of vocabularies.
#' @param ... Named arguments to be passed to `LDAgen()` (e.g., `K`, `alpha`, `beta`).
#' @param func Function name to use for LDA generation (default: `"LDAgen"`).
#' @param runs Either `"all"` or an integer to subsample from all grid combinations.
#' @param seed Random seed for reproducibility (default: 1337).
#' @param savelogs Logical. Save parameter combinations to `"model_logs.csv"`?
#' @return A list of LDA result objects, named `run1`, `run2`, ...
#' @importFrom utils write.csv
#' @export
#'
#' @examples
#' # multipleLDAs(docs, vocab, K = c(10, 15), alpha = 0.1, runs = 2)
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
