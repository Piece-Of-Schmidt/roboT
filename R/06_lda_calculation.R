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
    print(ggplot2::ggplot(merged_data, aes(x = date, y = doc_count, color = group)) +
            ggplot2::geom_smooth(se=F, span=0.1) +
            ggplot2::facet_wrap(~ topic) +
            ggplot2::theme_minimal() +
            ggplot2::labs(x = "Monat", y = "Anzahl Dokumente", color = "Quelle"))
  }

  result = if(out == "melt") merged_data else reshape2::dcast(merged_data, date + group ~ topic, value.var = "doc_count")

  return(result)
}
