#' Plot source frequencies in a textmeta corpus
#'
#' Aggregates and visualizes the number of documents over time,
#' grouped by a specified metadata column (e.g., "resource" or "medium").
#' Optionally includes smoothing lines using `geom_smooth()`.
#'
#' @param corpus A `textmeta` object as used in the \pkg{tosca} package.
#' @param category Character string specifying the metadata column to group by (default: `"resource"`).
#' @param unit Time unit for aggregation (e.g., `"month"`, `"week"`, `"day"`). Passed to `lubridate::floor_date()`.
#' @param area_alpha Transparency for `geom_area()` (default: `0.3`).
#' @param area_position Position adjustment for `geom_area()` (e.g., `"stack"`, `"identity"`; default: `"identity"`).
#' @param ... Additional arguments passed to `geom_area()`.
#' @param smooth Logical. Should a smoothed trend line be added? (default: `TRUE`)
#' @param span Smoothing span for `geom_smooth()` (default: `0.1`).
#' @param se Logical. Should confidence intervals be shown in smoothing? (default: `TRUE`)
#' @return Invisibly returns the aggregated data frame with columns `date`, `category_to_group_after`, and `n`.
#'         A `ggplot2` plot is printed to the console.
#' @importFrom dplyr mutate count
#' @importFrom magrittr %>%
#' @export
#'
#' @examples
#' # plotSources(corpus, category = "medium", unit = "month")
plotSources = function(corpus, category="resource", unit="month", area_alpha=0.3, area_position="identity", ..., smooth=T, span=0.1, se=T) {

  # rename column of interest
  corpus$meta$category_to_group_after = corpus$meta[,category]

  # count
  data = corpus$meta %>%
    mutate(date = lubridate::floor_date(date, unit)) %>%
    count(date, category_to_group_after)

  # generate plot
  if(smooth){
    p = ggplot2::ggplot(data, ggplot2::aes(date, n, fill=category_to_group_after, color=category_to_group_after))+
      ggplot2::geom_area(alpha=area_alpha, position=area_position,...)+
      ggplot2::geom_smooth(span=span, se=se, alpha=0.8)+
      ggplot2::theme_classic()+
      ggplot2::labs(color = category, fill = category)
  }else{
    p = ggplot2::ggplot(data, ggplot2::aes(date, n, fill=category_to_group_after, color=category_to_group_after))+
      ggplot2::geom_area(alpha=area_alpha, position=area_position,...)+
      ggplot2::theme_classic()+
      ggplot2::labs(color = category, fill = category)
  }
  print(p)

  invisible(data)
}


#' Downsample corpus to reduce frequency spikes
#'
#' Detects and flattens unusually large date chunks (e.g., sudden spikes in article counts)
#' by sampling documents within those time slices. Helps to normalize corpora before modeling.
#'
#' @param corp A `textmeta` object (from \pkg{tosca}).
#' @param unit Time unit for grouping (default: `"month"`).
#' @param reference A function name (as string) to compute the central tendency (default: `"mean"`).
#' @param fct A numeric multiplier applied to the standard deviation to define the sampling threshold (default: `1`).
#' @param seed Seed for reproducible random sampling (default: `1337`).
#' @return A new `textmeta` object where date chunks with unusually high counts are downsampled.
#' @importFrom stats var
#' @export
#'
#' @examples
#' # flattened <- flatten_corpus(corpus, unit = "month", reference = "median", fct = 1.5)
flatten_corpus = function(
    corp,
    unit = "month",
    reference = "mean",
    fct = 1,
    seed = 1337){

  # calculate date chunks
  floordates = lubridate::floor_date(as.Date(corp$meta$date), unit=unit); dates_table = table(floordates)
  med = do.call(reference, list(dates_table)); se = sqrt(var(dates_table)); thresh = med + se*fct
  rel_chunks_bool = dates_table >= thresh; rel_chunks = names(dates_table)[rel_chunks_bool]

  # iterate through chunks
  samples = lapply(rel_chunks, \(chunk){
    sub = subset(corp$meta, floordates==chunk, select="id", drop=T)
    set.seed(seed)
    tosca::filterID(corp, sample(sub, min(length(sub), thresh)))
  })

  # merge old and new data
  data = tosca::mergeTextmeta(
    append(
      samples,
      list(tosca::filterID(corp, corp$meta$id[!floordates %in% rel_chunks]))))

  # order data
  data$meta = data$meta[order(data$meta$date),]
  data$text = data$text[data$meta$id]

  return(data)
}
