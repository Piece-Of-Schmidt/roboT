#' Filter texts based on their title
#'
#' Removes or selects texts from a `textmeta` object based on their title.
#' Supports exact title matches or regular expression patterns.
#'
#' @param corpus A `textmeta` object (as used in the \pkg{tosca} package).
#' @param titles A character vector of titles or patterns to match.
#' @param pattern Logical. If `TRUE`, uses regex matching instead of exact title match.
#' @param ignore.case Logical. Should case be ignored in pattern matching?
#' @param print Logical. If `TRUE`, prints the matching titles.
#' @param invert Logical. If `TRUE` (default), returns texts that *do not* match the titles.
#' @param out Character. `"text"` (default) to return filtered corpus, `"bin"` to return logical mask.
#' @return Either a filtered `textmeta` object or a logical vector (depending on `out`).
#' @export
#'
#' @examples
#' # Remove all texts with title "Leserbriefe"
#' # corpus <- filterTitles(corpus, titles = "Leserbriefe", pattern = FALSE)
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


#' Filter texts based on word count thresholds
#'
#' Extends `tosca::filterCounts()` by allowing both absolute and relative
#' thresholds (quantiles) for word counts.
#'
#' @param corpus A `textmeta` object with tokenized texts (as lists).
#' @param lower_thresh Lower threshold. Either a number of tokens or a quantile (e.g. `0.05`).
#' @param upper_thresh Upper threshold. Either a number of tokens or a quantile (e.g. `0.95`).
#' @return A filtered `textmeta` object containing only texts within the given range.
#' @importFrom stats quantile
#' @export
#'
#' @examples
#' # Keep only the middle 90% of texts by word count
#' # corpus <- filterWordCounts(corpus, lower_thresh = 0.05, upper_thresh = 0.95)
filterWordCounts = function(corpus, lower_thresh=0, upper_thresh=1){

  lens = lengths(corpus$text) # expects tokenized corpus

  if(lower_thresh < 1) lower_thresh = quantile(lens, lower_thresh)
  if(upper_thresh <= 1) upper_thresh = quantile(lens, upper_thresh)

  # return restricted corpus
  mask = lens >= lower_thresh & lens <= upper_thresh
  return(tosca::filterID(corpus, names(mask)[mask]))

}


#' Remove duplicate articles based on their title
#'
#' Identifies and removes duplicate articles within the same time unit,
#' based on exact or case-insensitive title comparison.
#'
#' @param corpus A `textmeta` object.
#' @param unit Time unit for grouping duplicates (e.g., `"day"`, `"month"`, `"all"`).
#' @param ignore.case Logical. Should case be ignored when comparing titles?
#' @param out One of c("obj", "text", "ids", "logical").
#' @param message Logical. Should summary info be printed? (default: `TRUE`)
#' @param verbose Logical. Should duplicate titles be listed? (default: `FALSE`)
#' @return A filtered `textmeta` object without duplicates (based on title).
#' @export
#'
#' @examples
#' # corpus <- filterDups_titles(corpus, unit = "day", ignore.case = TRUE)
filterDups_titles = function(corpus, unit = "day", ignore.case=F, out=c("obj", "text", "ids", "logical"), message=T, verbose=F) {
  
  # Sicherstellen, dass das Eingabeformat korrekt ist
  if (!unit %in% c("all", "day", "days", "week", "weeks", "month", "months", "year", "years")) stop('unit must be one of c("all", "day", "days", "week", "weeks", "month", "months", "year", "years")')
  
  out = match.arg(out)
  before = nrow(corpus$meta)
  
  # corpus sortieren
  corpus$meta = corpus$meta[order(corpus$meta$date),]
  corpus$text = corpus$text[match(corpus$meta$id, names(corpus$text))]
  
  if(any(is.na(corpus$meta$date))){
    message("NAs found in meta data. Corpus is restricted to non-NA cases.")
    corpus = tosca::filterID(corpus, corpus$meta$id[!is.na(corpus$meta$date)])
  }
  
  # Erstellung der Datumsgruppen
  date_chunks = if (unit == "all") min(corpus$meta$date) else unique(lubridate::floor_date(corpus$meta$date, unit))
  floor_dates = if (unit == "all") rep(min(corpus$meta$date), nrow(corpus$meta)) else lubridate::floor_date(corpus$meta$date, unit)
  
  # controle case
  if(ignore.case) corpus$meta$title = tolower(corpus$meta$title)
  
  # find dups
  dups = unlist(sapply(date_chunks, function(chunk) {
    mask = floor_dates == chunk
    duplicated(corpus$meta$title[mask])
  }, simplify = T))
  unique_ids = corpus$meta$id[!dups]
  
  # print
  if(message & verbose){
    cat(sprintf("Kept %d out of %d articles | %d removed (%.2f%%)\n",
                sum(!dups), before, before - sum(!dups), 100 * (before - sum(!dups)) / before))
  }
  if(verbose){
    cat("Show 10 most prominent titles (which have been removed)\n")
    titlesdf = data.frame(sort(table(corpus$meta$title[dups]), decreasing=T)[1:10])
    print_dataframe(titlesdf, col_prop = c(0.98, 0.02), line_char = NULL)
    cat("\n")
  }
  
  # return
  if(out=="obj" && is.textmeta(corpus)){
    return(tosca::filterID(corpus, id = unique_ids, filtermeta = TRUE))
  }else if (out == "logical"){
    return(!dups)
  }else if (out == "ids"){
    return(unique_ids)
  }else return(as.character(texts[unique_ids]))
  
}


#' Remove duplicate articles based on text beginnings
#'
#' Detects and removes duplicates based on the first characters of a text
#' (e.g., first 120 characters), optionally within time units.
#'
#' @param corpus A `textmeta` object.
#' @param checkFirstChars Number of leading characters to use for duplication detection (default: `120`).
#' @param unit Time unit to limit comparisons (e.g., `"day"`, `"month"`, `"all"`).
#' @param ignore.case Logical. Should case be ignored in the comparison?
#' @param out One of c("obj", "text", "ids", "logical").
#' @param message Logical. Should summary info be printed? (default: `TRUE`)
#' @param verbose Logical. Should the most frequent leads be printed? (default: `FALSE`)
#' @return A filtered `textmeta` object (default) with duplicate leads removed.
#' @importFrom dplyr arrange group_by filter pull
#' @importFrom magrittr %>%
#' @export
#'
#' @examples
#' # corpus <- filterDups_leads(corpus, checkFirstChars = 100, unit = "day")

filterDups_leads = function(obj, checkFirstChars = 120, unit = "day", ignore.case=F, out=c("obj", "text", "ids", "logical"), message=T, verbose=F) {

  out = match.arg(out)
  texts = if (is.textmeta(obj)) obj$text else obj
  before = length(texts)
  
  # Vorbereitung der Daten
  leads = substr(texts, 1, checkFirstChars)
  ids = if(is.null(names(texts))) seq_along(texts) else names(texts)
  
  # control case
  if(ignore.case) leads = tolower(leads)
  
  if (is.textmeta(obj) && unit != "all") {
    dates = obj$meta$date[match(ids, obj$meta$id)]
    
    # Erstellen eines Datenrahmens
    df = data.frame(leads, ids, dates)
    
    # Konvertieren Sie Datumsangaben je nach Einheit
    df$dates = lubridate::floor_date(df$dates, unit)
    
    # find dups
    unique_ids = df %>%
      arrange(dates) %>%
      group_by(dates) %>%
      filter(!duplicated(leads)) %>%
      pull(ids)
    mask = ids %in% unique_ids
  } else {
    mask = !duplicated(leads)
    unique_ids = ids[mask]
  }
  
  # print
  if(message){
    cat(sprintf("Kept %d out of %d articles | %d removed (%.2f%%)\n",
                length(unique_ids), before, before - length(unique_ids), 100 * (before - length(unique_ids)) / before))
    
  }
  if(verbose){
    cat("Show 10 most prominent leads (which have identified documents as duplicates)\n")
    # print(cbind(sort(table(paste0(df$leads[df$ids %in% setdiff(df$ids, ids)], "...")), decreasing=T)[1:10]))
    leadsdf = data.frame(sort(table(paste0(leads[!ids %in% unique_ids], "...")), decreasing=T)[1:10])
    print_dataframe(leadsdf, col_prop = c(0.98, 0.02), line_char = NULL)
    cat("\n")
  }
  
  # return
  if(out=="obj" && is.textmeta(obj)){
    return(tosca::filterID(obj, id = unique_ids, filtermeta = TRUE))
  }else if (out == "logical"){
    return(mask)
  }else if (out == "ids"){
    return(unique_ids)
  }else return(as.character(texts[unique_ids]))
  
}

