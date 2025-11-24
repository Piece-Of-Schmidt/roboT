#' Perform full preprocessing pipeline on a textmeta corpus
#'
#' Applies a comprehensive cleaning routine to a `textmeta` object. Includes filtering
#' by text length, removal of duplicates (by ID, title, text lead), missing values,
#' UTF-8 conversion, resource redefinition (for HB/Welt/SZ), and optional memory cleanup.
#'
#' @param corpus A `textmeta` object (see \pkg{tosca}).
#' @param shorten_meta Logical. Keep only essential columns in `meta` (id, date, title, resource)?
#' @param remove_na_dates Logical. Remove texts without date information?
#' @param remove_na_sources Logical. Remove texts without resource information?
#' @param remove_na_titles Logical. Remove texts without title information?
#' @param sort_meta Logical. Should meta data be chronologically sorted? (default: `TRUE`)
#' @param min_text_length Minimum number of characters/tokens (absolute or quantile) to keep a text.
#' @param max_text_length Maximum number of characters/tokens (absolute or quantile) to keep a text.
#' @param check_dups_id Logical. Remove duplicated text IDs?
#' @param check_dups_duplist Logical. Use `tosca::duplist()` to remove near-duplicate texts?
#' @param check_dups_textleads Integer. Number of leading characters to check for duplicates (or `NULL` to skip).
#' @param check_dups_titles Logical. Remove duplicates based on identical titles?
#' @param check_dups_textleads_unit Time unit for `filterDups_leads()` (default: `"week"`).
#' @param check_dups_titles_unit Time unit for `filterDups_titles()` (default: `"day"`).
#' @param check_dups_ignore_case Logical. Should duplicate detection ignore case?
#' @param clean_titles Logical. Clean up multiple whitespaces and HTML escape characters in titles?
#' @param utf8 Logical. Convert text and title encoding to UTF-8?
#' @param calculate_text_len_type Type for `nchar()` calculation (`"bytes"` or `"chars"`).
#' @param clean_memory Logical. Call `gc()` after each major operation? Useful for large corpora.
#' @param hbweltsz Logical. Reassign sources according to common DoCMA corpus consisting of HB/Welt/SZ corpora.
#' @param verbose Logical. Print detailed info on removed duplicates.
#'
#' @return A cleaned `textmeta` object, reduced to documents meeting all constraints.
#' @importFrom stats quantile
#' @importFrom utils capture.output
#' @export
#'
#' @examples
#' # Clean corpus using default settings
#' # corpus_clean <- clean_complete(corpus, min_text_length = 750, max_text_length = 0.95)
clean_complete = function(
    corpus,
    shorten_meta=F,
    remove_na_dates=F,
    remove_na_sources=F,
    remove_na_titles=F,
    sort_meta=T,
    min_text_length = 0, # 750
    max_text_length = 1, # 0.99
    check_dups_id=T,
    check_dups_duplist=F,
    check_dups_textleads=120,
    check_dups_titles=T,
    check_dups_textleads_unit="week",
    check_dups_titles_unit="day",
    check_dups_ignore_case=T,
    clean_titles=T,
    utf8=F,
    calculate_text_len_type="bytes",
    clean_memory=F,
    hbweltsz=F,
    verbose=F
){

  # calculate text lengths based on 'calculate_text_len_type' method (default 'bytes')
  before_whole = length(corpus$text)
  if(!is.null(min_text_length) || !is.null(max_text_length)){
    a = Sys.time()
    cat("Calculate text lengths...")
    text_chars = nchar(unlist(corpus$text), type=calculate_text_len_type)
    diff = difftime(Sys.time(), a)
    cat(" Done. Time for computation:", round(diff,3), attr(diff, "unit"), "\n")
  }


  # remove too long texts
  if(!is.null(max_text_length) & max_text_length != 1){
    a = Sys.time()
    max_text_length = if(max_text_length<=1) quantile(text_chars, max_text_length, na.rm=T) else max_text_length
    before = nrow(corpus$meta)
    cat("Remove long texts (max_length =", max_text_length, "\b)...")
    mask = text_chars <= max_text_length
    text_chars = text_chars[mask]
    corpus = tosca::filterID(corpus, names(text_chars))
    cat(" Done. Time for computation:", round(diff,3), attr(diff, "unit"))
    cat(sprintf(" | Kept %d out of %d articles / %d removed (%.2f%%)\n",
                nrow(corpus$meta), before, before - nrow(corpus$meta), 100 * (before - nrow(corpus$meta)) / before))
    if(clean_memory) gc()
  }

  # Restrict corpus to long texts
  if(!is.null(min_text_length) & min_text_length != 0){
    a = Sys.time()
    before = nrow(corpus$meta)
    cat("Remove short texts (min_length =", min_text_length, "\b)...")
    mask = text_chars >= min_text_length
    text_chars = text_chars[mask]
    corpus = tosca::filterID(corpus, names(text_chars))
    diff = difftime(Sys.time(), a)
    cat(" Done. Time for computation:", round(diff,3), attr(diff, "unit"))
    cat(sprintf(" | Kept %d out of %d articles / %d removed (%.2f%%)\n",
                nrow(corpus$meta), before, before - nrow(corpus$meta), 100 * (before - nrow(corpus$meta)) / before))
    if(clean_memory) gc()
  }

  # if corp == "HBWeltSZ": Redefine resources
  if(hbweltsz){
    cat("Redefine resources...")
    a = Sys.time()
    corpus$meta$source_indicator[!is.na(corpus$meta$source)] = "HB"
    corpus$meta$source_indicator[!is.na(corpus$meta$resource)] = "Welt"
    corpus$meta$source_indicator[!is.na(corpus$meta$AnzChar)] = "SZ"
    corpus$meta$resource = corpus$meta$source_indicator
    diff = difftime(Sys.time(), a)
    cat(" Done. Time for computation:", round(diff,3), attr(diff, "unit"), "\n")
    if(clean_memory) gc()
  }

  # Shorten meta data
  if(shorten_meta){
    a = Sys.time()
    cat("Shorten meta data (only keep 'id', 'date', 'title', 'resource', if available)...")
    corpus$metamult = NULL
    corpus$meta = corpus$meta[, names(corpus$meta) %in% c("id","date","title","resource")]
    diff = difftime(Sys.time(), a)
    cat(" Done. Time for computation:", round(diff,3), attr(diff, "unit"), "\n")
    if(clean_memory) gc()
  }

  # remove texts with missing date info
  if(remove_na_dates){
    a = Sys.time()
    before = nrow(corpus$meta)
    cat("Remove docs with missing date info...")
    corpus = tryCatch( {
      c = tosca::filterID(corpus, corpus$meta$id[!is.na(corpus$meta$date)])
      diff = difftime(Sys.time(), a)
      cat(" Done. Time for computation:", round(diff,3), attr(diff, "unit"))
      cat(sprintf(" | Kept %d out of %d articles / %d removed (%.2f%%)\n",
                  nrow(corpus$meta), before, before - nrow(corpus$meta), 100 * (before - nrow(corpus$meta)) / before))
      c
    }, error = function(e) {
      cat(" Failed at finding missing date info. Make sure your meta data contains a 'date' column.\n", e, "\n")
      corpus
    })
    if(clean_memory) gc()
  }

  # remove texts with missing resource info
  if(remove_na_sources){
    a = Sys.time()
    before = nrow(corpus$meta)
    cat("Remove docs with missing resource info...")
    corpus = tryCatch( {
      c = tosca::filterID(corpus, corpus$meta$id[!is.na(corpus$meta$resource)])
      diff = difftime(Sys.time(), a)
      cat(" Done. Time for computation:", round(diff,3), attr(diff, "unit"))
      cat(sprintf(" | Kept %d out of %d articles / %d removed (%.2f%%)\n",
                  nrow(corpus$meta), before, before - nrow(corpus$meta), 100 * (before - nrow(corpus$meta)) / before))
      c
    }, error = function(e) {
      cat(" Failed at finding missing resource info. Make sure your meta data contains a 'resource' column.\n", e, "\n")
      corpus
    })
    if(clean_memory) gc()
  }

  # remove texts with missing title info
  if(remove_na_titles){
    a = Sys.time()
    before = nrow(corpus$meta)
    cat("Remove docs with missing title info...")
    corpus = tryCatch( {
      c = tosca::filterID(corpus, corpus$meta$id[!is.na(corpus$meta$title)])
      diff = difftime(Sys.time(), a)
      cat(" Done. Time for computation:", round(diff,3), attr(diff, "unit"))
      cat(sprintf(" | Kept %d out of %d articles / %d removed (%.2f%%)\n",
                  nrow(corpus$meta), before, before - nrow(corpus$meta), 100 * (before - nrow(corpus$meta)) / before))
      c

    }, error = function(e) {
      cat(" Failed at finding missing title info. Make sure your meta data contains a 'title' column.\n", e, "\n")
      corpus
    })
    if(clean_memory) gc()
  }

  # Reorder meta files
  if(sort_meta){
    a = Sys.time()
    cat("Reorder meta files...")
    corpus$meta = corpus$meta[order(corpus$meta$date),]
    corpus$text = corpus$text[match(corpus$meta$id, names(corpus$text))]
    corpus$meta = corpus$meta[corpus$meta$id %in% names(corpus$text),]
    diff = difftime(Sys.time(), a)
    cat(" Done. Time for computation:", round(diff,3), attr(diff, "unit"), "\n")
    if(clean_memory) gc()
  }

  # Convert texts and titles to utf8
  if(utf8){
    a = Sys.time()
    cat("Convert texts and titles to utf8...")
    corpus$text = lapply(corpus$text, utf8::as_utf8)
    corpus$meta$title = sapply(corpus$meta$title, utf8::as_utf8)
    diff = difftime(Sys.time(), a)
    cat(" Done. Time for computation:", round(diff,3), attr(diff, "unit"), "\n")
    if(clean_memory) gc()
  }

  # -------------------------------------------------------------------------
  # remove duplicates
  # -------------------------------------------------------------------------

  # Select unique text elements by ID
  if(check_dups_id){
    a = Sys.time()
    before = nrow(corpus$meta)
    cat("Select unique text elements by ID...")
    ids = unique(names(corpus$text))
    corpus$text = corpus$text[ids]
    corpus$meta = corpus$meta[match(ids,corpus$meta$id),]
    diff = difftime(Sys.time(), a)
    cat(" Done. Time for computation:", round(diff,3), attr(diff, "unit"))
    cat(sprintf("| Kept %d out of %d articles / %d removed (%.2f%%)\n",
                nrow(corpus$meta), before, before - nrow(corpus$meta), 100 * (before - nrow(corpus$meta)) / before))
    if(clean_memory) gc()
  }

  # duplist
  if(check_dups_duplist){
    a = Sys.time()
    before = nrow(corpus$meta)
    cat("Perform tosca::duplist() operation...")
    invisible(capture.output(duplist <- suppressMessages(tosca::duplist(corpus))))
    corpus = tosca::filterID(corpus, duplist$uniqueTexts)
    diff = difftime(Sys.time(), a)
    cat(" Done. Time for computation:", round(diff,3), attr(diff, "unit"))
    cat(sprintf(" | Kept %d out of %d articles / %d removed (%.2f%%)\n",
                nrow(corpus$meta), before, before - nrow(corpus$meta), 100 * (before - nrow(corpus$meta)) / before))
    if(clean_memory) gc()
  }

  # Filter Leads: Texte raus, die in den ersten X Zeichen uebereinstimmen
  if(!is.null(check_dups_textleads) && !is.logical(check_dups_textleads)){
    a = Sys.time()
    before = nrow(corpus$meta)
    cat("Filter Leads...")
    corpus = filterDups_leads(corpus, check_dups_textleads, check_dups_textleads_unit, ignore.case=check_dups_ignore_case, message=F, verbose=verbose)
    diff = difftime(Sys.time(), a)
    cat(" Done. Time for computation:", round(diff,3), attr(diff, "unit"))
    cat(sprintf(" | Kept %d out of %d articles / %d removed (%.2f%%)\n",
                nrow(corpus$meta), before, before - nrow(corpus$meta), 100 * (before - nrow(corpus$meta)) / before))
    if(clean_memory) gc()
  }

  # Filter Titles: Alle Texte raus, die am selben Tag erschienen sind und denselben Title haben
  if(check_dups_titles){
    a = Sys.time()
    before = nrow(corpus$meta)
    cat("Filter Titles...")
    corpus = filterDups_titles(corpus, check_dups_titles_unit, ignore.case=check_dups_ignore_case, message=F, verbose=verbose)
    diff = difftime(Sys.time(), a)
    cat(" Done. Time for computation:", round(diff,3), attr(diff, "unit"))
    cat(sprintf(" | Kept %d out of %d articles / %d removed (%.2f%%)\n",
                nrow(corpus$meta), before, before - nrow(corpus$meta), 100 * (before - nrow(corpus$meta)) / before))
    if(clean_memory) gc()
  }

  # Titles aufraeumen: Mehrfache Leerzeichen raus
  if(clean_titles){
    cat("Clean title strings...")
    a = Sys.time()
    corpus$meta$title = gsub("\\s+"," ",corpus$meta$title)
    corpus$meta$title = gsub("&quot;","'",corpus$meta$title)
    diff = difftime(Sys.time(), a)
    cat(" Done. Time for computation:", round(diff,3), attr(diff, "unit"), "\n\n")
    if(clean_memory) gc()
  }

  after = length(corpus$text)
  message(sprintf("Complete.\nKept %d out of %d articles / %d removed (%.2f%%)\n",
                  after, before_whole, before_whole - after, 100 * (before_whole - after) / before_whole))

  return(corpus)
}
