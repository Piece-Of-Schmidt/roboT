#' Highlight a pattern in a character vector
#'
#' Highlights one or more regex patterns in a character vector using ANSI color codes for console output.
#'
#' @param text A character vector (length 1 or more) in which to highlight.
#' @param pattern A single pattern or a pattern string with `|` separators for multiple alternatives.
#' @param color ANSI color code to use for highlighting (default: 31 = red).
#' @return Returns nothing explicitly; the highlighted text is printed via `cat()`.
#' @export
#'
#' @examples
#' highlight("this is a test string", "test|string")
highlight = function(text, pattern, color=31){
  if(grepl("\\|",pattern)) pattern = strsplit(pattern,"|",fixed=T)[[1]]
  if(length(text)>1) text = paste(text, collapse = "\n\n")
  for(pat in pattern) text = gsub(pat,paste0("\033[0;",color,"m",pat,"\033[0m"), text)
  return(cat(text))
}


#' Extract keyword-in-context (KWIC) windows
#'
#' Extracts the textual context around a pattern in a set of character strings,
#' similar to `quanteda::kwic()`. Optionally highlights the pattern and aligns results.
#'
#' @param texts A character vector of input texts.
#' @param pattern A regular expression to search for.
#' @param windowsize Number of characters to include before and after the pattern.
#' @param seperator Optional separator string to mark the pattern (e.g., `**` for markdown highlighting).
#' @param ignore.case Logical. Should pattern matching be case-insensitive? (default: `FALSE`)
#' @param perl Logical. Should Perl-compatible regex be used? (default: `FALSE`)
#' @param offset Logical. Should matches be horizontally aligned? (default: `TRUE`)
#' @param space_on_seperation Logical. Should spacing be added around seperation symbol? (default: `FALSE`)
#' @return A character vector with the extracted and optionally highlighted context windows.
#' @export
#'
#' @examples
#' get_context("this is a test string", "test", windowsize = 10)
get_context = function(texts, pattern, windowsize=30, seperator=NULL, ignore.case=F, perl=F, offset=T, space_on_seperation=F){

  out = sapply(texts, function(text){

    # find target word
    match = regexpr(pattern, text, ignore.case=ignore.case, perl=perl)

    # if target word is found do:
    if(match[1]!=-1){

      # tokenize text
      words = gregexpr("\\b\\w+\\b", text)[[1]]

      # only keep words around target word
      keep = words[words > match-windowsize & words < match+windowsize]

      first = keep[1] # first word to keep
      last = keep[length(keep)] # last word to keep

      # add word length to last word index so it is part of the context
      last = last + attr(words, "match.length")[match(last, words)]

      # actually restrict context to rel context and remove whitespace
      keep = gsub("\\s$","",substr(text, first, last))

      # highlight target word
      if(!is.null(seperator)){
        insert = ifelse(
          space_on_seperation,
          paste(seperator, pattern, seperator),
          paste0(seperator, pattern, seperator))
        sub(pattern, insert, keep)
      } else keep

    }else ""

  }, USE.NAMES = F)

  if(offset){
    # center texts
    offsets = regexpr(pattern, out)
    center = max(offsets)

    # return
    out = paste0(strrep(" ", center-offsets), out)
  }
  return(out)

}


#' Pretty-print a data frame in the console
#'
#' Formats and prints a data frame to the R console with controlled column widths and wrapping.
#' Useful for console-based output where visual clarity is important.
#'
#' @param df A data frame to be printed.
#' @param col_prop A numeric vector of relative column widths (values between 0 and 1).
#'   If `NULL`, columns are equally spaced.
#' @param buffer Integer to reduce each column width slightly (default: 2).
#' @param sep Character string used as column separator (default: `" | "`).
#' @param line_char Character used to draw row separators (default: `"-"`).
#' @param del_special_chars Character pattern to remove from cells (e.g. `"\r"`). Set to `NULL` to skip.
#' @return No return value. Output is printed to the console.
#' @importFrom stats setNames
#' @export
#'
#' @examples
#' df <- data.frame(a = c("short", "a very long piece of text"), b = 1:2)
#' print_dataframe(df)
print_dataframe = function(df,
                           col_prop  = NULL,   # Vektor von Anteilen (0‒1)
                           buffer    = 2,      # Sicherheits-Abzug je Spalte
                           sep       = " | ",  # Spaltentrenner
                           line_char = "-",    # Zeichen für die Trennlinie
                           del_special_chars = "\r") {
  stopifnot(is.data.frame(df))
  nc = ncol(df)
  if (nc < 2) df = data.frame(df, "")

  col_prop = c(col_prop, rep(NA_real_, nc))[1:nc]

  if (any(col_prop < 0 | col_prop > 1, na.rm = TRUE))
    stop("Anteile muessen zwischen 0 und 1 liegen.")

  fixed_sum   = sum(col_prop, na.rm = TRUE)
  num_missing = sum(is.na(col_prop))

  if (fixed_sum > 1 + .Machine$double.eps^0.5)
    stop("Vorgegebene Anteile summieren sich auf > 1.")

  if (num_missing > 0) {
    col_prop[is.na(col_prop)] = (1 - fixed_sum) / num_missing
  } else if (abs(fixed_sum - 1) > .Machine$double.eps^0.5) {
    stop("Anteile muessen sich auf 1 (= 100 %) addieren.")
  }

  # --- Spaltenbreiten berechnen ---------------------------------------------
  total_width = getOption("width")
  avail_width = total_width - (nc - 1) * nchar(sep)   # Platz ohne Trenner
  col_width   = pmax(floor(avail_width * col_prop) - buffer, 5)

  # ggf. Rundungsfehler korrigieren (Restpixel dem letzten Puffer zuschlagen)
  diff = avail_width - sum(col_width)
  if (diff > 0) col_width[nc] = col_width[nc] + diff

  if (any(col_width < 5))
    stop("zu viele Spalten / zu kleine Konsole")

  # replace characters
  if(!is.null(del_special_chars)){
    df = apply(df, 2, gsub, pattern=del_special_chars, replacement="", fixed=T)
  }

  # --- Hilfsfunktionen -------------------------------------------------------
  wrap_preserve = function(txt, width) {
    paragraphs = strsplit(as.character(txt), "\n", fixed = TRUE)[[1]]
    unlist(lapply(paragraphs, function(p)
      if (p == "") "" else strwrap(p, width = width)),
      use.names = FALSE)
  }
  divider = paste(rep(substr(line_char, 1, 1), total_width), collapse = "")

  # add colnames
  df = rbind(setNames(toupper(colnames(df)), colnames(df)), df)

  # --- Ausgabe ---------------------------------------------------------------
  for (row in seq_len(nrow(df))) {
    col_lines = mapply(function(x, w) wrap_preserve(x, w),
                       df[row, ], col_width,
                       SIMPLIFY = FALSE)
    max_lines = max(lengths(col_lines))
    col_lines = lapply(col_lines, function(x)
      c(x, rep("", max_lines - length(x))))

    for (i in seq_len(max_lines)) {
      pieces = vapply(seq_along(col_lines),
                      function(j) sprintf("%-*s", col_width[j], col_lines[[j]][i]),
                      character(1))
      cat(paste(pieces, collapse = sep), "\n", sep = "")
    }
    cat(divider, "\n")
  }
}
