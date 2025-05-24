# lighlights a pattern in a text
highlight = function(text, pattern, color=31){
  if(grepl("\\|",pattern)) pattern = strsplit(pattern,"|",fixed=T)[[1]]
  if(length(text)>1) text = paste(text, collapse = "\n\n")
  for(pat in pattern) text = gsub(pat,paste0("\033[0;",color,"m",pat,"\033[0m"), text)
  return(cat(text))
}


# get context around a pattern, like quanteda::kwic()
get_context = function(texts, pattern, windowsize=30, seperator=NULL, ignore.case=F, perl=F, offset=T){
  
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
      if(!is.null(seperator)) sub(pattern, paste(seperator, pattern, seperator), keep) else keep
      
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


# print dataframe in console
print_dataframe = function(df,
                           col_prop  = NULL,   # Vektor von Anteilen (0‒1)
                           buffer    = 2,      # Sicherheits-Abzug je Spalte
                           sep       = " | ",  # Spaltentrenner
                           line_char = "-",    # Zeichen für die Trennlinie
                           del_special_chars = "\r") {
  stopifnot(is.data.frame(df))
  nc = ncol(df)
  if (nc < 2) df = data.frame(df, "")
  
  # --- Anteilsvector interpretieren -----------------------------------------
  # if (is.null(col_prop)) {
  #   col_prop = rep(NA_real_, nc)              # alles ungesetzt  → gleich verteilen
  # } else if (length(col_prop) == 1L) {
  #   col_prop = c(col_prop, rep(NA_real_, nc - 1))
  # } else if (length(col_prop) != nc) {
  #   stop("col_prop muss Länge 1 oder ncol(df) haben.")
  # }
  #  NULL -> NA umwandeln (falls als Liste übergeben)
  # col_prop = vapply(col_prop, function(x) if (is.null(x)) NA_real_ else x,
  #                   numeric(1))
  
  col_prop = c(col_prop, rep(NA_real_, nc))[1:nc]
  
  if (any(col_prop < 0 | col_prop > 1, na.rm = TRUE))
    stop("Anteile müssen zwischen 0 und 1 liegen.")
  
  fixed_sum   = sum(col_prop, na.rm = TRUE)
  num_missing = sum(is.na(col_prop))
  
  if (fixed_sum > 1 + .Machine$double.eps^0.5)
    stop("Vorgegebene Anteile summieren sich auf > 1.")
  
  if (num_missing > 0) {
    col_prop[is.na(col_prop)] = (1 - fixed_sum) / num_missing
  } else if (abs(fixed_sum - 1) > .Machine$double.eps^0.5) {
    stop("Anteile müssen sich auf 1 (= 100 %) addieren.")
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
