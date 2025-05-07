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
                              buffer    = 2,      # kleiner Puffer je Spalte
                              sep       = " | ",  # Trenner zwischen Spalten
                              line_char = "-") {  # Zeichen für die Trennlinie
  stopifnot(is.data.frame(df))
  if (ncol(df) < 2)
    stop("Dataframe has to have at least 2 columns")
  
  # --- Breiten berechnen ----------------------------------------------------
  total_width = getOption("width")                # full width
  n_cols      = ncol(df)
  avail_width = total_width - (n_cols - 1) * nchar(sep)
  col_width   = floor(avail_width / n_cols) - buffer
  if (col_width < 5)
    stop("too many columns / too tiny console")
  
  # Hilfsfunktion: Text → Vektor umgebrochener Zeilen, \n bleibt erhalten
  wrap_preserve = function(txt) {
    paragraphs = strsplit(as.character(txt), "\n", fixed = TRUE)[[1]]
    unlist(lapply(paragraphs, function(p)
      if (p == "") "" else strwrap(p, width = col_width)),
      use.names = FALSE)
  }
  
  divider = paste(rep(substr(line_char, 1, 1), total_width), collapse = "")
  
  
  # print -------------------------------------------------------------------
  
  for (row in seq_len(nrow(df))) {
    # Zeilen‐Listen pro Spalte
    col_lines = lapply(df[row, ], wrap_preserve)
    max_lines = max(lengths(col_lines))
    
    # kürzere Spalten mit Leerzeilen auffüllen
    col_lines = lapply(col_lines, function(x)
      c(x, rep("", max_lines - length(x))))
    
    # Zeile für Zeile drucken
    for (i in seq_len(max_lines)) {
      pieces = vapply(col_lines, function(x)
        sprintf("%-*s", col_width, x[i]), character(1))
      cat(paste(pieces, collapse = sep), "\n", sep = "")
    }
    cat(divider, "\n")
  }
}
