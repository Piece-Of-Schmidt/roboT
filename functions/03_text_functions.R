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