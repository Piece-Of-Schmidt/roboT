

read_factiva = function(source, split="auto", clean=NULL, remove_newlines=TRUE, tolist=TRUE){
  
  if(split == "auto") split = "\n+Dokument \\w{10,}\n+"
  
  # data einlesen via readLines()
  data = readLines(source)
  
  # save text ids
  ids = sub("Dokument ", "", grep("Dokument \\w{15,}", data, value=T))
  
  # paste data
  data = paste(data, collapse = "\n")
  
  # split data
  data = strsplit(data, split)[[1]]
  data = data[-length(data)]
  
  # remove clean pattern
  if(!is.null(clean)) data = gsub(clean, "", data)
  
  # do extra cleaning
  data = gsub("\n+", "\n", data)
  data = gsub("\\s+", " ", data)
  data = gsub("(^\\s|\\s$)", "", data)
  if(remove_newlines) data = gsub("\n", " ", data)
  
  # to list
  if(tolist) data = setNames(as.list(data), ids)
  
  # return
  return(data)
}

to_meta = function(factiva_data){
  
  # extract meta infos
  titles = sub("\\d+(,\\d+)? Wörter.*", "", factiva_data)
  dates = regmatches(factiva_data, regexpr("\\d+ \\w+ \\d{4}", factiva_data))
  dates = as.Date(dates, format = "%d %b %y")
  ids = if(is.null(names(factiva_data))) paste0("Doc", seq(factiva_data)) else names(factiva_data)
  
  # make meta data
  meta = data.frame(id = ids, date = dates, title = titles)
  
  # return
  out = list(text = factiva_data,
             meta = meta)
}
