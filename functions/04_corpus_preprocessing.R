library(lubridate)

# Plot Sources
plotSources = function(corpus, category="resource", unit="month", area_alpha=0.3, area_position="identity", ..., smooth=T, span=0.1, se=T) {
  
  # rename column of interest
  corpus$meta$category_to_group_after = corpus$meta[,category]
  
  # count
  data = corpus$meta %>%
    mutate(date = floor_date(date, unit)) %>%
    count(date, category_to_group_after)
  
  # generate plot
  if(smooth){
    p = ggplot(data, aes(date, n, fill=category_to_group_after, color=category_to_group_after))+
      geom_area(alpha=area_alpha, position=area_position,...)+
      geom_smooth(span=span, se=se, alpha=0.8)+ 
      theme_classic()+
      labs(color = category, fill = category)
  }else{
    p = ggplot(data, aes(date, n, fill=category_to_group_after, color=category_to_group_after))+
      geom_area(alpha=area_alpha, position=area_position,...)+
      theme_classic()+
      labs(color = category, fill = category)
  }
  print(p)
  
  invisible(data)
}


# function that tries to tackle unusual spikes in text frequency counts
flatten_corpus = function(
    corp,
    unit = "month",
    reference = "mean",
    fct = 1,
    seed = 1337){
  
  # calculate date chunks
  floordates = floor_date(as.Date(corp$meta$date), unit=unit); dates_table = table(floordates)
  med = do.call(reference, list(dates_table)); se = sqrt(var(dates_table)); thresh = med + se*fct
  rel_chunks_bool = dates_table >= thresh; rel_chunks = names(dates_table)[rel_chunks_bool]
  
  # iterate through chunks
  samples = lapply(rel_chunks, \(chunk){
    sub = subset(corp$meta, floordates==chunk, select="id", drop=T)
    set.seed(seed)
    filterID(corp, sample(sub, min(length(sub), thresh)))
  })
  
  # merge old and new data
  data = mergeTextmeta(
    append(
      samples,
      list(filterID(corp, corp$meta$id[!floordates %in% rel_chunks]))))
  
  # order data
  data$meta = data$meta[order(data$meta$date),]
  data$text = data$text[data$meta$id]
  
  return(data)
}