#'as.textmeta
#'
#'transforms a named vector or a named list to a tosca::textmeta-file
#'
#'@param named_object Textfile to convert
#'@return textmeta-Objekt
#'@export
#'
as.textmeta = function(named_object){
  if(is.null(names(named_object))) names(named_object) = paste0("text_",seq(length(named_object)))
  
  if(!is.list(named_object)) named_object = as.list(named_object)
  return(
    tosca::textmeta(text = named_object,
                    meta = data.frame(id=names(named_object),
                                      date="",
                                      title="",
                                      fake="")
    )
  )
}


#'is.textmeta
#'
#'Testet, ob ein Objekt ein Textmeta-Objekt (im Sinne von tosca) ist
#'
#'@param obj Das Objekt, das getestet werden soll
#'@return TRUE oder FALSE
#'@examples is.textmeta(HB)
#'@export
#'
is.textmeta = function(obj){
  is.list(obj) && all(c("meta","text") %in% names(obj))
}


# # Erzeugt Samples basierend auf mehreren Text-Inputs. Input muss als Liste von Textsammlungen uebergeben werden.
# # Funktion erlaubt den Einsatz verschiedener Speicher-Funktionen. Ermoeglicht damit den flexiblen Einsatz von
# # write.csv(), write.csv2() fuer Windows-Files oder write.table(). 
# sample_texts = function(texts, sample_size=100, seed=1337, filenames=NULL, call="write.csv", ...){
#   
#   # build texts
#   lapply(seq(texts), function(idx){
#     
#     cat("Process subcorp ", idx, "/", length(texts), "\n", sep="")
#     
#     set.seed(seed)
#     s_texts = sample(texts[[idx]], size=sample_size)
#     
#     if(!is.null(filenames)){
#       filename = paste0(sub("[.]csv","",filenames[idx]), ".csv")
#       args = list(..., x = s_texts, file=filename)
#       do.call(call, args = args)
#     }
#     s_texts
#   })
# }


# # initializes progress bar
# progress.initialize = function(runner, track_time=F){
#   
#   # create new environment
#   progress.environment <<- new.env()
#   
#   # save params to new environment
#   local(runner <- runner, env=progress.environment) # running steps
#   local(width <- round((2/3)*getOption("width")) +
#           round((2/3)*getOption("width"))%%2,
#         env=progress.environment) # width of prgress bar (even number forced)
#   local(scaling <- width/length(runner), env=progress.environment) # scalar for each iteration
#   local(index_to_indicate_progress <- 1, env=progress.environment) # start index
#   
#   invisible(TRUE)
# }


# # plots progress bar
# progress.indicate = function(){
#   
#   # load params from environment
#   runner = local(runner, env=progress.environment)
#   width = local(width, env=progress.environment)
#   scaling = local(scaling, env=progress.environment)
#   index_to_indicate_progress = local(index_to_indicate_progress, env=progress.environment)
#   track_time = local(track_time, env=progress.environment)
#   
#   # calculate progress
#   progress = paste(floor(100*index_to_indicate_progress/length(runner)), "%  ")
#   
#   # print progress bar
#   cat("\r",
#       strrep("=", round(index_to_indicate_progress * scaling)),
#       strrep(" ", round(width - index_to_indicate_progress*scaling)),
#       " | ",
#       progress, sep="")
#   
#   # update running parameter
#   local(index_to_indicate_progress <- index_to_indicate_progress+1, env=progress.environment)
#   
#   # if done: reset running variable
#   if(local(index_to_indicate_progress, env=progress.environment) == length(runner)+1){
#     local(index_to_indicate_progress <- 1, env=progress.environment)
#     cat("\n")
#   }
#   
#   # return invisibly
#   invisible(TRUE)
# }