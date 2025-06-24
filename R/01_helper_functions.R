utils::globalVariables(c(
  "group", "topic", "doc_count", "category_to_group_after",
  "n", "topWords"
))


#' Convert a named vector or list to a textmeta object
#'
#' Transforms a named character vector or named list into a `textmeta` object
#' as used by the \pkg{tosca} package. Names are used as IDs.
#' If no names are present, dummy IDs are generated automatically.
#'
#' @param named_object A named character vector or named list to convert.
#' @return An object of class `textmeta`.
#' @export
#'
#' @examples
#' vec <- c(doc1 = "Text A", doc2 = "Text B")
#' vec.as.textmeta(vec)
vec.as.textmeta = function(named_object){
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


#' Check whether an object is a textmeta object
#'
#' Tests whether an object is a valid \pkg{tosca} `textmeta` object.
#' It simply checks if it is a list with `meta` and `text` components.
#'
#' @param obj Any R object.
#' @return Logical scalar: `TRUE` if object is a `textmeta`, `FALSE` otherwise.
#' @export
#'
#' @examples
#' is.textmeta(list(meta = data.frame(), text = list()))
#' is.textmeta("not a textmeta")
is.textmeta = function(obj){
  is.list(obj) && all(c("meta","text") %in% names(obj))
}
