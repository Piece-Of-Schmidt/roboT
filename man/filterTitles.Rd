% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/04_corpus_cleaning.R
\name{filterTitles}
\alias{filterTitles}
\title{Filter texts based on their title}
\usage{
filterTitles(
  corpus,
  titles,
  pattern = F,
  ignore.case = F,
  print = F,
  invert = T,
  out = "text"
)
}
\arguments{
\item{corpus}{A \code{textmeta} object (as used in the \pkg{tosca} package).}

\item{titles}{A character vector of titles or patterns to match.}

\item{pattern}{Logical. If \code{TRUE}, uses regex matching instead of exact title match.}

\item{ignore.case}{Logical. Should case be ignored in pattern matching?}

\item{print}{Logical. If \code{TRUE}, prints the matching titles.}

\item{invert}{Logical. If \code{TRUE} (default), returns texts that \emph{do not} match the titles.}

\item{out}{Character. \code{"text"} (default) to return filtered corpus, \code{"bin"} to return logical mask.}
}
\value{
Either a filtered \code{textmeta} object or a logical vector (depending on \code{out}).
}
\description{
Removes or selects texts from a \code{textmeta} object based on their title.
Supports exact title matches or regular expression patterns.
}
\examples{
# Remove all texts with title "Leserbriefe"
# corpus <- filterTitles(corpus, titles = "Leserbriefe", pattern = FALSE)
}
