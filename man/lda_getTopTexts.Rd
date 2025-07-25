% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/07_lda_evaluation.R
\name{lda_getTopTexts}
\alias{lda_getTopTexts}
\title{Export top documents per LDA topic to Excel}
\usage{
lda_getTopTexts(
  corpus,
  ldaresult,
  ldaID,
  nTopTexts = 50,
  file = "topTexts",
  translate = F,
  max_text_length = 32000,
  source_lang = NULL,
  deepl_key = NULL
)
}
\arguments{
\item{corpus}{A \code{textmeta} object or compatible list.}

\item{ldaresult}{LDA result from \code{tosca::LDAgen()}.}

\item{ldaID}{Vector of document IDs used in the model.}

\item{nTopTexts}{Number of top documents per topic (default: 50).}

\item{file}{Base filename for Excel export (default: \code{"topTexts"}).}

\item{translate}{Logical. Should text and titles be translated using DeepL?}

\item{max_text_length}{Maximum length of exported text strings (default: 32000).}

\item{source_lang}{Source language code (e.g., \code{"EN"}).}

\item{deepl_key}{DeepL API authentication key.}
}
\value{
Invisibly returns a list of data frames with top texts per topic.
}
\description{
For each topic, retrieves the most representative documents and optionally translates
them via DeepL. Outputs enhanced data with topic relevance and source, and writes them
to a local Excel file.
}
\examples{
# lda_getTopTexts(corpus, ldaresult, ldaID, nTopTexts = 20)
}
