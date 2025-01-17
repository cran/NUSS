% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/unigram_dictionary.R
\name{unigram_dictionary}
\alias{unigram_dictionary}
\title{Create unigram dictionary}
\usage{
unigram_dictionary(texts, points_filter = 1)
}
\arguments{
\item{texts}{character vector, these are the texts used to create ngrams
dictionary. Case-sensitive.}

\item{points_filter}{numeric, sets the minimal number of points (occurrences)
of an unigram to be included in the dictionary.}
}
\value{
The output always will be data.frame with 4 columns: 1) to_search,
2) to_replace, 3) id, 4) points.
}
\description{
\code{unigram_dictionary} returns the data.frame containing dictionary for
\link{unigram_sequence_segmentation}.
}
\examples{
texts <- c("this is science",
           "science is #fascinatingthing",
           "this is a scientific approach",
           "science is everywhere",
           "the beauty of science")
unigram_dictionary(texts)

}
