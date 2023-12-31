% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/mockVocabularyCdm.R
\name{mockVocabularyCdm}
\alias{mockVocabularyCdm}
\title{It creates a mock database with the vocabulary.}
\usage{
mockVocabularyCdm(
  cdmSource = NULL,
  concept = NULL,
  vocabulary = NULL,
  domain = NULL,
  conceptClass = NULL,
  conceptRelationship = NULL,
  conceptSynonym = NULL,
  conceptAncestor = NULL,
  sourceToConceptMap = NULL,
  drugStrength = NULL,
  cdmVersion = "5.3",
  cdmName = "mock cdm"
)
}
\arguments{
\item{cdmSource}{cdm source table.}

\item{concept}{Concept table.}

\item{vocabulary}{Vocabulary table}

\item{domain}{Domain table.}

\item{conceptClass}{Concept_class table.}

\item{conceptRelationship}{Concept_relationship table.}

\item{conceptSynonym}{Concept_synonym table.}

\item{conceptAncestor}{Concept_ancestor table.}

\item{sourceToConceptMap}{Source_to_concept_map table.}

\item{drugStrength}{Drug_strength table.}

\item{cdmVersion}{cdm version.}

\item{cdmName}{Name of the cdm.}
}
\value{
A cdm reference with the vocabulary mock tables
}
\description{
It creates a mock database with the vocabulary.
}
\examples{
\donttest{
library(OmopMocker)
cdm <- "cdm"
cdm
}

}
