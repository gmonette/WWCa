% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/survey.R
\name{ns}
\alias{ns}
\title{Sample and population strata counts}
\usage{
ns(sample, target_pop, formula)
}
\arguments{
\item{sample}{a data frame including stratification variables}

\item{target_pop}{a data frame with stratification variables and a variable named 'population' 
that contains population counts for each row. The data frame can include other stratification 
variables than those in \code{fmla} and counts will be aggregated over these variables}

\item{fmla}{a one-sided formula of the form \code{~a + b + c ...} where 'a', 'b', 'c', ... are 
stratification variables in both \code{sample} and in \code{target_pop}}
}
\value{
the \code{sample} data frame with two additional variables 'n', 'N' giving the stratum
       counts in the sample and in the population, respectively. If some strata combinations are
       missing in the sample, a row for each missing combination is added with n equal to 0 and 
       N equal to the count for that stratum in the population
}
\description{
Calculate counts for the stratum for each sampled unit
}

