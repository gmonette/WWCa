#' Population table (IN PROGRESS)
#' 
#' Population table by sex, age group (14 groups), raceethnicity (5 categories) within country (US and Puerto Rico),
#' state (50 states plus DC and PR) and county.
#' 
#' @format A data frame with 450800 rows and 8 variables.
#' \describe{
#'   \item{country}{a factor: US or PR}
#'   \item{state}{a factor: US state, DC or PR}
#'   \item{state.code}{2-letter state abbreviation and 2-digit code containing the first 2 digits
#'   of county codes}
#'   \item{sex}{a factor: Male or Female}
#'   \item{age}{an ordered factor with 14 age groups}
#'   \item{raceethnicity}{a factor with 4 categories plus 'OTHER'}
#'   \item{population}{a numeric variable with the population (IN PROGRESS -- USE ONLY FOR TESTING) in
#'   each row. Negative values in 'acsagetable' have been set to zero}
#'   \item{population_neg}{a numeric variable with values of population in 'acsagetable' including
#'   negative values}
#' }
#' @source 'acsagetable' from the WWC package. See the script creating this data frame in 'etc/popagetable-data.R'
#' @name popagetable
NULL