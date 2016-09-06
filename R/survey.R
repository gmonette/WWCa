#' Population frequencies
#' 
#' This data frame should be used only for testing
#' 
#' This data frame contains the number of individuals within each county in the 
#' US and Puerto Rico (PR) in clusters formed by combining categories of 
#' age (in 14 categories), raceethnicity (in 5 categories) and sex (Male or Female).
#' Each county is also identified by its 5-digit county code,  
#' its state (50 states using 2-letter abbreviations plus DC and PR),
#' and its country (US or PR). 
#' 
#' @format A data frame with 450,800 rows and 8 variables.
#' \describe{
#'   \item{state.code}{first 2 digits of county code identifying state}
#'   \item{sex}{Male or Female}
#'   \item{age}{an ordered factor with 14 age categories}
#'   \item{raceethnicity}{in 6 categories}
#'   \item{county}{5-digit county code}
#'   \item{state}{2-letter state abbreviation}
#'   \item{country}{US or PR for Puerto Rico}
#'   \item{date}{date at which population count is valid}
#'   \item{population}{count of people in a particular combination (Note that this number has been imputed through some process and should be used only for testing, not for production)}
#'   \item{population_neg}{count of people in a particular combination including negative values to be explained}
#' }
#' @source Datassist plus some arbitraty adjustments for population
#' @name popagetable
NULL
#' Functions for post-stratification adjustments in surveys
#' 
#' # Main utilities here:
#'
#' capply(x, by, FUN, ...): applies FUN to each 'chunk' of x defined by
#'                    levels of 'by'. "..." are additional arguments to FUN.
#'                    'by' can be a variable or a list, often a set of columns 
#'                    in the data set in which 'x' lives. The result has the 
#'                    same shape as 'x' and can thus be added as a variable to
#'                    the data frame for 'x'. If FUN returns a single value in
#'                    each chunk, the value is recycled to fill the elements
#'                    corresponding to the chunk.
#'
#'                    For example, in a data frame, data, with variables: 
#'                        state, county, sex, population
#'                    where population is the population size within each
#'                    county (within state) by sex combination:
#'                    > data$pop.state <- with(data,
#'                          capply(population, list(sex,state), sum))
#'                    creates a variable that is equal to the total population
#'                    within each state x sex combination repeated, of course, for
#'                    each county.
#'
#' up(data, by)       keeps rows corresponding to unique values of the strata
#'                    defined by the variable or list of variables in 'by'.
#'                    'by' can also be a formula, evaluated in 'data'. For example,
#'                    'by = ~ a + b', is equivalent to 'by = data[,c('a','b')]'.
#'                    For example, with 'data' used above:
#'                    
#'                    > data.state <- up(data, ~ state + sex)
#'
#'                    creates a data frame with one row per state x sex combination
#'                    and the variable 'pop.state' contains the total population in
#'                    in each combination. Variables, such as 'county' and 'population'
#'                    that are not invariant within levels of 'by' are dropped.
#'
#' tab(data, fmla):    
#' 
#' creates a frequency table showing the number of rows in 'data' for
#' each stratum formed by evaluating the formula 'fmla' in 'data'.
#' 
#' For example 'tab(data, ~ a + b + c)' will return an array of frequencies
#'                    for variables 'a', 'b', and 'c' in 'data'. 
#'                    If the formula has a variable on the left side, that variable is
#'                    summed to get the entries of the table. 
#'
#'                    For example, if 'population' contains the population in each row
#'                    where each row is a 'sex x state x county' combination:
#'
#'                    > tab(data, population ~ sex + age)  
#'
#'                    will produce a table of total population by sex and age, and
#'
#'                    > tab(data, population ~ age)
#'
#'                    will show overall totals by age group.  
#'
#'                    These tables can be tranformed into data frames with, e.g.,
#'
#'                    > as.data.frame(tab(data, population ~ age))
#' 
#' @family post-stratification functions
#' @seealso \code{\link{wtd_mean}} for weighted means, \code{link{lin_comb}} for linear
#'      combinations, \code{\link{jk_wtd_mean_se}} for a jacknife estimate of the SE of a weighted mean
#'      and \code{\link{jk_lin_comb_se}} for a jacknife estimate of the SE of a linear combination.
#' @name survey
NULL
## NULL
#' Weighted mean 
#' 
#' @param x numerical vector
#' @param w vector of weights replicated to have same length as \code{x}
#' @family Post-stratification survey functions
#' @seealso \code{\link{wtd_mean}} for weighted means, \code{link{lin_comb}} for linear
#'      combinations, \code{\link{jk_wtd_mean_se}} for a jacknife estimate of the SE of a weighted mean
#'      and \code{\link{jk_lin_comb_se}} for a jacknife estimate of the SE of a linear combination.
#' @export
wtd_mean <- function(x, w) {
  w <- rep(w, length.out = length(x))
  sum(x*w) / sum(w)
}
#' Post-stratification jacknife estimates of standard error
#' 
#' Jacknife estimates of the weighted mean with a stratified sample. 
#' Weights are adjusted to take the change in n into account when obtaining the
#' jacknife estimate dropping an observation within a stratum.
#' @param x numerical vector
#' @param by stratification variable or list of stratification variable
#' @param w vector of weights, e.g. Horvitz-Thomson weights or renormed version
#' @param check (default: TRUE) check that 'w' is constant within levels of 'by'
#' @param FUN (default: wtd_mean) function used to obtain estimate
#' @family Post-stratification survey functions
#' @seealso \code{\link{wtd_mean}} for weighted means, \code{link{lin_comb}} for linear
#'      combinations, \code{\link{jk_wtd_mean_se}} for a jacknife estimate of the SE of a weighted mean
#'      and \code{\link{jk_lin_comb_se}} for a jacknife estimate of the SE of a linear combination.
#' @export
jk_wtd_means <- function(x, by, w, check = TRUE, FUN = wtd_mean){
  # Value: vector of 'drop-one' estimates for jacknife estimator of SE
  #
  # x: response variable, a numeric vector
  # by: vector or list of vectors defining strata
  # w: numeric vector of sampling weights constant within each stratum
  # Check that weights are constant within levels of 'by'
  # library(spida2)  # devtools::install_github('gmonette/spida2')
  if(check) {
    constant <- function(z) length(unique(z)) <= 1 
    constant_by <- function(z, by) all(sapply(split(z, by), constant))
    if(!constant_by(w, by)) stop("w must constant within strata")
  }
  ns <- capply(x, by, length)
  force(w)
  by <- interaction(by)
  wtd_mean_drop_i <- function(i) {
    ns_drop <- capply(x[-i], by[-i], length)
    w_drop <- w[-i] * ns[-i] / ns_drop
    FUN(x[-i], w_drop)
  }
  sapply(seq_along(x), wtd_mean_drop_i)
}
#' Post-stratification jacknife estimate of SE of weighted mean
#' 
#' Jacknife estimates of the weighted mean with a stratified sample. 
#' Weights are adjusted to take the change in n into account when obtaining the
#' jacknife estimate dropping an observation within a stratum.
#' @param x numerical vector
#' @param by stratification variable or list of stratification variable. Default: single level for unstratified samples
#' @param w vector of weights, e.g. Horvitz-Thomson weights or renormed version. Default: 1
#' @param FUN (default: wtd_mean) function used to obtain estimate
#' @examples
#' \dontrun{
#' #
#' # This is a made-up survey in 3 states along the eastern edge of the Rockies.
#' #
#' ds <- read.table(header = TRUE, text = "
#'                  Gender   State    Income
#'                  Male     Utah        10
#'                  Male     Utah        11
#'                  Male     Utah        12
#'                  Male     Utah        13
#'                  Male     Utah        14
#'                  Male     Utah        15
#'                  Female   Utah        16
#'                  Female   Utah        17
#'                  Female   Utah        18
#'                  Female   Utah        19
#'                  Male     Idaho       20
#'                  Male     Idaho       21
#'                  Male     Idaho       22
#'                  Female   Idaho       23
#'                  Female   Idaho       24
#'                  Female   Idaho       25
#'                  Female   Idaho       26
#'                  Female   Idaho       27
#'                  Female   Idaho       28
#'                  Female   Idaho       29
#'                  Male     Arizona     30
#'                  Male     Arizona     31
#'                  Male     Arizona     32
#'                  Male     Arizona     33
#'                  Male     Arizona     34
#'                  Male     Arizona     35
#'                  Male     Arizona     36
#'                  Female   Arizona     37
#'                  Female   Arizona     38
#'                  Female   Arizona     39
#'                  ")
#' #
#' # and the population within each stratum (imaginary data):
#' # 
#' dpop <- read.table(header = TRUE, text = "
#'                    Gender   State          N
#'                    Male     Utah     1500000
#'                    Female   Utah     1500000
#'                    Male     Idaho     700000
#'                    Female   Idaho     900000
#'                    Male     Arizona  4000000
#'                    Female   Arizona  3000000
#'                    ")
#' # 
#' # First we create the 'overall' $n$ and $N$ variables.
#' # 
#' ds$n <- with(ds, capply(State, list(State, Gender), length))  
#' # 
#' # Note that first argument of 'capply', 'State', could be any variable in the dataset
#' # since it is used only for the length of the chunks within each 'State' by 'Gender' stratum.
#' # 
#' ds <- merge(ds, dpop)
#' head(ds)
#' #
#' # ### Overall Horvitz-Thompson weights
#' # 
#' ds$w_o_ht <- with(ds, N/n)
#' head(ds)
#' sum(ds$w_o_ht)
#' 
#' with(ds, wtd_mean(Income, w_o_ht))
#' with(ds, jk_wtd_mean_se(Income, list(Gender,State), w_o_ht))
#' with(ds, jk_wtd_mean_se(Income))
#' sd(ds$Income)/sqrt(length(ds$Income))
#' #
#' # ### Weighted mean within Utah
#' #
#' with(subset(ds, State == "Utah"), wtd_mean(Income, w_o_ht))
#' with(subset(ds, State == "Utah"), jk_wtd_mean_se(Income, Gender, w_o_ht))
#' # alternatively:
#' with(ds, wtd_mean(Income, w_o_ht * (State == "Utah")))
#' with(ds, jk_wtd_mean_se(Income, list(Gender, State), w_o_ht * (State == "Utah")))
#' #
#' # ### Weighted mean within Utah and Idaho
#' #
#' with(subset(ds, State %in% c("Utah","Idaho")), wtd_mean(Income, w_o_ht))
#' with(subset(ds, State %in% c("Utah","Idaho")), jk_wtd_mean_se(Income, list(Gender, State), w_o_ht))
#' # alternatively:
#' with(ds, wtd_mean(Income, w_o_ht * (State %in% c("Utah","Idaho"))))
#' with(ds, jk_wtd_mean_se(Income, list(Gender, State), w_o_ht * (State %in% c("Utah","Idaho"))))
#' #
#' # ### Weighted female income
#' #
#' with(subset(ds, Gender == "Female"), wtd_mean(Income, w_o_ht))
#' with(subset(ds, Gender == "Female"), jk_wtd_mean_se(Income, list(Gender, State), w_o_ht))
#' # alternatively:
#' with(ds, wtd_mean(Income, w_o_ht * (Gender == "Female")))
#' with(ds, jk_wtd_mean_se(Income, list(Gender, State), w_o_ht * (Gender == "Female")))
#' #
#' # We would get the same results with weights normed differently.
#' # 
#' # ### 'Sum to $n$' weights
#' # 
#' ds$w_o_sn <- with(ds, (N/n)/mean(N/n))
#' sum(ds$w_o_sn)
#' 
#' with(ds, wtd_mean(Income, w_o_sn))
#' with(ds, jk_wtd_mean_se(Income, list(Gender,State), w_o_sn))
#' # 
#' # ### 'Sum to 1' weights:
#' # 
#' ds$w_o_s1 <- with(ds, (N/n)/sum(N/n))
#' sum(ds$w_o_s1)
#' 
#' with(ds, wtd_mean(Income, w_o_s1))
#' with(ds, jk_wtd_mean_se(Income, list(Gender,State), w_o_s1))
#' 
#' #### Gender gap in mean salary in Utah
#' 
#' # With linear combinations, we can estimate population parameters that can't be
#' # estimated as weighted means. For example, the Gender gap in Utah:
#' 
#' coeffs <- with(ds, 
#'                std((State == "Utah") * (Gender == "Male") * w_o_ht)
#'                - std((State == "Utah") * (Gender == "Female") * w_o_ht) )
#' names(coeffs) <- with(ds, interaction(State,Gender))
#' cbind(coeffs)
#' with(ds, lin_comb(Income, coeffs))
#' with(ds, jk_lin_comb_se(Income, list(State, Gender), coeffs))
#' #'
#' #' #### Comparing two states
#' #' 
#' #' To compare Utah with Idaho using the population proportions of Gender:
#' #' 
#' coeffs <- with(ds, 
#'                std((State == "Utah") * w_o_ht)
#'                - std((State == "Idaho") *  w_o_ht) )
#' coeffs
#' names(coeffs) <- with(ds, interaction(State,Gender))
#' cbind(coeffs)
#' with(ds, lin_comb(Income, coeffs))
#' with(ds, jk_lin_comb_se(Income, list(State, Gender), coeffs))
#' #'
#' #' #### Unweighted average of two states
#' #' 
#' #' The unweighted average of Utah and Idaho using the Gender proportion in each state:
#' #' 
#' coeffs <- with(ds, 
#'                .5 * std((State == "Utah") * w_o_ht)
#'                + .5 * std((State == "Idaho") *  w_o_ht) )
#' coeffs
#' with(ds, lin_comb(Income, coeffs))
#' with(ds, jk_lin_comb_se(Income, list(State, Gender), coeffs))        
#' #'
#' #' #### The Gender gap within each state
#' #' 
#' states <- c("Idaho", "Utah", "Arizona")
#' names(states) <- states
#' coefs_gap <- lapply( states,
#'                      function(state)
#'                        with(ds, std((State == state) * (Gender == "Male") * w_o_ht) -
#'                               std((State == state) * (Gender == "Female") * w_o_ht) ))
#' mat <- do.call(cbind,coefs_gap)
#' rownames(mat) <- with(ds, interaction(State,Gender))
#' MASS::fractions(mat)
#' with(ds, lapply(coefs_gap, function(w) lin_comb(Income,w)))
#' with(ds, lapply(coefs_gap, function(w) jk_lin_comb_se(Income,list(Gender, State), w)))
#' #' 
#' #' ## Hierarchical weights
#' #' 
#' #' The following illustrates how to create within-state weights and then how to modify them to form overall 
#' #' weights.
#' #' 
#' #' We can start with the convenient fact that Horvitz-Thompson weights serve as both within-state and
#' #' overall weights.  To get within-state 'sum to $n$' weights, we need to norm the Horvitz-Thompson weights
#' #' within each state. We need to divide the Horvitz-Thompson weights by their within-state means:
#' #' 
#' ds$ht_ws_mean <- with(ds, capply(w_o_ht, State, mean))
#' ds$w_ws_sn <- with(ds, w_o_ht / ht_ws_mean)   # within-state 'sum to n' mean
#' 
#' with(ds, tapply(w_ws_sn, State, sum)) # check that they sum to within-state n
#' tab(ds, ~ State)
#' #'
#' #' As noted earlier, these weights need to be multiplied by $\frac{N_s/N}{n_s/n}$ to transform them into
#' #' overall 'sum to $n$' weights. The easiest way to generate $N_s$ is to use the Horvitz-Thomson weights:
#' #' 
#' ds$N_s <- with(ds, capply(w_o_ht, State, sum))
#' ds$n_s <- with(ds, capply(w_o_ht, State, length))
#' N_total <- sum(ds$w_o_ht)
#' n_total <- nrow(ds)
#' 
#' ds$w_o_sn2 <- with(ds, w_ws_sn * (N_s / N_total) / (n_s/n_total))
#' ds
#' #' Comparing these weighs with previous ones:
#' with(ds, max(abs(w_o_sn2 - w_o_sn)))
#' #'
#' #' Estimating means per stratum
#' #'
#' ds$mean_sg <- capply(ds, ds[c('State','Gender')], with, wtd_mean(Income, w_o_ht))
#' ds$mean_sg_se <- capply(ds, ds[c('State','Gender')], with, jk_wtd_mean_se(Income, w = w_o_ht))
#' ds$mean_s <- capply(ds, ds$State, with, wtd_mean(Income, w_o_ht))
#' ds$mean_s_se <- capply(ds, ds$State, with, jk_wtd_mean_se(Income, Gender, w_o_ht))
#' ds$mean_g <- capply(ds, ds$Gender, with, wtd_mean(Income, w_o_ht))
#' ds$mean_g_se <- capply(ds, ds$Gender, with, jk_wtd_mean_se(Income, State, w_o_ht))
#' # Define a round method for data frames so round will work on a data frame with non-numeric variables
#' round.data.frame <- function(x, digits = 0) as.data.frame(lapply(x, function(x) if(is.numeric(x)) round(x, digits = digits) else x))  
#' round(ds, 3)
#' dssum <- up(ds, ~ State/Gender) # keep all State x Gender invariant variables
#' round(dssum, 3)
#' }
#' @family Post-stratification survey functions
#' @seealso \code{\link{wtd_mean}} for weighted means, \code{link{lin_comb}} for linear
#'      combinations, \code{\link{jk_wtd_mean_se}} for a jacknife estimate of the SE of a weighted mean
#'      and \code{\link{jk_lin_comb_se}} for a jacknife estimate of the SE of a linear combination.
#' @export
jk_wtd_mean_se <- function(x, by = rep(1, length(x)), w = rep(1, length(x)), FUN = wtd_mean) {
  # Value: jacknife estimator of the standard error of a weighted mean
  # using post-stratification
  #
  # x: response variable, a numeric vector
  # by: vector or list of vectors defining strata
  # w: numeric vector of sampling weights constant within each stratum
  #
  theta_hat <- FUN(x, w)
  theta_hat_i <- jk_wtd_means(x, by, w, FUN = FUN)
  ns <- capply(x, by, length)
  sqrt(sum((theta_hat_i - theta_hat)^2 * (ns-1) / ns))
}
#' 
#'
#' ## Linear combinations
#' 
#' ### Functions for linear combinations
#' 
#' Standardize a vector of weights
#' 
#' @param x vector of weights
#' @param div divisor (default: sum(x))
#' @rdname survey
#' @export
std <- function(x, div = sum(x)) x/div
#' 
#' Linear combination of x
#' 
#' @param x numerical vector
#' @param w vector of weights replicated to have same length as 'x'
#' @family Post-stratification survey functions
#' @export
lin_comb <- function(x, w) {
  w <- rep(w, length.out = length(x))
  sum(x*w)
}
#' Post-stratification Jacknife estimate of SE for a linear combination
#' 
#' @inheritParams jk_wtd_mean_se
#' @rdname survey
#' @export
jk_lin_comb_se <- function(x, by, w, check = TRUE) jk_wtd_mean_se(x, by, w, FUN = lin_comb)
#' 
function (lhs, rhs) 
{
  parent <- parent.frame()
  env <- new.env(parent = parent)
  chain_parts <- split_chain(match.call(), env = env)
  pipes <- chain_parts[["pipes"]]
  rhss <- chain_parts[["rhss"]]
  lhs <- chain_parts[["lhs"]]
  env[["_function_list"]] <- lapply(1:length(rhss), function(i) wrap_function(rhss[[i]], 
                                                                              pipes[[i]], parent))
  env[["_fseq"]] <- `class<-`(eval(quote(function(value) freduce(value, 
                                                                 `_function_list`)), env, env), c("fseq", "function"))
  env[["freduce"]] <- freduce
  if (is_placeholder(lhs)) {
    env[["_fseq"]]
  }
  else {
    env[["_lhs"]] <- eval(lhs, parent, parent)
    result <- withVisible(eval(quote(`_fseq`(`_lhs`)), env, 
                               env))
    if (is_compound_pipe(pipes[[1L]])) {
      eval(call("<-", lhs, result[["value"]]), parent, 
           parent)
    }
    else {
      if (result[["visible"]]) 
        result[["value"]]
      else invisible(result[["value"]])
    }
  }
}

