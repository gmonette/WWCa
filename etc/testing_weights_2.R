#' ---
#' title: "Weighting for online surveys"
#' author: "Georges Monette"
#' date: "`r Sys.Date()`"
#' output: 
#'  html_document:
#'    toc: true
#'    toc_depth: 4
#'    toc_float: true
#'    keep_md: yes
#' bibliography: WWC.bib
#' link-citations: yes
#' ---
#+ setup, include=FALSE, eval=FALSE
devtools::install_github('gmonette/WWCa')
install.packages('magrittr')  # to use the '%>%' pipe imported from magrittr to dplyr
#+ load, include=FALSE
library(WWCa)
library(magrittr) # the original package for the '%>%' pipe
library(lattice)
library(latticeExtra)
library(knitr)
opts_chunk$set(comment=NA)
opts_knit$set(progress = TRUE, width = 90)
#' 
#' # Introduction
#' 
#' This document gives examples using of a few tools for survey analysis in an R package called 
#' [WWCa](https://github.com/gmonette/WWCa). The package borrows from and is inspired by the 
#' [WWC](https://github.com/heathermkrause/WWC).
#' 
#' Currently, the package contains:
#' 
#' 1. Two 'population' data frames with census counts by county for the U.S. and Puerto Rico. One data frame has
#'    counts for age, sex and raceethnicity by county and the second (to come) has counts for age,
#'    sex and education by county. __Q: Is is possible to get a sex by age by education by raceethnicity 
#'    data frame?__ These data frames were constructed from the 'acsagetable' and 'acsedutable' 
#'    data frames in the [WWC package](https://github.com/heathermkrause/WWC).
#' 1. A function 'sam' to create simulated samples from the population data frames. The function 
#'    accepts a sampling factor to allow over- or undersampling in arbitrary strata. Currently, the 
#'    response is binary and the probability of a 'yes' can also be specified in any strata.   
#' 2. A number of data manipulation utilities adapted from the 
#'    [spida2 package](https://github.com/gmonette/spida2).
#'    The main functions are 'tab' and 'tab_df' to create marginal frequency and population counts. 
#'    Other functions include 'capply', a function in the 'apply' family that allows function to
#'    operate within strata. 
#' 3. Functions for weighting and calculating weighted and unweighted estimates of population parameters.
#'    All estimates and standard errors are computed with the function 'est'.  Other estimators can be
#'    added to this function.
#' 
#' ## Overview of estimation functions
#' 
#' The basis of survey weights are population and sample counts within strata and the ratio of these two
#' counts, 
#' the Horvitz-Thompson weights.  The 'wts' function computes these quantities.
#'  
#' The structure of the functions should make it easy to add additional weight fitting methods and 
#' to identify nested hierarchies of subpopulations for which a sample can provide estimates with
#' different levels of reliability.
#' 
#' The 'wtd_mean' function estimates a weighted mean and a companion function 'jk_wtd_mean_se' estimates
#' a jacknife estimate of its standard error.  The functions 'lin_comb' and 'jk_lin_comb_se' provide
#' corresponding estimates for linear combinations. 
#' 
#' ## In progress
#' 
#' Methods to calculate and apply survey weights will be extended so they can include raking and generalized 
#' regression.  
#' 
#' Different weights will allow the identification of different hierarchies of sub-populations for which
#' the sample estimates have greater or lesser degree of reliability.   
#' 
#' ## Tools to view R objects
#' 
#' Define a flexible method to look at data frames
#' 
info <- function(x, ...) UseMethod('info')
info.data.frame <- function(x, n = 6, verbose = F, ...) {  
  cat("Dim: ",dim(x),"\n")
  if(nrow(x) <= n) print(x)
  else print(x[sample(nrow(x), n),])
  if(verbose) print(sapply(x,class))
  invisible(x)
}
info.default <- function(x,...) {
  cat("Not implemented for ", class(x),"\n")
  invisible(x)
}
info_ <- function(x,...) info(x, ..., verbose = TRUE)
#'
#' ## Computing population and sample counts
#' 
#' To weight a survey we need to specify the target population -- for example, 
#' the population of a specific state, the voting age population of a given subsets
#' of states, adult females between 18 and 65, etc. --
#' and we need to identify the variables to be 
#' used for adjustment -- for example,  sex and age, sex and raceethnicity, 
#' age and state, etc.
#' 
#' Including variables for adjustment reduces bias to the extent that 
#' 
#' 1. the variables are related to the response, __and__
#' 2. the distribution of the variables in the sample is different from that in 
#'    the population. 
#' 
#' Since there are so many possible combinations of target subpopulations and
#' choices of variables for adjustment, it would seem easier, if it can be done
#' efficiently, to have a function
#' that creates a population count data frame for any specified target and 
#' set of adjustment variables. 
#' 
#' The data frame 'popagetable' in this package was created from 'acsagetable' in 
#' 'WWC' by keeping the rows
#' corresponding to the finest level of aggregation, the 'county' level, and creating
#' variables for 'state' (50 states plus DC and PR for Puerto Rico) 
#' and 'country' (US or PR).
#' 
#' A number of negative population counts for categories in counties were imputed to 0.
#' The original population count variable with negative values is named 'population_neg'. 
#' 
#' The following example show how the 'subset' function (in the base package) combined with
#' 'tab' and 'tab_df' which are modified version of 'table' (in base), can be used to
#' create a population count data frame.
#' 
#' 
popagetable %>% info
#' Number of rows in each stratume
popagetable %>% tab(~age + sex)   
#' Population in each stratum
popagetable %>% tab(population ~ age + sex)   
#' Row percentages
popagetable %>% tab(population ~ age + sex, pct = 1)   %>% round(1)  
#' Column percentages
popagetable %>% tab(population ~ age + sex, pct = 2)   %>% round(1)  
#' As a data frame
popagetable %>% tab_df(population ~age + sex)    
#' Data frame with percentages: note that 'All' is included but not 'Total'
popagetable %>% tab_df(population ~age + sex, pct = 1)   
#' 
#' ## Negative and zero population values
#' 
#' Overall, about 1% of rows (strata x county combinations) have 
#' negative population values and 30% have zero values. 
#' 
popagetable %>% tab(~state + sign(population_neg))
popagetable %>% tab(~state + sign(population_neg), pct = 1) %>% round(1) 
#'
#' Aggregating over counties, at the state level all the negative values occur for
#' 'raceethnicity' equal to 'OTHER' in Puerto Rico.
#' 
popagetable %>% 
  tab_df(population_neg ~ sex + age + raceethnicity + state) %>% 
  subset(population_neg < 10)

#'
#' Focusing on California
#' 
popagetable %>% subset(state == "CA")  %>% tab(population ~ age + sex)
popagetable %>% subset(state == "CA")  %>% tab(population ~ age + sex, pct = 1) %>% round(2)
popagetable %>% subset(state == "CA")  %>% tab(population ~ age + sex, pct = 2) %>% round(2)
popagetable %>% subset(state == "CA")  %>% tab(population ~ age + sex, pct = 0) %>% round(2)
popagetable %>% subset(state == "CA")  %>% tab_df(population ~ age + sex)    # as a data frame
#'
#' Selecting a subset: female adults in NY and PA below 85 years of age. Note that age is an ordered
#' factor.
#'
targetdf <- popagetable %>% 
  subset(state %in% c('NY','PA') & 
           sex == "Female" &
           age > "15 to 17 years" &
           age < "85 years and over") %>% 
  tab_df(population ~ age + sex + state)
targetdf
#'
#' # Creating a sample
#'
#' Imagine that we are interested in making inferences about adults in AK using an online tool targeted
#' at them but inevitably contaminated by respondents that are out of scope.
#' 
#' Here, we create a sample using basic tools with the intention of packaging the method in a function.
#' 
#' To control the extent of over- and under-sampling, we generate a vector 'fac' to determine
#' relative sampling from each stratum. 
#' 
#' Here, we create a sample consisting mainly of adults in AK contaminated by other respondents. Later we
#' consider estimates and standard error estimates for various target populations. 
#' 
#' We suppose that the response variable has two values, 'yes' or 'no', and that the 
#' probability of a 'yes' depends on age and sex but not on raceethnicity. The probability of 
#' a 'yes' will be contained in a vector 'pr'.
#' 
#' We consider the consequences of adjusting for various combinations of the 
#' three variables.
#' 
#' The questionnaire will solicit information on raceethnicity, sex, age and state but not on county.
#' 
#' ## Step 1: Create a population data frame, aggregated over counties, with sampling parameters  
#' 
#' Aggregate to state level:
#' 
pop_agg <- popagetable %>% tab_df(population ~ sex + age + raceethnicity + state)
pop_agg %>% info
pop_agg %>% summary

#' Select main group targeted and assign values for sampling parameters

sample_parms <- subset(pop_agg, state == 'CA' & age > "18 and 19 years" & age < '65 to 74 years') %>% 
  tab_df(~ sex + age + state)
sample_parms %>% dim
sample_parms
#'
#' Assign sampling factor and probability of a yes:
#' 
#' We pretend that the sample is disproportionaly young among women and old among men
#' by creating a relative sampling factor. Since the sampling factors and the response
#' distribution don't depend on ethnicity, we don't need to take that into account at this
#' stage. 
sample_parms$Freq <- NULL
sample_parms$fac <- c(3,2,2,1,1,1,1,1,1,2,2,3)  # relative sampling factor
sample_parms$pr <- c(4,2,1,1,1,.5,.5,1,1,1,3,4)/5  # probability of a YES
sample_parms
#'
#' Merge into sampling data frame and assign values to remaining 'fac' and 'pr'
#'
pop_agg <- merge(pop_agg, sample_parms, all.x = T)
pop_agg %>% summary
pop_agg$fac[is.na(pop_agg$fac)] <- 0.01
pop_agg$pr[is.na(pop_agg$pr)] <- 0.5
pop_agg %>% summary
#'
#' ## Step 2: Sample respondents
#'
#' Take a sample of N = 1000 with probabilities proportional to 
#' population proportion times 'fac'
#' 
N <- 1000
pop_agg$prob <- with(pop_agg, population * fac)
sample_rows <- sample(nrow(pop_agg), N, replace = T, prob = pop_agg$prob)
#' Create sample data frame
sample <- pop_agg[sample_rows,]
sample %>% info
sample %>% tab(~state == 'CA')
#'
#' ## Step 3: Generate response
#'
sample$y <- rbinom(nrow(sample), 1, prob = sample$pr)

#'
#' Get rid of information we would not normally have in a sample
#'
sample <- subset(sample, select = c(sex, age, raceethnicity, state, y)) # keeps all the rows, selects columns
sample %>% info
#'
#' ## Sampling function
#' 
#' We put all this together in a sampling function
#'
sam <- function(N = 1, 
                pop = popagetable, # population frequency table
                fmla = population ~ sex + age + raceethnicity + state, # aggregation formula
                subset, # an expression that selects a subset, e.g. state == 'AK' & age > '5 to 9 years'
                fac, # a data frame or list of data frames to assign relative sampling frequencies,
                    # variable names are the RHS names in 'fmla' and 'fac', a numeric sampling factor
                   # If 'fac' is a list, the factors are applied multiplicatively
                prob) # same as fac except that prob. of a 'yes' is given in variable 'prob'. They are
                      # combined by summing log odds
                    # TO BE GENERALIZED!!
{
  # Note: reserved names: fac, prob, y 
  # local functions
  last <- function(x) x[length(x)]
  na2 <- function(x, rep = 0) {
    x[is.na(x)] <- rep
    x
  }
  # sampling frame
  sampling_frame <- tab_df(pop, fmla)
  library(magrittr) # for pipes
  strat_vars <- fmla %>% 
    as.character %>% 
    last %>% 
    gsub(" ","",.) %>% 
    strsplit("[*+/:]") %>% 
    unlist
    
  if(!missing(subset)) { # code from 'subset.data.frame'
    e <- substitute(subset)
    r <- eval(e, sampling_frame, parent.frame())
    if(!is.logical(r)) stop("'subset' must be logical")
    sampling_frame <- sampling_frame[r & !is.na(r),]
  } 
  # sampling factor
  sampling_frame$fac <-1
  if(!missing(fac)) {
    if(is.data.frame(fac)) fac <- list(fac)
    fac <- lapply(fac, function(d) d[,c(intersect(names(d),strat_vars),'fac')])
    for(dd in fac){
      sampling_frame <- merge(sampling_frame, dd, all.x = T, by = intersect(strat_vars, names(dd)))
      sampling_frame$fac <- sampling_frame$fac.x * na2(sampling_frame$fac.y, 1)
      sampling_frame$fac.y <- NULL
      sampling_frame$fac.x <- NULL
    }
  }
  # probability of a 'yes'
  logit <- function(p) log(p/(1-p))
  sampling_frame$logit <- 0
  if(!missing(prob)) {
    if(is.data.frame(prob)) prob <- list(prob)
    prob <- lapply(prob, function(d) d[,c(intersect(names(d),strat_vars),'prob')])
    for(dd in prob){
      dd$logit <- logit(dd$prob)
      dd$prob <- NULL
      sampling_frame <- merge(sampling_frame, dd, all.x = T, by = intersect(strat_vars, names(dd)))
      sampling_frame$logit <- sampling_frame$logit.x + na2(sampling_frame$logit.y, 0)
      sampling_frame$logit.y <- NULL
      sampling_frame$logit.x <- NULL
    }
  }
  sampling_frame$prob <- 1/(1+exp(-sampling_frame$logit))
  
  # select respondents
  
  rows <- sample(nrow(sampling_frame), N, replace = TRUE, 
                 prob = with(sampling_frame, population * fac))
  sample <- sampling_frame[rows,]
  
  # Generate response
  
  sample$y <- rbinom(nrow(sample), 1, prob = sample$prob)
  attr(sample,'fmla') <- fmla
  attr(sample,'strat_vars') <- strat_vars
  sample
}
#'
#' ### Examples
#'
#' A simple random sample from the population, default probability of 'yes' is 0.5 
sam(5) 
#' A simple random sample of women recording only age
sam(5, fmla = population ~ sex + age, subset = sex == "Female")                
#' A biased sample with the probability of a 'yes' depending on sex
fac <- list(
  data.frame(sex = c('Male','Female'), fac = c(3,2)), # with no omitted level, these are relative sampling factors
  data.frame(age = "20 to 24 years", fac = 5)   # omitted levels are set to 1
)
fac

prob <- data.frame(sex = 'Male', prob = .8) # the omitted level 'Female' defaults to .5
prob

sam(5, fmla = population ~ sex + age + state, fac = fac, prob = prob)  

#' A similar sample limited to California and recording raceethnicity as well

sam(5, fmla = population ~ sex + age + raceethnicity + state, subset = state == "CA",
    fac = fac, prob = prob)
      
#' Oversampling teen males

(fac <- tab_df(popagetable, ~ age + sex)) %>% info

fac$Freq <- NULL
fac$fac <- c(0,0,rep(1,12), 0,0, 10, 20, 20, 10, 5, rep(1,7))
fac
sam(5, fmla = population ~ sex + age, fac = fac, prob = prob)
#'
#' ### A larger sample: exploring some properties
#' 
(subset_expr <- quote(state == 'AK' & age > "18 and 19 years" & age < '65 to 74 years'))
(target_pop <- subset(popagetable, eval(subset_expr))) %>% info


fac <- popagetable %>% 
  subset(eval(subset_expr)) %>% 
  tab_df(~age + sex)
fac$Freq <- NULL
fac$fac <-  c(3,2,2,1,1,1,1,1,1,2,2,3)  # relative sampling factor
fac$prob <- c(4,2,1,1,1,.5,.5,1,1,1,3,4)/5  # probability of a YES
fac

sample <- sam(1000, fmla = population ~ age + sex + state + raceethnicity,
              subset = eval(subset_expr),
              fac = fac,
              prob = fac)
sample %>% info
#'
#' Exploring the sample
#' 
sample %>% tab(~age+sex)
sample %>% tab(~age+sex, pct = 0) %>% round(1)
sample %>% tab(~age+sex, pct = 1) %>% round(1)
sample %>% tab(~age+sex, pct = 2) %>% round(1)
#'
#' Over and undersampling
#'
#' Proportion in target population and sampling ratio
#' 
(target_prop <- target_pop %>% tab(population~age+sex, pct = 0)) %>% round(1)
(sampling_ratio <- tab(sample, ~ age + sex, pct = 0) / target_prop) %>% round(2)
xyplot(Freq~age , as.data.frame(sampling_ratio), groups = sex, type = 'b', 
       ylab = 'relative sampling ratio',
       auto.key = list(lines = T))
#'
#'
#' Percent of YES responses within each group
#'
(100 * tab(sample, y ~ age + sex)/tab(sample, ~age + sex))  %>% round(1)
(100 * tab_(sample, y ~ age + sex)/tab_(sample, ~age + sex))  %>% as.data.frame(responseName="Yes") -> sample_as
xyplot(Yes ~ age ,sample_as, groups = sex, type = 'b', auto.key = T, ylab ='Yes (%)')
#' make plots nicer, i.e. more like ggplot2
trellis.par.set(ggplot2like())
lattice.options(ggplot2like.opts())
xyplot(Yes ~ age ,sample_as, groups = sex, type = 'b', lwd = 2, auto.key = T, ylab ='Yes (%)')
#'
#' ## Estimating the probability of YES   
#'
#' 'True' value in the target population:
fac
fac_pop <- tab_df(target_pop, population ~ age + sex)
fac <- merge(fac, fac_pop)
fac %>% with(wtd_mean(prob,population)) 
#' Estimated value from the sample without weighing
sample %>% with(mean(y))
#' Estimated std. error:
sample %>% with(sd(y)/sqrt(length(y)))
#'
#'
#' ## Creating weights for the sample
#' 
#' Weights depend on the choice of variables to create strata. Within each stratum,
#' we need the population count and the sample count. 
#' 
#' This seems easiest to do with a simple function:
#' 
ns <- function(sample, target_pop, formula) {
  # The sample and the target_pop data frames 
  # must include the variables used in the formula of the
  # form, e.g. ~ sex + age.
  #
  # The variable 'population' is assumed to give 
  # population counts in each row of 'target_pop'
  # 
  # The function returns a data frame like 'sample'
  # with two added variables: n and N for the sample
  # and population counts respectively.
  ret <- sample
  by <- model.frame(formula, sample)
  # sample counts: WWCa::capply(x, by, FUN, ...) applies the function 'FUN'  
  ret$n <- capply(ret[[1]], by, length)
  # population counts
  pop <- tab_df(target_pop, formula, weight = target_pop$population)
  pop$N <- pop$Freq
  pop$Freq <- NULL
  ret <- merge(ret,pop, all = T)
  fac.names <- names(pop)[sapply(pop,is.factor)]
  for(nn in fac.names) {
    ret[[nn]] <- factor(ret[[nn]],levels=levels(pop[[nn]]))
    if(is.ordered(pop[[nn]])) ret[[nn]] <- ordered(ret[[nn]])
  }
  ret$n[is.na(ret$n)] <- 0
  ret
}
sample_as <- ns(sample, target_pop, ~ age + sex)
sample_as %>% info
#'
#' # Weights
#'
#' ## Horvitz-Thompson and related weights
#'
HTwts <- function(sample, cap = 3) {
  # sample with n and N
  ret <- sample
  ret$HT <- with(sample, N/n)
  ret$HTcap <- pmin(ret$HT, cap)
  ret$HT_mean <- ret$HT/mean(ret$HT)
  ret$HT_med <- ret$HT/median(ret$HT)
  ret$HT_mean_cap <- pmin(ret$HT_mean, cap)
  ret$HT_med_cap <- pmin(ret$HT_med, cap)
  ret$none <- 1
  ret
}
#'
(sample_asw <- HTwts(sample_as)) %>% info
sample_asw %>% with(wtd_mean(y,HT))
sample_asw %>% with(jk_wtd_mean_se(y,list(age,sex),HT))
sample_asw %>% with(wtd_mean(y,HT_mean_cap))
sample_asw %>% with(jk_wtd_mean_se(y,list(age,sex),HT_mean_cap))
#'
#' Using only sex for adjustment
#' 
sample_sw <- sample %>% ns(target_pop, ~sex) %>% HTwts
sample_sw %>% with(wtd_mean(y, HT))
sample_sw %>% with(jk_wtd_mean_se(y, list(sex), HT))
sample_sw %>% with(wtd_mean(y, HT_mean_cap))
sample_sw %>% with(jk_wtd_mean_se(y,list(sex),HT_mean_cap))
#'
#' Using only age for adjustment
#' 
sample_aw <- sample %>% ns(target_pop, ~age) %>% HTwts
sample_aw %>% with(wtd_mean(y,HT))
sample_aw %>% with(jk_wtd_mean_se(y,list(age),HT))
sample_aw %>% with(wtd_mean(y,HT_mean_cap))
sample_aw %>% with(jk_wtd_mean_se(y,list(age),HT_mean_cap))
#'
#' Using age, sex and raceethnicity for adjustment
#' 
sample_asrw <- sample %>% ns(target_pop, ~age+sex+raceethnicity) %>% HTwts
sample_asrw %>% with(wtd_mean(y,HT))
sample_asrw %>% with(jk_wtd_mean_se(y,list(age,sex,raceethnicity),HT))
sample_asrw %>% with(wtd_mean(y,HT_mean_cap))
sample_asrw %>% with(jk_wtd_mean_se(y, list(age, sex, raceethnicity), HT_mean_cap))
sample_asrw %>% with(wtd_mean(y, HT_med_cap, na.rm = T))
sample_asrw %>% with(jk_wtd_means(y, list(age, sex, raceethnicity), HT_med_cap, na.rm = T))
sample_asrw %>% with(jk_wtd_mean_se(y, list(age, sex, raceethnicity), HT_med_cap, na.rm = T))
sample_asrw %>% info
#'
#' # Simulating samples
#' 
#' We draw 100 samples of size 100, calculate the estimated proportion and its standard error
#' using different stratifications and see the empirical coverage of nominal 
#' approximate 95% confidence intervals.
#' 

#' Subsetting expression and sampling frame:
(subset_expr <- quote(state == 'AK' & age > "18 and 19 years" & age < '65 to 74 years'))
(sampling_frame <- subset(popagetable, eval(subset_expr))) %>% info

fac <- popagetable %>% 
  subset(eval(subset_expr)) %>% 
  tab_df(~age + sex)
fac$fac <-  c(3,2,2,1,1,1,1,1,1,2,2,3)  # relative sampling factor
fac$prob <- c(4,2,1,1,1,.5,.5,1,1,1,3,4)/5  # probability of a YES
fac$age %>% levels

system.time(
samples <- lapply(1:100, function(i) {
  sam(100, sampling_frame, fmla = population ~ age + sex + state + raceethnicity,
      subset = eval(subset_expr),
      fac = fac,
      prob = fac)
})
)
#'
#' Generate weights for different stratifications
#'
samples_w <- list(
  "~ age + sex" = lapply(samples, function(s) HTwts(ns(s, target_pop, ~ age + sex)) ),
  "~ age" = lapply(samples, function(s) HTwts(ns(s, target_pop, ~ age)) ),
  "~ sex" = lapply(samples, function(s) HTwts(ns(s, target_pop, ~ sex)) ),
  "~ age + sex + raceethnicity" = lapply(samples, function(s) HTwts(ns(s, target_pop, ~ age + sex + raceethnicity)) )
)
#'
#' Estimation functions using two types of weights
#'
est <- function(s, pop, fmla) {
  s <- ns(s, pop, fmla) %>% HTwts
  clist <- model.frame(fmla,s)
  with(s, list(
    est_raw = wtd_mean(y, none, na.rm = TRUE),
    se_raw = jk_wtd_mean_se(y, clist, none, na.rm = TRUE),
    est_HT = wtd_mean(y, HT, na.rm = TRUE), 
    se_HT = jk_wtd_mean_se(y, clist, HT, na.rm = TRUE),
    est_HTc = wtd_mean(y, HT_med_cap, na.rm = TRUE),
    se_HTc = jk_wtd_mean_se(y, clist, HT_med_cap, na.rm = TRUE)))
}
#'
#' ### Estimation
#'
#' The calculation of jacknife estimates takes more time than generating samples
#'
#+ cache=TRUE 
system.time(
estimates <- lapply(seq_along(samples_w), function(i)
  lapply(samples_w[[i]], est, sampling_frame, formula(names(samples_w)[[i]])))
)

estimates_df <- expand.grid(stat = c('est','se'), weight = c('raw','HT','HTc'), 
                            sample_num = 1:length(estimates[[1]]),
                            stratification = names(samples_w))
estimates_df$val <- unlist(estimates)

estimates_df %>% head(10)
estimates_df$id <- rep(1:(nrow(estimates_df)/2),each = 2)
estimates_df <- towide(estimates_df, id = 'id', time = 'stat')
head(estimates_df)
names(estimates_df) <- sub("val_", "", names(estimates_df))
#'
#' 
#'   

true.mean <- with(merge(sampling_frame,fac), wtd_mean(prob, population ))
estimates_df$t <- with(estimates_df, (est - true.mean)/se)
estimates_df$cover <- c('miss','cover')[with(estimates_df, abs(t) < 2)+1]
head(estimates_df)
#'
#' ### Raw (unweighted estimates)
#'
#'
z <- subset(estimates_df, weight == 'raw')

segplot(jitter(se) ~ I(est + 2*se) + I(est - 2*se) | stratification+cover,
        data = z,
        main = 'Unweighted estimates',
        ylab = '~95% confidence interval',
        xlab = 'estimated standard error',
        horizontal = FALSE,
        draw.bands = FALSE, centers = est, groups = cover,
        segments.fun = panel.arrows, ends = "both", auto.key = T,
        angle = 90, length = 1, unit = "mm")+ layer(panel.abline(h=true.mean))  
tab(z, ~ stratification + cover, pct = 1)
#'
#' ### Horvitz-Thompson weights
#'
#'
z <- subset(estimates_df, weight == 'HT')

segplot(jitter(se) ~ I(est + 2*se) + I(est - 2*se) | stratification*cover,
        data = z,
        main = 'Horvitz-Thompson weights',
        ylab = '~95% confidence interval',
        xlab = 'estimated standard error',
        horizontal = FALSE,
        draw.bands = FALSE, centers = est, groups = cover,
        segments.fun = panel.arrows, ends = "both", auto.key = T,
        angle = 90, length = 1, unit = "mm")+ layer(panel.abline(h=true.mean))
tab(z, ~ stratification + cover, pct = 1)
#'
#' ### Truncated Horvitz-Thompson weights
#'
z <- subset(estimates_df, weight == 'HTc')

segplot(jitter(se) ~ I(est + 2*se) + I(est - 2*se) | stratification*cover,
        data = z,
        ylab = '~95% confidence interval',
        xlab = 'estimated standard error',
        main = 'Truncated Horvitz-Thompson weights',
        horizontal = FALSE,
        draw.bands = FALSE, centers = est, groups = cover,
        segments.fun = panel.arrows, ends = "both", auto.key = T,
        angle = 90, length = 1, unit = "mm")+ layer(panel.abline(h=true.mean))
tab(z, ~ stratification + cover, pct = 1)