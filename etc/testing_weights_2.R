#' ---
#' title: "Weighting for online surveys"
#' date: "`r Sys.Date()`"
#' output: html_document
#' bibliography: WWC.bib
#' link-citations: yes
#' ---
#+ setup, include=FALSE eval=FALSE
devtools::install_github('gmonette/WWCa')
install.packages('magrittr')  # to use the '%>%' pipe imported from magrittr to dplyr
#' 
#' # Introduction
#' 
#' This document illustrates the use of a few tools that might make it easy to analyze and weight
#' online surveys.
#' 
#' The WWCa package includes two data frames with frequencies 
#' XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX 
#' from the data frames acsagetable and acsedutable in the WWC package.
#' 
#' The main tools are 'tab' and 'tab_df' to create tables and data frames with frequencies by strata
#' for survey data and population frequency tables. 
#' 
#' The 'wts' function takes
#'  
#' 1. two data frames, one with survey results and one with population frequencies, 
#' 2. a stratification formula to identify stratification variables, and 
#' 3. a list of CONDITIONING EXPRESSION to compute various types of weights
#' 
#' to form population mean estimates and standard errors adjusted for the stratification 
#' variables to estimate
#' population parameter in the target population defined by the CONDITIONING EXPRESSIONS.
#' 
#' # The 'tab' and 'tab_df' functions
#'        
library(surveygmds)
library(magrittr)
library(lattice)
library(latticeExtra)

#' A flexible method to look at data frames
info <- function(x, ...) UseMethod('info')
info.data.frame <- function(x, n = 6, ...) {  
  cat("Dim: ",dim(x),"\n")
  if(nrow(x) <= n) print(x)
  else print(x[sample(nrow(x), n),])
  invisible(x)
}
info.default <- function(x,...) {
  cat("Not implemented for ", class(x),"\n")
  invisible(x)
}

#'
#' # Computing population counts with 'tab' and 'tab_df'
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

popagetable %>% tab(~age + sex)    # number of rows in each stratum
popagetable %>% tab(population ~ age + sex)    # sum of population
popagetable %>% tab(population ~ age + sex, pct = 1)   %>% round(1)  # row percentages
popagetable %>% tab(population ~ age + sex, pct = 2)   %>% round(1)  # column percentages

popagetable %>% tab_df(population ~age + sex)    # as a data frame
popagetable %>% tab_df(population ~age + sex, pct = 1)    # as a data frame

#' 
#' ### Negative and zero population values
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
#' Focusing on Alaska
#' 
popagetable %>% subset(state == "AK")  %>% tab(population ~ age + sex)
popagetable %>% subset(state == "AK")  %>% tab(population ~ age + sex, pct = 1) %>% round(2)
popagetable %>% subset(state == "AK")  %>% tab(population ~ age + sex, pct = 2) %>% round(2)
popagetable %>% subset(state == "AK")  %>% tab(population ~ age + sex, pct = 0) %>% round(2)
popagetable %>% subset(state == "AK")  %>% tab_df(population ~ age + sex)    # as a data frame

#' Female adults in NY and PA below 85 years of age

targetdf <- popagetable %>% 
  subset(state %in% c('NY','PA') & 
           sex == "Female" &
           age > "15 to 17 years" &
           age < "85 years and over") %>% 
  tab_df(population ~ age + sex + state)
targetdf

#'
#' # Create a sample
#'
#' We will create a sample of adults in AK and illustrate the use of functions in the package to
#' obtain various kinds of weighted estimates and estimated standard errors.
#' 
#' We will suppose that the response depends on age and sex but not on raceethnicity and
#' we will consider the consequences of adjusting for various combinations of the 
#' three variables.
#' 
#' Step 1: Select target population from 'popagetable' 
#' 
target_pop <- subset(popagetable, state == 'AK' & age > "18 and 19 years" & age < '65 to 74 years')
target_pop %>% dim
target_pop %>% head
target_pop %>% tab(population ~ age + sex)
target_pop %>% tab(population ~ age + sex, pct = 2)  %>% round(1)
target_pop <- droplevels(target_pop) # remove empty levels from factors
#'
#' Step 2: Sample parameters: 
#' 
#' 1. choose 'sampling factors': relative degree of over or undersampling in different strata and 
#' 2. parameters for the response distribution in different strata
#'
#' We pretend that the sample is disproportionaly young among women and old among men
#' by creating a relative sampling factor. Since the sampling factors and the response
#' distribution don't depend on ethnicity, we don't need to take that into account at this
#' stage. 
#'  
sample_parms <- tab_df(target_pop, ~ age + sex) # don't need raceethnicity
sample_parms
sample_parms$Freq <- NULL
sample_parms$fac <- c(3,2,2,1,1,1,1,1,1,2,2,3)  # relative sampling factor
sample_parms$pr <- c(4,2,1,1,1,.5,.5,1,1,1,3,4)/5  # probability of a YES
sample_parms
#'
#' We merge (join) with target_pop
#' 
sample_parms <- merge(target_pop, sample_parms)
sample_parms %>% info
#'
#' take a sample of N = 1000 with probabilities proportional to 
#' population proportion times 'fac'
#' 
N <- 1000
prob <- with(sample_parms, population * fac)
sample_rows <- sample(nrow(sample_parms), N, replace = T, prob = prob)
sample <- sample_parms[sample_rows,]
#'
#' Step X: Generate response
#'
sample$y <- rbinom(nrow(sample), 1, prob = sample$pr)

#' Get rid of information we would not normally have in a sample
sample <- subset(sample, select = c(sex, age, raceethnicity, y)) # keeps all the rows, selects columns
sample %>% info
#'
#' Explore some properties of sample
#'
sample %>% tab(~age+sex)
#' Percent of YES responses within each group
(100 * tab(sample, y ~ age + sex)/tab(sample, ~age + sex))  %>% round(1)
(100 * tab_(sample, y ~ age + sex)/tab_(sample, ~age + sex))  %>% as.data.frame -> sample_as
sample_as$pct_YES <- sample_as$Freq
xyplot(pct_YES ~ age ,sample_as, groups = sex, type = 'l', auto.key = T)
#' make plots nicer, i.e. more like ggplot2
trellis.par.set(ggplot2like())
lattice.options(ggplot2like.opts())
xyplot(pct_YES ~ age ,sample_as, groups = sex, type = 'l', lwd = 2, auto.key = T)
#'
#' ### Estimating the probability of YES   
#'
sample_parms %>% head
#' 'True' value in the population:
sample_parms %>% with(wtd_mean(pr,population)) 
#' Estimated value from the sample without weighing
sample %>% with(mean(y))
#' Estimated std. error:
sample %>% with(sd(y)/sqrt(length(y)))
#'
#' ## Creating weights for the sample
#' 
#' Weights depend on the choice of variables to create strata. Withi each stratum,
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
  # sample counts:
  ret$n <- capply(ret[[1]], by, length)
  # population counts
  pop <- tab_df(target_pop, formula, weight = target_pop$population)
  pop$N <- pop$Freq
  pop$Freq <- NULL
  ret <- merge(ret,pop)
  ret
}
sample_as <- ns(sample, target_pop, ~ age + sex)
sample_as %>% info
#'
#' ## Weights
#'
#' Horvitz-Thompson and related weights
#'
wts <- function(sample, maxweight = 3) {
  # sample with n and N
  ret <- sample
  ret$HT <- with(sample, N/n)
  ret$HT_mean <- ret$HT/mean(ret$HT)
  ret$HT_med <- ret$HT/median(ret$HT)
  ret$HT_mean_t <- pmin(ret$HT_mean, maxweight)
  ret$HT_med_t <- pmin(ret$HT_med, maxweight)
  ret$none <- 1
  ret
}
#'
sample_asw <- wts(sample_as)
sample_asw %>% info
sample_asw %>% with(wtd_mean(y,HT))
sample_asw %>% with(jk_wtd_mean_se(y,list(age,sex),HT))
sample_asw %>% with(wtd_mean(y,HT_mean_t))
sample_asw %>% with(jk_wtd_mean_se(y,list(age,sex),HT_mean_t))
#'
#' Using only sex for adjustment
#' 
sample_sw <- sample %>% ns(target_pop, ~sex) %>% wts
sample_sw %>% with(wtd_mean(y,HT))
sample_sw %>% with(jk_wtd_mean_se(y,list(sex),HT))
sample_sw %>% with(wtd_mean(y,HT_mean_t))
sample_sw %>% with(jk_wtd_mean_se(y,list(sex),HT_mean_t))
#'
#' Using only age for adjustment
#' 
sample_aw <- sample %>% ns(target_pop, ~age) %>% wts
sample_aw %>% with(wtd_mean(y,HT))
sample_aw %>% with(jk_wtd_mean_se(y,list(age),HT))
sample_aw %>% with(wtd_mean(y,HT_mean_t))
sample_aw %>% with(jk_wtd_mean_se(y,list(age),HT_mean_t))
#'
#' Using only age, sex and raceethnicity for adjustment
#' 
sample_asrw <- sample %>% ns(target_pop, ~age+sex+raceethnicity) %>% wts
sample_asrw %>% with(wtd_mean(y,HT))
sample_asrw %>% with(jk_wtd_mean_se(y,list(age,sex,raceethnicity),HT))
sample_asrw %>% with(wtd_mean(y,HT_mean_t))
sample_asrw %>% with(jk_wtd_mean_se(y,list(age,sex,raceethnicity),HT_mean_t))
#'
sample_asrw %>% info
sample_asrw %>% xyplot(HT_mean~HT_mean_t,.)

#'
#'  # Simulating sample
#'
target_pop <- subset(popagetable, state == 'AK' & age > "18 and 19 years" & age < '65 to 74 years')
target_pop <- droplevels(target_pop)
target_pop %>% info


formula <- ~ age + sex
sample_parms <- tab_df(target_pop, formula) # don't need raceethnicity
sample_parms
sample_parms$Freq <- NULL
sample_parms$fac <- c(3,2,2,1,1,1,1,1,1,2,2,3)  # relative sampling factor
sample_parms$pr <- c(4,2,1,1,1,.5,.5,1,1,1,3,4)/5  # probability of a YES
sample_parms
pop_sample_parms <- merge(sample_parms, target_pop)
pop_sample_parms  %>% info

N <- 1000

sim <- function(pop_sample_parms, N, formula, fmlist = list(formula)){
  prob <- with(pop_sample_parms, population * fac)
  sample_rows <- sample(nrow(pop_sample_parms), N, replace = T, prob = prob)
  sample <- pop_sample_parms[sample_rows,]
  # Generate response
  sample$y <- rbinom(nrow(sample), 1, prob = sample$pr)
  wts.types <- c('none','HT','HT_mean_t','HT_med_t')
  fnames <- sapply(fmlist, function(x) as.character(x)[2])
  ret <- array(dim = c(length(fnames), length(wts.types), 2))
  dimnames(ret) <- list(adj = fnames, wt = wts.types, stat = c('mean','se'))
  for(i in seq_along(fmlist)){
    tmp <- sample %>% ns(pop_sample_parms, fmlist[[i]]) %>% wts
    for(j in seq_along(wts.types)){
      ret[i,j,1] <- wtd_mean(tmp$y, tmp[[wts.types[j]]])
      ret[i,j,2] <- jk_wtd_mean_se(tmp$y, model.frame(fmlist[[i]],tmp), tmp[[wts.types[j]]])
    }
  }
  ret
}

system.time(
zz <-replicate(100, sim(pop_sample_parms, N, formula))
)
z <- zz  %>% as.table %>% as.data.frame
info(z)
head(z,20)
z$est <- z$Freq
z$Freq <- NULL
zw <- with(z,towide(z, c('adj','wt','Var4'), timevar = 'stat' ))
head(zw)
true.mean <- with(pop_sample_parms, wtd_mean(pr,population))
xyplot(est_mean ~ est_se|wt, zw )+ layer(panel.abline(h=true.mean))
dim(zz)

segplot(est_se ~ I(est_mean + 2*est_se) + I(est_mean - 2*est_se) | wt,
                data = zw,
        horizontal = FALSE,
                 draw.bands = FALSE, centers = est_mean, 
                 segments.fun = panel.arrows, ends = "both", 
                 angle = 90, length = 1, unit = "mm")+ layer(panel.abline(h=true.mean))


