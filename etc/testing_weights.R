

# library(devtools)
# install_github('heathermkrause/WWC')

library(WWC)

library(dplyr)
summary(acsagetable)

# playing with acsagetable

library(spida2)   # gmonette/spida2 -- 
# Main utilities here:
#
# capply(x, by, FUN, ...): applies FUN to each 'chunk' of x defined by
#                    levels of 'by'. "..." are additional arguments to FUN.
#                    'by' can be a variable or a list, often a set of columns 
#                    in the data set in which 'x' lives. The result has the 
#                    same shape as 'x' and can thus be added as a variable to
#                    the data frame for 'x'. If FUN returns a single value in
#                    each chunk, the value is recycled to fill the elements
#                    corresponding to the chunk.
#
#                    For example, in a data frame, data, with variables: 
#                        state, county, sex, population
#                    where population is the population size within each
#                    county (within state) by sex combination:
#                    > data$pop.state <- with(data,
#                          capply(population, list(sex,state), sum))
#                    creates a variable that is equal to the total population
#                    within each state x sex combination repeated, of course, for
#                    each county.
#
# up(data, by)       keeps rows corresponding to unique values of the strata
#                    defined by the variable or list of variables in 'by'.
#                    'by' can also be a formula, evaluated in 'data'. For example,
#                    'by = ~ a + b', is equivalent to 'by = data[,c('a','b')]'.
#                    For example, with 'data' used above:
#                    
#                    > data.state <- up(data, ~ state + sex)
#
#                    creates a data frame with one row per state x sex combination
#                    and the variable 'pop.state' contains the total population in
#                    in each combination. Variables, such as 'county' and 'population'
#                    that are not invariant within levels of 'by' are dropped.
#
# tab(data, fmla)    creates a frequency table showing the number of rows in 'data' for
#                    each stratum formed by evaluating the formula 'fmla' in 'data'.
#                    For example 'tab(data, ~ a + b + c)' will return an array of frequencies
#                    for variables 'a', 'b', and 'c' in 'data'. 
#                    If the formula has a variable on the left side, that variable is
#                    summed to get the entries of the table. 
#
#                    For example, if 'population' contains the population in each row
#                    where each row is a 'sex x state x county' combination:
#
#                    > tab(data, population ~ sex + age)  
#
#                    will produce a table of total population by sex and age, and
#
#                    > tab(data, population ~ age)
#
#                    will show overall totals by age group.  
#
#                    These tables can be tranformed into data frames with, e.g.,
#
#                    > as.data.frame(tab(data, population ~ age))
#
#
#
#
library(surveygmds)
library(magrittr)
library(latticeExtra)

# Method for a quick look at a data frame
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
#' # Computing population counts
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
#' Note that approximately 
#' 
#' The following example show how the 'subset' function (in the base package) combined with
#' 'tab' and 'tab_df' which are modified version of 'table' (in base), can be used to
#' create a population count data frame.
#' 
#' 
popagetable %>% info

popagetable %>% tab(~age + sex)    # number of rows in each stratum
popagetable %>% tab(population ~ age + sex)    # sum of population
popagetable %>% tab_df(population ~age + sex)    # as a data frame

#' 
#' ### Negative and zero population values
#' 
#' Overall, about 1% of rows (strata x county combinations) have 
#' negative population values and 30% have zero values. 
#' 
popagetable %>% tab(~state + sign(population_neg))
popagetable %>% tab(~state + sign(population_neg), pct = 1) %>% round(1) 

#'
#' Aggregating at the state level, all the negative values occur for
#' 'raceethnicity' equal to 'OTHER' in Puerto Rico.
#' 
popagetable %>% tab_df(population_neg ~ sex + age + raceethnicity + state) -> aggdf
subset(aggdf, population_neg < 10)

#'
#' Focusing on Alaska
#' 
popagetable %>% subset(state == "AK")  %>% tab(population ~ age + sex)
popagetable %>% subset(state == "AK")  %>% tab(population ~ age + sex, pct = 1) %>% round(2)
popagetable %>% subset(state == "AK")  %>% tab(population ~ age + sex, pct = 2) %>% round(2)
popagetable %>% subset(state == "AK")  %>% tab(population ~ age + sex, pct = 0) %>% round(2)
popagetable %>% subset(state == "AK")  %>% tab_df(population ~ age + sex)    # as a data frame

#' Female adults in NY and PA

wtdf <- popagetable %>% 
  subset(state %in% c('NY','PA') & 
                                 sex == "Female" & 
                                 age > "15 to 17 years") %>% 
  tab_df(population ~ age + state)

 # '
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


