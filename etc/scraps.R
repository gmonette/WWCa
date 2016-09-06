#'
#' Weighing samples  
#'
#' 
#' # Population file
#' 
#' Depending on the variable used for adjusting a sample and on the target demographic
#' of inference, we need population counts restricted to the target
#' demographic groups (possible combination of region, age groups, etc.) and summed
#' within subgroups identified by the combinations of the adjusting variables.
#' 
#' This can be done by for any target group and any combination of adjusting variables
#' by selecting and aggregating from the most disaggregated data set available.
#' 
#' The data set 'popagetable' has frequencies within each county in the US and Puerto Rico
#' disaggregated by age group, race-ethnicity and sex.
#' 

summary(popagetable)
head(popagetable)    
library(surveygmds)
library(spida2)

#' Suppose a survey has been taken in "AK"  

popagetable %>% tab(population ~ sex+ country)
subset(popagetable, state == "AK") %>%  tab(population ~ age + sex)
subset(popagetable, state == "AK") %>%  tab(population ~ age + sex, pct = 2) %>% round(2)
subset(popagetable, state == "AK") %>%  tab(population ~ age + sex, pct = 1) %>% round(2)
subset(popagetable, state == "AK") %>%  tab_df(population ~ age + sex)
# to get a data frame with population counts by age and sex: (Note: 'tab_' excludes "Totals")

popagesexAR <- subset(popagetable, state = "AK") %>%  tab_(population ~ age + sex) %>% as.data.frame
popagesexAR 
  
# alternatively, we can do this 'up'

system.time(
subset(popagetable, state = "AK") %>%  up(~ sex + country, sum = ~ population)
)

# but 'tab_' is much faster
system.time(
subset(popagetable, state = "AK") %>%  tab_(population ~ sex+ country) %>% as.data.frame
)

system.time(
  popagetable %>%  tab_(population ~ sex+ country) %>% as.data.frame
)
system.time(
  popagetable %>%  up( ~ sex+ country, sum = ~ population) 
)


#' 



