# Creating popagetable from acsagetable
# 
library(WWC)
library(magrittr)
head(acsagetable)
zz <- acsagetable
zz$region.type <- with(zz,
                       ifelse(region=="US", 'country', 
                              ifelse(grepl("^[0-9]",region),"county","state")) )
#  create a relation for state codes and state names
library(WWCa)
dcounty <- subset(zz, region.type == 'county')
dcounty$state.code <- sub("...$",'',dcounty$region)
head(dcounty)
state.codes <- tab_df(dcounty, population ~ state.code)
head(state.codes)
state.names <- subset(zz, region.type == 'state') %>% tab_df(population~region)
head(state.names)

# use population as index variable to create relation

state_rel <- merge(state.codes, state.names, all = T)
levels(state_rel$region) <- c(levels(state_rel$region), "PR" )
state_rel$region[state_rel$state.code == "72"] <- "PR"
state_rel$country <- factor(ifelse(state_rel$region == "PR", "PR", "US"))
state_rel %>% head
names(state_rel) <- sub('^region$','state',names(state_rel))
state_rel %>% head
state_rel$population <- NULL
state_rel %>% head
dcounty %>% head
dcounty %>% dim
dcounty <- merge(dcounty, state_rel)
dim(dcounty)
dcounty %>% head
dcounty$sextotal <- NULL
dcounty$geototal <- NULL
dcounty$prob <- NULL
dcounty$region.type <- NULL
dcounty %>% head
sapply(dcounty, class)
dcounty[] <- lapply(dcounty, function(x) if(is.character(x)) factor(x) else x)
levels(dcounty$age)
ageorder <- as.numeric(sub(" .*$","",levels(dcounty$age)))
ageorder[is.na(ageorder)] <- 0
ageorder

# make age an ordered factor so one can use expressions like: age > "15 to 17 years" 

dcounty$age <- with(dcounty, ordered(age, levels = levels(age)[order(ageorder)]))

# make 'OTHER' last:

dcounty$raceethnicity <- with(dcounty, factor(raceethnicity, levels = levels(raceethnicity)[c(1,2,3,5,4)]))

# write to data

popagetable <- dcounty[c('country','state','state.code')]
popagetable$state.code <- with(popagetable, factor(paste0(state,'-',state.code)))
popagetable$county <- dcounty$region
popagetable$sex <- dcounty$sex
popagetable$age <- dcounty$age
popagetable$raceethnicity <- dcounty$raceethnicity
popagetable$population <- dcounty$population

# negative and zero values for population

tab(popagetable, ~ state.code + sign(population))
tab(popagetable, ~ state.code + sign(population), pct = 1)  %>% round(1)

# substitute 0 for negative values for population
popagetable$population_neg <- popagetable$population
popagetable$population <- pmax(popagetable$population, 0)

# date at which frequencies are valid

popagetable$date <- NA

save(popagetable, file = 'data/popagetable.rda')
popagetable %>% head
