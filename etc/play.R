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
opts_knit$set(width=120)
options(width=120)
opts_chunk$set(tidy=FALSE,comment='| ',fig.height=8,fig.width=10)

library(xtable)
options(xtable.type='html')
options(xtable.html.table.attributes = 'border=1 cellpadding=4 cellspacing=4')
options(xtable.format.args = list(big.mark=","))
options(xtable.caption.placement = "top")

# options(xtable.type='latex')
# options(xtable.html.table.attributes = 'border=1 cellpadding=4')
options(xtable.format.args = list(big.mark=","))
options(xtable.caption.placement = "top")
red <- "#8A084B"
blue <- "#013ADF"
green <- "#088A08"
dex <- head(as.data.frame(Seatbelts),10)
#'
dex
# to make tables look nice in html
#+ css-style,results='asis'
# NOTE: Important that <style ... be flush left
cat("
<style type='text/css'>
table {
    max-width: 95%;
    border: 3px solid #bbb;
}
th {
    background-color: #dddddd;
    color: #000000;
    padding: 2px 5px;
    font-family: 'Times New Roman';
    text-transform: capitalize;
    font-style: italic;
    font-size: 14px;
}
td {
    background-color: #eeeeee;
    padding: 1px 5px;
}
</style>
")
#+ results='asis'
xtable(dex)


#' 
#' # Introduction
#' 
cars
plot(cars)
#+ fig.cap="a caption"
plot(cars)
#+ tab.cap="a table caption?"
plot(cars)
#+ results='asis', message=FALSE
print(xtable(head(iris)))
#'
library(spida2)
#+ results='asis',
print(xtable(tab(hs,~Sex + Sector), caption = "this is the caption"))

