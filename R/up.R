##
##  up + constant: to check whether something is constant
##
#' Generic test for constant
#'
#' @param x a vector
#' @param na.rm (default FALSE)
#' @param \dots not currently used
#' @export
constant <- function(x,...) UseMethod("constant")
#' @describeIn constant default method for constant
#' @export
constant.default <- function(x, na.rm = FALSE,...) {
  # looks like litte gain from spida2::constant
  if(length(x) == 1) return(TRUE)
  if(na.rm) {
    x <- na.omit(x)
    if(length(x) <= 1) return(TRUE)
    x1 <- x[1]
    for(xx in x[-1]) if(xx != x1) return(FALSE)
    return(TRUE)
  } else {
    wh <- is.na(x)
    if(all(wh)) return(TRUE)
    x1 <- x[!wh][1]
    for(xx in x) if( is.na(xx) | xx != x1) return(FALSE)
    return(TRUE)
  }
}
#' @param id clustering variable
#' @param all all variable must be constant (default FALSE)
#' @param \dots not currently used
#' @describeIn constant data.frame method for constant
#' @export
constant.data.frame <- function( x, id  , all = FALSE , ...) {
  ## Description:    (G. Monette, June 13, 2005)
  ## check if values of variables are constant within levels of id
  ## if no id, check each variable
  ## if all == TRUE, report overall instead of within levels of id
  ## Possible improvements:
  ##    allow nested formulas: ~id1/id2 for id and report level of
  ##    each variable
  ##    [see varLevel()]

  ## note that the following code allows id to be given as a name or as
  ## a formula

  if (missing(id)) ret <- sapply(x, constant,...)
  else {
    id <- eval(substitute(id), x, parent.frame())
    if( inherits(id,"formula") ) id <- c( model.frame(id,x) )
    ret <- sapply(x, function(xx) tapply(xx, id, constant, ...))
    if ( all ) ret <- apply( ret, 2, all)
  }
  ret
}

###
### varLevel
###
#' Identify level of aggregation at which a variable in invariant
#'
#' Shows levels of each variable with respect to grouping formula
#' of form ~id or nested ids ~id1/id2
#'
#' @return Level 0 is a constant for the whole data frame,
#' Level <= 1 implies the variable is constant within levels of id1,
#' Level <= 2 implies the variable is constant within levels of id2,
#' ... etc.
#'
#' NOTE: NA counts as a distinct value
#' @param x data frame
#' @param form formula to identify clusters
#' @param \dots extra arguments to \code{\link{constant}} function
#' @export
varLevel <- function(x, form, ...) {
  ## Description:    (G. Monette, June 13, 2005)
  sel <- model.frame( form, x )
  z <- list()
  idx <- rep('',nrow(x))
  z[[1]] <- constant(x,...)
  for ( ii in 1:ncol(sel)) {
    idx <- paste(idx, as.character(sel[[ii]]), sep = ";")
    z[[ii+1]] <- constant( x, idx, all = TRUE,...)
  }
  # print(z)
  ret <- do.call("rbind", z)
  # print(ret)
  ret <- length(z) - apply( ret*1, 2 , sum)
  ret
}

###
### up
###

#' Create a data frame at a higher level of aggregation
#'
#' Produce a higher level data set with one row per cluster. The data set can
#' contain only variables that are invariant in each cluster or it can also
#' include summaries (means and sums for numeric variables, and relative frequency
#' or frequency matrices for factors) of variables that vary by cluster. 
#' 
#' \code{up(data, by)} keeps rows corresponding to unique values of the clusters
#' defined by the variable or list of variables in 'by'. 
#' 
#' 'by' can also be a formula, evaluated in 'data'. For example,
#' \code{by = ~ a + b}, is equivalent to \code{by = data[,c('a','b')]}.
#' For example, with 'data' used above:
#'                    
#' \code{> data.state <- up(data, ~ state + sex)}
#'
#' creates a data frame with one row per state x sex combination.
#' 
#' \code{> data.state <- up(data, ~ state + sex, sum = ~ population)}
#'
#' will take an existing variable, \code{population}, in 'data' and 
#' sum it over rows within each cluster to form the variable 'population'
#' in 'data.state'.
#'
#' \code{up} was created from \code{nlme::gsummary} and modified to make it
#' easier to use and to make an equivalent of \code{gsummary} available when
#' using \code{lme4}. The \code{agg} and \code{sum} arguments were added later
#' and provide easy aggreatation of survey data. 
#'
#' @param object a data frame to be aggregated.
#' @param form a one-sided formula identifying the variable(s) in \code{object}
#' that identifies clusters. e.g. ~ school/Sex to get a summary within each Sex
#' of each school.
#' @param agg (NEW: Aug 2016) a one-sided formula identifying variables to be aggregated,
#'        i.e. variables that vary within cluster and that need to be aggregated 
#'        (within-cluster mean for numeric variables and within-cluster incidence
#'        proportions for factors). Default: NULL
#' @param sum a one-sided formula identifying variables to be summed within
#'        clusters. Default: NULL 
#' @param sep.agg (NEW: Aug 2016) separator between factor names and factor
#'        level for within-cluster incidence proportions. Default: '_' 
#' @param sep.sum, like 'sep.agg' but for 'sum' variables.       
#' @param all if TRUE, include summaries of variables that vary within
#'        clusters, otherwise keep only cluster-invariant variables and variables
#'        listed in 'agg'
#' @param sep separator to form cluster names combining more than one
#' clustering variables.  If the separator leads to the same name for distinct
#' clusters (e.g. if var1 has levels 'a' and 'a/b' and var2 has levels 'b/c'
#' and 'c') the function produces an error and a different separator should be
#' used.
#' @param FUN function to be used for summaries.
#' @param omitGroupingFactor kept for compatibility with \code{gsummary}
#' @param groups kept for compatibility with \code{gsummary}
#' @param invariantsOnly kept for compatibility with \code{gsummary}
#' @param \dots additional arguments to \code{tapply} when summarizing
#' numerical variables. e.g. \code{na.rm = TRUE}
#' @return a data frame with one row per value of the variable in \code{form}
#' 
#' @examples
#'     data(hs)
#'     dim( hs )
#'     hsu <- up( hs, ~ school )
#'     dim( hsu )
#'
#'     # to also get cluster means of cluster-varying numeric variables and modes of factors:
#'
#'     hsa <- up( hs, ~ school , all = TRUE )
#'
#'     # to get summary proportions of cluster varying factors:
#'
#'     up( cbind( hs, model.matrix( ~ Sex -1 , hs)), ~ school, all = T)
#'
#'
#'     ## To plot a summary between-cluster panel along with within-cluster panels:
#'
#'     hsu <- up( hs, ~ school, all = TRUE)
#'     hsu$school <- ' between'  # space to make it come lexicographically before cluster names
#'
#'     require( lattice )
#'     xyplot( mathach ~ ses | school, rbind(hs,hsu),
#'         panel = function( x, y, ...) {
#'             panel.xyplot( x, y, ...)
#'             panel.lmline( x, y, ...)
#'         } )
#'
#' @author Georges Monette, 'all' is largely from gsummary in Bates & Pinheiro
#' @export
up <- function ( object, form = formula(object),
           agg = NULL, 
           sum = NULL,
           sep.agg = "_",
           sep.sum = sep.agg,
           all = FALSE, sep = "/",
           na.rm = TRUE,
           FUN = function(x) mean(x, na.rm = na.rm),
           omitGroupingFactor = FALSE,
           groups, invariantsOnly = !all , ...)
{
  if (!inherits(object, "data.frame")) {
    stop("Object must inherit from data.frame")
  }
  sel.mf <- model.frame( form , object , na.action = na.include )
  narows <- apply(sel.mf,1,function(x) any(is.na(x)))
  if(any(narows)) {
    warning("Rows with NAs in grouping variable(s) are omitted")
    sel.mf <- droplevels(sel.mf[!narows,,drop=FALSE])
    object <- object[!narows,,drop=FALSE]
  }
  if ( ncol(sel.mf) > 1) {
    sel <- apply( sel.mf, 1 , paste, collapse = sep)
    groups <- as.factor(sel)
    # Check if sep works to create unique group combinations
    sel2 <- apply(sel.mf,1, paste, collapse = as.character(sample(1000:9999,1)))
    if ( length( unique(sel)) != length( unique(sel2))) {
      stop( 'distinct grouping combinations have the same name: change the "sep" argument')
    }
  } else {
    groups <- as.factor(sel.mf[[1]])
  }
  
  if(!is.null(agg)) {
    agg.mf <- model.frame(agg, object, na.action = na.include)
    
    #ret <- object
    for (i in seq_along(agg.mf)) {
      x <- agg.mf[[i]]
      if(is.factor(x)) {
        mat <- cvar(x, sel.mf, all = T, na.rm = na.rm)
        colnames(mat) <- paste0(names(agg.mf[i]),sep.agg,colnames(mat))
      }
      else {
        mat <- cvar(x, sel.mf, na.rm = na.rm, ...)
        mat <- data.frame(x=mat)
        names(mat) <- names(agg.mf[i])
      }
      object <- cbind(object, mat)
    }
  }
  if(!is.null(sum)) {
    sum.mf <- model.frame(sum, object, na.action = na.include)
    
    #ret <- object
    for (i in seq_along(sum.mf)) {
      x <- sum.mf[[i]]
      if(is.factor(x)) {
        mat <- cvar(x, sel.mf, all = T, na.rm = na.rm)
        n <- capply(x, sel.mf, length)
        mat <- n*mat
        colnames(mat) <- paste0(names(sum.mf[i]),sep.sum,colnames(mat))
        
      }
      else {
        mat <- cvar(x, sel.mf, na.rm = na.rm, ...)
        n <- capply(x, sel.mf, length)
        mat <- data.frame(x = n * mat)
        names(mat) <- names(sum.mf[i])
      }
      object <- cbind(object, mat)
    }
  }
  gunique <- unique(groups)
  firstInGroup <- match(gunique, groups)
  asFirst <- firstInGroup[match(groups, gunique)]
  value <- as.data.frame(object[firstInGroup, , drop = FALSE])
  row.names(value) <- as.character(gunique)
  value <- value[as.character(sort(gunique)), , drop = FALSE]
  varying <- unlist(lapply(object, function(column, frst) {
    aux <- column
    if( is.matrix( aux))aux[] <- as.character( aux )
    else aux <- as.character(aux)
    if ( is.matrix( aux )) any(!identical( aux, aux[frst,]))
    else any(!identical(aux, aux[frst]))
  }, frst = asFirst))
  if (any(varying) && (!invariantsOnly)) {
    Mode <- function(x) {
      aux <- table(x)
      names(aux)[match(max(aux), aux)]
    }
    if (data.class(FUN) == "function") {
      FUN <- list(numeric = FUN, ordered = Mode, factor = Mode, logical = FUN)
    }
    else {
      if (!(is.list(FUN) && all(sapply(FUN, data.class) ==
                                "function"))) {
        stop("FUN can only be a function or a list of functions")
      }
      auxFUN <- list(numeric = mean, ordered = Mode, factor = Mode, logical = mean)
      aux <- names(auxFUN)[is.na(match(names(auxFUN), names(FUN)))]
      if (length(aux) > 0)
        FUN[aux] <- auxFUN[aux]
    }
    for (nm in names(object)[varying]) {
      dClass <- if (is.ordered(object[[nm]]))
        "ordered"
      else if (is.factor(object[[nm]]))
        "factor"
      else mode(object[[nm]])
      if (dClass == "numeric") {
        if( is.matrix ( object[[nm]])){
          zmat <- object[[nm]]
          ret <- list()
          for ( jj in seq_len(ncol(zmat))) {
            ret[[jj]] <- as.vector( tapply( zmat[,jj],
                                            groups, FUN[['numeric']],...))
          }
          value[[nm]] <- do.call(cbind, ret)
          
        } else {
          value[[nm]] <- as.vector(tapply(object[[nm]],
                                          groups, FUN[["numeric"]], ...))
        }
      }
      else {
        value[[nm]] <- as.vector(tapply(object[[nm]],
                                        groups, FUN[[dClass]]))
      }
    }
  }
  else {
    value <- value[, !varying, drop = FALSE]
  }
  if (omitGroupingFactor) {
    if (is.null(form)) {
      stop("Cannot omit grouping factor without \"form\"")
    }
    grpForm <- getGroupsFormula(form, asList = TRUE)
    if (missing(level))
      level <- length(grpForm)
    grpNames <- names(grpForm)[level]
    whichKeep <- is.na(match(names(value), grpNames))
    if (any(whichKeep)) {
      value <- value[, whichKeep, drop = FALSE]
    }
    else {
      return(NULL)
    }
  }
  value
}


#' na.include action
#'
#' From the Hmisc package, author: Frank Harrell
#'
#' @param obj a data.frame whose factors will be redefined so exclude = NULL
#' @export
na.include <-
function (obj)
{
  if (inherits(obj, "data.frame"))
    for (i in seq(along = obj)) obj[[i]] <- na.include(obj[[i]])
    else {
      if (length(levels(obj)) && any(is.na(obj)))
        obj <- factor(obj, exclude = NULL)
    }
    obj
}
#' Apply a function to clusters of rows in a data frame
#' 
#' Apply a function to clusters of rows in a data frame and return
#' the result so it is conformable with the data frame created by
#' 'up' applied to the same data frame and same clustering formula
#' 
#' @param object a data frame as source for an aggregated result
#' @param form a one-sided formula identifying the variable(s) in \code{object}
#' that identifies clusters. e.g. ~ school/Sex to get a summary within each Sex
#' of each school
#' @param FUN a function to be applied to each data frame consisting of a 
#'        cluster of rows of 'object'. The most common choice is \code{\link{with}} so that
#'        '...' can be an expression using variable names in 'object'. 
#' @param ... other arguments to FUN, frequently when FUN is 'with', an expression 
#'        using variable names in 'object'        
#' @examples
#' zd <- data.frame(a=c('a','a','b','b','c','c','c'),
#'       b = c("B","B","A","B","C","D","D"), x = 1:7, y = 11:17)
#' zd$n <- capply(zd$x, zd[c('a','b')], length)
#' zdu <- up(zd, ~a, agg = ~b)
#' zdu
#' zdu$p <- up_apply(zd, ~ a, with, sum(x)/sum(y))
#' zdu
#' @export
up_apply <-
  function(object, form, FUN , ...,  sep = '/') {
    sel.mf <- model.frame( form , object , na.action = na.include )
    narows <- apply(sel.mf,1,function(x) any(is.na(x)))
    if(any(narows)) {
      warning("Rows with NAs in grouping variable(s) are omitted")
      sel.mf <- droplevels(sel.mf[!narows,,drop=FALSE])
      object <- object[!narows,,drop=FALSE]
    }
    if ( ncol(sel.mf) > 1) {
      sel <- apply( sel.mf, 1 , paste, collapse = sep)
      groups <- as.factor(sel)
      # Check if sep works to create unique group combinations
      sel2 <- apply(sel.mf,1, paste, collapse = as.character(sample(1000:9999,1)))
      if ( length( unique(sel)) != length( unique(sel2))) {
        stop( 'distinct grouping combinations have the same name: change the "sep" argument')
      }
    } else {
      groups <- as.factor(sel.mf[[1]])
    }
    
    FUN <- match.fun(FUN)
    # if (inherits(by,'formula')) by <- model.frame( by , x , na.action = na.include)
    # if (is.character(by)) by <- factor(by)
    # if (is.factor(by)) by <- as.numeric(by)
    ret <- sapply ( split ( object , groups ), FUN, ...)
    if(is.null(dim(ret))) ret else t(ret)
  }

