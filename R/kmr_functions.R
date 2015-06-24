# install.packages("bootstrap")
library(bootstrap)

#' Standard error of the mean
#'
#' This function computes the standard error of the mean removing NAs.
#' @param x Vector of numeric values
#' @keywords sem
#' @export
#' @examples
#' sem()

sem <- function (x) {
    sd(x,na.rm=TRUE) / sqrt(length(x))
}

#' Anonymize Participant IDs
#'
#' This function gives participants IDs from 1:length(participants)
#' @param df Data frame with a column of participant ids that you want to anonymize.
#' @param subject_column_label The column name of your participant id variable.
#' @keywords anonymize
#' @export
#' @examples
#' anonymize_sids()

anonymize_sids <- function(df, subject_column_label) {
    subj_col = which(names(df) == subject_column_label) # get workerid column index
    temp <- data.frame(workerid = unique(df[,subj_col])) # make new df of unique workerids
    temp$subid <- 1:length(unique(df[,subj_col])) # make list of subids
    index <- match(df[,subj_col], temp$workerid)
    df$subids <- temp$subid[index]
    df[,subj_col] <- NULL
    df$subids  = as.factor(df$subids)
    return(df)
}


#' Boostrap Confidence Intervals
#'
#' This function computes 95 percent confidence intervals via non-parametric bootstrap.
#' @param x A vector of numeric values for which you need a boostrapped confidence interval
#' @keywords bootstrap confidence interval
#' @export
#' @examples
#' # returns a named numeric with length 2 that has lower and upper bound of CI
#' compute_ci(df$accuracy)


compute_ci <- function(x) {
    # assign function to be bootstrapped
    theta <- function(x,xdata,na.rm=T) {mean(xdata[x],na.rm=na.rm)}

    # get lower bound of confidence interval
    ci_low <- function(x,na.rm=T) {
        mean(x,na.rm=na.rm) - quantile(bootstrap(1:length(x), nboot = 1000, theta, x, na.rm=na.rm)$thetastar, .025, na.rm=na.rm)
        }

    # get upper bound of confidence interval
    ci_high <- function(x,na.rm=T) {
        quantile(bootstrap(1:length(x), nboot = 1000, theta, x, na.rm=na.rm)$thetastar, .975, na.rm=na.rm) - mean(x,na.rm=na.rm)
    }

    # return upper and lower bounds of CI
    ci <- c(ci_low(x), ci_high(x))

    return(ci)
}




