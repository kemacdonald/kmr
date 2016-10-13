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
#' @param .fun The function that you want to be bootsrtrapped
#' @param bound Which bound of the confidence interval: "upper" vs. "lower"
#' @param level Level of confidence (numeric)
#' @keywords bootstrap confidence interval
#' @export
#' @examples
#' # returns a numeric with either the lower and upper bound of the CI
#' boostrap_ci(x = df$accuracy, .fun = mean, bound = "upper", level = 0.975, na.rm=T)


bootstrap_ci <- function(x, .fun = NULL, bound, level, na.rm = T) {
    # assign function to be bootstrapped
    theta <- function(x,xdata,na.rm=T) {.fun(xdata[x],na.rm=na.rm)}

    if (bound == "lower") {
        ci_compute <- function(x,na.rm=T, .fun = NULL, bound) {
            .fun(x,na.rm=na.rm) - quantile(bootstrap::bootstrap(1:length(x), nboot = 1000, theta, x, na.rm=na.rm)$thetastar, level, na.rm=na.rm)
        }
    }

    if (bound == "upper") {
        ci_compute <- function(x,na.rm=T, .fun = NULL, bound) {
            quantile(bootstrap::bootstrap(1:length(x), nboot = 1000, theta, x, na.rm=na.rm)$thetastar, level, na.rm=na.rm) - .fun(x,na.rm=na.rm)
        }
    }

    ci <- ci_compute(x)
    return(as.numeric(ci))
}



#' Convert to numeric
#'
#' This function converts a value to a character than a numeric
#' @param x A non numeric value
#' @keywords to numeric
#' @export

to.n <- function(x) {
    as.numeric(as.character(x))
}



