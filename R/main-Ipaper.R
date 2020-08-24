which.notnull <- function(x) {
    which(!sapply(x, is.null))
}

is_empty <- function(x) length(x) == 0
