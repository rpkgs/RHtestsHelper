
range2 <- function(x, y, na.rm = TRUE) {
    c(min(x, y, na.rm = na.rm), max(x, y, na.rm = na.rm))
}

check_dir <- function(path) {
    for (path_i in path){
        if (!dir.exists(path_i)) {
            dir.create(path_i, recursive = TRUE)
        }
    }
    path
}

listk <- function (...)
{
    cols <- as.list(substitute(list(...)))[-1]
    vars <- names(cols)
    Id_noname <- if (is.null(vars))
        seq_along(cols)
    else which(vars == "")
    if (length(Id_noname) > 0)
        vars[Id_noname] <- sapply(cols[Id_noname], deparse)
    x <- setNames(list(...), vars)
    return(x)
}

#' @export
num2date <- function(x) {
    if (is(x, "Date")) return(x)
    # else
    as.character(x) %>%
        gsub("-", "", .) %>%
        gsub("0000$", "0101", .) %>%
        gsub(  "00$",   "01", .) %>% as.Date("%Y%m%d")
}

#' @export
date2num <- function(date) {
    as.character(date) %>% gsub("-", "", .) %>%
      as.numeric()
}

is.Date <- function(x) is(x, "Date")

which.notnull <- function(x) {
    which(!sapply(x, is.null))
}

which.isnull <- function(x) {
    which(sapply(x, is.null))
}

is_empty <- function(x) {
    # length(x) == 0 || nrow(x) == 0
    if (is.data.frame(x)) nrow(x) == 0 else length(x) == 0
}

rm_empty <- function(x) {
    if (is.list(x)) {
        # browser()
        x[!sapply(x, is_empty)]
    } else {
        x[!is.na(x)]
    }
}

melt_list <- function (list, var.name = "variable", na.rm = TRUE,
    fun_rbind = rbind, ...)
{
    if (is.null(list) || length(list) == 0) {
        return(NULL)
    }
    if (is.null(names(list)))
        names(list) <- seq_along(list)
    list <- rm_empty(list)

    first <- list[[1]]
    if (is.data.table(first)) {
        names <- names(list)
        for (i in seq_along(list)) {
            x <- list[[i]]
            eval(parse(text = sprintf("x$%s <- names[i]", var.name)))
            list[[i]] <- x
        }
        res <- do.call(fun_rbind, list)
    } else {
        id.vars <- colnames(first)
        res <- reshape2::melt(list, ..., id.vars = id.vars, na.rm = na.rm)
        colnames(res) <- c(id.vars, var.name)
    }
    reorder_name(res, var.name)
}

reorder_name <- function(d,
    headvars = c("site", "date", "year", "doy", "d8", "d16"),
    tailvars = "")
{
    names <- names(d)
    headvars %<>% intersect(names)
    tailvars %<>% intersect(names)
    varnames <- c(headvars, setdiff(names, union(headvars, tailvars)), tailvars)

    if (is.data.table(d)) {
        d[, varnames, with = F]
    } else if (is.data.frame(d)) {
        d[, varnames]
    } else if (is.list(d)){
        d[varnames]
    } else{
        stop("Unknown data type!")
    }
}
