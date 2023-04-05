cpt_meanvar <- function(y, date = NULL, fun = cpt.meanvar, ...){
  tryCatch({
    l = fun(y, ...)
    if (length(l@cpts) > 1) {
      as.data.table(l@param.est) %>%
        cbind(cpt = as.integer(l@cpts), .) %>%
        mutate(date = date[cpt], .before = "cpt")
    } else {
      NULL
    }
  }, error = function(e) {
    message(sprintf('%s', e$message))
  })
}

cpt_bcp <- function(y, date = NULL, fun = cpt.meanvar, ...){
  tryCatch({    
    l = bcp::bcp(y, ...)
    prob = l$posterior.prob
    inds = which(prob > 0.5)
    if (is_empty(inds)) return(NULL)

    data.table(date = date[inds], cpt = inds, prob = prob[inds])
  }, error = function(e) {
    message(sprintf('%s', e$message))
  })
}
