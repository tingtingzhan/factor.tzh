
#' @title \link[base]{factor} Return for \link[dplyr]{case_match}
#' 
#' @description
#' ..
#' 
#' @param .x,...,.default,.ptype see \link[dplyr]{case_match}
#' 
#' @param envir \link[base]{environment} to \link[base]{eval}uate function \link[dplyr]{case_match}.
#' Default `parent.frame()`
#' 
#' @details
#' If argument `.default` is missing, function [case_match_factor] converts 
#' the return of \link[dplyr]{case_match} into a \link[base]{factor}.
#' The order of \link[base]{levels} follows the order of formulas in
#' \link[rlang:dyn-dots]{dynamic dots} `...`.
#' 
#' @returns
#' Function [case_match_factor()] returns a \link[base]{factor}.
#' 
#' @examples
#' # from ?dplyr::case_match
#' x = c('a', 'b', 'a', 'd', 'b', NA, 'c', 'e')
#' dplyr::case_match(x, 'a' ~ 1, 'b' ~ 2, 'c' ~ 3, 'd' ~ 4)
#' case_match_factor(x, 'a' ~ 1, 'b' ~ 2, 'c' ~ 3, 'd' ~ 4)
#' case_match_factor(x, 'a' ~ 1, 'b' ~ 2, 'c' ~ 1, 'd' ~ 2)
#' 
#' case_match_factor(x, 'a' ~ 1, 'd' ~ 4, 'b' ~ 2, 'c' ~ 3) # order matters!
#' 
#' y = c(1, 2, 1, 3, 1, NA, 2, 4)
#' case_match_factor(y, NA ~ 0, .default = y)
#' @keywords internal
#' @importFrom dplyr case_match
#' @export
case_match_factor <- function(
    .x, ..., .default = NULL, .ptype = NULL,
    envir = parent.frame()
) {
  
  cl <- match.call()
  cl[[1L]] <- quote(dplyr::case_match)
  # must have `dplyr::` !!
  # otherwise ?devtools::check() says cannot find `case_match` (in parent.frame())
  # do not understand why..
  
  cl$envir <- NULL
  ret0 <- eval(cl, envir = envir) # must use `envir`, otherwise cannot find `substitute(x)`
  
  if (length(cl$.default)) {
    message('Presence of ', sQuote('.default'), ' prohibits conversion to factor')
    return(ret0)
  }
  
  arg_ <- as.list(cl)[-1L]
  target <- arg_[!nzchar(names(arg_))]
  lev_ <- vapply(target, FUN = \(i) as.character(i[[3L]]), FUN.VALUE = '')
  factor(ret0, levels = unique.default(lev_))
  
}
