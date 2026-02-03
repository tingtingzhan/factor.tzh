
#' @title \link[base]{factor} Return for \link[dplyr]{recode_values}
#' 
#' @description
#' ..
#' 
#' @param x,...,default,ptype see parameters of function \link[dplyr]{recode_values}
#' 
#' @param envir \link[base]{environment} to \link[base]{eval}uate function \link[dplyr]{recode_values}.
#' Default `parent.frame()`
#' 
#' @details
#' If argument `default` is missing, function [recode2factor()] converts 
#' the return of function \link[dplyr]{recode_values} into a \link[base]{factor}.
#' The order of \link[base]{levels} follows the order of formulas in
#' \link[rlang:dyn-dots]{dynamic dots} `...`.
#' 
#' @returns
#' Function [recode2factor()] returns a \link[base]{factor}.
#' 
#' @note
#' Functions \link[dplyr]{case_match} and \link[dplyr]{recode_factor} 
#' are deprecated (since v1.2.0) with \link[dplyr]{recode_values}.
#' 
#' @examples
#' # from ?dplyr::recode_values
#' x = c('a', 'b', 'a', 'd', 'b', NA, 'c', 'e')
#' dplyr::recode_values(x, 'a' ~ 1, 'b' ~ 2, 'c' ~ 3, 'd' ~ 4)
#' recode2factor(x, 'a' ~ 1, 'b' ~ 2, 'c' ~ 3, 'd' ~ 4)
#' recode2factor(x, 'a' ~ 1, 'b' ~ 2, 'c' ~ 1, 'd' ~ 2)
#' 
#' recode2factor(x, 'a' ~ 1, 'd' ~ 4, 'b' ~ 2, 'c' ~ 3) # order matters!
#' 
#' y = c(1, 2, 1, 3, 1, NA, 2, 4)
#' recode2factor(y, NA ~ 0, default = y)
#' @keywords internal
#' @importFrom dplyr recode_values
#' @export
#case_match_factor 
recode2factor <- function(
    x, ..., default = NULL, ptype = NULL,
    envir = parent.frame()
) {
  
  cl <- match.call()
  cl[[1L]] <- quote(dplyr::recode_values)
  # must have `dplyr::` !!
  # otherwise ?devtools::check() says cannot find `recode_values` (in parent.frame())
  # do not understand why..
  
  cl$envir <- NULL
  ret0 <- eval(cl, envir = envir) # must use `envir`, otherwise cannot find `substitute(x)`
  
  if (length(cl$default)) {
    message('Presence of ', sQuote('default'), ' prohibits conversion to factor')
    return(ret0)
  }
  
  arg_ <- as.list(cl)[-1L]
  
  arg_[!nzchar(names(arg_))] |> 
    vapply(FUN = \(i) as.character(i[[3L]]), FUN.VALUE = '') |>
    unique.default() |>
    factor(ret0, levels = _)
  
}
