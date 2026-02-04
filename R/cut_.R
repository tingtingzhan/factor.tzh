
#' @title Divide Numeric Into Intervals
#' 
#' @description 
#' Divide numeric into intervals; an alternative to function \link[base]{cut.default}.
#' 
#' @param x \link[base]{integer}, \link[base]{numeric}, \link[base]{difftime}, \link[base]{Date},
#' \link[base]{POSIXct} \link[base]{vector} or \link[base]{matrix}.
#' 
#' @param breaks \link[base]{vector} of the same \link[base]{class} as `x`. 
#' `-Inf` and `Inf` will be automatically added
#' 
#' @param probs \link[base]{double} \link[base]{vector} from 0 to 1,
#' probabilities to specify the \link[stats]{quantile}s to be used as `breaks`
#' 
#' @param data.name \link[base]{character} scalar, name of data.
#' R \link[base]{language} is also accepted.
#' Default is the argument call of `x`
#' 
#' @param ... additional parameters `right` and `include.lowest` of 
#' functions \link[base]{cut.default} and \link[base]{.bincode}.
#' 
#' @details 
#' 
#' Function [cut_()] is different from function \link[base]{cut.default}, that 
#' \itemize{
#' \item {More classes of `x` are accepted, see **Arguments**}
#' \item {`-Inf` and `Inf` are added to `breaks`, 
#' so that the values outside of `breaks` will be correctly categorized,
#' instead of returning an `NA_integer` per \link[base]{.bincode}}
#' \item {More user-friendly \link[base]{factor} \link[base]{levels}, see helper function [cut_levels]}
#' }
#' 
#' @examples 
#' morley$Speed |> 
#'  cut_(breaks = 100*(8:9), data.name = 'speed') |> 
#'  table()
#' morley$Speed |> 
#'  cut_(probs = c(.3, .6)) |> 
#'  table()
#' 
#' VADeaths |> 
#'  cut_(probs = c(.3, .6))
#' 
#' JohnsonJohnson |> 
#'  zoo::as.Date.ts() |>
#'  cut_(breaks = as.Date(c('1970-01-01', '1975-01-01')), 
#'    data.name = 'JnJ') |>
#'  table()
#' 
#' @keywords internal  
#' @name cut_
#' @importFrom stats quantile
#' @export
cut_ <- function(
    x, 
    breaks = quantile(x, probs = probs, na.rm = TRUE), 
    probs,
    data.name = substitute(x),
    ...
) {
  
  if (inherits(x, what = 'POSIXlt')) x <- as.POSIXct.POSIXlt(x)
  if (is.array(x)) {
    if (typeof(x) != 'double') stop('`x` must be typeof-\'double\' matrix')
  } else if (!inherits(x, what = c('integer', 'numeric', 'difftime', 'Date', 'POSIXct'))) {
    stop('illegal `x` ', sQuote(class(x)[1L]))
  }
  
  breaks <- c(breaks, -Inf, Inf) |> # `breaks` before `Inf` and `-Inf`, then the returned value has attributes of `breaks`
    unique() |> # dont want to use my [unique_allequal]
    sort() # *not* ?base::sort.int # I need to retain the class of `breaks`
  
  ret <- x
  storage.mode(ret) <- 'integer'
  ret[] <- .bincode(x, breaks = breaks, ...) # 'integer'
  # keeps `attributes(x)`
  
  attr(ret, which = 'levels') <- cut_levels(breaks = breaks, ..., data.name = data.name)
  class(ret) <- 'factor'
  return(ret)

}






#' @title [cut_levels()]
#' 
#' @param breaks \link[base]{vector} of the same \link[base]{class} as `x`. 
#' `-Inf` and `Inf` will be automatically added
#' 
#' @param right \link[base]{logical} scalar, default `TRUE`, 
#' see functions \link[base]{cut.default} and \link[base]{.bincode}.
#' 
#' @param include.lowest \link[base]{logical} scalar, default `TRUE`, 
#' see functions \link[base]{cut.default} and \link[base]{.bincode}.
#' 
#' @param data.name \link[base]{character} scalar, name of data.
#' R \link[base]{language} is also accepted.
#' Default is the argument call of `x`
#' 
#' @examples 
#' ## Examples on Helper Function cut_levels()
#' foo = \(...) {
#'  cbind(
#'   numeric(0) |> cut.default(...) |> levels(),
#'   cut_levels(...)
#'  )
#' }
#' foo(breaks = 1:4, right = TRUE, include.lowest = TRUE)
#' foo(breaks = 1:4, right = FALSE, include.lowest = TRUE)
#' foo(breaks = 1:4, right = TRUE, include.lowest = FALSE)
#' foo(breaks = 1:4, right = FALSE, include.lowest = FALSE)
#' set.seed(29); foo(breaks = c(-Inf, sort(rnorm(1:3)), Inf))
#' @keywords internal
#' @export
cut_levels <- function(
    breaks, 
    right = TRUE, 
    include.lowest = TRUE, 
    data.name = 'x'
) {
  
  if (is.language(data.name)) data.name <- deparse1(data.name)
  if (!is.character(data.name) || length(data.name) != 1L || is.na(data.name) || !nzchar(data.name)) stop('illegal `data.name`')
  nb <- length(breaks)
  
  fmt_breaks <- trimws(if (inherits(breaks, what = 'Date')) {
    breaks |> format() # use S3
  } else if (is.integer(breaks)) {
    breaks |> as.character.default()
  } else if (is.double(breaks)) {
    breaks |> sprintf(fmt = '%.3g')
  })
  b1 <- fmt_breaks[1:(nb-1L)]
  b2 <- fmt_breaks[2:nb]
  
  if (right) {
    # `r`elationship
    r1 <- if (!include.lowest) '<' else c('\u2264', rep('<', times = nb - 2L)) 
    r2 <- '\u2264'
  } else {
    r1 <- '\u2264'
    r2 <- if (!include.lowest) '<' else c(rep('<', times = nb - 2L), '\u2264') 
  }
  
  L <- gsub(pattern = '^-Inf\u2264|^-Inf<', replacement = '', x = paste0(b1, r1)) 
  R <- gsub(pattern = '\u2264Inf$|<Inf$', replacement = '', x = paste0(r2, b2)) 
  
  if (!nzchar(R[nr <- length(R)])) {
    # change last category from `a<X` or `a<=X` to `X>a` and `X>=a`
    L[nr] <- ''
    R[nr] <- if (identical(r1[length(r1)], '\u2264')) {
      paste0('\u2265', b1[nr])
    } else if (identical(r1[length(r1)], '<')) {
      paste0('>', b1[nr])
    } else stop('shouldnt come here')
  }
  
  paste0(L, data.name, R)
  
}


if (FALSE) {
  # ?base::.bincode much faster than ?base::findInterval
  x = 2:18
  v = c(5, 10, 15) # create two bins [5,10) and [10,15)
  findInterval(x, v)
  .bincode(x, v)
  library(microbenchmark)
  microbenchmark(findInterval(x, v), .bincode(x, v))
}


