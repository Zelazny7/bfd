#' @export
bfd <- function(df, path, overwrite=FALSE, write=TRUE) {

  ## create the bfd object
  if (file.exists(path)) {
    if (!overwrite) {
      stop(path, " already exists and overwrite=FALSE")
    } else {
      unlink(path, recursive = TRUE, force = TRUE)
      warning(path, " already exists and overwrite=TRUE. Deleting existing folder.")
    }
  }

  dir.create(path)

  value <- mapply(make_bfdcol, df, names(df), MoreArgs = list(path=path, write=write), SIMPLIFY = FALSE)
  attr(value, "class") <- "bfd"
  attr(value, "path") <- path
  
  value
}

#' @export
summary.bfd <- function(object, ...) {

  s <- sapply(object, summary)

  header <- sprintf("%-32s %10s %10s", "Variable", "Storage", "N")

  out <- data.frame(s, stringsAsFactors = F, row.names = NULL)
  names(out) <- header

  path <- attr(object, "path")
  print(out)
  cat(sprintf("[Location | %s]", path), sep = "\n")
}


## implement a head function

#' @export
dim.bfd <- function(x) {
  nr <- x[[1]]$n
  as.integer(c(nr, length(x)))
}

#' @export
`[.bfd` <- function(x, i, j) {

  has.i <- !missing(i)
  has.j <- !missing(j)


  ## Used to differntiate between these scenarios:
  ## df[1:3] => Return cols && df[1:3,] ==> return rows
  if (nargs() < 3L) {

    data.frame(lapply(unclass(x)[i], function(col) col$read()))

  } else {

    if (has.i && has.j) {

      data.frame(lapply(unclass(x)[j], function(col) col$read(i)))

    } else if (has.i) {

      data.frame(lapply(x, function(col) col$read(i)))

    } else {

      data.frame(lapply(unclass(x)[j], function(col) col$read()))

    }
  }

}
#' @export
head.bfd <- function(x, n=6L, ...) {

  stopifnot(length(n) == 1L)
  n <- if (n < 0L)
    max(nrow(x) + n, 0L)
  else min(n, nrow(x))

  x[seq_len(n),]
}

#' @export
tail.bfd <- function(x, n=6L, ...) {

  stopifnot(length(n) == 1L)
  xlen <- nrow(x)
  n <- if (n < 0L)
    max(xlen + n, 0L)
  else min(n, xlen)

  x[seq.int(to = xlen, length.out = n),]
}


#' @export
`$.bfd` <- function(x, name) {

  x[[name]]$read()

}

#' @export
print.bfd <- function(x, ...) {
  invisible(summary(x))
}


## replace
#' @export
`$<-.bfd` <- function(x, name, value) {

  nrows <- nrow(x)

  if (!is.null(value)) {

    N <- NROW(value)

    ## check lengths
    if (N > nrows) {
      stop(sprintf(ngettext(N, "replacement has %d row, data has %d",
                   "replacement has %d rows, data has %d"), N, nrows), domain = NA)
    }

    if (N < nrows) {
      if (N > 0L && (nrows%%N == 0L) && length(dim(value)) <= 1L) {
        ## recycle
        value <- rep(value, length.out = nrows)
      } else {
        stop(sprintf(ngettext(N, "replacement has %d row, data has %d",
                     "replacement has %d rows, data has %d"), N, nrows), domain = NA)
      }
      if (is.atomic(value) && !is.null(names(value))) names(value) <= NULL
    }

    x[[name]] <- make_bfdcol(value, name = name, path = attr(x, "path"), write = TRUE)
  } else {

    ## drop the column
    unlink(x[[name]]$file)
    x[[name]] <- NULL
  }

  x
}

## need to be able to append a data.frame to a bfd

append.bfd <- function(x, y) {
  
  ## check that they are all the same
  stopifnot(identical(names(x), names(y)))
  
  for (i in seq_along(x)) x[[i]]$append(y[[i]])
  
  x
}
