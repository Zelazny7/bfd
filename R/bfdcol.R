### get sizes of binary types
bfdcol <- setRefClass(
  "bfdcol",
  fields = list(
    name = "character",
    file = "character",
    what = "character",
    size = "ANY",
    n    = "numeric"
  ),

  methods = list(
    initialize = function(...) {
      n <<- 0
      callSuper(...)
    },

    write  = function(x) Write(.self, x),
    read   = function(i) {
      if (missing(i)) i <- seq.int(n)
      Read(.self, i)
    },
    append = function(x) Append(.self, x)
  )
)

#' @export
setGeneric("Read", def = function(col, i) standardGeneric("Read"))

#' @export
setGeneric("Write", def = function(col, x) standardGeneric("Write"))

#' @export
setGeneric("Append", def = function(col, x) standardGeneric("Append"))

#' @export
setMethod(
  "Read",
  c("bfdcol", "numeric"),
  function(col, i) {
    on.exit(close.connection(con))
    
    bytes <- if (col$what == "double") 8 else 4
    
    con <- file(col$file, open = "rb")
    
    l <- split(i, cumsum(c(TRUE, diff(i) != 1)))
    l <- lapply(l, function(x) list(where=x[[1]], n=length(x)))
    
    out <- vector("list", length(l))
    for (j in seq_along(l)) {
      ## go to position in file
      seek(con, where = (l[[j]]$where - 1) * bytes)
      
      out[[j]] <- readBin(con, what = col$what, n = l[[j]]$n)
      
    }
    return(unlist(out, use.names = FALSE))
  })

#' @export
setMethod(
  "Write",
  c("bfdcol", "ANY"),
  function(col, x) {
    on.exit(close.connection(con))
    con <- file(col$file, open = "wb")
    writeBin(x, con, col$size)
    col$n <- length(x)
  })

#' @export
setMethod(
  "Append",
  c("bfdcol", "ANY"),
  function(col, x) {
    on.exit(close.connection(con))
    con <- file(col$file, open = "ab")
    writeBin(x, con, col$size)
    col$n <- col$n + length(x)
  })

### Implement Append functions for when bfdcol and new data don't match
### Need to widen the data to the new type
### 1. Determine new type
### 2. Open connection to existing location
### 3. Read data in n bytes at a time
### 4. Write it and then append it in the new format
### 5. Append the new data

bfdcol_numeric <- setRefClass("bfdcol_numeric", contains="bfdcol")
bfdcol_integer <- setRefClass("bfdcol_integer", contains="bfdcol")
bfdcol_logical <- setRefClass("bfdcol_logical", contains="bfdcol")
bfdcol_character <- setRefClass("bfdcol_character", contains="bfdcol")
bfdcol_factor <- setRefClass("bfdcol_factor",
                             fields = list(levels="integer", labels="character"),
                             contains="bfdcol")

###### FACTOR COLUMN METHODS ######
#' @export
setMethod(
  "Write",
  c("bfdcol_factor", "factor"),
  function(col, x) {
    Write(col, as.integer(x))
  })

#' @export
setMethod(
  "Read",
  c("bfdcol_factor", "numeric"),
  function(col, i) {
    factor(callNextMethod(), levels=col$levels, labels=col$labels)
  })

#' @export
setMethod(
  "Append",
  c("bfdcol_factor", "factor"),
  function(col, x) {
    col$levels <- union(col$levels, seq_along(levels(x)))
    col$labels <- union(col$labels, levels(x))
    Append(col, match(x, col$labels))
  })


setGeneric("make_bfdcol", function(x, name, path, write=FALSE) standardGeneric("make_bfdcol"))

setMethod(
  "make_bfdcol",
  signature = c("integer"),
  definition = function(x, name, path, write) {

    col <- bfdcol(name = name,
                   what = "integer",
                   size = NA_integer_,
                   file  = tempfile("", path, ".bin"))

    if (write) col$write(x)
    col
  })

setMethod(
  "make_bfdcol",
  signature = c("numeric"),
  definition = function(x, name, path, write) {

    col <- bfdcol(name  = name,
                   what  = "double",
                   size  = NA_real_,
                   file  = tempfile("", path, ".bin"))

    if (write) col$write(x)
    col
  })


setMethod(
  "make_bfdcol",
  signature = c("logical"),
  definition = function(x, name, path, write) {

    col <- bfdcol(name = name,
                   what = "logical",
                   size = NA,
                   file = tempfile("", path, ".bin"),
                   lbls = levels(x))

    if (write) col$write(x)
    col
  })

setMethod(
  "make_bfdcol",
  signature = c("factor"),
  definition = function(x, name, path, write) {

    lvls <- levels(x)

    col <- bfdcol_factor(name = name,
                         what = "integer",
                         size = NA_integer_,
                         file  = tempfile("", path, ".bin"),
                         levels = seq.int(length(lvls)),
                         labels = lvls)

    if (write) col$write(x)
    col
  })


setMethod(
  "make_bfdcol",
  signature = c("character"),
  definition = function(x, name, path, write) {

    col <- bfdcol(name = name,
                   what = "character",
                   size = NA_character_,
                   file  = tempfile("", path, ".bin"))

    if (write) col$write(x)
    col
  })



#' @export
summary.bfdcol <- function(object, ...) {
  with(object, sprintf("%-32s %10s %10d", name, what, n))
}

