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

    write = function(x) {
      on.exit(close.connection(con))
      con <- file(file, open = "w+b")
      writeBin(x, con, size)
      n <<- length(x)
    },

    read = function() {
      on.exit(close.connection(con))
      con <- file(file, open = "r+b")
      out <- readBin(con, what = what, n = n)
      out
    },

    append = function(x) {
      on.exit(close.connection(con))
      con <- file(file, open = "a+b")
      writeBin(x, con, size)
      n <<- n + length(x)
    }
  )
)

bfdcol_factor <- setRefClass(
  "bfdcol_factor",
  fields = list(labels="character"),
  contains = "bfdcol",
  methods = list(
    write = function(x) callSuper(as.integer(x)),
    read = function() factor(callSuper(), labels=labels),
    append = function(x) callSuper(as.integer(x))
  )
)


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

    col <- bfdcol_factor(name = name,
                          what = "integer",
                          size = NA_integer_,
                          file  = tempfile("", path, ".bin"),
                          labels = levels(x))

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
