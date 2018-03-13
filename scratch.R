data(titanic, package = "onyx")
# # dir.create("titanic.bin")
#
#
cols <- mapply(make_mmdfcol, titanic, names(titanic), MoreArgs = list(path="titanic.bin", write=TRUE))
#
# ## append 100 times
#

for (j in 1:1000) {
  for (i in seq_along(titanic)) {
    x <- titanic[[i]]
    cols[[i]]$append(x)
  }
}



d <- data.frame(lapply(cols, function(x) x$read()))



#
