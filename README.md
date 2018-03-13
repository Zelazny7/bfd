Big Frame o' Data
-----------------

`bfd` is a small project that aims to handle big data. Big for memory,
not for hard drives. It is mostly a learning exercise but it may turn
into something more useful. The `fst` package will hopefully fill the
vacuum of R packages that can chunk over on-disk data, but until then
`bfd` is no big deal.

Classes
-------

`bfd` has a set of reference classes that handle writing and reading
vectors of all R's base types. These are created using the `make_bfdcol`
generic function:

    data(iris)

    unlink("iris.bin", TRUE, TRUE)

    dir.create("iris.bin")
    cols <- mapply(make_bfdcol, iris, names(iris), MoreArgs = list(path="iris.bin", write=TRUE))

The preceding code is a little inconvenient but a function to create a
`bfd` from a `data.frame` is in the works. It returns a list of
`bfdcols` which contain the following information:

    cols[[1]]

    ## Reference class object of class "bfdcol"
    ## Field "name":
    ## [1] "Sepal.Length"
    ## Field "file":
    ## [1] "iris.bin\\168722f2889.bin"
    ## Field "what":
    ## [1] "double"
    ## Field "size":
    ## [1] NA
    ## Field "n":
    ## [1] 150

Reading from a `bfd`
--------------------

Because the `bfdcol`s keep a reference to the binary file location,
reading data back into R requires no further arguments. Wrapping it in a
`data.frame` produces the original dataset:

    d <- data.frame(lapply(cols, function(x) x$read()))

    head(d)

    ##   Sepal.Length Sepal.Width Petal.Length Petal.Width Species
    ## 1          5.1         3.5          1.4         0.2  setosa
    ## 2          4.9         3.0          1.4         0.2  setosa
    ## 3          4.7         3.2          1.3         0.2  setosa
    ## 4          4.6         3.1          1.5         0.2  setosa
    ## 5          5.0         3.6          1.4         0.2  setosa
    ## 6          5.4         3.9          1.7         0.4  setosa

    all.equal(d, iris)

    ## [1] TRUE
