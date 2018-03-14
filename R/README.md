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

    library(bfd)

    data(iris)
    df <- bfd(iris, path = "iris.bin", overwrite = TRUE)

    ## Warning in bfd(iris, path = "iris.bin", overwrite = TRUE): iris.bin already
    ## exists and overwrite=TRUE. Deleting existing folder.

    df

    ##   Variable                            Storage          N
    ## 1 Sepal.Length                         double        150
    ## 2 Sepal.Width                          double        150
    ## 3 Petal.Length                         double        150
    ## 4 Petal.Width                          double        150
    ## 5 Species                             integer        150
    ## [Location | iris.bin]

Indexing operation
------------------

    df$Species[1:10]

    ##  [1] setosa setosa setosa setosa setosa setosa setosa setosa setosa setosa
    ## Levels: setosa versicolor virginica

    head(df)

    ##   Sepal.Length Sepal.Width Petal.Length Petal.Width Species
    ## 1          5.1         3.5          1.4         0.2  setosa
    ## 2          4.9         3.0          1.4         0.2  setosa
    ## 3          4.7         3.2          1.3         0.2  setosa
    ## 4          4.6         3.1          1.5         0.2  setosa
    ## 5          5.0         3.6          1.4         0.2  setosa
    ## 6          5.4         3.9          1.7         0.4  setosa

    tail(df)

    ##   Sepal.Length Sepal.Width Petal.Length Petal.Width   Species
    ## 1          6.7         3.3          5.7         2.5 virginica
    ## 2          6.7         3.0          5.2         2.3 virginica
    ## 3          6.3         2.5          5.0         1.9 virginica
    ## 4          6.5         3.0          5.2         2.0 virginica
    ## 5          6.2         3.4          5.4         2.3 virginica
    ## 6          5.9         3.0          5.1         1.8 virginica

    df[1:10, c(1,4)]

    ##    Sepal.Length Petal.Width
    ## 1           5.1         0.2
    ## 2           4.9         0.2
    ## 3           4.7         0.2
    ## 4           4.6         0.2
    ## 5           5.0         0.2
    ## 6           5.4         0.4
    ## 7           4.6         0.3
    ## 8           5.0         0.2
    ## 9           4.4         0.2
    ## 10          4.9         0.1

Combining bfds
--------------

These functions will eventually be called from `rbind` and `cbind`
equivalents:

    df <- combine.bfds(df, df)

    df

    ##   Variable                            Storage          N
    ## 1 Sepal.Length                         double        300
    ## 2 Sepal.Width                          double        300
    ## 3 Petal.Length                         double        300
    ## 4 Petal.Width                          double        300
    ## 5 Species                             integer        300
    ## [Location | iris.bin]
