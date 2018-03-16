data(titanic, package = "onyx")

df <- bfd(titanic, path = "titanic.bin", overwrite = TRUE)
unlink("titanic.bin", T, T)

library(data.table)


## read the first 10k
chunksize <- 10
# cc <- structure(
  # c("integer", "factor", "factor", "numeric", "integer", 
  #   "integer", "numeric", "factor"),
  # .Names = c("Survived", "Pclass", "Sex", "Age", "SibSp", "Parch", "Fare", "Embarked"))

f <- file("~/Documents/bigdata.csvh", open = "r")
df <- bfd(start, path = "bigdata.bin", overwrite=TRUE, write=TRUE)

start <- read.table(f, header = TRUE, sep = ",", nrows = chunksize, col.names = names(cc))
append.bfd(df, start)

#cc <- sapply(start, class)
while(f) {
  chunk <- read.table(f, header = FALSE, sep = ",", nrows = chunksize, colClasses = cc, col.names = names(cc))
  append.bfd(df, chunk)
}


append.bfd(df, titanic)

df

## try ff package

library(ff)


df$Pclass[1:10]

head(df)
tail(df)

df[1:10, c(1,3,5)]

combine.bfds(df, df)


file.size("bigdata.bin")
