data(titanic, package = "onyx")

df <- bfd(titanic, path = "titanic.bin", overwrite = TRUE)

df

df$Pclass[1:10]

head(df)
tail(df)

df[1:10, c(1,3,5)]

combine.bfds(df, df)
