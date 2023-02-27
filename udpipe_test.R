install.packages("installr")
library(installr)
find_rtools()
Sys.getenv("PATH")

library(udpipe)

# use this model, because the other one can't handle gendered language
dl <- udpipe_download_model(language = "german-gsd")

udmodel_german <- udpipe_load_model(file = dl$file_model)

x <- udpipe_annotate(udmodel_german, x = taz_subset$body)
x <- as.data.frame(x)
str(x)

table(x$upos)

## Tokenization + finds sentences, does not execute POS tagging, nor lemmatization or dependency parsing
x <- udpipe_annotate(udmodel_german, x = taz_subset$body, tagger = "none", parser = "none")
x <- as.data.frame(x)
table(x$upos)
table(x$dep_rel)


