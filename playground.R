library(data.table)
library(quanteda)
res <- fread("data/responses.csv")
names(res) <- c("timestamp",
                "prepare",
                "reading",
                "foreign",
                "team",
                "critique",
                "self",
                "research",
                "missing",
                "school")


words <- res$school[nchar(res$school) < 100 & nchar(res$school) > 0]
words <- gsub("und|unterricht|wissenschaften|bildnerisches","",
              do.call("c", strsplit(words,";")))

textplot_wordcloud(dfm(words),min_count = 1,color = viridis::viridis(5))