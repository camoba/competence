library(data.table)
library(quanteda)
library(ggplot2)
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

textplot_wordcloud(dfm(words),
                   min_count = 1,
                   min_size = 2,
                   color = viridis::viridis(5),
                   fixed_aspect = FALSE,
                   rotation = 0)


res_num <- res[, .SD,
               .SDcols = sapply(res, is.numeric)]
res_num[,id := 1:nrow(res_num)]
res_m <- melt(res_num,id.vars = "id")
avg <- res_m[, list(avg = mean(value)), variable]

gg_avg <- ggplot(data = avg)
gg_avg + geom_bar(aes(x = variable,
                      y = avg),
                  stat = "identity") +
  coord_flip() +
  theme_minimal() +
  theme(panel.grid.major.x = element_blank(),
        panel.spacing = unit(4, "lines"),
        text = element_text(size=15))


gg_read <- ggplot(data = res_m[variable %in% c("prepare",
                                          "reading","foreign"),])
gg_read +
  geom_bar(aes(x = as.factor(value), fill = variable)) +
  facet_wrap("variable", nrow = 1) +
  theme_minimal() +
  theme(panel.grid.major.x = element_blank(),
        panel.spacing = unit(4, "lines"),
        text = element_text(size=15)) +
  scale_x_discrete(name ="Skill",
                   limits=factor(1:5)) +
  scale_fill_viridis_d()


gg_soft <- ggplot(data = res_m[variable %in% c("team",
                                               "critique","self"),])
gg_soft +
  geom_bar(aes(x = as.factor(value), fill = variable)) +
  facet_wrap("variable", nrow = 1) +
  theme_minimal() +
  theme(panel.grid.major.x = element_blank(),
        panel.spacing = unit(4, "lines"),
        text = element_text(size=15)) +
  scale_x_discrete(name ="Skill",
                   limits=factor(1:5)) +
  scale_fill_viridis_d()


gg_research <- ggplot(data = res_m[variable %in% c("research"),])
gg_research +
  geom_bar(aes(x = as.factor(value), fill = variable)) +
  facet_wrap("variable", nrow = 1) +
  theme_minimal() +
  theme(panel.grid.major.x = element_blank(),
        panel.spacing = unit(4, "lines"),
        text = element_text(size=15)) +
  scale_x_discrete(name ="Skill",
                   limits=factor(1:5)) +
  scale_fill_viridis_d()




unique(res$missing[res$missing != ""])

nchar("Texte kritisch zu hinterfragen und nicht einfach nur des Informationsgehalts wegens zu lesen.")

out_text <- unique(res$missing[res$missing != ""])
out_stripped <- substr(out_text,1,95)
out_stripped[nchar(out_text) > 95] <- paste0(out_stripped[nchar(out_text) > 95],"[...]")
out_stripped

