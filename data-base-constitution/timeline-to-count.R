library(tidytext)
library(SnowballC)
library(dplyr)
library(tidyr)
library(stopwords)
library(stringi)
library(Dict)
library(stringr)
library(qdapRegex)

source("./param.R")

# info on journalists
load(file = "../data/journalist-and-description.RData")

# timelines
files  <- list.files(path = "../data/timelines/", pattern = '\\.RData')

print(paste0("Reading: ", files[1]))
load(paste0("../data/timelines/", files[1]))
all_data <- timeline
print(paste0("Size :", nrow(all_data)))



for (file in files[2:length(files)]){
  print(paste0("Reading: ", file))
  load(paste0("../data/timelines/", file))
  print(paste0("Size :", nrow(timeline)))
  all_data <- rbind(all_data, timeline)
}

print(paste0("Number of tweets: ", nrow(all_data)))
all_data <- unique(all_data)
print(paste0("Number of unique tweets: ", nrow(all_data)))

print(paste0("Number of journalists covered: ", length(unique(all_data$screen_name))))

# Add year, month, day column
all_data$year <- stri_sub(all_data$created_at, -4)
all_data$month <- stri_sub(all_data$created_at, from = 4, to = 7)
all_data$day <- stri_sub(all_data$created_at, from = 9, to = 10)

# Keep only relevant columns 
all_data <- all_data[, c("created_at", "id", "full_text", "is_quote_status", "favorite_count", "retweet_count", "lang", "retweeted", "year", "month", "day", "screen_name")]

# Only keep tweets in french
all_data <- all_data[all_data$lang == "fr",]
print(paste0("Number of tweets after keeping only french tweets: ", nrow(all_data)))

# delete urls
all_data$treated.text <- gsub("http[^[:space:]]*", "", all_data$full_text)

# delete words or tag referring to the newspapper
for (redaction in redactions$keys){
  for (tag in redaction){
    all_data$treated.text <- gsub(tag, "", all_data$treated.text)
  }
}


# delete all word starting with a @
all_data$treated.text <-  gsub("@\\w+ *", "", all_data$treated.text)


# remove emoji and non word like ;) with package qdapRegex
all_data$treated.text <- rm_non_words(all_data$treated.text)


# add redaction and duplicate text if necessary
journalist.and.redaction <- probable.journalist[, c("screen_name", "name", redactions$keys)]
journalist.and.redaction <- journalist.and.redaction %>%
  pivot_longer(cols = redactions$keys, names_to = "journal", values_to = "is.in.journal")

journalist.and.redaction <- journalist.and.redaction[journalist.and.redaction$is.in.journal == 1,]

all_data <- merge(journalist.and.redaction, all_data, by = "screen_name", all.x = T)


# Enlever les mots vides et lemmatisation
mots_vides <- tibble(mot = stopwords('fr'))
mots_vides <- rbind(mots_vides, data.frame("mot" = c("a", "h", "tf", "si", "ça", "à")))

timeline.to.tidy.text <- all_data[, c("screen_name", "year", "month", "day", "treated.text", "journal")] %>%
  mutate(treated.text = stringr::str_replace_all(.$treated.text, "’", " ")) %>% 
  unnest_tokens(mot, treated.text) %>%
  anti_join(mots_vides) %>%
  mutate(lemme = wordStem(mot, language = "fr"))


timeline.to.tidy.text <- na.omit(timeline.to.tidy.text)

timeline.to.count <- timeline.to.tidy.text %>%
  group_by(screen_name, mot) %>%
  summarise(count = n())

timeline.to.count <- timeline.to.count %>%
  pivot_wider(names_from = "screen_name", values_from = "count", values_fill = 0 )

write.csv(timeline.to.count, file = "../data/count-mot.csv")


timeline.to.count <- timeline.to.tidy.text %>%
  group_by(screen_name, lemme) %>%
  summarise(count = n())

timeline.to.count <- timeline.to.count %>%
  pivot_wider(names_from = "screen_name", values_from = "count", values_fill = 0 )

write.csv(timeline.to.count, file = "../data/count-lemme.csv")