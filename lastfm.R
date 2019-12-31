?read.csv
scrobles <- read.csv("/users/sjx/desktop/last.fm/shakingkelly.csv")
artists <- read.csv("/users/sjx/desktop/last.fm/artists.csv")

head(scrobles)
head(artists)

(last <- colnames(scrobles))
colnames(scrobles) <- c("artist", "album", "track", "timestamp")
scrobles <- rbind(last, scrobles)
head(scrobles)
str(scrobles)
scrobles[1, ]$album <- "The Most Hated"

scrobles$timestamp <- strptime(scrobles$timestamp, format = "%d %b %Y %H:%M")
scrobles[1, ]$timestamp <- as.POSIXct("23 Dec 2019 04:28", format="%d %b %Y %H:%M")
rm(last)

str(scrobles)
install.packages("lubridate")
library(lubridate)
scrobles$year <- year(scrobles$timestamp)
scrobles$month <- month(scrobles$timestamp)
scrobles$date <- day(scrobles$timestamp)
scrobles$hour <- hour(scrobles$timestamp)
scrobles$wday <- wday(scrobles$timestamp)
scrobles$id <- c(1:nrow(scrobles))
scrobles <- scrobles[, c(10, 1:9)]
head(scrobles)
scrobles$timestamp <- as.character(scrobles$timestamp)
scrobles %>% filter(id == 2) %>% select(album)
scrobles %>% filter(grepl("^Bach", track))

nrow(scrobles)
nrow(scrobles[complete.cases(scrobles), ])
nrow(scrobles %>% filter(album != ""))
scrobles_raw <- scrobles
scrobles <- scrobles_raw %>% filter(album != "")
(c(nrow(scrobles_raw), nrow(scrobles)))

scnt <- scrobles %>% group_by(year, month, artist) %>% count() 
scnt <- scnt[with(scnt, order(-year, -month, -n)), ]
scnt_top <- scnt %>% group_by(year, month) %>% filter(n == max(n))
scnt_top %>% print(n = Inf)
# library(data.table)
# scnt <- as.data.table(scnt)
# scnt[, SD(which.max(n)), by=list(year, month)]

library(ggplot2)
scrobles$ym <- paste(scrobles[, "year"], scrobles[, "month"])
scrobles %>% filter(year == 2019 & month == 12) %>% group_by(artist) %>% count() %>% filter(n > 50)
scrobles %>% group_by(year, month, artist) %>% count() %>% filter(n > 20)
# ggplot(scrobles %>% group_by(year, month, artist) %>% count() %>% filter(n > 20), aes(x=artist)) + 
#   geom_bar() + 
#   facet_wrap(~year+month)

# most listened artists by month
ggplot(scrobles %>% group_by(year, month, artist) %>% count() %>% filter(n > 100), aes(x=artist, y=n)) + 
  geom_col() + 
  facet_wrap(~year+month) + 
  theme(text = element_text(size=5), axis.text.x = element_text(angle = 90, hjust=1))

# all time most listened artists by year
scrobles$year <- factor(scrobles$year)
top_artists <- as.data.frame(scrobles %>% group_by(year, month, artist) %>% count() %>% filter(n > 100))[, "artist"]
ggplot(scrobles %>% filter(artist %in% top_artists), aes(x=artist, fill=year)) + 
  geom_bar(position="dodge") + 
  theme(text = element_text(size=5), axis.text.x = element_text(angle = 90, hjust=1))

# top 10 albums by year
top_albums <- scrobles %>% group_by(year, album) %>% count()
top_albums <- top_albums[with(top_albums, order(-n)), ]
top_albums <- top_albums %>% group_by(year) %>% top_n(10)
top_albums[with(top_albums, order(year, -n)), ] %>% print(n = Inf)
ggplot(top_albums %>% filter(year != "2017"), aes(x=album, y=n, fill=year), alpha=0.3) + 
  geom_col() + 
  theme(text = element_text(size=5), axis.text.x = element_text(angle = 90, hjust=1)) + 
  coord_flip()

# top tracks for my fav artists
muse <- scrobles %>% filter(artist == "Muse")
dir <- scrobles %>% filter(artist == "DIR EN GREY")
polyphia <- scrobles %>% filter(artist == "Polyphia")

muse_tracks <- muse %>% group_by(track) %>% count()
muse_tracks <- muse_tracks[with(muse_tracks, order(-n)), ] %>% filter(n > 20)
ggplot(muse_tracks, aes(x=reorder(track, -n), y=n)) + 
  geom_col() + 
  theme(text = element_text(size=5), axis.text.x = element_text(angle = 90, hjust=1))

dir_tracks <- dir %>% group_by(track) %>% count()
dir_tracks <- dir_tracks[with(dir_tracks, order(-n)), ] %>% filter(n > 20)
ggplot(dir_tracks, aes(x=reorder(track, -n), y=n)) + 
  geom_col() + 
  theme(text = element_text(size=5, family="HiraKakuProN-W3"), axis.text.x = element_text(angle = 90, hjust=1)) 

polyphia_tracks <- polyphia %>% group_by(album, track) %>% count()
polyphia_tracks <- polyphia_tracks[with(polyphia_tracks, order(-n)), ] %>% filter(n > 20)
ggplot(polyphia_tracks, aes(x=reorder(track, -n), y=n, fill=album)) + 
  geom_col() + 
  theme(text = element_text(size=5), axis.text.x = element_text(angle = 90, hjust=1))

# artists
artists_raw <- artists
artists <- artists_raw %>% filter(stats_playcount > 1000)
artists <- artists[with(artists, order(-stats_playcount)), ]

top_artists <- unique(top_artists)
most_playcounts <- artists %>% top_n(100, wt=stats_playcount) %>% select("name")
most_listeners <- artists %>% top_n(100, wt=stats_listeners) %>% select("name")
most_dedicated <- artists %>% mutate(ratio=stats_playcount/stats_listeners) %>% top_n(100, wt=ratio) %>% select("name")

most_popular <- cbind(most_playcounts, most_listeners, most_dedicated)
colnames(most_popular) <- c("most_playcounts", "most_listeners", "most_dedicated")
most_popular

absolute_biggies <- intersect(intersect(most_dedicated[, "name"], 
                                        most_playcounts[, "name"]), 
                              most_listeners[, "name"])
cults <- setdiff(setdiff(most_dedicated[, "name"], most_playcounts[, "name"]), most_listeners[, "name"])

library(dplyr)
library(ggplot2)
artists$ratio <- artists$stats_playcount/artists$stats_listeners
artists %>% mutate(type=case_when(name %in% absolute_biggies ~ "biggies", name %in% cults ~ "cults"))

library(cowplot)
p1 <- ggplot() + 
  geom_point(data=artists %>% filter(name %in% cults), aes(x=reorder(name, -ratio), y=ratio), color="red") + 
  theme(text = element_text(size=5, family="HiraKakuProN-W3"), axis.text.x = element_text(angle = 90, hjust=1)) + 
  ylim(50, 250)
p2 <- ggplot() + 
  geom_point(data=artists %>% filter(name %in% absolute_biggies), aes(x=reorder(name, -ratio), y=ratio), color="blue") + 
  theme(text = element_text(size=5, family="HiraKakuProN-W3"), axis.text.x = element_text(angle = 90, hjust=1)) + 
  ylim(50, 250) 
plot_grid(p1, p2)


# tags
artists$tags <- as.character(artists$tags)
tag_set <- c()
for (row in 1:nrow(artists)) {
  tags <- unlist(strsplit(artists[row, "tags"], ", "))
  tag_set <- union(tag_set, tags)
}
tag_set

tag_words <- c()
for (row in 1:nrow(artists)) {
  tags <- unlist(strsplit(artists[row, "tags"], ", "))
  for (tag in tags) {
    for (word in unlist(strsplit(tag, " "))) {
      words <- unlist(strsplit(word, "-"))
      words <- unlist(lapply(words, tolower))
      tag_words <- c(tag_words, words)
    }
  }
}
length(tag_words)

tag_word_count <- as.data.frame(table(tag_words))
tag_word_count <- tag_word_count[order(tag_word_count$Freq, decreasing=TRUE), ]
head(tag_word_count, 20)
tag_word_count %>% filter(Freq > 20) %>% mutate(Prop = Freq / sum(Freq))

tag_phrases <- c()
for (row in 1:nrow(artists)) {
  tags <- unlist(strsplit(artists[row, "tags"], ", "))
  for (tag in tags) {
    phrases <- unlist(lapply(tag, tolower))
    tag_phrases <- c(tag_phrases, phrases)
  }
}

tag_phrase_count <- as.data.frame(table(tag_phrases))
tag_phrase_count <- tag_phrase_count[order(tag_phrase_count$Freq, decreasing=TRUE), ]
tag_phrase_count <- tag_phrase_count %>% filter(Freq > 1 & tag_phrases != "seen live")
tag_phrases <- intersect(tag_phrases, tag_phrase_count[, "tag_phrases"])
tag_phrases

ggplot(tag_phrase_count %>% filter(Freq > 20) %>% mutate(Prop = Freq / sum(Freq)), 
       aes(x=reorder(tag_phrases, -Freq), y=Freq)) + 
  geom_col() + 
  theme(text = element_text(size=5), axis.text.x = element_text(angle = 90, hjust=1))

# dummy tag variables
head(artists[, c("name", "tags")], 100)

tag_df <- data.frame(matrix(0, ncol = length(tag_phrases), nrow = nrow(artists)))
names(tag_df) <- tag_phrases
head(tag_df)

arttag <- cbind(artists[, c("name", "tags")], tag_df)
str(arttag)

for (row in 1:nrow(arttag)) {
  tags <- unlist(strsplit(arttag[row, "tags"], ", "))
  # clean_tags <- c()
  for (tag in tags) {
    clean_tag <- unlist(lapply(tag, tolower))
    if (clean_tag %in% tag_phrases) {
      # clean_tags <- c(clean_tags, clean_tag)
      arttag[row, clean_tag] <- 1
    }
  }
}

arttag <- merge(arttag, scrobles %>% group_by(artist) %>% count(), by.x="name", by.y="artist")
for (row in 1:nrow(arttag)){
  for (col in tag_phrases) {
    arttag[row, col] <- arttag[row, col] * arttag[row, "n.x"]
  }
}
head(arttag[, c("name", "n.x", "rock")], 50)

rescale <- function(x) (x-min(x))/(max(x) - min(x)) * 100
arttag %>% mutate_if(is.numeric, rescale)
max(rescale(arttag$rock))
max(arttag[, "funk"])
for (col in tag_phrases) {
  arttag[, col] <- rescale(arttag[, col])
}
summary(arttag[c("rock", "metal", "funk", "alternative")])

tag_scores <- unname(colSums(arttag[sapply(arttag, is.numeric)]))
tag_score_df <- data.frame(tag_phrases, tag_scores[1:305])
names(tag_score_df) <- c("tag_phrases", "score")
tag_score_df[with(tag_score_df, order(-score)), ]
head(tag_score_df)
ggplot(tag_score_df, aes(x=reorder(tag_phrases, -score), y=score)) + 
  geom_col() + 
  theme(text = element_text(size=5, family="HiraKakuProN-W3"), axis.text.x = element_text(angle = 90, hjust=1)) 
