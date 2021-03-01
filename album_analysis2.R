library(dplyr)
library(Rspotify)
library(plotly)
library(ggplot2)
library(tidyr)
library(pastecs)
library(qwraps2)
library(stringr)

#----- 
# Set tokens
id <- "14618d70712e4bf4bdf0c8f00ba9538e"
secret <- "bf8b56a00fc94036b40df7545578c05b"

token <- spotifyOAuth("test", id, secret)

theme_set(theme_bw())

#----- 
# Get data
# Get album
album_id <- "2ANVost0y2y52ema1E9xAZ" # MODIFICAR SOLO ESTA LINEA
album <- getAlbum(album_id, token)
album$id <- as.character(album$id)

drops <- c("duration_ms", "available_markets", "preview_url")
album <- album[ , !(names(album) %in% drops)]

# Get features
data <- data.frame(id = character(0), max1 = numeric(0), max2 = numeric(0))

#-----
# Join tables
for(i in 1:nrow(album)) { 
  temp <- Rspotify::getFeatures(album$id[i], token)
  data <- rbind(data, temp)
}

# Join album and features
album_features <- inner_join(album, data, by = 'id')

# Add Key as characters
key <- c(0,1,2,3,4,5,6,7,8,9,10,11)
key <- as.numeric(key)
keyChar <- c("C","C#","D","D#","E","F","F#","G","G#","A","A#","B")

key_key <- data.frame(cbind(key, keyChar))
key_key$key <- as.numeric(key_key$key)

album_features <- inner_join(album_features, key_key, by = 'key')

# ----- 
# Set titles
album_info <- getAlbumInfo(album_id, token)
album_title <- album_info$name
artist <- album_info$artist


#----- Plots ------
# Get key frequencies
album_features$keyChar <- as.factor(album_features$keyChar) 

p_key <- ggplot(data = album_features,
       aes(y = keyChar, fill = as.factor(mode)))+
  geom_bar(stat = "count", color = "black") +
  labs(title=paste(album_title, "by", artist), 
       subtitle="Song count by key and mode",
       x = "Song Count", y = "Key") +
  scale_fill_discrete(name = "Mode", labels = c("minor", "major"))


# Plot album features
album_long <- gather(album_features, variable, value, danceability:valence, factor_key=TRUE)

p_features <- album_long %>% filter(variable != "loudness" & variable != "key" & variable != "mode") %>%
ggplot(aes(x = variable, y = value))+
  geom_boxplot() +
  labs(title=paste(album_title, "by", artist), 
       subtitle="Album features distribution",
       x = "Features", y = "Value")


# Key/mode vs valence
p_modes <- ggplot(album_features)+
  geom_boxplot(aes(x = as.factor(mode), y = valence, fill = keyChar)) +
  labs(title=paste(album_title, "by", artist), 
       subtitle="Key and mode distribution",
       x = "Mode", y = "Valence")


# Song Valence
album_features$name <- factor(album_features$name, levels = album_features$name)  # convert to factor to retain sorted order in plot.


p_valence <- ggplot(album_features, aes(x= `name`, y= as.numeric(valence), label= as.numeric(valence))) + 
  geom_point(stat='identity', fill="black", size=10)  +
  geom_segment(aes(y = 0, 
                   x = `name`, 
                   yend = as.numeric(valence), 
                   xend = `name`), 
               color = "black") +
  geom_text(color="white", size=2.5) +
  labs(title=paste(album_title, "by", artist), 
       subtitle="Song valence (0: negative / 1: positive)",
       x = "Song name", y = "Song valence") + 
  ylim(0, 1) +
  coord_flip()


# Song energy
p_energy <- ggplot(album_features, aes(x= `name`, y= as.numeric(energy), label= as.numeric(energy))) + 
  geom_point(stat='identity', fill="black", size=10)  +
  geom_segment(aes(y = 0, 
                   x = `name`, 
                   yend = as.numeric(energy), 
                   xend = `name`), 
               color = "black") +
  geom_text(color="white", size=2.5) +
  labs(title=paste(album_title, "by", artist), 
       subtitle="Song energy",
       x = "Song name", y = "Song energy") + 
  ylim(0, 1) +
  coord_flip()

# Song danceability
p_dance <- ggplot(album_features, aes(x= `name`, y= as.numeric(danceability), label= as.numeric(danceability))) + 
  geom_point(stat='identity', fill="black", size=10)  +
  geom_segment(aes(y = 0, 
                   x = `name`, 
                   yend = as.numeric(danceability), 
                   xend = `name`), 
               color = "black") +
  geom_text(color="white", size=2.5) +
  labs(title=paste(album_title, "by", artist), 
       subtitle="Song danceability",
       x = "Song name", y = "Song danceability") + 
  ylim(0, 1) +
  coord_flip()

# Song speechiness
p_speech <- ggplot(album_features, aes(x= `name`, y= as.numeric(speechiness), label= as.numeric(speechiness))) + 
  geom_point(stat='identity', fill="black", size=10)  +
  geom_segment(aes(y = 0, 
                   x = `name`, 
                   yend = as.numeric(speechiness), 
                   xend = `name`), 
               color = "black") +
  geom_text(color="white", size=2.5) +
  labs(title=paste(album_title, "by", artist), 
       subtitle="Song speechiness",
       x = "Song name", y = "Song speechiness") + 
  ylim(0, 1) +
  coord_flip()

# Song acousticness
p_acoustic <- ggplot(album_features, aes(x= `name`, y= as.numeric(acousticness), label= as.numeric(acousticness))) + 
  geom_point(stat='identity', fill="black", size=10)  +
  geom_segment(aes(y = 0, 
                   x = `name`, 
                   yend = as.numeric(acousticness), 
                   xend = `name`), 
               color = "black") +
  geom_text(color="white", size=2.5) +
  labs(title=paste(album_title, "by", artist), 
       subtitle="Song acousticness",
       x = "Song name", y = "Song acousticness") + 
  ylim(0, 1) +
  coord_flip()

# Song acousticness
p_instrument <- ggplot(album_features, aes(x= `name`, y= as.numeric(instrumentalness), label= as.numeric(instrumentalness))) + 
  geom_point(stat='identity', fill="black", size=10)  +
  geom_segment(aes(y = 0, 
                   x = `name`, 
                   yend = as.numeric(instrumentalness), 
                   xend = `name`), 
               color = "black") +
  geom_text(color="white", size=2.5) +
  labs(title=paste(album_title, "by", artist), 
       subtitle="Song instrumentalness",
       x = "Song name", y = "Song instrumentalness") + 
  ylim(0, 1) +
  coord_flip()

#----- 
# Basic Stats
features <- album_features[ ,5:16]
drops_feat <- c("key", "mode")

features <- features[ , !(names(features) %in% drops_feat)]

results <- data.frame(sapply(features, median))

res <- stat.desc(features)
res <- round(res, 3)

med_val <- results[8,]
med_val

# Tempo

name <- album_features$name
time_s <- album_features$time_signature

df <- data.frame(name, time_s)

# -----
# Popularity model (pendiente)

# Get features
pop <- data.frame(id = character(0), max1 = numeric(0), max2 = numeric(0))

#-----
# Join tables

searchTrackid<-function(id,token){
  req <- httr::GET(paste0("https://api.spotify.com/v1/tracks/", id), httr::config(token = token))
  json1 <- httr::content(req)
  x<-json1
  id = x$id
  display_name = x$name
  popularity = x$popularity
  res <- data.frame(id, display_name, popularity, stringsAsFactors = F)
  return(res)
}

for(i in 1:nrow(album_features)) { 
  temp <- searchTrackid(album_features$id[i], token)
  pop <- rbind(pop, temp)
}

album_pop <- inner_join(album_features, pop, by = 'id')

searchTrackid<-function(id,token){
  req <- httr::GET(paste0("https://api.spotify.com/v1/tracks/", id), httr::config(token = token))
  json1 <- httr::content(req)
  x<-json1
  id = x$id
  display_name = x$name
  popularity = x$popularity
  res <- data.frame(id, display_name, popularity, stringsAsFactors = F)
  return(res)
}


#-----
# Popularity plots

p_popen <- ggplot(data = album_pop,
       aes(x = energy, y = popularity))+
  geom_point() +
  geom_smooth()+
  labs(title=paste(album_title, "by", artist), 
       subtitle="Song energy vs popularity",
       x = "Energy", y = "Popularity")

p_popdan <- ggplot(data = album_pop,
       aes(x = danceability, y =   popularity))+
  geom_point() +
  geom_smooth()+
  labs(title=paste(album_title, "by", artist), 
       subtitle="Song danceability vs popularity",
       x = "Danceability", y = "Popularity")

rownames(features) <- album_features$name

# Heatmap

heat <- as.matrix(features)

#------
# Produce PDF

pdf(str_replace_all(paste0(sprintf("%s", album_info$name), ".pdf"), ":",""), 
    width = 15, height = 8, onefile=TRUE)
print(p_key)
print(p_modes)
print(p_features)
heatmap(as.matrix(features), Colv = NA, Rowv = NA, scale = "column", margins = c(10,14),
        main = paste(album_title, "by", artist, "feature comparison"), xlab = "Features",
        ylab = "Song name")
print(p_dance) 
print(p_energy)
print(p_speech)
print(p_acoustic)
print(p_instrument)
print(p_valence)
print(p_popen)
print(p_popdan)
dev.off()




library(corrplot)
res <- cor(features)
res
corrplot(res, type = "upper", order = "hclust", 
         tl.col = "black", tl.srt = 45)

