library(dplyr)
library(Rspotify)
library(plotly)
library(ggplot2)
library(tidyr)
library(pastecs)
library(grid)

# Set tokens
id <- "14618d70712e4bf4bdf0c8f00ba9538e"
secret <- "bf8b56a00fc94036b40df7545578c05b"

token <- spotifyOAuth("test", id, secret)

theme_set(theme_bw())


mex50 <- getPlaylistSongs("spotify", "37i9dQZF1DX92MLsP3K1fI", offset = 0, token)

# Get features
data <- data.frame(id = character(0), max1 = numeric(0), max2 = numeric(0))

for(i in 1:nrow(mex50)) { 
  temp <- getFeatures(mex50$id[i], token)
  data <- rbind(data, temp)
}

playlist_features <- inner_join(mex50, data, by = 'id')

ggplot(data = playlist_features,
       aes(x = danceability, y = popularity))+
  geom_point() +
  geom_smooth()+
  labs(title=paste("Spotify Top 50 in Mexico"), 
       subtitle="Song danceability vs popularity",
       x = "Danceability", y = "Popularity")

ggplot(data = playlist_features,
       aes(x = energy, y = popularity))+
  geom_point() +
  geom_smooth()+
  labs(title=paste("Spotify Top 50 in Mexico"), 
       subtitle="Song energy vs popularity",
       x = "Energy", y = "Popularity")

ggplot(data = playlist_features,
       aes(x = speechiness, y = popularity))+
  geom_point() +
  geom_smooth()+
  labs(title=paste("Spotify Top 50 in Mexico"), 
       subtitle="Song speechiness vs popularity",
       x = "Speechiness", y = "Popularity")

model <- lm(popularity ~ danceability + energy + speechiness + acousticness + 
              instrumentalness + liveness + valence + tempo + duration_ms, 
            data = playlist_features)

summary(model)

library(car)
Anova(model)


ggplot(data = playlist_features,
       aes(x = energy, y = danceability))+
  geom_point() +
  geom_smooth()+
  labs(title=paste("Spotify Top 50 in Mexico"), 
       subtitle="Song energy vs danceability",
       x = "Energy", y = "Danceability")


ggplot(data = playlist_features,
       aes(x = energy, y = danceability))+
  geom_point() +
  geom_smooth()+
  labs(title=paste("Spotify Top 50 in Mexico"), 
       subtitle="Song danceability vs valence",
       x = "Danceability", y = "Valence")
