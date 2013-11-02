source('~/R/r-youtube-ranking/R/getDataFromGoogle.r');
source('~/R/r-youtube-ranking/R/getYoutube.r');
source('~/R/r-youtube-ranking/R/getTwitter.r');
data <- getData();
youtube.data <- NULL;

for (i in 1:nrow(data)) {
  youtube <- data[i,]['youtube'][[1]];
  message(paste('trying: ', youtube, sep=''));
  append.with <- getYoutube(youtube);
  rbind(youtube.data, append.with) -> youtube.data;
}

ext.data <- merge(data, youtube.data, by = 'youtube', all.x = TRUE);

twitter.data <- data.frame(youtube = data$youtube);
twitter.data$twitter.sentiment <- 0;
#twitter.data$test <- 'lol';
#twitter.data$twitter.sentiment <- getTwitter(as.character(twitter.data$test));
for (i in 1:nrow(twitter.data)) {
  youtube <- as.character(data[i,]['youtube'][[1]]);
  #message(getTwitter(paste('lol ', youtube, sep='')));
  twitter.data[i,]['twitter.sentiment'] <- getTwitter(youtube);
}


ext.data$profit <- ext.data$gross - ext.data$budget;
ext.data$score <- log(ext.data$youtube.views + ext.data$youtube.likes - ext.data$youtube.dislikes + ext.data$youtube.comments + ext.data$youtube.sentiment)
ext.data$index <- ((ext.data$score) * 100) / max(ext.data$score);

model <- lm(gross ~ score + budget, data = ext.data);

require(ggplot2)

ggplot(ext.data, aes(x = gross, y = score)) + geom_point() +
  stat_smooth(method = 'lm', aes(colour = 'linear'), se = FALSE) +
  stat_smooth(method = 'lm', formula = y ~ poly(x,2), aes(colour = 'polynomial'), se= FALSE) +
  stat_smooth(method = 'nls', formula = y ~ a * log(x) +b, aes(colour = 'logarithmic'), se = FALSE, start = list(a=1,b=1)) +
  theme_bw() +
  scale_colour_brewer(name = 'Trendline', palette = 'Set2')

#model <- lm(log(gross) ~ youtube.likes + youtube.views + youtube.dislikes + youtube.comments*youtube.sentiment + log(budget), data = ext.data);