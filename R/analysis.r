source('~/R/r-youtube-ranking/R/getDataFromGoogle.r');
source('~/R/r-youtube-ranking/R/getYoutube.r');
source('~/R/r-youtube-ranking/R/getTwitter.r');
source('~/R/r-youtube-ranking/R/attachTwitter.r');
data <- getData();
youtube.data <- NULL;

for (i in 1:nrow(data)) {
  youtube <- data[i,]['youtube'][[1]];
  message(paste('trying: ', youtube, sep=''));
  append.with <- getYoutube(youtube);
  rbind(youtube.data, append.with) -> youtube.data;
}

ext.data <- merge(data, youtube.data, by = 'youtube', all.x = TRUE);

# attach twitter
raw.tweets <- NULL;
for (i in 1:nrow(data)) {
  youtube <- as.character(data[i,]['youtube'][[1]]);
  append.with <- getTwitterFromLocal(youtube);
  rbind(raw.tweets, append.with) -> raw.tweets;
}

# remove duplicates based on tweet AND screenName
raw.tweets.unique <- raw.tweets[!duplicated(raw.tweets[1:2]),];

# build data frame with tweep info
tweeps <- data.frame(
  screen.name = unique(raw.tweets.unique$screenName),
  followers = NA,
  friends = NA
  );

#total <- nrow(tweeps);

#for (n in 201:300) {
#  t.object <- getUser(tweeps$screen.name[n]);
#  followers <- t.object$followersCount;
#  friends <- t.object$friendsCount;
#  tweeps$followers[n] <- followers
#  tweeps$friends[n] <- friends
#  message(paste(tweeps$screen.name[n], ' has followers: ', followers, ' and friends: ', friends, sep=''));
#}

# attach followers and friends to tweeps 
batch <- 20;
runs <- ceiling(nrow(tweeps) / batch);
#t.all <- NULL;
for (i in 775:(runs-1)) {
  start <- (batch*i)+1;
  message(paste('trying from: ', start, ' batch: ', (start + batch) ,sep=''));
  t.users <- lookupUsers(tweeps$screen.name[start:(start + batch)]);
  message(paste('acquired: ', length(t.users), sep=''));
  t.df <- NULL;
  tt <- 1;
  for (tt in 1:batch) {
    append.with <- t.users[[tt]]$toDataFrame();
    rbind(t.df, append.with) -> t.df;
  }
  rbind(t.all, t.df) -> t.all;
  Sys.sleep(1 * runif(1, 1.0, 3.0));
}


# attach sentiment to raw.tweets.unique - works
raw.tweets.unique$sentiment <- polarity(raw.tweets.unique$tweet)$all$polarity;

# attach twitter user metrics to raw.tweets.unique
#raw.tweets.unique$followers <- t.all$followersCount[t.all$screenName == raw.tweets.unique$screenName];
quantify.sentiment <- data.frame(
  screenName = t.all$screenName,
  statuses = t.all$statusesCount,
  favorites = t.all$favoritesCount,
  friends = t.all$friendsCount,
  followers = t.all$followersCount,
  listed = t.all$listedCount,
  ff.score = (t.all$followersCount/(t.all$friendsCount + 1))
  );

quantify.sentiment$ff.index <- ((quantify.sentiment$ff.score) * 100) / max(quantify.sentiment$ff.score);
quantify.sentiment$l.index <- ((quantify.sentiment$listed) * 100) / max(quantify.sentiment$listed);
quantify.sentiment$index <- ((quantify.sentiment$ff.index + quantify.sentiment$l.index) * 100) / max(quantify.sentiment$ff.index + quantify.sentiment$l.index)

#twp <- lm(statuses ~ index, data = quantify.sentiment);
raw.tweets.unique$sentiment <- raw.tweets.unique$sentiment + 1;
tweet.score <- merge(raw.tweets.unique, quantify.sentiment, by = 'screenName', all.x = TRUE);
tweet.score$total <- tweet.score$sentiment * tweet.score$followers * tweet.score$index;

# attach average sentiment to ext.data - works
save(ext.data, file="~/R/r-youtube-ranking/df/ext.data.Rda");
#save(t.all, file="~/R/r-youtube-ranking/df/t.all.Rda");
for (i in 1:nrow(ext.data)) {
  youtube <- as.character(ext.data[i,]['youtube'][[1]]);
  subset <- raw.tweets.unique[raw.tweets.unique$youtube == youtube,];
  total <- tweet.score$total[tweet.score$youtube == youtube];
  ext.data$twitter.sentiment[i] <- mean(subset$sentiment);
  ext.data$twitter.count[i] <- length(subset$sentiment);
  ext.data$twitter.q[i] <- mean(total, na.rm=TRUE);
}

#raw.tweets.unique$followers <- getUser(raw.tweets.unique$screenName)$followersCount

#twitter.data <- data.frame(youtube = data$youtube);
#twitter.data$twitter.sentiment <- 0;
#twitter.data$test <- 'lol';
#twitter.data$twitter.sentiment <- getTwitter(as.character(twitter.data$test));
#for (i in 1:nrow(twitter.data)) {
  #youtube <- as.character(data[i,]['youtube'][[1]]);
  #message(getTwitter(paste('lol ', youtube, sep='')));
#  twitter.data[i,]['twitter.sentiment'] <- getTwitter(youtube);
#}


ext.data$profit <- ext.data$gross - ext.data$budget;
ext.data$score <- log(ext.data$youtube.views + ext.data$youtube.likes - ext.data$youtube.dislikes + ext.data$youtube.comments + ext.data$youtube.sentiment)
ext.data$index <- ((ext.data$score) * 100) / max(ext.data$score);
ext.data$twitter.index <- ((ext.data$twitter.q) * 100) / max(ext.data$twitter.q);
ext.data$t.score <- (ext.data$twitter.index + ext.data$index) / 2;


model <- lm(gross ~ t.score + budget, data = ext.data);

require(ggplot2)

ggplot(ext.data, aes(x = gross, y = log(t.score))) + geom_point() +
  stat_smooth(method = 'lm', aes(colour = 'linear'), se = FALSE) +
  stat_smooth(method = 'lm', formula = y ~ poly(x,2), aes(colour = 'polynomial'), se= FALSE) +
  stat_smooth(method = 'nls', formula = y ~ a * log(x) +b, aes(colour = 'logarithmic'), se = FALSE, start = list(a=1,b=1)) +
  theme_bw() +
  scale_colour_brewer(name = 'Trendline', palette = 'Set2')

# trying with imdbrate metascore

data.martin <- getData()[c('youtube', 'imdb.rating', 'meta.score')];
ext.martin <- merge(ext.data, data.martin, by = 'youtube', all.x = TRUE);
model <- lm(gross ~ t.score + meta.score + budget, data = ext.martin);


#model <- lm(log(gross) ~ youtube.likes + youtube.views + youtube.dislikes + youtube.comments*youtube.sentiment + log(budget), data = ext.data);