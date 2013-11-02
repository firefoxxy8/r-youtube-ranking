getTwitter <- function(youtube.video) {
  library(twitteR);
  #cred <- OAuthFactory$new(consumerKey='hrZLkOR4WD4o1vMoaVjA',
  #                         consumerSecret='Xjfdyfv8N7Cva6KngyQY4mgm8SouqbVCBEiPbePrt80',
  #                         requestURL='https://api.twitter.com/oauth/request_token',
  #                         accessURL='http://api.twitter.com/oauth/access_token',
  #                         authURL='http://api.twitter.com/oauth/authorize');
  #cred$handshake();
  #cred <- readRDS('~/R/r-youtube-ranking/tcred.RData');
  #registerTwitterOAuth(cred);
  
  youtube.video <- 'lol';
  raw.twitter <- searchTwitter(youtube.video, n=100, lang='en', since="2011-01-01", until="2012-12-31");
  df.raw.twitter <- data.frame(tweet = sapply(raw.twitter, function(x) x$getText()));
  df.raw.twitter$polarity <- 0;
  
  for (i in 1:nrow(df.raw.twitter)) {
    tryCatch({
      df.raw.twitter[i,]$polarity <- polarity(df.raw.twitter[i,][['tweet']])$all[4][[1]];
    }, error = function(e){
      message(':(')
    })
  }
  
  tryCatch({
    return(mean(df.raw.twitter$polarity[df.raw.twitter$polarity != 0]));
  }, error = function(e){
    return(NA);
  })
}