getTwitter <- function(youtube.video) {
  library(twitteR);
  #cred <- OAuthFactory$new(consumerKey='hrZLkOR4WD4o1vMoaVjA',
  #                         consumerSecret='Xjfdyfv8N7Cva6KngyQY4mgm8SouqbVCBEiPbePrt80',
  #                         requestURL='https://api.twitter.com/oauth/request_token',
  #                         accessURL='http://api.twitter.com/oauth/access_token',
  #                         authURL='http://api.twitter.com/oauth/authorize');
  
  #cred <- OAuthFactory$new(consumerKey='hrZLkOR4WD4o1vMoaVjA',
  #                         consumerSecret='Xjfdyfv8N7Cva6KngyQY4mgm8SouqbVCBEiPbePrt80',
  #                         requestURL='https://api.twitter.com/oauth/request_token',
  #                         accessURL='http://api.twitter.com/oauth/access_token',
  #                         authURL='http://api.twitter.com/oauth/authorize');
  
  #cred$handshake();
  #cred <- readRDS('~/R/r-youtube-ranking/tcred.RData');
  #registerTwitterOAuth(cred);
  
  youtube.video <- 'nsUEd2cUIqo';
  raw.twitter <- searchTwitter(youtube.video, n=1500, lang='en');
  df.raw.twitter <- data.frame(
    tweet = sapply(raw.twitter, function(x) x$getText()),
    tweep = sapply(raw.twitter, function(x) x$getScreenName())
    );
  df.raw.twitter$tweet.clean <- gsub("[[:punct:]]", "", df.raw.twitter$tweet)
  df.raw.twitter$tweet.cleaner <- gsub("[^[:alnum:]]", " ", df.raw.twitter$tweet)
  df.raw.twitter$polarity <- polarity(df.raw.twitter$tweet.cleaner)$all$polarity;
  
  df.raw.twitter$tweep.followers <- getUser(df.raw.twitter$tweep)$followersCount;
  df.raw.twitter$tweep.friends <- getUser(df.raw.twitter$tweep)$friendsCount;
  
  # attach user info
  df.raw.twitter$tweep.followers <- 0;
  df.raw.twitter$tweep.friends <- 0;
  for (i in 1:nrow(df.raw.twitter)) {
    user <- getUser(df.raw.twitter[i,]$tweep);
    message(user$followersCount);
    #message(df.raw.twitter[i,]$tweep.followers)
    df.raw.twitter[i,]$tweep.followers <- user$followersCount;
    df.raw.twitter[i,]$tweep.friends <- user$friendsCount;
  }
  
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