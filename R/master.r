# case: http://www.youtube.com/watch?v=xwndLOKQTDs

library(RJSONIO);

library(RCurl);

library(qdap);

library(twitteR);
options(RCurlOptions = list(verbose = FALSE, capath = system.file("CurlSSL", "cacert.pem", package = "RCurl"), ssl.verifypeer = FALSE))

youtube.video <- "xwndLOKQTDs";
youtube.key <- "AIzaSyA8_l-irwC9QoXgymfM_N4WQuXBWCGX9bc";

youtube.url <- paste("https://www.googleapis.com/youtube/v3/videos?id=", youtube.video, "&key=", youtube.key, "&fields=items(id,snippet(channelId,title,categoryId),statistics)&part=snippet,statistics", sep="")
youtube.json <- getURL(youtube.url);
youtube.json <- RJSONIO::fromJSON(youtube.json);
  
stats <- youtube.json$items[[1]]$statistics;
snippet <- youtube.json$items[[1]]$snippet;
df <- data.frame("video" = youtube.video, 
           "youtube.title" = snippet[["title"]],
           "youtube.channel" = snippet[["channelId"]],
           "youtube.category" = snippet[["categoryId"]],
           "youtube.views" = as.integer(stats[["viewCount"]]),
           "youtube.likes" = as.integer(stats[["likeCount"]]),
           "youtube.dislikes" = as.integer(stats[["dislikeCount"]]),
           "youtube.comments" = as.integer(stats[["commentCount"]]))


# comments
# https://gdata.youtube.com/feeds/api/videos/xwndLOKQTDs/comments?v=2&alt=json&max-results=50

max <- df$youtube.comments
batch <- 50
runs <- ceiling(max/batch)
df.comments <- NULL;

for (i in 0:(runs-1)) {
  start.index <- i * batch + 1;
  if (start.index <= 1000) {
    url <- paste("https://gdata.youtube.com/feeds/api/videos/",
                 youtube.video,
                 "/comments?v=2&alt=json&max-results=",
                 batch,
                 "&start-index=",
                 start.index, sep="");
    
    message(paste('trying: ', url));
    data.url <- getURL(url);
    tryCatch({
      data <- RJSONIO::fromJSON(data.url);
      mat <- matrix(data$feed$entry);
      for (i in 1:length(mat)) {
        entry <- data.frame(
          comment = mat[i,][[1]]$content[[1]],
          polarity = polarity(
            text.var = mat[i,][[1]]$content[[1]])$all[4][[1]]
          );
        
        rbind(df.comments, entry) -> df.comments
      }
    }, error = function(e){})
  }
}

hist(df.comments$polarity);
df$youtube.sentiment <- mean(df.comments$polarity);

# Twitter stuff

#cred <- OAuthFactory$new(consumerKey='hrZLkOR4WD4o1vMoaVjA',
#                         consumerSecret='Xjfdyfv8N7Cva6KngyQY4mgm8SouqbVCBEiPbePrt80',
#                         requestURL='https://api.twitter.com/oauth/request_token',
#                         accessURL='http://api.twitter.com/oauth/access_token',
#                         authURL='http://api.twitter.com/oauth/authorize');

#cred <- readRDS('tcred.RData');
#cred$handshake();
#registerTwitterOAuth(cred);

raw.twitter <- searchTwitter(youtube.video, n=1000, lang="da", retryOnRateLimit=4);
df.raw.twitter <- data.frame(tweet = sapply(raw.twitter, function(x) x$getText()));
df.raw.twitter$polarity <- 0;
for (i in 1:nrow(df.raw.twitter)) {
  tryCatch({
    df.raw.twitter[i,]$polarity <- polarity(df.raw.twitter[i,][['tweet']])$all[4][[1]];
  }, error = function(e){
    message(':(');
  })
}

df$twitter.sentiment <- mean(df.raw.twitter$polarity);

#facebook
# CAACEdEose0cBAKjpDrBcySEajRLfsKX2SEyLTUBAQCxlfc9hkNTnKNblcjowBkCjepLMNL85nAo9DfigZCrB3fZCF7lQkeJeHlH7L4j3ZCMiyXAdtOBwlW61wypJqsNWunZCgqcpKu0ZCUbkl2nGCTQ6WSzrGh3fiWxVjZB1pafNAohXNqZChNVK0QYwKSeS6d4RpGbXAHpmAZDZD
facebook.url <- paste('https://graph.facebook.com/search?q=',
             youtube.video,
             '&type=post&access_token=CAACEdEose0cBAKjpDrBcySEajRLfsKX2SEyLTUBAQCxlfc9hkNTnKNblcjowBkCjepLMNL85nAo9DfigZCrB3fZCF7lQkeJeHlH7L4j3ZCMiyXAdtOBwlW61wypJqsNWunZCgqcpKu0ZCUbkl2nGCTQ6WSzrGh3fiWxVjZB1pafNAohXNqZChNVK0QYwKSeS6d4RpGbXAHpmAZDZD',
             sep = '');

fb <- getURL(facebook.url);
fb <- fromJSON(fb);