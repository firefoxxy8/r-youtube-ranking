# case: http://www.youtube.com/watch?v=xwndLOKQTDs

library(RJSONIO);

library(RCurl);

library(qdap);

youtube.video <- "lUPMjC9mq5Y";
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
          polarity = polarity(mat[i,][[1]]$content[[1]])$all[4][[1]]);
        rbind(df.comments, entry) -> df.comments
      }
    }, error = function(e){})
  }
}

df$youtube.sentiment <- mean(df.comments$polarity)

#mat <- matrix(bop$feed$entry);
#df.comments <- NULL;
#for (i in 1:length(mat)) {
#  entry <- data.frame(comment = mat[i,][[1]]$content[[1]]);
#  rbind(df.comments, entry) -> df.comments
#}

#matrix(bop$feed$entry)[,1])

#test <- data.frame(
#    "author" = 
#    )
#df.comments <- data.frame("comment.id" = "1", 
#                          "comment.published" = "", 
#                          "comment.author" = "", 
#                          "comment.content" = "")

