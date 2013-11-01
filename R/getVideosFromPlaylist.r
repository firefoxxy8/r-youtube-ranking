library(RJSONIO);
# example: PLEDC57AE8C75E6393
getVideosFromPlaylist <- function(playlist) {
  url <- paste('http://gdata.youtube.com/feeds/api/playlists/',
               playlist,
               '?v=2&alt=json&max-results=50&start-index=1', 
               sep='');
  data.url <- getURL(url);
  data.raw <- RJSONIO::fromJSON(data.url);
  data <- data.raw$feed$entry;
  out.raw <<- data.raw;
  output <- NULL;
  total <- data.raw[3]$feed[17]$`openSearch$totalResults`[[1]];
  
  message(paste('Fetching ', total ,' videos from "', data.raw[3]$feed[11]$title[[1]], '" by "', data.raw[3]$feed[15]$author[[1]]$name[[1]], '"', sep=''));
  
  for (i in 1:length(data)) {
    dislikes <- data[i][1][[1]]$`yt$rating`[1][[1]];
    likes <- data[i][1][[1]]$`yt$rating`[2][[1]];
    message(paste('dislikes: ', dislikes, ' likes: ', likes, sep=''));
    
    entry <- data.frame(
      videoID = data[[i]]$`media$group`$`yt$videoid`[[1]],
      title = data[[i]]$title[[1]],
      views = data[i][1][[1]]$`yt$statistics`[2][[1]],
      likes = likes,
      dislikes = dislikes
    );
    
    rbind(output, entry) -> output;
  }
  return(output);
}

test <- getVideosFromPlaylist('PLVfin74Qx3tV8bgAhzbfDpnfPoGmJWAcn')