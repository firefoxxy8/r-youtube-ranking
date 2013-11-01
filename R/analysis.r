source('~/R/r-youtube-ranking/R/getDataFromGoogle.r');
source('~/R/r-youtube-ranking/R/getYoutube.r');
data <- getData();
youtube.data <- NULL;

for (i in 1:nrow(data)) {
  youtube <- data[i,]['youtube'][[1]];
  message(paste('trying: ', youtube, sep=''));
  append.with <- getYoutube(youtube);
  rbind(youtube.data, append.with) -> youtube.data;
}

ext.data <- merge(data, youtube.data, by='youtube', all.x=TRUE);