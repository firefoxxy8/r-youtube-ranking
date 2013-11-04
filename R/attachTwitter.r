require(RJSONIO);
require(plyr);

youtube.video <- '0UPckModN14';
getTwitterFromLocal <- function(youtube.video) {
  json.file <- paste('~/R/r-youtube-ranking/data/', youtube.video ,'.json', sep='');
  json.data <- RJSONIO::fromJSON(paste(readLines(json.file), collapse=""));
  
  json.data <- do.call("rbind.fill", lapply(json.data, as.data.frame));
  json.data$youtube <- youtube.video;
  return(json.data);
}