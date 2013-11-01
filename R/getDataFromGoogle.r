require(RCurl);
url <- 'https://docs.google.com/spreadsheet/pub?key=0AqLRdLacgu1ydHpabktjcG1ydlQzZjdlWHNjdkhyMHc&single=true&gid=0&output=csv';
csv <- getURL(url);
data <- read.csv(textConnection(csv))