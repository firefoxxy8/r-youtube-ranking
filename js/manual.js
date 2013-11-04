jQuery.extend({
    stringify  : function stringify(obj) {
        var t = typeof (obj);
        if (t != "object" || obj === null) {
            // simple data type
            if (t == "string") obj = '"' + obj + '"';
            return String(obj);
        } else {
            // recurse array or object
            var n, v, json = [], arr = (obj && obj.constructor == Array);
 
            for (n in obj) {
                v = obj[n];
                t = typeof(v);
                if (obj.hasOwnProperty(n)) {
                    if (t == "string") v = '"' + v + '"'; else if (t == "object" && v !== null) v = jQuery.stringify(v);
                    json.push((arr ? "" : '"' + n + '":') + String(v));
                }
            }
            return (arr ? "[" : "{") + String(json) + (arr ? "]" : "}");
        }
    }
});

var getTweets = function() {
	window.tweets = [];
	$('ol#stream-items-id li.stream-item').each(function(i, v) {
		window.tweets.push({
			tweet : $(this).find('.tweet-text').text().replace(/<(?:.|\n)*?>/gm, '').replace(/\W/g, ' '),//.replace(/"/g, '\'').replace(/[^[:alnum:]]/g, 'x')
			screenName: $(this).find('.original-tweet').data('screen-name'),
			time: $(this).find('.original-tweet .time ._timestamp').data('time')
		})
	});

	var raw = $.stringify(window.tweets);
	return raw;
}

var download = function(raw) {
	name = document.location.search.match(/\?q=(.+)%20/)[1];

	window.requestFileSystem = window.requestFileSystem || window.webkitRequestFileSystem;

	window.requestFileSystem(window.TEMPORARY, 1024*1024, function(fs) {
		fs.root.getFile(name + '.json', {create: true}, function(fileEntry) {
			fileEntry.createWriter(function(fileWriter) {
	        	

	        	var blob = new Blob([raw], {type: 'application/json'});

	        	fileWriter.addEventListener("writeend", function() {
	            	// navigate to file, will download
	            	location.href = fileEntry.toURL();
	        	}, false);

	        	fileWriter.write(blob);
	    	}, function() {});
		}, function() {});
	}, function() {});
}

download(getTweets());