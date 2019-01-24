// Search settings
addSearchAliasX('D', 'ddgH', 'https://duckduckgo.com/html/?q=', 's', 'https://duckduckgo.com/ac/?q=', function(response) {
    var res = JSON.parse(response.text);
    return res.map(function(r){
        return r.phrase;
    });
});
settings.defaultSearchEngine = 'D';

// Hints settings
Hints.characters = "asdfgqwertzxcvb";
Hints.scrollKeys = "";
settings.hintAlign = "left";
settings.smoothScroll = false;
