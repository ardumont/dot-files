//https://raw.githubusercontent.com/ardumont/dot-files/master/.surfingkeys.js

// follow link in current tab
map('d', 'f'); unmap('f');
// follow link in a new tab (do not open the tab yet)
map('f', 'gf'); unmap('gf');
// pin/unpin tab
map('<Alt-p>', '<Alt-j>'); unmap('<Alt-p>');

// interactively switch tab
map('T', 'xb'); unmap('T');
// deactivate close tab
unmap('x');
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

// deactivate surfingkeys by default on those sites
settings.blacklist = {
    "https://github.com": 1,
    "https://mail.google.com/mail": 1,
}
