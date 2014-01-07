// Info

let PLUGIN_INFO =
<KeySnailPlugin>
    <name>History</name>
    <description>History from KeySnail</description>
    <description lang="ja">履歴を検索</description>
    <iconURL>https://sites.google.com/site/958site/Home/files/history.ks.png</iconURL>
    <updateURL>https://gist.github.com/958/895953/raw/history.ks.js</updateURL>
    <author>958</author>
    <version>0.0.3</version>
    <license>MIT</license>
    <minVersion>1.8.0</minVersion>
    <include>main</include>
    <detail lang="ja"><![CDATA[
=== 使い方 ===
履歴を表示・検索します

キーマップを変更したい人は、次のような設定を .keysnail.js の PRESERVE エリアへ

>||
plugins.options["hitsory.keymap"] = {
    "C-z"   : "prompt-toggle-edit-mode",
    //"SPC"   : "prompt-next-page",
    //"b"     : "prompt-previous-page",
    //"j"     : "prompt-next-completion",
    //"k"     : "prompt-previous-completion",
    //"g"     : "prompt-beginning-of-candidates",
    //"G"     : "prompt-end-of-candidates",
    //"q"     : "prompt-cancel",
    // history specific actions
    "/"     : "search-all-histories",
    //"o"     : "open"
};
||<

コマンドを拡張して使うことも

>||
shell.add(["history"], "search history",

  function(args, extra) {

    ext.exec("history-show", args.join(' '), null);

  }, { argCount: '*' }, true

);
||<
]]></detail>
</KeySnailPlugin>;

// Option

let pOptions = plugins.setupOptions("history", {
    "max-results" : {
        preset: 1000,
        description: M({
            ja: "検索最大データ数 (default: 1000)",
            en: "Max number of results (default: 1000)"
        })
    },
    "keymap": {
        preset: {
            "C-z"   : "prompt-toggle-edit-mode",
            // history specific actions
            "/"     : "search-all-histories",
        },
        description: M({
            ja: "メイン画面の操作用キーマップ",
            en: "Local keymap for manipulation"
        })
    },
}, PLUGIN_INFO);

// Add ext

let gPrompt = {
    get visible() !document.getElementById("keysnail-prompt").hidden,
    forced : false,
    close  : function () {
        if (!gPrompt.forced)
            return;

        gPrompt.forced = false;

        if (gPrompt.visible)
            prompt.finish(true);
    }
};

let LocalHistory = {
    _search: function (q) {
        //search option
        let options = PlacesUtils.history.getNewQueryOptions();
        options.maxResults = pOptions['max-results'];
        options.queryType = Ci.nsINavHistoryQueryOptions.QUERY_TYPE_HISTORY;
        //options.sortingMode = Ci.nsINavHistoryQueryOptions.SORT_BY_FRECENCY_DESCENDING;
        options.sortingMode = Ci.nsINavHistoryQueryOptions.SORT_BY_DATE_DESCENDING;
        options.includeHidden = true;

        //search query
        let query = PlacesUtils.history.getNewQuery();
        query.searchTerms  = q;

        let result = PlacesUtils.history.executeQuery(query, options);
        let root = result.root;
        root.containerOpen = true;

        let collection = [];
        let now = new Date().getTime();
        for (let i = 0; i < root.childCount; i++) {
            let siteNode = root.getChild(i);
            collection.push([siteNode.icon, siteNode.title, siteNode.uri, toAgo(now - (siteNode.time / 1000))]);
        }
        root.containerOpen = false;

        return collection;
    },

    _openPrompt: function(collection) {
        let self = this;
        prompt.selector(
            {
                message    : "pattern:",
                collection : collection,
                flags      : [ICON | IGNORE, 0, 0, 0],
                style      : [style.prompt.description, style.prompt.url, style.prompt.description],
                header     : ["Title", "Url", "Last visited"],
                width      : [40, 50, 10],
                keymap     : pOptions["keymap"],
                actions    : [
                    [function (aIndex) {
                         if (aIndex >= 0) {
                             let url = collection[aIndex][2];
                             openUILinkIn(url, "tab");
                         }
                     },
                     'Open',
                     'open'],
                    [function (aIndex) {
                         gPrompt.close();
                         prompt.read("search:", function (word) {
                             if (word)
                                 self.search(word);
                         }, null, null, null, 0, "history_search");
                     },
                     'Search',
                     "search-all-histories"]
                ]
            }
        );
    },

    search: function(q) {
        let result = this._search(q);
        if (result.length > 0) {
            this._openPrompt(result);
        } else {
            gPrompt.close();
            prompt.read("search:", function (word) {
                if (word)
                    search(word);
            }, null, null, null, 0, "history_search");
            display.echoStatusBar("No results", 3000);
        }
    }
};

plugins.withProvides(function (provide) {
    provide("history-show",
        function (ev, arg) { LocalHistory.search(arg); },
        M({ja: "History - リストを表示", en: "History - Show histories list"}));
}, PLUGIN_INFO);

// Util

// timeago: a jQuery plugin
// http://timeago.yarp.com/
function toAgo(time){
    var strings = {
        suffixAgo: "ago",
        minute: "about a minute",
        minutes: "%d minutes",
        hour: "about an hour",
        hours: "about %d hours",
        day: "a day",
        days: "%d days",
        month: "about a month",
        months: "%d months",
        year: "about a year",
        years: "%d years"
    };
    var $l = strings;
    var prefix = $l.prefixAgo;
    var suffix = $l.suffixAgo;

    var distanceMillis = time;

    var seconds = distanceMillis / 1000;
    var minutes = seconds / 60;
    var hours = minutes / 60;
    var days = hours / 24;
    var years = days / 365;

    function substitute(string, number) {
        var value = ($l.numbers && $l.numbers[number]) || number;
        return string.replace('%d', value);
    }

    var words =
        seconds < 90 && substitute($l.minute, 1) ||
        minutes < 45 && substitute($l.minutes, Math.round(minutes)) ||
        minutes < 90 && substitute($l.hour, 1) ||
        hours < 24 && substitute($l.hours, Math.round(hours)) ||
        hours < 48 && substitute($l.day, 1) ||
        days < 30 && substitute($l.days, Math.floor(days)) ||
        days < 60 && substitute($l.month, 1) ||
        days < 365 && substitute($l.months, Math.floor(days / 30)) ||
        years < 2 && substitute($l.year, 1) ||
        substitute($l.years, Math.floor(years));

    return [prefix, words, suffix].join(" ").trim();
}
