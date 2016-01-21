// Info

let PLUGIN_INFO =
<KeySnailPlugin>
    <name>PocketSnail</name>
    <description>Pocket from KeySnail</description>
    <description lang="ja">Pocket 拡張と連携</description>
    <author>958</author>
    <iconURL>https://sites.google.com/site/958site/Home/files/RILnail.ks.png</iconURL>
    <updateURL>https://gist.github.com/958/895703/raw/RILnail.ks.js</updateURL>
    <version>0.2.2</version>
    <license>MIT</license>
    <minVersion>1.8.0</minVersion>
    <include>main</include>
    <detail lang="ja"><![CDATA[
=== 使い方 ===
Firefox 拡張 Pocket が必須です
https://addons.mozilla.org/ja/firefox/addon/read-it-later/

HoK 拡張ヒントモードを使って、リンクを Pocket に追加することも

キーマップを変更したい人は、次のような設定を .keysnail.js の PRESERVE エリアへ

>||
plugins.options["ril.keymap"] = {
    "C-z"   : "prompt-toggle-edit-mode",
    "j"     : "prompt-next-completion",
    "k"     : "prompt-previous-completion",
    "g"     : "prompt-beginning-of-candidates",
    "G"     : "prompt-end-of-candidates",
    "D"     : "prompt-cancel",
    // PocketSnail specific actions
    "o"     : "open,c",
    "O"     : "open-background,c",
    "t"     : "open-text,c",
    "T"     : "open-text-background,c",
    "d"     : "delete",
    "r"     : "sync"
};
||<
]]></detail>
</KeySnailPlugin>;

// Option

let pOptions = plugins.setupOptions("ril", {
    "list_sort_type" : {
        preset: 1,
        description: M({
            en: "List sort type 0:ASC by date 1:DESC by date (Default: 1)",
            ja: "リストの並び順 0:日付昇順 1:日付降順 (Default: 1)"})
    },
    "hok_add_ril" : {
        preset: 'l',
        description: M({
            en: "Key bound to `Append Pocket` in the HoK extended hint mode (Default: l)",
            ja: "HoK 拡張ヒントモードにおいて `リンクを Pocket に追加` へ割り当てるキー (デフォルト: l)"})
    },
    "hok_add_ril_multi" : {
        preset: 'L',
        description: M({
            en: "Key bound to `Append Pocket multiple hints` in the HoK extended hint mode (Default: L)",
            ja: "HoK 拡張ヒントモードにおいて `連続してリンクを Pocket に追加` へ割り当てるキー (デフォルト: L)"})
    },
    "keymap" : {
        preset: {
            "C-z"   : "prompt-toggle-edit-mode",
            "j"     : "prompt-next-completion",
            "k"     : "prompt-previous-completion",
            "g"     : "prompt-beginning-of-candidates",
            "G"     : "prompt-end-of-candidates",
            "D"     : "prompt-cancel",
            // PocketSnail specific actions
            "o"     : "open,c",
            "O"     : "open-background,c",
            "t"     : "open-text,c",
            "T"     : "open-text-background,c",
            "d"     : "delete",
            "r"     : "sync"
        },
        description: M({
            ja: "メイン画面の操作用キーマップ",
            en: "Local keymap for manipulation"
        })
    }
}, PLUGIN_INFO);

// Extend HoK

hook.addToHook('PluginLoaded', function () {
//    RIL.APP.registerObserver('ril-api-request-finished');

    if (!plugins.hok)
        return;

    let AddRIL = function (elem) {
        if (elem) {
            var link = RIL.bubbleToTagName(elem, 'A');
            var title = RIL.APP.stripTags(link.innerHTML);
            if (title.length == 0 && link.firstChild) {
                if (link.firstChild.title.length > 0) {
                    title = link.firstChild.title;
                } else if (link.firstChild.alt.length > 0) {
                    title = link.firstChild.title;
                }
            }
            RIL.saveLink(link.href, title, RIL.xul('clickToSaveTags').value);
            display.echoStatusBar("Append Pocket - " + title, 3000);
        }
    };

    plugins.hok.addActions([
        // append Pocket
        [pOptions["hok_add_ril"],
         M({ja: "リンクを Pocket に追加", en: "Append Pocket"}),
         AddRIL],
        // append Pocket multiple hints
        [pOptions["hok_add_ril_multi"],
         M({ja: "連続してリンクを Pocket に追加", en: "Append Pocket multiple hints"}),
         AddRIL, false, true]
    ]);
});

// Add ext

plugins.withProvides(function (provide) {
    provide("ril-append",
        (ev, arg) => {
            let item = RIL.APP.LIST.itemByUrl(window.content.location.href);
            if (!item) {
                let itemId = RIL.addCurrent();
                display.echoStatusBar("Append Pocket - " + window.content.document.wrappedJSObject.title.toString() || "", 3000);
            }
        },
        M({ja: "Pocket - 現在のタブを Pocket に追加", en: "Pocket - Append current tab"}));
    provide("ril-remove",
        (ev, arg) => {
            let item = RIL.APP.LIST.itemByUrl(window.content.location.href);
            if (item) {
                RIL.markCurrentAsRead()
                display.echoStatusBar("Remove Pocket - " + window.content.document.wrappedJSObject.title.toString() || "", 3000);
            }
        },
        M({ja: "Pocket - 現在のタブを Pocket から削除", en: "Pocket - Remove current tab"}));
    provide("ril-toggle",
        (ev, arg) => {
            let item = RIL.APP.LIST.itemByUrl(window.content.location.href);
            if (item) {
                RIL.markCurrentAsRead()
                display.echoStatusBar("Remove Pocket - " + window.content.document.wrappedJSObject.title.toString() || "", 3000);
            } else {
                let itemId = RIL.addCurrent();
                display.echoStatusBar("Append Pocket - " + window.content.document.wrappedJSObject.title.toString() || "", 3000);
            }
        },
        M({ja: "Pocket - 現在のタブを Pocket に追加 または 削除", en: "Pocket - Append or remove current tab"}));
    provide("ril-append-and-close",
        (ev, arg) => {
            let item = RIL.APP.LIST.itemByUrl(window.content.location.href);
            if (!item) {
                let itemId = RIL.addCurrent();
                display.echoStatusBar("Append Pocket - " + window.content.document.wrappedJSObject.title.toString() || "", 3000);
            }
            if (arg || !item) {
                gBrowser.removeTab(gBrowser.mCurrentTab);
            }
        },
        M({ja: "Pocket - 現在のタブを Pocket に追加してタブを閉じる", en: "Pocket - Append current tab and close"}));
    provide("ril-open-text",
        (ev, arg) => RIL.retrieveAndOpenTextForUrl(window.content.location.href, 'current'),
        M({ja: "Pocket - 現在のタブのテキストビューを表示", en: "Pocket - Open Text View current tab"}));
    provide("ril-open-text-tab",
        (ev, arg) => RIL.retrieveAndOpenTextForUrl(window.content.location.href, 'tab'),
        M({ja: "Pocket - 現在のタブのテキストビューを新しいタブで表示", en: "Pocket - Open Text View in new tab"}));
    provide("ril-open-text-background-tab",
        (ev, arg) => RIL.retrieveAndOpenTextForUrl(window.content.location.href, 'tabshifted'),
        M({ja: "Pocket - 現在のタブのテキストビューをバックグラウンドタブで表示", en: "Pocket - Open Text View in new background tab"}));
    provide("ril-show-reading-list",
        (ev, arg) => {
            function getFaviconPath(uri) {
                return new Promise((resolve, reject) => {
                    var faviconService = PlacesUtils.favicons;
                    faviconService.getFaviconURLForPage(
                        NetUtil.newURI(uri, null, null),
                        {
                            onComplete: (aURI, aDataLen, aData, aMimeType) => {
                                resolve((aURI) ? aURI.spec : "chrome://mozapps/skin/places/defaultFavicon.png");
                            },
                        }
                    );
                })
            }
            function itemList() {
                return RIL.APP.LIST.list.map((item) => {
                    return {
                        title: item.title,
                        url: item.url,
                        time: item.timeUpdated,
                        itemId: item.itemId,
                    }
                });
            }
            function showSelector(collection) {
                let sortType = pOptions['list_sort_type']
                collection.sort((a,b) =>(sortType == 0) ?  ((a[4] < b[4]) ? 1 : -1) : ((a[4] < b[4]) ? -1 : 1));
                prompt.selector( {
                    message    : "pattern:",
                    collection : collection,
                    flags      : [ICON | IGNORE, 0, 0, 0, HIDDEN | IGNORE, HIDDEN | IGNORE],
                    style      : [style.prompt.description, style.prompt.url],
                    header     : ["Title", "Url", "Date"],
                    width      : [45, 45, 10],
                    keymap     : pOptions["keymap"],
                    actions    : [
                        [(aIndex, items) => {
                             if (items.length > 0 && aIndex >= 0) {
                                 let url = items[aIndex][2];
                                 openUILinkIn(url, "tab");
                             }
                         },
                         M({ja: '選択中アイテムを新しいタブで開く',
                            en: "Open in new tab"}),
                         "open"],
                        [(aIndex, items) => {
                             if (items.length > 0 && aIndex >= 0) {
                                 let url = items[aIndex][2];
                                 openUILinkIn(url, "tabshifted");
                             }
                         },
                         M({ja: '選択中アイテムを新しいバックグラウンドタブで開く',
                            en: "Open in background new tab"}),
                         "open-background"],
                        [(aIndex, items) => {
                             if (items.length > 0 && aIndex >= 0) {
                                 let url = items[aIndex][2];
                                 RIL.retrieveAndOpenTextForUrl(url, 'tab');
                             }
                         },
                         M({ja: '選択中アイテムのテキストビューを新しいタブで開く',
                            en: "Open Text View in new tab"}),
                         "open-text"],
                        [(aIndex, items) => {
                             if (items.length > 0 && aIndex >= 0) {
                                 let url = items[aIndex][2];
                                 RIL.retrieveAndOpenTextForUrl(url, 'tabshifted');
                             }
                         },
                         M({ja: '選択中アイテムのテキストビューを新しいバックグラウンドタブで開く',
                            en: "Open Text View in background new tab"}),
                         "open-text-background"],
                        [(aIndex, items) => {
                             if (items.length > 0 && aIndex >= 0)
                                 removeItem(items, aIndex, true);
                         },
                         M({ja: '選択中アイテムを削除する',
                            en: "Delete"}),
                         "delete,c"],
                        [(aIndex, items) => {
                             if (RIL.APP.SYNC.syncing && !RIL.APP.SYNC.syncInBackgroundTillResults)
                                 RIL.APP.SYNC.cancelSync();
                             else
                                 RIL.APP.SYNC.sync(false, true);
                         },
                         M({ja: '同期',
                            en: "Sync"}),
                         "sync"]
                    ]
                });
            }
            function removeItem(items, aIndex, deep) {
                 if (deep) {
                     RIL.APP.LIST.mark(items[aIndex][5], true);
                     RIL.APP.LIST.endBatchAndRefresh();
                 }
                 items.splice(aIndex, 1);
                 if (items.length == 0)
                     prompt.finish();
                 else
                     prompt.refresh();
            }

            function collect(favicon) {
                collection.push([
                    favicon,
                    html.unEscapeTag(item.title).replace(/&amp;/g, '&'),
                    item.url,
                    toAgo(item.time * 1000),
                    item.time,
                    item.itemId
                ]);
                item = items.next();
                if (item)
                    getFaviconPath(item.url).then(collect);
                else
                    showSelector(collection);
            }

            let items = itemList();
            let collection = [];
            items.reduce(
                (promise, item) => {
                    return getFaviconPath(item.url).then((favicon) => {
                        collection.push([
                            favicon,
                            html.unEscapeTag(item.title).replace(/&amp;/g, '&'),
                            item.url,
                            toAgo(item.time * 1000),
                            item.time,
                            item.itemId
                        ]);
                    });
                }, Promise.resolve())
                .then(() => {
                    showSelector(collection);
                });
        },
        M({ja: "Pocket - リストを表示", en: "Pocket - Show reading list"}));
}, PLUGIN_INFO);


// Util

// timeago: a jQuery plugin
// http://timeago.yarp.com/
function toAgo(time){
	var strings = {
		suffixAgo: "ago",
		suffixFromNow: "from now",
		seconds: "less than a minute",
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

	var distanceMillis = new Date().getTime() - time;

	var seconds = distanceMillis / 1000;
	var minutes = seconds / 60;
	var hours = minutes / 60;
	var days = hours / 24;
	var years = days / 365;

	function substitute(string, number) {
		var value = ($l.numbers && $l.numbers[number]) || number;
		return string.replace(/%d/i, value);
	}

	var words = seconds < 45 && substitute($l.seconds, Math.round(seconds)) ||
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
