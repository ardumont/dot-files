var PLUGIN_INFO =
<KeySnailPlugin>
    <name>Tanything</name>
    <name lang="ja">Tanything</name>
    <description>Tanything</description>
    <description lang="ja">KeySnailからタブを操作</description>
    <version>0.1.4</version>
    <iconURL>http://github.com/myuhe/KeySnail_Plugin/raw/master/Tanything.png</iconURL>
    <updateURL>http://github.com/myuhe/KeySnail_Plugin/raw/master/Tanything.ks.js</updateURL>
    <author mail="yuhei.maeda_at_gmail.com" homepage="http://sheephead.homelinux.org/">myuhe</author>
    <license>The MIT License</license>
    <license lang="ja">MIT ライセンス</license>
    <minVersion>1.8.5</minVersion>
    <include>main</include>
    <detail><![CDATA[
==== What's this ====

This plugin allows you to manipulate tabs using prompt.selector.

==== Launching ====

Paste code below to your .keysnail.js file and you can call tanything by pressing *a* key.

>||
key.setViewKey("a", function (ev, arg) {
                   ext.exec("tanything", arg);
               }, "view all tabs", true);
||<

==== Setting keybindings ====

You can set keybindings by inserting the settings like below example to the PRESERVE area in your .keysnail.js file.

>||
plugins.options["tanything_opt.keymap"] = {
    "C-z"   : "prompt-toggle-edit-mode",
    "SPC"   : "prompt-next-page",
    "b"     : "prompt-previous-page",
    "j"     : "prompt-next-completion",
    "k"     : "prompt-previous-completion",
    "g"     : "prompt-beginning-of-candidates",
    "G"     : "prompt-end-of-candidates",
    "D"     : "prompt-cancel",
    // Tanything specific actions
    "O"     : "localOpen",
    "q"     : "localClose",
    "p"     : "localLeftclose",
    "n"     : "localRightclose",
    "a"     : "localAllclose",
    "d"     : "localDomainclose",
    "c"     : "localClipUT",
    "C"     : "localClipU",
    "e"     : "localMovetoend"
};
||<

When you want to input the alphabet which bounds to the command, press C-z or click *Earth icon* and switch to the edit mode.
]]></detail>
    <detail lang="ja"><![CDATA[
==== 機能 ====

タブをKeySnailから操作します。

==== 起動 ====

適当なキーへ Tanythingを割り当てます。
.keysnail.js へ以下のようなスクリプトを張り付けてください。以下の例では"a"に割り当ててます。

>||
key.setViewKey("a", function (ev, arg) {
                   ext.exec("tanything", arg);
               }, "タブを一覧表示", true);
||<

==== キーバインドの設定 ====

次のような設定を .keysnail.js の PRESERVE エリアへ張り付けておくと、かくだんに操作がしやすくなります。

>||
plugins.options["tanything_opt.keymap"] = {
    "C-z"   : "prompt-toggle-edit-mode",
    "SPC"   : "prompt-next-page",
    "b"     : "prompt-previous-page",
    "j"     : "prompt-next-completion",
    "k"     : "prompt-previous-completion",
    "g"     : "prompt-beginning-of-candidates",
    "G"     : "prompt-end-of-candidates",
    "D"     : "prompt-cancel",
    // Tanything specific actions
    "O"     : "localOpen",
    "q"     : "localClose",
    "p"     : "localLeftclose",
    "n"     : "localRightclose",
    "a"     : "localAllclose",
    "d"     : "localDomainclose",
    "c"     : "localClipUT",
    "C"     : "localClipU",
    "e"     : "localMovetoend",
    "p"     : "localTogglePin"
};
||<

このままではアルファベットが入力できないので、もし絞り込み検索などでアルファベットを入力したくなった場合は C-z を入力するか「閉じる」ボタン左の「地球マーク」をクリックし、編集モードへと切り替えてください。
]]></detail>
</KeySnailPlugin>;

// ================ Key Bindings ====================== //

let pOptions = plugins.setupOptions("tanything_opt", {
    keymap : {
        preset: {}
    },
    pinned_tab_style : {
        preset: "font-weight : bold;",
        description: M({ja: "ピン留めされたタブのスタイル",
                        en: "Style of the pinned tab"})
    }
}, PLUGIN_INFO);

var tanything =
    (function () {
         var currentCollection;

         var tanythingAction = [
             [function (aIndex) {
                  if (aIndex >= 0) open(aIndex);
              }, M({ja: "このタブを開く : ", en: ""}) + "open tab", "localOpen,c"],
             [function (aIndex) {
                  if (aIndex >= 0) close(aIndex);
              }, M({ja: "このタブを閉じる : ", en: ""}) + "close tab", "localClose,c"],
             [function (aIndex) {
                  if (aIndex >= 0) leftclose(aIndex);
              }, M({ja: "左のタブをすべて閉じる : ", en: ""}) + "close left tab", "localLeftclose,c"],
             [function (aIndex) {
                  if (aIndex >= 0) rightclose(aIndex);
              }, M({ja: "右のタブをすべて閉じる : ", en: ""}) + "close right tab", "localRightclose,c"],
             [function (aIndex) {
                  if (aIndex >= 0) allclose(aIndex);
              }, M({ja: "他のタブをすべて閉じる : ", en: ""}) + "close other all tab", "localAllclose,c"],
             [function (aIndex) {
                  if (aIndex >= 0) domainclose(aIndex);
              }, M({ja: "同じドメインのタブをすべて閉じる : ", en: ""}) + "close same domain tab", "localDomainclose,c"],
             [function (aIndex) {
                  if (aIndex >= 0) clipUT(aIndex);
              }, M({ja: "URLとタイトルをHTMLタグ付きでクリップボードにコピー : ", en: ""}) + "copy URL and title", "localClipUT,c"],
             [function (aIndex) {
                  if (aIndex >= 0) clipU(aIndex);
              },  M({ja: "URLをHTMLタグ付きでクリップボードにコピー : ", en: ""}) + "copy URL", "localClipU,c"],
             [function (aIndex) {
                  if (aIndex >= 0) movetoend(aIndex);
              }, M({ja: "タブを末尾に移動する : ", en: ""}) + "move to end", "localMovetoend,c"],
             [function (aIndex) {
                  if (aIndex >= 0) movetostart(aIndex);
              }, M({ja: "タブを先頭に移動する : ", en: ""}) + "move to start", "localMovetostart,c"],
             [function (aIndex) {
                  if (aIndex >= 0) addToBookmarks(aIndex);
              }, M({ja: "タブをブックマークに追加 : ", en: ""}) + "add selected tab to bookmarks", "localAddBokmark,c"],
             [function (aIndex) {
                  if (aIndex >= 0) togglePin(aIndex);
              }, M({ja: "タブをピン留め / ピン留めを外す : ", en: ""}) + "toggle pin", "localTogglePin,c"]
         ];

         function getTabs() Array.slice(gBrowser.mTabContainer.childNodes);

         function callSelector() {
             function getIconFor(tab) {
                 return (tab.linkedBrowser.__SS_data) ?
                     tab.linkedBrowser.__SS_data.attributes.image :
                     util.getFaviconPath(tab.linkedBrowser.contentDocument.URL);
             }

             function getInfoForTab(tab) {
                 let browser = tab.linkedBrowser;
                 let win     = browser.contentWindow;

                 let title = tab.label;
                 let url   = win.location.href;

                 return [util.getFaviconPath(url), title, url, tab];
             }

             currentCollection = [getInfoForTab(tab) for each (tab in getTabs())];

             prompt.selector({
                 message             : "select tab: ",
                 initialIndex        : gBrowser.mTabContainer.selectedIndex,
                 flags               : [ICON | IGNORE, 0, 0, IGNORE | HIDDEN],
                 collection          : currentCollection,
                 header              : ["title", "url"],
                 keymap              : pOptions.keymap,
                 actions             : tanythingAction,
                 supressRecoverFocus : true,
                 onFinish            : focusContent,
                 stylist             : function (args, n, current) {
                     if (current !== currentCollection)
                         return null;

                     let tab = args[3];
                     if (tab.pinned)
                         return pOptions.pinned_tab_style;
                     else
                         return null;
                 }
             });
         }

         function focusContent() {
             gBrowser.focus();
             _content.focus();
         }

         function open(aIndex) {
             gBrowser.mTabContainer.selectedIndex = aIndex;
         }

         function close(aIndex) {
             if (currentCollection.length === 1)
             {
                 prompt.finish(true);
                 return;
             }

             gBrowser.removeTab(getTabs()[aIndex]);
             currentCollection.splice(aIndex, 1);
             prompt.refresh();
         }

         function leftclose(aIndex) {
             let tabs = getTabs();

             for (let i = 0; i < aIndex; ++i)
                 gBrowser.removeTab(tabs[i]);

             currentCollection.splice(0, aIndex);
             prompt.refresh(0);
         }

         function rightclose(aIndex) {
             let tabs = getTabs();

             for (let i = aIndex + 1; i < tabs.length; ++i)
                 gBrowser.removeTab(tabs[i]);

             currentCollection.splice(aIndex + 1, tabs.length - (aIndex + 1));
             prompt.refresh(aIndex);
         }

         function allclose(aIndex) {
             let tabs = getTabs();

             for (let i = 0; i < tabs.length; ++i)
             {
                 if (i !== aIndex)
                     gBrowser.removeTab(tabs[i]);
             }

             currentCollection = [currentCollection[aIndex]];
             prompt.refresh(0);
         }

         function getURIFromTab(aTab) aTab.linkedBrowser.currentURI;

         function domainclose(aIndex) {
             function getHost(aNsURI) {
                 try {
                     return aNsURI.host;
                 } catch (e) {
                     return "";
                 }
             }

             let tabs        = getTabs();
             let selectedURI = getURIFromTab(tabs[aIndex]);
             let host        = getHost(selectedURI);

             if (host)
             {
                 for (let i = tabs.length - 1; i >= 0; --i)
                 {
                     if (host === getHost(getURIFromTab(tabs[i])))
                     {
                         gBrowser.removeTab(tabs[i]);
                         currentCollection.splice(i, 1);
                     }
                 }
             }

             prompt.refresh();
         }

         function clipUT(aIndex) {
             let row   = currentCollection[aIndex];
             let uri   = row[2];
             let title = row[1];

             var txt = "<a href=\"" + uri + "\">" + title + "</a>";
             command.setClipboardText(txt);
         }

         function clipU(aIndex) {
             let row   = currentCollection[aIndex];
             let uri   = row[2];
             let title = row[1];

             var txt = "<a href=\"" + uri + "\">" + "</a>";
             command.setClipboardText(txt);
         }

         function movetoend(aIndex) {
             let browser = gBrowser;
             let tabs    = getTabs();

             browser.moveTabTo(tabs[aIndex], tabs.length - 1);

             let selected = currentCollection[aIndex].slice(0);
             currentCollection.splice(aIndex, 1);
             currentCollection.push(selected);

             prompt.refresh(tabs.length - 1);
         }

         function movetostart(aIndex) {
             let browser = gBrowser;
             let tabs    = getTabs();

             browser.moveTabTo(tabs[aIndex], 0);

             let selected = currentCollection[aIndex].slice(0);
             currentCollection.splice(aIndex, 1);
             currentCollection.unshift(selected);

             prompt.refresh(0);
         }

         function addToBookmarks(aIndex) {
             let tab = getTabs()[aIndex];

             let [title, uri] = [tab.linkedBrowser.contentDocument.title, getURIFromTab(tab)];
             PlacesUIUtils.showAddBookmarkUI(uri, title);
         }

         function togglePin(aIndex) {
             if (!("pinTab" in gBrowser))
                 return;

             let tab = getTabs()[aIndex];

             if (tab.pinned)
                 gBrowser.unpinTab(tab);
             else
                 gBrowser.pinTab(tab);

             let tabs = getTabs();

             // move pinned tab
             let newIdx = tabs.indexOf(tab);
             currentCollection.splice(newIdx, 0, currentCollection.splice(aIndex, 1)[0]);

             prompt.refresh(aIndex);
         }

         var self = {
             showAlltab: function () {
                 callSelector();
             }
         };

         return self;
     })();

plugins.withProvides(function (provide) {
    provide("tanything", tanything.showAlltab, M({
        ja: "タブを一覧表示",
        en: "view all tabs "
    }));
}, PLUGIN_INFO);
