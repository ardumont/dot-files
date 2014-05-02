var PLUGIN_INFO =
        <KeySnailPlugin>
            <name>LinkDownloads</name>
            <description>リンクから選択してダウンロード</description>
            <include>main</include>
            <version>0.0.1</version>
            <updateURL>https://github.com/azu/KeySnail-Plugins/raw/master/LinkDownloads/link-downloads.ks.js</updateURL>
            <iconURL>https://github.com/azu/KeySnail-Plugins/raw/master/LinkDownloads/MyIcon.png</iconURL>
            <author mail="info@efcl.info" homepage="http://efcl.info/">azu</author>
            <license>The MIT License</license>
            <provides>
                <ext>linkdownloads-open-prompt</ext>
            </provides>
            <options>
                <option>
                    <name>linkdownloads.direcotry_path</name>
                    <type>string</type>
                    <description>set download directry path</description>
                    <description lang="ja">ダウンロードするディレクトリのパスを設定</description>
                </option>
            </options>
            <detail><![CDATA[]]></detail>
            <detail lang="ja"><![CDATA[
=== 使い方 ===
downloads-open-prompt をキーにセットするか、エクステ一覧からdownloads-open-prompt実行する。
また、プロンプトなしで直接ダウンロードする場合は以下のように、ダウンロードディレクトリを指定する必要があります。
>||
// .keysnail.jsに記述
plugins.options["linkdownloads.direcotry_path"] = "d:\\Downloads";
||<
]]></detail>
        </KeySnailPlugin>;
function downloads(win, doc) {
    var _cache = {};

    function getLinkAll() {
        var links;
        links = doc.querySelectorAll("a");
        return links;
    }

    function getLinkAry(links) {
        var result = [];
        for (var i = 0,len = links.length; i < len; i++) {
            var link = links[i];
            if (link.hasAttribute("href")) {
                result.push([link.title || link.textContent || "none" , link.href])
            }
        }
        return result;
    }

    function saveFile(aFileName, aURL, aSkipPrompt) {
        try {
            saveURL(aURL, aFileName, null, true, aSkipPrompt/*prompt*/, doc.documentURIObjectt)
        } catch (e) {
            util.message(e.lineNumber + e.message);
        }
    }

    function saveFilebyElement(elem) {
        if (!plugins.options["linkdownloads.direcotry_path"]) {
            display.prettyPrint('Please set "linkdownloads.direcotry_path"');
            throw 'Please set "linkdownloads.direcotry_path"';
        }
        var doc = elem.ownerDocument;
        var elemSrc = elem.href ? elem.href : elem.src;
        var url = window.makeURLAbsolute(elem.baseURI, elemSrc);
        var leafname = elemSrc.split("/").pop();
        // leafnameには?.*という文字列が追加されている場合があるので、?以下を取り除く
        var m = leafname.match(/([^?]+)\?.*$/);
        leafname = (m && m[1]) || leafname;
        try {
            window.urlSecurityCheck(url, doc.nodePrincipal);
            //new obj_URI object
            var obj_URI = Components.classes["@mozilla.org/network/io-service;1"]
                    .getService(Components.interfaces.nsIIOService).newURI(elemSrc, null, null);
            //new file object
            var obj_TargetFile = Components.classes["@mozilla.org/file/local;1"]
                    .createInstance(Components.interfaces.nsILocalFile);
            //set file with path
            obj_TargetFile.initWithPath(plugins.options["linkdownloads.direcotry_path"]);
            obj_TargetFile.append(leafname);
            //if file doesn't exist, create
            if (!obj_TargetFile.exists()) {
                obj_TargetFile.create(0x00, 0644);
            }
            //new persitence object
            var obj_Persist = Components.classes["@mozilla.org/embedding/browser/nsWebBrowserPersist;1"]
                    .createInstance(Components.interfaces.nsIWebBrowserPersist);
            // with persist flags if desired
            const nsIWBP = Components.interfaces.nsIWebBrowserPersist;
            const flags = nsIWBP.PERSIST_FLAGS_REPLACE_EXISTING_FILES;
            obj_Persist.persistFlags = flags | nsIWBP.PERSIST_FLAGS_FROM_CACHE;

            //save file to target
            obj_Persist.saveURI(obj_URI, null, null, null, null, obj_TargetFile);
            display.showPopup("Start SaveFile", L(obj_TargetFile.path));
        } catch(e) {
            util.message(e.lineNumber + e.message);
        }
    }

    function addStyle(css) {
        var head, style;
        head = doc.getElementsByTagName("head")[0];
        if (!head) {
            return;
        }
        style = doc.createElement("style");
        style.type = "text/css";
        style.innerHTML = css;
        head.appendChild(style);
    }

    function getElementPosition(elem) {
        var position = elem.getBoundingClientRect();
        return {
            left:Math.round(win.scrollX + position.left),
            top:Math.round(win.scrollY + position.top)
        }
    }

    function scrollToElement(ele) {
        var height = getElementPosition(ele).top;
        height = height > 200 ? height - 200 : height;
        win.scrollTo(0, height);
    }

    return {
        "getLinkAry" : getLinkAry,
        "getLinkAll" : getLinkAll,
        "saveFile" : saveFile,
        "saveFilebyElement" : saveFilebyElement,
        "addStyle" : addStyle,
        "scrollToElement": scrollToElement
    };
}
// コマンド追加
ext.add("linkdownloads-open-prompt",
        openPrompt,
        M({ja: "リンクからダウンロード候補を検索",
            en: "search for download form links"}));
function openPrompt() {
    var context = downloads(window.content, window.content.document);
    var allLinks = context.getLinkAll();
    var collection = context.getLinkAry(allLinks);

    function getLinkElement(aIndex) {
        return allLinks[aIndex];
    }

    context.addStyle(String(<>
        <![CDATA[
            .keysnail-element-select{
                background-clip: border-box;
                border: 3px solid #f00;
                border-radius: 20px;
            }
    ]]></>));
    var selectedIndex = null;
    prompt.selector({
        message    : "pattern:",
        collection : collection,
        flags      : [0 , 0],
        style      : [null, style.prompt.description],
        header     : ["Title", "URL"],
        width      : [25, 45],
        onChange: function (arg) {
            // TODO : 初回の要素のクラスがなぜか残る
            var index = arg.index;
            if (selectedIndex !== index) {
                var targetElement = getLinkElement(index);
                targetElement.classList.add("keysnail-element-select");
                context.scrollToElement(targetElement);
                if (selectedIndex) {
                    getLinkElement(selectedIndex).classList.remove("keysnail-element-select");
                }
                selectedIndex = index;
            }
        },
        onFinish:function (arg) {
            if (selectedIndex) {
                var targetElement = getLinkElement(selectedIndex);
                targetElement.classList.remove("keysnail-element-select");
            }
        },
        actions    : [
            [function (aIndex) {
                if (aIndex >= 0) {
                    var target = getLinkElement(aIndex);
                    context.saveFilebyElement(target);
                }
            },"Save File","save-file"],
            [function (aIndex) {
                if (aIndex >= 0) {
                    var target = collection[aIndex];
                    context.savefile(target[0], target[1]);
                }
            },"Save File(Prompt)","save-file-prompt"],
        ]

    });
}
