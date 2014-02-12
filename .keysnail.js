// ========================== KeySnail Init File =========================== //

// You can preserve your code in this area when generating the init file using GUI.
// Put all your code except special key, set*key, hook, blacklist.
// ========================================================================= //
//{{%PRESERVE%
// Put your codes here
//}}%PRESERVE%
// ========================================================================= //

// ========================= Special key settings ========================== //

key.quitKey              = "C-g";
key.helpKey              = "<f1>";
key.escapeKey            = "C-q";
key.macroStartKey        = ["C-x", "("];
key.macroEndKey          = ["C-x", ")"];
key.universalArgumentKey = "C-u";
key.negativeArgument1Key = "C--";
key.negativeArgument2Key = "C-M--";
key.negativeArgument3Key = "M--";
key.suspendKey           = "<f2>";

// ================================= Hooks ================================= //

ext.add("activate-caret-mode",
        function (ev, arg) {
            setCaretMode(true);
        }, M({ja: "activate-caret-mode", en: "activate-caret-mode"}));

ext.add("deactivate-caret-mode",
        function (ev, arg) {
            setCaretMode(false);
        }, M({ja: "deactivate-caret-mode", en: "deactivate-caret-mode"}));

function setCaretMode(b) {
    display.echoStatusBar((b ? "A" : "Dea") + "ctivate the caret mode", 2000);
    util.setBoolPref("accessibility.browsewithcaret", b);
}

hook.setHook('KeyBoardQuit', function (aEvent) {
    if (key.currentKeySequence.length) {
        return;
    }

    command.closeFindBar();

    var marked = command.marked(aEvent);

    if (util.isCaretEnabled()) {
        if (marked) {
            command.resetMark(aEvent);
        } else {
            if ("blur" in aEvent.target) {
                aEvent.target.blur();
            }
            gBrowser.focus();
            _content.focus();
        }
    } else {
        goDoCommand("cmd_selectNone");
    }
    if (KeySnail.windowType === "navigator:browser" && !marked) {
        key.generateKey(aEvent.originalTarget, KeyEvent.DOM_VK_ESCAPE, true);
    }
});

// ============================== Black list =============================== //

hook.addToHook("LocationChange", function (aNsURI) {
    var URL = aNsURI ? aNsURI.spec : null;
    key.suspendWhenMatched(URL, key.blackList);
});

key.blackList = [
    // 'http://mail.google.com/*',
    // 'https://mail.google.com/*',
    // 'http://gmail.com/*',
    // 'https://gmail.com/*'
];

// ============================= Key bindings ============================== //

key.setGlobalKey('C-M-r', function (ev) {
    userscript.reload();
}, 'Reload the initialization file', true);

key.setGlobalKey('M-x', function (ev, arg) {
    ext.select(arg, ev);
}, 'List exts and execute selected one', true);

key.setGlobalKey('M-:', function (ev) {
    command.interpreter();
}, 'Command interpreter', true);

key.setGlobalKey(['<f1>', 'b'], function (ev) {
    key.listKeyBindings();
}, 'List all keybindings');

key.setGlobalKey(['<f1>', 'F'], function (ev) {
    openHelpLink("firefox-help");
}, 'Display Firefox help');

key.setGlobalKey(['C-x', 'l'], function (ev) {
    command.focusToById("urlbar");
}, 'Focus to the location bar', true);

key.setGlobalKey(['C-x', 'g'], function (ev) {
    command.focusToById("searchbar");
}, 'Focus to the search bar', true);

key.setGlobalKey(['C-x', 't'], function (ev) {
    command.focusElement(command.elementsRetrieverTextarea, 0);
}, 'Focus to the first textarea', true);

key.setGlobalKey(['C-x', 's'], function (ev) {
    command.focusElement(command.elementsRetrieverButton, 0);
}, 'Focus to the first button', true);

key.setGlobalKey(['C-x', 'k'], function (ev) {
    BrowserCloseTabOrWindow();
}, 'Close tab / window');

key.setGlobalKey(['C-x', 'K'], function (ev) {
    closeWindow(true);
}, 'Close the window');

key.setGlobalKey(['C-x', 'n'], function (ev) {
    OpenBrowserWindow();
}, 'Open new window');

key.setGlobalKey(['C-x', 'C-c'], function (ev) {
    goQuitApplication();
}, 'Exit Firefox', true);

key.setGlobalKey(['C-x', 'o'], function (ev, arg) {
    command.focusOtherFrame(arg);
}, 'Select next frame');

key.setGlobalKey(['C-x', '1'], function (ev) {
    window.loadURI(ev.target.ownerDocument.location.href);
}, 'Show current frame only', true);

key.setGlobalKey(['C-x', 'C-f'], function (ev) {
    BrowserOpenFileWindow();
}, 'Open the local file', true);

key.setGlobalKey(['C-x', 'C-s'], function (ev) {
    saveDocument(window.content.document);
}, 'Save current page to the file', true);

key.setGlobalKey('M-w', function (ev) {
    command.copyRegion(ev);
    setCaretMode(false);
}, 'Copy selected text', true);

key.setGlobalKey('C-s', function (ev) {
    setCaretMode(true);
    command.iSearchForwardKs(ev);
}, 'Emacs like incremental search forward', true);

key.setGlobalKey('C-r', function (ev) {
    setCaretMode(true);
    command.iSearchBackwardKs(ev);
}, 'Emacs like incremental search backward', true);

key.setGlobalKey(['C-c', 'u'], function (ev) {
    undoCloseTab();
}, 'Undo closed tab');

key.setGlobalKey(['C-c', 'C-c', 'C-v'], function (ev) {
    toJavaScriptConsole();
}, 'Display JavaScript console', true);

key.setGlobalKey(['C-c', 'C-c', 'C-c'], function (ev) {
    command.clearConsole();
}, 'Clear Javascript console', true);

key.setGlobalKey('M-n', function (ev) {
    getBrowser().mTabContainer.advanceSelectedTab(1, true);
}, 'Select next tab');

key.setGlobalKey('M-p', function (ev) {
    getBrowser().mTabContainer.advanceSelectedTab(-1, true);
}, 'Select previous tab');

key.setGlobalKey(['C-c', 'c', 'u'], function (ev) {
    display.echoStatusBar("Copy document url '" + content.location.href + "' to clipboard!", 2000);
    command.setClipboardText(content.location.href);
}, 'Copy document url', true);

key.setGlobalKey(['C-i'], function (ev) {
    document.commandDispatcher.advanceFocus();
}, 'Tabulation', true);

key.setGlobalKey(['C-M-i'], function (ev) {
    document.commandDispatcher.rewindFocus();
}, 'Shift-tabulation', true);

key.setGlobalKey(['C-j'], function (ev) {
    key.generateKey(ev.originalTarget, KeyEvent.DOM_VK_RETURN, true);
}, 'Enter', true);

// key.setGlobalKey('C-g', function (ev) {
//     display.echoStatusBar("Escaping...", 2000);
//     key.generateKey(ev.originalTarget, KeyEvent.DOM_VK_ESCAPE, true);
// }, 'Quit');

key.setViewKey('C-n', function (ev) {
    key.generateKey(ev.originalTarget, KeyEvent.DOM_VK_DOWN, true);
}, 'Scroll line down');

key.setViewKey('C-p', function (ev) {
    key.generateKey(ev.originalTarget, KeyEvent.DOM_VK_UP, true);
}, 'Scroll line up');

key.setViewKey('C-f', function (ev) {
    key.generateKey(ev.originalTarget, KeyEvent.DOM_VK_RIGHT, true);
}, 'Scroll right');

key.setViewKey('C-b', function (ev) {
    key.generateKey(ev.originalTarget, KeyEvent.DOM_VK_LEFT, true);
}, 'Scroll left');

key.setViewKey('M-v', function (ev) {
    goDoCommand("cmd_scrollPageUp");
}, 'Scroll page up');

key.setViewKey('C-v', function (ev) {
    goDoCommand("cmd_scrollPageDown");
}, 'Scroll page down');

key.setViewKey('M-<', function (ev) {
    goDoCommand("cmd_scrollTop");
}, 'Scroll to the top of the page', true);

key.setViewKey([['M->'], ['G']], function (ev) {
    goDoCommand("cmd_scrollBottom");
}, 'Scroll to the bottom of the page', true);

key.setViewKey([['R'], ['C-c', 'r']], function (ev) {
    BrowserReload();
}, 'Reload the page', true);

key.setViewKey([['B'], ['C-c', 'b']], function (ev) {
    BrowserBack();
}, 'Back');

key.setViewKey([['F'], ['C-c', 'f']], function (ev) {
    BrowserForward();
}, 'Forward');

key.setViewKey(['C-x', 'h'], function (ev) {
    goDoCommand("cmd_selectAll");
}, 'Select all', true);

key.setEditKey(['C-x', 'h'], function (ev) {
    command.selectAll(ev);
}, 'Select whole text', true);

key.setEditKey([['C-x', 'u'], ['C-/']], function (ev) {
    display.echoStatusBar("Undo!", 2000);
    goDoCommand("cmd_undo");
}, 'Undo');

key.setEditKey(['C-x', 'r', 'd'], function (ev, arg) {
    command.replaceRectangle(ev.originalTarget, "", false, !arg);
}, 'Delete text in the region-rectangle', true);

key.setEditKey(['C-x', 'r', 't'], function (ev) {
    prompt.read("String rectangle: ", function (aStr, aInput) {
        command.replaceRectangle(aInput, aStr);
    },
    ev.originalTarget);
}, 'Replace text in the region-rectangle with user inputted string', true);

key.setEditKey(['C-x', 'r', 'o'], function (ev) {
    command.openRectangle(ev.originalTarget);
}, 'Blank out the region-rectangle, shifting text right', true);

key.setEditKey(['C-x', 'r', 'k'], function (ev, arg) {
    command.kill.buffer = command.killRectangle(ev.originalTarget, !arg);
}, 'Delete the region-rectangle and save it as the last killed one', true);

key.setEditKey(['C-x', 'r', 'y'], function (ev) {
    command.yankRectangle(ev.originalTarget, command.kill.buffer);
}, 'Yank the last killed rectangle with upper left corner at point', true);

key.setEditKey([['C-SPC'], ['C-@']], function (ev) {
    if(!util.isCaretEnabled()) {// we need to activate the caret mode, F7 to do so
        key.generateKey(ev.originalTarget, KeyEvent.DOM_VK_LEFT, true);
    }
    command.setMark(ev);
}, 'Set the mark', true);

key.setEditKey('C-o', function (ev) {
    command.openLine(ev);
}, 'Open line');

key.setEditKey('C-\\', function (ev) {
    display.echoStatusBar("Redo!", 2000);
    goDoCommand("cmd_redo");
}, 'Redo');

key.setEditKey('C-a', function (ev) {
    command.beginLine(ev);
}, 'Beginning of the line');

key.setEditKey('C-e', function (ev) {
    command.endLine(ev);
}, 'End of the line');

key.setEditKey('C-f', function (ev) {
    command.nextChar(ev);
}, 'Forward char');

key.setEditKey('C-b', function (ev) {
    command.previousChar(ev);
}, 'Backward char');

key.setEditKey('M-f', function (ev) {
    command.forwardWord(ev);
}, 'Next word');

key.setEditKey('M-b', function (ev) {
    command.backwardWord(ev);
}, 'Previous word');

key.setEditKey('C-n', function (ev) {
    command.nextLine(ev);
}, 'Next line');

key.setEditKey('C-p', function (ev) {
    command.previousLine(ev);
}, 'Previous line');

key.setEditKey('C-v', function (ev) {
    command.pageDown(ev);
}, 'Page down');

key.setEditKey('M-v', function (ev) {
    command.pageUp(ev);
}, 'Page up');

key.setEditKey('M-<', function (ev) {
    command.moveTop(ev);
}, 'Beginning of the text area');

key.setEditKey('M->', function (ev) {
    command.moveBottom(ev);
}, 'End of the text area');

key.setEditKey('C-d', function (ev) {
    goDoCommand("cmd_deleteCharForward");
}, 'Delete forward char');

key.setEditKey('C-h', function (ev) {
    goDoCommand("cmd_deleteCharBackward");
}, 'Delete backward char');

key.setEditKey('M-d', function (ev) {
    command.deleteForwardWord(ev);
}, 'Delete forward word');

key.setEditKey([['C-<backspace>'], ['M-<delete>'], ['C-M-h']], function (ev) {
    command.deleteBackwardWord(ev);
}, 'Delete backward word');

key.setEditKey('M-u', function (ev, arg) {
    command.wordCommand(ev, arg, command.upcaseForwardWord, command.upcaseBackwardWord);
}, 'Convert following word to upper case');

key.setEditKey('M-l', function (ev, arg) {
    command.wordCommand(ev, arg, command.downcaseForwardWord, command.downcaseBackwardWord);
}, 'Convert following word to lower case');

key.setEditKey('M-c', function (ev, arg) {
    command.wordCommand(ev, arg, command.capitalizeForwardWord, command.capitalizeBackwardWord);
}, 'Capitalize the following word');

key.setEditKey('C-k', function (ev) {
    command.killLine(ev);
}, 'Kill the rest of the line');

key.setEditKey('C-y', command.yank, 'Paste (Yank)');

key.setEditKey('M-y', command.yankPop, 'Paste pop (Yank pop)', true);

key.setEditKey('C-M-y', function (ev) {
    if (!command.kill.ring.length) return;

    let(ct = command.getClipboardText())(!command.kill.ring.length || ct != command.kill.ring[0]) && command.pushKillRing(ct);

    prompt.selector({
        message: "Paste:",
        collection: command.kill.ring,
        callback: function (i) {
            if (i >= 0) key.insertText(command.kill.ring[i]);
        }
    });
}, 'Show kill-ring and select text to paste', true);

key.setEditKey('C-w', function (ev) {
    goDoCommand("cmd_copy");
    goDoCommand("cmd_delete");
    command.resetMark(ev);
}, 'Cut current region', true);

key.setEditKey('M-n', function (ev) {
    command.walkInputElement(command.elementsRetrieverTextarea, true, true);
}, 'Focus to the next text area');

key.setEditKey('M-p', function (ev) {
    command.walkInputElement(command.elementsRetrieverTextarea, false, true);
}, 'Focus to the previous text area');

key.setEditKey('M-w', function (ev) {
    command.copyRegion(ev);
}, 'Copy selected text');

key.setCaretKey('C-a', function (ev) {
    ev.target.ksMarked ? goDoCommand("cmd_selectBeginLine") : goDoCommand("cmd_beginLine");
}, 'Move caret to the beginning of the line');

key.setCaretKey([['C-e'], ['M->'], ['G']], function (ev) {
    ev.target.ksMarked ? goDoCommand("cmd_selectEndLine") : goDoCommand("cmd_endLine");
}, 'Move caret to the end of the line');

key.setCaretKey('C-n', function (ev) {
    ev.target.ksMarked ? goDoCommand("cmd_selectLineNext") : goDoCommand("cmd_scrollLineDown");
}, 'Move caret to the next line');

key.setCaretKey('C-p', function (ev) {
    ev.target.ksMarked ? goDoCommand("cmd_selectLinePrevious") : goDoCommand("cmd_scrollLineUp");
}, 'Move caret to the previous line');

key.setCaretKey('C-f', function (ev) {
    ev.target.ksMarked ? goDoCommand("cmd_selectCharNext") : goDoCommand("cmd_scrollRight");
}, 'Move caret to the right');

key.setCaretKey([['C-b'], ['C-h']], function (ev) {
    ev.target.ksMarked ? goDoCommand("cmd_selectCharPrevious") : goDoCommand("cmd_scrollLeft");
}, 'Move caret to the left');

key.setCaretKey('M-f', function (ev) {
    ev.target.ksMarked ? goDoCommand("cmd_selectWordNext") : goDoCommand("cmd_wordNext");
}, 'Move caret to the right by word');

key.setCaretKey('M-b', function (ev) {
    ev.target.ksMarked ? goDoCommand("cmd_selectWordPrevious") : goDoCommand("cmd_wordPrevious");
}, 'Move caret to the left by word');

key.setCaretKey([['C-v'], ['SPC']], function (ev) {
    ev.target.ksMarked ? goDoCommand("cmd_selectPageNext") : goDoCommand("cmd_movePageDown");
}, 'Move caret down by page');

key.setCaretKey('M-v', function (ev) {
    ev.target.ksMarked ? goDoCommand("cmd_selectPagePrevious") : goDoCommand("cmd_movePageUp");
}, 'Move caret up by page');

key.setCaretKey('M-<', function (ev) {
    ev.target.ksMarked ? goDoCommand("cmd_selectTop") : goDoCommand("cmd_scrollTop");
}, 'Move caret to the top of the page');

key.setCaretKey([['C-SPC'], ['C-@']], function (ev) {
    command.setMark(ev);
}, 'Set the mark', true);

key.setCaretKey([['R'], ['C-c', 'r']], function (ev) {
    BrowserReload();
}, 'Reload the page', true);

key.setCaretKey([['B'], ['C-c', 'b']], function (ev) {
    BrowserBack();
}, 'Back');

key.setCaretKey([['F'], ['C-c', 'f']], function (ev) {
    BrowserForward();
}, 'Forward');

key.setCaretKey(['C-x', 'h'], function (ev) {
    goDoCommand("cmd_selectAll");
}, 'Select all', true);

// ==================== hok

key.setViewKey('d', function (aEvent, aArg) {
    ext.exec("hok-start-foreground-mode", aArg);
}, 'Hok - Foreground hint mode', true);

key.setViewKey('f', function (aEvent, aArg) {
    ext.exec("hok-start-background-mode", aArg);
}, 'HoK - Background hint mode', true);

key.setViewKey(';', function (aEvent, aArg) {
    ext.exec("hok-start-extended-mode", aArg);
}, 'HoK - Extented hint mode', true);

key.setViewKey(['C-c', 'C-e'], function (aEvent, aArg) {
    ext.exec("hok-start-continuous-mode", aArg);
}, 'Start continuous HaH', true);

key.setViewKey('c', function (aEvent, aArg) {
    ext.exec("hok-yank-foreground-mode", aArg);
}, 'Hok - Foreground yank hint mode', true);

plugins.options["hok.hint_base_style"] = {
    "position"       : 'absolute',
    "z-index"        : '2147483647',
    "color"          : '#000',
    "font-family"    : 'monospace',
    "font-size"      : '9pt',
    "font-weight"    : 'italic',
    "line-height"    : '10pt',
    "padding"        : '2px',
    "margin"         : '0px',
    "text-transform" : 'lowercase'
};

// creating links
plugins.options["hok.hint_keys"] = "qwertyzvbshkl";

// ==================== list bookmarks

key.setViewKey([':', 'b'], function (ev, arg) {
    ext.exec("bmany-list-all-bookmarks", arg, ev);
}, 'bmany - List all bookmarks');

key.setViewKey([':', 'B'], function (ev, arg) {
    ext.exec("bmany-list-bookmarklets", arg, ev);
}, "bmany - List all bookmarklets");

key.setViewKey([':', 'k'], function (ev, arg) {
    ext.exec("bmany-list-bookmarks-with-keyword", arg, ev);
}, "bmany - List bookmarks with keyword");

key.setViewKey([':', 't'], function (ev, arg) {
    ext.exec("bmany-list-bookmarks-with-tag", arg, ev);
}, "bmany - List bookmarks with tag");

// ==================== tanything

key.setViewKey(['C-x', 'b'], function (ev, arg) {
                   ext.exec("tanything", arg);
               }, "view all tabs", true);

// plugins.options["tanything_opt.keymap"] = {
//     "C-z"   : "prompt-toggle-edit-mode",
//     "SPC"   : "prompt-next-page",
//     "b"     : "prompt-previous-page",
//     "j"     : "prompt-next-completion",
//     "k"     : "prompt-previous-completion",
//     "g"     : "prompt-beginning-of-candidates",
//     "G"     : "prompt-end-of-candidates",
//     "D"     : "prompt-cancel",
//     // Tanything specific actions
//     "O"     : "localOpen",
//     "q"     : "localClose",
//     "p"     : "localLeftclose",
//     "n"     : "localRightclose",
//     "a"     : "localAllclose",
//     "d"     : "localDomainclose",
//     "c"     : "localClipUT",
//     "C"     : "localClipU",
//     "e"     : "localMovetoend"
// };

// ==================== site local keymap

key.setGlobalKey("C-;", function (ev, arg) {
    ext.exec("site-local-keymap-toggle-status", arg, ev);
}, 'Site local keymap', true);

var local = {};
plugins.options["site_local_keymap.local_keymap"] = local;
plugins.options["site_local_keymap.disable_in_textarea"] = true;

function fake(k, i) function () { key.feed(k, i); };
function pass(k, i) [k, fake(k, i)];
function ignore(k, i) [k, null];

local["^https?://github.com/"] = [
    pass(['g', 'i']),
    pass(['g', 'c']),
    pass(['g', 'p'])
    // navigation
    ["j", null],
    ["k", null],
    ["?", null],
    ["x", null],
    ["o", null],
    // actions
    ["c", null],
    ["l", null],
    ["u", null],
    ["/", null],
    ["r", null]
];

local["^https?://twitter.com/"] = [
    pass(['g', 'h']),
    pass(['g', 'c']),
    pass(['g', 'r']),
    pass(['g', 'd']),
    pass(['g', 'p']),
    pass(['g', 'f']),
    pass(['g', 'l']),
    pass(['g', 'm']),
    pass(['g', 's']),
    pass(['g', 'u']),
    // navigation
    ["j", null],
    ["k", null],
    ["?", null],
    [".", null],
    ["o", null],
    // actions
    ["n", null],
    ["f", null],
    ["r", null],
    ["t", null],
    ["b", null],
    ["u", null],
    ["l", null],
    ["/", null]
];

local["^https?://mail.google.com/mail/"] = [
    pass(['g', 'i']),
    pass(['g', 's']),
    pass(['g', 't']),
    pass(['g', 'd']),
    pass(['g', 'a']),
    pass(['g', 'c']),
    pass(['g', 'k']),
    pass(['g', 'l']),
    // thread list
    pass(['*', 'a']),
    pass(['*', 'n']),
    // pass(['*', 'r']),
    pass(['*', 'u']),
    pass(['*', 's']),
    pass(['*', 't']),
    // navigation
    ['u', null],
    ['k', null],
    ['j', null],
    ['o', null],
    ['p', null],
    ['n', null],
    // application
    ['c', null],
    ['/', null],
    ['q', null],
    ['?', null],
    // manipulation
    ['x', null],
    ['s', null],
    ['y', null],
    ['e', null],
    ['m', null],
    ['!', null],
    ['#', null],
//    ['r', null],
    ['R', null],
    ['a', null],
    ['A', null],
    ['f', null],
    ['F', null],
    ['N', null],
    pass(['<tab>', 'RET']),
    ['ESC', null],
    [']', null],
    ['[', null],
    ['z', null],
    ['.', null],
    ['I', null],
    ['U', null],
    ['C-s', null],
    ['T', null]
];

local["^http://www.google.(co.jp|com)/reader/view/"] = [
    // jump
    pass(["g", "h"]),
    pass(["g", "a"]),
    pass(["g", "s"]),
    pass(["g", "S"]),
    pass(["g", "u"]),
    pass(["g", "t"]),
    pass(["g", "T"]),
    pass(["g", "d"]),
    pass(["g", "f"]),
    pass(["g", "F"]),
    pass(["g", "c"]),
    pass(["g", "C"]),
    pass(["g", "e"]),
    pass(["g", "p"]),
    // navigation
    ["j", null],
    ["k", null],
    ["n", null],
    ["p", null],
    ["N", null],
    ["P", null],
    ["X", null],
    ["o", null],
    // item
    ["s", null],
    ["L", null],
    ["t", null],
    ["e", null],
    ["S", null],
    ["d", null],
    ["v", null],
    ["o", null],
    ["c", null],
    ["C", null],
    ["m", null],
    ["A", null],
    ["T", null],
    // application
    ["r", null],
    ["u", null],
    ["1", null],
    ["2", null],
    ["/", null],
    ["a", null],
    ["=", null],
    ["-", null]
];

// ==================== read-it-later/pocket

key.setViewKey(['C-c', 'p', 't'], function (aEvent, aArg) {
    ext.exec("ril-toggle", aArg);
}, 'Start continuous HaH', true);
