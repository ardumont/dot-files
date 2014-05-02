// Info

let PLUGIN_INFO =
<KeySnailPlugin>
  <name>Firebugnail</name>
  <description>Manipulate Firebug with KeySnail</description>
  <description lang="ja">Firebugを操作</description>
  <updateURL>https://gist.github.com/958/945713/raw/firebug.ks.js</updateURL>
  <iconURL>https://sites.google.com/site/958site/Home/files/firebug.ks.png</iconURL>
  <author>958</author>
  <version>0.0.7</version>
  <license>MIT</license>
  <minVersion>1.8.0</minVersion>
  <include>main</include>
  <detail lang="ja"><![CDATA[
=== 使い方 ===
Firebugを操作します

=== 流用元 ===
Vimperator Firebug.js
 author Eric Van Dewoetine (ervandew@gmail.com)
http://code.google.com/p/vimperator-labs/issues/detail?id=14
]]></detail>
  <detail><![CDATA[
Manipulating Firebug

=== Special thanks ===
Quoten from Vimperator Firebug.js
 author Eric Van Dewoetine (ervandew@gmail.com)
http://code.google.com/p/vimperator-labs/issues/detail?id=14
]]></detail>
</KeySnailPlugin>;

// Option

let pOptions = plugins.setupOptions("firebug", {
  'hok_inspect_node': {
    preset: 'g',
    description: M({
      en: "Key bound to `Inspect node` in the HoK extended hint mode (Default: g)",
      ja: "HoK 拡張ヒントモードにおいて `Inspect node` へ割り当てるキー (デフォルト: g)"
    })
  },
  'hok_copy_xpath' : {
    preset: 'G',
    description: M({
      en: "Key bound to `Copy XPath` in the HoK extended hint mode (Default: G)",
      ja: "HoK 拡張ヒントモードにおいて `Copy XPath` へ割り当てるキー (デフォルト: G)"
    })
  }
}, PLUGIN_INFO);

// Extend HoK

hook.addToHook('PluginLoaded', function () {
  if (!plugins.hok)
    return;

  let actions = [
    [
      pOptions['hok_inspect_node'],
      M({ja: '要素を調査', en: 'Inspect node'}),
      function(e) fbv.inspect(e),
      false, false,
      "*"
    ],
    [
      pOptions['hok_copy_xpath'],
      M({ja: 'XPathをコピー', en: 'Copy XPath'}),
      function(e) fbv.copyXPath(e),
      false, false,
      "*"
    ]
  ];

  plugins.hok.addActions(actions);
});

// Add Ext

function FirebugVimperator(){
  var fbContentBox = document.getElementById('fbContentBox') || document.getElementById('fbMainFrame');
  function getContentBox(callback) {
    if (!fbContentBox)
      (Firebug.GlobalUI || Firebug.browserOverlay).startFirebug(function() {
        fbContentBox = document.getElementById('fbContentBox') || document.getElementById('fbMainFrame')
        callback();
      });
    else
      callback();
  }

  return {
    open: function(){
      getContentBox(function() {
        if (fbContentBox.collapsed)
          Firebug.toggleBar(true, 'console');
      });
    },

    off: function() {
      Firebug.closeFirebug(true);
    },

    close: function(){
      getContentBox(function() {
        if (!fbContentBox.collapsed)
          Firebug.toggleBar();
      });
    },

    toggle: function(){
      getContentBox(function() {
        if (fbContentBox.collapsed){
         fbv.open();
        }else{
          fbv.close();
        }
      });
    },

    console_focus: function(){
      getContentBox(function() {
        if (fbContentBox.collapsed){
          fbv.open();
        }
        Firebug.chrome.switchToPanel(window, "console");
        /*
        var commandLine = Firebug.chrome.$("fbCommandEditor");
        setTimeout(function(){
          commandLine.focus();
        }, 100);
        */
        if (Firebug.commandEditor)
          Firebug.CommandEditor.focus();
        else
          Firebug.CommandLine.focus();
      });
    },

    console_clear: function(){
      getContentBox(function() {
        if (!fbContentBox.collapsed){
          Firebug.Console.clear();
        }
      });
    },

    tab: function(args){
      getContentBox(function() {
        let f = function(name) {
          fbv.open();
          if (name == 'css'){
            name = 'stylesheet';
          }
          fbv._focusPanel(name);
        };

        if (args.length > 0 && typeof args[0] === 'string' && args[0].length > 1) {
          f(args[0].toLowerCase());
        } else {
          let panels = fbv._getPanelNames();
          prompt.selector( {
            message    : "Select tab:",
            collection : panels,
            callback   : function(i) { f(panels[i]); }
            });
        }
      });
    },
/*
    tabnext: function(args){
      try{
        fbv.open();
        fbv._gotoNextPrevTabName((args.length > 1 && typeof args[1] === 'number') ? args[1] : 1, false);
      }catch(e){}
    },

    tabprevious: function(args){
      try{
        fbv.open();
        fbv._gotoNextPrevTabName((args.length > 1 && typeof args[1] === 'number') ? args[1] : 1, true);
      }catch(e){}
    },
*/
    _execute: function(args){
      var name = args.length ? args.shift().replace('-', '_') : 'open';
      var cmd = fbv[name];
      if (!cmd) {
        util.message('Unsupported firebug command: ' + name);
        return false;
      }
      return cmd(args);
    },

    _gotoNextPrevTabName: function(count, previous){
      var names = fbv._getPanelNames();
      var browser = FirebugChrome.getCurrentBrowser();
      var index = names.indexOf(browser.chrome.getSelectedPanel().name);
      count = count % names.length;
      if(previous){
        index = index - count;
        if (index < 0){
          index += names.length;
        }
      }else{
        index = index + count;
        if (index >= names.length){
          index -= names.length;
        }
      }
      fbv.tab([names[index]]);
    },

    _getPanelNames: function(){
      var panels = Firebug.getMainPanelTypes(window);
      var ret = [];

      for each (p in panels) {
        let name = p.prototype.name;
        ret.push(name);
      }
      return ret;
    },
    _focusPanel: function(name){
      var browser = window.Firebug;
      browser.chrome.selectPanel(name);
      browser.chrome.syncPanel();
      Firebug.showBar(true);
      return true;
    },
    _commands: function() {
      var commands = [];
      for (var name in fbv){
        if (name.indexOf('_') !== 0 && fbv.hasOwnProperty(name)){
          commands.push(name.replace('_', '-'));
        }
      }
      return commands;
    },

    _completer: function(args, extra){
      let commands = fbv._commands();
      let ret = completer.matcher.header(commands)(extra.query || "");
      return ret;
    },

    inspect: function(e){
      getContentBox(function() {
        Firebug.Inspector.inspectFromContextMenu(e)
      });
    },

    copyXPath: function(e) {
      getContentBox(function() {
        command.setClipboardText(FBL.getElementXPath(e));
      });
    },

  };
}

var fbv = new FirebugVimperator();
/*
shell.add(['firebug'],
  'Control firebug from within vimperator.',
  function(args) { fbv._execute(args); },
  { argCount: '*', completer: fbv._completer },
  true
);
*/
plugins.withProvides(function (provide) {
  (function(){
    let ret = [];
    fbv._commands().forEach(function(v) {
      ret.push([
        'firebug-' + v,
        function(ev, arg) { fbv._execute([v, '', arg]); },
        'Firebug - ' + v
      ]);
    });
    return ret;
  })().forEach(function(row) {
    provide(row[0], row[1], row[2]);
  });
}, PLUGIN_INFO);

// vim: expandtab sw=2 sts=2
