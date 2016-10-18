{ pkgs ? import <nixpkgs> {} }:

let
  myEmacs = pkgs.emacs;
  emacsWithPackages = (pkgs.emacsPackagesNgGen myEmacs).emacsWithPackages;
in
  emacsWithPackages (epkgs: (with epkgs.melpaStablePackages; [
    magit          # ; Integrate git <C-x g>
    zerodark-theme # ; Nicolas' theme
    jabber
  ]) ++ (with epkgs.melpaPackages; [
    jabber-otr
    repl-toggle
    undo-tree      # ; <C-x u> to show the undo tree
    zoom-frm       # ; increase/decrease font size for all buffers %lt;C-x C-+>
    offlineimap
    flycheck
    flycheck-pos-tip
    markdown-toc
    org-trello
    org2jekyll
    ox-reveal
    auto-complete
    ac-haskell-process
    company
    ace-jump-mode
    exec-path-from-shell
    magit
    projectile
    switch-window
    smart-mode-line
    undo-tree
    use-package
    el-get
    dash
    dash-functional
    s
    ht
    deferred
    diminish
    popup
    helm
    helm-notmuch
    yasnippet
    ag
    change-inner
    circe
    expand-region
    hi2
    el-mock
    goto-last-change
    aggressive-indent
    crux
    smart-mode-line
    color-theme
    spacemacs-theme
    emr
    ert-expectations
    idris-mode
    monokai-theme
    smartparens
    volatile-highlights
    wgrep
    flymake-shell
    flymake-hlint
    projectile
    iedit
    ace-window
    overseer
    move-text
    dockerfile-mode
    htmlize
    google-this
    multiple-cursors
    buffer-move
    mediawiki
    ox-mediawiki
    deferred
    ghci-completion
    haskell-mode
    shm
    ac-math
    git-gutter
    fullframe
    monky
    highlight
    paredit
    fold-dwim
    async
    google-contacts
    restclient
    conkeror-minor-mode
    eval-sexp-fu
    cider
    clojure-mode
    clj-refactor
    yasnippet
    company
    cljsbuild-mode
    tuareg
    # merlin
    cask
    dash
    dash-functional
    s
    diminish
    nix-mode
    helm-nixos-options
    company-nixos-options
    exec-path-from-shell
    popwin
    twittering-mode
    puppet-mode
    eclim
    company-emacs-eclim
    smartscan
    php-mode
    groovy-mode
    weechat
    creds
    guide-key
    marmalade-client
    purescript-mode
    psci
    idris-mode
    python-mode
    elpy
    graphviz-dot-mode
    powershell
    powerline
  ]) ++ (with epkgs.orgPackages; [
     org-plus-contrib
  ]) ++ (with epkgs.elpaPackages; [
    auctex         # ; LaTeX mode
    beacon         # ; highlight my cursor when scrolling
    nameless       # ; hide current package name everywhere in elisp code
  ]) ++ (with pkgs; [
    notmuch                                  # From main packages set
    offlineimap
    haskellPackages.structured-haskell-mode  # From haskell set
    haskellPackages.hlint
    haskellPackages.hasktags
    haskellPackages.stylish-haskell
    opam
    ocamlPackages.merlin                     # From ocaml set
    ocamlPackages.ocpIndent
    dejavu_fonts
  ]))
