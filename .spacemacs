;; -*- mode: emacs-lisp -*-
;; This file is loaded by Spacemacs at startup.
;; It must be stored in your home directory.

(defun dotspacemacs/layers ()
  "Configuration Layers declaration.
You should not put any user code in this function besides modifying the variable
values."
  (setq-default
   ;; Base distribution to use. This is a layer contained in the directory
   ;; `+distribution'. For now available distributions are `spacemacs-base'
   ;; or `spacemacs'. (default 'spacemacs)
   dotspacemacs-distribution 'spacemacs
   ;; Lazy installation of layers (i.e. layers are installed only when a file
   ;; with a supported type is opened). Possible values are `all', `unused'
   ;; and `nil'. `unused' will lazy install only unused layers (i.e. layers
   ;; not listed in variable `dotspacemacs-configuration-layers'), `all' will
   ;; lazy install any layer that support lazy installation even the layers
   ;; listed in `dotspacemacs-configuration-layers'. `nil' disable the lazy
   ;; installation feature and you have to explicitly list a layer in the
   ;; variable `dotspacemacs-configuration-layers' to install it.
   ;; (vimscript
   ;; default 'unused
   dotspacemacs-enable-lazy-installation 'unused
   ;; If non-nil then Spacemacs will ask for confirmation before installing
   ;; a layer lazily. (default t)
   dotspacemacs-ask-for-lazy-installation t
   ;; List of additional paths where to look for configuration layers.
   ;; Paths must have a trailing slash (i.e. `~/.mycontribs/')
   dotspacemacs-configuration-layer-path '("~/.emacs.d/private/")
   ;; List of configuration layers to load.
   dotspacemacs-configuration-layers
   '(typescript
     html
     csv
     rust
     windows-scripts
     nginx
     sql
     ;; ----------------------------------------------------------------
     ;; Example of useful layers you may want to use right away.
     ;; Uncomment some layer names and press <SPC f e R> (Vim style) or
     ;; <M-m f e R> (Emacs style) to install them.
     ;; ----------------------------------------------------------------
     (auto-completion :variables
                      company-idle-delay 0.5
                      auto-completion-enable-snippets-in-popup t
                      auto-completion-enable-sort-by-usage t
                      auto-completion-return-key-behavior 'complete
                      auto-completion-tab-key-behavior 'cycle
                      auto-completion-complete-with-key-sequence "jk")
     ;; better-defaults
     emacs-lisp
     react
     git
     ;; github
     clojure
     (tern :variables tern-command '("/home/asok/.nvm/versions/node/v10.16.3/bin/tern"))
     (javascript :variables javascript-backend nil)
     (org :variables
          org-enable-jira-support t
          jiralib-url "https://gabi-com.atlassian.net")
     markdown
     dash
     themes-megapack
     ;; (lsp
     ;;  :variables
     ;;      lsp-ui-sideline-show-symbol   t
     ;;      lsp-ui-sideline-enable        nil
     ;;      lsp-ui-doc-enable             nil
     ;;      lsp-ui-remap-xref-keybindings t
     ;;      )
     (ruby :variables
           ruby-version-manager 'chruby
           ruby-test-runner     'rspec
           ;; ruby-backend         'lsp
           )
     ruby-on-rails
     (shell :variables
            shell-default-term-shell "/bin/zsh"
            shell-default-shell 'vterm)
     yaml
     chrome
     (syntax-checking :variables flycheck-disabled-checkers '(emacs-lisp-checkdoc))
     ranger
     slack
     elixir
     ivy
     (elfeed :variables
             elfeed-feeds '("http://planet.emacsen.org/atom.xml"
                            ))
     (slack :variables
            slack-prefer-current-team t
            slack-display-team-name nil)
     html
     restclient
     docker
     (mu4e :variables
           mu4e-enable-async-operations t
           mu4e-attachment-dir "~/Downloads"
           mu4e-compose-signature-auto-include nil
           mu4e-drafts-folder "/gmail/Drafts"
           mu4e-get-mail-command "mbsync -a"
           mu4e-maildir "~/Maildir"
           mu4e-refile-folder "/gmail/Archive"
           mu4e-sent-folder "/gmail/Sent Mail"
           mu4e-maildir-shortcuts
           '(("/gmail/INBOX" . ?i)
             ("/gmail/All Mail" . ?a)
             ("/gmail/Deleted Items" . ?d)
             ("/gmail/Drafts" . ?D)
             ("/gmail/Important" . ?i)
             ("/gmail/Sent Mail" . ?s)
             ("/gmail/Starred" . ?S))
           mu4e-trash-folder "/gmail/Trash"
           mu4e-update-interval 300
           mu4e-use-fancy-chars t
           mu4e-view-show-addresses t
           mu4e-view-show-images t)
     )


   ;; List of additional packages that will be installed without being
   ;; wrapped in a layer.  If you need some configuration for these
   ;; packages, then consider creating a layer. You can also put the
   ;; configuration in `dotspacemacs/user-config'.
   dotspacemacs-additional-packages '(fullframe
                                      beacon
                                      ;; epc
                                      ;; ert-runner
                                      inf-ruby
                                      tramp-term
                                      wttrin
                                      metalheart-theme
                                      rainbow-mode
                                      rjsx-mode
                                      madhat2r-theme
                                      all-the-icons-dired
                                      all-the-icons
                                      evil-lion
                                      doom-themes
                                      ruby-hash-syntax
                                      counsel-dash
                                      nubox
                                      nodejs-repl
                                      helpful
                                      enh-ruby-mode
                                      flycheck-flow
                                      vlf
                                      sunburn-theme
                                      es-mode
                                      powerthesaurus
                                      company-tabnine
                                      nord-theme
                                      better-jumper
                                      ivy-posframe
                                      evil-vimish-fold
                                      leuven-theme
                                      nvm
                                      jest-test-mode
                                      ivy-prescient
                                      base16-theme
                                      )
   ;; A list of packages and/or extensions that will not be install and loaded.
   dotspacemacs-excluded-packages '()
   ;; If non-nil spacemacs will delete any orphan packages, i.e. packages that
   ;; are declared in a layer which is not a member of
   ;; the list `dotspacemacs-configuration-layers'. (default t)
   dotspacemacs-delete-orphan-packages t))

(defun dotspacemacs/init ()
  "Initialization function.
This function is called at the very startup of Spacemacs initialization
before layers configuration.
You should not put any user code in there besides modifying the variable
values."
  ;; This setq-default sexp is an exhaustive list of all the supported
  ;; spacemacs settings.
  (setq-default
   ;; If non nil ELPA repositories are contacted via HTTPS whenever it's
   ;; possible. Set it to nil if you have no way to use HTTPS in your
   ;; environment, otherwise it is strongly recommended to let it set to t.
   ;; This variable has no effect if Emacs is launched with the parameter
   ;; `--insecure' which forces the value of this variable to nil.
   ;; (default t)
   dotspacemacs-elpa-https t
   ;; Maximum allowed time in seconds to contact an ELPA repository.
   dotspacemacs-elpa-timeout 5


   ;; Set `gc-cons-threshold' and `gc-cons-percentage' when startup finishes.
   ;; This is an advanced option and should not be changed unless you suspect
   ;; performance issues due to garbage collection operations.
   ;; (default '(100000000 0.1))
   dotspacemacs-gc-cons '(100000000 0.1)

   ;; If non-nil then Spacelpa repository is the primary source to install
   ;; a locked version of packages. If nil then Spacemacs will install the
   ;; latest version of packages from MELPA. (default nil)
   dotspacemacs-use-spacelpa nil

   ;; If non-nil then verify the signature for downloaded Spacelpa archives.
   ;; (default nil)
   dotspacemacs-verify-spacelpa-archives nil

   ;; If non-nil then spacemacs will check for updates at startup
   ;; when the current branch is not `develop'. Note that checking for
   ;; new versions works via git commands, thus it calls GitHub services
   ;; whenever you start Emacs. (default nil)
   dotspacemacs-check-for-update nil

   ;; If non-nil, a form that evaluates to a package directory. For example, to
   ;; use different package directories for different Emacs versions, set this
   ;; to `emacs-version'.
   dotspacemacs-elpa-subdirectory nil
   ;; One of `vim', `emacs' or `hybrid'. Evil is always enabled but if the
   ;; variable is `emacs' then the `holy-mode' is enabled at startup. `hybrid'
   ;; uses emacs key bindings for vim's insert mode, but otherwise leaves evil
   ;; unchanged. (default 'vim)
   dotspacemacs-editing-style 'vim
   ;; If non nil output loading progress in `*Messages*' buffer. (default nil)
   dotspacemacs-verbose-loading nil
   ;; Specify the startup banner. Default value is `official', it displays
   ;; the official spacemacs logo. An integer value is the index of text
   ;; banner, `random' chooses a random text banner in `core/banners'
   ;; directory. A string value must be a path to an image format supported
   ;; by your Emacs build.
   ;; If the value is nil then no banner is displayed. (default 'official)
   dotspacemacs-startup-banner 'random
   ;; List of items to show in the startup buffer. If nil it is disabled.
   ;; Possible values are: `recents' `bookmarks' `projects'.
   ;; (default '(recents projects))
   dotspacemacs-startup-lists '((recents . 10))
   ;; True if the home buffer should respond to resize events.
   dotspacemacs-startup-buffer-responsive t
   ;; Number of recent files to show in the startup buffer. Ignored if
   ;; `dotspacemacs-startup-lists' doesn't include `recents'. (default 5)
   dotspacemacs-startup-recent-list-size 5
   ;; Default major mode of the scratch buffer (default `text-mode')
   dotspacemacs-scratch-mode 'text-mode

   ;; Initial message in the scratch buffer, such as "Welcome to Spacemacs!"
   ;; (default nil)
   dotspacemacs-initial-scratch-message nil

   ;; List of themes, the first of the list is loaded when spacemacs starts.
   ;; Press `SPC T n' to cycle to the next theme in the list (works great
   ;; with 2 themes variants, one dark and one light)
   dotspacemacs-themes '(
                         base16-tomorrow
                         doom-tomorrow-day
                         twilight-bright
                         flatui
                         doom-spacegrey
                         solarized-light
                         solarized-dark
                         spacemacs-light
                         doom-city-lights
                         doom-nord
                         doom-spacegrey
                         doom-one
                         madhat2r
                         gotham
                         ;; brin
                         flatland
                         birds-of-paradise-plus
                         twilight
                         sanityinc-tomorrow-night
                         junio
                         spacemacs-dark
                         spacemacs-light
                         leuven
                         monokai
                         zenburn)
   ;; Set the theme for the Spaceline. Supported themes are `spacemacs',
   ;; `all-the-icons', `custom', `doom', `vim-powerline' and `vanilla'. The
   ;; first three are spaceline themes. `doom' is the doom-emacs mode-line.
   ;; `vanilla' is default Emacs mode-line. `custom' is a user defined themes,
   ;; refer to the DOCUMENTATION.org for more info on how to create your own
   ;; spaceline theme. Value can be a symbol or list with additional properties.
   ;; (default '(spacemacs :separator wave :separator-scale 1.5))
   dotspacemacs-mode-line-theme 'doom

   ;; If non nil the cursor color matches the state color in GUI Emacs.
   dotspacemacs-colorize-cursor-according-to-state t
   ;; Default font. `powerline-scale' allows to quickly tweak the mode-line
   ;; size to make separators look not too crappy.
   dotspacemacs-default-font `("Iosevka Term"
                               :size ,(if (<= (x-display-pixel-width) 1920)
                                          13.0
                                        13.0)
                               :weight normal
                               :width normal
                               :powerline-scale 1.0)
   ;; The leader key
   dotspacemacs-leader-key "SPC"

   ;; The key used for Emacs commands `M-x' (after pressing on the leader key).
   ;; (default "SPC")
   dotspacemacs-emacs-command-key "SPC"

   ;; The key used for Vim Ex commands (default ":")
   dotspacemacs-ex-command-key ":"

   ;; The leader key accessible in `emacs state' and `insert state'
   ;; (default "M-m")
   dotspacemacs-emacs-leader-key "M-m"
   ;; Major mode leader key is a shortcut key which is the equivalent of
   ;; pressing `<leader> m`. Set it to `nil` to disable it. (default ",")
   dotspacemacs-major-mode-leader-key ","
   ;; Major mode leader key accessible in `emacs state' and `insert state'.
   ;; (default "C-M-m)
   dotspacemacs-major-mode-emacs-leader-key "C-M-m"
   ;; These variables control whether separate commands are bound in the GUI to
   ;; the key pairs C-i, TAB and C-m, RET.
   ;; Setting it to a non-nil value, allows for separate commands under <C-i>
   ;; and TAB or <C-m> and RET.
   ;; In the terminal, these pairs are generally indistinguishable, so this only
   ;; works in the GUI. (default nil)
   dotspacemacs-distinguish-gui-tab t
   ;; (Not implemented) dotspacemacs-distinguish-gui-ret nil
   ;; The command key used for Evil commands (ex-commands) and
   ;; Emacs commands (M-x).
   ;; By default the command key is `:' so ex-commands are executed like in Vim
   ;; with `:' and Emacs commands are executed with `<leader> :'.
   dotspacemacs-command-key ":"
   ;; If non nil `Y' is remapped to `y$'. (default t)
   dotspacemacs-remap-Y-to-y$ t
   ;; If non-nil, inverse the meaning of `g' in `:substitute' Evil ex-command.
   ;; (default nil)
   ;; Name of the default layout (default "Default")
   dotspacemacs-ex-substitute-global t
   dotspacemacs-default-layout-name "Default"
   ;; If non nil the default layout name is displayed in the mode-line.
   ;; (default nil)
   dotspacemacs-display-default-layout nil
   ;; If non nil then the last auto saved layouts are resume automatically upon
   ;; start. (default nil)
   dotspacemacs-auto-resume-layouts nil

   ;; If non-nil, auto-generate layout name when creating new layouts. Only has
   ;; effect when using the "jump to layout by number" commands. (default nil)
   dotspacemacs-auto-generate-layout-names nil

   ;; Size (in MB) above which spacemacs will prompt to open the large file
   ;; literally to avoid performance issues. Opening a file literally means that
   ;; no major mode or minor modes are active. (default is 1)
   dotspacemacs-large-file-size 1

   ;; Location where to auto-save files. Possible values are `original' to
   ;; auto-save the file in-place, `cache' to auto-save the file to another
   ;; file stored in the cache directory and `nil' to disable auto-saving.
   ;; (default 'cache)
   dotspacemacs-auto-save-file-location 'cache
   ;; Maximum number of rollback slots to keep in the cache. (default 5)
   dotspacemacs-max-rollback-slots 5

   ;; If non-nil, the paste transient-state is enabled. While enabled, after you
   ;; paste something, pressing `C-j' and `C-k' several times cycles through the
   ;; elements in the `kill-ring'. (default nil)
   dotspacemacs-enable-paste-transient-state nil

   ;; Which-key delay in seconds. The which-key buffer is the popup listing
   ;; the commands bound to the current keystroke sequence. (default 0.4)
   dotspacemacs-which-key-delay 0.8
   ;; Which-key frame position. Possible values are `right', `bottom' and
   ;; `right-then-bottom'. right-then-bottom tries to display the frame to the
   ;; right; if there is insufficient space it displays it at the bottom.
   ;; (default 'bottom)
   dotspacemacs-which-key-position 'bottom
   ;; Control where `switch-to-buffer' displays the buffer. If nil,
   ;; `switch-to-buffer' displays the buffer in the current window even if
   ;; another same-purpose window is available. If non-nil, `switch-to-buffer'
   ;; displays the buffer in a same-purpose window even if the buffer can be
   ;; displayed in the current window. (default nil)
   dotspacemacs-switch-to-buffer-prefers-purpose nil
   ;; If non nil a progress bar is displayed when spacemacs is loading. This
   ;; may increase the boot time on some systems and emacs builds, set it to
   ;; nil to boost the loading time. (default t)
   dotspacemacs-loading-progress-bar nil
   ;; If non nil the frame is fullscreen when Emacs starts up. (default nil)
   ;; (Emacs 24.4+ only)
   dotspacemacs-fullscreen-at-startup nil
   ;; If non nil `spacemacs/toggle-fullscreen' will not use native fullscreen.
   ;; Use to disable fullscreen animations in OSX. (default nil)
   dotspacemacs-fullscreen-use-non-native nil
   ;; If non nil the frame is maximized when Emacs starts up.
   ;; Takes effect only if `dotspacemacs-fullscreen-at-startup' is nil.
   ;; (default nil) (Emacs 24.4+ only)
   dotspacemacs-maximized-at-startup t
   ;; A value from the range (0..100), in increasing opacity, which describes
   ;; the transparency level of a frame when it's active or selected.
   ;; Transparency can be toggled through `toggle-transparency'. (default 90)
   dotspacemacs-active-transparency 90
   ;; A value from the range (0..100), in increasing opacity, which describes
   ;; the transparency level of a frame when it's inactive or deselected.
   ;; Transparency can be toggled through `toggle-transparency'. (default 90)
   dotspacemacs-inactive-transparency 90
   ;; If non-nil show the titles of transient states. (default t)
   dotspacemacs-show-transient-state-title t
   ;; If non-nil show the color guide hint for transient state keys. (default t)
   dotspacemacs-show-transient-state-color-guide t

   ;; If non-nil unicode symbols are displayed in the mode line.
   ;; If you use Emacs as a daemon and wants unicode characters only in GUI set
   ;; the value to quoted `display-graphic-p'. (default t)
   dotspacemacs-mode-line-unicode-symbols t
   ;; If non nil smooth scrolling (native-scrolling) is enabled. Smooth
   ;; scrolling overrides the default behavior of Emacs which recenters the
   ;; point when it reaches the top or bottom of the screen. (default t)
   dotspacemacs-smooth-scrolling nil
   ;; If non nil line numbers are turned on in all `prog-mode' and `text-mode'
   ;; derivatives. If set to `relative', also turns on relative line numbers.
   ;; (default nil)
   dotspacemacs-line-numbers nil
   ;; Code folding method. Possible values are `evil' and `origami'.
   ;; (default 'evil)
   dotspacemacs-folding-method 'evil

   ;; If non-nil `smartparens-strict-mode' will be enabled in programming modes.
   ;; (default nil)
   dotspacemacs-smartparens-strict-mode nil
   ;; Select a scope to highlight delimiters. Possible values are `any',
   ;; `current', `all' or `nil'. Default is `all' (highlight any scope and
   ;; emphasis the current one). (default 'all)
   dotspacemacs-highlight-delimiters 'all
   ;; If non-nil pressing the closing parenthesis `)' key in insert mode passes
   ;; over any automatically added closing parenthesis, bracket, quote, etc…
   ;; This can be temporary disabled by pressing `C-q' before `)'. (default nil)
   dotspacemacs-smart-closing-parenthesis nil

   ;; Select a scope to highlight delimiters. Possible values are `any',
   ;; `current', `all' or `nil'. Default is `all' (highlight any scope and
   ;; emphasis the current one). (default 'all)
   dotspacemacs-highlight-delimiters 'all

   ;; If non-nil, start an Emacs server if one is not already running.
   ;; (default nil)
   dotspacemacs-enable-server nil

   ;; Set the emacs server socket location.
   ;; If nil, uses whatever the Emacs default is, otherwise a directory path
   ;; like \"~/.emacs.d/server\". It has no effect if
   ;; `dotspacemacs-enable-server' is nil.
   ;; (default nil)
   dotspacemacs-server-socket-dir nil

   ;; If non-nil, advise quit functions to keep server open when quitting.
   ;; (default nil)
   dotspacemacs-persistent-server nil
   ;; List of search tool executable names. Spacemacs uses the first installed
   ;; tool of the list. Supported tools are `rg', `ag', `pt', `ack' and `grep'.
   ;; (default '("rg" "ag" "pt" "ack" "grep"))
   dotspacemacs-search-tools '("rg" "ag" "pt" "ack" "grep")

   ;; Format specification for setting the frame title.
   ;; %a - the `abbreviated-file-name', or `buffer-name'
   ;; %t - `projectile-project-name'
   ;; %I - `invocation-name'
   ;; %S - `system-name'
   ;; %U - contents of $USER
   ;; %b - buffer name
   ;; %f - visited file name
   ;; %F - frame name
   ;; %s - process status
   ;; %p - percent of buffer above top of window, or Top, Bot or All
   ;; %P - percent of buffer above bottom of window, perhaps plus Top, or Bot or All
   ;; %m - mode name
   ;; %n - Narrow if appropriate
   ;; %z - mnemonics of buffer, terminal, and keyboard coding systems
   ;; %Z - like %z, but including the end-of-line format
   ;; (default "%I@%S")
   dotspacemacs-frame-title-format "%I@%S"

   ;; Format specification for setting the icon title format
   ;; (default nil - same as frame-title-format)
   dotspacemacs-icon-title-format nil

   ;; Delete whitespace while saving buffer. Possible values are `all'
   ;; to aggressively delete empty line and long sequences of whitespace,
   ;; `trailing' to delete only the whitespace at end of lines, `changed' to
   ;; delete only whitespace for changed lines or `nil' to disable cleanup.
   ;; (default nil)
   dotspacemacs-whitespace-cleanup 'trailing

   ;; Either nil or a number of seconds. If non-nil zone out after the specified
   ;; number of seconds. (default nil)
   dotspacemacs-zone-out-when-idle nil

   ;; Run `spacemacs/prettify-org-buffer' when
   ;; visiting README.org files of Spacemacs.
   ;; (default nil)
   dotspacemacs-pretty-docs nil
   ))

(defun dotspacemacs/user-env ()
  "Environment variables setup.
This function defines the environment variables for your Emacs session. By
default it calls `spacemacs/load-spacemacs-env' which loads the environment
variables declared in `~/.spacemacs.env' or `~/.spacemacs.d/.spacemacs.env'.
See the header of this file for more information."
  (spacemacs/load-spacemacs-env))

(defun dotspacemacs/user-init ()
  "Initialization function for user code.
It is called immediately after `dotspacemacs/init'.  You are free to put almost
any user code here.  The exception is org related code, which should be placed
in `dotspacemacs/user-config'."
  (setq-default
   ;; js2-mode
   js2-basic-offset 2
   js-indent-level 2
   ;; web-mode
   css-indent-offset 2
   web-mode-markup-indent-offset 2
   web-mode-css-indent-offset 2
   web-mode-code-indent-offset 2
   web-mode-attr-indent-offset 2
   clojure-defun-style-default-indent t
   evil-escape-key-sequence nil
   evil-symbol-word-search t
   evil-search-module 'isearch
   evil-want-fine-undo t
   evil-jumps-cross-buffers nil
   wgrep-auto-save-buffer t
   git-commit-finish-query-functions '()
   init-file-debug t
   rspec-autosave-buffer t
   projectile-enable-caching t
   projectile-indexing-method 'alien
   shell-pop-autocd-to-working-dir nil
   doc-view-continuous t
   confirm-kill-emacs 'y-or-n-p
   auto-window-vscroll nil
   better-jumper-context 'buffer
   lsp-auto-guess-root t
   lsp-eldoc-render-all nil
   lsp-eldoc-enable-hover nil
   vterm-max-scrollback 10000
   read-process-output-max (* 1024 1024))

  (spacemacs/add-to-hooks '(lambda ()
                             (setq-local compilation-always-kill t))
                          '(rspec-mode-hook rspec-verifiable-mode-hook rspec-compilation-mode-hook))

  (add-hook 'rspec-compilation-mode-hook '(lambda ()
                                            (setq-local compilation-scroll-output nil)))

   (setq configuration-layer--elpa-archives
    '(("melpa"    . "melpa.org/packages/")
        ("org"      . "orgmode.org/elpa/")
        ("gnu"      . "elpa.gnu.org/packages/")))

  ;; (with-eval-after-load 'web-mode
  ;;   (add-to-list 'web-mode-indentation-params '("lineup-args" . nil))
  ;;   (add-to-list 'web-mode-indentation-params '("lineup-concats" . nil))
  ;;   (add-to-list 'web-mode-indentation-params '("lineup-calls" . nil)))

  (setq slack-enable-emoji t)

  (when (file-exists-p "~/bin/protool/elisp")
    (add-to-list 'load-path "~/bin/protool/elisp"))

  )

(defun dotspacemacs/user-config ()
  "Configuration function for user code.
This function is called at the very end of Spacemacs initialization after
layers configuration. You are free to put any user code."
  (setq-default
   beacon-blink-when-focused t
   beacon-blink-when-point-moves-vertically nil
   ivy-use-virtual-buffers t
   ivy-re-builders-alist '((t      . ivy--regex-plus))
   ivy-extra-directories '("./")
   ivy-height 25
   wttrin-default-cities '("Łódź")
   js2-mode-show-parse-errors nil
   js2-mode-show-strict-warnings nil
   magit-save-repository-buffers 'dontask
   neo-theme 'icons
   ns-right-option-modifier nil
   explicit-shell-file-name "/bin/bash"
   evil-ex-search-highlight-all nil
   enh-ruby-deep-indent-paren t
   vc-handled-backends nil
   markdown-hide-urls t
   lsp-prefer-capf t
   lsp-solargraph-use-bundler nil
   bidi-paragraph-direction 'left-to-right)
  )

(defun dotspacemacs/user-load ()
  "Library to load while dumping.
This function is called only while dumping Spacemacs configuration. You can
`require' or `load' the libraries of your choice that will be included in the
dump."
  )

(defun dotspacemacs/user-config ()
  "Configuration for user code:
This function is called at the very end of Spacemacs startup, after layer
configuration.
Put your configuration code here, except for variables that should be set
before packages are loaded."

  (setq
   ivy-initial-inputs-alist nil
   bidi-inhibit-bpa t)

  (evil-define-key 'normal global-map (kbd "C-M-SPC") #'spacemacs/buffer-transient-state/body)
  (evil-define-key 'normal global-map (kbd "M-SPC") #'ivy-switch-buffer)
  (define-key global-map (kbd "M-SPC") #'ivy-switch-buffer)

  (eval-after-load 'magithub
    '(progn
       (require 'magit-popup)
       ;; (magithub-feature-autoinject t))
       (magithub-feature-autoinject '(commit-browse completion pull-requests-section))
       (define-key magit-magithub-comment-section-map (kbd "SPC") nil)
       (define-key magit-magithub-comment-section-map (kbd "RET") #'magithub-comment-view)
       ))

  ;; (beacon-mode 1)

  (with-eval-after-load 'magit
    (magit-auto-revert-mode 1)
    (setq magit-save-repository-buffers 'dontask)
    (remove-hook 'magit-status-sections-hook 'magit-insert-tags-header)
    (remove-hook 'magit-status-sections-hook 'magit-insert-unpushed-to-pushremote)
    (remove-hook 'magit-status-sections-hook 'magit-insert-unpulled-from-pushremote)
    (remove-hook 'magit-status-sections-hook 'magit-insert-unpulled-from-upstream)
    (remove-hook 'magit-status-sections-hook 'magit-insert-unpushed-to-upstream-or-recent)
    )

  (add-hook 'dired-mode-hook 'all-the-icons-dired-mode)

  (with-eval-after-load 'evil
    (evil-define-key 'visual global-map (kbd "v") #'er/expand-region)

    (evil-define-key 'insert global-map (kbd "C-j") #'yas-expand)
    (evil-define-key 'normal global-map (kbd "C-j") #'yas-expand)
    (evil-define-key 'normal global-map (kbd "M-p") (lambda () (interactive) (set-mark-command 4)))

    (add-to-list 'evil-normal-state-modes 'shell-mode)

    (add-to-list 'evil-emacs-state-modes 'term-mode)


    )

  (spacemacs/set-leader-keys
    "\""  (lambda () (interactive) (eshell t)))

  (with-eval-after-load 'magithub-comment
    (define-key magit-magithub-comment-section-map (kbd "SPC") nil))

  (with-eval-after-load 'shell-pop
    (defun asok/shell-pop-emacs-state-maybe ()
      (if (eq shell-default-shell 'ansi-term)
          (evil-emacs-state)))

    (add-hook 'shell-pop-in-after-hook #'asok/shell-pop-emacs-state-maybe))

  (let ((secrets "~/.secrets.sh"))
    (when (file-exists-p secrets)
      (with-temp-buffer
        (insert-file-contents secrets)
        (while (re-search-forward "\\bexport \\(.+\\)=\".*\"" nil t)
          (add-to-list 'exec-path-from-shell-variables (match-string 1))))))

  (defun asok/at-comment-p ()
    (let ((face (face-at-point t)))
      (or
       (eq face 'font-lock-comment-delimiter-face)
       (eq face 'font-lock-comment-face))))

  (defun asok/sp-change-line ()
    (interactive)
    (call-interactively #'sp-kill-hybrid-sexp)
    (evil-insert-state))

  (defun asok/sp-open-line-below-sexp ()
    (interactive)
    (sp-end-of-sexp)
    (newline-and-indent)
    (evil-insert-state))

  (defun asok/sp-insert-at-the-sexp-end ()
    (interactive)
    (sp-end-of-sexp)
    (evil-insert-state))

  (eval-after-load 'smartparens
    '(progn
       (evil-define-command asok/sp-change-line-command ()
         :repeat t
         (asok/sp-change-line))

       (evil-define-command asok/sp-open-line-below-sexp-command ()
         :repeat t
         (asok/sp-open-line-below-sexp))

       (evil-define-command asok/sp-insert-at-the-sexp-end-command ()
         :repeat t
         (asok/sp-insert-at-the-sexp-end))

       (evil-define-key 'normal smartparens-mode-map
         (kbd "M-r") #'sp-raise-sexp
         (kbd "M-o") #'asok/sp-open-line-below-sexp-command
         (kbd "C") #'asok/sp-change-line-command
         (kbd "D") #'sp-kill-hybrid-sexp)))

  (spacemacs/set-leader-keys
    ":"   #'eval-expression
    "SPC" #'counsel-M-x
    "j k" #'dumb-jump-go
    "j K" #'dumb-jump-go-other-window)


  (define-key spacemacs-default-map (kbd "q Q") nil)
  (define-key spacemacs-default-map (kbd "q q") nil)
  (define-key spacemacs-default-map (kbd "q R") nil)
  (define-key spacemacs-default-map (kbd "q r") nil)

  (with-eval-after-load 'slim-mode
    (add-to-list 'auto-mode-alist '("\\.slime\\'" . slim-mode)))

  ;; (setq spacemacs-default-jump-handlers '(dumb-jump-go evil-goto-definition))

  (with-eval-after-load 'ivy
    (ivy-set-actions
     'counsel-find-file
     `((,(propertize "delete" 'face 'font-lock-warning-face)
        (lambda (x) (delete-file (expand-file-name x ivy--directory)))))))

  (defun asok/sp-wrap-with-curly-braces ()
    (interactive)
    (sp-wrap-with-pair "{")
    (evil-insert 1))

  (defun asok/sp-wrap-with-square-brackets ()
    (interactive)
    (sp-wrap-with-pair "[")
    (evil-insert 1))

  (defun asok/sp-wrap-with-quotes ()
    (interactive)
    (sp-wrap-with-pair "\"")
    (evil-insert 1))

  (spacemacs/set-leader-keys
    "k{" #'asok/sp-wrap-with-curly-braces
    "k[" #'asok/sp-wrap-with-square-brackets
    "k\"" #'asok/sp-wrap-with-quotes)

  (fullframe magit-status magit-mode-quit-window)
  ;; (fullframe magit-log magit-mode-quit-window)
  (fullframe wttrin wttrin-exit)

  (add-hook 'eval-expression-minibuffer-setup-hook #'eldoc-mode)
  (add-hook 'eval-expression-minibuffer-setup-hook #'paredit-mode)

  (defadvice load-theme
      (before theme-dont-propagate activate)
    (mapcar #'disable-theme custom-enabled-themes))

  ;; (defadvice inf-ruby-switch-from-compilation
  ;;     (after asok/switch-inf-ruby-compilation-buffer activate)
  ;;   (switch-to-buffer-other-window (current-buffer)))

  ;; (defun asok/detect-react-buffer ()
  ;;   (string-match-p ".*^import React" (buffer-string)))

  (add-to-list 'magic-mode-alist '("[\s\n]import React" . rjsx-mode))


  (defun asok/bundle--around (fun &rest args)
    "Run FUN from the project root."
    (projectile-with-default-dir (projectile-project-root)
                                 (apply fun args)))

  (advice-add 'bundle-command :around #'asok/bundle--around)
  (advice-add 'bundle-open :around #'asok/bundle--around)

  (add-hook 'dired-mode-hook #'hl-line-mode)

  (global-vi-tilde-fringe-mode -1)

  (add-hook 'prog-mode-hook
            (lambda ()
              ;; turn off `linum-mode' when there are more than 5000 lines
              ;; use `wc -c file' for performance reason
              (if (and (executable-find "wc")
                       (> (string-to-number (shell-command-to-string (format "wc -c %s" (buffer-file-name))))
                          (* 5000 80)))
                  (linum-mode -1))))

  (add-hook 'alchemist-test-report-mode-hook '(lambda ()
                                                (make-local-variable 'truncate-partial-width-windows)
                                                (make-local-variable 'truncate-lines)
                                                (setq truncate-partial-width-windows nil
                                                      truncate-lines nil)))

  (with-eval-after-load 'elixir
    (add-to-list 'elixir-mode-hook
                 (defun auto-activate-ruby-end-mode-for-elixir-mode ()
                   (set (make-variable-buffer-local 'ruby-end-expand-keywords-before-re)
                        "\\(?:^\\|\\s-+\\)\\(?:do\\)")
                   (set (make-variable-buffer-local 'ruby-end-check-statement-modifiers) nil)
                   (ruby-end-mode +1))))

  (with-eval-after-load 'smartparens
    (sp-with-modes '(elixir-mode)
      (sp-local-pair "fn" "end"
                     :when '(("SPC" "RET"))
                     :actions '(insert navigate))
      (sp-local-pair "do" "end"
                     :when '(("SPC" "RET"))
                     :post-handlers '(sp-ruby-def-post-handler)
                     :actions '(insert navigate))))


  (require 'smartparens-ruby)

  (with-eval-after-load 'term
    (evil-define-key 'normal term-raw-map (kbd "i") #'evil-emacs-state)
    (evil-define-key 'normal term-raw-map (kbd "a") '(lambda ()
                                                       (interactive)
                                                       (evil-emacs-state)
                                                       (forward-char 1)))
    (evil-define-key 'emacs term-raw-map (kbd "ESC") #'evil-normal-state))

  (with-eval-after-load 'volatile-highlights
    (vhl/give-advice-to-make-vhl-on-changes evil-paste-after)
    (vhl/give-advice-to-make-vhl-on-changes evil-paste-before)
    (vhl/give-advice-to-make-vhl-on-changes evil-paste-pop))

  (with-eval-after-load 'alchemist-mix
    (defun alchemist-mix-test-this-buffer ()
      "Run the current buffer or a test for the current buffer through mix test."
      (interactive)
      (if (alchemist-utils-test-file-p)
          (alchemist-mix--test-file buffer-file-name)
        (alchemist-project-run-tests-for-current-file)))

    )

  (with-eval-after-load 'ivy-posframe

    (set-face-attribute 'ivy-posframe-border nil :background "black")

    (setq ivy-posframe-border-width 1)

    (setq ivy-posframe-display-functions-alist
          '((swiper                        . ivy-display-function-fallback)
            (spacemacs/search-project-auto . ivy-display-function-fallback)
            (counsel-rg                    . ivy-display-function-fallback)
            (complete-symbol               . ivy-posframe-display-at-point)
            (t                             . ivy-posframe-display-at-frame-center))))

  ;; (ivy-posframe-mode)

  (defun asok/align-hash ()
    (interactive)
    (align-regexp (region-beginning) (region-end) "\\(\\s-*\\)=>"))

  (with-eval-after-load 'compilation-mode
    (defun endless/send-input (input &optional nl)
      "Send INPUT to the current process.
Interactively also sends a terminating newline."
      (interactive "MInput: \nd")
      (let ((string (concat input (if nl "\n"))))
        ;; This is just for visual feedback.
        (let ((inhibit-read-only t))
          (insert-before-markers string))
        ;; This is the important part.
        (process-send-string
         (get-buffer-process (current-buffer))
         string)))

    (defun endless/send-self ()
      "Send the pressed key to the current process."
      (interactive)
      (endless/send-input
       (apply #'string
              (append (this-command-keys-vector) nil))))

    (define-key compilation-mode-map (kbd "C-c i")
      #'endless/send-input)

    (dolist (key '("\C-d" "\C-j" "y" "n"))
      (define-key compilation-mode-map key
        #'endless/send-self)))

  (with-eval-after-load 'counsel-dash
    (add-hook 'rjsx-mode-hook
              (lambda ()
                (setq-local counsel-dash-docsets '("NodeJS" "React"))
                ;; (setq zeal-at-point-docset "ruby,rails")
                ))
    (setq counsel-dash-browser-func 'eww))

  (with-eval-after-load 'vterm
    (defun asok/vterm-c-r ()
      (interactive)
      (vterm-send-key "r" nil nil t))

    (evil-define-key 'insert vterm-mode-map (kbd "C-r") #'asok/vterm-c-r))

  (defun asok/setup-company-lsp-backends ()
    (setq-local company-backends '((company-lsp company-dabbrev))))

  ;; (add-hook 'ruby-mode-hook #'asok/setup-company-lsp-backends)

  (with-eval-after-load 'projectile-rails
    (setq projectile-rails-expand-snippet-with-magic-comment t)

    (add-hook 'projectile-rails-mode-hook
              (lambda ()
                (setq-local counsel-dash-docsets '("Ruby" "Ruby On Rails"))
                (setq zeal-at-point-docset "ruby,rails")))

    (defun rake-rails-db-migrate ()
      (interactive)
      (rake-compile "db:migrate"
                    'projectile-rails-compilation-mode))

    (defun rake-rails-db-migrate-test ()
      (interactive)
      (rake-compile "db:migrate RAILS_ENV=test"
                    'projectile-rails-compilation-mode))

    (defun rake-rails-db-rollback ()
      (interactive)
      (rake-compile "db:rollback"
                    'projectile-rails-compilation-mode))

    (defun rake-rails-db-rollback-test ()
      (interactive)
      (rake-compile "db:rollback RAILS_ENV=test"
                    'projectile-rails-compilation-mode))

    (defun rake-rails-recreate-db ()
      (interactive)
      (rake-compile "db:drop db:create RAILS_ENV=development"
                    'projectile-rails-compilation-mode))

    (defun spring-restart ()
      (interactive)
      (let ((default-directory (projectile-rails-root)))
        (shell-command "spring stop")
        (shell-command "spring restart")))

    (spacemacs|define-jump-handlers web-mode projectile-rails-goto-file-at-point)
    (spacemacs|define-jump-handlers haml-mode projectile-rails-goto-file-at-point)
    (spacemacs|define-jump-handlers slim-mode projectile-rails-goto-file-at-point)

    ;; (add-hook 'projectile-rails-mode-hook #'chruby-use-corresponding)
    )

  (remove-hook 'ruby-mode-hook 'robe-mode)

  (remove-hook 'projectile-rails-mode-hook 'projectile-rails-expand-snippet-maybe)

  (add-hook 'js2-mode-hook
            (lambda ()
              (push '("function" . ?λ) prettify-symbols-alist)))

  ;; (defun asok/paste-and-reload ()
  ;;   (interactive)
  ;;   (evil-paste-after)
  ;;   (web-mode-reload))

  ;; (evil-define-key 'normal web-mode-map (kbd "p") #'asok/paste-and-reload)

  (with-eval-after-load 'rjsx-mode
    (add-hook 'rjsx-mode-hook 'flycheck-mode)
    (with-eval-after-load 'flycheck
      (require 'flycheck-flow)
      (flycheck-add-mode 'javascript-flow 'rjsx-mode))
    (evil-define-key 'normal rjsx-mode-map (kbd "C-d") 'rjsx-delete-creates-full-tag))

  (with-eval-after-load 'swiper
    (evil-define-key 'normal global-map (kbd "C-s") 'counsel-grep-or-swiper)

    (setq counsel-grep-base-command
          "rg -i -M 120 --no-heading --line-number --color never '%s' %s"))

  ;; (spacemacs|disable-company ruby-mode)
  ;; (add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))
  ;; (add-to-list 'default-frame-alist '(ns-appearance . light))

  (with-eval-after-load 'gotham-theme
    (custom-theme-set-faces 'gotham '(js2-object-property ((t (:inherit 'font-lock-type-face))))))

  (add-to-load-path "~/projects/all-the-icons-ivy")
  (require 'all-the-icons-ivy)
  (all-the-icons-ivy-setup)

  (add-to-list 'custom-theme-load-path "~/projects/solo-jazz-emacs-theme/")
  (load-theme 'solo-jazz t)

  ;; (defadvice find-file (before make-directory-maybe (filename &optional wildcards) activate)
  ;;   "Create parent directory if not exists while visiting file."
  ;;   (unless (file-exists-p filename)
  ;;     (let ((dir (file-name-directory filename)))
  ;;       (unless (file-exists-p dir)
  ;;         (make-directory dir)))))
  (show-smartparens-global-mode -1)
  (global-highlight-parentheses-mode +1)

  (defvar so-long-threshold 500)
  (defvar so-long-max-lines 5)

  (with-eval-after-load 'doom-city-lights
    (custom-theme-set-faces
     'doom-city-lights
     '(ivy-current-match ((t (:underline t))))
     '(font-lock-variable-name-face ((t (:foreground "#5EC4FF"))))))

  ;; Not working...
  (with-eval-after-load 'solo-jazz-theme
    (solo-jazz-with-color-variables
      (custom-theme-set-faces
      'solo-jazz
      `(font-lock-type-face        	 ((t (:foreground ,solo-jazz-pink))))
      `(font-lock-function-name-face ((t (:foreground ,solo-jazz-blue))))
      ))
    )

  (with-eval-after-load 'spacemacs-light
    (custom-theme-set-faces
     'spacemacs-light
     '(font-lock-constant-face ((t (:inherit 'font-lock-type-face :weight normal)))))
    )

  (defun so-long-line-detected-p ()
    "Following any initial comments and blank lines, the next N lines of the
buffer will be tested for excessive length (where \"excessive\" means above
`so-long-threshold', and N is `so-long-max-lines').

Returns non-nil if any such excessive-length line is detected."
    (let ((count 0))
      (save-excursion
        (goto-char (point-min))
        (while (comment-forward)) ;; clears whitespace at minimum
        (catch 'excessive
          (while (< count so-long-max-lines)
            (if (> (- (line-end-position 1) (point))
                   so-long-threshold)
                (throw 'excessive t)
              (forward-line)
              (setq count (1+ count))))))))

  (defun asok/turn-on-fundamental-if-so-long ()
    (when (so-long-line-detected-p)
      (fundamental-mode)
      (spacemacs/disable-smooth-scrolling)))

  (with-eval-after-load 'beacon
    (add-hook 'beacon-dont-blink-predicates #'so-long-line-detected-p))

  (add-hook 'find-file-hook 'asok/turn-on-fundamental-if-so-long)

  (with-eval-after-load 'slack
    (when (file-exists-p "~/projects/gabi/emacs-slack.el")
      (load "~/projects/gabi/emacs-slack.el")))

  (with-eval-after-load 'eshell
    ;; (eshell/alias "be" "bundle exec \$*")
    ;; (eshell/alias "cdpr" "cd (projectile-rails-root)")
    )

  ;; (with-eval-after-load 'company
  ;;   (add-to-list 'company-backends #'company-tabnine))

  (defun asok/setup-ruby ()
    (setq ruby-align-to-stmt-keywords '(begin def))
    (robe-mode -1)
    (company-mode -1)
    (remove-hook 'ruby-mode-hook 'spacemacs//ruby-setup-backend)

    (spacemacs/set-leader-keys-for-major-mode 'ruby-mode
      "rl"  'multi-line)

    (set (make-local-variable 'company-backends-ruby-mode)
         (remq 'company-robe company-backends)))

  (add-hook 'ruby-mode-hook #'asok/setup-ruby 99)

  (defun asok/rubocop-doc-url (id)
    (format
     "https://docs.rubocop.org/en/latest/cops_%s/#%s"
     (downcase (car (split-string id "/")))
     (downcase (replace-regexp-in-string "/" "" id))
     ))

  (defun asok/rubocop-doc-eww (id)
    (interactive "sCop name: ")
    (eww (asok/rubocop-doc-url id)))

  (defun asok/rubocop-doc-browse (id)
    (interactive "sCop name: ")
    (browse-url (asok/rubocop-doc-url id)))

  (defun asok/rubocop-cops-at-point ()
    (-map 'flycheck-error-id
          (--select (eq (flycheck-error-checker it) 'ruby-rubocop)
                    (flycheck-overlay-errors-at (point)))))

  (defun asok/rubocop-choose-cop (cops)
    (if (> 1 (length cops))
        (completing-read "Cop name: " cops nil t)
      (car cops)))

  (defun asok/rubocop-doc-at-point ()
    (interactive)
    (if-let ((cops (asok/rubocop-cops-at-point)))
        (asok/rubocop-doc-browse (asok/rubocop-choose-cop cops))
      (warn "No error for ruby-rubocop checker found at the current pos")))

  (defun asok/rubocop-doc-browse (id)
    (interactive "sCop name: ")
    (browse-url (asok/rubocop-doc-url id)))

  (defun asok/rubocop-toggle-cop! (cop)
    (save-excursion
      (end-of-line)
      (insert (format " # rubocop:disable %s" cop))))

  (defun asok/rubocop-toggle-cop ()
    (interactive)
    (if-let ((cops (asok/rubocop-cops-at-point)))
        (asok/rubocop-toggle-cop! (asok/rubocop-choose-cop cops))
      (warn "No error for ruby-rubocop checker found at the current pos")))

  (defun asok/pry-cd-and-go ()
    (interactive)
    (let* ((method (ruby-add-log-current-method))
           (constant (car (cdr (s-match "\\([^#]+\\)#?" method)))))
      (with-temp-buffer
        (insert (concat "cd " constant))
        (ruby-send-buffer-and-go))))

  (spacemacs/set-leader-keys-for-major-mode 'ruby-mode
    "sc" 'asok/pry-cd-and-go)

  (add-hook 'js2-mode-hook
            (lambda ()
              (push '("function" . ?λ) prettify-symbols-alist)))

  (global-hl-line-mode -1)

  (defun asok/yarn-test-file ()
    (interactive)
    (compile (concat "yarn test " (buffer-file-name))))

  (evil-define-key 'normal js2-mode-map (kbd ", tt") 'asok/yarn-test-file)

  (nvm-use "v14.8.0")

  (savehist-mode -1) ; causes slowness

  (require 'counsel)
  ;; (setq ivy-prescient-sort-commands '(:not
  ;;                                     swiper
  ;;                                     swiper-isearch
  ;;                                     counsel-grep-or-swiper
  ;;                                     counsel-yank-pop
  ;;                                     ivy-switch-buffer))
  ;; (setq prescient-filter-method '(literal regexp initialism))
  ;; (ivy-prescient-mode +1)
  (highlight-parentheses-mode -1)

  ;; OSX specific
  (when-let ((git (executable-find "git")))
    (setq magit-git-executable git))
  (global-auto-revert-mode -1)

  (define-key process-menu-mode-map (kbd "C-k") 'joaot/delete-process-at-point)

  (defun joaot/delete-process-at-point ()
    (interactive)
    (let ((process (get-text-property (point) 'tabulated-list-id)))
      (cond ((and process
                  (processp process))
             (delete-process process)
             (revert-buffer))
            (t
             (error "no process at point!")))))
  )

;; Do not write anything past this comment. This is where Emacs will
;; auto-generate custom variable definitions.
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-faces-vector
   [default bold shadow italic underline bold bold-italic bold])
 '(custom-safe-themes
   '("a2e7b508533d46b701ad3b055e7c708323fb110b6676a8be458a758dd8f24e27" "5999e12c8070b9090a2a1bbcd02ec28906e150bb2cdce5ace4f965c76cf30476" "d320493111089afba1563bc3962d8ea1117dd2b3abb189aeebdc8c51b5517ddb" "7356632cebc6a11a87bc5fcffaa49bae528026a78637acd03cae57c091afd9b9" "04dd0236a367865e591927a3810f178e8d33c372ad5bfef48b5ce90d4b476481" "39dd7106e6387e0c45dfce8ed44351078f6acd29a345d8b22e7b8e54ac25bac4" "44eec3c3e6e673c0d41b523a67b64c43b6e38f8879a7969f306604dcf908832c" "5b24babd20e58465e070a8d7850ec573fe30aca66c8383a62a5e7a3588db830b" "72a81c54c97b9e5efcc3ea214382615649ebb539cb4f2fe3a46cd12af72c7607" "5e2dc1360a92bb73dafa11c46ba0f30fa5f49df887a8ede4e3533c3ab6270e08" "1b8d67b43ff1723960eb5e0cba512a2c7a2ad544ddb2533a90101fd1852b426e" "bb08c73af94ee74453c90422485b29e5643b73b05e8de029a6909af6a3fb3f58" "c1390663960169cd92f58aad44ba3253227d8f715c026438303c09b9fb66cdfb" "9b59e147dbbde5e638ea1cde5ec0a358d5f269d27bd2b893a0947c4a867e14c1" "2dd32048690787844d8cba601ed3dd8b2f419e9bd985898d0c3792671a05b96b" "6bb466c89b7e3eedc1f19f5a0cfa53be9baf6077f4d4a6f9b5d087f0231de9c8" "f782ed87369a7d568cee28d14922aa6d639f49dd676124d817dd82c8208985d0" "9e147cee63e1a2a6b16021e0645bc66c633c42b849e78b8e295df4b7fe55c56a" "06f0b439b62164c6f8f84fdda32b62fb50b6d00e8b01c2208e55543a6337433a" "590759adc4a5bf7a183df81654cce13b96089e026af67d92b5eec658fb3fe22f" "4aee8551b53a43a883cb0b7f3255d6859d766b6c5e14bcb01bed572fcbef4328" "ce557950466bf42096853c6dac6875b9ae9c782b8665f62478980cc5e3b6028d" "d79ece4768dfc4bab488475b85c2a8748dcdc3690e11a922f6be5e526a20b485" "d09467d742f713443c7699a546c0300db1a75fed347e09e3f178ab2f3aa2c617" "72c7c8b431179cbcfcea4193234be6a0e6916d04c44405fc87905ae16bed422a" "8abee8a14e028101f90a2d314f1b03bed1cde7fd3f1eb945ada6ffc15b1d7d65" "cedd3b4295ac0a41ef48376e16b4745c25fa8e7b4f706173083f16d5792bb379" "5c6d40ef6e7bbe9e83dc0e32db794c7e9a6a0d9eb7d6a874aaf9744c053842b4" "19ba41b6dc0b5dd34e1b8628ad7ae47deb19f968fe8c31853d64ea8c4df252b8" "f04122bbc305a202967fa1838e20ff741455307c2ae80a26035fbf5d637e325f" "3632cf223c62cb7da121be0ed641a2243f7ec0130178722554e613c9ab3131de" "66132890ee1f884b4f8e901f0c61c5ed078809626a547dbefbb201f900d03fd8" "d8f76414f8f2dcb045a37eb155bfaa2e1d17b6573ed43fb1d18b936febc7bbc2" "fa2b58bb98b62c3b8cf3b6f02f058ef7827a8e497125de0254f56e373abee088" "5f8f1e226274b73f6e706431399a597dbfd64db34f3fba56a6ccf57d148a0e01" "4cf3221feff536e2b3385209e9b9dc4c2e0818a69a1cdb4b522756bcdf4e00a4" "51e228ffd6c4fff9b5168b31d5927c27734e82ec61f414970fc6bcce23bc140d" "68d36308fc6e7395f7e6355f92c1dd9029c7a672cbecf8048e2933a053cf27e6" "bffa9739ce0752a37d9b1eee78fc00ba159748f50dc328af4be661484848e476" default))
 '(diary-entry-marker 'font-lock-variable-name-face)
 '(emms-mode-line-icon-image-cache
   '(image :type xpm :ascent center :data "/* XPM */
static char *note[] = {
/* width height num_colors chars_per_pixel */
\"    10   11        2            1\",
/* colors */
\". c #1ba1a1\",
\"# c None s None\",
/* pixels */
\"###...####\",
\"###.#...##\",
\"###.###...\",
\"###.#####.\",
\"###.#####.\",
\"#...#####.\",
\"....#####.\",
\"#..######.\",
\"#######...\",
\"######....\",
\"#######..#\" };"))
 '(evil-want-Y-yank-to-eol t)
 '(fci-rule-character-color "#d9d9d9")
 '(fci-rule-color "#d9d9d9" t)
 '(gnus-logo-colors '("#1ec1c4" "#bababa") t)
 '(gnus-mode-line-image-cache
   '(image :type xpm :ascent center :data "/* XPM */
static char *gnus-pointer[] = {
/* width height num_colors chars_per_pixel */
\"    18    13        2            1\",
/* colors */
\". c #1ba1a1\",
\"# c None s None\",
/* pixels */
\"##################\",
\"######..##..######\",
\"#####........#####\",
\"#.##.##..##...####\",
\"#...####.###...##.\",
\"#..###.######.....\",
\"#####.########...#\",
\"###########.######\",
\"####.###.#..######\",
\"######..###.######\",
\"###....####.######\",
\"###..######.######\"" ",
\"###########.######\" };") t)
 '(minimap-mode nil)
 '(package-selected-packages
   '(magit-gh-pulls lenlen-theme github-search github-clone github-browse-file gist gh marshal logito pcache winum white-sand-theme wgrep vlf toml-mode tide typescript-mode sunburn-theme smex oauth2 ruby-hash-syntax rjsx-mode rebecca-theme racer powerthesaurus jeison org-category-capture org-mime ob-restclient ob-http nubox nord-theme nodejs-repl nginx-mode madhat2r-theme ivy-posframe posframe ivy-hydra parent-mode helpful elisp-refs loop haml-mode ham-mode html-to-markdown fuzzy flymd flycheck-rust pos-tip flycheck-flow flycheck-credo flx exotica-theme evil-vimish-fold vimish-fold transient evil-lion goto-chg es-mode spark elfeed-web elfeed-org elfeed-goodies ace-jump-mode noflet elfeed doom-themes dockerfile-mode docker tablist docker-tramp json-snatcher diminish dash-at-point autothemer csv-mode counsel-dash dash-docs company-tabnine unicode-escape names company-restclient restclient know-your-http-well peg lv eval-sexp-fu sesman queue parseedn parseclj a cargo rust-mode better-jumper all-the-icons-dired all-the-icons memoize log4e gntp pkg-info epl powershell inflections seq birds-of-paradise-plus-theme-theme sourcerer-theme pug-mode ob-elixir org minitest ivy-purpose window-purpose imenu-list hide-comnt ht rake evil-unimpaired drupal-mode counsel-projectile counsel swiper undo-tree ivy rainbow-mode flycheck-elixir-dogma metalheart-theme ruby-end org-projectile git-link flycheck-mix darkokai-theme emojify dash-functional iedit highlight fzf sql-indent wttrin alchemist elixir-mode jinja2-mode ansible-doc ansible tramp-term ssh powerline slack circe request websocket ranger js2-mode projectile flycheck magit magit-popup git-commit with-editor smartparens web-completion-data tern hydra edn multiple-cursors paredit cider spinner clojure-mode packed avy auto-complete anzu markdown-mode yasnippet company gitignore-mode helm popup helm-core async json-reformat alert f s dash package-build bind-key bind-map evil org-download skewer-mode simple-httpd evil-visual-mark-mode dumb-jump shut-up ansi commander ctable concurrent deferred ert-runner epc sanityinc-tomorrow-night-theme-theme inf-ruby ac-inf-ruby evil-jumper eyebrowse column-enforce-mode zonokai-theme zenburn-theme zen-and-art-theme zeal-at-point yaml-mode xterm-color ws-butler window-numbering which-key wgrep-ag web-mode web-beautify volatile-highlights vi-tilde-fringe uuidgen use-package underwater-theme ujelly-theme twilight-theme twilight-bright-theme twilight-anti-bright-theme tronesque-theme toxi-theme toc-org tao-theme tangotango-theme tango-plus-theme tango-2-theme tagedit sunny-day-theme sublime-themes subatomic256-theme subatomic-theme stekene-theme spacemacs-theme spaceline spacegray-theme soothe-theme soft-stone-theme soft-morning-theme soft-charcoal-theme smyx-theme smooth-scrolling smeargle slim-mode shell-pop seti-theme scss-mode sass-mode rvm ruby-tools ruby-test-mode rubocop rspec-mode robe reverse-theme restart-emacs rbenv rainbow-delimiters railscasts-theme quelpa purple-haze-theme projectile-rails professional-theme popwin planet-theme phoenix-dark-pink-theme phoenix-dark-mono-theme persp-mode pcre2el pastels-on-dark-theme paradox page-break-lines orgit organic-green-theme org-repo-todo org-present org-pomodoro org-plus-contrib org-bullets open-junk-file omtose-phellack-theme oldlace-theme occidental-theme obsidian-theme noctilux-theme niflheim-theme neotree naquadah-theme mustang-theme multi-term move-text monokai-theme monochrome-theme molokai-theme moe-theme mmm-mode minimal-theme material-theme markdown-toc majapahit-theme magit-gitflow macrostep lush-theme lorem-ipsum livid-mode linum-relative link-hint light-soap-theme leuven-theme less-css-mode json-mode js2-refactor js-doc jbeans-theme jazz-theme jade-mode ir-black-theme inkpot-theme info+ indent-guide ido-vertical-mode hungry-delete htmlize hl-todo highlight-parentheses highlight-numbers highlight-indentation heroku-theme hemisu-theme help-fns+ helm-themes helm-swoop helm-projectile helm-mode-manager helm-make helm-gitignore helm-flx helm-descbinds helm-dash helm-css-scss helm-company helm-c-yasnippet helm-ag hc-zenburn-theme gruvbox-theme gruber-darker-theme grandshell-theme gotham-theme google-translate golden-ratio gnuplot gmail-message-mode gitconfig-mode gitattributes-mode git-timemachine git-messenger gh-md gandalf-theme fullframe flycheck-pos-tip flx-ido flatui-theme flatland-theme firebelly-theme fill-column-indicator feature-mode farmhouse-theme fancy-battery expand-region exec-path-from-shell evil-visualstar evil-tutor evil-surround evil-search-highlight-persist evil-numbers evil-nerd-commenter evil-mc evil-matchit evil-magit evil-lisp-state evil-indent-plus evil-iedit-state evil-exchange evil-escape evil-ediff evil-args evil-anzu espresso-theme eshell-z eshell-prompt-extras esh-help emmet-mode elisp-slime-nav edit-server dracula-theme django-theme define-word darktooth-theme darkmine-theme darkburn-theme dakrone-theme cyberpunk-theme company-web company-tern company-statistics company-quickhelp colorsarenice-theme color-theme-sanityinc-tomorrow color-theme-sanityinc-solarized coffee-mode clues-theme clojure-snippets clj-refactor clean-aindent-mode cider-eval-sexp-fu chruby cherry-blossom-theme busybee-theme bundler buffer-move bubbleberry-theme bracketed-paste birds-of-paradise-plus-theme beacon badwolf-theme auto-yasnippet auto-highlight-symbol auto-compile apropospriate-theme anti-zenburn-theme ample-zen-theme ample-theme alect-themes aggressive-indent ag afternoon-theme adaptive-wrap ace-window ace-link ace-jump-helm-line ac-ispell))
 '(paradox-github-token t)
 '(safe-local-variable-values
   '((eval setq cider-cljs-lein-repl "(do (use 'figwheel-sidecar.repl-api) (start-figwheel!) (cljs-repl))")
     (elixir-enable-compilation-checking . t)
     (elixir-enable-compilation-checking)
     (flycheck-disabled-checkers emacs-lisp-checkdoc)))
 '(vc-annotate-background nil)
 '(vc-annotate-color-map
   '((20 . "#d54e53")
     (40 . "#e78c45")
     (60 . "#e7c547")
     (80 . "#b9ca4a")
     (100 . "#70c0b1")
     (120 . "#7aa6da")
     (140 . "#c397d8")
     (160 . "#d54e53")
     (180 . "#e78c45")
     (200 . "#e7c547")
     (220 . "#b9ca4a")
     (240 . "#70c0b1")
     (260 . "#7aa6da")
     (280 . "#c397d8")
     (300 . "#d54e53")
     (320 . "#e78c45")
     (340 . "#e7c547")
     (360 . "#b9ca4a")))
 '(vc-annotate-very-old-color nil))
(defun dotspacemacs/emacs-custom-settings ()
  "Emacs custom settings.
This is an auto-generated function, do not modify its content directly, use
Emacs customize menu instead.
This function is called at the very end of Spacemacs initialization."
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-faces-vector
   [default bold shadow italic underline bold bold-italic bold])
 '(ansi-color-names-vector
   ["#0d0f11" "#DF8C8C" "#A8CE93" "#DADA93" "#83AFE5" "#c9b4cf" "#7FC1CA" "#e6eef3"])
 '(ansi-term-color-vector
   [unspecified "#1F1611" "#660000" "#144212" "#EFC232" "#5798AE" "#BE73FD" "#93C1BC" "#E6E1DC"] t)
 '(auto-insert-mode t)
 '(awesome-tray-mode-line-active-color "#2fafff")
 '(awesome-tray-mode-line-inactive-color "#2f2f2f")
 '(beacon-color "#d33682")
 '(company-quickhelp-color-background "#b0b0b0")
 '(company-quickhelp-color-foreground "#232333")
 '(compilation-message-face 'default)
 '(cua-global-mark-cursor-color "#2aa198")
 '(cua-normal-cursor-color "#839496")
 '(cua-overwrite-cursor-color "#b58900")
 '(cua-read-only-cursor-color "#859900")
 '(custom-safe-themes
   '("76b4632612953d1a8976d983c4fdf5c3af92d216e2f87ce2b0726a1f37606158" "e7ba99d0f4c93b9c5ca0a3f795c155fa29361927cadb99cfce301caf96055dfd" "8c1dd3d6fdfb2bee6b8f05d13d167f200befe1712d0abfdc47bb6d3b706c3434" "ef403aa0588ca64e05269a7a5df03a5259a00303ef6dfbd2519a9b81e4bce95c" "732ccca2e9170bcfd4ee5070159923f0c811e52b019106b1fc5eaa043dff4030" "99c86852decaeb0c6f51ce8bd46e4906a4f28ab4c5b201bdc3fdf85b24f88518" "a85e40c7d2df4a5e993742929dfd903899b66a667547f740872797198778d7b5" "36746ad57649893434c443567cb3831828df33232a7790d232df6f5908263692" "146061a7ceea4ccc75d975a3bb41432382f656c50b9989c7dc1a7bb6952f6eb4" "34ed3e2fa4a1cb2ce7400c7f1a6c8f12931d8021435bad841fdc1192bd1cc7da" "3380a2766cf0590d50d6366c5a91e976bdc3c413df963a0ab9952314b4577299" "c9f102cf31165896631747fd20a0ca0b9c64ecae019ce5c2786713a5b7d6315e" "1f38fb71e55e5ec5f14a39d03ca7d7a416123d3f0847745c7bade053ca58f043" "60e09d2e58343186a59d9ed52a9b13d822a174b33f20bdc1d4abb86e6b17f45b" "c614d2423075491e6b7f38a4b7ea1c68f31764b9b815e35c9741e9490119efc0" "a61109d38200252de49997a49d84045c726fa8d0f4dd637fce0b8affaa5c8620" "8cf1002c7f805360115700144c0031b9cfa4d03edc6a0f38718cef7b7cabe382" "2a998a3b66a0a6068bcb8b53cd3b519d230dd1527b07232e54c8b9d84061d48d" "0c3b1358ea01895e56d1c0193f72559449462e5952bded28c81a8e09b53f103f" "bc4c89a7b91cfbd3e28b2a8e9e6750079a985237b960384f158515d32c7f0490" "d2bd16a8bcf295dce0b70e1d2b5c17bb34cb28224a86ee770d56e6c22a565013" "cea3ec09c821b7eaf235882e6555c3ffa2fd23de92459751e18f26ad035d2142" "6b80b5b0762a814c62ce858e9d72745a05dd5fc66f821a1c5023b4f2a76bc910" "bf798e9e8ff00d4bf2512597f36e5a135ce48e477ce88a0764cfb5d8104e8163" "730a87ed3dc2bf318f3ea3626ce21fb054cd3a1471dcd59c81a4071df02cb601" "13a8eaddb003fd0d561096e11e1a91b029d3c9d64554f8e897b2513dbf14b277" "37144b437478e4c235824f0e94afa740ee2c7d16952e69ac3c5ed4352209eefb" "76bfa9318742342233d8b0b42e824130b3a50dcc732866ff8e47366aed69de11" "987b709680284a5858d5fe7e4e428463a20dfabe0a6f2a6146b3b8c7c529f08b" "37a4701758378c93159ad6c7aceb19fd6fb523e044efe47f2116bc7398ce20c9" "621595cbf6c622556432e881945dda779528e48bb57107b65d428e61a8bb7955" "bf387180109d222aee6bb089db48ed38403a1e330c9ec69fe1f52460a8936b66" "2f1518e906a8b60fac943d02ad415f1d8b3933a5a7f75e307e6e9a26ef5bf570" "8d805143f2c71cfad5207155234089729bb742a1cb67b7f60357fdd952044315" "fc6697788f00629cd01f4d2cc23f1994d08edb3535e4c0facef6b7247b41f5c7" "cd7ffd461946d2a644af8013d529870ea0761dccec33ac5c51a7aaeadec861c2" "89536596ee5bdc5ef9ea3d3d5b515ea616285fa9274c836263024f1993f6b3dd" "c48551a5fb7b9fc019bf3f61ebf14cf7c9cdca79bcb2a4219195371c02268f11" "58c6711a3b568437bab07a30385d34aacf64156cc5137ea20e799984f4227265" "e9776d12e4ccb722a2a732c6e80423331bcb93f02e089ba2a4b02e85de1cf00e" "7f1d414afda803f3244c6fb4c2c64bea44dac040ed3731ec9d75275b9e831fe5" "2809bcb77ad21312897b541134981282dc455ccd7c14d74cc333b6e549b824f3" "c83c095dd01cde64b631fb0fe5980587deec3834dc55144a6e78ff91ebc80b19" "e1ef2d5b8091f4953fe17b4ca3dd143d476c106e221d92ded38614266cea3c8b" "7a994c16aa550678846e82edc8c9d6a7d39cc6564baaaacc305a3fdc0bd8725f" "f2c35f8562f6a1e5b3f4c543d5ff8f24100fae1da29aeb1864bbc17758f52b70" "26b0ca3a8d8bc4bf366d01c9acca6da0c31cf28543c847fe99d0bace546aed6d" "4138944fbed88c047c9973f68908b36b4153646a045648a22083bd622d1e636d" "450f3382907de50be905ae8a242ecede05ea9b858a8ed3cc8d1fbdf2d57090af" "72085337718a3a9b4a7d8857079aa1144ea42d07a4a7696f86627e46ac52f50b" "afd761c9b0f52ac19764b99d7a4d871fc329f7392dfc6cd29710e8209c691477" "392395ee6e6844aec5a76ca4f5c820b97119ddc5290f4e0f58b38c9748181e8d" "8dce5b23232d0a490f16d62112d3abff6babeef86ae3853241a85856f9b0a6e7" "99ea831ca79a916f1bd789de366b639d09811501e8c092c85b2cb7d697777f93" "93ed23c504b202cf96ee591138b0012c295338f38046a1f3c14522d4a64d7308" "e074be1c799b509f52870ee596a5977b519f6d269455b84ed998666cf6fc802a" "dde8c620311ea241c0b490af8e6f570fdd3b941d7bc209e55cd87884eb733b0e" "f7216d3573e1bd2a2b47a2331f368b45e7b5182ddbe396d02b964b1ea5c5dc27" "f2927d7d87e8207fa9a0a003c0f222d45c948845de162c885bf6ad2a255babfd" "c4bdbbd52c8e07112d1bfd00fee22bf0f25e727e95623ecb20c4fa098b74c1bd" "8a97050c9dd0af1cd8c3290b061f4b6032ccf2044ddc4d3c2c39e516239b2463" "a2e7b508533d46b701ad3b055e7c708323fb110b6676a8be458a758dd8f24e27" "5999e12c8070b9090a2a1bbcd02ec28906e150bb2cdce5ace4f965c76cf30476" "d320493111089afba1563bc3962d8ea1117dd2b3abb189aeebdc8c51b5517ddb" "7356632cebc6a11a87bc5fcffaa49bae528026a78637acd03cae57c091afd9b9" "04dd0236a367865e591927a3810f178e8d33c372ad5bfef48b5ce90d4b476481" "39dd7106e6387e0c45dfce8ed44351078f6acd29a345d8b22e7b8e54ac25bac4" "44eec3c3e6e673c0d41b523a67b64c43b6e38f8879a7969f306604dcf908832c" "5b24babd20e58465e070a8d7850ec573fe30aca66c8383a62a5e7a3588db830b" "72a81c54c97b9e5efcc3ea214382615649ebb539cb4f2fe3a46cd12af72c7607" "5e2dc1360a92bb73dafa11c46ba0f30fa5f49df887a8ede4e3533c3ab6270e08" "1b8d67b43ff1723960eb5e0cba512a2c7a2ad544ddb2533a90101fd1852b426e" "bb08c73af94ee74453c90422485b29e5643b73b05e8de029a6909af6a3fb3f58" "c1390663960169cd92f58aad44ba3253227d8f715c026438303c09b9fb66cdfb" "9b59e147dbbde5e638ea1cde5ec0a358d5f269d27bd2b893a0947c4a867e14c1" "2dd32048690787844d8cba601ed3dd8b2f419e9bd985898d0c3792671a05b96b" "6bb466c89b7e3eedc1f19f5a0cfa53be9baf6077f4d4a6f9b5d087f0231de9c8" "f782ed87369a7d568cee28d14922aa6d639f49dd676124d817dd82c8208985d0" "9e147cee63e1a2a6b16021e0645bc66c633c42b849e78b8e295df4b7fe55c56a" "06f0b439b62164c6f8f84fdda32b62fb50b6d00e8b01c2208e55543a6337433a" "590759adc4a5bf7a183df81654cce13b96089e026af67d92b5eec658fb3fe22f" "4aee8551b53a43a883cb0b7f3255d6859d766b6c5e14bcb01bed572fcbef4328" "ce557950466bf42096853c6dac6875b9ae9c782b8665f62478980cc5e3b6028d" "d79ece4768dfc4bab488475b85c2a8748dcdc3690e11a922f6be5e526a20b485" "d09467d742f713443c7699a546c0300db1a75fed347e09e3f178ab2f3aa2c617" "72c7c8b431179cbcfcea4193234be6a0e6916d04c44405fc87905ae16bed422a" "8abee8a14e028101f90a2d314f1b03bed1cde7fd3f1eb945ada6ffc15b1d7d65" "cedd3b4295ac0a41ef48376e16b4745c25fa8e7b4f706173083f16d5792bb379" "5c6d40ef6e7bbe9e83dc0e32db794c7e9a6a0d9eb7d6a874aaf9744c053842b4" "19ba41b6dc0b5dd34e1b8628ad7ae47deb19f968fe8c31853d64ea8c4df252b8" "f04122bbc305a202967fa1838e20ff741455307c2ae80a26035fbf5d637e325f" "3632cf223c62cb7da121be0ed641a2243f7ec0130178722554e613c9ab3131de" "66132890ee1f884b4f8e901f0c61c5ed078809626a547dbefbb201f900d03fd8" "d8f76414f8f2dcb045a37eb155bfaa2e1d17b6573ed43fb1d18b936febc7bbc2" "fa2b58bb98b62c3b8cf3b6f02f058ef7827a8e497125de0254f56e373abee088" "5f8f1e226274b73f6e706431399a597dbfd64db34f3fba56a6ccf57d148a0e01" "4cf3221feff536e2b3385209e9b9dc4c2e0818a69a1cdb4b522756bcdf4e00a4" "51e228ffd6c4fff9b5168b31d5927c27734e82ec61f414970fc6bcce23bc140d" "68d36308fc6e7395f7e6355f92c1dd9029c7a672cbecf8048e2933a053cf27e6" "bffa9739ce0752a37d9b1eee78fc00ba159748f50dc328af4be661484848e476" default))
 '(diary-entry-marker 'font-lock-variable-name-face)
 '(emms-mode-line-icon-image-cache
   '(image :type xpm :ascent center :data "/* XPM */
static char *note[] = {
/* width height num_colors chars_per_pixel */
\"    10   11        2            1\",
/* colors */
\". c #1ba1a1\",
\"# c None s None\",
/* pixels */
\"###...####\",
\"###.#...##\",
\"###.###...\",
\"###.#####.\",
\"###.#####.\",
\"#...#####.\",
\"....#####.\",
\"#..######.\",
\"#######...\",
\"######....\",
\"#######..#\" };"))
 '(evil-emacs-state-cursor '("#E57373" hbar) t)
 '(evil-insert-state-cursor '("#E57373" bar) t)
 '(evil-normal-state-cursor '("#FFEE58" box) t)
 '(evil-visual-state-cursor '("#C5E1A5" box) t)
 '(evil-want-Y-yank-to-eol t)
 '(fci-rule-character-color "#d9d9d9")
 '(fci-rule-color "#d9d9d9" t)
 '(flycheck-color-mode-line-face-to-color 'mode-line-buffer-id)
 '(flycheck-javascript-flow-args nil)
 '(flymake-error-bitmap '(flymake-double-exclamation-mark modus-theme-fringe-red))
 '(flymake-note-bitmap '(exclamation-mark modus-theme-fringe-cyan))
 '(flymake-warning-bitmap '(exclamation-mark modus-theme-fringe-yellow))
 '(frame-background-mode 'light)
 '(fringe-mode 6 nil (fringe))
 '(global-auto-revert-mode t)
 '(global-hl-line-mode t)
 '(gnus-logo-colors '("#1ec1c4" "#bababa") t)
 '(gnus-mode-line-image-cache
   '(image :type xpm :ascent center :data "/* XPM */
static char *gnus-pointer[] = {
/* width height num_colors chars_per_pixel */
\"    18    13        2            1\",
/* colors */
\". c #1ba1a1\",
\"# c None s None\",
/* pixels */
\"##################\",
\"######..##..######\",
\"#####........#####\",
\"#.##.##..##...####\",
\"#...####.###...##.\",
\"#..###.######.....\",
\"#####.########...#\",
\"###########.######\",
\"####.###.#..######\",
\"######..###.######\",
\"###....####.######\",
\"###..######.######\"" ",
\"###########.######\" };") t)
 '(highlight-changes-colors '("#d33682" "#6c71c4"))
 '(highlight-indent-guides-auto-enabled nil)
 '(highlight-parentheses-background-colors '("#2492db" "#95a5a6" nil))
 '(highlight-parentheses-colors '("#2aa198" "#b58900" "#268bd2" "#6c71c4" "#859900"))
 '(highlight-symbol-colors
   (--map
    (solarized-color-blend it "#002b36" 0.25)
    '("#b58900" "#2aa198" "#dc322f" "#6c71c4" "#859900" "#cb4b16" "#268bd2")))
 '(highlight-symbol-foreground-color "#93a1a1")
 '(highlight-tail-colors
   '(("#073642" . 0)
     ("#546E00" . 20)
     ("#00736F" . 30)
     ("#00629D" . 50)
     ("#7B6000" . 60)
     ("#8B2C02" . 70)
     ("#93115C" . 85)
     ("#073642" . 100)))
 '(hl-bg-colors
   '("#7B6000" "#8B2C02" "#990A1B" "#93115C" "#3F4D91" "#00629D" "#00736F" "#546E00"))
 '(hl-fg-colors
   '("#002b36" "#002b36" "#002b36" "#002b36" "#002b36" "#002b36" "#002b36" "#002b36"))
 '(hl-sexp-background-color "#efebe9")
 '(hl-todo-keyword-faces
   '(("TODO" . "#dc752f")
     ("NEXT" . "#dc752f")
     ("THEM" . "#2d9574")
     ("PROG" . "#4f97d7")
     ("OKAY" . "#4f97d7")
     ("DONT" . "#f2241f")
     ("FAIL" . "#f2241f")
     ("DONE" . "#86dc2f")
     ("NOTE" . "#b1951d")
     ("KLUDGE" . "#b1951d")
     ("HACK" . "#b1951d")
     ("TEMP" . "#b1951d")
     ("FIXME" . "#dc752f")
     ("XXX" . "#dc752f")
     ("XXXX" . "#dc752f")))
 '(ibuffer-deletion-face 'modus-theme-mark-del)
 '(ibuffer-filter-group-name-face 'modus-theme-mark-symbol)
 '(ibuffer-marked-face 'modus-theme-mark-sel)
 '(ibuffer-title-face 'modus-theme-header)
 '(ivy-posframe-mode nil nil (ivy-posframe))
 '(jdee-db-active-breakpoint-face-colors (cons "#0d0f11" "#7FC1CA"))
 '(jdee-db-requested-breakpoint-face-colors (cons "#0d0f11" "#A8CE93"))
 '(jdee-db-spec-breakpoint-face-colors (cons "#0d0f11" "#899BA6"))
 '(line-number-mode t)
 '(linum-format 'dynamic)
 '(lsp-ui-doc-border "#9eacac")
 '(magit-diff-use-overlays nil)
 '(minimap-mode nil)
 '(nrepl-message-colors
   '("#dc322f" "#cb4b16" "#b58900" "#546E00" "#B4C342" "#00629D" "#2aa198" "#d33682" "#6c71c4"))
 '(objed-cursor-color "#c82829")
 '(package-selected-packages
   '(diredc evil-snipe base16-theme mini-frame consult-selectrum ivy-prescient yapfify stickyfunc-enhance pytest pyenv-mode py-isort pippel pipenv pyvenv pip-requirements lsp-python-ms live-py-mode importmagic helm-pydoc helm-gtags helm-cscope ggtags cython-mode counsel-gtags company-anaconda blacken anaconda-mode pythonic magit-gh-pulls lenlen-theme github-search github-clone github-browse-file gist gh marshal logito pcache winum white-sand-theme wgrep vlf toml-mode tide typescript-mode sunburn-theme smex oauth2 ruby-hash-syntax rjsx-mode rebecca-theme racer powerthesaurus jeison org-category-capture org-mime ob-restclient ob-http nubox nord-theme nodejs-repl nginx-mode madhat2r-theme ivy-posframe posframe ivy-hydra parent-mode helpful elisp-refs loop haml-mode ham-mode html-to-markdown fuzzy flymd flycheck-rust pos-tip flycheck-flow flycheck-credo flx exotica-theme evil-vimish-fold vimish-fold transient evil-lion goto-chg es-mode spark elfeed-web elfeed-goodies ace-jump-mode noflet elfeed doom-themes dockerfile-mode docker tablist docker-tramp json-snatcher diminish dash-at-point autothemer csv-mode counsel-dash dash-docs company-tabnine unicode-escape names company-restclient restclient know-your-http-well peg lv eval-sexp-fu sesman queue parseedn parseclj a cargo rust-mode better-jumper all-the-icons-dired all-the-icons memoize log4e gntp pkg-info epl powershell inflections seq birds-of-paradise-plus-theme-theme sourcerer-theme pug-mode ob-elixir org minitest ivy-purpose window-purpose imenu-list hide-comnt ht rake evil-unimpaired drupal-mode counsel-projectile counsel swiper undo-tree ivy rainbow-mode flycheck-elixir-dogma metalheart-theme ruby-end org-projectile git-link flycheck-mix darkokai-theme emojify dash-functional iedit highlight fzf sql-indent wttrin alchemist elixir-mode jinja2-mode ansible-doc ansible tramp-term ssh powerline slack circe request websocket ranger js2-mode projectile flycheck magit magit-popup git-commit with-editor smartparens web-completion-data tern hydra edn multiple-cursors paredit cider spinner clojure-mode packed avy auto-complete anzu markdown-mode yasnippet company gitignore-mode helm popup helm-core async json-reformat alert f s dash package-build bind-key bind-map evil org-download skewer-mode simple-httpd evil-visual-mark-mode dumb-jump shut-up ansi commander ctable concurrent deferred ert-runner epc sanityinc-tomorrow-night-theme-theme inf-ruby ac-inf-ruby evil-jumper eyebrowse column-enforce-mode zonokai-theme zenburn-theme zen-and-art-theme zeal-at-point yaml-mode xterm-color ws-butler window-numbering which-key wgrep-ag web-mode web-beautify volatile-highlights vi-tilde-fringe uuidgen use-package underwater-theme ujelly-theme twilight-theme twilight-bright-theme twilight-anti-bright-theme tronesque-theme toxi-theme toc-org tao-theme tangotango-theme tango-plus-theme tango-2-theme tagedit sunny-day-theme sublime-themes subatomic256-theme subatomic-theme stekene-theme spacemacs-theme spaceline spacegray-theme soothe-theme soft-stone-theme soft-morning-theme soft-charcoal-theme smyx-theme smooth-scrolling smeargle slim-mode shell-pop seti-theme scss-mode sass-mode rvm ruby-tools ruby-test-mode rubocop rspec-mode robe reverse-theme restart-emacs rbenv rainbow-delimiters railscasts-theme quelpa purple-haze-theme projectile-rails professional-theme popwin planet-theme phoenix-dark-pink-theme phoenix-dark-mono-theme persp-mode pcre2el pastels-on-dark-theme paradox page-break-lines organic-green-theme org-repo-todo org-present org-pomodoro org-bullets open-junk-file omtose-phellack-theme oldlace-theme occidental-theme obsidian-theme noctilux-theme niflheim-theme neotree naquadah-theme mustang-theme multi-term move-text monokai-theme monochrome-theme molokai-theme moe-theme mmm-mode minimal-theme material-theme markdown-toc majapahit-theme magit-gitflow macrostep lush-theme lorem-ipsum livid-mode linum-relative link-hint light-soap-theme leuven-theme less-css-mode json-mode js2-refactor js-doc jbeans-theme jazz-theme jade-mode ir-black-theme inkpot-theme info+ indent-guide ido-vertical-mode hungry-delete htmlize hl-todo highlight-parentheses highlight-numbers highlight-indentation heroku-theme hemisu-theme help-fns+ helm-themes helm-swoop helm-projectile helm-mode-manager helm-make helm-gitignore helm-flx helm-descbinds helm-dash helm-css-scss helm-company helm-c-yasnippet helm-ag hc-zenburn-theme gruvbox-theme gruber-darker-theme grandshell-theme gotham-theme google-translate golden-ratio gnuplot gmail-message-mode gitconfig-mode gitattributes-mode git-timemachine git-messenger gh-md gandalf-theme fullframe flycheck-pos-tip flx-ido flatui-theme flatland-theme firebelly-theme fill-column-indicator feature-mode farmhouse-theme fancy-battery expand-region exec-path-from-shell evil-visualstar evil-tutor evil-surround evil-search-highlight-persist evil-numbers evil-nerd-commenter evil-mc evil-matchit evil-magit evil-lisp-state evil-indent-plus evil-iedit-state evil-exchange evil-escape evil-ediff evil-args evil-anzu espresso-theme eshell-z eshell-prompt-extras esh-help emmet-mode elisp-slime-nav edit-server dracula-theme django-theme define-word darktooth-theme darkmine-theme darkburn-theme dakrone-theme cyberpunk-theme company-web company-tern company-statistics company-quickhelp colorsarenice-theme color-theme-sanityinc-tomorrow color-theme-sanityinc-solarized coffee-mode clues-theme clojure-snippets clj-refactor clean-aindent-mode cider-eval-sexp-fu chruby cherry-blossom-theme busybee-theme bundler buffer-move bubbleberry-theme bracketed-paste birds-of-paradise-plus-theme beacon badwolf-theme auto-yasnippet auto-highlight-symbol auto-compile apropospriate-theme anti-zenburn-theme ample-zen-theme ample-theme alect-themes aggressive-indent ag afternoon-theme adaptive-wrap ace-window ace-link ace-jump-helm-line ac-ispell))
 '(paradox-github-token t)
 '(pdf-view-midnight-colors '("#b2b2b2" . "#292b2e"))
 '(pos-tip-background-color "#073642")
 '(pos-tip-foreground-color "#93a1a1")
 '(rustic-ansi-faces
   ["#ffffff" "#c82829" "#718c00" "#eab700" "#3e999f" "#c9b4cf" "#8abeb7" "#4d4d4c"])
 '(safe-local-variable-values
   '((git-commit-major-mode . git-commit-elisp-text-mode)
     (eval setq cider-cljs-lein-repl "(do (use 'figwheel-sidecar.repl-api) (start-figwheel!) (cljs-repl))")
     (elixir-enable-compilation-checking . t)
     (elixir-enable-compilation-checking)
     (flycheck-disabled-checkers emacs-lisp-checkdoc)))
 '(smartrep-mode-line-active-bg (solarized-color-blend "#859900" "#073642" 0.2))
 '(sml/active-background-color "#34495e")
 '(sml/active-foreground-color "#ecf0f1")
 '(sml/inactive-background-color "#dfe4ea")
 '(sml/inactive-foreground-color "#34495e")
 '(tabbar-background-color "#357535753575")
 '(term-default-bg-color "#002b36")
 '(term-default-fg-color "#839496")
 '(vc-annotate-background nil)
 '(vc-annotate-background-mode nil)
 '(vc-annotate-color-map
   '((20 . "#d54e53")
     (40 . "#e78c45")
     (60 . "#e7c547")
     (80 . "#b9ca4a")
     (100 . "#70c0b1")
     (120 . "#7aa6da")
     (140 . "#c397d8")
     (160 . "#d54e53")
     (180 . "#e78c45")
     (200 . "#e7c547")
     (220 . "#b9ca4a")
     (240 . "#70c0b1")
     (260 . "#7aa6da")
     (280 . "#c397d8")
     (300 . "#d54e53")
     (320 . "#e78c45")
     (340 . "#e7c547")
     (360 . "#b9ca4a")))
 '(vc-annotate-very-old-color nil)
 '(weechat-color-list
   '(unspecified "#002b36" "#073642" "#990A1B" "#dc322f" "#546E00" "#859900" "#7B6000" "#b58900" "#00629D" "#268bd2" "#93115C" "#d33682" "#00736F" "#2aa198" "#839496" "#657b83"))
 '(which-key-posframe-mode t)
 '(window-divider-mode nil)
 '(xterm-color-names
   ["#073642" "#dc322f" "#859900" "#b58900" "#268bd2" "#d33682" "#2aa198" "#eee8d5"])
 '(xterm-color-names-bright
   ["#002b36" "#cb4b16" "#586e75" "#657b83" "#839496" "#6c71c4" "#93a1a1" "#fdf6e3"]))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:background nil)))))
)

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:background nil)))))
