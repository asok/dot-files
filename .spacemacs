;; -*- mode: emacs-lisp; lexical-binding: t -*-
;; This file is loaded by Spacemacs at startup.
;; It must be stored in your home directory.

(defun dotspacemacs/layers ()
  "Layer configuration:
This function should only modify configuration layer settings."
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
   ;; (default 'unused)
   dotspacemacs-enable-lazy-installation 'unused

   ;; If non-nil then Spacemacs will ask for confirmation before installing
   ;; a layer lazily. (default t)
   dotspacemacs-ask-for-lazy-installation t

   ;; List of additional paths where to look for configuration layers.
   ;; Paths must have a trailing slash (i.e. `~/.mycontribs/')
   dotspacemacs-configuration-layer-path '("~/.emacs.d/private/")
   ;; List of configuration layers to load.
   dotspacemacs-configuration-layers
   '(vimscript
     shell-scripts
     typescript
     html
     csv
     rust
     windows-scripts
     nginx
     sql
     ;; ----------------------------------------------------------------
     ;; Example of useful layers you may want to use right away.
     ;; Uncomment some layer names and press `SPC f e R' (Vim style) or
     ;; `M-m f e R' (Emacs style) to install them.
     ;; ----------------------------------------------------------------
     (auto-completion :variables
                      auto-completion-idle-delay 0.2
                      auto-completion-minimum-prefix-length 2
                      auto-completion-enable-snippets-in-popup nil
                      auto-completion-enable-sort-by-usage nil
                      auto-completion-return-key-behavior nil
                      auto-completion-tab-key-behavior 'complete
                      auto-completion-complete-with-key-sequence nil
                      auto-completion-use-company-posframe t)

     ;; better-defaults
     emacs-lisp
     react
     git
     ;; github
     clojure
     tern
     ;; (tern :variables tern-command '("/home/asok/.nvm/versions/node/v10.16.3/bin/tern"))
     (javascript :variables javascript-backend nil)
     (org :variables
          org-enable-jira-support t
          jiralib-url "https://gabi-com.atlassian.net")
     markdown
     dash
     themes-megapack
     (lsp :variables
          lsp-ui                         t
          lsp-ui-sideline-enable         t
          lsp-ui-doc-enable              nil
          lsp-enable-symbol-highlighting nil)
     (ruby :variables
           ruby-version-manager 'chruby
           ruby-test-runner     'rspec
           ruby-backend         'lsp)
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
     ;; brew install gpg2 gpg-suite
     ;; gpg-suite for caching the password
     ;; (mu4e :variables
     ;;       mu4e-use-maildirs-extension t
     ;;       mu4e-enable-async-operations t
     ;;       mu4e-attachment-dir (expand-file-name "~/Downloads")
     ;;       mu4e-compose-signature-auto-include nil
     ;;       mu4e-get-mail-command "mbsync -a"
     ;;       mu4e-maildir (expand-file-name "~/Maildir")
     ;;       mu4e-maildir-shortcuts '()
     ;;       mu4e-trash-folder "/trash"
     ;;       mu4e-drafts-folder "/drafts"
     ;;       mu4e-sent-folder "/sent"
     ;;       mu4e-update-interval 600
     ;;       mu4e-use-fancy-chars t
     ;;       mu4e-view-show-addresses t
     ;;       mu4e-view-show-images t
     ;;       mu4e-compose-dont-reply-to-self t
     ;;       mu4e-enable-notifications t
     ;;       mu4e-enable-mode-line nil
     ;;       mu4e-maildir-shortcuts '(("/public-gmail/INBOX" . ?i)
     ;;                                ("/sent" . ?s)
     ;;                                ("/drafts" . ?d))

     ;;       smtpmail-stream-type 'starttls
     ;;       smtpmail-default-smtp-server "smtp.gmail.com"
     ;;       smtpmail-smtp-server "smtp.gmail.com"
     ;;       smtpmail-smtp-server "smtp.gmail.com"
     ;;       smtpmail-smtp-service 587
     ;;       smtpmail-debug-info t
     ;;       )
     (osx :variables
          osx-command-as       nil
          osx-option-as        nil
          osx-control-as       nil
          osx-function-as      nil
          osx-right-command-as nil
          osx-right-option-as  nil
          osx-right-control-as nil
          osx-swap-option-and-command nil)

     hackernews
     selectric

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
                                      all-the-icons-ivy
                                      evil-lion
                                      doom-themes
                                      ruby-hash-syntax
                                      counsel-dash
                                      nubox
                                      nodejs-repl
                                      helpful
                                      flycheck-flow
                                      vlf
                                      sunburn-theme
                                      es-mode
                                      powerthesaurus
                                      nord-theme
                                      better-jumper
                                      ivy-posframe
                                      evil-vimish-fold
                                      leuven-theme
                                      nvm
                                      jest-test-mode
                                      ivy-prescient
                                      base16-theme
                                      exec-path-from-shell
                                      devdocs-browser
                                      )
   ;; A list of packages and/or extensions that will not be install and loaded.
   dotspacemacs-excluded-packages '()

   ;; Defines the behaviour of Spacemacs when installing packages.
   ;; Possible values are `used-only', `used-but-keep-unused' and `all'.
   ;; `used-only' installs only explicitly used packages and deletes any unused
   ;; packages as well as their unused dependencies. `used-but-keep-unused'
   ;; installs only the used packages but won't delete unused ones. `all'
   ;; installs *all* packages supported by Spacemacs and never uninstalls them.
   ;; (default is `used-only')
   dotspacemacs-install-packages 'used-only))

(defun dotspacemacs/init ()
  "Initialization:
This function is called at the very beginning of Spacemacs startup,
before layer configuration.
It should only modify the values of Spacemacs settings."
  ;; This setq-default sexp is an exhaustive list of all the supported
  ;; spacemacs settings.
  (setq-default
   ;; If non-nil then enable support for the portable dumper. You'll need
   ;; to compile Emacs 27 from source following the instructions in file
   ;; EXPERIMENTAL.org at to root of the git repository.
   ;; (default nil)
   dotspacemacs-enable-emacs-pdumper nil

   ;; Name of executable file pointing to emacs 27+. This executable must be
   ;; in your PATH.
   ;; (default "emacs")
   dotspacemacs-emacs-pdumper-executable-file "emacs"

   ;; Name of the Spacemacs dump file. This is the file will be created by the
   ;; portable dumper in the cache directory under dumps sub-directory.
   ;; To load it when starting Emacs add the parameter `--dump-file'
   ;; when invoking Emacs 27.1 executable on the command line, for instance:
   ;;   ./emacs --dump-file=$HOME/.emacs.d/.cache/dumps/spacemacs-27.1.pdmp
   ;; (default (format "spacemacs-%s.pdmp" emacs-version))
   dotspacemacs-emacs-dumper-dump-file (format "spacemacs-%s.pdmp" emacs-version)

   ;; If non-nil ELPA repositories are contacted via HTTPS whenever it's
   ;; possible. Set it to nil if you have no way to use HTTPS in your
   ;; environment, otherwise it is strongly recommended to let it set to t.
   ;; This variable has no effect if Emacs is launched with the parameter
   ;; `--insecure' which forces the value of this variable to nil.
   ;; (default t)
   dotspacemacs-elpa-https t

   ;; Maximum allowed time in seconds to contact an ELPA repository.
   ;; (default 5)
   dotspacemacs-elpa-timeout 5

   ;; Set `gc-cons-threshold' and `gc-cons-percentage' when startup finishes.
   ;; This is an advanced option and should not be changed unless you suspect
   ;; performance issues due to garbage collection operations.
   ;; (default '(100000000 0.1))
   dotspacemacs-gc-cons '(100000000 0.1)

   ;; Set `read-process-output-max' when startup finishes.
   ;; This defines how much data is read from a foreign process.
   ;; Setting this >= 1 MB should increase performance for lsp servers
   ;; in emacs 27.
   ;; (default (* 1024 1024))
   dotspacemacs-read-process-output-max (* 1024 1024)

   ;; If non-nil then Spacelpa repository is the primary source to install
   ;; a locked version of packages. If nil then Spacemacs will install the
   ;; latest version of packages from MELPA. Spacelpa is currently in
   ;; experimental state please use only for testing purposes.
   ;; (default nil)
   dotspacemacs-use-spacelpa nil

   ;; If non-nil then verify the signature for downloaded Spacelpa archives.
   ;; (default t)
   dotspacemacs-verify-spacelpa-archives t

   ;; If non-nil then spacemacs will check for updates at startup
   ;; when the current branch is not `develop'. Note that checking for
   ;; new versions works via git commands, thus it calls GitHub services
   ;; whenever you start Emacs. (default nil)
   dotspacemacs-check-for-update nil

   ;; If non-nil, a form that evaluates to a package directory. For example, to
   ;; use different package directories for different Emacs versions, set this
   ;; to `emacs-version'. (default 'emacs-version)
   dotspacemacs-elpa-subdirectory 'emacs-version

   ;; One of `vim', `emacs' or `hybrid'.
   ;; `hybrid' is like `vim' except that `insert state' is replaced by the
   ;; `hybrid state' with `emacs' key bindings. The value can also be a list
   ;; with `:variables' keyword (similar to layers). Check the editing styles
   ;; section of the documentation for details on available variables.
   ;; (default 'vim)
   dotspacemacs-editing-style 'vim

   ;; If non-nil show the version string in the Spacemacs buffer. It will
   ;; appear as (spacemacs version)@(emacs version)
   ;; (default t)
   dotspacemacs-startup-buffer-show-version t

   ;; Specify the startup banner. Default value is `official', it displays
   ;; the official spacemacs logo. An integer value is the index of text
   ;; banner, `random' chooses a random text banner in `core/banners'
   ;; directory. A string value must be a path to an image format supported
   ;; by your Emacs build.
   ;; If the value is nil then no banner is displayed. (default 'official)
   dotspacemacs-startup-banner 'official

   ;; List of items to show in startup buffer or an association list of
   ;; the form `(list-type . list-size)`. If nil then it is disabled.
   ;; Possible values for list-type are:
   ;; `recents' `recents-by-project' `bookmarks' `projects' `agenda' `todos'.
   ;; List sizes may be nil, in which case
   ;; `spacemacs-buffer-startup-lists-length' takes effect.
   ;; The exceptional case is `recents-by-project', where list-type must be a
   ;; pair of numbers, e.g. `(recents-by-project . (7 .  5))', where the first
   ;; number is the project limit and the second the limit on the recent files
   ;; within a project.
   dotspacemacs-startup-lists '((recents . 5)
                                (projects . 7))

   ;; True if the home buffer should respond to resize events. (default t)
   dotspacemacs-startup-buffer-responsive t

   ;; Show numbers before the startup list lines. (default t)
   dotspacemacs-show-startup-list-numbers t

   ;; The minimum delay in seconds between number key presses. (default 0.4)
   dotspacemacs-startup-buffer-multi-digit-delay 0.4

   ;; Default major mode for a new empty buffer. Possible values are mode
   ;; names such as `text-mode'; and `nil' to use Fundamental mode.
   ;; (default `text-mode')
   dotspacemacs-new-empty-buffer-major-mode 'text-mode

   ;; Default major mode of the scratch buffer (default `text-mode')
   dotspacemacs-scratch-mode 'text-mode

   ;; If non-nil, *scratch* buffer will be persistent. Things you write down in
   ;; *scratch* buffer will be saved and restored automatically.
   dotspacemacs-scratch-buffer-persistent nil

   ;; If non-nil, `kill-buffer' on *scratch* buffer
   ;; will bury it instead of killing.
   dotspacemacs-scratch-buffer-unkillable nil

   ;; Initial message in the scratch buffer, such as "Welcome to Spacemacs!"
   ;; (default nil)
   dotspacemacs-initial-scratch-message nil

   ;; List of themes, the first of the list is loaded when spacemacs starts.
   ;; Press `SPC T n' to cycle to the next theme in the list (works great
   ;; with 2 themes variants, one dark and one light)
   dotspacemacs-themes '(
                         doom-oceanic-next
                         spacemacs-light
                         base16-google-light
                         base16-material-palenight
                         base16-tomorrow
                         doom-tomorrow-day
                         twilight-bright
                         flatui
                         doom-spacegrey
                         solarized-light
                         solarized-dark
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
   dotspacemacs-mode-line-theme '(spacemacs :separator wave :separator-scale 1.5)

   ;; If non-nil the cursor color matches the state color in GUI Emacs.
   ;; (default t)
   dotspacemacs-colorize-cursor-according-to-state t

   ;; Default font or prioritized list of fonts. The `:size' can be specified as
   ;; a non-negative integer (pixel size), or a floating-point (point size).
   ;; Point size is recommended, because it's device independent. (default 10.0)
   dotspacemacs-default-font '("Monaco"
                               :size 12.0
                               :weight normal
                               :width normal)

   ;; The leader key (default "SPC")
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
   ;; (default "C-M-m" for terminal mode, "<M-return>" for GUI mode).
   ;; Thus M-RET should work as leader key in both GUI and terminal modes.
   ;; C-M-m also should work in terminal mode, but not in GUI mode.
   dotspacemacs-major-mode-emacs-leader-key (if window-system "<M-return>" "C-M-m")

   ;; These variables control whether separate commands are bound in the GUI to
   ;; the key pairs `C-i', `TAB' and `C-m', `RET'.
   ;; Setting it to a non-nil value, allows for separate commands under `C-i'
   ;; and TAB or `C-m' and `RET'.
   ;; In the terminal, these pairs are generally indistinguishable, so this only
   ;; works in the GUI. (default nil)
   dotspacemacs-distinguish-gui-tab nil

   ;; Name of the default layout (default "Default")
   dotspacemacs-ex-substitute-global t
   dotspacemacs-default-layout-name "Default"

   ;; If non-nil the default layout name is displayed in the mode-line.
   ;; (default nil)
   dotspacemacs-display-default-layout nil

   ;; If non-nil then the last auto saved layouts are resumed automatically upon
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
   dotspacemacs-which-key-delay 0.4
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

   ;; If non-nil a progress bar is displayed when spacemacs is loading. This
   ;; may increase the boot time on some systems and emacs builds, set it to
   ;; nil to boost the loading time. (default t)
   dotspacemacs-loading-progress-bar t

   ;; If non-nil the frame is fullscreen when Emacs starts up. (default nil)
   ;; (Emacs 24.4+ only)
   dotspacemacs-fullscreen-at-startup nil

   ;; If non-nil `spacemacs/toggle-fullscreen' will not use native fullscreen.
   ;; Use to disable fullscreen animations in OSX. (default nil)
   dotspacemacs-fullscreen-use-non-native nil

   ;; If non-nil the frame is maximized when Emacs starts up.
   ;; Takes effect only if `dotspacemacs-fullscreen-at-startup' is nil.
   ;; (default nil) (Emacs 24.4+ only)
   dotspacemacs-maximized-at-startup nil

   ;; If non-nil the frame is undecorated when Emacs starts up. Combine this
   ;; variable with `dotspacemacs-maximized-at-startup' in OSX to obtain
   ;; borderless fullscreen. (default nil)
   dotspacemacs-undecorated-at-startup nil

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

   ;; If non-nil smooth scrolling (native-scrolling) is enabled. Smooth
   ;; scrolling overrides the default behavior of Emacs which recenters point
   ;; when it reaches the top or bottom of the screen. (default t)
   dotspacemacs-smooth-scrolling nil

   ;; Show the scroll bar while scrolling. The auto hide time can be configured
   ;; by setting this variable to a number. (default t)
   dotspacemacs-scroll-bar-while-scrolling nil

   ;; Control line numbers activation.
   ;; If set to `t', `relative' or `visual' then line numbers are enabled in all
   ;; `prog-mode' and `text-mode' derivatives. If set to `relative', line
   ;; numbers are relative. If set to `visual', line numbers are also relative,
   ;; but only visual lines are counted. For example, folded lines will not be
   ;; counted and wrapped lines are counted as multiple lines.
   ;; This variable can also be set to a property list for finer control:
   ;; '(:relative nil
   ;;   :visual nil
   ;;   :disabled-for-modes dired-mode
   ;;                       doc-view-mode
   ;;                       markdown-mode
   ;;                       org-mode
   ;;                       pdf-view-mode
   ;;                       text-mode
   ;;   :size-limit-kb 1000)
   ;; When used in a plist, `visual' takes precedence over `relative'.
   ;; (default nil)
   dotspacemacs-line-numbers nil

   ;; Code folding method. Possible values are `evil', `origami' and `vimish'.
   ;; (default 'evil)
   dotspacemacs-folding-method 'evil

   ;; If non-nil and `dotspacemacs-activate-smartparens-mode' is also non-nil,
   ;; `smartparens-strict-mode' will be enabled in programming modes.
   ;; (default nil)
   dotspacemacs-smartparens-strict-mode nil

   ;; If non-nil smartparens-mode will be enabled in programming modes.
   ;; (default t)
   dotspacemacs-activate-smartparens-mode t

   ;; If non-nil pressing the closing parenthesis `' key in insert mode passes
   ;; over any automatically added closing parenthesis, bracket, quote, etc...
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
   ;; If nil then Spacemacs uses default `frame-title-format' to avoid
   ;; performance issues, instead of calculating the frame title by
   ;; `spacemacs/title-prepare' all the time.
   ;; (default "%I@%S")
   dotspacemacs-frame-title-format "%I@%S"

   ;; Format specification for setting the icon title format
   ;; (default nil - same as frame-title-format)
   dotspacemacs-icon-title-format nil

   ;; Show trailing whitespace (default t)
   dotspacemacs-show-trailing-whitespace t

   ;; Delete whitespace while saving buffer. Possible values are `all'
   ;; to aggressively delete empty line and long sequences of whitespace,
   ;; `trailing' to delete only the whitespace at end of lines, `changed' to
   ;; delete only whitespace for changed lines or `nil' to disable cleanup.
   ;; (default nil)
   dotspacemacs-whitespace-cleanup 'trailing

   ;; If non-nil activate `clean-aindent-mode' which tries to correct
   ;; virtual indentation of simple modes. This can interfere with mode specific
   ;; indent handling like has been reported for `go-mode'.
   ;; If it does deactivate it here.
   ;; (default t)
   dotspacemacs-use-clean-aindent-mode t

   ;; Accept SPC as y for prompts if non-nil. (default nil)
   dotspacemacs-use-SPC-as-y nil

   ;; If non-nil shift your number row to match the entered keyboard layout
   ;; (only in insert state). Currently supported keyboard layouts are:
   ;; `qwerty-us', `qwertz-de' and `querty-ca-fr'.
   ;; New layouts can be added in `spacemacs-editing' layer.
   ;; (default nil)
   dotspacemacs-swap-number-row nil

   ;; Either nil or a number of seconds. If non-nil zone out after the specified
   ;; number of seconds. (default nil)
   dotspacemacs-zone-out-when-idle nil

   ;; Run `spacemacs/prettify-org-buffer' when
   ;; visiting README.org files of Spacemacs.
   ;; (default nil)
   dotspacemacs-pretty-docs nil

   ;; If nil the home buffer shows the full path of agenda items
   ;; and todos. If non-nil only the file name is shown.
   dotspacemacs-home-shorten-agenda-source nil

   ;; If non-nil then byte-compile some of Spacemacs files.
   dotspacemacs-byte-compile nil))

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
   lsp-headerline-breadcrumb-enable nil
   vterm-max-scrollback 10000
   read-process-output-max (* 1024 1024)
   compilation-ask-about-save nil)

  (let ((file "~/.emacs.d/private/custom-variables.el"))
    (setq custom-file file)

    (when (file-exists-p file)
      (load file)))

  (spacemacs/add-to-hooks '(lambda ()
                             (setq-local compilation-always-kill t))
                          '(rspec-mode-hook rspec-verifiable-mode-hook rspec-compilation-mode-hook))

  (add-hook 'rspec-compilation-mode-hook '(lambda ()
                                            (setq-local compilation-scroll-output nil)))

  (setq configuration-layer--elpa-archives
        '(("melpa"    . "melpa.org/packages/")
          ("org"      . "orgmode.org/elpa/")
          ("gnu"      . "elpa.gnu.org/packages/")))

  (setq slack-enable-emoji t)

  )

(defun dotspacemacs/user-load ()
  "Library to load while dumping.
This function is called only while dumping Spacemacs configuration. You can
`require' or `load' the libraries of your choice that will be included in the
dump.")

(defun dotspacemacs/user-config ()
  "Configuration for user code:
This function is called at the very end of Spacemacs startup, after layer
configuration.
Put your configuration code here, except for variables that should be set
before packages are loaded."

  (setq-default
   beacon-blink-when-focused t
   beacon-blink-when-point-moves-vertically nil
   ivy-use-virtual-buffers t
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
    (evil-define-key 'normal global-map
      (kbd "C-M-SPC") #'spacemacs/buffer-transient-state/body
      (kbd "M-SPC")   #'ivy-switch-buffer
      (kbd "s-<left>")   #'switch-to-prev-buffer
      (kbd "s-<right>")   #'switch-to-next-buffer
      (kbd "M-p") (lambda () (interactive) (set-mark-command 4)))

    (evil-define-key 'emacs global-map
      (kbd "C-M-SPC") #'spacemacs/buffer-transient-state/body
      (kbd "M-SPC")   #'ivy-switch-buffer
      (kbd "s-<left>")   #'switch-to-prev-buffer
      (kbd "s-<right>")   #'switch-to-next-buffer
      (kbd "M-p") (lambda () (interactive) (set-mark-command 4)))

    (evil-define-key 'visual global-map (kbd "v") #'er/expand-region)

    (define-key global-map (kbd "s-<left>")   #'switch-to-prev-buffer)
    (define-key global-map (kbd "s-<right>")  #'switch-to-next-buffer)

    (add-hook 'git-commit-mode-hook 'evil-insert-state)
    (evil-set-initial-state 'magit-log-edit-mode 'insert))

  (spacemacs/set-leader-keys
    "\""  (lambda () (interactive) (eshell t)))

  (with-eval-after-load 'shell-pop
    (defun asok/shell-pop-emacs-state-maybe ()
      (if (eq shell-default-shell 'ansi-term)
          (evil-emacs-state)))

    (add-hook 'shell-pop-in-after-hook #'asok/shell-pop-emacs-state-maybe))

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
  (fullframe wttrin wttrin-exit)

  (add-hook 'eval-expression-minibuffer-setup-hook #'eldoc-mode)
  (add-hook 'eval-expression-minibuffer-setup-hook #'paredit-mode)

  (defadvice load-theme
      (before theme-dont-propagate activate)
    (mapcar #'disable-theme custom-enabled-themes))

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

  (with-eval-after-load 'devdocs-browser
    (setq devdocs-browser-cache-directory "~/.emacs.d/private/")

    (add-to-list 'devdocs-browser-major-mode-docs-alist
                 '(ruby-mode "ruby"))
    )

  (with-eval-after-load 'vterm
    (defun asok/vterm-c-r ()
      (interactive)
      (vterm-send-key "r" nil nil t))

    (evil-define-key 'insert vterm-mode-map (kbd "C-r") #'asok/vterm-c-r))

  (with-eval-after-load 'projectile-rails
    (setq projectile-rails-expand-snippet-with-magic-comment t)

    (spacemacs/set-leader-keys
      "ardd" #'devdocs-browser-open)

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
    )

  (remove-hook 'ruby-mode-hook 'robe-mode)

  (remove-hook 'projectile-rails-mode-hook 'projectile-rails-expand-snippet-maybe)

  (add-hook 'js2-mode-hook
            (lambda ()
              (push '("function" . ?λ) prettify-symbols-alist)))

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

  (with-eval-after-load 'gotham-theme
    (custom-theme-set-faces 'gotham '(js2-object-property ((t (:inherit 'font-lock-type-face))))))

  (spacemacs//add-to-load-path "~/projects/all-the-icons-ivy")
  (require 'all-the-icons-ivy)
  (all-the-icons-ivy-setup)

  (show-smartparens-global-mode -1)
  (global-highlight-parentheses-mode +1)

  ;; Not working...
  (require 'ivy-posframe)
  (ivy-posframe-mode +1)

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

  (with-eval-after-load 'slack
    (when (file-exists-p "~/projects/gabi/emacs-slack.el")
      (load "~/projects/gabi/emacs-slack.el")))

  (with-eval-after-load 'lsp
    ;; Without it lsp flycheck is not working
    (require 'lsp-headerline)
    (require 'lsp-diagnostics))

  (defun asok/setup-ruby ()
    (setq ruby-align-to-stmt-keywords '(begin def))

    (spacemacs/set-leader-keys-for-major-mode 'ruby-mode
      "rl"  'multi-line))

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
    (let* ((linenum (line-number-at-pos))
           (filename (buffer-file-name))
           (re (concat filename ":" (number-to-string linenum) ":[0-9]+: [A-Z]+: \\(.+\\):")))
      (with-temp-buffer
        (insert (shell-command-to-string (concat "rubocop -D -f emacs " filename)))

        (let ((matches))
          (save-match-data
            (save-excursion
              (with-current-buffer (current-buffer)
                (save-restriction
                  (widen)
                  (goto-char 1)
                  (while (search-forward-regexp re nil t 1)
                    (push (match-string 1) matches)))))
            matches)))))

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

  (defun asok/yarn-test-file ()
    (interactive)
    (compile (concat "yarn test " (buffer-file-name))))

  (evil-define-key 'normal js2-mode-map (kbd ", tt") 'asok/yarn-test-file)

  (savehist-mode -1) ; causes slowness

  (require 'counsel)
  (highlight-parentheses-mode -1)

  ;; OSX specific
  (when-let ((git (executable-find "git")))
    (setq magit-git-executable git))
  (global-auto-revert-mode -1)

  (define-key process-menu-mode-map (kbd "C-k") 'joaot/delete-process-at-point)

  (defun asok/toggle-ns-alternate-modifier ()
    (interactive)
    (if ns-alternate-modifier
        (setq ns-alternate-modifier nil)
      (setq ns-alternate-modifier 'meta)))

  (defun joaot/delete-process-at-point ()
    (interactive)
    (let ((process (get-text-property (point) 'tabulated-list-id)))
      (cond ((and process
                  (processp process))
             (delete-process process)
             (revert-buffer))
            (t
             (error "no process at point!")))))

  (defun asok/visit-gh-pr-url ()
    "Visit the current branch's PR on Github."
    (interactive)
    (browse-url (format "https://github.com/%s/pull/new/%s"
                        (replace-regexp-in-string
                         ;; "\\.+github\\.com:\\(.+\\)\\.git\\'" "\\1" ; git protocol
                         ".+github\\.com/\\(.+\\)\\.git" "\\1"         ; https protocol
                         (magit-get "remote" (magit-get-push-remote) "url"))
                        (magit-get-current-branch))))

  (with-eval-after-load 'rspec-mode
    (rspec-install-snippets)

    (defun asok/rspec-toggle-spec-and-target (fn)
      (let ((file (rspec-spec-or-target)))
        (if (file-exists-p file)
            (call-interactively fn)
          (when (yes-or-no-p "File does not exist, switch nevertheless?")
            (call-interactively fn)))))

    (advice-add 'rspec-toggle-spec-and-target :around #'asok/rspec-toggle-spec-and-target)

    (make-variable-buffer-local 'asok/rspec-force-spring)

    (defun asok/rspec-force-spring (fn &rest args)
      (if asok/rspec-force-spring
          "spring rspec"
        (apply fn args)))

    (advice-add 'rspec-runner :around #'asok/rspec-force-spring)
    )

  (with-eval-after-load 'smartparens
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
      (kbd "D") #'sp-kill-hybrid-sexp))

  (with-eval-after-load 'ivy
    (evil-add-hjkl-bindings ivy-occur-grep-mode-map 'normal
      (kbd "RET") 'ivy-occur-press-and-switch
      (kbd "C-d") 'ivy-occur-delete-candidate)

    (ivy-set-actions
     'counsel-find-file
     `((,(propertize "delete" 'face 'font-lock-warning-face)
        (lambda (x) (delete-file (expand-file-name x ivy--directory)))))))
    )

;; Do not write anything past this comment. This is where Emacs will
;; auto-generate custom variable definitions.

(defun dotspacemacs/emacs-custom-settings ()
  "Emacs custom settings.
This is an auto-generated function, do not modify its content directly, use
Emacs customize menu instead.
This function is called at the very end of Spacemacs initialization.")
