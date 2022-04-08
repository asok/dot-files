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
   '(shell-scripts
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
                      auto-completion-enable-snippets-in-popup t
                      auto-completion-enable-sort-by-usage t
                      auto-completion-return-key-behavior nil
                      auto-completion-tab-key-behavior 'complete
                      auto-completion-complete-with-key-sequence nil)

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
     (lsp
      :variables
          lsp-ui-sideline-enable         t
          lsp-ui-doc-enable              t
          lsp-enable-symbol-highlighting nil
          ;; lsp-ui-remap-xref-keybindings t
          )
     ;; lsp
     (ruby :variables
           ruby-version-manager 'chruby
           ruby-test-runner     'rspec
           ruby-backend         'lsp
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
                                      exec-path-from-shell
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
   dotspacemacs-default-font '("Source Code Pro"
                               :size 11.0
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

   ;; If non-nil pressing the closing parenthesis `)' key in insert mode passes
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

  (spacemacs/add-to-hooks '(lambda ()
                             (setq-local compilation-always-kill t))
                          '(rspec-mode-hook rspec-verifiable-mode-hook rspec-compilation-mode-hook))

  (eval-after-load 'rspec-mode
    '(rspec-install-snippets)

    (defun asok/rspec-toggle-spec-and-target (fn)
      (let ((file (rspec-spec-or-target)))
        (if (file-exists-p file)
            (call-interactively fn)
          (when (yes-or-no-p "File does not exist, switch nevertheless?")
            (call-interactively fn)))))

    (advice-add 'rspec-toggle-spec-and-target :around #'asok/rspec-toggle-spec-and-target)
    )

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
dump.")


(defun dotspacemacs/user-config ()
  "Configuration for user code:
This function is called at the very end of Spacemacs startup, after layer
configuration.
Put your configuration code here, except for variables that should be set
before packages are loaded."

       (setq
        ivy-initial-inputs-alist nil
        bidi-inhibit-bpa t)


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
         (evil-set-initial-state 'magit-log-edit-mode 'insert)
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
         (evil-add-hjkl-bindings ivy-occur-grep-mode-map 'normal
           (kbd "RET") 'ivy-occur-press-and-switch
           (kbd "C-d") 'ivy-occur-delete-candidate)

         (ivy-set-actions
          'counsel-find-file
          `((,(propertize "delete" 'face 'font-lock-warning-face)
             (lambda (x) (delete-file (expand-file-name x ivy--directory))))))

         (ivy-posframe-mode +1))

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

       (spacemacs//add-to-load-path "~/projects/all-the-icons-ivy")
       (require 'all-the-icons-ivy)
       (all-the-icons-ivy-setup)

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
         ;; (robe-mode -1)
         ;; (company-mode -1)
         ;; (remove-hook 'ruby-mode-hook 'spacemacs//ruby-setup-backend)

         (spacemacs/set-leader-keys-for-major-mode 'ruby-mode
           "rl"  'multi-line)

         ;; (set (make-local-variable 'company-backends-ruby-mode)
         ;;      (remq 'company-robe company-backends))
         )

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

       ;; (nvm-use "v14.8.0")

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

       ;; (setq mu4e-contexts
       ;;       `( ,(make-mu4e-context
       ;;            :name "Public"
       ;;            :enter-func (lambda () (mu4e-message "Switch to the Public context"))
       ;;            ;; leave-func not defined
       ;;            :match-func (lambda (msg)
       ;;                          (when msg
       ;;                            (mu4e-message-contact-field-matches
       ;;                             msg
       ;;                             :to "adam.sokolnicki@gmail.com")))
       ;;            :vars '(  ( user-mail-address      . "adam.sokolnicki@gmail.com"  )
       ;;                      ( user-full-name     . "Adam Sokolnicki")
       ;;                      ( mu4e-compose-signature .
       ;;                        (concat
       ;;                         "Cheers,\n"
       ;;                         "Adam Sokolnicki\n"))))
       ;;          ,(make-mu4e-context
       ;;            :name "Gabi"
       ;;            :enter-func (lambda () (mu4e-message "Switch to the Gabi context"))
       ;;            ;; leave-fun not defined
       ;;            :match-func (lambda (msg)
       ;;                          (when msg
       ;;                            (mu4e-message-contact-field-matches
       ;;                             msg
       ;;                             :to "adam@gabi.com")))
       ;;            :vars '(  ( user-mail-address      . "adam@gabi.com" )
       ;;                      ( user-full-name     . "Adam Sokolnicki" )
       ;;                      ( mu4e-compose-signature .
       ;;                        (concat
       ;;                         "Cheers,\n"
       ;;                         "Adam Sokolnicki\n"))))))

       ;; (add-to-list 'auth-sources (expand-file-name "~/.emacs.d/mu4e/.mbsyncpass-public-gmail.gpg"))

       ;; (with-eval-after-load 'mu4e-alert
         ;; Enable Desktop notifications
         ;; (mu4e-alert-set-default-style 'notifications)) ; For Linux.
         ;; (mu4e-alert-set-default-style 'libnotify))  ; Alternative for Linux
         ;; (mu4e-alert-set-default-style 'notifier))   ; For macOS (through the terminal notifier app).
       ;; (mu4e-alert-set-default-style 'growl))      ; Alternative for macOS.

       ;; (when (memq window-system '(mac ns x))
         ;; (exec-path-from-shell-initialize))

       (ivy-posframe-mode +1)

       )

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-faces-vector
   [default bold shadow italic underline bold bold-italic bold])
 '(ansi-color-names-vector
   ["#073642" "#dc322f" "#859900" "#b58900" "#268bd2" "#d33682" "#2aa198" "#657b83"])
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
   '("e6f3a4a582ffb5de0471c9b640a5f0212ccf258a987ba421ae2659f1eaa39b09" "6c531d6c3dbc344045af7829a3a20a09929e6c41d7a7278963f7d3215139f6a7" "23c806e34594a583ea5bbf5adf9a964afe4f28b4467d28777bcba0d35aa0872e" "d6844d1e698d76ef048a53cefe713dbbe3af43a1362de81cdd3aefa3711eae0d" "835868dcd17131ba8b9619d14c67c127aa18b90a82438c8613586331129dda63" "76ed126dd3c3b653601ec8447f28d8e71a59be07d010cd96c55794c3008df4d7" "1704976a1797342a1b4ea7a75bdbb3be1569f4619134341bd5a4c1cfb16abad4" "1d5e33500bc9548f800f9e248b57d1b2a9ecde79cb40c0b1398dec51ee820daf" "67f0f440afa2e68d9d00219b5a56308761af45832fb60769d2b2fd36e3fead45" "e9776d12e4ccb722a2a732c6e80423331bcb93f02e089ba2a4b02e85de1cf00e" "dc8ad8b5833ae06e373cc3d64be28e67e6c3d084ea5f0e9e77225b3badbec661" "0d75aa06198c4245ac2a8877bfc56503d5d8199cc85da2c65a6791b84afb9024" "3a2e0c5597f6d74d99daa2b5bbbc2a653d02d6b88fcd73d3c84ebf25cde37b3f" "d268b67e0935b9ebc427cad88ded41e875abfcc27abd409726a92e55459e0d01" "ae88c445c558b7632fc2d72b7d4b8dfb9427ac06aa82faab8d760fff8b8f243c" "246a9596178bb806c5f41e5b571546bb6e0f4bd41a9da0df5dfbca7ec6e2250c" "f91395598d4cb3e2ae6a2db8527ceb83fed79dbaf007f435de3e91e5bda485fb" "5784d048e5a985627520beb8a101561b502a191b52fa401139f4dd20acb07607" "613aedadd3b9e2554f39afe760708fc3285bf594f6447822dd29f947f0775d6c" "e8df30cd7fb42e56a4efc585540a2e63b0c6eeb9f4dc053373e05d774332fc13" "a9a67b318b7417adbedaab02f05fa679973e9718d9d26075c6235b1f0db703c8" "c2aeb1bd4aa80f1e4f95746bda040aafb78b1808de07d340007ba898efa484f5" "97db542a8a1731ef44b60bc97406c1eb7ed4528b0d7296997cbb53969df852d6" "e7ba99d0f4c93b9c5ca0a3f795c155fa29361927cadb99cfce301caf96055dfd" "4c56af497ddf0e30f65a7232a8ee21b3d62a8c332c6b268c81e9ea99b11da0d3" "fee7287586b17efbfda432f05539b58e86e059e78006ce9237b8732fde991b4c" "16dd114a84d0aeccc5ad6fd64752a11ea2e841e3853234f19dc02a7b91f5d661" "aea30125ef2e48831f46695418677b9d676c3babf43959c8e978c0ad672a7329" "00aa8bf5a2d4463b35091c6e07072fe0658adc2d60439fa476f88d5e0097fc4b" "c560237b7505f67a271def31c706151afd7aa6eba9f69af77ec05bde5408dbcd" "36746ad57649893434c443567cb3831828df33232a7790d232df6f5908263692" "a0be7a38e2de974d1598cf247f607d5c1841dbcef1ccd97cded8bea95a7c7639" "a2e7b508533d46b701ad3b055e7c708323fb110b6676a8be458a758dd8f24e27" "5999e12c8070b9090a2a1bbcd02ec28906e150bb2cdce5ace4f965c76cf30476" "d320493111089afba1563bc3962d8ea1117dd2b3abb189aeebdc8c51b5517ddb" "7356632cebc6a11a87bc5fcffaa49bae528026a78637acd03cae57c091afd9b9" "04dd0236a367865e591927a3810f178e8d33c372ad5bfef48b5ce90d4b476481" "39dd7106e6387e0c45dfce8ed44351078f6acd29a345d8b22e7b8e54ac25bac4" "44eec3c3e6e673c0d41b523a67b64c43b6e38f8879a7969f306604dcf908832c" "5b24babd20e58465e070a8d7850ec573fe30aca66c8383a62a5e7a3588db830b" "72a81c54c97b9e5efcc3ea214382615649ebb539cb4f2fe3a46cd12af72c7607" "5e2dc1360a92bb73dafa11c46ba0f30fa5f49df887a8ede4e3533c3ab6270e08" "1b8d67b43ff1723960eb5e0cba512a2c7a2ad544ddb2533a90101fd1852b426e" "bb08c73af94ee74453c90422485b29e5643b73b05e8de029a6909af6a3fb3f58" "c1390663960169cd92f58aad44ba3253227d8f715c026438303c09b9fb66cdfb" "9b59e147dbbde5e638ea1cde5ec0a358d5f269d27bd2b893a0947c4a867e14c1" "2dd32048690787844d8cba601ed3dd8b2f419e9bd985898d0c3792671a05b96b" "6bb466c89b7e3eedc1f19f5a0cfa53be9baf6077f4d4a6f9b5d087f0231de9c8" "f782ed87369a7d568cee28d14922aa6d639f49dd676124d817dd82c8208985d0" "9e147cee63e1a2a6b16021e0645bc66c633c42b849e78b8e295df4b7fe55c56a" "06f0b439b62164c6f8f84fdda32b62fb50b6d00e8b01c2208e55543a6337433a" "590759adc4a5bf7a183df81654cce13b96089e026af67d92b5eec658fb3fe22f" "4aee8551b53a43a883cb0b7f3255d6859d766b6c5e14bcb01bed572fcbef4328" "ce557950466bf42096853c6dac6875b9ae9c782b8665f62478980cc5e3b6028d" "d79ece4768dfc4bab488475b85c2a8748dcdc3690e11a922f6be5e526a20b485" "d09467d742f713443c7699a546c0300db1a75fed347e09e3f178ab2f3aa2c617" "72c7c8b431179cbcfcea4193234be6a0e6916d04c44405fc87905ae16bed422a" "8abee8a14e028101f90a2d314f1b03bed1cde7fd3f1eb945ada6ffc15b1d7d65" "cedd3b4295ac0a41ef48376e16b4745c25fa8e7b4f706173083f16d5792bb379" "5c6d40ef6e7bbe9e83dc0e32db794c7e9a6a0d9eb7d6a874aaf9744c053842b4" "19ba41b6dc0b5dd34e1b8628ad7ae47deb19f968fe8c31853d64ea8c4df252b8" "f04122bbc305a202967fa1838e20ff741455307c2ae80a26035fbf5d637e325f" "3632cf223c62cb7da121be0ed641a2243f7ec0130178722554e613c9ab3131de" "66132890ee1f884b4f8e901f0c61c5ed078809626a547dbefbb201f900d03fd8" "d8f76414f8f2dcb045a37eb155bfaa2e1d17b6573ed43fb1d18b936febc7bbc2" "fa2b58bb98b62c3b8cf3b6f02f058ef7827a8e497125de0254f56e373abee088" "5f8f1e226274b73f6e706431399a597dbfd64db34f3fba56a6ccf57d148a0e01" "4cf3221feff536e2b3385209e9b9dc4c2e0818a69a1cdb4b522756bcdf4e00a4" "51e228ffd6c4fff9b5168b31d5927c27734e82ec61f414970fc6bcce23bc140d" "68d36308fc6e7395f7e6355f92c1dd9029c7a672cbecf8048e2933a053cf27e6" "bffa9739ce0752a37d9b1eee78fc00ba159748f50dc328af4be661484848e476" default))
 '(diary-entry-marker 'font-lock-variable-name-face)
 '(emms-mode-line-icon-image-cache
   '(image :type xpm :ascent center :data "/* XPM */\12static char *note[] = {\12/* width height num_colors chars_per_pixel */\12\"    10   11        2            1\",\12/* colors */\12\". c #1ba1a1\",\12\"# c None s None\",\12/* pixels */\12\"###...####\",\12\"###.#...##\",\12\"###.###...\",\12\"###.#####.\",\12\"###.#####.\",\12\"#...#####.\",\12\"....#####.\",\12\"#..######.\",\12\"#######...\",\12\"######....\",\12\"#######..#\" };"))
 '(evil-emacs-state-cursor '("#E57373" hbar) t)
 '(evil-insert-state-cursor '("#E57373" bar) t)
 '(evil-normal-state-cursor '("#FFEE58" box) t)
 '(evil-visual-state-cursor '("#C5E1A5" box) t)
 '(evil-want-Y-yank-to-eol t)
 '(fci-rule-character-color "#d9d9d9")
 '(fci-rule-color "#073642" t)
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
   '(image :type xpm :ascent center :data "/* XPM */\12static char *gnus-pointer[] = {\12/* width height num_colors chars_per_pixel */\12\"    18    13        2            1\",\12/* colors */\12\". c #1ba1a1\",\12\"# c None s None\",\12/* pixels */\12\"##################\",\12\"######..##..######\",\12\"#####........#####\",\12\"#.##.##..##...####\",\12\"#...####.###...##.\",\12\"#..###.######.....\",\12\"#####.########...#\",\12\"###########.######\",\12\"####.###.#..######\",\12\"######..###.######\",\12\"###....####.######\",\12\"###..######.######\"" ",\12\"###########.######\" };") t)
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
 '(lsp-ui-doc-border "#93a1a1")
 '(magit-diff-use-overlays nil)
 '(minimap-mode nil)
 '(nrepl-message-colors
   '("#dc322f" "#cb4b16" "#b58900" "#546E00" "#B4C342" "#00629D" "#2aa198" "#d33682" "#6c71c4"))
 '(objed-cursor-color "#c82829")
 '(package-selected-packages
   '(mentor kubernetes-evil dap-mode bui magit-gh-pulls lenlen-theme github-search github-clone github-browse-file gist gh marshal logito pcache winum white-sand-theme wgrep vlf toml-mode tide typescript-mode sunburn-theme smex oauth2 ruby-hash-syntax rjsx-mode rebecca-theme racer powerthesaurus jeison org-category-capture org-mime ob-restclient ob-http nubox nord-theme nodejs-repl nginx-mode madhat2r-theme ivy-posframe posframe ivy-hydra parent-mode helpful elisp-refs loop haml-mode ham-mode html-to-markdown fuzzy flymd flycheck-rust pos-tip flycheck-flow flycheck-credo flx exotica-theme evil-vimish-fold vimish-fold transient evil-lion goto-chg es-mode spark elfeed-web elfeed-org elfeed-goodies ace-jump-mode noflet elfeed doom-themes dockerfile-mode docker tablist docker-tramp json-snatcher diminish dash-at-point autothemer csv-mode counsel-dash dash-docs company-tabnine unicode-escape names company-restclient restclient know-your-http-well peg lv eval-sexp-fu sesman queue parseedn parseclj a cargo rust-mode better-jumper all-the-icons-dired all-the-icons memoize log4e gntp pkg-info epl powershell inflections seq birds-of-paradise-plus-theme-theme sourcerer-theme pug-mode ob-elixir org minitest ivy-purpose window-purpose imenu-list hide-comnt ht rake evil-unimpaired drupal-mode counsel-projectile counsel swiper undo-tree ivy rainbow-mode flycheck-elixir-dogma metalheart-theme ruby-end org-projectile git-link flycheck-mix darkokai-theme emojify dash-functional iedit highlight fzf sql-indent wttrin alchemist elixir-mode jinja2-mode ansible-doc ansible tramp-term ssh powerline slack circe request websocket ranger js2-mode projectile flycheck magit magit-popup git-commit with-editor smartparens web-completion-data tern hydra edn multiple-cursors paredit cider spinner clojure-mode packed avy auto-complete anzu markdown-mode yasnippet company gitignore-mode helm popup helm-core async json-reformat alert f s dash package-build bind-key bind-map evil org-download skewer-mode simple-httpd evil-visual-mark-mode dumb-jump shut-up ansi commander ctable concurrent deferred ert-runner epc sanityinc-tomorrow-night-theme-theme inf-ruby ac-inf-ruby evil-jumper eyebrowse column-enforce-mode zonokai-theme zenburn-theme zen-and-art-theme zeal-at-point yaml-mode xterm-color ws-butler window-numbering which-key wgrep-ag web-mode web-beautify volatile-highlights vi-tilde-fringe uuidgen use-package underwater-theme ujelly-theme twilight-theme twilight-bright-theme twilight-anti-bright-theme tronesque-theme toxi-theme toc-org tao-theme tangotango-theme tango-plus-theme tango-2-theme tagedit sunny-day-theme sublime-themes subatomic256-theme subatomic-theme stekene-theme spacemacs-theme spaceline spacegray-theme soothe-theme soft-stone-theme soft-morning-theme soft-charcoal-theme smyx-theme smooth-scrolling smeargle slim-mode shell-pop seti-theme scss-mode sass-mode rvm ruby-tools ruby-test-mode rubocop rspec-mode robe reverse-theme restart-emacs rbenv rainbow-delimiters railscasts-theme quelpa purple-haze-theme projectile-rails professional-theme popwin planet-theme phoenix-dark-pink-theme phoenix-dark-mono-theme persp-mode pcre2el pastels-on-dark-theme paradox page-break-lines orgit organic-green-theme org-repo-todo org-present org-pomodoro org-plus-contrib org-bullets open-junk-file omtose-phellack-theme oldlace-theme occidental-theme obsidian-theme noctilux-theme niflheim-theme neotree naquadah-theme mustang-theme multi-term move-text monokai-theme monochrome-theme molokai-theme moe-theme mmm-mode minimal-theme material-theme markdown-toc majapahit-theme magit-gitflow macrostep lush-theme lorem-ipsum livid-mode linum-relative link-hint light-soap-theme leuven-theme less-css-mode json-mode js2-refactor js-doc jbeans-theme jazz-theme jade-mode ir-black-theme inkpot-theme info+ indent-guide ido-vertical-mode hungry-delete htmlize hl-todo highlight-parentheses highlight-numbers highlight-indentation heroku-theme hemisu-theme help-fns+ helm-themes helm-swoop helm-projectile helm-mode-manager helm-make helm-gitignore helm-flx helm-descbinds helm-dash helm-css-scss helm-company helm-c-yasnippet helm-ag hc-zenburn-theme gruvbox-theme gruber-darker-theme grandshell-theme gotham-theme google-translate golden-ratio gnuplot gmail-message-mode gitconfig-mode gitattributes-mode git-timemachine git-messenger gh-md gandalf-theme fullframe flycheck-pos-tip flx-ido flatui-theme flatland-theme firebelly-theme fill-column-indicator feature-mode farmhouse-theme fancy-battery expand-region exec-path-from-shell evil-visualstar evil-tutor evil-surround evil-search-highlight-persist evil-numbers evil-nerd-commenter evil-mc evil-matchit evil-magit evil-lisp-state evil-indent-plus evil-iedit-state evil-exchange evil-escape evil-ediff evil-args evil-anzu espresso-theme eshell-z eshell-prompt-extras esh-help emmet-mode elisp-slime-nav edit-server dracula-theme django-theme define-word darktooth-theme darkmine-theme darkburn-theme dakrone-theme cyberpunk-theme company-web company-tern company-statistics company-quickhelp colorsarenice-theme color-theme-sanityinc-tomorrow color-theme-sanityinc-solarized coffee-mode clues-theme clojure-snippets clj-refactor clean-aindent-mode cider-eval-sexp-fu chruby cherry-blossom-theme busybee-theme bundler buffer-move bubbleberry-theme bracketed-paste birds-of-paradise-plus-theme beacon badwolf-theme auto-yasnippet auto-highlight-symbol auto-compile apropospriate-theme anti-zenburn-theme ample-zen-theme ample-theme alect-themes aggressive-indent ag afternoon-theme adaptive-wrap ace-window ace-link ace-jump-helm-line ac-ispell))
 '(paradox-github-token t)
 '(pdf-view-midnight-colors '("#b2b2b2" . "#292b2e"))
 '(pos-tip-background-color "#073642")
 '(pos-tip-foreground-color "#93a1a1")
 '(rustic-ansi-faces
   ["#ffffff" "#c82829" "#718c00" "#eab700" "#3e999f" "#c9b4cf" "#8abeb7" "#4d4d4c"])
 '(safe-local-variable-values
   '((eval setq cider-cljs-lein-repl "(do (use 'figwheel-sidecar.repl-api) (start-figwheel!) (cljs-repl))")
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
 '(warning-suppress-log-types '((comp)))
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
 '(default ((t (:background nil))))
 '(highlight-parentheses-highlight ((nil (:weight ultra-bold))) t))
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

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:background nil)))))
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
   ["#073642" "#dc322f" "#859900" "#b58900" "#268bd2" "#d33682" "#2aa198" "#657b83"])
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
   '("246a9596178bb806c5f41e5b571546bb6e0f4bd41a9da0df5dfbca7ec6e2250c" "e6f3a4a582ffb5de0471c9b640a5f0212ccf258a987ba421ae2659f1eaa39b09" "a2e7b508533d46b701ad3b055e7c708323fb110b6676a8be458a758dd8f24e27" "5999e12c8070b9090a2a1bbcd02ec28906e150bb2cdce5ace4f965c76cf30476" "d320493111089afba1563bc3962d8ea1117dd2b3abb189aeebdc8c51b5517ddb" "7356632cebc6a11a87bc5fcffaa49bae528026a78637acd03cae57c091afd9b9" "04dd0236a367865e591927a3810f178e8d33c372ad5bfef48b5ce90d4b476481" "39dd7106e6387e0c45dfce8ed44351078f6acd29a345d8b22e7b8e54ac25bac4" "44eec3c3e6e673c0d41b523a67b64c43b6e38f8879a7969f306604dcf908832c" "5b24babd20e58465e070a8d7850ec573fe30aca66c8383a62a5e7a3588db830b" "72a81c54c97b9e5efcc3ea214382615649ebb539cb4f2fe3a46cd12af72c7607" "5e2dc1360a92bb73dafa11c46ba0f30fa5f49df887a8ede4e3533c3ab6270e08" "1b8d67b43ff1723960eb5e0cba512a2c7a2ad544ddb2533a90101fd1852b426e" "bb08c73af94ee74453c90422485b29e5643b73b05e8de029a6909af6a3fb3f58" "c1390663960169cd92f58aad44ba3253227d8f715c026438303c09b9fb66cdfb" "9b59e147dbbde5e638ea1cde5ec0a358d5f269d27bd2b893a0947c4a867e14c1" "2dd32048690787844d8cba601ed3dd8b2f419e9bd985898d0c3792671a05b96b" "6bb466c89b7e3eedc1f19f5a0cfa53be9baf6077f4d4a6f9b5d087f0231de9c8" "f782ed87369a7d568cee28d14922aa6d639f49dd676124d817dd82c8208985d0" "9e147cee63e1a2a6b16021e0645bc66c633c42b849e78b8e295df4b7fe55c56a" "06f0b439b62164c6f8f84fdda32b62fb50b6d00e8b01c2208e55543a6337433a" "590759adc4a5bf7a183df81654cce13b96089e026af67d92b5eec658fb3fe22f" "4aee8551b53a43a883cb0b7f3255d6859d766b6c5e14bcb01bed572fcbef4328" "ce557950466bf42096853c6dac6875b9ae9c782b8665f62478980cc5e3b6028d" "d79ece4768dfc4bab488475b85c2a8748dcdc3690e11a922f6be5e526a20b485" "d09467d742f713443c7699a546c0300db1a75fed347e09e3f178ab2f3aa2c617" "72c7c8b431179cbcfcea4193234be6a0e6916d04c44405fc87905ae16bed422a" "8abee8a14e028101f90a2d314f1b03bed1cde7fd3f1eb945ada6ffc15b1d7d65" "cedd3b4295ac0a41ef48376e16b4745c25fa8e7b4f706173083f16d5792bb379" "5c6d40ef6e7bbe9e83dc0e32db794c7e9a6a0d9eb7d6a874aaf9744c053842b4" "19ba41b6dc0b5dd34e1b8628ad7ae47deb19f968fe8c31853d64ea8c4df252b8" "f04122bbc305a202967fa1838e20ff741455307c2ae80a26035fbf5d637e325f" "3632cf223c62cb7da121be0ed641a2243f7ec0130178722554e613c9ab3131de" "66132890ee1f884b4f8e901f0c61c5ed078809626a547dbefbb201f900d03fd8" "d8f76414f8f2dcb045a37eb155bfaa2e1d17b6573ed43fb1d18b936febc7bbc2" "fa2b58bb98b62c3b8cf3b6f02f058ef7827a8e497125de0254f56e373abee088" "5f8f1e226274b73f6e706431399a597dbfd64db34f3fba56a6ccf57d148a0e01" "4cf3221feff536e2b3385209e9b9dc4c2e0818a69a1cdb4b522756bcdf4e00a4" "51e228ffd6c4fff9b5168b31d5927c27734e82ec61f414970fc6bcce23bc140d" "68d36308fc6e7395f7e6355f92c1dd9029c7a672cbecf8048e2933a053cf27e6" "bffa9739ce0752a37d9b1eee78fc00ba159748f50dc328af4be661484848e476" default))
 '(diary-entry-marker 'font-lock-variable-name-face)
 '(emms-mode-line-icon-image-cache
   '(image :type xpm :ascent center :data "/* XPM */\12static char *note[] = {\12/* width height num_colors chars_per_pixel */\12\"    10   11        2            1\",\12/* colors */\12\". c #1ba1a1\",\12\"# c None s None\",\12/* pixels */\12\"###...####\",\12\"###.#...##\",\12\"###.###...\",\12\"###.#####.\",\12\"###.#####.\",\12\"#...#####.\",\12\"....#####.\",\12\"#..######.\",\12\"#######...\",\12\"######....\",\12\"#######..#\" };"))
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
   '(image :type xpm :ascent center :data "/* XPM */\12static char *gnus-pointer[] = {\12/* width height num_colors chars_per_pixel */\12\"    18    13        2            1\",\12/* colors */\12\". c #1ba1a1\",\12\"# c None s None\",\12/* pixels */\12\"##################\",\12\"######..##..######\",\12\"#####........#####\",\12\"#.##.##..##...####\",\12\"#...####.###...##.\",\12\"#..###.######.....\",\12\"#####.########...#\",\12\"###########.######\",\12\"####.###.#..######\",\12\"######..###.######\",\12\"###....####.######\",\12\"###..######.######\"" ",\12\"###########.######\" };") t)
 '(highlight-changes-colors '("#d33682" "#6c71c4"))
 '(highlight-indent-guides-auto-enabled nil)
 '(highlight-parentheses-background-colors '("#2492db" "#95a5a6" nil))
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
 '(lsp-ui-doc-border "#93a1a1")
 '(magit-diff-use-overlays nil)
 '(minimap-mode nil)
 '(nrepl-message-colors
   '("#dc322f" "#cb4b16" "#b58900" "#546E00" "#B4C342" "#00629D" "#2aa198" "#d33682" "#6c71c4"))
 '(objed-cursor-color "#c82829")
 '(package-selected-packages
   '(magit-gh-pulls lenlen-theme github-search github-clone github-browse-file gist gh marshal logito pcache winum white-sand-theme wgrep vlf toml-mode tide typescript-mode sunburn-theme smex oauth2 ruby-hash-syntax rjsx-mode rebecca-theme racer powerthesaurus jeison org-category-capture org-mime ob-restclient ob-http nubox nord-theme nodejs-repl nginx-mode madhat2r-theme ivy-posframe posframe ivy-hydra parent-mode helpful elisp-refs loop haml-mode ham-mode html-to-markdown fuzzy flymd flycheck-rust pos-tip flycheck-flow flycheck-credo flx exotica-theme evil-vimish-fold vimish-fold transient evil-lion goto-chg es-mode spark elfeed-web elfeed-org elfeed-goodies ace-jump-mode noflet elfeed doom-themes dockerfile-mode docker tablist docker-tramp json-snatcher diminish dash-at-point autothemer csv-mode counsel-dash dash-docs company-tabnine unicode-escape names company-restclient restclient know-your-http-well peg lv eval-sexp-fu sesman queue parseedn parseclj a cargo rust-mode better-jumper all-the-icons-dired all-the-icons memoize log4e gntp pkg-info epl powershell inflections seq birds-of-paradise-plus-theme-theme sourcerer-theme pug-mode ob-elixir org minitest ivy-purpose window-purpose imenu-list hide-comnt ht rake evil-unimpaired drupal-mode counsel-projectile counsel swiper undo-tree ivy rainbow-mode flycheck-elixir-dogma metalheart-theme ruby-end org-projectile git-link flycheck-mix darkokai-theme emojify dash-functional iedit highlight fzf sql-indent wttrin alchemist elixir-mode jinja2-mode ansible-doc ansible tramp-term ssh powerline slack circe request websocket ranger js2-mode projectile flycheck magit magit-popup git-commit with-editor smartparens web-completion-data tern hydra edn multiple-cursors paredit cider spinner clojure-mode packed avy auto-complete anzu markdown-mode yasnippet company gitignore-mode helm popup helm-core async json-reformat alert f s dash package-build bind-key bind-map evil org-download skewer-mode simple-httpd evil-visual-mark-mode dumb-jump shut-up ansi commander ctable concurrent deferred ert-runner epc sanityinc-tomorrow-night-theme-theme inf-ruby ac-inf-ruby evil-jumper eyebrowse column-enforce-mode zonokai-theme zenburn-theme zen-and-art-theme zeal-at-point yaml-mode xterm-color ws-butler window-numbering which-key wgrep-ag web-mode web-beautify volatile-highlights vi-tilde-fringe uuidgen use-package underwater-theme ujelly-theme twilight-theme twilight-bright-theme twilight-anti-bright-theme tronesque-theme toxi-theme toc-org tao-theme tangotango-theme tango-plus-theme tango-2-theme tagedit sunny-day-theme sublime-themes subatomic256-theme subatomic-theme stekene-theme spacemacs-theme spaceline spacegray-theme soothe-theme soft-stone-theme soft-morning-theme soft-charcoal-theme smyx-theme smooth-scrolling smeargle slim-mode shell-pop seti-theme scss-mode sass-mode rvm ruby-tools ruby-test-mode rubocop rspec-mode robe reverse-theme restart-emacs rbenv rainbow-delimiters railscasts-theme quelpa purple-haze-theme projectile-rails professional-theme popwin planet-theme phoenix-dark-pink-theme phoenix-dark-mono-theme persp-mode pcre2el pastels-on-dark-theme paradox page-break-lines orgit organic-green-theme org-repo-todo org-present org-pomodoro org-plus-contrib org-bullets open-junk-file omtose-phellack-theme oldlace-theme occidental-theme obsidian-theme noctilux-theme niflheim-theme neotree naquadah-theme mustang-theme multi-term move-text monokai-theme monochrome-theme molokai-theme moe-theme mmm-mode minimal-theme material-theme markdown-toc majapahit-theme magit-gitflow macrostep lush-theme lorem-ipsum livid-mode linum-relative link-hint light-soap-theme leuven-theme less-css-mode json-mode js2-refactor js-doc jbeans-theme jazz-theme jade-mode ir-black-theme inkpot-theme info+ indent-guide ido-vertical-mode hungry-delete htmlize hl-todo highlight-parentheses highlight-numbers highlight-indentation heroku-theme hemisu-theme help-fns+ helm-themes helm-swoop helm-projectile helm-mode-manager helm-make helm-gitignore helm-flx helm-descbinds helm-dash helm-css-scss helm-company helm-c-yasnippet helm-ag hc-zenburn-theme gruvbox-theme gruber-darker-theme grandshell-theme gotham-theme google-translate golden-ratio gnuplot gmail-message-mode gitconfig-mode gitattributes-mode git-timemachine git-messenger gh-md gandalf-theme fullframe flycheck-pos-tip flx-ido flatui-theme flatland-theme firebelly-theme fill-column-indicator feature-mode farmhouse-theme fancy-battery expand-region exec-path-from-shell evil-visualstar evil-tutor evil-surround evil-search-highlight-persist evil-numbers evil-nerd-commenter evil-mc evil-matchit evil-magit evil-lisp-state evil-indent-plus evil-iedit-state evil-exchange evil-escape evil-ediff evil-args evil-anzu espresso-theme eshell-z eshell-prompt-extras esh-help emmet-mode elisp-slime-nav edit-server dracula-theme django-theme define-word darktooth-theme darkmine-theme darkburn-theme dakrone-theme cyberpunk-theme company-web company-tern company-statistics company-quickhelp colorsarenice-theme color-theme-sanityinc-tomorrow color-theme-sanityinc-solarized coffee-mode clues-theme clojure-snippets clj-refactor clean-aindent-mode cider-eval-sexp-fu chruby cherry-blossom-theme busybee-theme bundler buffer-move bubbleberry-theme bracketed-paste birds-of-paradise-plus-theme beacon badwolf-theme auto-yasnippet auto-highlight-symbol auto-compile apropospriate-theme anti-zenburn-theme ample-zen-theme ample-theme alect-themes aggressive-indent ag afternoon-theme adaptive-wrap ace-window ace-link ace-jump-helm-line ac-ispell))
 '(paradox-github-token t)
 '(pdf-view-midnight-colors '("#b2b2b2" . "#292b2e"))
 '(pos-tip-background-color "#073642")
 '(pos-tip-foreground-color "#93a1a1")
 '(rustic-ansi-faces
   ["#ffffff" "#c82829" "#718c00" "#eab700" "#3e999f" "#c9b4cf" "#8abeb7" "#4d4d4c"])
 '(safe-local-variable-values
   '((rspec-use-chruby . t)
     (rspec-use-docker-when-possible . t)
     (rspec-docker-cwd . "/backend/")
     (rspec-docker-container . test)
     (projectile-rails-custom-console-command . "docker-compose run console")
     (typescript-backend . tide)
     (typescript-backend . lsp)
     (javascript-backend . tide)
     (javascript-backend . tern)
     (javascript-backend . lsp)
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
 '(warning-suppress-log-types '((comp)))
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
 '(default ((t (:background nil))))
 '(highlight-parentheses-highlight ((nil (:weight ultra-bold))) t))
)
