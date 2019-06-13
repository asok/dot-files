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
   dotspacemacs-configuration-layer-path '()
   ;; List of configuration layers to load.
   dotspacemacs-configuration-layers
   '(typescript
     html
     csv
     rust
     windows-scripts
     php
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
                      auto-completion-return-key-behavior nil
                      auto-completion-tab-key-behavior 'complete
                      auto-completion-complete-with-key-sequence "jk")
     ;; better-defaults
     emacs-lisp
     ;; react
     git
     clojure
     javascript
     org
     markdown
     dash
     themes-megapack
     lsp
     (ruby :variables
           ruby-version-manager 'chruby
           ruby-test-runner     'rspec
           ruby-backend         'lsp)
     ruby-on-rails
     (shell :variables shell-default-term-shell "/bin/zsh"
            shell-default-shell 'eshell)
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
   dotspacemacs-startup-lists '(recents projects)
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
                         solarized-light
                         solarized-dark
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
   dotspacemacs-default-font `("Fira Code"
                               :size ,(if (<= (x-display-pixel-width) 1920)
                                          12.0
                                        12.0)
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
   evil-escape-key-sequence nil;"kj"
   evil-symbol-word-search t
   evil-search-module 'isearch
   wgrep-auto-save-buffer t
   git-commit-finish-query-functions '()
   init-file-debug t
   rspec-autosave-buffer t
   projectile-enable-caching t
   projectile-indexing-method 'alien
   shell-pop-autocd-to-working-dir nil
   doc-view-continuous t
   evil-want-fine-undo t
   confirm-kill-emacs 'y-or-n-p
   auto-window-vscroll nil
   better-jumper-context 'buffer
   lsp-auto-guess-root t)

  (with-eval-after-load 'evil-maps
    (define-key evil-motion-state-map (kbd "C-o") 'better-jumper-jump-backward)
    (define-key evil-motion-state-map (kbd "<C-i>") 'better-jumper-jump-forward))

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
   beacon-blink-when-point-moves-vertically 5
   ivy-initial-inputs-alist nil
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
   )

  (eval-after-load 'magithub
    '(progn
       (require 'magit-popup)
       ;; (magithub-feature-autoinject t))
       (magithub-feature-autoinject '(commit-browse completion pull-requests-section))
       (define-key magit-magithub-comment-section-map (kbd "SPC") nil)
       (define-key magit-magithub-comment-section-map (kbd "RET") #'magithub-comment-view)
       ))

  (beacon-mode 1)

  (with-eval-after-load 'magit
    (magit-auto-revert-mode 1))

  (add-hook 'dired-mode-hook 'all-the-icons-dired-mode)

  (with-eval-after-load 'evil
    (evil-define-key 'visual global-map (kbd "v") #'er/expand-region)

    (evil-define-key 'insert global-map (kbd "C-j") #'yas-expand)
    (evil-define-key 'normal global-map (kbd "C-j") #'yas-expand)
    (evil-define-key 'normal global-map (kbd "M-p") (lambda () (interactive) (set-mark-command 4)))

    (add-to-list 'evil-normal-state-modes 'shell-mode)

    (add-to-list 'evil-emacs-state-modes 'term-mode)

    (add-hook 'term-mode-hook #'evil-emacs-state)
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

  (global-hl-line-mode -1)

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

  (setq spacemacs-default-jump-handlers '(dumb-jump-go evil-goto-definition))

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
  (fullframe magit-log magit-mode-quit-window)
  (fullframe wttrin wttrin-exit)

  (add-hook 'eval-expression-minibuffer-setup-hook #'eldoc-mode)
  (add-hook 'eval-expression-minibuffer-setup-hook #'paredit-mode)

  (defadvice load-theme
      (before theme-dont-propagate activate)
    (mapcar #'disable-theme custom-enabled-themes))

  (defadvice inf-ruby-switch-from-compilation
      (after asok/switch-inf-ruby-compilation-buffer activate)
    (switch-to-buffer-other-window (current-buffer)))

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
    (setq ivy-posframe-display-functions-alist
          '((swiper          . nil)
            (complete-symbol . ivy-posframe-display-at-point)
            (t               . ivy-posframe-display-at-frame-center))))

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

  (with-eval-after-load 'projectile-rails
    (add-hook 'projectile-rails-mode-hook
              (lambda ()
                (setq-local counsel-dash-docsets '("Ruby" "Ruby On Rails" "jQuery"))
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

  (add-hook 'js2-mode-hook
            (lambda ()
              (push '("function" . ?λ) prettify-symbols-alist)))

  ;; (defun asok/paste-and-reload ()
  ;;   (interactive)
  ;;   (evil-paste-after)
  ;;   (web-mode-reload))

  ;; (evil-define-key 'normal web-mode-map (kbd "p") #'asok/paste-and-reload)

  (evil-define-key 'normal global-map (kbd "M-SPC") #'ivy-switch-buffer)
  (define-key global-map (kbd "M-SPC") #'ivy-switch-buffer)

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
  (setq default-frame-alist '((ns-transparent-titlebar . t)
                              (ns-appearance . 'nil)))

  (with-eval-after-load 'gotham-theme
    (custom-theme-set-faces 'gotham '(js2-object-property ((t (:inherit 'font-lock-type-face)))))   )

  (add-to-load-path "~/projects/all-the-icons-ivy")
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

  (custom-theme-set-faces
   'doom-city-lights
   '(ivy-current-match ((t (:underline t))))
   '(font-lock-variable-name-face ((t (:foreground "#5EC4FF")))))

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

  ;; Unfortunately this causes a lot Emacs hangs (in ediff for example)
  ;; (when (window-system)
  ;;   (let ((alist '((33 . ".\\(?:\\(?:==\\|!!\\)\\|[!=]\\)")
  ;;                  (35 . ".\\(?:###\\|##\\|_(\\|[#(?[_{]\\)")
  ;;                  (36 . ".\\(?:>\\)")
  ;;                  (37 . ".\\(?:\\(?:%%\\)\\|%\\)")
  ;;                  (38 . ".\\(?:\\(?:&&\\)\\|&\\)")
  ;;                  (42 . ".\\(?:\\(?:\\*\\*/\\)\\|\\(?:\\*[*/]\\)\\|[*/>]\\)")
  ;;                  (43 . ".\\(?:\\(?:\\+\\+\\)\\|[+>]\\)")
  ;;                  (45 . ".\\(?:\\(?:-[>-]\\|<<\\|>>\\)\\|[<>}~-]\\)")
  ;;                  (46 . ".\\(?:\\(?:\\.[.<]\\)\\|[.=-]\\)")
  ;;                  (47 . ".\\(?:\\(?:\\*\\*\\|//\\|==\\)\\|[*/=>]\\)")
  ;;                  (48 . ".\\(?:x[a-zA-Z]\\)")
  ;;                  (58 . ".\\(?:::\\|[:=]\\)")
  ;;                  (59 . ".\\(?:;;\\|;\\)")
  ;;                  (60 . ".\\(?:\\(?:!--\\)\\|\\(?:~~\\|->\\|\\$>\\|\\*>\\|\\+>\\|--\\|<[<=-]\\|=[<=>]\\||>\\)\\|[*$+~/<=>|-]\\)")
  ;;                  (61 . ".\\(?:\\(?:/=\\|:=\\|<<\\|=[=>]\\|>>\\)\\|[<=>~]\\)")
  ;;                  (62 . ".\\(?:\\(?:=>\\|>[=>-]\\)\\|[=>-]\\)")
  ;;                  (63 . ".\\(?:\\(\\?\\?\\)\\|[:=?]\\)")
  ;;                  (91 . ".\\(?:]\\)")
  ;;                  (92 . ".\\(?:\\(?:\\\\\\\\\\)\\|\\\\\\)")
  ;;                  (94 . ".\\(?:=\\)")
  ;;                  (119 . ".\\(?:ww\\)")
  ;;                  (123 . ".\\(?:-\\)")
  ;;                  (124 . ".\\(?:\\(?:|[=|]\\)\\|[=>|]\\)")
  ;;                  (126 . ".\\(?:~>\\|~~\\|[>=@~-]\\)")
  ;;                  )
  ;;                ))
  ;;     (dolist (char-regexp alist)
  ;;       (set-char-table-range composition-function-table (car char-regexp)
  ;;                             `([,(cdr char-regexp) 0 font-shape-gstring])))))


  (with-eval-after-load 'eshell
    ;; (eshell/alias "be" "bundle exec \$*")
    ;; (eshell/alias "cdpr" "cd (projectile-rails-root)")
    )

  (with-eval-after-load 'company
    (add-to-list 'company-backends #'company-tabnine))

  (defun asok/setup-lsp ()
    ;; (lsp)
    ;; (setq-local url-http-attempt-keepalives nil)
    )

  (defun asok/setup-ruby ()
    (setq ruby-align-to-stmt-keywords '(if begin def)))

  (add-hook 'ruby-mode-hook #'asok/setup-lsp t)
  (add-hook 'ruby-mode-hook #'asok/setup-ruby)

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

  (add-hook 'js2-mode-hook
            (lambda ()
              (push '("function" . ?λ) prettify-symbols-alist)))
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
 ;; '(ansi-term-color-vector
 ;;   [unspecified "#FFFFFF" "#d15120" "#5f9411" "#d2ad00" "#6b82a7" "#a66bab" "#6b82a7" "#505050"] t)
 '(custom-safe-themes
   (quote
    ("a2e7b508533d46b701ad3b055e7c708323fb110b6676a8be458a758dd8f24e27" "5999e12c8070b9090a2a1bbcd02ec28906e150bb2cdce5ace4f965c76cf30476" "d320493111089afba1563bc3962d8ea1117dd2b3abb189aeebdc8c51b5517ddb" "7356632cebc6a11a87bc5fcffaa49bae528026a78637acd03cae57c091afd9b9" "04dd0236a367865e591927a3810f178e8d33c372ad5bfef48b5ce90d4b476481" "39dd7106e6387e0c45dfce8ed44351078f6acd29a345d8b22e7b8e54ac25bac4" "44eec3c3e6e673c0d41b523a67b64c43b6e38f8879a7969f306604dcf908832c" "5b24babd20e58465e070a8d7850ec573fe30aca66c8383a62a5e7a3588db830b" "72a81c54c97b9e5efcc3ea214382615649ebb539cb4f2fe3a46cd12af72c7607" "5e2dc1360a92bb73dafa11c46ba0f30fa5f49df887a8ede4e3533c3ab6270e08" "1b8d67b43ff1723960eb5e0cba512a2c7a2ad544ddb2533a90101fd1852b426e" "bb08c73af94ee74453c90422485b29e5643b73b05e8de029a6909af6a3fb3f58" "c1390663960169cd92f58aad44ba3253227d8f715c026438303c09b9fb66cdfb" "9b59e147dbbde5e638ea1cde5ec0a358d5f269d27bd2b893a0947c4a867e14c1" "2dd32048690787844d8cba601ed3dd8b2f419e9bd985898d0c3792671a05b96b" "6bb466c89b7e3eedc1f19f5a0cfa53be9baf6077f4d4a6f9b5d087f0231de9c8" "f782ed87369a7d568cee28d14922aa6d639f49dd676124d817dd82c8208985d0" "9e147cee63e1a2a6b16021e0645bc66c633c42b849e78b8e295df4b7fe55c56a" "06f0b439b62164c6f8f84fdda32b62fb50b6d00e8b01c2208e55543a6337433a" "590759adc4a5bf7a183df81654cce13b96089e026af67d92b5eec658fb3fe22f" "4aee8551b53a43a883cb0b7f3255d6859d766b6c5e14bcb01bed572fcbef4328" "ce557950466bf42096853c6dac6875b9ae9c782b8665f62478980cc5e3b6028d" "d79ece4768dfc4bab488475b85c2a8748dcdc3690e11a922f6be5e526a20b485" "d09467d742f713443c7699a546c0300db1a75fed347e09e3f178ab2f3aa2c617" "72c7c8b431179cbcfcea4193234be6a0e6916d04c44405fc87905ae16bed422a" "8abee8a14e028101f90a2d314f1b03bed1cde7fd3f1eb945ada6ffc15b1d7d65" "cedd3b4295ac0a41ef48376e16b4745c25fa8e7b4f706173083f16d5792bb379" "5c6d40ef6e7bbe9e83dc0e32db794c7e9a6a0d9eb7d6a874aaf9744c053842b4" "19ba41b6dc0b5dd34e1b8628ad7ae47deb19f968fe8c31853d64ea8c4df252b8" "f04122bbc305a202967fa1838e20ff741455307c2ae80a26035fbf5d637e325f" "3632cf223c62cb7da121be0ed641a2243f7ec0130178722554e613c9ab3131de" "66132890ee1f884b4f8e901f0c61c5ed078809626a547dbefbb201f900d03fd8" "d8f76414f8f2dcb045a37eb155bfaa2e1d17b6573ed43fb1d18b936febc7bbc2" "fa2b58bb98b62c3b8cf3b6f02f058ef7827a8e497125de0254f56e373abee088" "5f8f1e226274b73f6e706431399a597dbfd64db34f3fba56a6ccf57d148a0e01" "4cf3221feff536e2b3385209e9b9dc4c2e0818a69a1cdb4b522756bcdf4e00a4" "51e228ffd6c4fff9b5168b31d5927c27734e82ec61f414970fc6bcce23bc140d" "68d36308fc6e7395f7e6355f92c1dd9029c7a672cbecf8048e2933a053cf27e6" "bffa9739ce0752a37d9b1eee78fc00ba159748f50dc328af4be661484848e476" default)))
 '(diary-entry-marker (quote font-lock-variable-name-face))
 '(emms-mode-line-icon-image-cache
   (quote
    (image :type xpm :ascent center :data "/* XPM */
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
\"#######..#\" };")))
 '(evil-want-Y-yank-to-eol t)
 '(fci-rule-character-color "#d9d9d9")
 '(fci-rule-color "#d9d9d9" t)
 '(gnus-logo-colors (quote ("#1ec1c4" "#bababa")) t)
 '(gnus-mode-line-image-cache
   (quote
    (image :type xpm :ascent center :data "/* XPM */
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
\"###########.######\" };")) t)
 '(minimap-mode nil)
 '(package-selected-packages
   (quote
    (powershell inflections seq birds-of-paradise-plus-theme-theme sourcerer-theme pug-mode ob-elixir org minitest ivy-purpose window-purpose imenu-list hide-comnt ht rake evil-unimpaired drupal-mode counsel-projectile counsel swiper undo-tree ivy php-mode rainbow-mode flycheck-elixir-dogma metalheart-theme ruby-end org-projectile git-link flycheck-mix darkokai-theme emojify dash-functional iedit highlight fzf sql-indent wttrin alchemist elixir-mode jinja2-mode ansible-doc ansible tramp-term ssh powerline slack circe request websocket ranger js2-mode projectile flycheck magit magit-popup git-commit with-editor smartparens web-completion-data tern hydra edn multiple-cursors paredit cider spinner clojure-mode packed avy auto-complete anzu markdown-mode yasnippet company gitignore-mode helm popup helm-core async json-reformat alert f s dash package-build bind-key bind-map evil org-download skewer-mode simple-httpd evil-visual-mark-mode dumb-jump shut-up ansi commander ctable concurrent deferred ert-runner epc sanityinc-tomorrow-night-theme-theme inf-ruby ac-inf-ruby evil-jumper eyebrowse column-enforce-mode zonokai-theme zenburn-theme zen-and-art-theme zeal-at-point yaml-mode xterm-color ws-butler window-numbering which-key wgrep-ag web-mode web-beautify volatile-highlights vi-tilde-fringe uuidgen use-package underwater-theme ujelly-theme twilight-theme twilight-bright-theme twilight-anti-bright-theme tronesque-theme toxi-theme toc-org tao-theme tangotango-theme tango-plus-theme tango-2-theme tagedit sunny-day-theme sublime-themes subatomic256-theme subatomic-theme stekene-theme spacemacs-theme spaceline spacegray-theme soothe-theme soft-stone-theme soft-morning-theme soft-charcoal-theme smyx-theme smooth-scrolling smeargle slim-mode shell-pop seti-theme scss-mode sass-mode rvm ruby-tools ruby-test-mode rubocop rspec-mode robe reverse-theme restart-emacs rbenv rainbow-delimiters railscasts-theme quelpa purple-haze-theme projectile-rails professional-theme popwin planet-theme phoenix-dark-pink-theme phoenix-dark-mono-theme persp-mode pcre2el pastels-on-dark-theme paradox page-break-lines orgit organic-green-theme org-repo-todo org-present org-pomodoro org-plus-contrib org-bullets open-junk-file omtose-phellack-theme oldlace-theme occidental-theme obsidian-theme noctilux-theme niflheim-theme neotree naquadah-theme mustang-theme multi-term move-text monokai-theme monochrome-theme molokai-theme moe-theme mmm-mode minimal-theme material-theme markdown-toc majapahit-theme magit-gitflow macrostep lush-theme lorem-ipsum livid-mode linum-relative link-hint light-soap-theme leuven-theme less-css-mode json-mode js2-refactor js-doc jbeans-theme jazz-theme jade-mode ir-black-theme inkpot-theme info+ indent-guide ido-vertical-mode hungry-delete htmlize hl-todo highlight-parentheses highlight-numbers highlight-indentation heroku-theme hemisu-theme help-fns+ helm-themes helm-swoop helm-projectile helm-mode-manager helm-make helm-gitignore helm-flx helm-descbinds helm-dash helm-css-scss helm-company helm-c-yasnippet helm-ag hc-zenburn-theme gruvbox-theme gruber-darker-theme grandshell-theme gotham-theme google-translate golden-ratio gnuplot gmail-message-mode gitconfig-mode gitattributes-mode git-timemachine git-messenger gh-md gandalf-theme fullframe flycheck-pos-tip flx-ido flatui-theme flatland-theme firebelly-theme fill-column-indicator feature-mode farmhouse-theme fancy-battery expand-region exec-path-from-shell evil-visualstar evil-tutor evil-surround evil-search-highlight-persist evil-numbers evil-nerd-commenter evil-mc evil-matchit evil-magit evil-lisp-state evil-indent-plus evil-iedit-state evil-exchange evil-escape evil-ediff evil-args evil-anzu espresso-theme eshell-z eshell-prompt-extras esh-help emmet-mode elisp-slime-nav edit-server dracula-theme django-theme define-word darktooth-theme darkmine-theme darkburn-theme dakrone-theme cyberpunk-theme company-web company-tern company-statistics company-quickhelp colorsarenice-theme color-theme-sanityinc-tomorrow color-theme-sanityinc-solarized coffee-mode clues-theme clojure-snippets clj-refactor clean-aindent-mode cider-eval-sexp-fu chruby cherry-blossom-theme busybee-theme bundler buffer-move bubbleberry-theme bracketed-paste birds-of-paradise-plus-theme beacon badwolf-theme auto-yasnippet auto-highlight-symbol auto-compile apropospriate-theme anti-zenburn-theme ample-zen-theme ample-theme alect-themes aggressive-indent ag afternoon-theme adaptive-wrap ace-window ace-link ace-jump-helm-line ac-ispell)))
 '(paradox-github-token t)
 '(safe-local-variable-values
   (quote
    ((eval setq cider-cljs-lein-repl "(do (use 'figwheel-sidecar.repl-api) (start-figwheel!) (cljs-repl))")
     (elixir-enable-compilation-checking . t)
     (elixir-enable-compilation-checking)
     (flycheck-disabled-checkers emacs-lisp-checkdoc))))
 '(vc-annotate-background nil)
 '(vc-annotate-color-map
   (quote
    ((20 . "#d54e53")
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
     (360 . "#b9ca4a"))))
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
 '(beacon-color "#d33682")
 '(custom-safe-themes
   (quote
    ("c7f10959cb1bc7a36ee355c765a1768d48929ec55dde137da51077ac7f899521" "bd7b7c5df1174796deefce5debc2d976b264585d51852c962362be83932873d9" "450f3382907de50be905ae8a242ecede05ea9b858a8ed3cc8d1fbdf2d57090af" "4b19d61c560a93ef90767abe513c11f236caec2864617d718aa366618133704c" "9b35c097a5025d5da1c97dba45fed027e4fb92faecbd2f89c2a79d2d80975181" "862a0ccc73c12df4df325427f9285fa6a5bbba593a77257f43b01c84269f51b0" "d2e9c7e31e574bf38f4b0fb927aaff20c1e5f92f72001102758005e53d77b8c9" "6d589ac0e52375d311afaa745205abb6ccb3b21f6ba037104d71111e7e76a3fc" "bf390ecb203806cbe351b966a88fc3036f3ff68cd2547db6ee3676e87327b311" "10461a3c8ca61c52dfbbdedd974319b7f7fd720b091996481c8fb1dded6c6116" "93a0885d5f46d2aeac12bf6be1754faa7d5e28b27926b8aa812840fe7d0b7983" "4697a2d4afca3f5ed4fdf5f715e36a6cac5c6154e105f3596b44a4874ae52c45" "6b2636879127bf6124ce541b1b2824800afc49c6ccd65439d6eb987dbf200c36" "725a0ac226fc6a7372074c8924c18394448bb011916c05a87518ad4563738668" "a2e7b508533d46b701ad3b055e7c708323fb110b6676a8be458a758dd8f24e27" "5999e12c8070b9090a2a1bbcd02ec28906e150bb2cdce5ace4f965c76cf30476" "d320493111089afba1563bc3962d8ea1117dd2b3abb189aeebdc8c51b5517ddb" "7356632cebc6a11a87bc5fcffaa49bae528026a78637acd03cae57c091afd9b9" "04dd0236a367865e591927a3810f178e8d33c372ad5bfef48b5ce90d4b476481" "39dd7106e6387e0c45dfce8ed44351078f6acd29a345d8b22e7b8e54ac25bac4" "44eec3c3e6e673c0d41b523a67b64c43b6e38f8879a7969f306604dcf908832c" "5b24babd20e58465e070a8d7850ec573fe30aca66c8383a62a5e7a3588db830b" "72a81c54c97b9e5efcc3ea214382615649ebb539cb4f2fe3a46cd12af72c7607" "5e2dc1360a92bb73dafa11c46ba0f30fa5f49df887a8ede4e3533c3ab6270e08" "1b8d67b43ff1723960eb5e0cba512a2c7a2ad544ddb2533a90101fd1852b426e" "bb08c73af94ee74453c90422485b29e5643b73b05e8de029a6909af6a3fb3f58" "c1390663960169cd92f58aad44ba3253227d8f715c026438303c09b9fb66cdfb" "9b59e147dbbde5e638ea1cde5ec0a358d5f269d27bd2b893a0947c4a867e14c1" "2dd32048690787844d8cba601ed3dd8b2f419e9bd985898d0c3792671a05b96b" "6bb466c89b7e3eedc1f19f5a0cfa53be9baf6077f4d4a6f9b5d087f0231de9c8" "f782ed87369a7d568cee28d14922aa6d639f49dd676124d817dd82c8208985d0" "9e147cee63e1a2a6b16021e0645bc66c633c42b849e78b8e295df4b7fe55c56a" "06f0b439b62164c6f8f84fdda32b62fb50b6d00e8b01c2208e55543a6337433a" "590759adc4a5bf7a183df81654cce13b96089e026af67d92b5eec658fb3fe22f" "4aee8551b53a43a883cb0b7f3255d6859d766b6c5e14bcb01bed572fcbef4328" "ce557950466bf42096853c6dac6875b9ae9c782b8665f62478980cc5e3b6028d" "d79ece4768dfc4bab488475b85c2a8748dcdc3690e11a922f6be5e526a20b485" "d09467d742f713443c7699a546c0300db1a75fed347e09e3f178ab2f3aa2c617" "72c7c8b431179cbcfcea4193234be6a0e6916d04c44405fc87905ae16bed422a" "8abee8a14e028101f90a2d314f1b03bed1cde7fd3f1eb945ada6ffc15b1d7d65" "cedd3b4295ac0a41ef48376e16b4745c25fa8e7b4f706173083f16d5792bb379" "5c6d40ef6e7bbe9e83dc0e32db794c7e9a6a0d9eb7d6a874aaf9744c053842b4" "19ba41b6dc0b5dd34e1b8628ad7ae47deb19f968fe8c31853d64ea8c4df252b8" "f04122bbc305a202967fa1838e20ff741455307c2ae80a26035fbf5d637e325f" "3632cf223c62cb7da121be0ed641a2243f7ec0130178722554e613c9ab3131de" "66132890ee1f884b4f8e901f0c61c5ed078809626a547dbefbb201f900d03fd8" "d8f76414f8f2dcb045a37eb155bfaa2e1d17b6573ed43fb1d18b936febc7bbc2" "fa2b58bb98b62c3b8cf3b6f02f058ef7827a8e497125de0254f56e373abee088" "5f8f1e226274b73f6e706431399a597dbfd64db34f3fba56a6ccf57d148a0e01" "4cf3221feff536e2b3385209e9b9dc4c2e0818a69a1cdb4b522756bcdf4e00a4" "51e228ffd6c4fff9b5168b31d5927c27734e82ec61f414970fc6bcce23bc140d" "68d36308fc6e7395f7e6355f92c1dd9029c7a672cbecf8048e2933a053cf27e6" "bffa9739ce0752a37d9b1eee78fc00ba159748f50dc328af4be661484848e476" default)))
 '(diary-entry-marker (quote font-lock-variable-name-face))
 '(emms-mode-line-icon-image-cache
   (quote
    (image :type xpm :ascent center :data "/* XPM */
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
\"#######..#\" };")))
 '(evil-want-Y-yank-to-eol t)
 '(fci-rule-character-color "#d9d9d9")
 '(fci-rule-color "#d9d9d9")
 '(flycheck-color-mode-line-face-to-color (quote mode-line-buffer-id))
 '(frame-background-mode (quote light))
 '(gnus-logo-colors (quote ("#1ec1c4" "#bababa")) t)
 '(gnus-mode-line-image-cache
   (quote
    (image :type xpm :ascent center :data "/* XPM */
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
\"###########.######\" };")) t)
 '(hl-todo-keyword-faces
   (quote
    (("TODO" . "#dc752f")
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
     ("XXXX" . "#dc752f"))))
 '(jdee-db-active-breakpoint-face-colors (cons "#0d0f11" "#7FC1CA"))
 '(jdee-db-requested-breakpoint-face-colors (cons "#0d0f11" "#A8CE93"))
 '(jdee-db-spec-breakpoint-face-colors (cons "#0d0f11" "#899BA6"))
 '(minimap-mode nil)
 '(package-selected-packages
   (quote
    (ivy-posframe better-jumper posframe load-bash-alias nord-theme ivy-rich yasnippet-snippets writeroom-mode visual-fill-column tide solarized-theme ruby-hash-syntax rjsx-mode racer org-brain magit-svn lsp-ui kaolin-themes json-navigator ivy-hydra helpful flycheck-flow transient elfeed-goodies editorconfig doom-themes doom-modeline eldoc-eval docker company-lsp lsp-mode eval-sexp-fu centered-cursor-mode cargo rust-mode sesman ac-php-core elfeed ghub treepy graphql restclient haml-mode all-the-icons goto-chg powershell inflections seq birds-of-paradise-plus-theme-theme sourcerer-theme pug-mode ob-elixir org minitest ivy-purpose window-purpose imenu-list hide-comnt ht rake evil-unimpaired drupal-mode counsel-projectile counsel swiper undo-tree ivy php-mode rainbow-mode flycheck-elixir-dogma metalheart-theme ruby-end org-projectile git-link flycheck-mix darkokai-theme emojify dash-functional iedit highlight fzf sql-indent wttrin alchemist elixir-mode jinja2-mode ansible-doc ansible tramp-term ssh powerline slack circe request websocket ranger js2-mode projectile flycheck magit magit-popup git-commit with-editor smartparens web-completion-data tern hydra edn multiple-cursors paredit cider spinner clojure-mode packed avy auto-complete anzu markdown-mode yasnippet company gitignore-mode helm popup helm-core async json-reformat alert f s dash package-build bind-key bind-map evil org-download skewer-mode simple-httpd evil-visual-mark-mode dumb-jump shut-up ansi commander ctable concurrent deferred ert-runner epc sanityinc-tomorrow-night-theme-theme inf-ruby ac-inf-ruby evil-jumper eyebrowse column-enforce-mode zonokai-theme zenburn-theme zen-and-art-theme zeal-at-point yaml-mode xterm-color ws-butler window-numbering which-key wgrep-ag web-mode web-beautify volatile-highlights vi-tilde-fringe uuidgen use-package underwater-theme ujelly-theme twilight-theme twilight-bright-theme twilight-anti-bright-theme tronesque-theme toxi-theme toc-org tao-theme tangotango-theme tango-plus-theme tango-2-theme tagedit sunny-day-theme sublime-themes subatomic256-theme subatomic-theme stekene-theme spacemacs-theme spaceline spacegray-theme soothe-theme soft-stone-theme soft-morning-theme soft-charcoal-theme smyx-theme smooth-scrolling smeargle slim-mode shell-pop seti-theme scss-mode sass-mode rvm ruby-tools ruby-test-mode rubocop rspec-mode robe reverse-theme restart-emacs rbenv rainbow-delimiters railscasts-theme quelpa purple-haze-theme projectile-rails professional-theme popwin planet-theme phoenix-dark-pink-theme phoenix-dark-mono-theme persp-mode pcre2el pastels-on-dark-theme paradox page-break-lines orgit organic-green-theme org-repo-todo org-present org-pomodoro org-plus-contrib org-bullets open-junk-file omtose-phellack-theme oldlace-theme occidental-theme obsidian-theme noctilux-theme niflheim-theme neotree naquadah-theme mustang-theme multi-term move-text monokai-theme monochrome-theme molokai-theme moe-theme mmm-mode minimal-theme material-theme markdown-toc majapahit-theme magit-gitflow macrostep lush-theme lorem-ipsum livid-mode linum-relative link-hint light-soap-theme leuven-theme less-css-mode json-mode js2-refactor js-doc jbeans-theme jazz-theme jade-mode ir-black-theme inkpot-theme info+ indent-guide ido-vertical-mode hungry-delete htmlize hl-todo highlight-parentheses highlight-numbers highlight-indentation heroku-theme hemisu-theme help-fns+ helm-themes helm-swoop helm-projectile helm-mode-manager helm-make helm-gitignore helm-flx helm-descbinds helm-dash helm-css-scss helm-company helm-c-yasnippet helm-ag hc-zenburn-theme gruvbox-theme gruber-darker-theme grandshell-theme gotham-theme google-translate golden-ratio gnuplot gmail-message-mode gitconfig-mode gitattributes-mode git-timemachine git-messenger gh-md gandalf-theme fullframe flycheck-pos-tip flx-ido flatui-theme flatland-theme firebelly-theme fill-column-indicator feature-mode farmhouse-theme fancy-battery expand-region exec-path-from-shell evil-visualstar evil-tutor evil-surround evil-search-highlight-persist evil-numbers evil-nerd-commenter evil-mc evil-matchit evil-magit evil-lisp-state evil-indent-plus evil-iedit-state evil-exchange evil-escape evil-ediff evil-args evil-anzu espresso-theme eshell-z eshell-prompt-extras esh-help emmet-mode elisp-slime-nav edit-server dracula-theme django-theme define-word darktooth-theme darkmine-theme darkburn-theme dakrone-theme cyberpunk-theme company-web company-tern company-statistics company-quickhelp colorsarenice-theme color-theme-sanityinc-tomorrow color-theme-sanityinc-solarized coffee-mode clues-theme clojure-snippets clj-refactor clean-aindent-mode cider-eval-sexp-fu chruby cherry-blossom-theme busybee-theme bundler buffer-move bubbleberry-theme bracketed-paste birds-of-paradise-plus-theme beacon badwolf-theme auto-yasnippet auto-highlight-symbol auto-compile apropospriate-theme anti-zenburn-theme ample-zen-theme ample-theme alect-themes aggressive-indent ag afternoon-theme adaptive-wrap ace-window ace-link ace-jump-helm-line ac-ispell)))
 '(paradox-github-token t)
 '(pdf-view-midnight-colors (quote ("#b2b2b2" . "#292b2e")))
 '(safe-local-variable-values
   (quote
    ((eval setq cider-cljs-lein-repl "(do (use 'figwheel-sidecar.repl-api) (start-figwheel!) (cljs-repl))")
     (elixir-enable-compilation-checking . t)
     (elixir-enable-compilation-checking)
     (flycheck-disabled-checkers emacs-lisp-checkdoc))))
 '(vc-annotate-background nil)
 '(vc-annotate-color-map
   (quote
    ((20 . "#d54e53")
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
     (360 . "#b9ca4a"))))
 '(vc-annotate-very-old-color nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
)
