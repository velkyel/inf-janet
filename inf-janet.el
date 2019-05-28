;;; inf-janet.el --- Run an external Janet process in an Emacs buffer -*- lexical-binding: t; -*-

;; * Add the following lines to your .emacs file:
;;
;;    (require 'inf-janet)

;;    (setq inf-janet-program "~/janet/build/janet")
;;    or
;;    (setq inf-janet-program '("localhost" . 5555))

;;    (add-hook 'janet-mode-hook #'inf-janet-minor-mode)

(require 'comint)
(require 'janet-mode)


(defgroup inf-janet nil
  "Run an external janet process (REPL) in an Emacs buffer."
  :group 'janet-mode)

(defcustom inf-janet-prompt-read-only t
  "If non-nil, the prompt will be read-only.

Also see the description of `ielm-prompt-read-only'."
  :type 'boolean
  :group 'inf-janet)

;; TODO:
(defcustom inf-janet-filter-regexp
  "\\`\\s *\\(:\\(\\w\\|\\s_\\)\\)?\\s *\\'"
  "What not to save on inferior janet's input history.
Input matching this regexp is not saved on the input history in
Inferior janet mode.  Default is whitespace followed by 0 or 1
single-letter colon-keyword \(as in :a, :c, etc.)"
  :type 'regexp
  :group 'inf-janet)

(defvar inf-janet-mode-map
  (let ((map (copy-keymap comint-mode-map)))
    (define-key map "\C-x\C-e" #'inf-janet-eval-last-sexp)
    ;; (define-key map "\C-c\C-l" #'inf-janet-load-file)
    (define-key map "\C-c\M-o" #'inf-janet-clear-repl-buffer)
    map))

(defvar inf-janet-minor-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "\M-\C-x"  #'inf-janet-eval-defun)     ; Gnu convention
    (define-key map "\C-x\C-e" #'inf-janet-eval-last-sexp) ; Gnu convention
    (define-key map "\C-c\C-e" #'inf-janet-eval-last-sexp)
    (define-key map "\C-c\C-c" #'inf-janet-eval-defun)     ; SLIME/CIDER style
    (define-key map "\C-c\C-b" #'inf-janet-eval-buffer)
    (define-key map "\C-c\C-r" #'inf-janet-eval-region)
    (define-key map "\C-c\C-n" #'inf-janet-eval-form-and-next)
    (define-key map "\C-c\C-z" #'inf-janet-switch-to-repl)
    (define-key map "\C-c\C-l" #'inf-janet-load-file)
    map))

;;;###autoload
(define-minor-mode inf-janet-minor-mode
  "Minor mode for interacting with the inferior janet process buffer.

The following commands are available:

\\{inf-janet-minor-mode-map}"
  :lighter "" :keymap inf-janet-minor-mode-map
  nil)

(defcustom inf-janet-program "flisp"
  "The command used to start an inferior janet process in `inf-janet-mode'.

Alternative you can specify a TCP connection cons pair, instead
of command, consisting of a host and port
number (e.g. (\"localhost\" . 5555)).  That's useful if you're
often connecting to a remote REPL process."
  :type '(choice (string)
                 (cons string integer))
  :group 'inf-janet)

(defcustom inf-janet-load-command "(load \"%s\")\n"
  "Format-string for building a janet expression to load a file."
  :type 'string
  :group 'inf-janet)

(defcustom inf-janet-prompt "^[^=> \n]+=> *"
  "Regexp to recognize prompts in the Inferior janet mode."
  :type 'regexp
  :group 'inf-janet)

(defcustom inf-janet-subprompt " *#_=> *"
  "Regexp to recognize subprompts in the Inferior janet mode."
  :type 'regexp
  :group 'inf-janet)

(defvar inf-janet-buffer nil)

(defvar inf-janet-mode-hook '()
  "Hook for customizing Inferior janet mode.")

(put 'inf-janet-mode 'mode-class 'special)

(define-derived-mode inf-janet-mode comint-mode "Inferior janet"
  (setq comint-prompt-regexp inf-janet-prompt)
  (setq mode-line-process '(":%s"))
  ;; (scheme-mode-variables)
  (setq comint-get-old-input #'inf-janet-get-old-input)
  (setq comint-input-filter #'inf-janet-input-filter)
  (set (make-local-variable 'comint-prompt-read-only) inf-janet-prompt-read-only)
  (add-hook 'comint-preoutput-filter-functions #'inf-janet-preoutput-filter nil t))

(defun inf-janet-get-old-input ()
  (save-excursion
    (let ((end (point)))
      (backward-sexp)
      (buffer-substring (point) end))))

(defun inf-janet-input-filter (str)
  "Return t if STR does not match `inf-janet-filter-regexp'."
  (not (string-match inf-janet-filter-regexp str)))

(defun inf-janet-chomp (string)
  "Remove final newline from STRING."
  (if (string-match "[\n]\\'" string)
      (replace-match "" t t string)
    string))

(defun inf-janet-remove-subprompts (string)
  "Remove subprompts from STRING."
  (replace-regexp-in-string inf-janet-subprompt "" string))

(defun inf-janet-preoutput-filter (str)
  "Preprocess the output STR from interactive commands."
  (cond
   ((string-prefix-p "inf-janet-" (symbol-name (or this-command last-command)))
    ;; Remove subprompts and prepend a newline to the output string
    (inf-janet-chomp (concat "\n" (inf-janet-remove-subprompts str))))
   (t str)))

(defvar inf-janet-project-root-files    ;; TODO
  '("_darcs")
  "A list of files that can be considered project markers.")

(defun inf-janet-project-root ()
  "Retrieve the root directory of a project if available.

Fallback to `default-directory.' if not within a project."
  (or (car (remove nil
                   (mapcar (lambda
                             (file)
                             (locate-dominating-file default-directory file))
                           inf-janet-project-root-files)))
      default-directory))

(defun inf-janet-clear-repl-buffer ()
  (interactive)
  (let ((comint-buffer-maximum-size 0))
    (comint-truncate-buffer)))

;;;###autoload
(defun inf-janet (cmd)
  (interactive (list (if current-prefix-arg
                         (read-string "Run janet: " inf-janet-program)
                       inf-janet-program)))
  (if (not (comint-check-proc "*inf-janet*"))
      ;; run the new process in the project's root when in a project folder
      (let ((default-directory (inf-janet-project-root))
            (cmdlist (if (consp cmd)
                         (list cmd)
                       (split-string cmd))))
        (set-buffer (apply #'make-comint
                           "inf-janet" (car cmdlist) nil (cdr cmdlist)))
        (inf-janet-mode)))
  (setq inf-janet-buffer "*inf-janet*")
  (pop-to-buffer-same-window "*inf-janet*"))

;;;###autoload
(defalias 'run-janet 'inf-janet)

(defun inf-janet-eval-region (start end &optional and-go)
  (interactive "r\nP")
  ;; replace multiple newlines at the end of the region by a single one
  ;; or add one if there was no newline
  (let ((str (replace-regexp-in-string
              "[\n]*\\'" "\n"
              (buffer-substring-no-properties start end))))
    (comint-send-string (inf-janet-proc) str))
  (if and-go (inf-janet-switch-to-repl t)))

(defun inf-janet-eval-string (code)
  (comint-send-string (inf-janet-proc) (concat code "\n")))

(defun inf-janet-eval-defun (&optional and-go)
  (interactive "P")
  (save-excursion
    (end-of-defun)
    (let ((end (point)) (case-fold-search t))
      (beginning-of-defun)
      (inf-janet-eval-region (point) end and-go))))

(defun inf-janet-eval-buffer (&optional and-go)
  (interactive "P")
  (save-excursion
    (widen)
    (let ((case-fold-search t))
      (inf-janet-eval-region (point-min) (point-max) and-go))))

(defun inf-janet-eval-last-sexp (&optional and-go)
  (interactive "P")
  (inf-janet-eval-region (save-excursion (backward-sexp) (point)) (point) and-go))

(defun inf-janet-eval-form-and-next ()
  (interactive "")
  (while (not (zerop (car (syntax-ppss))))
    (up-list))
  (inf-janet-eval-last-sexp)
  (forward-sexp))

(defun inf-janet-switch-to-repl (eob-p)
  "Switch to the inferior process buffer.
With prefix argument EOB-P, positions cursor at end of buffer."
  (interactive "P")
  (if (get-buffer-process inf-janet-buffer)
      (let ((pop-up-frames
             ;; Be willing to use another frame
             ;; that already has the window in it.
             (or pop-up-frames
                 (get-buffer-window inf-janet-buffer t))))
        (pop-to-buffer inf-janet-buffer))
    (run-janet inf-janet-program))
  (when eob-p
    (push-mark)
    (goto-char (point-max))))


;;; Now that inf-janet-eval-/defun/region takes an optional prefix arg,
;;; these commands are redundant. But they are kept around for the user
;;; to bind if he wishes, for backwards functionality, and because it's
;;; easier to type C-c e than C-u C-c C-e.

(defun inf-janet-eval-region-and-go (start end)
  (interactive "r")
  (inf-janet-eval-region start end t))

(defun inf-janet-eval-defun-and-go ()
  (interactive)
  (inf-janet-eval-defun t))

(defvar inf-janet-prev-l/c-dir/file nil
  "Record last directory and file used in loading or compiling.
This holds a cons cell of the form `(DIRECTORY . FILE)'
describing the last `inf-janet-load-file' command.")

(defcustom inf-janet-source-modes '(janet-mode)
  "Used to determine if a buffer contains source code.
If it's loaded into a buffer that is in one of these major modes, it's
considered a janet source file by `inf-janet-load-file'.
Used by this command to determine defaults."
  :type '(repeat symbol)
  :group 'inf-janet)

;; (defun inf-janet-load-file (file-name)
;;   "Load a source file FILE-NAME into the inferior janet process."
;;   (interactive (comint-get-source "Load file: " inf-janet-prev-l/c-dir/file
;;                                   inf-janet-source-modes nil)) ; nil because LOAD
;;                                         ; doesn't need an exact name
;;   (comint-check-source file-name) ; Check to see if buffer needs saved.
;;   (setq inf-janet-prev-l/c-dir/file (cons (file-name-directory    file-name)
;;                                             (file-name-nondirectory file-name)))
;;   (comint-send-string (inf-janet-proc)
;;                       (format inf-janet-load-command file-name))
;;   (inf-janet-switch-to-repl t))

(defun inf-janet-connected-p ()
  (not (null inf-janet-buffer)))

;;; Documentation functions

;;; Command strings
;;; ===============


(defun inf-janet-proc ()
  "Return the current inferior process.
See variable `inf-janet-buffer'."
  (let ((proc (get-buffer-process (if (derived-mode-p 'inf-janet-mode)
                                      (current-buffer)
                                    inf-janet-buffer))))
    (or proc
        (error "No janet subprocess; see variable `inf-janet-buffer'"))))

(provide 'inf-janet)

