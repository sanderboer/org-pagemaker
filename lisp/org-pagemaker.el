;;; org-pagemaker.el --- Org-mode assistant for typst-pagemaker -*- lexical-binding: t; -*-

;; Package-Requires: ((emacs "27.1") (org "9.3"))
;; Keywords: documents, tools, convenience
;; URL: https://github.com/your/repo

;;; Commentary:
;;
;; A lightweight Emacs minor mode to help author Org-mode documents for the
;; A101 pagemaker pipeline. It provides:
;; - Snippets/templates for documents, pages, and elements
;; - Integration with the pagemaker CLI to build Typst, PDF, validate, and watch
;; - Font helpers (list/install/validate/search/analyze/specimen) via the CLI
;;
;; This package is intended to live in a separate repository later; for now it
;; sits alongside the pagemaker source to simplify development.
;;
;;; Code:

(require 'org)
(require 'subr-x)

(defgroup org-pagemaker nil
  "Org-mode assistant for A101 pagemaker."
  :group 'tools
  :prefix "org-pagemaker-")

(defcustom org-pagemaker-export-dir "export"
  "Default export directory for pagemaker builds."
  :type 'directory
  :group 'org-pagemaker)

(defcustom org-pagemaker-typst-output "deck.typ"
  "Default Typst filename produced by build/pdf commands."
  :type 'string
  :group 'org-pagemaker)

(defcustom org-pagemaker-pdf-output nil
  "Default PDF filename (nil means derive from Org filename)."
  :type '(choice (const :tag "Auto" nil) string)
  :group 'org-pagemaker)

(defcustom org-pagemaker-prefer-cli-binary t
  "If non-nil, prefer the `pagemaker` executable over python -m backend."
  :type 'boolean
  :group 'org-pagemaker)

(defcustom org-pagemaker-python-executable "python3"
  "Python executable to use when invoking the CLI module."
  :type 'string
  :group 'org-pagemaker)

(defcustom org-pagemaker-sanitize-pdfs nil
  "When non-nil, pass --sanitize-pdfs to PDF/watch builds."
  :type 'boolean
  :group 'org-pagemaker)

(defcustom org-pagemaker-validate-fonts nil
  "When non-nil, pass --validate-fonts to build/pdf commands."
  :type 'boolean
  :group 'org-pagemaker)

(defcustom org-pagemaker-strict-fonts nil
  "When non-nil, pass --strict-fonts to build/pdf commands."
  :type 'boolean
  :group 'org-pagemaker)

(defcustom org-pagemaker-use-compilation-mode t
  "When non-nil, display build/pdf/watch/validate buffers in `compilation-mode`."
  :type 'boolean
  :group 'org-pagemaker)

(defvar org-pagemaker--watch-proc nil
  "Internal: running watch process for current Emacs session (single).")

(defun org-pagemaker--ensure-saved ()
  "Save current buffer if visiting a file and modified."
  (when (and (buffer-file-name) (buffer-modified-p))
    (save-buffer)))

(defun org-pagemaker--project-root ()
  "Find the project root by looking for pyproject.toml or .git."
  (or (and (buffer-file-name)
           (locate-dominating-file (buffer-file-name) "pyproject.toml"))
      (and (buffer-file-name)
           (locate-dominating-file (buffer-file-name) ".git"))
      (and (buffer-file-name)
           (file-name-directory (buffer-file-name)))
      default-directory))

(defun org-pagemaker--binary-available-p ()
  (and org-pagemaker-prefer-cli-binary (executable-find "pagemaker")))

(defun org-pagemaker--pythonpath-env (root)
  "Return environment list adding src to PYTHONPATH under ROOT."
  (let* ((src (expand-file-name "src" root))
         (cur (getenv "PYTHONPATH"))
         (sep (if (eq system-type 'windows-nt) ";" ":"))
         (val (if (and cur (not (string-empty-p cur)))
                  (concat src sep cur)
                src)))
    (cons (concat "PYTHONPATH=" val) process-environment))

(defun org-pagemaker--cmd (&rest args)
  "Build command list to run pagemaker with ARGS."
  (if (org-pagemaker--binary-available-p)
      (cons "pagemaker" args)
    (append (list org-pagemaker-python-executable "-m" "pagemaker.cli") args)))

(defun org-pagemaker--maybe-apply-compilation-mode (buffer name)
  "Apply compilation-mode to BUFFER for certain command NAMEs."
  (when (and org-pagemaker-use-compilation-mode
             (string-match-p (rx "org-pagemaker-" (or "build" "pdf" "validate" "watch")) name)
             (fboundp 'compilation-mode))
    (with-current-buffer buffer
      (compilation-mode)
      ;; Add Typst-style location regexes (" --> file.typ:line:col")
      (let ((re1 '(org-pagemaker-typst-arrow
                   " --\?>? \(.*?\\.typ\):\\([0-9]+\\):\\([0-9]+\\)"
                   1 2 3))
            (re2 '(org-pagemaker-typst-arrow2
                   " --> \(.*?\\.typ\):\\([0-9]+\\):\\([0-9]+\\)"
                   1 2 3)))
        (when (boundp 'compilation-error-regexp-alist-alist)
          (unless (assoc 'org-pagemaker-typst-arrow compilation-error-regexp-alist-alist)
            (add-to-list 'compilation-error-regexp-alist-alist re1))
          (unless (assoc 'org-pagemaker-typst-arrow2 compilation-error-regexp-alist-alist)
            (add-to-list 'compilation-error-regexp-alist-alist re2)))
        (when (boundp 'compilation-error-regexp-alist)
          (setq-local compilation-error-regexp-alist
                      (append '(org-pagemaker-typst-arrow org-pagemaker-typst-arrow2)
                              compilation-error-regexp-alist)))))))

(defun org-pagemaker--start-process (name &rest args)
  "Start pagemaker process NAME with ARGS. Output goes to *NAME* buffer."
  (let* ((root (file-name-as-directory (org-pagemaker--project-root)))
         (default-directory root)
         (env (unless (org-pagemaker--binary-available-p)
                (org-pagemaker--pythonpath-env root)))
         (cmd (apply #'org-pagemaker--cmd args))
         (buf (get-buffer-create (format "*%s*" name)))
         (proc (make-process :name name
                             :buffer buf
                             :command cmd
                             :coding 'utf-8-unix
                             :noquery t
                             :connection-type 'pipe
                             :environment env)))
    (with-current-buffer buf
      (read-only-mode -1)
      (erase-buffer)
      (insert (format "$ cd %s\n$ %s\n\n"
                      default-directory
                      (mapconcat #'shell-quote-argument cmd " ")))
      (read-only-mode 1)
      (display-buffer buf))
    (set-process-sentinel
     proc
     (lambda (p e)
       (when (buffer-live-p (process-buffer p))
         (with-current-buffer (process-buffer p)
           (let ((inhibit-read-only t))
             (goto-char (point-max))
             (insert (format "\n[%s] %s" (string-trim (format "%s" p)) (string-trim e))))))))
    (org-pagemaker--maybe-apply-compilation-mode buf name)
    proc))

(defun org-pagemaker--current-org-file ()
  (unless (and (buffer-file-name)
               (string-match-p "\\.org\\'" (buffer-file-name)))
    (user-error "Current buffer is not visiting an Org file"))
  (file-relative-name (buffer-file-name) (org-pagemaker--project-root)))

;;;###autoload
(defun org-pagemaker-build (&optional arg)
  "Run `pagemaker build` for the current Org file.
With prefix ARG, prompt for export dir and typst output."
  (interactive "P")
  (org-pagemaker--ensure-saved)
  (let* ((org (org-pagemaker--current-org-file))
         (export (if arg (read-directory-name "Export dir: " nil nil nil org-pagemaker-export-dir)
                   org-pagemaker-export-dir))
         (typ (if arg (read-string "Typst output: " org-pagemaker-typst-output)
                org-pagemaker-typst-output))
         (args (list "build" org "--export-dir" export "-o" typ)))
    (when org-pagemaker-validate-fonts
      (setq args (append args (list "--validate-fonts"))))
    (when org-pagemaker-strict-fonts
      (setq args (append args (list "--strict-fonts"))))
    (apply #'org-pagemaker--start-process
           "org-pagemaker-build"
           args)))

;;;###autoload
(defun org-pagemaker-pdf (&optional arg)
  "Run `pagemaker pdf` for the current Org file.
With prefix ARG, prompt for export dir, typst and pdf output filenames."
  (interactive "P")
  (org-pagemaker--ensure-saved)
  (let* ((org (org-pagemaker--current-org-file))
         (export (if arg (read-directory-name "Export dir: " nil nil nil org-pagemaker-export-dir)
                   org-pagemaker-export-dir))
         (typ (if arg (read-string "Typst output: " org-pagemaker-typst-output)
                org-pagemaker-typst-output))
         (pdf (if arg (read-string "PDF output (nil=auto): " (or org-pagemaker-pdf-output ""))
                org-pagemaker-pdf-output))
         (args (list "pdf" org "--export-dir" export "-o" typ)))
    (when (and pdf (not (string-empty-p pdf)))
      (setq args (append args (list "--pdf-output" pdf))))
    (when org-pagemaker-validate-fonts
      (setq args (append args (list "--validate-fonts"))))
    (when org-pagemaker-strict-fonts
      (setq args (append args (list "--strict-fonts"))))
    (when org-pagemaker-sanitize-pdfs
      (setq args (append args (list "--sanitize-pdfs"))))
    (apply #'org-pagemaker--start-process "org-pagemaker-pdf" args)))

;;;###autoload
(defun org-pagemaker-open-last-pdf ()
  "Open the last (expected) PDF in the export directory for current Org."
  (interactive)
  (let* ((root (file-name-as-directory (org-pagemaker--project-root)))
         (org (org-pagemaker--current-org-file))
         (pdf-name (or org-pagemaker-pdf-output
                       (concat (file-name-base org) ".pdf")))
         (pdf-path (expand-file-name (concat (file-name-as-directory org-pagemaker-export-dir) pdf-name)
                                     root)))
    (if (file-exists-p pdf-path)
        (if (fboundp 'org-open-file)
            (org-open-file pdf-path)
          (find-file pdf-path))
      (user-error "PDF not found: %s" pdf-path))))

;;;###autoload
(defun org-pagemaker-validate (&optional strict)
  "Run `pagemaker validate` for the current Org file.
With prefix STRICT (C-u), pass --strict-assets."
  (interactive "P")
  (org-pagemaker--ensure-saved)
  (let* ((org (org-pagemaker--current-org-file))
         (args (list "validate" org)))
    (when strict
      (setq args (append args (list "--strict-assets"))))
    (apply #'org-pagemaker--start-process "org-pagemaker-validate" args)))

;;;###autoload
(defun org-pagemaker-watch (&optional pdf)
  "Run `pagemaker watch` for the current Org file.
With prefix PDF (C-u), build PDF on changes; otherwise Typst only."
  (interactive "P")
  (when (process-live-p org-pagemaker--watch-proc)
    (user-error "A pagemaker watch is already running; stop it first"))
  (org-pagemaker--ensure-saved)
  (let* ((org (org-pagemaker--current-org-file))
         (args (list "watch" org "--export-dir" org-pagemaker-export-dir "-o" org-pagemaker-typst-output)))
    (when pdf
      (setq args (append args (list "--pdf")))
      (when org-pagemaker-pdf-output
        (setq args (append args (list "--pdf-output" org-pagemaker-pdf-output))))
      (when org-pagemaker-sanitize-pdfs
        (setq args (append args (list "--sanitize-pdfs")))))
    (setq org-pagemaker--watch-proc (apply #'org-pagemaker--start-process "org-pagemaker-watch" args))
    (message "pagemaker watch started")))

;;;###autoload
(defun org-pagemaker-stop-watch ()
  "Stop the running pagemaker watch process, if any."
  (interactive)
  (if (and org-pagemaker--watch-proc (process-live-p org-pagemaker--watch-proc))
      (progn
        (delete-process org-pagemaker--watch-proc)
        (setq org-pagemaker--watch-proc nil)
        (message "pagemaker watch stopped"))
    (message "No running pagemaker watch")))

;;;###autoload
(defun org-pagemaker-ir ()
  "Run `pagemaker ir` for the current Org file and show JSON."
  (interactive)
  (org-pagemaker--ensure-saved)
  (let* ((root (file-name-as-directory (org-pagemaker--project-root)))
         (default-directory root)
         (env (unless (org-pagemaker--binary-available-p)
                (org-pagemaker--pythonpath-env root)))
         (cmd (apply #'org-pagemaker--cmd (list "ir" (org-pagemaker--current-org-file))))
         (program (car cmd))
         (args (cdr cmd))
         (buf (get-buffer-create "*org-pagemaker-ir.json*"))
         (status nil)
         (output (with-temp-buffer
                   (let ((process-environment (or env process-environment)))
                     (setq status (apply #'call-process program nil t nil args))
                     (buffer-string)))))
    (with-current-buffer buf
      (erase-buffer)
      (insert output)
      (goto-char (point-min))
      (cond
       ((fboundp 'json-mode) (json-mode))
       ((fboundp 'json-ts-mode) (json-ts-mode))
       (t (when (fboundp 'js-mode) (js-mode))))
      (display-buffer buf))
    (unless (and (integerp status) (zerop status))
      (message "pagemaker ir exited with status %s" status))))

;;;###autoload
(defun org-pagemaker-fonts-list (&optional details)
  "Run `pagemaker fonts list-all`. With prefix DETAILS (C-u), add --details."
  (interactive "P")
  (let ((args (list "fonts" "list-all")))
    (when details (setq args (append args (list "--details"))))
    (apply #'org-pagemaker--start-process "org-pagemaker-fonts" args)))

;;;###autoload
(defun org-pagemaker-fonts-install (font &optional variants force)
  "Install FONT via `pagemaker fonts install`. Optionally set VARIANTS and FORCE."
  (interactive
   (list (read-string "Font family: ")
         (let ((v (read-string "Variants (comma, empty=default): ")))
           (unless (string-empty-p v) v))
         current-prefix-arg))
  (let ((args (list "fonts" "install" font)))
    (when (and variants (not (string-empty-p variants)))
      (setq args (append args (list "--variants" variants))))
    (when force
      (setq args (append args (list "--force"))))
    (apply #'org-pagemaker--start-process "org-pagemaker-fonts-install" args)))

;;;###autoload
(defun org-pagemaker-fonts-validate (font &optional details)
  "Validate FONT availability via `pagemaker fonts validate`. With prefix DETAILS, add --details."
  (interactive
   (list (read-string "Font family: ") current-prefix-arg))
  (let ((args (list "fonts" "validate" font)))
    (when details (setq args (append args (list "--details"))))
    (apply #'org-pagemaker--start-process "org-pagemaker-fonts-validate" args)))

;;;###autoload
(defun org-pagemaker-fonts-search (query &optional details)
  "Search Google Fonts via `pagemaker fonts search`. With prefix DETAILS, add --details."
  (interactive (list (read-string "Search query: ") current-prefix-arg))
  (let ((args (list "fonts" "search" query)))
    (when details (setq args (append args (list "--details"))))
    (apply #'org-pagemaker--start-process "org-pagemaker-fonts-search" args)))

;;;###autoload
(defun org-pagemaker-fonts-analyze (&optional details)
  "Analyze current Org's font usage via `pagemaker fonts analyze`. With prefix DETAILS, add --details."
  (interactive "P")
  (org-pagemaker--ensure-saved)
  (let* ((org (org-pagemaker--current-org-file))
         (args (list "fonts" "analyze" org)))
    (when details (setq args (append args (list "--details"))))
    (apply #'org-pagemaker--start-process "org-pagemaker-fonts-analyze" args)))

;;;###autoload
(defun org-pagemaker-fonts-specimen (&optional pdf type)
  "Generate a font specimen via `pagemaker fonts specimen`.
With prefix PDF (C-u), also build the PDF. Prompt for TYPE when called with double prefix."
  (interactive "P")
  (let* ((do-pdf (and pdf t))
         (spec-type (if (and (listp current-prefix-arg)
                             (= (car current-prefix-arg) 16))
                        (completing-read "Specimen type: " '("showcase" "comparison" "simple") nil t nil nil "showcase")
                      "showcase"))
         (args (list "fonts" "specimen")))
    (setq args (append args (list "--type" spec-type)))
    (when do-pdf
      (setq args (append args (list "--pdf"))))
    (apply #'org-pagemaker--start-process "org-pagemaker-fonts-specimen" args)))

;; Template insertion (requires org-pagemaker-templates)
(autoload 'org-pagemaker-insert-document-template "org-pagemaker-templates" "Insert document header template." t)
(autoload 'org-pagemaker-insert-page "org-pagemaker-templates" "Insert a page with property drawer." t)
(autoload 'org-pagemaker-insert-element "org-pagemaker-templates" "Insert an element with properties." t)

(defvar org-pagemaker-mode-map
  (let ((map (make-sparse-keymap)))
    ;; Builds
    (define-key map (kbd "C-c p b") #'org-pagemaker-build)
    (define-key map (kbd "C-c p p") #'org-pagemaker-pdf)
    (define-key map (kbd "C-c p o") #'org-pagemaker-open-last-pdf)
    (define-key map (kbd "C-c p v") #'org-pagemaker-validate)
    (define-key map (kbd "C-c p w") #'org-pagemaker-watch)
    (define-key map (kbd "C-c p s") #'org-pagemaker-stop-watch)
    (define-key map (kbd "C-c p i") #'org-pagemaker-ir)
    ;; Templates
    (define-key map (kbd "C-c p t d") #'org-pagemaker-insert-document-template)
    (define-key map (kbd "C-c p t p") #'org-pagemaker-insert-page)
    (define-key map (kbd "C-c p t e") #'org-pagemaker-insert-element)
    ;; Fonts
    (define-key map (kbd "C-c p f l") #'org-pagemaker-fonts-list)
    (define-key map (kbd "C-c p f i") #'org-pagemaker-fonts-install)
    (define-key map (kbd "C-c p f v") #'org-pagemaker-fonts-validate)
    (define-key map (kbd "C-c p f s") #'org-pagemaker-fonts-search)
    (define-key map (kbd "C-c p f a") #'org-pagemaker-fonts-analyze)
    (define-key map (kbd "C-c p f p") #'org-pagemaker-fonts-specimen)
    map)
  "Keymap for `org-pagemaker-mode'.")

;;;###autoload
(define-minor-mode org-pagemaker-mode
  "Minor mode to assist authoring pagemaker Org documents."
  :lighter " PM"
  :keymap org-pagemaker-mode-map)

(provide 'org-pagemaker)
;;; org-pagemaker.el ends here
