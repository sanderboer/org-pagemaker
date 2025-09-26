;;; pagemaker-el.el --- Compatibility shims for org-pagemaker -*- lexical-binding: t; -*-

;; This file provides backward-compatible aliases from the old
;; pagemaker-el package names to the new org-pagemaker package.

;;; Code:

(require 'org-pagemaker)
(require 'org-pagemaker-templates)

;; Variable aliases (obsolete)
(define-obsolete-variable-alias 'pagemaker-el-export-dir 'org-pagemaker-export-dir "0.1")
(define-obsolete-variable-alias 'pagemaker-el-typst-output 'org-pagemaker-typst-output "0.1")
(define-obsolete-variable-alias 'pagemaker-el-pdf-output 'org-pagemaker-pdf-output "0.1")
(define-obsolete-variable-alias 'pagemaker-el-prefer-cli-binary 'org-pagemaker-prefer-cli-binary "0.1")
(define-obsolete-variable-alias 'pagemaker-el-python-executable 'org-pagemaker-python-executable "0.1")
(define-obsolete-variable-alias 'pagemaker-el-sanitize-pdfs 'org-pagemaker-sanitize-pdfs "0.1")
(define-obsolete-variable-alias 'pagemaker-el-validate-fonts 'org-pagemaker-validate-fonts "0.1")
(define-obsolete-variable-alias 'pagemaker-el-strict-fonts 'org-pagemaker-strict-fonts "0.1")
(define-obsolete-variable-alias 'pagemaker-el-use-compilation-mode 'org-pagemaker-use-compilation-mode "0.1")
(defvaralias 'pagemaker-el-mode-map 'org-pagemaker-mode-map)
(defvaralias 'pagemaker-el--watch-proc 'org-pagemaker--watch-proc)

;; Function aliases (obsolete)
;;;###autoload
(define-obsolete-function-alias 'pagemaker-el-build 'org-pagemaker-build "0.1")
;;;###autoload
(define-obsolete-function-alias 'pagemaker-el-pdf 'org-pagemaker-pdf "0.1")
;;;###autoload
(define-obsolete-function-alias 'pagemaker-el-open-last-pdf 'org-pagemaker-open-last-pdf "0.1")
;;;###autoload
(define-obsolete-function-alias 'pagemaker-el-validate 'org-pagemaker-validate "0.1")
;;;###autoload
(define-obsolete-function-alias 'pagemaker-el-watch 'org-pagemaker-watch "0.1")
;;;###autoload
(define-obsolete-function-alias 'pagemaker-el-stop-watch 'org-pagemaker-stop-watch "0.1")
;;;###autoload
(define-obsolete-function-alias 'pagemaker-el-ir 'org-pagemaker-ir "0.1")

;; Fonts subcommands
;;;###autoload
(define-obsolete-function-alias 'pagemaker-el-fonts-list 'org-pagemaker-fonts-list "0.1")
;;;###autoload
(define-obsolete-function-alias 'pagemaker-el-fonts-install 'org-pagemaker-fonts-install "0.1")
;;;###autoload
(define-obsolete-function-alias 'pagemaker-el-fonts-validate 'org-pagemaker-fonts-validate "0.1")
;;;###autoload
(define-obsolete-function-alias 'pagemaker-el-fonts-search 'org-pagemaker-fonts-search "0.1")
;;;###autoload
(define-obsolete-function-alias 'pagemaker-el-fonts-analyze 'org-pagemaker-fonts-analyze "0.1")
;;;###autoload
(define-obsolete-function-alias 'pagemaker-el-fonts-specimen 'org-pagemaker-fonts-specimen "0.1")

;; Minor mode alias
;;;###autoload
(define-obsolete-function-alias 'pagemaker-el-mode 'org-pagemaker-mode "0.1")

;; Template inserters
;;;###autoload
(define-obsolete-function-alias 'pagemaker-el-insert-document-template 'org-pagemaker-insert-document-template "0.1")
;;;###autoload
(define-obsolete-function-alias 'pagemaker-el-insert-page 'org-pagemaker-insert-page "0.1")
;;;###autoload
(define-obsolete-function-alias 'pagemaker-el-insert-element 'org-pagemaker-insert-element "0.1")

(provide 'pagemaker-el)
;;; pagemaker-el.el ends here
