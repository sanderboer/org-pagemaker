;;; pagemaker-el-templates.el --- Templates for pagemaker Org docs -*- lexical-binding: t; -*-
;; Version: 0.1.1
;; URL: https://github.com/sanderboer/org-pagemaker

;;; Commentary:
;; Interactive inserters for document headers, pages, and elements recognized by
;; the typst-pagemaker pipeline.

;;; Code:

(require 'org)
(require 'subr-x)

(defgroup pagemaker-el-templates nil
  "Templates for pagemaker Org docs."
  :group 'pagemaker-el)

(defconst pagemaker-el--page-sizes '("A5" "A4" "A3" "A2" "A1")
  "Known page sizes supported by parser.")

(defconst pagemaker-el--orientations '("landscape" "portrait"))
(defconst pagemaker-el--fit-options '("contain" "cover" "fill" "stretch"))
(defconst pagemaker-el--align-options '("left" "center" "right"))
(defconst pagemaker-el--valign-options '("top" "middle" "bottom"))
(defconst pagemaker-el--flow-options '("normal" "bottom-up" "center-out"))
(defconst pagemaker-el--element-types '("header" "subheader" "body" "figure" "svg" "pdf" "rectangle" "toc"))

(defun pagemaker-el--insert-lines (&rest lines)
  (dolist (ln lines)
    (insert ln)
    (unless (string-suffix-p "\n" ln)
      (insert "\n"))))

(defun pagemaker-el--relpath-from (path root)
  (let* ((abs (expand-file-name path))
         (root (file-name-as-directory (expand-file-name (or root default-directory)))))
    (file-relative-name abs root)))

;;;###autoload
(defun pagemaker-el-insert-document-template ()
  "Insert a document-level header template with common meta keys."
  (interactive)
  (let* ((title (read-string "Title: " (or (and (buffer-file-name)
                                                (file-name-base (buffer-file-name)))
                                            "Untitled")))
         (ps (completing-read "Page size: " pagemaker-el--page-sizes nil t nil nil "A4"))
         (orient (completing-read "Orientation: " pagemaker-el--orientations nil t nil nil "landscape"))
         (grid (read-string "Grid (colsxrows): " "12x8"))
         (margins (read-string "Margins TRBL mm (empty=none): " "15,15,15,15"))
         (font (read-string "Default font (#+FONT): " "Manrope"))
         (theme (completing-read "Theme: " '("light") nil t nil nil "light"))
         (grid-debug (completing-read "Grid debug: " '("false" "true") nil t nil nil "false")))
    (pagemaker-el--insert-lines
     (format "#+TITLE: %s" title)
     (format "#+PAGESIZE: %s" ps)
     (format "#+ORIENTATION: %s" orient)
     (format "#+GRID: %s" grid)
     (when (not (string-empty-p margins)) (format "#+MARGINS: %s" margins))
     (format "#+THEME: %s" theme)
     (format "#+GRID_DEBUG: %s" grid-debug)
     (format "#+FONT: %s" font)
     "#+STYLE_HEADER: font: Manrope, weight: bold, size: 24pt"
     "#+STYLE_SUBHEADER: font: Manrope, weight: semibold, size: 18pt"
     "#+STYLE_BODY: font: Manrope, size: 11pt"
     ""
     "* Page 1"
     ":PROPERTIES:"
     (format ":PAGE_SIZE: %s" ps)
     (format ":ORIENTATION: %s" orient)
     (format ":GRID: %s" grid)
     (when (not (string-empty-p margins)) (format ":MARGINS: %s" margins))
     ":END:"
     "")))

;;;###autoload
(defun pagemaker-el-insert-page (&optional master)
  "Insert a new page heading with property drawer.
With prefix MASTER (C-u), prompt for a master name; if provided, insert a
master-def page (sets :MASTER_DEF: name)."
  (interactive "P")
  (let* ((title (read-string "Page title: " "Page"))
         (ps (completing-read "Page size (empty=inherit): " (cons "" pagemaker-el--page-sizes) nil t nil nil ""))
         (orient (completing-read "Orientation (empty=inherit): " (cons "" pagemaker-el--orientations) nil t nil nil ""))
         (grid (read-string "Grid (empty=inherit): " ""))
         (margins (read-string "Margins TRBL mm (empty=inherit): " ""))
         (master-name (when master (read-string "Master name: ")))
         (is-master (and master-name (not (string-empty-p master-name)))))
    (pagemaker-el--insert-lines
     (format "* %s" title)
     ":PROPERTIES:"
     (when (and ps (not (string-empty-p ps))) (format ":PAGE_SIZE: %s" ps))
     (when (and orient (not (string-empty-p orient))) (format ":ORIENTATION: %s" orient))
     (when (and grid (not (string-empty-p grid))) (format ":GRID: %s" grid))
     (when (and margins (not (string-empty-p margins))) (format ":MARGINS: %s" margins))
     (when is-master (format ":MASTER_DEF: %s" master-name))
     ":END:"
     "")))

(defun pagemaker-el--insert-text-element (level type area style align valign justify padding flow)
  (pagemaker-el--insert-lines
   (make-string level ?*) " " (capitalize type)
   ":PROPERTIES:"
   (format ":TYPE: %s" type)
   (when (and area (not (string-empty-p area))) (format ":AREA: %s" area))
   (when (and style (not (string-empty-p style))) (format ":STYLE: %s" style))
   (when (and align (not (string-empty-p align))) (format ":ALIGN: %s" align))
   (when (and valign (not (string-empty-p valign))) (format ":VALIGN: %s" valign))
   (when (and justify (not (string-empty-p justify))) (format ":JUSTIFY: %s" justify))
   (when (and padding (not (string-empty-p padding))) (format ":PADDING: %s" padding))
   (when (and flow (not (string-empty-p flow))) (format ":FLOW: %s" flow))
   ":END:"
   ""
   "Your text here."
   ""))

(defun pagemaker-el--insert-figure-element (level area fit padding)
  (let* ((path (read-file-name "Image file: "))
         (rel (pagemaker-el--relpath-from path (locate-dominating-file default-directory 
                                                                       (lambda (d) (or (file-exists-p (expand-file-name "pyproject.toml" d))
                                                                                       (file-directory-p (expand-file-name ".git" d))))))))
    (pagemaker-el--insert-lines
     (make-string level ?*) " Figure"
     ":PROPERTIES:"
     ":TYPE: figure"
     (when (and area (not (string-empty-p area))) (format ":AREA: %s" area))
     (when (and fit (not (string-empty-p fit))) (format ":FIT: %s" fit))
     (when (and padding (not (string-empty-p padding))) (format ":PADDING: %s" padding))
     ":END:"
     (format "[[file:%s]]" rel)
     "")))

(defun pagemaker-el--insert-svg-element (level area padding)
  (let* ((path (read-file-name "SVG file: "))
         (rel (pagemaker-el--relpath-from path (locate-dominating-file default-directory 
                                                                       (lambda (d) (or (file-exists-p (expand-file-name "pyproject.toml" d))
                                                                                       (file-directory-p (expand-file-name ".git" d))))))))
    (pagemaker-el--insert-lines
     (make-string level ?*) " SVG"
     ":PROPERTIES:"
     ":TYPE: svg"
     (when (and area (not (string-empty-p area))) (format ":AREA: %s" area))
     (format ":SVG: %s" rel)
     (when (and padding (not (string-empty-p padding))) (format ":PADDING: %s" padding))
     ":END:"
     "")))

(defun pagemaker-el--insert-pdf-element (level area page scale padding)
  (let* ((path (read-file-name "PDF (or image) file: "))
         (rel (pagemaker-el--relpath-from path (locate-dominating-file default-directory 
                                                                       (lambda (d) (or (file-exists-p (expand-file-name "pyproject.toml" d))
                                                                                       (file-directory-p (expand-file-name ".git" d))))))))
    (pagemaker-el--insert-lines
     (make-string level ?*) " PDF"
     ":PROPERTIES:"
     ":TYPE: pdf"
     (when (and area (not (string-empty-p area))) (format ":AREA: %s" area))
     (format ":PDF: %s" rel)
     (when page (format ":PAGE: %s" page))
     (when scale (format ":SCALE: %s" scale))
     (when (and padding (not (string-empty-p padding))) (format ":PADDING: %s" padding))
     ":END:"
     "")))

(defun pagemaker-el--insert-rectangle-element (level area color alpha)
  (pagemaker-el--insert-lines
   (make-string level ?*) " Rectangle"
   ":PROPERTIES:"
   ":TYPE: rectangle"
   (when (and area (not (string-empty-p area))) (format ":AREA: %s" area))
   (format ":COLOR: %s" (or color "#3498db"))
   (format ":ALPHA: %s" (or alpha "1.0"))
   ":END:"
   ""))

(defun pagemaker-el--insert-toc-element (level area padding align)
  (pagemaker-el--insert-lines
   (make-string level ?*) " Table of Contents"
   ":PROPERTIES:"
   ":TYPE: toc"
   (when (and area (not (string-empty-p area))) (format ":AREA: %s" area))
   (when (and padding (not (string-empty-p padding))) (format ":PADDING: %s" padding))
   (when (and align (not (string-empty-p align))) (format ":ALIGN: %s" align))
   ":END:"
   ""))

;;;###autoload
(defun pagemaker-el-insert-element (&optional level)
  "Insert an element with pagemaker properties at outline LEVEL (default 2)."
  (interactive "P")
  (let* ((level (or (prefix-numeric-value level) 2))
         (type (completing-read "Element type: " pagemaker-el--element-types nil t))
         (area (read-string "Area (A1 or A1,C2, empty=auto): " "")))
    (pcase type
      ((or "header" "subheader" "body")
       (let* ((style (read-string "Style name (empty=use type): " ""))
              (align (completing-read "Align (empty=none): " (cons "" pagemaker-el--align-options) nil t nil nil ""))
              (valign (completing-read "VAlign (empty=none): " (cons "" pagemaker-el--valign-options) nil t nil nil ""))
              (justify (completing-read "Justify (empty=auto): " '("" "true" "false") nil t nil nil ""))
              (padding (read-string "Padding mm (e.g., 4 or 4,6 or 4,6,4,6): " ""))
              (flow (completing-read "Flow (empty=none): " (cons "" pagemaker-el--flow-options) nil t nil nil "")))
         (pagemaker-el--insert-text-element level type area style align valign justify padding flow)))
      ("figure"
       (let* ((fit (completing-read "Fit: " pagemaker-el--fit-options nil t nil nil "contain"))
              (padding (read-string "Padding mm (optional): " "")))
         (pagemaker-el--insert-figure-element level area fit padding)))
      ("svg"
       (let* ((padding (read-string "Padding mm (optional): " "")))
         (pagemaker-el--insert-svg-element level area padding)))
      ("pdf"
       (let* ((page (read-string "Page number: " "1"))
              (scale (read-string "Scale (1.0): " "1.0"))
              (padding (read-string "Padding mm (optional): " "")))
         (pagemaker-el--insert-pdf-element level area page scale padding)))
      ("rectangle"
       (let* ((color (read-string "Color (#hex or name): " "#3498db"))
              (alpha (read-string "Alpha (0..1): " "1.0")))
         (pagemaker-el--insert-rectangle-element level area color alpha)))
      ("toc"
       (let* ((padding (read-string "Padding mm (optional): " ""))
              (align (completing-read "Align (empty=none): " (cons "" pagemaker-el--align-options) nil t nil nil "")))
         (pagemaker-el--insert-toc-element level area padding align))))))

(provide 'pagemaker-el-templates)
;;; pagemaker-el-templates.el ends here
