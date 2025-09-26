;;; org-pagemaker-templates.el --- Templates for org-pagemaker Org docs -*- lexical-binding: t; -*-

;;; Commentary:
;; Interactive inserters for document headers, pages, and elements recognized by
;; the typst-pagemaker pipeline.

;;; Code:

(require 'org)
(require 'subr-x)

(defgroup org-pagemaker-templates nil
  "Templates for org-pagemaker Org docs."
  :group 'org-pagemaker)

(defconst org-pagemaker--page-sizes '("A5" "A4" "A3" "A2" "A1")
  "Known page sizes supported by parser.")

(defconst org-pagemaker--orientations '("landscape" "portrait"))
(defconst org-pagemaker--fit-options '("contain" "cover" "fill" "stretch"))
(defconst org-pagemaker--align-options '("left" "center" "right"))
(defconst org-pagemaker--valign-options '("top" "middle" "bottom"))
(defconst org-pagemaker--flow-options '("normal" "bottom-up" "center-out"))
(defconst org-pagemaker--element-types '("header" "subheader" "body" "figure" "svg" "pdf" "rectangle" "toc"))

(defun org-pagemaker--insert-lines (&rest lines)
  (dolist (ln lines)
    (insert ln)
    (unless (string-suffix-p "\n" ln)
      (insert "\n"))))

(defun org-pagemaker--relpath-from (path root)
  (let* ((abs (expand-file-name path))
         (root (file-name-as-directory (expand-file-name (or root default-directory)))))
    (file-relative-name abs root)))

;;;###autoload
(defun org-pagemaker-insert-document-template ()
  "Insert a document-level header template with common meta keys."
  (interactive)
  (let* ((title (read-string "Title: " (or (and (buffer-file-name)
                                                (file-name-base (buffer-file-name)))
                                            "Untitled")))
         (ps (completing-read "Page size: " org-pagemaker--page-sizes nil t nil nil "A4"))
         (orient (completing-read "Orientation: " org-pagemaker--orientations nil t nil nil "landscape"))
         (grid (read-string "Grid (colsxrows): " "12x8"))
         (margins (read-string "Margins TRBL mm (empty=none): " "15,15,15,15"))
         (font (read-string "Default font (#+FONT): " "Manrope"))
         (theme (completing-read "Theme: " '("light") nil t nil nil "light"))
         (grid-debug (completing-read "Grid debug: " '("false" "true") nil t nil nil "false")))
    (org-pagemaker--insert-lines
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
(defun org-pagemaker-insert-page (&optional master)
  "Insert a new page heading with property drawer.
With prefix MASTER (C-u), prompt for a master name; if provided, insert a
master-def page (sets :MASTER_DEF: name)."
  (interactive "P")
  (let* ((title (read-string "Page title: " "Page"))
         (ps (completing-read "Page size (empty=inherit): " (cons "" org-pagemaker--page-sizes) nil t nil nil ""))
         (orient (completing-read "Orientation (empty=inherit): " (cons "" org-pagemaker--orientations) nil t nil nil ""))
         (grid (read-string "Grid (empty=inherit): " ""))
         (margins (read-string "Margins TRBL mm (empty=inherit): " ""))
         (master-name (when master (read-string "Master name: ")))
         (is-master (and master-name (not (string-empty-p master-name)))))
    (org-pagemaker--insert-lines
     (format "* %s" title)
     ":PROPERTIES:"
     (when (and ps (not (string-empty-p ps))) (format ":PAGE_SIZE: %s" ps))
     (when (and orient (not (string-empty-p orient))) (format ":ORIENTATION: %s" orient))
     (when (and grid (not (string-empty-p grid))) (format ":GRID: %s" grid))
     (when (and margins (not (string-empty-p margins))) (format ":MARGINS: %s" margins))
     (when is-master (format ":MASTER_DEF: %s" master-name))
     ":END:"
     "")))

(defun org-pagemaker--insert-text-element (level type area style align valign justify padding flow)
  (org-pagemaker--insert-lines
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

(defun org-pagemaker--insert-figure-element (level area fit padding)
  (let* ((path (read-file-name "Image file: "))
         (rel (org-pagemaker--relpath-from path (locate-dominating-file default-directory 
                                                                       (lambda (d) (or (file-exists-p (expand-file-name "pyproject.toml" d))
                                                                                       (file-directory-p (expand-file-name ".git" d))))))))
    (org-pagemaker--insert-lines
     (make-string level ?*) " Figure"
     ":PROPERTIES:"
     ":TYPE: figure"
     (when (and area (not (string-empty-p area))) (format ":AREA: %s" area))
     (when (and fit (not (string-empty-p fit))) (format ":FIT: %s" fit))
     (when (and padding (not (string-empty-p padding))) (format ":PADDING: %s" padding))
     ":END:"
     (format "[[file:%s]]" rel)
     "")))

(defun org-pagemaker--insert-svg-element (level area padding)
  (let* ((path (read-file-name "SVG file: "))
         (rel (org-pagemaker--relpath-from path (locate-dominating-file default-directory 
                                                                       (lambda (d) (or (file-exists-p (expand-file-name "pyproject.toml" d))
                                                                                       (file-directory-p (expand-file-name ".git" d))))))))
    (org-pagemaker--insert-lines
     (make-string level ?*) " SVG"
     ":PROPERTIES:"
     ":TYPE: svg"
     (when (and area (not (string-empty-p area))) (format ":AREA: %s" area))
     (format ":SVG: %s" rel)
     (when (and padding (not (string-empty-p padding))) (format ":PADDING: %s" padding))
     ":END:"
     "")))

(defun org-pagemaker--insert-pdf-element (level area page scale padding)
  (let* ((path (read-file-name "PDF (or image) file: "))
         (rel (org-pagemaker--relpath-from path (locate-dominating-file default-directory 
                                                                       (lambda (d) (or (file-exists-p (expand-file-name "pyproject.toml" d))
                                                                                       (file-directory-p (expand-file-name ".git" d))))))))
    (org-pagemaker--insert-lines
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

(defun org-pagemaker--insert-rectangle-element (level area color alpha)
  (org-pagemaker--insert-lines
   (make-string level ?*) " Rectangle"
   ":PROPERTIES:"
   ":TYPE: rectangle"
   (when (and area (not (string-empty-p area))) (format ":AREA: %s" area))
   (format ":COLOR: %s" (or color "#3498db"))
   (format ":ALPHA: %s" (or alpha "1.0"))
   ":END:"
   ""))

(defun org-pagemaker--insert-toc-element (level area padding align)
  (org-pagemaker--insert-lines
   (make-string level ?*) " Table of Contents"
   ":PROPERTIES:"
   ":TYPE: toc"
   (when (and area (not (string-empty-p area))) (format ":AREA: %s" area))
   (when (and padding (not (string-empty-p padding))) (format ":PADDING: %s" padding))
   (when (and align (not (string-empty-p align))) (format ":ALIGN: %s" align))
   ":END:"
   ""))

;;;###autoload
(defun org-pagemaker-insert-element (&optional level)
  "Insert an element with pagemaker properties at outline LEVEL (default 2)."
  (interactive "P")
  (let* ((level (or (prefix-numeric-value level) 2))
         (type (completing-read "Element type: " org-pagemaker--element-types nil t))
         (area (read-string "Area (A1 or A1,C2, empty=auto): " "")))
    (pcase type
      ((or "header" "subheader" "body")
       (let* ((style (read-string "Style name (empty=use type): " ""))
              (align (completing-read "Align (empty=none): " (cons "" org-pagemaker--align-options) nil t nil nil ""))
              (valign (completing-read "VAlign (empty=none): " (cons "" org-pagemaker--valign-options) nil t nil nil ""))
              (justify (completing-read "Justify (empty=auto): " '("" "true" "false") nil t nil nil ""))
              (padding (read-string "Padding mm (e.g., 4 or 4,6 or 4,6,4,6): " ""))
              (flow (completing-read "Flow (empty=none): " (cons "" org-pagemaker--flow-options) nil t nil nil "")))
         (org-pagemaker--insert-text-element level type area style align valign justify padding flow)))
      ("figure"
       (let* ((fit (completing-read "Fit: " org-pagemaker--fit-options nil t nil nil "contain"))
              (padding (read-string "Padding mm (optional): " "")))
         (org-pagemaker--insert-figure-element level area fit padding)))
      ("svg"
       (let* ((padding (read-string "Padding mm (optional): " "")))
         (org-pagemaker--insert-svg-element level area padding)))
      ("pdf"
       (let* ((page (read-string "Page number: " "1"))
              (scale (read-string "Scale (1.0): " "1.0"))
              (padding (read-string "Padding mm (optional): " "")))
         (org-pagemaker--insert-pdf-element level area page scale padding)))
      ("rectangle"
       (let* ((color (read-string "Color (#hex or name): " "#3498db"))
              (alpha (read-string "Alpha (0..1): " "1.0")))
         (org-pagemaker--insert-rectangle-element level area color alpha)))
      ("toc"
       (let* ((padding (read-string "Padding mm (optional): " ""))
              (align (completing-read "Align (empty=none): " (cons "" org-pagemaker--align-options) nil t nil nil "")))
         (org-pagemaker--insert-toc-element level area padding align))))))

(provide 'org-pagemaker-templates)
;;; org-pagemaker-templates.el ends here
