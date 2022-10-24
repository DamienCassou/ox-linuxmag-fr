;;; ox-linuxmag.el --- Org-mode exporter for the French GNU/Linux Magazine -*- lexical-binding: t; -*-

;; Copyright (C) 2022 Damien Cassou

;; Authors: Damien Cassou <damien@cassou.me>
;; Version: 0.1.0
;; URL: https://github.com/DamienCassou/ox-linuxmag
;; Package-Requires: ((emacs "28.1"))
;; Created: 15 Oct 2022

;; This file is not part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This Org exporter is based on the OpenDocument exporter and changes
;; only what needs to be changed so the output is compatible with
;; GNU/Linux Magazine (http://www.gnulinuxmag.com/).
;;
;; The ./resources/styles.xml and ./resources/template.xml files are
;; extracted from the OpenOffice Writer template file at
;; https://github.com/GLMF/outils_auteurs.

;;; Code:

(require 'cl-lib)
(require 'org-compat)
(require 'org-macs)
(require 'ox)
(require 'ox-odt)


(defconst ox-linuxmag--resources-dir
  (expand-file-name "resources" (file-name-directory (or load-file-name (buffer-file-name))))
  "Directory containing the style and template XML files.")

(defvar ox-linuxmag--inline-code-style "code_5f_par")

;; Create the 'linuxmag Org export backend:
(let ((odt-backend (org-export-get-backend 'odt)))
  (org-export-register-backend
   (org-export-create-backend
    :name 'linuxmag
    :parent (org-export-backend-name odt-backend)
    :transcoders
    `((template . ox-linuxmag--template)
      (bold . ox-linuxmag--bold)
      (code . ox-linuxmag--code)
      (headline . ox-linuxmag--headline)
      (italic . ox-linuxmag--italic)
      (item . ox-linuxmag--format-contents)
      (link . ox-linuxmag--link)
      (paragraph . ox-linuxmag--paragraph)
      (plain-list . ox-linuxmag--format-contents)
      (special-block . ox-linuxmag--special-block)
      (src-block . ox-linuxmag--src-block)
      (table . ox-linuxmag--table)
      (table-cell . ox-linuxmag--table-cell)
      (target . ox-linuxmag--target)
      (underline . ox-linuxmag--underline)
      ,@(org-export-backend-transcoders odt-backend))
    :menu '(?g "Export to ODT for GNU/Linux Magazine"
               ((?g "As ODT file" ox-linuxmag--export-to-odt)))
    :options
    `((:author-description "AUTHOR_DESCRIPTION" nil nil newline)
      (:logos "LOGOS" nil nil newline)
      ,@(org-export-backend-options odt-backend)))))

;; Main exporter functions

(defun ox-linuxmag--template (contents info)
  "Return complete document string after ODT conversion.
CONTENTS is the transcoded contents string.  RAW-DATA is the
original parsed data.  INFO is a plist holding export options."
  (ox-linuxmag--write-meta-file info)
  (ox-linuxmag--write-styles-file)
  (ox-linuxmag--write-template-file contents info))

(defun ox-linuxmag--write-meta-file (info)
  "Create the contents of the meta.xml file.

INFO is a plist holding contextual information."
  (let ((title (org-export-data (plist-get info :title) info))
	(subtitle (org-export-data (plist-get info :subtitle) info))
	(author (let ((author (plist-get info :author)))
		  (if (not author) "" (org-export-data author info))))
	(keywords (or (plist-get info :keywords) ""))
	(description (or (plist-get info :description) "")))
    (write-region
     (concat
      "<?xml version=\"1.0\" encoding=\"UTF-8\"?>
     <office:document-meta
         xmlns:office=\"urn:oasis:names:tc:opendocument:xmlns:office:1.0\"
         xmlns:xlink=\"http://www.w3.org/1999/xlink\"
         xmlns:dc=\"http://purl.org/dc/elements/1.1/\"
         xmlns:meta=\"urn:oasis:names:tc:opendocument:xmlns:meta:1.0\"
         xmlns:ooo=\"http://openoffice.org/2004/office\"
         office:version=\"1.2\">
       <office:meta>\n"
      (format "<dc:creator>%s</dc:creator>\n" author)
      (format "<meta:initial-creator>%s</meta:initial-creator>\n" author)
      ;; Date, if required.
      (when (plist-get info :with-date)
	;; Check if DATE is specified as an Org-timestamp.  If yes,
	;; include it as meta information.  Otherwise, just use
	;; today's date.
	(let* ((date (let ((date (plist-get info :date)))
		       (and (not (cdr date))
			    (eq (org-element-type (car date)) 'timestamp)
			    (car date)))))
	  (let ((iso-date (org-odt--format-timestamp date nil 'iso-date)))
	    (concat
	     (format "<dc:date>%s</dc:date>\n" iso-date)
	     (format "<meta:creation-date>%s</meta:creation-date>\n"
		     iso-date)))))
      (format "<meta:generator>%s</meta:generator>\n"
	      (plist-get info :creator))
      (format "<meta:keyword>%s</meta:keyword>\n" keywords)
      (format "<dc:subject>%s</dc:subject>\n" description)
      (format "<dc:title>%s</dc:title>\n" title)
      (when (org-string-nw-p subtitle)
	(format
	 "<meta:user-defined meta:name=\"subtitle\">%s</meta:user-defined>\n"
	 subtitle))
      "\n"
      "  </office:meta>\n" "</office:document-meta>")
     nil (concat org-odt-zip-dir "meta.xml"))
    ;; Add meta.xml in to manifest.
    (org-odt-create-manifest-file-entry "text/xml" "meta.xml")))

(defun ox-linuxmag--write-styles-file ()
  "Create the contents of the styles.xml file."
  (let* ((styles-file (expand-file-name "styles.xml" ox-linuxmag--resources-dir)))
    (copy-file styles-file (concat org-odt-zip-dir "styles.xml") t)
    ;; create a manifest entry for styles.xml
    (org-odt-create-manifest-file-entry "text/xml" "styles.xml")
    ;; Ensure we have write permissions to this file.
    (set-file-modes (concat org-odt-zip-dir "styles.xml") #o600)))

(defun ox-linuxmag--write-template-file (contents info)
  "Create the contents of the template.xml file.

CONTENTS is the exported text.  INFO is a plist holding
contextual information."
  (let* (;; `org-display-custom-times' should be accessed right
	 ;; within the context of the Org buffer.  So obtain its
	 ;; value before moving on to temp-buffer context down below.
	 (custom-time-fmts
	  (if org-display-custom-times
	      (cons (substring (car org-time-stamp-custom-formats) 1 -1)
		    (substring (cdr org-time-stamp-custom-formats) 1 -1))
	    '("%Y-%M-%d %a" . "%Y-%M-%d %a %H:%M"))))
    (with-temp-buffer
      (insert-file-contents
       (expand-file-name "template.xml" ox-linuxmag--resources-dir))
      ;; Write automatic styles.
      ;; - Position the cursor.
      (goto-char (point-min))
      (re-search-forward "  </office:automatic-styles>" nil t)
      (goto-char (match-beginning 0))
      ;; - Dump automatic table styles.
      (cl-loop for (style-name props) in
	       (plist-get org-odt-automatic-styles 'Table) do
	       (when (setq props (or (plist-get props :rel-width) "96"))
		 (insert (format org-odt-table-style-format style-name props))))
      ;; - Dump date-styles.
      (when (plist-get info :odt-use-date-fields)
	(insert (org-odt--build-date-styles (car custom-time-fmts)
					    "OrgDate1")
		(org-odt--build-date-styles (cdr custom-time-fmts)
					    "OrgDate2")))
      ;; Position the cursor to document body.
      (goto-char (point-min))
      (re-search-forward "</office:text>" nil nil)
      (goto-char (match-beginning 0))

      (ox-linuxmag--write-preamble info)
      (insert contents)
      (buffer-substring-no-properties (point-min) (point-max)))))

(defun ox-linuxmag--write-preamble (info)
  "Insert the first body lines of a template.xml file into the current buffer.

INFO is a plist holding contextual information."
  (cl-labels ((get-property (keyword) (plist-get info keyword))
              (export-property (keyword) (org-export-data (get-property keyword) info)))
    (insert (ox-linuxmag--format-texth (export-property :title) "Heading"))
    (insert (ox-linuxmag--format-textp (export-property :author) "Signature"))
    (insert (ox-linuxmag--format-textp
             (format "[%s]" (export-property :author-description))
             "Signature"))
    (insert (ox-linuxmag--format-textp (export-property :description) "chapeau"))
    (insert (ox-linuxmag--format-pragma "Mots-clés"))
    (insert (ox-linuxmag--format-textp (export-property :keywords)))
    (insert (ox-linuxmag--format-pragma (format "Fin %s" "Mots-clés")))
    (dolist (logo (split-string (get-property :logos) ","))
      (insert (ox-linuxmag--format-pragma (format "Logo : %s" (string-trim logo)))))))

(defvar nxml-auto-insert-xml-declaration-flag)

;;; Notes:
;;; - same as org-odt-export-to-odt but with 'ox-linuxmag as backend
(defun ox-linuxmag--export-to-odt (&optional async subtreep visible-only ext-plist)
  "Export current buffer to a ODT file.

If narrowing is active in the current buffer, only export its
narrowed part.

If a region is active, export that region.

A non-nil optional argument ASYNC means the process should happen
asynchronously.  The resulting file should be accessible through
the `org-export-stack' interface.

When optional argument SUBTREEP is non-nil, export the sub-tree
at point, extracting information from the headline properties
first.

When optional argument VISIBLE-ONLY is non-nil, don't export
contents of hidden elements.

EXT-PLIST, when provided, is a property list with external
parameters overriding Org default settings, but still inferior to
file-local settings.

Return output file's name."
  (interactive)
  (let ((outfile (org-export-output-file-name ".odt" subtreep)))
    (if async
	(org-export-async-start (lambda (f) (org-export-add-to-stack f 'linuxmag))
	  `(expand-file-name
	    (org-odt--export-wrap
	     ,outfile
	     (let* ((org-odt-embedded-images-count 0)
		    (org-odt-embedded-formulas-count 0)
		    (org-odt-automatic-styles nil)
		    (org-odt-object-counters nil)
		    ;; Let `htmlfontify' know that we are interested in
		    ;; collecting styles.
		    (hfy-user-sheet-assoc nil))
	       ;; Initialize content.xml and kick-off the export
	       ;; process.
	       (let ((out-buf
		      (progn
			(require 'nxml-mode)
			(let ((nxml-auto-insert-xml-declaration-flag nil))
			  (find-file-noselect
			   (concat org-odt-zip-dir "content.xml") t))))
		     (output (org-export-as
			      'linuxmag ,subtreep ,visible-only nil ,ext-plist)))
		 (with-current-buffer out-buf
		   (erase-buffer)
		   (insert output)))))))
      (org-odt--export-wrap
       outfile
       (let* ((org-odt-embedded-images-count 0)
	      (org-odt-embedded-formulas-count 0)
	      (org-odt-automatic-styles nil)
	      (org-odt-object-counters nil))
	 ;; Initialize content.xml and kick-off the export process.
	 (let ((output (org-export-as 'linuxmag subtreep visible-only nil ext-plist))
	       (out-buf (progn
			  (require 'nxml-mode)
			  (let ((nxml-auto-insert-xml-declaration-flag nil))
			    (find-file-noselect
			     (concat org-odt-zip-dir "content.xml") t)))))
	   (with-current-buffer out-buf (erase-buffer) (insert output))))))))


;; Transcoders

(defun ox-linuxmag--bold (_bold contents _info)
  "Transcode BOLD from Org to ODT.
CONTENTS is the text with bold markup.  INFO is a plist holding
contextual information."
  (ox-linuxmag--format-textspan contents "gras"))

(defun ox-linuxmag--code (code _contents _info)
  "Transcode a CODE object from Org to ODT.
CONTENTS is nil.  INFO is a plist used as a communication
channel."
  (format "<text:span text:style-name=\"%s\">%s</text:span>"
	  ox-linuxmag--inline-code-style
          (org-odt--encode-plain-text (org-element-property :value code))))

(defun ox-linuxmag--headline (headline contents info)
  "Transcode a HEADLINE element from Org to ODT.
CONTENTS holds the contents of the headline.  INFO is a plist
holding contextual information."
  (let* ((numbered (org-export-numbered-headline-p headline info))
         (format-headline-function
          (cl-function
           (lambda (_todo _todo-type _priority text _tags &key section-number &allow-other-keys)
             (cond
              ((not numbered) text)
              ((seq-contains-p section-number ?.) (format "%s %s" section-number text))
              (t (format "%s. %s" section-number text))))))
         (full-text (org-odt-format-headline--wrap headline nil info format-headline-function))
	 (level (org-export-get-relative-level headline info)))
    (concat
     (ox-linuxmag--format-texth full-text (format "Heading_20_%s" level))
     contents)))

(defun ox-linuxmag--italic (_italic contents _info)
  "Transcode ITALIC from Org to ODT.
CONTENTS is the text with italic markup.  INFO is a plist holding
contextual information."
  (ox-linuxmag--format-textspan contents "italic"))

(defun ox-linuxmag--link (link _desc _info)
  "Transcode a LINK object from Org to ODT.

DESC is the description part of the link, or the empty string.
INFO is a plist holding contextual information.  See
`org-export-data'."
  (let ((raw-link (org-element-property :raw-link link)))
    (cl-case (intern (org-element-property :type link))
      (fuzzy (ox-linuxmag--format-textspan (format "[%s]" raw-link) "gras"))
      ;; file links are taken care of by the containing paragraph:
      (file nil)
      (t (ox-linuxmag--format-textspan raw-link "url")))))

(defun ox-linuxmag--paragraph (paragraph contents info)
  "Transcode a PARAGRAPH element from Org to ODT.
CONTENTS is the contents of the paragraph, as a string.  INFO is
the plist used as a communication channel."
  (let ((note-type (org-export-read-attribute :attr_linuxmag paragraph :note)))
    (cond
     (note-type
      (ox-linuxmag--format-note contents note-type))
     ((org-odt--standalone-link-p paragraph info)
      (ox-linuxmag--format-figure paragraph contents info))
     (t
      (let* ((parent (org-export-get-parent paragraph))
             (parent-type (org-element-type parent))
             (prefix (when (eq parent-type 'item) "- ")))
        (org-odt--format-paragraph
         paragraph
         (concat prefix contents)
         info
         "Normal" "" ""))))))

(defun ox-linuxmag--format-note (contents note-type)
  "Return a string containing CONTENTS with a box markup.

NOTE-TYPE is a string representing the kind of box."
  (cl-case (intern note-type)
    ('PAO (concat
           (ox-linuxmag--format-pragma "Début note PAO")
           (ox-linuxmag--format-textp contents "pragma")
           (ox-linuxmag--format-pragma "Fin note PAO")))
    ((attention avertissement)
     (concat
      (ox-linuxmag--format-pragma (format "Début note : %s" (capitalize note-type)))
      (ox-linuxmag--format-textp contents)
      (ox-linuxmag--format-pragma "Fin note")))
    (t
     (concat
      (ox-linuxmag--format-pragma (format "Début note"))
      (ox-linuxmag--format-textp contents)
      (ox-linuxmag--format-pragma "Fin note")))))

(defun ox-linuxmag--format-figure (paragraph _contents info)
  "Return a string representing the figure in PARAGRAPH.

INFO is a plist holding contextual information."
  (let* ((link (org-element-map paragraph 'link #'identity info t))
         (path (org-element-property :path link))
         (pragma (format "Image : %s" (file-name-nondirectory path)))
         (filename (file-name-sans-extension path))
         ;; the figure number is the last number of the filename:
         (figure-number (save-match-data
                          (string-match (rx ?_ (* ?0) (group-n 1 (+ (any digit))) string-end) filename)
                          (match-string 1 filename)))
         (legend (org-export-data (org-export-get-caption paragraph) info)))
    (concat
     (ox-linuxmag--format-pragma pragma)
     (ox-linuxmag--format-textp (format "Fig. %s : %s" figure-number legend) "legende"))))

(defun ox-linuxmag--special-block (special-block contents _info)
  "Transcode a SPECIAL-BLOCK element from Org to ODT.
CONTENTS holds the contents of the item.  INFO is a plist holding
contextual information."
  (let ((type (intern (org-element-property :type special-block))))
    (cl-case type
      (encadre (ox-linuxmag--format-encadre special-block contents))
      (t contents))))

(defun ox-linuxmag--format-encadre (special-block contents)
  "Return a string containing CONTENTS surrounded with markup for a box.

SPECIAL-BLOCK is the element containing the whole block."
  (let ((title (org-export-read-attribute :attr_linuxmag special-block :titre)))
    (concat
     (ox-linuxmag--format-pragma "Début encadré")
     (format "<text:h text:style-name=\"Heading_20_1\" text:outline-level=\"1\">%s</text:h>" title)
     contents
     (ox-linuxmag--format-pragma "Fin encadré"))))

(defun ox-linuxmag--src-block (src-block _contents info)
  "Transcode a SRC-BLOCK element from Org to ODT.
CONTENTS holds the contents of the item.  INFO is a plist holding
contextual information."
  (let ((block-type (or (org-export-read-attribute :attr_linuxmag src-block :type)
                        "code")))
    (mapconcat
     (lambda (line)
       (ox-linuxmag--format-textp
        (let ((ox-linuxmag--inline-code-style "code_5f_em"))
          (org-export-data
           (org-element-parse-secondary-string line '(code) src-block) info))
        block-type))
     (org-split-string (org-element-property :value src-block) "\n")
     "\n")))

(defun ox-linuxmag--table (table contents info)
  "Transcode a TABLE element from Org to ODT.
CONTENTS holds the contents of the item.  INFO is a plist holding
contextual information."
  (concat
   (format "<table:table table:style-name=\"Tableau\">")
   (format "<table:table-column table:style-name=\"Tableau.A\" table:number-columns-repeated=\"%s\"/>"
           (length (org-odt-table-first-row-data-cells table info)))
   contents
   (format "</table:table>")))

(defun ox-linuxmag--table-cell (_table-cell contents _info)
  "Transcode a TABLE-CELL element from Org to ODT.
CONTENTS is nil.  INFO is a plist used as a communication
channel."
  (concat
   (format "<table:table-cell table:style-name=\"Tableau.A1\" office:value-type=\"string\">")
   (ox-linuxmag--format-textp contents)
   (format "</table:table-cell>")))

(defun ox-linuxmag--target (target _contents _info)
  "Transcode a TARGET object from Org to ODT.
CONTENTS is nil.  INFO is a plist holding contextual
information."
  (ox-linuxmag--format-textspan
   (format "[%s]" (org-element-property :value target))
   "gras"))

(defun ox-linuxmag--underline (_underline contents _info)
  "Transcode UNDERLINE from Org to ODT.
CONTENTS is the text with underline markup.  INFO is a plist
holding contextual information."
  (ox-linuxmag--format-textspan contents "menu"))


;; Utility functions

(defun ox-linuxmag--format-contents (_element contents _info)
  "Return CONTENTS."
  contents)

(defun ox-linuxmag--format-pragma (pragma)
  "Return a string containing a paragraph with PRAGMA."
  (ox-linuxmag--format-textp (format "/// %s ///" pragma) "pragma"))

(defun ox-linuxmag--format-textp (content &optional style)
  "Return a string containing a paragraph with CONTENT.
Use STYLE as the paragraph's style or \"Normal\" if nil."
  (format "<text:p text:style-name=\"%s\">%s</text:p>" (or style "Normal") content))

(defun ox-linuxmag--format-texth (content style)
  "Return a string containing a header with CONTENT.
Use STYLE as the header's style."
  (format "<text:h text:style-name=\"%s\">%s</text:h>" style content))

(defun ox-linuxmag--format-textspan (content style)
  "Return a string containing a span with CONTENT.
Use STYLE as the span's style."
  (format "<text:span text:style-name=\"%s\">%s</text:span>" style content))

(provide 'ox-linuxmag)
;;; ox-linuxmag.el ends here

;; LocalWords:  Transcode xml OpenDocument https github outils
;; LocalWords:  plist auteurs
;; LocalWords:  transcoded
