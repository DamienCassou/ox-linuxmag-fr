;;; ox-linuxmag-tests.el --- Tests for ox-linuxmag   -*- lexical-binding: t; -*-

;; Copyright (C) 2022  Damien Cassou

;; Author: Damien Cassou <damien@cassou.me>
;; Version: 0.1.0
;; URL: https://github.com/DamienCassou/ox-linuxmag
;; Package-Requires: ((emacs "28.1"))

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Tests for ox-linuxmag.el.

;;; Code:

(require 'ert)
(require 'ox-linuxmag)

(defconst ox-linuxmag-tests-default-preamble
  "#+title: Titre de l'article
  #+author: Tristan Colombo
  #+author_description: Rédacteur en chef de GLMF
  #+description: Ceci est le chapeau de l'article.
  #+keywords: keyword1, keyword2, keyword3, keyword4
  #+logos: Logo1, Logo2
  ")

(defun ox-linuxmag-tests-export (string)
  "Export Org content STRING into a Linux Magazine ODT.
Make the buffer containing the result current."
  (let* ((xml-content (with-temp-buffer
                        (unless (string-prefix-p "#+title:" string)
                          (insert ox-linuxmag-tests-default-preamble)
                          (insert "\n"))
                        (insert string)
                        (org-export-as 'linuxmag)))
         (buffer (get-buffer-create "*ox-linuxmag-test*")))
    (switch-to-buffer buffer)
    (erase-buffer)
    (insert xml-content)
    (goto-char (point-min))))

(defun ox-linuxmag-tests-contain (string)
  "Return non-nil if the current buffer has STRING after point."
  (search-forward string nil t))

(defun ox-linuxmag-tests-contain-explainer (_string)
  "Return the content of the current buffer."
  (buffer-substring-no-properties (point-min) (point-max)))

(put #'ox-linuxmag-tests-contain 'ert-explainer #'ox-linuxmag-tests-contain-explainer)

(ert-deftest ox-linuxmag-tests-write-preamble ()
  (ox-linuxmag-tests-export
   "#+title: Titre de l'article
#+author: Tristan Colombo
#+author_description: Rédacteur en chef de GLMF
#+description: Ceci est le chapeau de l'article.
#+keywords: keyword1, keyword2, keyword3, keyword4
#+logos: Logo1, Logo2
")
  (should (ox-linuxmag-tests-contain "<text:h text:style-name=\"Heading\">Titre de l'article</text:h>"))
  (should (ox-linuxmag-tests-contain "<text:p text:style-name=\"Signature\">Tristan Colombo</text:p>"))
  (should (ox-linuxmag-tests-contain "<text:p text:style-name=\"Signature\">[Rédacteur en chef de GLMF]</text:p>"))
  (should (ox-linuxmag-tests-contain "<text:p text:style-name=\"chapeau\">Ceci est le chapeau de l'article.</text:p>"))
  (should (ox-linuxmag-tests-contain "<text:p text:style-name=\"Normal\">keyword1, keyword2, keyword3, keyword4</text:p>"))
  (should (ox-linuxmag-tests-contain "<text:p text:style-name=\"pragma\">/// Logo : Logo1 ///</text:p>"))
  (should (ox-linuxmag-tests-contain "<text:p text:style-name=\"pragma\">/// Logo : Logo2 ///</text:p>")))

(ert-deftest ox-linuxmag-tests-bold ()
  (ox-linuxmag-tests-export "Test *foo*")
  (should (ox-linuxmag-tests-contain "Test <text:span text:style-name=\"gras\">foo</text:span>")))

(ert-deftest ox-linuxmag-tests-code ()
  (ox-linuxmag-tests-export "Test ~foo~")
  (should (ox-linuxmag-tests-contain "Test <text:span text:style-name=\"code_5f_par\">foo</text:span>")))

(ert-deftest ox-linuxmag-tests-headline-numbered ()
  (ox-linuxmag-tests-export "* Titre 1\n** Titre 1.1")
  (should (ox-linuxmag-tests-contain "<text:h text:style-name=\"Heading_20_1\">1. Titre 1</text:h>"))
  (should (ox-linuxmag-tests-contain "<text:h text:style-name=\"Heading_20_2\">1.1 Titre 1.1</text:h>")))

(ert-deftest ox-linuxmag-tests-headline-unnumbered ()
  (ox-linuxmag-tests-export "* Titre 1\n:PROPERTIES:\n:UNNUMBERED: t\n:END:\n")
  (should (ox-linuxmag-tests-contain "<text:h text:style-name=\"Heading_20_1\">Titre 1</text:h>")))

(ert-deftest ox-linuxmag-tests-italic ()
  (ox-linuxmag-tests-export "Test /foo/")
  (should (ox-linuxmag-tests-contain "Test <text:span text:style-name=\"italic\">foo</text:span>")))

(ert-deftest ox-linuxmag-tests-link-fuzzy ()
  (ox-linuxmag-tests-export "Foo [[1]]")
  (should (ox-linuxmag-tests-contain "Foo <text:span text:style-name=\"gras\">[1]</text:span>")))

(ert-deftest ox-linuxmag-tests-link-url ()
  (ox-linuxmag-tests-export "Foo https://damien.cassou.me")
  (should (ox-linuxmag-tests-contain "Foo <text:span text:style-name=\"url\">https://damien.cassou.me</text:span>")))

(ert-deftest ox-linuxmag-tests-paragraph-note-PAO ()
  (ox-linuxmag-tests-export "#+ATTR_LINUXMAG: :note PAO\nLe texte")
  (should (ox-linuxmag-tests-contain "<text:p text:style-name=\"pragma\">/// Début note PAO ///</text:p>"))
  (should (ox-linuxmag-tests-contain "<text:p text:style-name=\"pragma\">Le texte</text:p>"))
  (should (ox-linuxmag-tests-contain "<text:p text:style-name=\"pragma\">/// Fin note PAO ///</text:p>")))

(ert-deftest ox-linuxmag-tests-paragraph-note-attention ()
  (ox-linuxmag-tests-export "#+ATTR_LINUXMAG: :note attention\nLe texte")
  (should (ox-linuxmag-tests-contain "<text:p text:style-name=\"pragma\">/// Début note : Attention ///</text:p>"))
  (should (ox-linuxmag-tests-contain "<text:p text:style-name=\"Normal\">Le texte</text:p>"))
  (should (ox-linuxmag-tests-contain "<text:p text:style-name=\"pragma\">/// Fin note ///</text:p>")))

(ert-deftest ox-linuxmag-tests-paragraph-note-avertissement ()
  (ox-linuxmag-tests-export "#+ATTR_LINUXMAG: :note avertissement\nLe texte")
  (should (ox-linuxmag-tests-contain "<text:p text:style-name=\"pragma\">/// Début note : Avertissement ///</text:p>"))
  (should (ox-linuxmag-tests-contain "<text:p text:style-name=\"Normal\">Le texte</text:p>"))
  (should (ox-linuxmag-tests-contain "<text:p text:style-name=\"pragma\">/// Fin note ///</text:p>")))

(ert-deftest ox-linuxmag-tests-paragraph-note-default ()
  (ox-linuxmag-tests-export "#+ATTR_LINUXMAG: :note t\nLe texte")
  (should (ox-linuxmag-tests-contain "<text:p text:style-name=\"pragma\">/// Début note ///</text:p>"))
  (should (ox-linuxmag-tests-contain "<text:p text:style-name=\"Normal\">Le texte</text:p>"))
  (should (ox-linuxmag-tests-contain "<text:p text:style-name=\"pragma\">/// Fin note ///</text:p>")))

(ert-deftest ox-linuxmag-tests-paragraph-picture ()
  (ox-linuxmag-tests-export "#+CAPTION: Une légende\n[[file:media/mypicture_01.png]]")
  (should (ox-linuxmag-tests-contain "<text:p text:style-name=\"pragma\">/// Image : mypicture_01.png ///</text:p>"))
  (should (ox-linuxmag-tests-contain "<text:p text:style-name=\"legende\">Fig. 1 : Une légende</text:p>")))

(ert-deftest ox-linuxmag-tests-paragraph-default ()
  (ox-linuxmag-tests-export "Some discussion.")
  (should (ox-linuxmag-tests-contain "<text:p text:style-name=\"Normal\">Some discussion.</text:p>")))

(ert-deftest ox-linuxmag-tests-unordered-list ()
  (ox-linuxmag-tests-export "- item 1\n- item 2\n")
  (should (ox-linuxmag-tests-contain "<text:p text:style-name=\"Normal\">- item 1\n</text:p>"))
  (should (ox-linuxmag-tests-contain "<text:p text:style-name=\"Normal\">- item 2\n</text:p>")))

(ert-deftest ox-linuxmag-tests-special-block-encadre ()
  (ox-linuxmag-tests-export "
#+ATTR_LINUXMAG: :titre Titre
#+begin_encadre
texte
#+end_encadre")
  (should (ox-linuxmag-tests-contain "<text:p text:style-name=\"pragma\">/// Début encadré ///</text:p>"))
  (should (ox-linuxmag-tests-contain "<text:h text:style-name=\"Heading_20_1\" text:outline-level=\"1\">Titre</text:h>"))
  (should (ox-linuxmag-tests-contain "<text:p text:style-name=\"Normal\">texte\n</text:p>"))
  (should (ox-linuxmag-tests-contain "<text:p text:style-name=\"pragma\">/// Fin encadré ///</text:p>")))

(ert-deftest ox-linuxmag-tests-src-block-default ()
  (ox-linuxmag-tests-export "
#+begin_src text
foo ~text~
bar
#+end_src")
  (should (ox-linuxmag-tests-contain "<text:p text:style-name=\"code\">foo <text:span text:style-name=\"code_5f_em\">text</text:span></text:p>"))
  (should (ox-linuxmag-tests-contain "<text:p text:style-name=\"code\">bar</text:p>")))

(ert-deftest ox-linuxmag-tests-src-block-console ()
  (ox-linuxmag-tests-export "
#+ATTR_LINUXMAG: :type console
#+begin_src text
bar
#+end_src")
  (should (ox-linuxmag-tests-contain "<text:p text:style-name=\"console\">bar</text:p>")))

(ert-deftest ox-linuxmag-tests-table ()
  (ox-linuxmag-tests-export "
| cell1.1 | cell1.2 |
| cell2.1 | cell2.2 |
")
  (should (ox-linuxmag-tests-contain "<table:table table:style-name=\"Tableau\">"))
  (should (ox-linuxmag-tests-contain "<table:table-column table:style-name=\"Tableau.A\" table:number-columns-repeated=\"2\"/>"))
  (should (ox-linuxmag-tests-contain "<table:table-cell table:style-name=\"Tableau.A1\" office:value-type=\"string\">"))
  (should (ox-linuxmag-tests-contain "<text:p text:style-name=\"Normal\">cell1.1</text:p>"))
  (should (ox-linuxmag-tests-contain "</table:table-cell>"))
  (should (ox-linuxmag-tests-contain "</table:table>")))

(ert-deftest ox-linuxmag-tests-target ()
  (ox-linuxmag-tests-export "<<1>> foo")
  (should (ox-linuxmag-tests-contain "<text:span text:style-name=\"gras\">[1]</text:span> foo")))

(ert-deftest ox-linuxmag-tests-underline ()
  (ox-linuxmag-tests-export "Click on _File_.")
  (should (ox-linuxmag-tests-contain "Click on <text:span text:style-name=\"menu\">File</text:span>.")))

;; tableb
;; table-cell
;; target
;; underline

(provide 'ox-linuxmag-tests)
;;; ox-linuxmag-tests.el ends here
