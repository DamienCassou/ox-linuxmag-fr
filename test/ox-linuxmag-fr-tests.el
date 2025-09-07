;;; ox-linuxmag-fr-tests.el --- Tests for ox-linuxmag-fr   -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2023  Damien Cassou

;; Author: Damien Cassou <damien@cassou.me>
;; Version: 0.3.0
;; URL: https://github.com/DamienCassou/ox-linuxmag-fr
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

;; Tests for ox-linuxmag-fr.el.

;;; Code:

(require 'ert)
(require 'ox-linuxmag-fr)
(require 'ob-shell)

(defconst ox-linuxmag-fr-tests-default-preamble
  "#+title: Titre de l'article
  #+author: Tristan Colombo
  #+author_description: Rédacteur en chef de GLMF
  #+description: Ceci est le chapeau de l'article.
  #+keywords: keyword1, keyword2, keyword3, keyword4
  #+logos: Logo1, Logo2
  ")

(cl-defun ox-linuxmag-fr-tests-export
    (string &key
            (ox-linuxmag-fr-figure-files (make-hash-table))
            (ox-linuxmag-fr-other-files (make-hash-table))
            (ox-linuxmag-fr-basename "article"))
  "Export Org content STRING into a Linux Magazine ODT.
Make the buffer containing the result current.

OX-LINUXMAG-FR-FIGURE-FILES and OX-LINUXMAG-FR-OTHER-FILES are
hashtables.  OX-LINUXMAG-FR-BASENAME is a string."
  (let* ((org-confirm-babel-evaluate nil)
         (xml-content (with-temp-buffer
                        (unless (string-prefix-p "#+title:" string)
                          (insert ox-linuxmag-fr-tests-default-preamble)
                          (insert "\n"))
                        (insert string)
                        (org-export-as 'linuxmag-fr nil nil nil
                                       (list :ox-linuxmag-fr-figure-files
                                             ox-linuxmag-fr-figure-files
                                             :ox-linuxmag-fr-other-files
                                             ox-linuxmag-fr-other-files
                                             :ox-linuxmag-fr-basename
                                             ox-linuxmag-fr-basename
                                             :with-latex 'verbatim))))
         (buffer (get-buffer-create "*ox-linuxmag-fr-test*")))
    (switch-to-buffer buffer)
    (erase-buffer)
    (insert xml-content)
    (goto-char (point-min))))

(defun ox-linuxmag-fr-tests-contain (string)
  "Return non-nil if the current buffer has STRING after point."
  (search-forward string nil t))

(defun ox-linuxmag-fr-tests-contain-explainer (_string)
  "Return the content of the current buffer."
  (buffer-substring-no-properties (point-min) (point-max)))

(put #'ox-linuxmag-fr-tests-contain 'ert-explainer #'ox-linuxmag-fr-tests-contain-explainer)

(ert-deftest ox-linuxmag-fr-tests-write-preamble ()
  (ox-linuxmag-fr-tests-export
   "#+title: Titre de l'article
#+author: Tristan Colombo
#+author_description: Rédacteur en chef de GLMF
#+description: Ceci est le chapeau de l'article.
#+keywords: keyword1, keyword2, keyword3, keyword4
#+logos: Logo1, Logo2
")
  (should (ox-linuxmag-fr-tests-contain "<text:h text:style-name=\"Heading\">Titre de l'article</text:h>"))
  (should (ox-linuxmag-fr-tests-contain "<text:p text:style-name=\"Signature\">Tristan Colombo</text:p>"))
  (should (ox-linuxmag-fr-tests-contain "<text:p text:style-name=\"Signature\">[Rédacteur en chef de GLMF]</text:p>"))
  (should (ox-linuxmag-fr-tests-contain "<text:p text:style-name=\"chapeau\">Ceci est le chapeau de l'article.</text:p>"))
  (should (ox-linuxmag-fr-tests-contain "<text:p text:style-name=\"Normal\">keyword1, keyword2, keyword3, keyword4</text:p>"))
  (should (ox-linuxmag-fr-tests-contain "<text:p text:style-name=\"pragma\">/// Logo : Logo1 ///</text:p>"))
  (should (ox-linuxmag-fr-tests-contain "<text:p text:style-name=\"pragma\">/// Logo : Logo2 ///</text:p>")))

(ert-deftest ox-linuxmag-fr-tests-write-preamble-no-logos ()
  "Logos shouldn't be mandatory."
  (ox-linuxmag-fr-tests-export
   "#+title: Titre de l'article
#+author: Tristan Colombo
#+author_description: Rédacteur en chef de GLMF
#+description: Ceci est le chapeau de l'article.
#+keywords: keyword1, keyword2, keyword3, keyword4
")
  (should-not (ox-linuxmag-fr-tests-contain "<text:p text:style-name=\"pragma\">/// Logo")))

(ert-deftest ox-linuxmag-fr-tests-bold ()
  (ox-linuxmag-fr-tests-export "Test *foo*")
  (should (ox-linuxmag-fr-tests-contain "Test <text:span text:style-name=\"gras\">foo</text:span>")))

(ert-deftest ox-linuxmag-fr-tests-code ()
  (ox-linuxmag-fr-tests-export "Test ~foo~")
  (should (ox-linuxmag-fr-tests-contain "Test <text:span text:style-name=\"code_5f_par\">foo</text:span>")))

(ert-deftest ox-linuxmag-fr-tests-verbatim ()
  (ox-linuxmag-fr-tests-export "Test =foo=")
  (should (ox-linuxmag-fr-tests-contain "Test <text:span text:style-name=\"code_5f_par\">foo</text:span>")))

(ert-deftest ox-linuxmag-fr-tests-inline-src-block ()
  (ox-linuxmag-fr-tests-export "Test src_sh{echo foo}")
  (should (ox-linuxmag-fr-tests-contain "Test <text:span text:style-name=\"code_5f_par\">foo</text:span>")))

(ert-deftest ox-linuxmag-fr-tests-headline-numbered ()
  (ox-linuxmag-fr-tests-export "* Titre 1\n** Titre 1.1")
  (should (ox-linuxmag-fr-tests-contain "<text:h text:style-name=\"Heading_20_1\">1. Titre 1</text:h>"))
  (should (ox-linuxmag-fr-tests-contain "<text:h text:style-name=\"Heading_20_2\">1.1 Titre 1.1</text:h>")))

(ert-deftest ox-linuxmag-fr-tests-headline-unnumbered ()
  (ox-linuxmag-fr-tests-export "* Titre 1\n:PROPERTIES:\n:UNNUMBERED: t\n:END:\n")
  (should (ox-linuxmag-fr-tests-contain "<text:h text:style-name=\"Heading_20_1\">Titre 1</text:h>")))

(ert-deftest ox-linuxmag-fr-tests-italic ()
  (ox-linuxmag-fr-tests-export "Test /foo/")
  (should (ox-linuxmag-fr-tests-contain "Test <text:span text:style-name=\"italic\">foo</text:span>")))

(ert-deftest ox-linuxmag-fr-tests-link-fuzzy ()
  (ox-linuxmag-fr-tests-export "Foo [[1]]

<<1>> https://www.wikipedia.org/")
  (should (ox-linuxmag-fr-tests-contain "Foo <text:span text:style-name=\"gras\">[1]</text:span>")))

(ert-deftest ox-linuxmag-fr-tests-link-fuzzy-with-desc ()
  (ox-linuxmag-fr-tests-export "Voir le [[1][site wikipedia.org]]

<<1>> https://www.wikipedia.org/")
  (should (ox-linuxmag-fr-tests-contain "Voir le site wikipedia.org <text:span text:style-name=\"gras\">[1]</text:span>")))

(ert-deftest ox-linuxmag-fr-tests-link-fuzzy-figure ()
  (ox-linuxmag-fr-tests-export "
See figure [[mypicture]].

#+NAME: mypicture
#+CAPTION: A legend
[[file:media/mypicture.png]]

#+NAME: mypicture2
#+CAPTION: Another legend
[[file:media/mypicture2.png]]


See figure [[mypicture2]].")
  (should (ox-linuxmag-fr-tests-contain "See figure 1."))
  (should (ox-linuxmag-fr-tests-contain "See figure 2.")))

(ert-deftest ox-linuxmag-fr-tests-link-url ()
  (ox-linuxmag-fr-tests-export "Foo https://damien.cassou.me")
  (should (ox-linuxmag-fr-tests-contain "Foo <text:span text:style-name=\"url\">https://damien.cassou.me</text:span>")))

(ert-deftest ox-linuxmag-fr-tests-paragraph-note-PAO ()
  (ox-linuxmag-fr-tests-export "
#+begin_note_pao
Le texte
#+end_note_pao")
  (should (ox-linuxmag-fr-tests-contain "<text:p text:style-name=\"pragma\">/// Début note PAO ///</text:p>"))
  (should (ox-linuxmag-fr-tests-contain "<text:p text:style-name=\"pragma\">Le texte\n</text:p>"))
  (should (ox-linuxmag-fr-tests-contain "<text:p text:style-name=\"pragma\">/// Fin note PAO ///</text:p>")))

(ert-deftest ox-linuxmag-fr-tests-paragraph-note-attention ()
  (ox-linuxmag-fr-tests-export "
#+begin_note_attention
Le texte
#+end_note_attention")
  (should (ox-linuxmag-fr-tests-contain "<text:p text:style-name=\"pragma\">/// Début note : Attention ///</text:p>"))
  (should (ox-linuxmag-fr-tests-contain "<text:p text:style-name=\"Normal\">Le texte\n</text:p>"))
  (should (ox-linuxmag-fr-tests-contain "<text:p text:style-name=\"pragma\">/// Fin note ///</text:p>")))

(ert-deftest ox-linuxmag-fr-tests-paragraph-note-avertissement ()
  (ox-linuxmag-fr-tests-export "
#+begin_note_avertissement
Le texte
#+end_note_avertissement")
  (should (ox-linuxmag-fr-tests-contain "<text:p text:style-name=\"pragma\">/// Début note : Avertissement ///</text:p>"))
  (should (ox-linuxmag-fr-tests-contain "<text:p text:style-name=\"Normal\">Le texte\n</text:p>"))
  (should (ox-linuxmag-fr-tests-contain "<text:p text:style-name=\"pragma\">/// Fin note ///</text:p>")))

(ert-deftest ox-linuxmag-fr-tests-paragraph-note-default ()
  (ox-linuxmag-fr-tests-export "
#+begin_note
Le texte
#+end_note")
  (should (ox-linuxmag-fr-tests-contain "<text:p text:style-name=\"pragma\">/// Début note ///</text:p>"))
  (should (ox-linuxmag-fr-tests-contain "<text:p text:style-name=\"Normal\">Le texte\n</text:p>"))
  (should (ox-linuxmag-fr-tests-contain "<text:p text:style-name=\"pragma\">/// Fin note ///</text:p>")))

(ert-deftest ox-linuxmag-fr-tests-paragraph-note-default-several-paragraphs ()
  (ox-linuxmag-fr-tests-export "
#+begin_note
Le texte

Autre texte
#+end_note")
  (should (ox-linuxmag-fr-tests-contain "<text:p text:style-name=\"pragma\">/// Début note ///</text:p>"))
  (should (ox-linuxmag-fr-tests-contain "<text:p text:style-name=\"Normal\">Le texte\n</text:p>"))
  (should (ox-linuxmag-fr-tests-contain "<text:p text:style-name=\"Normal\">Autre texte\n</text:p>"))
  (should (ox-linuxmag-fr-tests-contain "<text:p text:style-name=\"pragma\">/// Fin note ///</text:p>")))

(ert-deftest ox-linuxmag-fr-tests-paragraph-picture ()
  (ox-linuxmag-fr-tests-export "
#+CAPTION: A legend
[[file:media/myPicture.png]]

#+CAPTION: Another legend
[[file:media/myOtherPicture.png]]
" :ox-linuxmag-fr-basename "myArticle")
  (should (ox-linuxmag-fr-tests-contain "<text:p text:style-name=\"pragma\">/// Image : myArticle_myPicture_01.png ///</text:p>"))
  (should (ox-linuxmag-fr-tests-contain "<text:p text:style-name=\"legende\">Fig. 1 : A legend</text:p>"))
  (should (ox-linuxmag-fr-tests-contain "<text:p text:style-name=\"pragma\">/// Image : myArticle_myOtherPicture_02.png ///</text:p>"))
  (should (ox-linuxmag-fr-tests-contain "<text:p text:style-name=\"legende\">Fig. 2 : Another legend</text:p>")))

(ert-deftest ox-linuxmag-fr-tests-paragraph-picture-several-caption-lines ()
  (ox-linuxmag-fr-tests-export "
#+CAPTION: first line,
#+CAPTION: second line
[[file:media/myPicture.png]]
" :ox-linuxmag-fr-basename "myArticle")
  (should (ox-linuxmag-fr-tests-contain "<text:p text:style-name=\"pragma\">/// Image : myArticle_myPicture_01.png ///</text:p>"))
  (should (ox-linuxmag-fr-tests-contain "<text:p text:style-name=\"legende\">Fig. 1 : first line, "))
  (should (ox-linuxmag-fr-tests-contain "second line</text:p>")))

(ert-deftest ox-linuxmag-fr-tests-paragraph-default ()
  (ox-linuxmag-fr-tests-export "Some discussion.")
  (should (ox-linuxmag-fr-tests-contain "<text:p text:style-name=\"Normal\">Some discussion.</text:p>")))

(ert-deftest ox-linuxmag-fr-tests-unordered-list ()
  (ox-linuxmag-fr-tests-export "- item 1\n- item 2\n")
  (should (ox-linuxmag-fr-tests-contain "<text:p text:style-name=\"Normal\">- item 1\n</text:p>"))
  (should (ox-linuxmag-fr-tests-contain "<text:p text:style-name=\"Normal\">- item 2\n</text:p>")))

(ert-deftest ox-linuxmag-fr-tests-ordered-list ()
  (ox-linuxmag-fr-tests-export "1. item 1\n1. item 2\n")
  (should (ox-linuxmag-fr-tests-contain "<text:p text:style-name=\"Normal\">1. item 1\n</text:p>"))
  (should (ox-linuxmag-fr-tests-contain "<text:p text:style-name=\"Normal\">2. item 2\n</text:p>")))

(ert-deftest ox-linuxmag-fr-tests-special-block-encadre ()
  (ox-linuxmag-fr-tests-export "
#+ATTR_LINUXMAG-FR: :titre Titre
#+begin_encadre
texte
#+end_encadre")
  (should (ox-linuxmag-fr-tests-contain "<text:p text:style-name=\"pragma\">/// Début encadré ///</text:p>"))
  (should (ox-linuxmag-fr-tests-contain "<text:h text:style-name=\"Heading_20_1\" text:outline-level=\"1\">Titre</text:h>"))
  (should (ox-linuxmag-fr-tests-contain "<text:p text:style-name=\"Normal\">texte\n</text:p>"))
  (should (ox-linuxmag-fr-tests-contain "<text:p text:style-name=\"pragma\">/// Fin encadré ///</text:p>")))

(ert-deftest ox-linuxmag-fr-tests-src-block-default ()
  (ox-linuxmag-fr-tests-export "
#+begin_src text
foo ~text~
bar
#+end_src")
  (should (ox-linuxmag-fr-tests-contain "<text:p text:style-name=\"code\">foo <text:span text:style-name=\"code_5f_em\">text</text:span></text:p>"))
  (should (ox-linuxmag-fr-tests-contain "<text:p text:style-name=\"code\">bar</text:p>")))

(ert-deftest ox-linuxmag-fr-tests-src-block-console ()
  (ox-linuxmag-fr-tests-export "
#+ATTR_LINUXMAG-FR: :type console
#+begin_src text
bar
#+end_src")
  (should (ox-linuxmag-fr-tests-contain "<text:p text:style-name=\"console\">bar</text:p>")))

(ert-deftest ox-linuxmag-fr-tests-src-block-default-with-indentation ()
  (ox-linuxmag-fr-tests-export "
#+begin_src text
 not-indented
  indented-1
   indented-2
#+end_src")
  (should (ox-linuxmag-fr-tests-contain "<text:p text:style-name=\"code\">not-indented</text:p>"))
  (should (ox-linuxmag-fr-tests-contain "<text:p text:style-name=\"code\"> indented-1</text:p>"))
  (should (ox-linuxmag-fr-tests-contain "<text:p text:style-name=\"code\"> <text:s text:c=\"1\"/>indented-2</text:p>")))

(ert-deftest ox-linuxmag-fr-tests-src-block-preserves-white-space ()
  (ox-linuxmag-fr-tests-export "
#+begin_src text
a     b
#+end_src")
  (should (ox-linuxmag-fr-tests-contain "<text:p text:style-name=\"code\">a <text:s text:c=\"4\"/>b</text:p>")))

(ert-deftest ox-linuxmag-fr-tests-src-block-protects-xml-chars ()
  (ox-linuxmag-fr-tests-export "
#+begin_src text
 <damien@cassou.me>
#+end_src")
  (should (ox-linuxmag-fr-tests-contain "<text:p text:style-name=\"code\">&lt;damien@cassou.me&gt;</text:p>")))

(ert-deftest ox-linuxmag-fr-tests-src-block-not-converting-dash-dash ()
  (ox-linuxmag-fr-tests-export "
#+begin_src text
ls --size
#+end_src")
  (should (ox-linuxmag-fr-tests-contain "<text:p text:style-name=\"code\">ls --size</text:p>")))

(ert-deftest ox-linuxmag-fr-tests-table ()
  (ox-linuxmag-fr-tests-export "
| cell1.1 | cell1.2 |
| cell2.1 | cell2.2 |
")
  (should (ox-linuxmag-fr-tests-contain "<table:table table:style-name=\"Tableau1\">"))
  (should (ox-linuxmag-fr-tests-contain "<table:table-column table:style-name=\"Tableau1.A\" table:number-columns-repeated=\"2\"/>"))
  (should (ox-linuxmag-fr-tests-contain "<table:table-cell table:style-name=\"Tableau1.A1\" office:value-type=\"string\">"))
  (should (ox-linuxmag-fr-tests-contain "<text:p text:style-name=\"Normal\">cell1.1</text:p>"))
  (should (ox-linuxmag-fr-tests-contain "<text:p text:style-name=\"Normal\">cell1.2</text:p>"))
  (should (ox-linuxmag-fr-tests-contain "<text:p text:style-name=\"Normal\">cell2.1</text:p>"))
  (should (ox-linuxmag-fr-tests-contain "<text:p text:style-name=\"Normal\">cell2.2</text:p>"))
  (should (ox-linuxmag-fr-tests-contain "</table:table-cell>"))
  (should (ox-linuxmag-fr-tests-contain "</table:table>")))

(ert-deftest ox-linuxmag-fr-tests-table-empty-cell ()
  (ox-linuxmag-fr-tests-export "
| cell1.1 |  |
")
  (should (ox-linuxmag-fr-tests-contain "cell1.1"))
  (should (ox-linuxmag-fr-tests-contain "<table:table-cell table:style-name=\"Tableau1.A1\" office:value-type=\"string\"></table:table-cell>")))

(ert-deftest ox-linuxmag-fr-tests-target ()
  (ox-linuxmag-fr-tests-export "<<1>> foo")
  (should (ox-linuxmag-fr-tests-contain "<text:span text:style-name=\"gras\">[1]</text:span> foo")))

(ert-deftest ox-linuxmag-fr-tests-underline ()
  (ox-linuxmag-fr-tests-export "Click on _File_.")
  (should (ox-linuxmag-fr-tests-contain "Click on <text:span text:style-name=\"menu\">File</text:span>.")))

(provide 'ox-linuxmag-fr-tests)
;;; ox-linuxmag-fr-tests.el ends here
