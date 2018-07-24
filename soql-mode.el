;;; soql-mode.el --- Salesforce SOQL Major Mode   -*- lexical-binding: t -*-

;; Copyright (C) 2015 Magnus Nyberg

;; Author: Magnus Nyberg <magnus@nexter.se>
;; Keywords: languages, soql, apex, force, sfdc, salesforce
;; URL: https://github.com/nxtr/soql-mode

;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
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

;; This is a major mode for editing Salesforce Object Query Language
;; (SOQL) code.

;;; Code:

(defgroup salesforce nil
  "Support for Salesforce and force.com."
  :group 'emacs
  :group 'external)

(defgroup soql nil
  "Salesforce Object Query Language (SOQL)."
  :group 'languages
  :group 'salesforce
  :prefix "soql-")

(defcustom soql-mode-indent-basic 4
  "Basic amount of indentation."
  :type 'integer)

(defcustom soql-mode-hook nil
  "Hook called by `soql-mode'."
  :type 'hook)

(require 'smie)

(defconst soql-mode--grammar
  (smie-prec2->grammar
   (smie-bnf->prec2
    '((stmt ("SELECT" fields "FROM" types "USING" "SCOPE" scope)
            (stmt "WHERE" exp)
            (stmt "WITH" exp)
            (stmt "WITH" "DATA" "CATEGORY" exp)
            (stmt "GROUP" "BY" fields "HAVING" exp)
            (stmt "GROUP" "BY" "ROLLUP" fields "HAVING" exp)
            (stmt "GROUP" "BY" "CUBE" fields "HAVING" exp)
            (stmt "ORDER" "BY" fields)
            (stmt "LIMIT" num)
            (stmt "OFFSET" num)
            (stmt "FOR" "VIEW")
            (stmt "FOR" "REFERENCE")
            (stmt "UPDATE" "TRACKING")
            (stmt "UPDATE" "VIEWSTAT"))
      (field)
      (fields (field "," field))
      (type)
      (types (type "," type))
      (scope)
      (exp)
      (num))
    '((assoc ",")))))

(defun soql-mode--rules (kind token)
  (pcase (cons kind token)
    (`(:elem . basic) soql-mode-indent-basic)
    (`(,_ . ",") (smie-rule-separator kind))
    (`(:list-intro . ,(or `"WHERE" `"WITH" `"CATEGORY" `"HAVING")) t)))

(defvar soql-mode-syntax-table
  (let ((table (make-syntax-table)))
    (modify-syntax-entry ?\" "."  table)
    (modify-syntax-entry ?%  "."  table)
    (modify-syntax-entry ?\' "\"" table)
    (modify-syntax-entry ?+  "."  table)
    (modify-syntax-entry ?-  "."  table)
    (modify-syntax-entry ?<  "."  table)
    (modify-syntax-entry ?=  "."  table)
    (modify-syntax-entry ?>  "."  table)
    (modify-syntax-entry ?_  "w"  table)
    table))

;;;###autoload
(define-derived-mode soql-mode prog-mode "SOQL"
  "Major mode for editing Salesforce Object Query Language (SOQL) code."
  (smie-setup soql-mode--grammar #'soql-mode--rules)
  ;; Dummy comment settings
  (setq comment-start "#")
  (setq comment-start-skip "\\`.\\`"))

(provide 'soql-mode)

;; Local Variables:
;; indent-tabs-mode: nil
;; End:

;;; soql-mode.el ends here
