;;; soql-mode.el --- Salesforce SOQL Major Mode   -*- lexical-binding: t -*-

;; Copyright (C) 2015-2018 Magnus Nyberg

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

(defvar soql-mode--kwds-regexp
  (eval-when-compile
    (list
     (regexp-opt
      '("SELECT" "FROM" "USING" "SCOPE" "WHERE" "WITH" "DATA" "CATEGORY"
        "GROUP" "BY" "ROLLUP" "CUBE" "HAVING"
        "ORDER" "ASC" "DESC" "NULLS" "FIRST" "LAST"
        "LIMIT"
        "OFFSET"
        "FOR" "VIEW" "REFERENCE" "UPDATE" "TRACKING" "VIEWSTAT"
        "null" "NULL" "toLabel" "TRUE" "true" "FALSE" "false"
        "LIKE" "IN" "NOT" "INCLUDES" "includes" "EXCLUDES" "excludes" "AND" "OR"
        "YESTERDAY" "TODAY" "TOMORROW"
        "LAST_WEEK" "THIS_WEEK" "NEXT_WEEK"
        "LAST_MONTH" "THIS_MONTH" "NEXT_MONTH"
        "LAST_90_DAYS" "NEXT_90_DAYS"
        "LAST_N_DAYS" "NEXT_N_DAYS"
        "NEXT_N_WEEKS" "LAST_N_WEEKS"
        "NEXT_N_MONTHS" "LAST_N_MONTHS"
        "THIS_QUARTER" "LAST_QUARTER" "NEXT_QUARTER"
        "NEXT_N_QUARTERS" "LAST_N_QUARTERS"
        "THIS_YEAR" "LAST_YEAR" "NEXT_YEAR"
        "NEXT_N_YEARS" "LAST_N_YEARS"
        "THIS_FISCAL_QUARTER" "LAST_FISCAL_QUARTER" "NEXT_FISCAL_QUARTER"
        "NEXT_N_FISCAL_QUARTERS" "LAST_N_FISCAL_QUARTERS"
        "THIS_FISCAL_YEAR" "LAST_FISCAL_YEAR" "NEXT_FISCAL_YEAR"
        "NEXT_N_FISCAL_YEARS" "LAST_N_FISCAL_YEARS"
        "AT" "ABOVE" "BELOW" "ABOVE_OR_BELOW"
        "FORMAT"
        "GROUPING"
        "AVG" "COUNT" "COUNT_DISTINCT" "MIN" "MAX" "SUM"
        "CALENDAR_MONTH" "CALENDAR_QUARTER" "CALENDAR_YEAR"
        "DAY_IN_MONTH" "DAY_IN_WEEK" "DAY_IN_YEAR"
        "DAY_ONLY" "FISCAL_MONTH" "FISCAL_QUARTER" "FISCAL_YEAR"
        "HOUR_IN_DAY" "WEEK_IN_MONTH" "WEEK_IN_YEAR"
        "convertTimezone"
        "convertCurrency"
        "DISTANCE" "GEOLOCATION")
      'words))))

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
  (setq font-lock-defaults '(soql-mode--kwds-regexp))
  (smie-setup soql-mode--grammar #'soql-mode--rules)
  ;; Dummy comment settings
  (setq comment-start "#")
  (setq comment-start-skip "\\`.\\`"))

(provide 'soql-mode)

;; Local Variables:
;; indent-tabs-mode: nil
;; End:

;;; soql-mode.el ends here
