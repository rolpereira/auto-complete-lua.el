;;; auto-complete-lua-tests.el --- ERT tests for auto-complete-lua.el  -*- lexical-binding: t; -*-

;; Copyright (C) 2014  Rolando Pereira

;; Author: Rolando Pereira <rolando_pereira@sapo.pt>
;; Keywords: 

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

;; A test suite for auto-complete-lua.el package

;;; Code:

(require 'ert)
(require 'auto-complete-lua)


(ert-deftest auto-complete-lua-tests--no-repeated-functions ()
  "Check that there are no repeated functions in `auto-complete-lua--builtin-functions'."
  (let ((count-functions (make-hash-table :test #'equal))
         (not-unique-functions '())     ; List containing functions that appear more than once
         )
    (mapc (lambda (data)
            (let* ((function-name (car data))
                    (current-count (gethash function-name count-functions 0)))
              (puthash function-name (1+ current-count) count-functions)))
      auto-complete-lua--builtin-functions)
    (maphash (lambda (key value)        ; key = function-name and value = count
               (when (> value 1)
                 (push key not-unique-functions)))
      count-functions)
    (should (null not-unique-functions))))


(provide 'auto-complete-lua-tests)
;;; auto-complete-lua-tests.el ends here
