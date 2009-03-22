;;; company-pabbrev.el --- 
;;
;; Copyright (C) 2009 Nikolaj Schumacher
;;
;; Author: Nikolaj Schumacher <bugs * nschum de>
;; Version: 
;; Keywords: 
;; URL: http://nschum.de/src/emacs/company-pabbrev/
;; Compatibility: GNU Emacs 22.x, GNU Emacs 23.x
;;
;; This file is NOT part of GNU Emacs.
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 2
;; of the License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.
;;
;;; Commentary:
;;
;;; Change Log:
;;
;;    Initial release.
;;
;;; Code:

;; (defun company-pabbrev (command &optional arg &rest ignored)
;;   (case command
;;     ('prefix (pabbrev-post-command-check-movement)
;;              (company-grab "\\<\\w+\\>"))
;;     ('candidates (mapcar 'car (pabbrev-fetch-all-suggestions-for-prefix arg)))))




;; ('post-command )
;; ('sorted 'non-alphabetically)))

;; (defun company-pabbrev (command &optional arg &rest ignored)
;;   (case command
;;     ('prefix (company-grab "\\<\\w+\\>"))
;;     ('candidates (sort (pabbrev-fetch-all-suggestions-for-prefix arg)
;;                        (lambda (a b) (< (cdr a) (cdr b)))))
;;     ('post-command (pabbrev-post-command-check-movement))
;;     ('sorted 'non-alphabetically)))



(provide 'company-pabbrev)
;;; company-pabbrev.el ends here
