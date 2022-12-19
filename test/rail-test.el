;;; rail-utils-tests.el ---

;; Copyright © 2022 Fermin MF
;;
;; Author: Fermin MF <fmfs@posteo.net>

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
;; along with this program. If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;; Code:

(require 'ert)
(require 'rail)
(require 'rail-test-helper)


(ert-deftest test-sync-hello ()
  (unless (get-buffer rail-test-helper-buffer)
    (rail-test-helper-launch-server))
  (should (string= ""
		   (progn
		     (rail "localhost:7888")
		     (sit-for 0.3)
		     (with-current-buffer (get-buffer-create rail-test-helper-buffer)
		       (rail-send-sync-request `(("op" ."clone")))))))
  )
