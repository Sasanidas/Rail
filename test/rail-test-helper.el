;;; rail-test-helper.el --- Test helper for test suites

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
;;

;;; Code:
(require 'cl-lib)
(require 'vc-git)

(defvar rail-test-helper-buffer "*rail-test-server*")

(cl-defmacro rail-test-helper-request-wrapper (type &body body)
  `(progn
     (rail-test-helper-launch-server ,type)
     (let ((count 0) (max 20))
       (while (not (condition-case nil
		       (rail "localhost:7888")
		     (error nil)))
	 (if (= count max)
	     (error "Maximum attempt limit reached"))
	 (sit-for 0.5)
	 (setf count (+ count 1))))

     ,@body
     (rail-test-helper-shutdown-server)))

(cl-defmethod rail-test-helper-launch-server ((type (eql :python)))
  (let* ((name "python-nrepl")
	 (url (format "https://gitlab.com/sasanidas/%s.git" name)))
    
    (message "Downloading %s..." name)
    (unless (directory-empty-p name)
      (vc-git-clone url name nil))

    (shell-command (format "cd %s && poetry install" name))
    (message "Starting NREPL python-server ...")
    (start-process-shell-command "nrepl-python-server"
				 (get-buffer-create rail-test-helper-buffer)
				 (format "cd %s && make debug" name))))
(cl-defmethod rail-test-helper-launch-server ((type (eql :lein)))
  (let* ((path (or
		(and (file-directory-p "dummy_clojure")
		     "dummy_clojure")
		"test/dummy_clojure")))

    (message "Starting NREPL lein ...")
    (start-process-shell-command "nrepl-lein-server"
				 (get-buffer-create rail-test-helper-buffer)
				 (format "cd %s && lein trampoline repl :headless :host 127.0.0.1 :port 7888" path))))
(defun rail-test-helper-shutdown-server ()
  (ignore-errors
    (kill-process (get-buffer-process rail-test-helper-buffer))
    (with-current-buffer rail-test-helper-buffer
      (setq-local kill-buffer-query-functions
		  (delq 'process-kill-buffer-query-function kill-buffer-query-functions))
      (set-buffer-modified-p nil)
      (kill-buffer rail-test-helper-buffer))))

(defun rail-test-helper-debug-info ()
  (with-current-buffer rail-test-helper-buffer
    (buffer-substring-no-properties (point-min) (point-max))))


(provide 'rail-test-helper)

;;; rail-test-helper.el ends here
