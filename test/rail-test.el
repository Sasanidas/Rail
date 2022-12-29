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
(require 'cl-lib)

(require 'rail)
(require 'rail-test-helper)


(cl-defmethod rail-test-describe ((type (eql :python)))
  (with-current-buffer (get-buffer-create (concat "*rail: " "localhost:7888" "*"))
    (sit-for 0.5)
    (let ((describe (rail-send-sync-request '(("op" . "describe")))))
      (should (string= "done" (car (cl-getf describe :status ))))
      (should (equal '("clone" "describe" "eval" "complete" "ls-sessions" "load-file")
		     (cl-getf (cl-getf describe :server-capabilities)
			      :ops))))))

(cl-defmethod rail-test-describe ((type (eql :lein)))
  (with-current-buffer (get-buffer-create (concat "*rail: " "localhost:7888" "*"))
    (sit-for 0.5)
    (let ((describe (rail-send-sync-request '(("op" . "describe")))))
      (should (string= "done" (car (cl-getf describe :status ))))
      (should (equal '(:clone :close
			      :describe :eval
			      :interrupt :load-file
			      :ls-sessions :stdin)
		     (delete nil
			     (cl-getf (cddr describe) :ops)))))))

(cl-defmethod rail-test-eval ((type (eql :python)))
  (with-current-buffer (get-buffer-create (concat "*rail: " "localhost:7888" "*"))
    (sit-for 0.5)
    (let ((sum (rail-send-sync-request `(("op" . "eval")
					 ("session" . ,(rail-current-session))
					 ("code" . "3")
					 ("ns" . "user"))))
	  (cgi-name (and (rail-send-sync-request `(("op" . "eval")
						   ("session" . ,(rail-current-session))
						   ("code" . "import cgi")))
			 (rail-send-sync-request `(("op" . "eval")
						   ("session" . ,(rail-current-session))
						   ("code" . "cgi.__name__"))))))

      (should (string= "3" (cl-getf sum :value)))
      (should (string= "cgi" (cl-getf cgi-name :value))))))

(cl-defmethod rail-test-eval ((type (eql :lein)))
  (with-current-buffer (get-buffer-create (concat "*rail: " "localhost:7888" "*"))
    (sit-for 0.5)
    (let ((sum (rail-send-sync-request `(("op" . "eval")
					 ("session" . ,(rail-current-session))
					 ("code" . "3")
					 ("ns" . "user")))))
      (should (string= "3" (cl-getf sum :value ))))))

(ert-deftest test-python-sync-describe ()
  (rail-test-helper-request-wrapper
   :python (rail-test-describe :python)))

(ert-deftest test-lein-sync-describe ()
  (rail-test-helper-request-wrapper
   :lein (rail-test-describe :lein)))

(ert-deftest test-python-sync-eval ()
  (rail-test-helper-request-wrapper
   :python (rail-test-eval :python)))

(ert-deftest test-lein-sync-eval ()
  (rail-test-helper-request-wrapper
   :lein (rail-test-eval :lein)))




;; (cl-getf '(:status ("done")
;; 		   :time-stamp "2022-12-19 12:19:48.979450" :server-capabilities
;; 		   (:ops
;; 		    ("clone" "describe" "eval" "complete")
;; 		    :ns
;; 		    ("user"))
;; 		   :id "1" "started" "2022-12-19 12:19:48.978601")
;; 	 :status
;; 	 )

;; (equal '("clone" "describe" "eval" "complete") '("clone" "describe" "eval" "complete"))
