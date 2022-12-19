(let* ((project-files-list '("rail-test-helper.el"
			     "rail-test.el"))
       (current-directory (file-name-directory load-file-name))
       (project-test-path (expand-file-name "." current-directory))
       (project-root-path (expand-file-name ".." current-directory)))

  ;; add the package being tested to 'load-path so it can be 'require-d
  (add-to-list 'load-path project-root-path)
  (add-to-list 'load-path project-test-path)

  ;; load the files with tests
  (mapc (lambda (test-file)
	  (load (expand-file-name test-file project-test-path) nil t))
	project-files-list))

(ert-run-tests-batch-and-exit)
