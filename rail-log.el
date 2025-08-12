;;; rail-log.el --- Rail-logging -*- lexical-binding: t; -*-

(defgroup rail-log nil
  "Logging settings for Rail."
  :group 'rail)

;;; variables

(defconst rail-log-level-debug 0 "Debug level for detailed internal information.")
(defconst rail-log-level-info  1 "Info level for general operation information.")
(defconst rail-log-level-warn  2 "Warn level for potential issues.")
(defconst rail-log-level-error 3 "Error level for errors encountered.")
(defconst rail-log-level-none  4 "Disable logging.")

(defcustom rail-log-level rail-log-level-info
  "Set the logging level for Rail. Lower value means more verbose.
0: DEBUG, 1: INFO, 2: WARN, 3: ERROR, 4: NONE.
Default log level is 1: INFO."
  :type '(choice (const :tag "DEBUG" rail-log-level-debug)
                 (const :tag "INFO" rail-log-level-info)
                 (const :tag "WARN" rail-log-level-warn)
                 (const :tag "ERROR" rail-log-level-error)
                 (const :tag "NONE" rail-log-level-none))
  :group 'rail-log)

(defcustom rail-log-buffer-name "*rail-log*"
  "Name of the buffer to write Rail logs."
  :type 'string
  :group 'rail-log)

(defcustom rail-log-max-lines 10000
  "Maximum number of lines to keep in the log buffer.
Set to nil for no limit."
  :type '(choice (const   :tag "No Limit" nil)
                 (integer :tag "Max Lines"))
  :group 'rail-log)

;;; log functions

(defun rail--get-log-buffer ()
  "Get or create the Rail log buffer."
  (let ((log-buffer (get-buffer-create rail-log-buffer-name)))
    log-buffer))

(defun rail-log (level format-string &rest args)
  "Log a message if LEVEL is equal or higher than `rail-log-level`.
The message is formatted using FORMAT-STRING and ARGS, and inserted
into the buffer specified by `rail-log-buffer-name`, prefixed
with a timestamp and log level indicator."
  (when (<= rail-log-level level)
    (condition-case err
        (let ((log-buffer (rail--get-log-buffer)))
          (when (and log-buffer (buffer-live-p log-buffer))
            (with-current-buffer log-buffer
              (let ((buffer-read-only nil) ; Temporarily cancel read-only
                    (inhibit-read-only t))
                (goto-char (point-max))
                (insert (format-time-string "[%Y-%m-%d %H:%M:%S.%3N] ")
                        (format "[%s] " (cond
                                         ((= level rail-log-level-debug) "DEBUG")
                                         ((= level rail-log-level-info) "INFO")
                                         ((= level rail-log-level-warn) "WARN")
                                         ((= level rail-log-level-error "ERROR"))
                                         (t "UNKNOWN")))
                        ;; message body
                        (apply #'format format-string args)
                        "\n")
                ;; Buffer size limit (optional)
                (when rail-log-max-lines
                  (when (> (line-number-at-pos (point-max)) rail-log-max-lines)
                    (goto-char (point-min))
                    (forward-line (- (line-number-at-pos (point-max)) rail-log-max-lines))
                    (delete-region (point-min) (point))))))))
      ;; Errors in the log output are not gripped, but displayed as warnings
      (error (message "rail-log error: %s" err)))))

(defmacro rail-log-debug (format-string &rest args)
  "Log a message at DEBUG level."
  `(rail-log ,rail-log-level-debug ,format-string ,@args))

(defmacro rail-log-info (format-string &rest args)
  "Log a message at INFO level."
  `(rail-log ,rail-log-level-info ,format-string ,@args))

(defmacro rail-log-warn (format-string &rest args)
  "Log a message at WARN level."
  `(rail-log ,rail-log-level-warn ,format-string ,@args))

(defmacro rail-log-error (format-string &rest args)
  "Log a message at ERROR level."
  `(rail-log ,rail-log-level-error ,format-string ,@args))

(provide 'rail-log)
