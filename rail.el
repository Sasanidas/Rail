;;; rail.el --- Yet another client for nREPL -*- indent-tabs-mode: nil -*-

;; Copyright (c) 2014-2018 Sanel Zukan
;; Copyright (c) 2022 Fermin MF

;; Author: Sanel Zukan <sanelz@gmail.com>

;; Maintainer: Fermin MF
;; URL: https://github.com/Sasanidas/Rail
;; Version: 0.1.0
;; Keywords: languages, nrepl, lisp

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Provides another elisp client to connect to nREPL servers.

;;; Installation:

;; Copy it to your load-path and run with:
;; M-: (require 'rail)

;;; Usage:

;; M-x rail

;;; Code:

(require 'comint)
(require 'cl-lib)
(require 'subr-x)
(require 'rail-bencode)

(defgroup rail nil
  "Interaction with the nREPL Server."
  :prefix "rail-"
  :group 'applications)

(defcustom rail-repl-prompt-format "%s=> "
  "String used for displaying prompt.
'%s' is used as placeholder for storing current namespace."
  :type 'string
  :group 'rail)

(defcustom rail-prompt-regexp "^[^> \n]*>+:? *"
  "Regexp to recognize prompts in Rail more.
The same regexp is used in `inferior-lisp'."
  :type 'regexp
  :group 'rail)

(defcustom rail-default-host "localhost:7888"
  "Default location to connect.
Unless explicitly given location and port.
Location and port should be delimited with ':'."
  :type 'string
  :group 'rail)

(defcustom rail-detail-stacktraces nil
  "If set to true, Rail will try to get full stacktrace from thrown exception.
Otherwise will just behave as standard REPL version."
  :type 'boolean
  :group 'rail)

(define-obsolete-variable-alias
  'rail-old-style-stacktraces
  'rail-print-stack-trace-function
  "0.4.0")

(defcustom rail-print-stack-trace-function nil
  "Set to a clojure-side function in order to override stack-trace printing.

Will be called upon error when `rail-detail-stacktraces' is non-nil.

e.g. clojure.stacktrace/print-stack-trace for old-style stack traces."
  :type 'symbol
  :group 'rail)

(defvar rail-version "0.1.0"
  "The current rail version.")

(defvar rail-session nil
  "Current nREPL session id.")

(defvar rail-requests (make-hash-table :test 'equal)
  "Map of requests to be processed.")

(defvar rail-process nil
  "Current NREPL process.")

(defvar rail-requests-counter 0
  "Serial number for message.")

(defvar rail-nrepl-sync-timeout 5
  "Number of seconds to wait for a sync response.")

(defvar rail-custom-handlers (make-hash-table :test 'equal)
  "Map of handlers for custom ops.")

(defvar rail-buffer-ns "user"
  "Current clojure namespace for this buffer.
This namespace is only advertised until first expression is
evaluated, then is updated to the one used on nrepl side.")

(defvar rail-nrepl-server-cmd "lein"
  "Command to start nrepl server.
Defaults to Leiningen")

(defvar rail-nrepl-server-cmd-args "trampoline repl :headless"
  "Arguments to pass to the nrepl command.
Defaults to: trampoline repl :headless")

(defvar rail-nrepl-server-buffer-name "rail nrepl server")

(defvar rail-nrepl-server-project-file "project.clj")

(make-variable-buffer-local 'rail-session)
(make-variable-buffer-local 'rail-requests)
(make-variable-buffer-local 'rail-requests-counter)
(make-variable-buffer-local 'rail-buffer-ns)

;;; message stuff

;; Idea for message handling (via callbacks) and destructuring response is shamelessly
;; stolen from nrepl.el.
(defmacro rail-dbind-response (response keys &rest body)
  "Destructure an nREPL RESPONSE dict.
Bind the value of the provided KEYS and execute BODY."
  `(let ,(cl-loop for key in keys
                  collect `(,key (plist-get ,response ,(intern (format ":%s" key)))))
     ,@body))

(defun rail-send-request (request callback)
  "Send REQUEST and assign CALLBACK.
The CALLBACK function will be called when reply is received."
  (let* ((id       (number-to-string (cl-incf rail-requests-counter)))
         (hash (make-hash-table :test 'equal)))
    (puthash "id" id hash)
    (cl-loop for (key . value) in request
             do (puthash key value hash))

    (puthash id callback rail-requests)
    (process-send-string (rail-connection) (rail-bencode-encode hash))))

(defun rail-send-sync-request (request)
  "Send request to nREPL server synchronously."
  (let ((time0 (current-time))
        response
        global-status)
    (rail-send-request request (lambda (resp) (setq response resp)))
    (while (not (member "done" global-status))
      (rail-dbind-response response (status)
        (setq global-status status))
      (when (time-less-p rail-nrepl-sync-timeout
                         (time-subtract nil time0))
        (error "Sync nREPL request timed out %s" request))
      (accept-process-output nil 0.01))
    (rail-dbind-response response (id status)
      (when id
        (remhash id rail-requests)))
    response))

(defun rail-clear-request-table ()
  "Erases current request table."
  (clrhash rail-requests)
  (setq rail-requests-counter 0))

(defun rail-current-session ()
  "Returns current session id."
  (with-current-buffer (process-buffer (rail-connection)) rail-session))

;;; nrepl messages we knows about

(defun rail-send-hello (callback)
  "Initiate nREPL session."
  (rail-send-request '(("op" ."clone")) callback))

(defun rail-send-describe (callback)
  "Produce a machine- and human-readable directory and documentation for
the operations supported by an nREPL endpoint."
  (rail-send-request '(("op" . "describe")) callback))

(defun rail-send-load-file (file-name file-content callback)
  "Produce a machine- and human-readable directory and documentation for
the operations supported by an nREPL endpoint."
  (rail-send-request `(("op" . "load-file")
                   ("file" . ,file-content)
                   ("file-name" . ,file-name))
                 callback))

(defun rail-send-ls-sessions (callback)
  "Get a list of all the sessions currently running in the server."
  (rail-send-request '(("op" . "ls-sessions")) callback))

(cl-defun rail-send-eval-string (str callback &optional ns)
  "Send code for evaluation on given namespace."
  (let ((request `(("op" . "eval")
                   ("session" . ,(rail-current-session))
                   ("code" . ,(substring-no-properties str)))))
    (when ns
      (setf request (append request
                            `(("ns" . ,ns)))))

    (rail-send-request request callback)))

(defun rail-send-stdin (str callback)
  "Send stdin value."
  (rail-send-request
   `(("op" . "stdin")
     ("session" . ,(rail-current-session))
     ("stdin" . ,(substring-no-properties str)))
   callback))

(defun rail-send-interrupt (request-id callback)
  "Send interrupt for pending requests."
  (rail-send-request
   `(("op" . "interrupt")
     ("session" . ,(rail-current-session))
     ("interrupt-id" . ,request-id))
   callback))

;;; code

(defun rail-make-response-handler ()
  "Returns a function that will be called when event is received."
  (lambda (response)
    (rail-dbind-response response (id ns value err out ex root-ex status)
                     (let ((output (concat err out
                                           (if value
                                               (concat value "\n"))))
                           (process (get-buffer-process (rail-repl-buffer))))
                       ;; update namespace if needed
                       (if ns (setq rail-buffer-ns ns))
                       (comint-output-filter process output)
                       ;; now handle status
                       (when status
                         (when (and rail-detail-stacktraces (member "eval-error" status))
                           (rail-get-stacktrace))
                         (when (member "eval-error" status)
                           (message root-ex))
                         (when (member "interrupted" status)
                           (message "Evaluation interrupted."))
                         (when (member "need-input" status)
                           (rail-handle-input))
                         (when (member "done" status)
                           (remhash id rail-requests)))
                       ;; show prompt only when no messages are pending
                       (when (hash-table-empty-p rail-requests)
                         (comint-output-filter process (format rail-repl-prompt-format rail-buffer-ns)))))))

(defun rail-input-sender (proc input &optional ns)
  "Called when user enter data in REPL and when something is received in."
  (rail-send-eval-string input (rail-make-response-handler) ns))

(defun rail-handle-input ()
  "Called when requested user input."
  (rail-send-stdin
   (concat (read-from-minibuffer "Stdin: ") "\n")
   (rail-make-response-handler)))

(defun rail-sentinel (process message)
  "Called when connection is changed; in out case dropped."
  (message "nREPL connection closed: %s" message)
  (kill-buffer (process-buffer process))
  (rail-disconnect))

(defun rail-dispatch (msg)
  "Find associated callback for a message by id or by op."
  (rail-dbind-response msg (id op)
    (let ((callback (or (gethash id rail-requests)
                        (gethash op rail-custom-handlers))))
      (when callback
        (funcall callback msg)))))

(defun rail-net-decode ()
  "Decode the data in the current buffer and remove the processed data from the
buffer if the decode successful."
  (let* ((start   (point-min))
         (end     (point-max))
         (data    (buffer-substring-no-properties
                   start end))
         (decoded (rail-bencode-decode data)))
    (delete-region start end)
    decoded))

(defun rail-net-filter (process string)
  "Called when the new message is received. Process will redirect
all received output to this function; it will decode it and put in
rail-repl-buffer."
  (with-current-buffer (process-buffer process)
    (goto-char (point-max))
    (insert string)
    ;; Stolen from Cider. Assure we have end of the message so decoding can work;
    ;; to make sure we are at the real end (session id can contain 'e' character), we call
    ;; 'accept-process-output' once more.
    ;;
    ;; This 'ignore-errors' is a hard hack here since 'accept-process-output' will call filter
    ;; which will be this function causing Emacs to hit max stack size limit.
    (ignore-errors
      (when (eq ?e (aref string (- (length string) 1)))
        (unless (accept-process-output process 0.01)
          (while (> (buffer-size) 1)
            (rail-dispatch (rail-net-decode))))))))

(defun rail-new-session-handler (process)
  "Returns callback that is called when new connection is established."
  (lambda (response)
    (rail-dbind-response response (id new-session)
                     (when new-session
                       (message "Connected.")
                       (setq rail-session new-session)
                       (remhash id rail-requests)))))

(defun rail-valid-host-string (str default)
  "Used for getting valid string for host/port part."
  (if (and str (not (string= "" str)))
    str
    default))

(defun rail-locate-port-file ()
  (locate-dominating-file default-directory ".nrepl-port"))

(defun rail-locate-running-nrepl-host ()
  "Return host of running nREPL server."
  (let ((dir (rail-locate-port-file)))
    (when dir
      (with-temp-buffer
        (insert-file-contents (concat dir ".nrepl-port"))
        (concat "localhost:" (buffer-string))))))

(defun rail-extract-host (buff-name)
  "Take host from rail buffers."
  (car (last (split-string (substring buff-name 1 -1) " "))))

(defun rail-repl-buffer ()
  "Returns right rail buffer."
  (or (get-buffer (format "*rail: %s*" (rail-locate-running-nrepl-host)))
      (get-buffer
       (format "*rail: %s*"
               (rail-extract-host (buffer-name (current-buffer)))))))

(defun rail-connection ()
  "Returns right rail connection."
  (or (get-process (concat "rail/" (rail-locate-running-nrepl-host)))
      (get-process
       (concat "rail/"
               (rail-extract-host (buffer-name (current-buffer)))))))

(defun rail-strip-protocol (host)
  "Check if protocol was given and strip it."
  (let ((host (replace-regexp-in-string "[ \t]" "" host)))
    (if (string-match "^nrepl://" host)
        (substring host 8)
      host)))

(cl-defun rail-valid-connection-p (host-and-port)
  "Validate that HOST-AND-PORT are valid for a connection."
  (condition-case nil
      (cl-destructuring-bind (host port)
          (split-string (rail-strip-protocol host-and-port) ":")
        (and (not (string= "" host))
             (not (string= "" port))
             (cons host (string-to-number port))))
    (error nil)))


;;(when (get-buffer name) (rail-disconnect))

(cl-defun rail-connect (&key (host-and-port rail-default-host))
  "Connect to remote endpoint using provided hostname and port."
  (let* ((name (concat "*rail-connection: " host-and-port "*"))
         (valid-connection (rail-valid-connection-p host-and-port))
         process)
    (if valid-connection
        (cl-destructuring-bind (host . port ) valid-connection
          (message "Connecting to nREPL host on '%s:%d'..." host port)
          (setf process (open-network-stream
                         (concat "rail/" host-and-port) name host port))

          (set-process-filter process 'rail-net-filter)
          (set-process-sentinel process 'rail-sentinel)
          (set-process-coding-system process 'utf-8-unix 'utf-8-unix)

          (rail-send-hello (rail-new-session-handler (process-buffer process)))
          process)
      (error "Connection with %s not possible" host-and-port))))

(defun rail-disconnect ()
  "Disconnect from current nrepl connection. Calling this function directly
will force connection closing, which will as result call '(rail-sentinel)'."
  (rail-clear-request-table)
  (let ((delete-process-safe (lambda (p)
                               (when (and p (process-live-p p))
                                 (delete-process p))))
        (proc1 (get-buffer-process (rail-repl-buffer)))
        (proc2 (rail-connection)))
    (funcall delete-process-safe proc1)
    (funcall delete-process-safe proc2)))

;;; keys

(defun rail-eval-region (start end &optional ns)
  "Evaluate selected region."
  (interactive "r")
  (rail-input-sender
   (get-buffer-process (rail-repl-buffer))
   (buffer-substring-no-properties start end)
   ns))

(defun rail-eval-buffer ()
  "Evaluate the buffer."
  (interactive)
  (rail-eval-region (point-min) (point-max)))

(defun rail-eval-defun ()
  "Figure out top-level expression and send it to evaluation."
  (interactive)
  (save-excursion
    (end-of-defun)
    (let ((end (point)))
      (beginning-of-defun)
      (rail-eval-region (point) end
                    (substring-no-properties (rail-get-clojure-ns))))))

(defun rail-eval-expression-at-point ()
  "Figure out expression at point and send it for evaluation."
  (interactive)
  (save-excursion
    (let ((end (point)))
      (backward-sexp)
      (rail-eval-region (point) end))))

(defun rail-eval-namespace ()
  "Tries to evaluate Clojure ns form. It does this by matching first
expression at the beginning of the file and evaluating it. Not something
that is 100% accurate, but Clojure practice is to keep ns forms always
at the top of the file."
  (interactive)
  (when (rail-get-clojure-ns)
    (save-excursion
      (goto-char (match-beginning 0))
      (rail-eval-defun))))

(defun rail-eval-doc (symbol)
  "Internal function to actually ask for symbol documentation via nrepl protocol."
  (rail-input-sender
   (get-buffer-process (rail-repl-buffer))
   (format "(do (require 'clojure.repl) (clojure.repl/doc %s))" symbol)))

(defvar rail-translate-path-function 'identity
  "This function is called on all paths returned by `rail-jump'.
You can use it to translate paths if you are running an nrepl server remotely or
inside a container.")

(defun rail-jump-find-file (file)
  "Internal function to find a file on the disk or inside a jar."
  (if (not (string-match "^jar:file:\\(.+\\)!\\(.+\\)" file))
      (find-file (substring file 5))
    (let* ((jar (match-string 1 file))
           (clj (match-string 2 file))
           (already-open (get-buffer (file-name-nondirectory jar))))
      (find-file jar)
      (goto-char (point-min))
      (search-forward-regexp (concat " " (substring clj 1) "$"))
      (let ((archive-buffer (current-buffer)))
        (declare-function archive-extract "arc-mode")
        (archive-extract)
        (when (not already-open)
          (kill-buffer archive-buffer))))))

(defun rail-eval-jump (ns var)
  "Internal function to actually ask for var location via nrepl protocol."
  (rail-send-request
   `(("op" . "lookup")
     ("sym" . ,(substring-no-properties var))
     ("ns" . ,ns))
   (lambda (response)
     (rail-dbind-response response (id info status)
                      (when (member "done" status)
                        (remhash id rail-requests))
                      (when info
                        (rail-dbind-response info (file line)
                                         (rail-jump-find-file (funcall rail-translate-path-function file))
                                         (when line
                                           (goto-char (point-min))
                                           (forward-line (1- line)))))))))

(defun rail-completion-at-point ()
  "Function to be used for the hook `completion-at-point-functions'."
  (interactive)
  (let* ((bnds (bounds-of-thing-at-point 'symbol))
         (start (car bnds))
         (end (cdr bnds))
         (ns (or (rail-get-clojure-ns) rail-buffer-ns))
         (sym (or (thing-at-point 'symbol t) ""))
         (response (rail-send-sync-request
                    `(("op" . "completions")
                      ("ns" . ,ns)
                      ("prefix" . ,sym)))))
    (rail-dbind-response response (completions)
                     (when completions
                       (list start end
                             (cl-loop for pcandidate in completions
                                      collect
                                      (string-trim
                                       (plist-get pcandidate :candidate)))
                             nil)))))

(defun rail-get-stacktrace ()
  "When error happens, print the stack trace"
  (let ((pst rail-print-stack-trace-function))
    (rail-send-eval-string
     (format "(do (require (symbol (namespace '%s))) (%s *e))" pst pst)
     (rail-make-response-handler))))

(defun rail-get-clojure-ns ()
  "If available, get the correct clojure namespace."
  (and (eq major-mode 'clojure-mode)
       (fboundp 'clojure-find-ns)
       (funcall 'clojure-find-ns)))

(defun rail-get-directory ()
  "Internal function to get project directory."
  (locate-dominating-file default-directory rail-nrepl-server-project-file))

(defun rail-describe (symbol)
  "Ask user about symbol and show symbol documentation if found."
  (interactive
   (list
    (let* ((sym (thing-at-point 'symbol))
           (sym (if sym (substring-no-properties sym)))
           (prompt "Describe")
           (prompt (if sym
                     (format "%s (default %s): " prompt sym)
                     (concat prompt ": "))))
      (read-string prompt nil nil sym))))
  (rail-eval-doc symbol))

(defun rail-load-file (path)
  ""
  (interactive "f")
  (let* ((buffer (find-file-noselect path))
         (file-content (with-current-buffer buffer
                         (buffer-substring-no-properties
                          (point-min) (point-max)))))
    (rail-send-load-file
     (buffer-file-name buffer)
     file-content
     (lambda (response)
       (rail-dbind-response response (id ex root-ex status)
                        (let ((process (get-buffer-process (rail-repl-buffer))))
                          ;; now handle status
                          (when (member "done" status)
                            (remhash id rail-requests))))
       (message "File loaded!")))))

(defun rail-jump (var)
  "Jump to definition of var at point."
  (interactive
   (list (if (thing-at-point 'symbol)
             (thing-at-point 'symbol t)
           (read-string "Find var: "))))
  (defvar find-tag-marker-ring) ;; etags.el
  (require 'etags)
  (ring-insert find-tag-marker-ring (point-marker))
  (rail-eval-jump (rail-get-clojure-ns) var))

(defun rail-jump-pop ()
  "Return point to the position and buffer before running `rail-jump'."
  (interactive)
  (defvar find-tag-marker-ring) ;; etags.el
  (require 'etags)
  (let ((marker (ring-remove find-tag-marker-ring 0)))
    (switch-to-buffer (marker-buffer marker))
    (goto-char (marker-position marker))))

(defun rail-switch-to-repl ()
  (interactive)
  (pop-to-buffer (rail-repl-buffer)))

(defun rail-nrepl-server-start ()
  "Starts nrepl server. Uses rail-nrepl-server-cmd +
rail-nrepl-server-cmd-args as the command. Finds project root
by locatin rail-nrepl-server-project-file"
  (interactive)
  (let* ((nrepl-buf-name (concat "*" rail-nrepl-server-buffer-name "*"))
         (repl-started-dir (rail-locate-port-file)))
    (if repl-started-dir
        (message "nREPL server already running in %s" repl-started-dir)
      (progn
        (message "Starting nREPL server in %s" (rail-get-directory))
        (async-shell-command (concat rail-nrepl-server-cmd " " rail-nrepl-server-cmd-args)
                             nrepl-buf-name)))))

(defun rail-interrupt ()
  "Send interrupt to all pending requests."
  (interactive)
  (cl-loop for id being the hash-key of rail-requests
           do (rail-send-interrupt id (rail-make-response-handler))))

;; keys for interacting with Rail REPL buffer
(defvar rail-interaction-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "\C-c\C-c" 'rail-eval-defun)
    (define-key map "\C-c\C-e" 'rail-eval-expression-at-point)
    (define-key map "\C-c\C-r" 'rail-eval-region)
    (define-key map "\C-c\C-k" 'rail-eval-buffer)
    (define-key map "\C-c\C-n" 'rail-eval-namespace)
    (define-key map "\C-c\C-d" 'rail-describe)
    (define-key map "\C-c\C-b" 'rail-interrupt)
    (define-key map "\C-c\C-l" 'rail-load-file)
    (define-key map "\M-."     'rail-jump)
    (define-key map "\M-,"     'rail-jump-pop)
    (define-key map "\C-c\C-z" 'rail-switch-to-repl)
    map))

;; keys for interacting inside Rail REPL buffer
(defvar rail-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map comint-mode-map)
    (define-key map "\C-c\C-d" 'rail-describe)
    (define-key map "\C-c\C-c" 'rail-interrupt)
    (define-key map "\M-."     'rail-jump)
    map))

;;; rest

(define-derived-mode rail-mode comint-mode "Rail nREPL"
  "Major mode for evaluating commands over nREPL.

The following keys are available in `rail-mode':

  \\{rail-mode-map}"

  :syntax-table lisp-mode-syntax-table
  (setq-local comint-prompt-regexp rail-prompt-regexp
              comint-input-sender 'rail-input-sender
              comint-prompt-read-only t
              mode-line-process '(":%s"))
  (add-hook 'completion-at-point-functions #'rail-completion-at-point nil t)

  ;; a hack to keep comint happy
  (unless (comint-check-proc (current-buffer))
    (let ((fake-proc (start-process "rail" (current-buffer) nil)))
      (set-process-query-on-exit-flag fake-proc nil)
      (insert (format ";; Rail nREPL %s\n" rail-version))
      (set-marker (process-mark fake-proc) (point))
      (comint-output-filter fake-proc (format rail-repl-prompt-format rail-buffer-ns)))))

;;; user command

;;;###autoload
(define-minor-mode rail-interaction-mode
  "Minor mode for Rail interaction from a buffer.

The following keys are available in `rail-interaction-mode`:

  \\{rail-interaction-mode}"

  :init-value nil :lighter " Rail" :keymap rail-interaction-mode-map)

;;;###autoload
(defun rail (host-and-port)
  "Load rail by setting up appropriate mode, asking user for
connection endpoint."
  (interactive
   (let ((host (or (rail-locate-running-nrepl-host) rail-default-host)))
     (list
      (read-string (format "Host (default '%s'): " host)
                   nil nil host))))

  (unless
      (with-current-buffer
          (get-buffer-create (concat "*rail: " host-and-port "*"))
        (when-let ((connection (rail-connect :host-and-port host-and-port)))
          (setf rail-process connection)
          (goto-char (point-max))
          (rail-mode)
          (when (called-interactively-p 'interactive)
            (pop-to-buffer (current-buffer)))))

    (message "Unable to connect to %s" host-and-port)))
(provide 'rail)

;;; rail.el ends here
