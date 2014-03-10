;; -*- stumpwm -*-

(in-package #:stumpwm)

(defvar *keybindings-recording* '() "List of bindings to record/replay.")

(defun key-recorder-fn (key key-seq cmd)
  "Add the key to the bindings recording."
  (push key *keybindings-recording*))

(defcommand start-macros () ()
  "Start the key bindings recording."
  (if *keybindings-recording*
      (message "Already defining keyboard macro")
    (add-hook *key-press-hook* 'key-recorder-fn)))

(defcommand stop-macros () ()
  "Start the key bindings recording"
  (if *keybindings-recording*
      (progn
        ;; stop recording
        (remove-hook *key-press-hook* 'key-recorder-fn)
        ;; remove last call
        (pop *keybindings-recording*))
      (message "No macro defined...")))

(defcommand replay-macros () ()
  "Start the bindings recording"
  (message "Replay macros...")
  (if *keybindings-recording*
      (mapcar (lambda (key)
                (message "sending ~A" key)
                (send-fake-key (current-window) key)) *keybindings-recording*)
    (message "No macro defined...")))

;; (window-send-string "hello")
;; (send-fake-key (current-window) (kbd "h"))

(defvar *key-macro-start*  (kbd "(") "Binding to start the key bindings recording.")
(defvar *key-macro-stop*   (kbd ")") "Binding to stop the key bindings recording.")
(defvar *key-macro-replay* (kbd "M-e") "Binding to replay the key bindings recording")

;; (setf *key-macro-start*  (kbd "("))
;; (setf *key-macro-stop*   (kbd ")"))
;; (setf *key-macro-replay* (kbd "M-e"))

(define-key *root-map* *key-macro-start*  "start-macros")
(define-key *root-map* *key-macro-stop*   "stop-macros")
(define-key *root-map* *key-macro-replay* "replay-macros")
