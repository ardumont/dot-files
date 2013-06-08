(in-package :stumpwm)

;; ========================== Functions

(defun shell-command (command)
  "Run a shell command and display output to screen.
   This must be used in a functional side-effects-free style!
   If a program does not exit, Stumpwm might hang!"
  (check-type command string)
  (echo-string (current-screen) (run-shell-command command t)))

(defun run-shl (&rest cmd)
  "A simpler command to run-shell-command with multiple params"
  (run-shell-command (apply #'concatenate 'string cmd)))

(defun toggle-touchpad-manual (status)
  "Activate/Deactivate the touchpad depending on the status parameter (0/1)."
  (run-shl "~/bin/stumpwm/toggle-touchpad-manual.sh " status)
  ;; banish the mouse from the screen when we deactivate the touchpad
  (if (equal "0" status) (banish)))

(defmacro roraise-and-touchpad-off (command class)
  "Wrap a call to the run-or-raise command but before toggle off the touchpad (works with window class names)."
  `(progn
     (toggle-touchpad-manual "0")
     (run-or-raise ,command '(:class ,class))))

(defmacro roraise-and-touchpad-on (command class)
  "Wrap a call to the run-or-raise command but before toggle on the touchpad (works with window class name)."
  `(progn
     (toggle-touchpad-manual "1")
     (run-or-raise ,command '(:class ,class))))

(defmacro roraise-and-touchpad-on-instance (command instance)
  "Wrap a call to the run-or-raise command but before toggle off the touchpad - for the small software that deals better with the stumpwm instance property."
  `(progn
     (toggle-touchpad-manual "1")
     (run-or-raise ,command '(:instance ,instance))))

;; wrapper around the zenity cli
(defcommand zenity (command) ()
   "Wrap a call to the cli and output the result into a zenity window"
   (run-shl "zenity --info --text \"$(" command ")\""))

;; C-t T - Toggle on/off the touchpad
(defcommand toggle-touchpad-auto () ()
  "Activate/Deactivate automatically the touchpad depending on the current status."
  (run-shl "~/bin/stumpwm/toggle-touchpad.sh")
  (banish))
(define-key *root-map* (kbd "T") "toggle-touchpad-auto")

;; ========================== macro definition

(defmacro mapcro (macro args)
  "A macro is not a first class citizen, thus map over macro is not possible.
   Here is a macro that does such job.
   Iterate over a list of list of parameters and executes the macro 'macro' on such args."
  `(progn
     ,@(mapcar
        (lambda (&rest x) `(,macro ,@x)) args)))

;; zenity - shell command with result inside a pop-up window
(defmacro zenity-cmd (metacmd)
  (destructuring-bind (fn-name doc-string command binding) metacmd
    `(progn
       (defcommand ,fn-name () ()
         ,doc-string
         (zenity ,command))
       (define-key *root-map* (kbd ,binding) (string-downcase (symbol-name ',fn-name))))))

;; (macroexpand-1
;;  '(zenity-cmd (sshadd "list ssh identities held by agent" "ssh-add -L" "q")))

;; (macroexpand-1
;;  '(mapcro
;;    zenity-cmd
;;    ((ssh-add-list          "List the keys the user-agent holds"      "ssh-add -L"           "q")
;;     (cat-etc-environment10 "Display the content of /etc/environment" "cat /etc/environment" "E")
;;     (cat-etc-hosts10       "Display the content of /etc/hosts"       "cat /etc/hosts"       "H")
;;     (sbin-ifconfig10       "/sbin/ifconfig"                          "/sbin/ifconfig"       "I")
;;     (acpi-cmd10            "Display battery"                         "/usr/bin/acpi -b"     "B"))))

(defmacro zenity-category (data)
  `(mapcro zenity-cmd ,data))

;; ========================== run-or-raise-touchpad-off

(defmacro ror-touchpad-off-cmd (metacmd)
  (destructuring-bind (fn-name doc-string command class binding) metacmd
    `(progn
       (defcommand ,fn-name () ()
         ,doc-string
         (roraise-and-touchpad-off ,command ,class))
       (define-key *root-map* (kbd ,binding) (string-downcase (symbol-name ',fn-name))))))

(defmacro ror-tpd-off-category (data)
  `(mapcro ror-touchpad-off-cmd ,data))

;; ========================== run-or-raise-touchpad-off

(defmacro ror-touchpad-on-cmd (metacmd)
  (destructuring-bind (fn-name doc-string command class binding) metacmd
    `(progn
       (defcommand ,fn-name () ()
         ,doc-string
         (roraise-and-touchpad-on ,command ,class))
       (define-key *root-map* (kbd ,binding) (string-downcase (symbol-name ',fn-name))))))

(defmacro ror-tpd-on-category (data)
  `(mapcro ror-touchpad-on-cmd ,data))

;; ========================== run-or-raise-instance-touchpad-on

(defmacro ror-id-touchpad-on-cmd (metacmd)
  (destructuring-bind (fn-name doc-string command id binding) metacmd
    `(progn
       (defcommand ,fn-name () ()
         ,doc-string
         (roraise-and-touchpad-on-instance ,command ,id))
       (define-key *root-map* (kbd ,binding) (string-downcase (symbol-name ',fn-name))))))

(defmacro ror-id-tpd-on-category (data)
  `(mapcro ror-id-touchpad-on-cmd ,data))

;; ========================== run-shell-command

(defmacro run-shell-command-cmd (metacmd)
  (destructuring-bind (fn-name doc-string command binding) metacmd
    `(progn
       (defcommand ,fn-name () ()
         ,doc-string
         (run-shell-command ,command))
       (define-key *root-map* (kbd ,binding) (string-downcase (symbol-name ',fn-name))))))

(defmacro run-shell-command-category (data)
  `(mapcro run-shell-command-cmd ,data))

;; ========================== shell-command

(defmacro shell-command-cmd (metacmd)
  (destructuring-bind (fn-name doc-string command binding) metacmd
    `(progn
       (defcommand ,fn-name () ()
         ,doc-string
         (shell-command ,command))
       (define-key *root-map* (kbd ,binding) (string-downcase (symbol-name ',fn-name))))))

(defmacro shell-command-category (data)
  `(mapcro shell-command-cmd ,data))
