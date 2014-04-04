
(in-package #:clx-truetype)

(defvar *font-dirs* #+(or unix netbsd openbsd freebsd) (list "/usr/share/fonts/"
                                     (namestring (merge-pathnames ".fonts/" (user-homedir-pathname))))
        #+darwin (list "/Library/Fonts/")
        #+windows  (list (namestring
                          (merge-pathnames "fonts/" 
                                           (pathname (concatenate 'string (asdf:getenv "WINDIR") "/")))))
        "List of directories, which contain TrueType fonts.")

;; family ->
;;   subfamily -> filename
;;   subfamily -> filename
;;(defparameter *font-cache* (make-hash-table :test 'equal))
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defparameter +font-cache-filename+ 
    #.(merge-pathnames "font-cache.sexp"
                       (merge-pathnames ".fonts/" (user-homedir-pathname)))))

(eval-when (:load-toplevel :execute)
  (defparameter *font-cache*
    (if (fad:file-exists-p +font-cache-filename+)
        (cl-store:restore +font-cache-filename+)
        (make-hash-table :test 'equal))
    "Hashmap for caching font families, subfamilies and files."))

;;(pushnew (xlib:font-path *display*) *font-dirs*)
(defun cache-font-file (pathname)
  "Caches font file."
  (declare (special *font-cache*))
  (handler-case 
    (zpb-ttf:with-font-loader (font pathname)
      (multiple-value-bind (hash-table exists-p)
          (gethash (zpb-ttf:family-name font) *font-cache*
                   (make-hash-table :test 'equal))
        (setf (gethash (zpb-ttf:subfamily-name font) hash-table)
              pathname)
        (unless exists-p
          (setf (gethash (zpb-ttf:family-name font) *font-cache*)
                hash-table))))
    (condition () (return-from cache-font-file))))

(defun ttf-pathname-test (pathname)
  (string-equal "ttf" (pathname-type pathname)))

(defun cache-fonts ()
  "Caches fonts from @refvar{*font-dirs*} directories."
  (declare (special *font-cache*))
  (clrhash *font-cache*)
  (dolist (font-dir *font-dirs*)
    (fad:walk-directory font-dir #'cache-font-file :if-does-not-exist :ignore
                        :test #'ttf-pathname-test))
  (ensure-directories-exist +font-cache-filename+)
  (cl-store:store *font-cache* +font-cache-filename+))

(defun get-font-families ()
  "Returns cached font families."
  (declare (special *font-cache*))
  (let ((result (list)))
    (maphash (lambda (key value)
               (declare (ignorable value))
               (push key result)) *font-cache*)
    (nreverse result)))

(defun get-font-subfamilies (font-family)
  "Returns font subfamilies for current @var{font-family}. For e.g. regular, italic, bold, etc."
  (declare (special *font-cache*))
  (let ((result (list)))
    (maphash (lambda (family value)
               (declare (ignorable family))
               (when (string-equal font-family family)
                 (maphash (lambda (subfamily pathname)
                            (declare (ignorable pathname))
                            (push subfamily result)) value)
                 (return-from get-font-subfamilies 
                   (nreverse result)))) *font-cache*)
    (nreverse result)))

