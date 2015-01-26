;;; Utils

(in-package #:clx-truetype)

(defun drawable-screen (drawable)
  (typecase drawable
    (xlib:drawable
     (dolist (screen (xlib:display-roots (xlib:drawable-display drawable)))
       (when (xlib:drawable-equal (xlib:screen-root screen) (xlib:drawable-root drawable))
         (return screen))))
    (xlib:screen drawable)
    (t nil)))
