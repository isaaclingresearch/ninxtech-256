(in-package :256)

(defun on-new-window (body)
  "Start the application for a new window"
  (setf (title (html-document body)) "256")
  (let ((h1 (create-child body "<h1>Hello</h1>")))
    (set-on-click h1
		  (lambda (obj)
		    (declare (ignore obj))
		    (setf (color h1) :red)))))

(defun start-256 ()
  (initialize #'on-new-window)
  (open-browser))
