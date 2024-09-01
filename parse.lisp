(load "config.lisp")
(load "capture.lisp")
(ql:quickload "cl-ppcre")
(defvar *posts* (
	mapcar (
		lambda (x) (subseq x 2)
	) (
		ppcre:all-matches-as-strings ">>[0-9]{9}" INCLUDE
	)
))
(format t "~a" *posts*)
