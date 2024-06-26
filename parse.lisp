(load "config.lsp")
(load "capture.lsp")
(ql:quickload "cl-ppcre")
(defvar *posts* (
	mapcar (
		lambda (x) (subseq x 2)
	) (
		ppcre:all-matches-as-strings ">>[0-9]{9}" INCLUDE
	)
))
(setq *posts* (remove "101125290" *posts* :test 'string-equal))
(format t "~a" *posts*)
