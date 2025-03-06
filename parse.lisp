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
(defconstant *expands* (
	mapcar (
		lambda (x) (subseq x 2)
	) (
		ppcre:all-matches-as-strings ">>[0-9]{9}" EXPANDS
	)
))
(setq THREAD (;;4chan.og
	subseq THREAD (
		if (
			ppcre:scan "^https://" THREAD
		) 25 17
)))
(defconstant ARCHIVE (
	if (
		member (
			subseq THREAD 0 ( ;;board name
				position #\/ THREAD
			)
		) (list "a" "aco" "an" "c" "cgl" "co" "d" "fit" "g" "his" "int" "k" "m" "mlp" "mu" "q" "qa" "r9k" "tg" "trash" "vr" "wsg") :test 'string-equal
	) (
		concatenate 'string "https://desuarchive.org/" THREAD
	)
))
