#!/bin/sbcl --script
;;https://www.w3.org/TR/selectors-3/
;;https://imagemagick.org/api/magick-property.php
;;https://imagemagick.org/api/magick-image.php
(require :uiop)
(uiop:chdir (uiop:pathname-directory-pathname *load-truename*))
(format t "~a" (uiop:getcwd))
;;TODO: make loading paths absolute
(load "parse.lisp")
(ql:quickload "lisp-magick-wand")
(ql:quickload "dexador")
(ql:quickload "lquery")
(ql:quickload "local-time")
(ql:quickload "str")
(defun pangofy (element)
	(progn
		(lquery:$ element "br" (replace-with "
"))
		(lquery:$ element "span[class=greentext] > a[class=backlink]" (wrap-inner (
			format nil "<span foreground=~a>" QUOTED
		)) (children) (unwrap) (unwrap) ) ;;quote
		(lquery:$ element "a" (wrap-inner(
			format nil "<span foreground=~a>" LINK
		)) (children) (unwrap)) ;;link
		(lquery:$ element "span[class=greentext]" (attr "foreground" GREENTEXT) (remove-attr "class"));;greentext
		(string-trim
			" " (vector-pop(
				lquery:$ element (html)
		)))
	)
)
;;TODO read thread from capture.lisp
;;NOTE: not using 4chan API because of deleted threads/posts
(defconstant PAGE (
	lquery:$ (
		initialize (
			dex:get "https://desuarchive.org/g/thread/102165191"
))))
(defvar *image* (
	magick:new-magick-wand
))
(defvar *pixel-wand* (
	magick:new-pixel-wand
))
(magick:set-size *image* 2000 1000);;TODO: Calculate dimensions
(magick:read-image *image* (
	concatenate 'string "xc:" BACKGROUND
))
;;TODO: figure out text wrapping
;;TODO: figure out text indentation around image
(let* (
	(op-id (
		lquery:$1 PAGE "header > div[class=post_data] > a[data-function=highlight]" (attr "data-post")
	))
	(op-image-element (
		lquery:$1 PAGE "a[class=thread_image_link]"
	))
	(op-image-width (
		parse-integer(
			lquery:$1 op-image-element "img" (attr "width")
		)
	))
)
	(if (
		or INCLUDE-OP (
			member op-id *posts* :test 'string-equal
		)
	)
		(progn
			(let (
				(op-image (magick:new-magick-wand))
			)
				(magick:read-image op-image (
					lquery:$1 op-image-element (attr "href")
				))
				(magick:resize-image op-image op-image-width (
					parse-integer(
						lquery:$1 op-image-element "img" (attr "height")
					)
				) 4;;CubicGaussian 
				)
				(magick:composite-image *image* op-image
					54 ;;OverCompositeImage https://github.com/ImageMagick/ImageMagick/blob/5fcf6ae2a93af8771b6a407eb8e14a27ced54bc2/MagickCore/composite.h#L81
				T 0 0)
			)
			(let (
				(op-post (magick:new-magick-wand))
			)
				(magick:set-font op-post "fonts/ARIAL.TTF")
				(magick:set-pointsize op-post 10.0)
				(magick:pixel-set-color *pixel-wand* BACKGROUND)
				(magick:set-background-color op-post *pixel-wand*)
				(magick:read-image op-post (
					format nil "pango:<span background=\"~a\" weight=\"bold\"><span foreground=\"~a\">~a</span> <span foreground=\"~a\">~a</span> <span foreground=\"~a\">~a No.~a  <span foreground=\"~a\">~a</span></span>

~a</span>" BACKGROUND TITLE (
						str:replace-all "&" "&#38;" (
							lquery:$1 page "h2[class=post_title]" (text) ;; thread title
					)) NAME (
						lquery:$1 page "header > div[class=post_data] > span[class=post_poster_data] > span[class=post_author]" (text) ;;op name
					)
					TEXT (
						local-time:format-timestring nil (
							local-time:parse-timestring (
								lquery:$1 PAGE "header > div[class=post_data] > span[class=time_wrap] > time" (attr "datetime")
							)
						) :format '(
							(:month 2) "/" (:day 2) "/" :short-year "(" :short-weekday ")" (:hour 2) ":" (:min 2) ":" (:sec 2)
						)
					) op-id QUOTEDBY (
						lquery:$1 PAGE "div[class=backlink_list] > span[class=post_backlink]" (text)
					) (
						pangofy(
							lquery:$1 PAGE "div[class=text]"
				))))
				(magick:composite-image *image* op-post 54 T (
					+ op-image-width 20
				) 10)
				(setq *posts* (
					remove op-id *posts* :test 'string-equal
				))
			)
		)
	)
)
(magick:pixel-set-color *pixel-wand* POSTBACKGROUND)
(loop for post-id in *posts* do (
	let* (
		(post-selector (
			format nil "article[id=~a]" post-id
		))
		(post-element (
			lquery:$1 PAGE post-selector
		))
		(post-header (
			magick:new-magick-wand
		))
		(post-text (
			magick:new-magick-wand
		))
		(post-image-element(
			lquery:$1 post-element "div[class=post_wrapper]  > div[class=thread_image_box]"
		))
	)
	(magick:set-background-color post-header *pixel-wand*)
	(magick:set-font post-header "fonts/ARIAL.TTF")
	(magick:set-pointsize post-header 10.0)
	(magick:set-background-color post-text *pixel-wand*)
	(magick:set-font post-text "fonts/ARIAL.TTF")
	(magick:set-pointsize post-text 10.0)
	(magick:read-image post-header (
		format nil "pango:<span background=\"~a\" weight=\"bold\"><span foreground=\"~a\">~a</span> <span foreground=\"~a\">~a No.~a  <span foreground=\"~a\">~a</span></span></span>" POSTBACKGROUND NAME (
			lquery:$1 post-element "div[class=post_wrapper] > header > div[class=post_data] > span[class=post_poster_data] > span[class=post_author]" (text) ;;poster name
		)
		TEXT (
			local-time:format-timestring nil ( ;;post time
				local-time:parse-timestring (
					lquery:$1 post-element "div[class=post_wrapper] > header > div[class=post_data] > span[class=time_wrap] > time" (attr "datetime")
				)
			) :format '(
				(:month 2) "/" (:day 2) "/" :short-year "(" :short-weekday ")" (:hour 2) ":" (:min 2) ":" (:sec 2)
			)
		) post-id QUOTEDBY (
			lquery:$1 post-element "div[class=backlink_list] > span[class=post_backlink]" (text)
		)
	))
	(lquery:$ post-element "div[class=text] br" (replace-with "
"))
;;TODO: add "(OP)" when OP is quoted
	(lquery:$ post-element "div[class=text] > span[class=greentext] > a[data-backlink=true]" (unwrap) (wrap-inner (format nil "<span foreground=\"~a\">" QUOTED)) (children) (unwrap)) ;; highlight quotes
	(lquery:$ post-element "div[class=text] > a[target=_blank]" (wrap-inner (format nil "<span foreground=\"~a\">" LINK)) (children) (unwrap));; color links
;;TODO:greentext
;;TODO:links
;;TODO:code
;;TODO:/pol/ flags and ids
;;TODO:trips
	(magick:read-image post-text (
		format nil "pango:<span background=\"~a\" weight=\"bold\">~a</span>" POSTBACKGROUND (
			str:substring 19 -7 (
				lquery:$1 post-element "div[class=text]" (serialize)
			)
		)
	))
))
(magick:write-image *image* "test.png")
