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
(ql:quickload "cl-pango")
(ql:quickload "cl-cairo2")
(ql:quickload "dexador")
(ql:quickload "lquery")
(ql:quickload "local-time")
(ql:quickload "str")
(ql:quickload "cl-ppcre")
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
(defun post-text (gap_x gap_y post_width background_color pango-text)
	(let* (
			(image (
				magick:new-magick-wand
			))
			(recording-surface (
				cairo:create-recording-surface :color-alpha
			))
			(surface NIL)
			(cr (
				cairo:create-context recording-surface
			))
			(context (
				pango:pango_font_map_create_context (
					pango:pango_cairo_font_map_new
				)
			))
			(layout (
				pango:pango_layout_new context
			))
			(last-rendered-index 0)
			(height 0)
		)
		(cairo:set-source-rgba (
				/ (
					parse-integer (
						subseq TEXT 1 3
					) :radix 16
				) 255.0
			) (
				/ (
					parse-integer (
						subseq TEXT 3 5
					) :radix 16
				) 255.0
			) (
				/ (
					parse-integer (
						subseq TEXT 5 7
					) :radix 16
				) 255.0
			)
			1.0 cr)
		(cairo:move-to gap_x 0 cr)
		(pango:pango_layout_set_font_description layout (
			pango:pango_font_description_from_string "Arial 10"
		))
		(pango:pango_layout_set_markup layout pango-text -1)
		(pango:pango_layout_set_width layout (
			* pango:PANGO_SCALE (
				- post_width gap_x
		)))
		(pango:pango_layout_set_height layout (
			* pango:PANGO_SCALE gap_y
		))
		(pango:pango_cairo_layout_path (
			slot-value cr 'cairo::pointer
		) layout)
		(pango:pango_layout_set_wrap layout :PANGO_WRAP_WORD_CHAR)
		(pango:pango_cairo_show_layout (
			slot-value cr 'cairo::pointer
		) layout)
		(setq last-rendered-index (
			- (
				length (
					pango:pango_layout_get_text layout
				)) 2
		))
		(cffi:with-foreign-objects (
			(;find wrap cursor
				rect 'pango:PangoRectangle
			)
			(
				height-var :double
			)
			(
				_ :double
			)
		)
			(pango:pango_layout_index_to_pos layout last-rendered-index rect)
			(if (
				= 0 ( ;wrap needed
						cffi:foreign-slot-value rect 'pango:PangoRectangle 'pango::height
					))
				(let (
					(cursor 0)
					(open-tag NIL)
					(close-tag NIL)
					(in-escape NIL)
					(tags NIL)
					(bottom-text NIL)
				)
				(setq bottom-text (
					subseq pango-text 
						(-
							(loop for i from 0 below (length pango-text);find wrap point
							for char = (char pango-text i) do;ChatGPT
							(pango:pango_layout_index_to_pos layout cursor rect)
							(if (
								= 0 (
									cffi:foreign-slot-value rect 'pango:PangoRectangle 'pango::height
								)
							) (return i))
							(case char
								(#\< (
									if (
										eq (
											char pango-text (
												+ 1 i
											)) #\/
										)(;closing tag
											progn 
											(setq close-tag T)
											(setq tags (
												cdr tags
											))
										)(
											setq open-tag i;mark index for string extraction
										)))
								 (#\> (
									if open-tag (
										progn (
											setq tags (cons (
												subseq pango-text open-tag (
													+ i 1
												))
											   tags ))
										(setq open-tag NIL))
										(setq close-tag NIL)
									   ))
								 (#\& (
									   setq in-escape T
									   )(
									   setq cursor (+ cursor 1)
									   ))
								 (#\; (
									   setq in-escape NIL
									   ))
								 (otherwise (
									if (
										not (
											or open-tag close-tag in-escape
										))(setq cursor (
											+ cursor 1
										)))))
							) 1)
						))
				(cairo:move-to 0 gap_y cr)
				(pango:pango_layout_set_width layout (
					* pango:PANGO_SCALE post_width
				))
				(pango:pango_layout_set_width layout (
					* pango:PANGO_SCALE post_width
				))
				(pango:pango_layout_set_height layout -1)
				(pango:pango_layout_set_markup layout (
					str:concat (
						format nil "~{~A~}" (
							reverse tags
						)
					) bottom-text
				) -1)
				(pango:pango_cairo_layout_path (
					slot-value cr 'cairo::pointer
				) layout)
				(pango:pango_layout_set_wrap layout :PANGO_WRAP_WORD_CHAR)
				(pango:pango_cairo_show_layout (
					slot-value cr 'cairo::pointer
				) layout)
				))
				(cairo::cairo_recording_surface_ink_extents (cairo::get-pointer recording-surface) _ _ _ height-var)
				(setq height (
					ceiling (
						cffi:mem-ref height-var :double
				)))
				(setq surface (
					cairo:create-image-surface :argb32 post_width height
				))
				(setq cr (
					cairo:create-context surface
				))
				(cairo:set-source-rgba (
						/ (
							parse-integer (
								subseq background 1 3
							) :radix 16
						) 255.0
					) (
						/ (
							parse-integer (
								subseq background 3 5
							) :radix 16
						) 255.0
					) (
						/ (
							parse-integer (
								subseq background 5 7
							) :radix 16
						) 255.0
					)
					1.0 cr)
				(cairo:paint cr);;paint background
				(cairo:set-source-surface recording-surface 0 0 cr)
				(cairo:paint cr);; paint text
				(magick:constitute-image image post_width height "BGRA" ':char (
					sb-sys:vector-sap (
						make-array (
							* 4 post_width height; total pixel data
						) :element-type '(unsigned-byte 8) :initial-contents (
							coerce (
								cairo:image-surface-get-data surface
							) 'list
						))))
			)
			(cairo:destroy surface)
			(return-from post-text image)
	)
)
;;TODO read thread from capture.lisp
;;NOTE: not using 4chan API because of deleted threads/posts
(defconstant PAGE (
	lquery:$ (
		initialize (
			dex:get "https://desuarchive.org/g/thread/103105320"
))))
(defvar *image* (
	magick:new-magick-wand
))
(defvar *pixel-wand* (
	magick:new-pixel-wand
))
(magick:set-size *image* 2000 2000);;TODO: Calculate dimensions
(magick:read-image *image* (
	concatenate 'string "xc:" BACKGROUND
))
(let (
	(op-id (
		lquery:$1 PAGE "header > div[class=post_data] > a[data-function=highlight]" (attr "data-post")
	))
)
	(if (
		or INCLUDE-OP (
			member op-id *posts* :test 'string-equal
		)
	)
		(let* (
			(op-image-element (
				lquery:$1 PAGE "a[class=thread_image_link]"
			))
			(op-image-width (
				parse-integer(
					lquery:$1 op-image-element "img" (attr "width")
				)))
			(op-image-x (
				+ 20 op-image-width
			))
			(op-image-y (
				parse-integer(
					lquery:$1 op-image-element "img" (attr "height")
			)))
			(op-post (
				magick:new-magick-wand
			))
			(op-image (
				magick:new-magick-wand
			))
			(op-post-info (
				magick:new-magick-wand
			))
			(op-image-info (
				magick:new-magick-wand
			))
			(op-post-text NIL)
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
			(magick:set-font op-post-info "fonts/ARIAL.TTF")
			(magick:set-pointsize op-post-info 10.0)
			(magick:set-font op-image-info "fonts/ARIAL.TTF")
			(magick:set-pointsize op-image-info 10.0)
			(magick:pixel-set-color *pixel-wand* BACKGROUND)
			(magick:set-background-color op-post-info *pixel-wand*)
			(magick:read-image op-post-info (
				format nil "pango:<span background=\"~a\" weight=\"bold\"><span foreground=\"~a\">~a</span> <span foreground=\"~a\">~a</span> <span foreground=\"~a\">~a No.~a  <span foreground=\"~a\">~a</span></span></span>" BACKGROUND TITLE (
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
			)))
			(magick:read-image op-image-info (
				format nil "pango:<span background=\"~a\" weight=\"bold\">File: <span foreground=\"~a\" underline='single'>~a</span> ~a</span>" BACKGROUND LINK (
					lquery:$1 PAGE "div[class=post_file] > a[class=post_file_filename]" (text)
				) ((
					lambda (info-string)
						(let* (
							(infos (
								cl-ppcre:split "," info-string
							))
							(filesize (
								pop infos
							))
						)
						(format nil "(~a KB, ~a)" (
							round (
								* 1.024 (
									parse-integer (
										subseq filesize 0 (
											- (
												length filesize
											) 3
						))))) (
							pop infos
						)))
				) (
					lquery:$1 PAGE "div[class=post_file]" (text)
				))
			))
			(setq op-post-text (
				post-text op-image-x op-image-y MAX-WIDTH BACKGROUND (
					pangofy (
						lquery:$1 PAGE "div[class=text]"
			))))
			(magick:set-size op-post MAX-WIDTH 2000);;TODO: Calculate dimensions
			(magick:read-image op-post (
				concatenate 'string "xc:" BACKGROUND
			))
			(magick:composite-image op-post op-image-info 54 T 0 0)
			(magick:composite-image op-post op-post-info 54 T (
				+ op-image-x 20
			) 19)
			(setq *posts* (
				remove op-id *posts* :test 'string-equal
			))
			(magick:composite-image op-post op-post-text 54 T 20 51);;19+13+19+20
			(magick:composite-image op-post op-image
				54 ;;OverCompositeImage https://github.com/ImageMagick/ImageMagick/blob/5fcf6ae2a93af8771b6a407eb8e14a27ced54bc2/MagickCore/composite.h#L81
			T 20 19)
			(magick:write-image op-post "test.png") ;; testing
		)))
;; (let* (
;; ))
