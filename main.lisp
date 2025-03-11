#!/bin/sbcl --script
;;https://www.w3.org/TR/selectors-3/
;;https://imagemagick.org/api/magick-property.php
;;https://imagemagick.org/api/magick-image.php
;;https://imagemagick.org/api/drawing-wand.php
(require :uiop)
(uiop:chdir (uiop:pathname-directory-pathname *load-truename*))
(format t "~a" (uiop:getcwd))
;;TODO: make loading paths absolute
;;TODO: fix squares (checkboxes)
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
(defun post-text (gap_x gap_y background_color pango-text)
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
			(post_width MAX-WIDTH)
			(last-rendered-index 0)
			(height 0)
			(width 0)
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
				width-var :double
			)
			(
				height-var :double
			)
			(
				x-var :double
			)
			(
				y-var :double
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
				(cairo::cairo_recording_surface_ink_extents (cairo::get-pointer recording-surface) x-var y-var width-var height-var)
				(setq height (
						ceiling (
							+ (
								cffi:mem-ref height-var :double
							) (
								cffi:mem-ref y-var :double
				))))
				(setq width (
					ceiling (
						+ (
							cffi:mem-ref width-var :double
						) (
							cffi:mem-ref x-var :double
				))))
				(if (
					< width post_width
				) (
					setq post_width width
				))
				(setq surface (
					cairo:create-image-surface :argb32 post_width height
				))
				(setq cr (
					cairo:create-context surface
				))
				(cairo:set-source-rgba (
						/ (
							parse-integer (
								subseq background_color 1 3
							) :radix 16
						) 255.0
					) (
						/ (
							parse-integer (
								subseq background_color 3 5
							) :radix 16
						) 255.0
					) (
						/ (
							parse-integer (
								subseq background_color 5 7
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
(defun get-image (post-id element) (
	let* (
		(image-link (
			lquery:$1 element (attr "href")
		))
		(is-video (
			ppcre:scan "\.(webm|gif|mp4)$" image-link
		))
		(img (
			magick:new-magick-wand
		))
	) (
		if (
			expand post-id
		) (
			progn
			(if is-video (
				progn
				(asdf:run-shell-command "ffmpeg -i ~a ~a /tmp/~a.~a" image-link (
					if (
						string-equal OUTPUT-EXTENSION "gif"
					) "-frames 1"  ""
				) post-id OUTPUT-EXTENSION)
				(magick:read-image img (
					format nil "/tmp/~a.~a" post-id OUTPUT-EXTENSION
				))
			) (
				magick:read-image img image-link
			)) 
			(values img (
				magick:get-image-width img
			) (
				magick:get-image-height img
			))
		) (
			let (
				(img-height (
					parse-integer(
						lquery:$1 element "img" (attr "height")
				)))
				(img-width (
					parse-integer(
						lquery:$1 element "img" (attr "width")
				)))
			)
				(if is-video (
					progn
					(asdf:run-shell-command "ffmpeg -i ~a ~a -vf scale=~a:~a /tmp/~a.~a" image-link (
						if (
							string-equal OUTPUT-EXTENSION "gif"
						) "-frames 1"  ""
					) img-width img-height post-id OUTPUT-EXTENSION)
					(magick:read-image img (
						format nil "/tmp/~a.~a" post-id OUTPUT-EXTENSION
					))
				) (
					progn
					(magick:read-image img image-link)
					(magick:resize-image img img-width img-height 4);;CubicGaussian 
				)) 
				(values img img-width img-height)
		)
	)
))
;;NOTE: not using 4chan API because of deleted threads/posts
(defconstant PAGE (
	lquery:$ (
		initialize (
			dex:get ARCHIVE
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
	)(
		multiple-value-bind (op-image op-image-x op-image-y) (
			get-image op-id (
				lquery:$1 PAGE "a[class=thread_image_link]"
		)) (
			let* (
				(op-post (
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
				(if (
					 > op-image-x MAX-WIDTH
				) (
					error "OP image size exceeds MAx-WIDTH"
				))
				(magick:set-font op-post-info "fonts/ARIAL.TTF")
				(magick:set-pointsize op-post-info 10.0)
				(magick:set-font op-image-info "fonts/ARIAL.TTF")
				(magick:set-pointsize op-image-info 10.0)
				(magick:pixel-set-color *pixel-wand* BACKGROUND)
				(magick:set-background-color op-post-info *pixel-wand*)
				(magick:read-image op-post-info (
					format nil "pango:<span background=\"~a\" weight=\"bold\"><span foreground=\"~a\">~a</span> <span foreground=\"~a\">~a~a</span> <span foreground=\"~a\">~a No.~a  <span foreground=\"~a\">~a</span></span></span>" BACKGROUND TITLE (
						str:replace-all "&" "&#38;" (
							lquery:$1 page "h2[class=post_title]" (text) ;; thread title
					)) NAME CHECKBOX (
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
								round ( ;;display integer
									* 1.024 ( ;;KiB to KB
										parse-integer (
											subseq filesize 0 ( ;;extract value
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
					post-text (
						+ 20 op-image-x
					) op-image-y BACKGROUND (
						pangofy (
							lquery:$1 PAGE "div[class=text]"
				))))
				(magick:set-size op-post (
					+ 20 (
						magick:get-image-width op-post-text ;;TODOL return instead on call
					)
				) (
					+ 51 (
						max (
							 magick:get-image-height op-post-text
						) op-image-y
					)
				))
				(magick:read-image op-post (
					concatenate 'string "xc:" BACKGROUND
				))
				(magick:composite-image op-post op-image-info 54 T 0 0)
				(magick:composite-image op-post op-post-info 54 T (
					+ op-image-x 40
				) 19)
				(setq *posts* (
					remove op-id *posts* :test 'string-equal
				))
				(magick:composite-image op-post op-post-text 54 T 20 51);;19+13+19+20
				(magick:composite-image op-post op-image
					54 ;;OverCompositeImage https://github.com/ImageMagick/ImageMagick/blob/5fcf6ae2a93af8771b6a407eb8e14a27ced54bc2/MagickCore/composite.h#L81
				T 20 19)
				(magick:write-image op-post "op.png")
))))
(defconstant MAX-ALLOWABLE-SIZE (
	- MAX-WIDTH 40
))
(let (
		(post-image-width 0)
		(post-image-height 0)
		(post-image nil)
		(post-info (
			magick:new-magick-wand
		))
		(post-image-info (
			magick:new-magick-wand
		))
		(post-box (
			magick:new-magick-wand
		))
		(post-box-height 0)
		(post-box-width 0)
		(post-text-image NIL)
		(draw NIL)
		(draw-wand (
			magick:new-drawing-wand
		))
		(pixel-wand (
			magick:new-pixel-wand
		))
	)
	(loop for post-id in *posts* do
		(let*(
			(post-selector (
				format nil "aside[class=posts] > article[id=~a]" post-id
			))
			(post-element (
				lquery:$1 PAGE  post-selector
			))
			(post-image-element (
				lquery:$1 post-element "a[class=thread_image_link]"
			))
		)
			(magick:set-font post-info "fonts/ARIAL.TTF")
			(magick:set-pointsize post-info 10.0)
			(magick:set-font post-image-info"fonts/ARIAL.TTF")
			(magick:set-pointsize post-image-info 10.0)
			(if post-image-element (
				progn
				(multiple-value-setq (post-image post-image-width post-image-height) (
					get-image post-id post-image-element
				))
				(if (
					> post-image-width MAX-ALLOWABLE-SIZE
				)
					(
					error (
						format nil "Image of post ~a exceeds maximum allowable width of ~a." post-id MAX-ALLOWABLE-SIZE
					)
				))
			))
			(magick:read-image post-info (
				format nil "pango:<span background=\"~a\" weight=\"bold\"><span foreground=\"~a\">~a~a</span> <span foreground=\"~a\">~a No.~a  <span foreground=\"~a\">~a</span></span></span>" POSTBACKGROUND NAME CHECKBOX (
					lquery:$1 post-element "header > div[class=post_data] > span[class=post_poster_data] > span[class=post_author]" (text) ;;op name
				)
				TEXT (
					local-time:format-timestring nil (
						local-time:parse-timestring (
							lquery:$1 post-element "header > div[class=post_data] > span[class=time_wrap] > time" (attr "datetime")
						)
					) :format '(
						(:month 2) "/" (:day 2) "/" :short-year "(" :short-weekday ")" (:hour 2) ":" (:min 2) ":" (:sec 2)
					)
				) post-id QUOTEDBY (
					lquery:$1 post-element "div[class=backlink_list] > span[class=post_backlink]" (text)
			)))
			(setq post-text-image (
				post-text (
					+ 20 post-image-width
				) post-image-height POSTBACKGROUND (
					pangofy (
						lquery:$1 post-element "div[class=text]"
				))
			))
			(magick:read-image post-image-info (
				format nil "pango:<span background=\"~a\" weight=\"bold\">File: <span foreground=\"~a\" underline='single'>~a</span> ~a</span>" POSTBACKGROUND LINK (
					lquery:$1 post-element "div[class=post_file] > a[class=post_file_filename]" (text)
					) ((
						lambda (info-string)
							(let* (
								(infos (
									cl-ppcre:split ", " info-string
								))
								(filesize (
									pop infos
								))
							)
								(format nil "(~a KB, ~a)" (
									round (
										* 1.024 (
											parse-integer (
												subseq filesize 1 (
													- (
														length filesize
													) 3
								))))) (
									pop infos
								))
							)
					) (
						lquery:$1 PAGE "div[class=post_file]" (text)
					))
				))
			(setq post-box-width (
				+ 5 (
					max (
						magick:get-image-width post-info
					) (
						magick:get-image-width post-image-info
					) (
						+ 20 (
							magick:get-image-width post-text-image
					))
				) ;;2+2+1
			))
			(setq post-box-height (
				+ 67 (;;2+19+15+3+13.33+13.33+2
					max post-image-height (
						magick:get-image-height post-text-image ;;TODO return instead of call
					)
				)
			))
			(magick:set-size post-box post-box-width post-box-height)
			(magick:read-image post-box (
				concatenate 'string "xc:" POSTBACKGROUND
			))
			(magick:pixel-set-color pixel-wand POSTBORDER)
			(magick:draw-set-stroke-color draw-wand pixel-wand)
			(magick:draw-set-stroke-width draw-wand 2.0);;one pixel on image one off image
			(magick:draw-line draw-wand 0 post-box-height post-box-width post-box-height);;add bottom border
			(magick:draw-line draw-wand post-box-width 0 post-box-width post-box-height);; add right border
			(magick:draw-image post-box draw-wand)
			(magick:composite-image post-box post-info 54 T 0 0)
			(magick:composite-image post-box post-text-image 54 T 20 37);;19+15+3
			(if post-image-element (
				progn
				(magick:composite-image post-box post-image-info 54 T 0 19)
				(magick:composite-image post-box post-image 54 T 20 37);;19+15+3
			))
			(magick:write-image post-box "post-box.png");;testing
			(magick:destroy-magick-wand post-image)
			(return);;testing
	))
)
