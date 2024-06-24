#!/bin/sbcl --script
;;https://www.w3.org/TR/selectors-3/
;;https://imagemagick.org/api/magick-property.php
;;https://imagemagick.org/api/magick-image.php
(require :uiop)
(uiop:chdir (uiop:pathname-directory-pathname *load-truename*))
(format t "~a" (uiop:getcwd))
(load "config.lsp")
(ql:quickload "lisp-magick-wand")
(ql:quickload "dexador")
(ql:quickload "lquery")
(defvar *page* (
	lquery:$ (
		initialize (
			dex:get "https://desuarchive.org/g/thread/100883730"
))))
(defmacro css (element selector &body action)`(vector-pop(
	lquery:$ ,element ,selector ,@action
)))
(format t "~a" (css *page* "a[class=thread_image_link] img" (serialize)))
(defvar *image* (
	magick:new-magick-wand
))
(magick:set-size *image* 1000 1000)
(magick:read-image *image* (
	concatenate 'string "xc:" BACKGROUND
))
(let ((op-image (magick:new-magick-wand))
	(op-image-element (
		css *page* "a[class=thread_image_link]"
	))
)
	(magick:read-image op-image (
		css op-image-element (attr "href")
	))
	(magick:resize-image op-image 
		(parse-integer(
			css op-image-element "img" (attr "width")
		))
		(parse-integer(
			css op-image-element "img" (attr "height")
		))
		4;;CubicGaussian 
	)
	(magick:composite-image *image* op-image
		54 ;;OverCompositeImage https://github.com/ImageMagick/ImageMagick/blob/5fcf6ae2a93af8771b6a407eb8e14a27ced54bc2/MagickCore/composite.h#L81
	T 0 0)
)
(magick:write-image *image* "test.png")
