(load "~/.local/lib/quicklisp/setup");;quicklisp path
(load "themes/yotsuba_b.lsp");;theme
(defconstant INCLUDE-OP t)
(defconstant OUTPUT-EXTENSION "GIF")
(defconstant CHECKBOX "<span background=\"white\" foreground=\"black\" weight=\"ultralight\">⬜</span>");;set to blank string to remove checkboxes
(defconstant MAX-WIDTH 1261)
(defun EXPAND (post_id)( ;;change to nil to expand none, change to T to expand all
	member post_id *expands* :test 'string-equal
))
