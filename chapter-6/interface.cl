(defun say-hello ()
  (print "名前を入力してくだしあ: ") ;; 表示位置が行頭でなければ改行
  (let ((your-name (read)))
    (prin1 "こんちは！") ;; 改行しない
    (print your-name)))

;; "" で囲わないと error
(defun say-hello-with-exception ()
  (print "名前を入力してくださいあ: ")
  (let ((your-name (read)))
    (if (typep your-name 'string)
	(format t "こんにちは！ ~a" your-name)
	(error "your-name が string ではない"))))


(defun say-hello-princ ()
  (princ "名前を入力してくだしあ: ") ;; string に "" がつかず出力される
  (let ((your-name (read)))
    (princ "こんちは！") 
    (princ your-name)))

(defun game-read ()
  (let ((cmd (read-from-string
	      (concatenate 'string "(" (read-line) ")"))))
    (flet ((quote-it (x)
	     (list 'quote x)))
      (cons (car cmd) (mapcar #'quote-it (cdr cmd))))))

(defparameter *allowed-commands* '(look walk pickup players-inventory))

(defun game-eval (sexp)
  (if (member (car sexp) *allowed-commands*)
      (eval sexp)
      '(そんな command しらない！)))

;; ばかでかい
(defun tweak-text (lst caps lit) ;; caps は文頭かを示す, 
  (when lst
    (let ((item (car lst))
	  (rest (cdr lst)))
      (cond
	;; item が " " なら 自身の後ろの文字列をわたす
	((eql item #\space) (cons item (tweak-text rest caps lit)))
	;; item が "!", "?", "." のいずれかなら自身の後ろの文字列をわたす
	((member item '(#\! #\? #\.)) (cons item (tweak-text rest t lit)))
	
	((eql item #\") (tweak-text rest caps (not lit)))
	
	(lit (cons item (tweak-text rest nil lit)))

	(caps (cons (char-upcase item) (tweak-text rest nil lit)))

	(t (cons (char-downcase item) (tweak-text rest nil nil)))))))

(defun game-print (lst)
  ;; coerce: 強制する, 脅かすの意らしい 初めてみたけどきもい
  ;; (coerce object 'result-type) で object を result-type にかえるらしい 便利ね~
  (princ (coerce
	  (tweak-text (coerce
		       (string-trim "() " (prin1-to-string lst)) 'list) t nil)
	  'string))
  (fresh-line))

(defun game-repl ()
  (let ((cmd (game-read)))
    (unless (eq (car cmd) 'quit)
      (game-print (game-eval cmd))
      (game-repl))))

