;; ここでの日本語は symbol 扱いらしい すごい
(defparameter *nodes* '((living-room (あなたは居間にいる。魔法使いはソファでいびきをかいている。))
			(garden (あなたは美しい庭にいる。目の前に井戸がある。))
			(attic (あなたは屋根裏にいる。部屋の隅に巨大な溶接バーナーがある。))))

(defparameter *objects* '(ウイスキー バケツ カエル チェーン))

(defparameter *object-locations* '((ウイスキー living-room)
				   (バケツ living-room)
				   (チェーン garden)
				   (カエル garden)))

(defun describe-location (location nodes)
  (nth 1 (assoc location nodes))) ;; assoc、要は C++ だと map の nodes[location] ってことで

(defparameter *edges* '((living-room
			 (garden 西 扉)
			 (attic 上 はしご))
			(garden
			 (living-room 東 扉))
			(attic
			 (living-room 下 はしご))))

(defun describe-path (edge)
  `(ここから ,(nth 1 edge) に向かう ,(nth 2 edge) がある。))

;; (mapcar #'sqrt '(1 2 3 4 5)) -> (1.0 1.4142135 1.7320508 2.0 2.236068)
;; 第二引数の各要素に第一引数の関数を適用して新しい list を返す

;; (append '(hoge) '(fuga)) -> (HOGE FUGA)
;; list を結合

;; (apply #'append '((hoge) (fuga))) -> (HOGE FUGA)
;; 第二引数に第一引数の関数を適用する

;; mapcar は 1 変数関数, apply は多変数関数？

(defun describe-paths (location edges)
  (apply #'append (mapcar #'describe-path (cdr (assoc location edges)))))

(defun objects-at (location objects object-locations)
  (labels ((at-locations-p (objects)
	     (eq (cadr (assoc objects object-locations)) location)))
    (remove-if-not #'at-locations-p objects)))

(defun describe-objects (location objects object-locations)
  (labels ((describe-obj (obj)
	     `(床に ,obj がある。)))
    (apply #'append (mapcar #'describe-obj (objects-at location objects object-locations)))))

(defparameter *players-location* 'living-room)

(defun look ()
  (append (describe-location *players-location* *nodes*)
	  (describe-paths *players-location* *edges*)
	  (describe-objects *players-location* *objects* *object-locations*)))


;; :key のあとの関数が要素の比較前に適用され、第一引数が第二引数にあるか検索（あったらそれを返す）
(defun walk (direction)
  (let ((next (find direction
		    (cdr (assoc *players-location* *edges*))
		    :key #'cadr)))
    (if next
	(progn (setf *players-location* (nth 0 next))
	       (look))
	'(そっちには進めない。))))

;; member は 第二引数の list に 第一引数 key があるか確認
(defun pickup (object)
  (cond ((member object (objects-at *players-location* *objects* *object-locations*))
	 (push (list object 'body) *object-locations*)
	 `(あなたは ,object を持っている。))
	(t '(あなたはそれを入手できない。))))

(defun players-inventory ()
  (cons 'アイテム- (objects-at 'body *objects* *object-locations*)))
