;;; game_of_life.lsp

;;; ============================================================== ;;;
;;; ================== GAME OF LIFE SIMULATION =================== ;;;
;;; ============================================================== ;;;

;;; (ql:quickload :lispbuilder-sdl)

(defparameter *matrix* nil)
(defparameter *matrix_h* 20)
(defparameter *matrix_w* 36)

(defun _blinker (i j)
	(setf (aref *matrix* i j) 1)
  	(setf (aref *matrix* i (+ j 1)) 1)
  	(setf (aref *matrix* i (+ j 2)) 1) t)

(defun blinker (i j)
  (if (or (>= i *matrix_h*) (>= (+ j 2) *matrix_w*) (< i 0) (< j 0))
	'nil
	(_blinker i j)))

(defun _block4 (i j)
	(setf (aref *matrix* i j) 1)
  	(setf (aref *matrix* i (+ j 1)) 1)
  	(setf (aref *matrix* (+ i 1) j) 1)
  	(setf (aref *matrix* (+ i 1) (+ j 1)) 1) t)

(defun block4 (i j)
  (if (or (>= (+ i 1) *matrix_h*) (>= (+ j 1) *matrix_w*) (< i 0) (< j 0))
	'nil
	(_block4 i j)))

(defun _glider-gun (i j)
  	(block4 (+ i 4) j)
  	(block4 (+ i 2) (+ j 34))
  	(setf (aref *matrix* i (+ j 24)) 1)
  	(setf (aref *matrix* (+ i 1) (+ j 22)) 1)
  	(setf (aref *matrix* (+ i 1) (+ j 24)) 1)
  	(setf (aref *matrix* (+ i 2) (+ j 12)) 1)
  	(setf (aref *matrix* (+ i 2) (+ j 13)) 1)
  	(setf (aref *matrix* (+ i 2) (+ j 20)) 1)
  	(setf (aref *matrix* (+ i 2) (+ j 21)) 1)
  	(setf (aref *matrix* (+ i 3) (+ j 11)) 1)
  	(setf (aref *matrix* (+ i 3) (+ j 15)) 1)
  	(setf (aref *matrix* (+ i 3) (+ j 20)) 1)
  	(setf (aref *matrix* (+ i 3) (+ j 21)) 1)
  	(setf (aref *matrix* (+ i 4) (+ j 10)) 1)
  	(setf (aref *matrix* (+ i 4) (+ j 16)) 1)
  	(setf (aref *matrix* (+ i 4) (+ j 20)) 1)
  	(setf (aref *matrix* (+ i 4) (+ j 21)) 1)
  	(setf (aref *matrix* (+ i 5) (+ j 10)) 1)
  	(setf (aref *matrix* (+ i 5) (+ j 14)) 1)
  	(setf (aref *matrix* (+ i 5) (+ j 16)) 1)
  	(setf (aref *matrix* (+ i 5) (+ j 17)) 1)
  	(setf (aref *matrix* (+ i 5) (+ j 22)) 1)
  	(setf (aref *matrix* (+ i 5) (+ j 24)) 1)
  	(setf (aref *matrix* (+ i 6) (+ j 10)) 1)
  	(setf (aref *matrix* (+ i 6) (+ j 16)) 1)
  	(setf (aref *matrix* (+ i 6) (+ j 24)) 1)
  	(setf (aref *matrix* (+ i 7) (+ j 11)) 1)
  	(setf (aref *matrix* (+ i 7) (+ j 15)) 1)
  	(setf (aref *matrix* (+ i 8) (+ j 12)) 1)
  	(setf (aref *matrix* (+ i 8) (+ j 13)) 1) t)

(defun glider-gun (i j)
  (if (or (>= (+ i 8) *matrix_h*) (>= (+ j 35) *matrix_w*) (< i 0) (< j 0))
	'nil
	(_glider-gun i j)))

(defun _ship (i j)
	(setf (aref *matrix* i j) 1)
	(setf (aref *matrix* i (+ j 1)) 1)
  	(setf (aref *matrix* (+ i 1) j) 1)
  	(setf (aref *matrix* (+ i 2) (+ j 1)) 1)
  	(setf (aref *matrix* (+ i 1) (+ j 2)) 1)
  	(setf (aref *matrix* (+ i 2) (+ j 2)) 1) t)

(defun ship (i j)
  (if (or (>= (+ i 2) *matrix_h*) (>= (+ j 2) *matrix_w*) (< i 0) (< j 0))
	'nil
	(_ship i j)))

(defun _light-ship (i j)
	(setf (aref *matrix* i (+ j 1)) 1)
	(setf (aref *matrix* i (+ j 4)) 1)
	(setf (aref *matrix* (+ i 1) j) 1)
  	(setf (aref *matrix* (+ i 2) j) 1)
  	(setf (aref *matrix* (+ i 3) j) 1)
  	(setf (aref *matrix* (+ i 2) (+ j 4)) 1)
  	(setf (aref *matrix* (+ i 3) (+ j 1)) 1)
  	(setf (aref *matrix* (+ i 3) (+ j 2)) 1)
  	(setf (aref *matrix* (+ i 3) (+ j 3)) 1) t)

(defun light-ship (i j)
  (if (or (>= (+ i 3) *matrix_h*) (>= (+ j 4) *matrix_w*) (< i 0) (< j 0))
	'nil
	(_light-ship i j)))

(defun _glider (i j)
	(setf (aref *matrix* i (+ j 1)) 1)
	(setf (aref *matrix* (+ i 1) (+ j 2)) 1)
  	(setf (aref *matrix* (+ i 2) j) 1)
  	(setf (aref *matrix* (+ i 2) (+ j 1)) 1)
  	(setf (aref *matrix* (+ i 2) (+ j 2)) 1) t)

(defun glider (i j)
  (if (or (>= (+ i 2) *matrix_h*) (>= (+ j 2) *matrix_w*) (< i 0) (< j 0))
	'nil
	(_glider i j)))

(defun init-cells ()
  (setq *matrix* (make-array (list *matrix_h* *matrix_w*))))

(defun get-val (i j)
  (if (or (< i 0) (< j 0) (>= i *matrix_h*) (>= j *matrix_w*))
	'0
	(rem (aref *matrix* i j) 2 )))

(defun life-rules (life sum)
  (if (= (rem life 2) 1)
	(if (or (= 2 sum) (= 3 sum))
	  '1
	  '3)
	(if (= sum 3)
	  '2
	  '0)))

(defun get-neighbour (i j)
  (let ((sum 0))
	(setf sum(+ sum (get-val (- i 1) j)))
	(setf sum(+ sum (get-val (+ i 1) j)))
	(setf sum(+ sum (get-val (- i 1) (- j 1))))
	(setf sum(+ sum (get-val (+ i 1) (- j 1))))
	(setf sum(+ sum (get-val (- i 1) (+ j 1))))
	(setf sum(+ sum (get-val (+ i 1) (+ j 1))))
	(setf sum(+ sum (get-val i (+ j 1))))
	(setf sum(+ sum (get-val i (- j 1))))
	sum))

(defun get-state(value)
  (if (or (= value 0) (= value 1))
	(+ value 0)
	(if (= value 3)
	'0
	'1)))

(defun god ()
  (dotimes (i *matrix_h*)
	(dotimes (j *matrix_w*)
	  (setf (aref *matrix* i j) (get-state (aref *matrix* i j))))) *matrix*)

(defun walk ()
  (dotimes (i *matrix_h*)
	(dotimes (j *matrix_w*)
	  (setf (aref *matrix* i j) (life-rules (aref *matrix* i j) (get-neighbour i j))))) *matrix*)

(defun usage ()
  (with-open-file (stream "game_of_life.txt")
    (let ((data (make-string (file-length stream))))
        (read-sequence data stream)
		data)))

(defun cycle ()
  (print *matrix*)
  (walk)
  (god)
  (print '0) t)

(defun main ()
  (usage)
  (init-cells)
  (glider-gun 0 0)
  (light-ship 15 15)
  (loop for i from 0 to 20
  	do (cycle)))
