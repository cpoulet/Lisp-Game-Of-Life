(ql:quickload 'lispbuilder-sdl)

(defparameter *random-color* sdl:*white*)
(defparameter *matrix* nil)
(defparameter *matrix_h* 40)
(defparameter *matrix_w* 100)
(defparameter *win-w* 2500)
(defparameter *win-h* 1200)
(defparameter *square* 10)

(defun init-cells ()
  (setq *matrix* (make-array (list *matrix_h* *matrix_w*))))

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

(defun draw-life (i j)
  (sdl:draw-box
	(sdl:rectangle
	  :x (floor (+ j (* j *square*)))
	  :y (floor (+ i (* i *square*)))
	  :w (floor *square*)
	  :h (floor *square*))
	:color (sdl:color
			 :r (if (= 0 (aref *matrix* i j)) 200 255)
			 :g (if (= 0 (aref *matrix* i j)) 200 255)
			 :b (if (= 0 (aref *matrix* i j)) 200 0))))

(defun loop-life ()
  (dotimes (i *matrix_h*)
	(dotimes (j *matrix_w*)
	  (draw-life i j))))

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

(defun cycle ()
  (loop-life) ;Fonction d'affichage
  (sdl:update-display)
  (sdl:clear-display
	(sdl:color
	  :r 127
	  :g 127
	  :b 127))
  (walk) ;fonction calcul step + 1
  (god) ;fonction modif matrix step + 1
  )

(defun draw-a-box-in-window ()
  (sdl:with-init () 
				 (sdl:window *win-w* *win-h*)
				 (setf (sdl:frame-rate) 60)
				 (sdl:clear-display
				   (sdl:color
					 :r 127
					 :g 127
					 :b 127))
					(cycle)
				 (sdl:with-events ()
				   (:quit-event () t)
				   (:key-down-event ()
									(when (sdl:key-down-p :sdl-key-q)
									  (sdl:push-quit-event))))))


(defun main (argv &aux (argc (length argv)))
  (declare (ignore argc))
  (init-cells)
  (glider-gun 10 20)
  (draw-a-box-in-window)
  (sb-ext:exit)
  )

(sb-int:with-float-traps-masked
  (:invalid :inexact :overflow)
  (main *posix-argv*)
  )
