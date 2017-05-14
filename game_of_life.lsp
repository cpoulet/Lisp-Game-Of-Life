;;; game_of_life.lsp

;;; ============================================================== ;;;
;;; ================== GAME OF LIFE SIMULATION =================== ;;;
;;; ============================================================== ;;;

(ql:quickload 'lispbuilder-sdl)
(load "input.lsp")
;(load "~/.sbcl/quicklisp/setup.lisp")

(defparameter *matrix* nil)
(defparameter *matrix-h* 50)
(defparameter *matrix-w* 100)
(defparameter *matrix-x* 0)
(defparameter *matrix-y* 0)
(defparameter *win-w* 1500)
(defparameter *win-h* 800)
(defparameter *square* 10)
(defparameter *running* nil)
(defparameter *counter* 0)
(defparameter *slowdown* 10)
(defparameter *grille* 1)
(defparameter *vec-x* 0)
(defparameter *vec-y* 0)

(defun init-cells ()
  (setq *matrix* (make-array (list *matrix-h* *matrix-w*))))

(defun _blinker (i j)
  (setf (aref *matrix* i j) 1)
  (setf (aref *matrix* i (+ j 1)) 1)
  (setf (aref *matrix* i (+ j 2)) 1) t)

(defun blinker (i j)
  (if (or (>= i *matrix-h*) (>= (+ j 2) *matrix-w*) (< i 0) (< j 0))
	'nil
	(_blinker i j)))

(defun _block4 (i j)
  (setf (aref *matrix* i j) 1)
  (setf (aref *matrix* i (+ j 1)) 1)
  (setf (aref *matrix* (+ i 1) j) 1)
  (setf (aref *matrix* (+ i 1) (+ j 1)) 1) t)
(defun block4 (i j)
  (if (or (>= (+ i 1) *matrix-h*) (>= (+ j 1) *matrix-w*) (< i 0) (< j 0))
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
  (if (or (>= (+ i 8) *matrix-h*) (>= (+ j 35) *matrix-w*) (< i 0) (< j 0))
	'nil
	(_glider-gun i j)))

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
  (if (or (>= (+ i 3) *matrix-h*) (>= (+ j 4) *matrix-w*) (< i 0) (< j 0))
	'nil
	(_light-ship i j)))

(defun _glider (i j)
  (setf (aref *matrix* i (+ j 1)) 1)
  (setf (aref *matrix* (+ i 1) (+ j 2)) 1)
  (setf (aref *matrix* (+ i 2) j) 1)
  (setf (aref *matrix* (+ i 2) (+ j 1)) 1)
  (setf (aref *matrix* (+ i 2) (+ j 2)) 1) t)

(defun glider (i j)
  (if (or (>= (+ i 2) *matrix-h*) (>= (+ j 2) *matrix-w*) (< i 0) (< j 0))
	'nil
	(_glider i j)))

(defun draw-life (i j)
  (sdl:draw-box
	(sdl:rectangle
	  :x (floor (+ *matrix-y* (+ (* j *grille*) (* j *square*))))
	  :y (floor (+ *matrix-x* (+ (* i *grille*) (* i *square*))))
	  :w (floor *square*)
	  :h (floor *square*))
	:color (sdl:color
			 :r (if (= 0 (aref *matrix* i j)) 200 255)
			 :g (if (= 0 (aref *matrix* i j)) 200 255)
			 :b (if (= 0 (aref *matrix* i j)) 200 0))))

(defun loop-life ()
  (dotimes (i *matrix-h*)
	(dotimes (j *matrix-w*)
	  (draw-life i j))))

(defun resetgrid ()
  (dotimes (i *matrix-h*)
	(dotimes (j *matrix-w*)
	  (setf *matrix-x* 0)
	  (setf *matrix-y* 0)
	  (setf *square* 10)
	  (setf *running* nil)
	  (setf (aref *matrix* i j) 0))))

(defun get-val (i j)
  (if (or (< i 0) (< j 0) (>= i *matrix-h*) (>= j *matrix-w*))
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
  (if (= value 3)
	'0
	'1))

(defun god ()
  (dotimes (i *matrix-h*)
	(dotimes (j *matrix-w*)
	  (if (or (= 3 (aref *matrix* i j)) (= 2 (aref *matrix* i j)))
		(list (setf (aref *matrix* i j) (get-state (aref *matrix* i j))) (draw-life i j))))))
(defun walk ()
  (dotimes (i *matrix-h*)
	(dotimes (j *matrix-w*)
	  (setf (aref *matrix* i j) (life-rules (aref *matrix* i j) (get-neighbour i j))))) *matrix*)

(defun cycle ()
  (sdl:update-display)
  (if (>= *counter* *slowdown*)
	(progn
	  (setf *counter* 0)
	  (if *running*
		(progn
		  (walk)
		  (god))))
	)
  (incf *counter*)
  )

(defun size-x ()
  (+ (* *square* *matrix-h*) (* *grille* (- *matrix-h* 1))))

(defun size-y ()
  (+ (* *square* *matrix-w*) (* *grille* (- *matrix-w* 1))))

(defun get-i (x size)
  (floor (* (/ (- x *matrix-x*) size) *matrix-h*)))

(defun get-j (y size)
  (floor (* (/ (- y *matrix-y*) size) *matrix-w*)))

(defun touch (x y)
  (let ((i (get-i x (size-x)))(j (get-j y (size-y))))
	(if (and (< x (+ (size-x) *matrix-x*)) (> x *matrix-x*) (< y (+ (size-y) *matrix-y*)) (> y *matrix-y*))
	  (setf (aref *matrix* i j) (if (= 1 (aref *matrix* i j))
								  '0
								  '1))
	  'nil)))

(defun zoom()
  (if (>= *square* 40)
	'0
	(setf *square* (+ *square* 1))))

(defun unzoom()
  (if (<= *square* 2)
	'0
	(setf *square* (- *square* 1))))

(defun speedup()
  (if (< 0 *slowdown*)
	(decf *slowdown*)))

(defun slowdown()
  (if (> 150 *slowdown*)
	(incf *slowdown*)))

(defun zoom_mouse(mx my)
  (let ((sx (size-x))(sy (size-y)))
	(zoom)
	(let ((nsx (size-x))(nsy (size-y)))
	  (setf *matrix-y* (floor(- mx (* nsx (/ sx (- mx *matrix-y*))))))
	  (setf *matrix-x* (floor(- my (* nsy (/ sy (- my *matrix-x*))))))))
  (sdl:clear-display
	(sdl:color
	  :r 127
	  :g 127
	  :b 127))
  (loop-life))

(defun unzoom_mouse(mx my)
  (let ((sx (size-x))(sy (size-y)))
	(unzoom)
	(let ((nsx (size-x))(nsy (size-y)))
	  (setf *matrix-y* (floor(- mx (* nsx (/ sx (- mx *matrix-y*))))))
	  (setf *matrix-x* (floor(- my (* nsy (/ sy (- my *matrix-x*))))))))
  (sdl:clear-display
	(sdl:color
	  :r 127
	  :g 127
	  :b 127))
  (loop-life))

(defun draw-a-box-in-window ()
  (sdl:with-init ()
				 (sdl:window *win-w* *win-h*)
				 (setf (sdl:frame-rate) 60)
				 (sdl:clear-display
				   (sdl:color
					 :r 127
					 :g 127
					 :b 127))
				 (loop-life)
				 (sdl:with-events ()
								  (:quit-event () t)
								  (:key-down-event ()
												   (when (or (sdl:key-down-p :sdl-key-q) (sdl:key-down-p :sdl-key-escape))
													 (sdl:push-quit-event))
												   (when (or (sdl:key-down-p :sdl-key-down) (sdl:key-down-p :sdl-key-s))
													 (setf *matrix-x* (+ *matrix-x* *square*)))
												   (when (or (sdl:key-down-p :sdl-key-up) (sdl:key-down-p :sdl-key-w))
													 (setf *matrix-x* (- *matrix-x* *square*)))
												   (when (or (sdl:key-down-p :sdl-key-left) (sdl:key-down-p :sdl-key-a))
													 (setf *matrix-y* (- *matrix-y* *square*)))
												   (when (or (sdl:key-down-p :sdl-key-right) (sdl:key-down-p :sdl-key-d))
													 (setf *matrix-y* (+ *matrix-y* *square*)))
												   (when (sdl:key-down-p :sdl-key-p)
													 (if *running*
													   (setf *running* nil)
													   (setf *running* t)))
												   (when (sdl:key-down-p :sdl-key-comma)
													 (slowdown))
												   (when (sdl:key-down-p :sdl-key-period)
													 (speedup))
												   (when (sdl:key-down-p :sdl-key-kp-plus)
													 (progn (zoom_mouse (sdl:mouse-x) (sdl:mouse-y)) (zoom_mouse (sdl:mouse-x) (sdl:mouse-y))))
												   (when (sdl:key-down-p :sdl-key-kp-minus)
													 (progn (unzoom_mouse (sdl:mouse-x) (sdl:mouse-y)) (unzoom_mouse (sdl:mouse-x) (sdl:mouse-y))))
												   (when (sdl:key-down-p :sdl-key-r)
													 (resetgrid))
												   (when (sdl:key-down-p :sdl-key-g)
													 (setf *grille* (- 1 *grille*))
													 (if (= *grille* 1) (setf *square*  (- *square* 1)) (setf *square* (+ *square* 1))))
												   (sdl:clear-display
													 (sdl:color
													   :r 127
													   :g 127
													   :b 127))
												   (loop-life))
								  (:mouse-button-up-event (:button button :x mouse-x :y mouse-y)
														  (if (and (= button 1) (not  (sdl:key-down-p :sdl-key-lshift)))
															(touch mouse-y mouse-x))
														  (if (= button 4)
															(if (or (sdl:key-down-p :sdl-key-lshift) (sdl:key-down-p :sdl-key-rshift))
															  (speedup)
															  (progn (unzoom_mouse mouse-x mouse-y) (unzoom_mouse mouse-x mouse-y))))
														  (if (= button 5)
															(if (or (sdl:key-down-p :sdl-key-lshift) (sdl:key-down-p :sdl-key-rshift))
															  (slowdown)
															  (progn (zoom_mouse mouse-x mouse-y) (zoom_mouse mouse-x mouse-y))))
														  (sdl:clear-display
															(sdl:color
															  :r 127
															  :g 127
															  :b 127))
														  (loop-life))
								  (:idle ()
										 (when (and (sdl:mouse-left-p) (or (sdl:key-down-p :sdl-key-lshift) (sdl:key-down-p :sdl-key-rshift)))
										   (let ((relative-pos (sdl:mouse-relative-position)))
											 (setf *matrix-x* (+ *matrix-x* (aref relative-pos 1)))
											 (setf *matrix-y* (+ *matrix-y* (aref relative-pos 0)))
											 )
										   (sdl:clear-display
											 (sdl:color
											   :r 127
											   :g 127
											   :b 127))
										   (loop-life))
										 (cycle)
										 ))))

(defun main (argv)
  (program-input-parse argv)
  (setf *matrix-w* (parse-integer (nth 1 argv)))
  (setf *matrix-h* (parse-integer (nth 2 argv)))
  (init-cells)
  (glider-gun 10 20)
  (draw-a-box-in-window)
  (sb-ext:exit)
  )

(sb-int:with-float-traps-masked
  (:invalid :inexact :overflow)
  (main *posix-argv*)
  )
