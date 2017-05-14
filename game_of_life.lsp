;;; game_of_life.lsp

;;; ============================================================== ;;;
;;; ================== GAME OF LIFE SIMULATION =================== ;;;
;;; ============================================================== ;;;

(ql:quickload 'lispbuilder-sdl)

(defparameter *matrix* nil)
(defparameter *matrix_h* 50)
(defparameter *matrix_w* 100)
(defparameter *matrix-x* 0)
(defparameter *matrix-y* 0)
(defparameter *win-w* 1500)
(defparameter *win-h* 800)
(defparameter *square* 10)
(defparameter *running* nil)
(defparameter *counter* 0)
(defparameter *slowdown* 10)
(defparameter *grille* 1)

(defun init-cells ()
  (setq *matrix* (make-array (list *matrix_h* *matrix_w*))))

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
  (dotimes (i *matrix_h*)
	(dotimes (j *matrix_w*)
	  (draw-life i j))))

(defun resetgrid ()
  (dotimes (i *matrix_h*)
	(dotimes (j *matrix_w*)
	  (setf *matrix-x* 0)
	  (setf *matrix-y* 0)
	  (setf *square* 10)
	  (setf *running* nil)
	  (setf (aref *matrix* i j) 0))))

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
	(+ value 0) ;could be modified
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
  (loop-life)
  (sdl:update-display)
  (sdl:clear-display
   (sdl:color
    :r 127
    :g 127
    :b 127))
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
  (+ (* *square* *matrix_h*) (* *grille* (- *matrix_h* 1))))

(defun size-y ()
  (+ (* *square* *matrix_w*) (* *grille* (- *matrix_w* 1))))

(defun get-i (x size)
  (floor (* (/ (- x *matrix-x*) size) *matrix_h*)))

(defun get-j (y size)
  (floor (* (/ (- y *matrix-y*) size) *matrix_w*)))

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
  (if (> 50 *slowdown*)
    (incf *slowdown*)))

(defun zoom_mouse(mx my)
  (let ((sx (size-x))(sy (size-y)))
	(zoom)
  	(let ((nsx (size-x))(nsy (size-y)))
	  (setf *matrix-y* (floor(- mx (* nsx (/ sx (- mx *matrix-y*))))))
	  (setf *matrix-x* (floor(- my (* nsy (/ sy (- my *matrix-x*)))))))))
	
(defun unzoom_mouse(mx my)
  (let ((sx (size-x))(sy (size-y)))
	(unzoom)
  	(let ((nsx (size-x))(nsy (size-y)))
	  (setf *matrix-y* (floor(- mx (* nsx (/ sx (- mx *matrix-y*))))))
	  (setf *matrix-x* (floor(- my (* nsy (/ sy (- my *matrix-x*)))))))))

  (defun draw-a-box-in-window ()
  (sdl:with-init ()
    (sdl:window *win-w* *win-h*)
    (setf (sdl:frame-rate) 60)
    (sdl:clear-display
     (sdl:color
      :r 127
      :g 127
      :b 127))
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
		 (if (= *grille* 1) (setf *square*  (- *square* 1)) (setf *square* (+ *square* 1)))))
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
		  		(progn (zoom_mouse mouse-x mouse-y) (zoom_mouse mouse-x mouse-y)))))
      (:idle ()
			(when (and (sdl:mouse-left-p) (or (sdl:key-down-p :sdl-key-lshift) (sdl:key-down-p :sdl-key-rshift)))
               (let ((relative-pos (sdl:mouse-relative-position)))
                 (setf *matrix-x* (+ *matrix-x* (aref relative-pos 1)))
                 (setf *matrix-y* (+ *matrix-y* (aref relative-pos 0)))
                 ))
             (cycle)
             ))))

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
