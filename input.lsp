;******************************************************************************;
;                                                                              ;
;                                                         :::      ::::::::    ;
;    input.lisp                                         :+:      :+:    :+:    ;
;                                                     +:+ +:+         +:+      ;
;    By: aguellil <aguellil@student.42.fr>          +#+  +:+       +#+         ;
;                                                 +#+#+#+#+#+   +#+            ;
;    Created: 2017/05/14 07:22:20 by aguellil          #+#    #+#              ;
;    Updated: 2017/05/14 07:22:22 by aguellil         ###   ########.fr        ;
;                                                                              ;
;******************************************************************************;

;;;;;;;;;;;;;;;;;
; Program input ;
;;;;;;;;;;;;;;;;;

(defun program-input-parse (argv)
	(let ((argc (length argv)))
	   	(if (not (= argc 3))
			(list 
			 (format t "usage: ~A --load width height~%" (nth 0 argv))
			 (format t "~Cwidth: width of the grid~%" #\tab)
			 (format t "~Cheight: height of the grid~%" #\tab)
			 (exit)
			)
		)
	)
)
