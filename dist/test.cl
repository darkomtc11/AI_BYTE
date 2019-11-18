;; (defun getStack (row column)
;;     (setq key (concatenate 'string row (write-to-string column)))
;;     (cadr (assoc key flowers :test #'equalp))
;; )

;; ('((a (b)) (b (c)) (c (a b))) a dubina) => '((b 2) (c 2))


(defun getDepths (depth n)
    (cond ((= n 0)())
    (T (cons (list depth) (getDepths depth (- n 1))))
    )
    
)

(defun formirajPoDubini (graph node depth processed siblings)

    ;; (print graph)
    
    ;; (print children)
    ;; (print node)
        (cond 
            ((null node) ())
            ((find node processed) ())
            (T
                (let* (
                    (new (cadr (assoc node graph))) 
                    (children (append (mapcar #'cons new (getDepths depth (length new))) siblings)))
                        (cond 
                            ((null children) (list (cons node (list depth))))
                            (T
                                (cons (cons node (list depth)) (formirajPoDubini graph (caar children) (+ (car (cdar children)) 1) (cons node processed) (cdr children)))
                            )   
                        )
                )
            )
        )
    )
(setq graf1 '((a (b c)) (b (e)) (c (f d b)) (d (g)) (e ()) (f (c)) (g ()))) 

(print (formirajPoDubini graf1 'a 0 '() '()))


;; (list (cons node (list depth)))