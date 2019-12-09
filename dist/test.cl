;; (defun getStack (row column)
;;     (setq key (concatenate 'string row (write-to-string column)))
;;     (cadr (assoc key flowers :test #'equalp))
;; )

;; ('((a (b)) (b (c)) (c (a b))) a dubina) => '((b 2) (c 2))


;; (defun getDepths (depth n)
;;     (cond ((= n 0)())
;;     (T (cons (list depth) (getDepths depth (- n 1))))
;;     )
    
;; )

;; (defun formirajPoDubini (graph node depth processed siblings)
;;     (cond 
;;         ((null node) ())
;;         (T
;;             (let* ((new (cadr (assoc node graph))) (reducednew (set-difference new processed)) (children (append (mapcar #'cons reducednew (getDepths depth (length new))) siblings)))
;;                 ;; (print reducednew)
;;                 (cond 
;;                     ((null children) (list (cons node (list depth))))
;;                     ((find node processed) ())

;;                     (T
;;                         (cons (cons node (list depth)) (formirajPoDubini graph (caar children) (+ (car (cdar children)) 1) (cons node processed) (cdr children)))
;;                     )   
;;                 )
;;             )
;;         )
;;     )
;; )
;; (setq graf1 '((a (b c)) (b (e)) (c (f d b)) (d (g)) (e ()) (f (c)) (g ()))) 
;; (trace formirajPoDubini)
;; (print (formirajPoDubini graf1 'a 0 '() '()))


;; (list (cons node (list depth)))

(defstruct node state (children '()))
(defvar state '((1 4) (2 3) (3 4) (4 4) (5 8) (6 7) (7 3) (8 2)))

(defun generateChild (state n type)
    (let ((newState (copy-tree state)))
        (cond 
            ((= type 1)
                (setf (cadr (assoc n newState)) (+ (cadr (assoc n newState)) 1))
            )
            (T
                (setf (cadr (assoc n newState)) (- (cadr (assoc n newState)) 1))
            )
        )
        (make-node :state newState)
    )
)

(defun generateChildren (state n type)
    (cond 
        ((= (length state) (- n 1)) ())
        ((= type 1) (cons (list (generateChild state n 1)) (generateChildren state n (+ type 1))))
        ((= type 2) (cons (list (generateChild state n 2)) (generateChildren state (+ n 1) 1)))
    )
)

(defun deepen (nodelist)
    (cond 
        ((null nodelist) ())
        (T 
            (setf (node-children (car nodelist)) (generateChildren (node-state (car nodelist)) 1 1))
            (append (node-children (car nodelist)) (deepen (cdr nodelist)))
        )
    )
)

(print state)
(setq root (generateChild state 1 2))
;; (setf (node-children root) (generateChildren state 1 1))
(deepen (list root))
(print root)