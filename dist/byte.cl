(defvar *letters* '(A B C D E F G H I J))
(defvar *allowed1* '(1 3 5 7 9))
(defvar *allowed2* '(2 4 6 8 10))

(defun generateState ()
    (generateField *n*)
)

(defun generateField (current)
    (cond 
        ((= current 0) ())
        (T
            (cons (generateRow current) (generateField (- current 1)))
        )
    )
)

(defun generateRow (type)
    (let ((index (- *n* type)) (size (/ *n* 2)))
        (cond 
            ((or (= type 1) (= type *n*))
                (cons (nth index *letters*) (list (generateStacks type size ())))
            )
            ((= (mod type 2) 0)
                (cons (nth index *letters*) (list (generateStacks type size '(O))))
            )
            (T
                (cons (nth index *letters*) (list (generateStacks type size '(X))))
            )
        )
    )
)

(defun generateStacks (type curr data)
    (let ((index (- (/ *n* 2) curr)) (next (- curr 1)))
        (cond 
            ((= curr 0) ())
            ((= (mod type 2) 0)
                (cons (cons (nth index *allowed1*) (list data)) (generateStacks type next data))
            )
            (T (cons (cons (nth index *allowed2*) (list data)) (generateStacks type next data)))
        )
    )
)

(defun writeNumbers (n)
    (cond 
        ((= n 0) (write-string ""))
        (T 
            (writeNumbers (- n 1))
            (write-string "   ")
            (write n)
        )
    )
)

(defun printBoard (field first)
    (cond 
        ((= (length field) 0) ())
        (first 
            (writeNumbers (length field))
            (write-line "")
            (printBoard field ())
        )
        (T 
            (printRow (car field) 1 *n*)
            (write-line "")
            (printBoard (cdr field) ())
        )
    )
)

(defun printRow (row type curr)
    "Prints row in three parts."
    (cond 
        ((= curr -1)
            
            (cond 
                ((= type 3) ())
                (T
                    (write-line "")
                    (printRow row (+ type 1) *n*)
                )
            )
        )
        (T
            (if (and (= type 2) (= curr *n*)) 
                (write (car row))

                (if (= curr *n*) 
                    (write-string " ")
                )
            )
           
            (let ((square (assoc (+ (- *n* curr) 1) (cadr row))))
                (cond 
                    ((null square)
                        (write-string "    ")
                    )
                    (T
                        (write-string " ")
                        (printSquare square type)
                    )
                )
            )
            
            (printRow row type (- curr 1))
        )
    )
)

(defun printSquare (square part)
    "Prints one third of stacked checkers on given square."
    (cond 
        ((= part 1)
            (printChecker (nth 8 (cadr square)))
            (printChecker (nth 7 (cadr square)))
            (printChecker (nth 6 (cadr square)))
        )
        ((= part 2)
            (printChecker (nth 5 (cadr square)))
            (printChecker (nth 4 (cadr square)))
            (printChecker (nth 3 (cadr square)))
        )
        ((= part 3)
            (printChecker (nth 2 (cadr square)))
            (printChecker (nth 1 (cadr square)))
            (printChecker (nth 0 (cadr square)))
        )
    )
)

(defun printChecker (checker)
    "Prints single checker if it exists."
    (cond 
        ((null checker) (write-string "-"))
        (T (write checker))
    )
)

(defun checkSquare (row column)
    "Checks is given row/column returns valid square."
    (not (null (assoc column (cadr (assoc row *state*)))))
)

(defun checkSquare (row column)
    "Returns square stack on given row/column."
    (cadr (assoc column (cadr (assoc row *state*))))
)

(defun playMove ()
    "Allows player to choose a valid move to play and returns a new game state."
    (write-line "")
    (write-line "Enter row:")
    (let ((row (read )))
        (write-line "Enter column:")
        (let ((column (read )))
            (if (checkSquare row column)
                (progn (write row) (write column) (write-string ":    ") (write (checkSquare row column)))
                (write-string "Not valid square!")
            )
        )
    )
)

(write-line "")
(write-line "Enter field size:")

(defvar *n* (read ))
(defvar *state* (generateState ))

(write-string "1: print board, 2: write state, 3: play a move, -1: exit")
(write-line "")
(setq action (read ))
(loop while (not (<= action -1)) do
    (cond 
        ((= action 1)
            (printBoard *state* T)
        )
        ((= action 2)
            (write *state*)
        )
        ((= action 3)
            (playMove )
        )
        (T (write-line "Please choose correct action."))
    )
    (write-line "")
    (write-line "Choose another action:")
    (write-line "")
    (setq action (read ))
)