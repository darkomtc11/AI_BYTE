(defvar *letters* '(A B C D E F G H I J K L M N O P Q R S T U))
(defvar *allowed1* '(1 3 5 7 9 11 13 15 17 19))
(defvar *allowed2* '(2 4 6 8 10 12 14 16 18 20))

(defun generatePlayingField (current)
    "Generates whole playing field by generating rows."
    (cond 
        ((= current 0) ())
        (T
            (cons (generateRow current) (generatePlayingField (- current 1)))
        )
    )
)

(defun generateRow (rowIndex)
    "Initiates full row stack generation."
    (let ((index (- *n* rowIndex)) (size (/ *n* 2)) (type (mod rowIndex 2)))
        (cons (nth index *letters*) 
            (list (generateStacks type size
                (cond 
                    ((or (= rowIndex 1) (= rowIndex *n*))
                        ()
                    )
                    ((= type 0)
                        '(X)
                    )
                    (T
                        '(O)
                    )
                )
            ))
        )
    )
)

(defun generateStacks (type curr data)
    "Generates all stacks of one row based on type (odd/even) and puts given data on stack."
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
    "Prints column numbers."
    (cond 
        ((= n 0) (write-string ""))
        (T 
            (writeNumbers (- n 1))
            (write-string (format nil "~4D" n))
        )
    )
)

(defun printBoard (field first)
    "Initiaces full board printing"
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
    "Prints row in three parts. Calls it self 2 times with different type."
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

(defun getSquareStack (row column)
    "Returns square stack on given row/column."
    (cadr (assoc column (cadr (assoc row *state*))))
)

(defun getSquare (row column)
    "Returns square on given row/column alongside column key."
    (assoc column (cadr (assoc row *state*)))
)

(defun moveStack (sRow sColumn dRow dColumn level)
    "Moves one stack to another or to an empty place. Takes top of stack by level."
    (cond 
        ((and (checkSquare sRow sColumn) (checkSquare dRow dColumn))
            (let* ((src (getSquareStack sRow sColumn)) (dest (getSquareStack dRow dColumn)) (list (splitStack dest src level)) (sDest (car list)) (sSrc (cadr list)))
                (print sSrc)
                (print sDest)
                (setf (cadr (getSquare sRow sColumn)) sSrc)
                (setf (cadr (getSquare dRow dColumn)) sDest)
            )
        )
        (T (write-string "Invalid move"))
    )
)

(defun splitStack (stay move count)
    "Splits 'move' and moves top part it to 'stay'"
    (list (append stay (nthcdr (- count 1) move)) (subseq move 0 (- count 1)))
)

(defun playMove ()
    "Allows player to choose a valid move to play and returns a new game state."
    (write-line "")
    (write-line "Enter source row (A B C ...):")
    (let ((sRow (read )))
        (write-line "Enter source column (1 2 3...):")
        (let ((sColumn (read )))
            (write-line "Enter destination row (A B C ...):")
                (let ((dRow (read )))
                    (write-line "Enter destination column (1 2 3...):")
                    (let ((dColumn (read )))
                        (moveStack sRow sColumn dRow dColumn 1)
                    )
                )
        )
    )
)

(write-line "")
(write-line "Enter field size:")

(defvar *n* (read ))
(defvar *state* (generatePlayingField *n*))
(defvar *player* 0)

(write-string "1: print board, 2: write state, 3: play a move, -1: exit")
(write-line "")
(setq action (read ))
(loop while (not (<= action -1)) do
    (cond 
        ((= action 1)
            (write-line "")
            (printBoard *state* T)
        )
        ((= action 2)
            (write-line "")
            (write *state*)
        )
        ((= action 3)
            (playMove )
        )
        ((= action 4)
            (setf test '(1 2 3))
            (print test)
        )
        (T (write-line "Please choose correct action."))
    )
    (write-line "")
    (write-line "Choose another action:")
    (write-line "")
    (setq action (read ))
)