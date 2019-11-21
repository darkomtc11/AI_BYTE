(defvar *letters* '(A B C D E F G H I J K L M N O P Q R S T U))
(defvar *allowed1* '(1 3 5 7 9 11 13 15 17 19))
(defvar *allowed2* '(2 4 6 8 10 12 14 16 18 20))

(defstruct node state (value 'HV) (children '()))

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
        ((= n 0) (write-string " "))
        (T 
            (writeNumbers (- n 1))
            (write-string (format nil "~4D   " n))
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
                        (write-string "       ")
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
        ((null checker) (write-string "- "))
        (T (write checker) (write-string " "))
    )
)

(defun checkSquare (row column state)
    "Checks is given row/column returns valid square."
    (not (null (assoc column (cadr (assoc row state)))))
)

(defun getSquareStack (row column state)
    "Returns square stack on given row/column."
    (cadr (assoc column (cadr (assoc row state))))
)

(defun getSquare (row column state)
    "Returns square on given row/column alongside column key."
    (assoc column (cadr (assoc row state)))
)

(defun moveStack (sRow sColumn dRow dColumn level state)
    "Moves one stack to another or to an empty place. Takes top of stack by level."
    (let ((newState (copy-tree state)))
        (cond 
            ((and (checkSquare sRow sColumn newState) (checkSquare dRow dColumn newState))
                (let* ((src (getSquareStack sRow sColumn newState)) (dest (getSquareStack dRow dColumn newState)) (list (splitStack dest src level)) (sDest (car list)) (sSrc (cadr list)))
                    ;; (print sSrc)
                    ;; (print sDest)
                    (setf (cadr (getSquare sRow sColumn newState)) sSrc)
                    (setf (cadr (getSquare dRow dColumn newState)) sDest)
                )
            )
            ;; (T (write-string "Invalid move"))
        )
        newState
    )
)

(defun splitStack (stay move count)
    "Splits 'move' and moves top part to 'stay'"
    (list (append stay (nthcdr (- count 1) move)) (subseq move 0 (- count 1)))
)

(defun playMove (state)
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
                    (write-line "Enter source level:")
                    (let ((level (read )))
                        (moveStack sRow sColumn dRow dColumn level state)
                    )
                )
            )
        )
    )
)

(defun isQuasiTerminal (node)
    "Checks if node is quasi terminal (no children)."
    (null (node-children node))
)

(defun generatePossibleOutcomes (state player)
    "Generates possible outcomes (next states) by state and player."
    (cond 
        ((= player 0)
            (let ((outcomes (list 
                (moveStack 'G 1 'F 2 1 state) 
                (moveStack 'G 3 'F 4 1 state) 
                (moveStack 'G 3 'F 4 1 state) 
                (moveStack 'G 3 'F 4 1 state) 
                (moveStack 'G 3 'F 4 1 state) 
                (moveStack 'G 3 'F 4 1 state) 
                (moveStack 'G 3 'F 4 1 state) 
                (moveStack 'G 3 'F 4 1 state) 
                (moveStack 'G 3 'F 4 1 state) 
                (moveStack 'G 3 'F 4 1 state) 
                (moveStack 'G 3 'F 4 1 state) 
                (moveStack 'G 3 'F 4 1 state) 
                (moveStack 'G 3 'F 4 1 state) 
                (moveStack 'G 3 'F 4 1 state) 
                (moveStack 'G 3 'F 4 1 state) 
                (moveStack 'G 3 'F 4 1 state) 
                (moveStack 'G 3 'F 4 1 state) 
                (moveStack 'G 3 'F 4 1 state) 
                (moveStack 'G 3 'F 4 1 state) 
                (moveStack 'G 3 'F 4 1 state) 
                (moveStack 'G 3 'F 4 1 state) 
                (moveStack 'G 3 'F 4 1 state) 
                (moveStack 'G 3 'F 4 1 state) 
                (moveStack 'G 3 'F 4 1 state)
                (moveStack 'G 3 'F 4 1 state) 
                (moveStack 'G 3 'F 4 1 state) 
                (moveStack 'G 3 'F 4 1 state) 
                (moveStack 'G 3 'F 4 1 state) 
                (moveStack 'G 3 'F 4 1 state) 
                (moveStack 'G 3 'F 4 1 state)
                (moveStack 'G 3 'F 4 1 state) 
                (moveStack 'G 3 'F 4 1 state) 
                (moveStack 'G 3 'F 4 1 state) 
                (moveStack 'G 3 'F 4 1 state) 
                (moveStack 'G 3 'F 4 1 state) 
                ;; (moveStack 'G 3 'F 4 1 state) 
                ;; (moveStack 'G 3 'F 4 1 state) 
                ;; (moveStack 'G 3 'F 4 1 state) 
                ;; (moveStack 'G 3 'F 4 1 state) 
                ;; (moveStack 'G 3 'F 4 1 state) 
                ;; (moveStack 'G 3 'F 4 1 state) 
                ;; (moveStack 'G 3 'F 4 1 state) 
                ;; (moveStack 'G 3 'F 4 1 state) 
                ;; (moveStack 'G 3 'F 4 1 state) 
                ;; (moveStack 'G 3 'F 4 1 state) 
                ;; (moveStack 'G 3 'F 4 1 state) 
                ;; (moveStack 'G 3 'F 4 1 state) 
                ;; (moveStack 'G 3 'F 4 1 state) 
                ;; (moveStack 'G 3 'F 4 1 state) 
                ;; (moveStack 'G 3 'F 4 1 state) 
                ;; (moveStack 'G 3 'F 4 1 state) 
                ;; (moveStack 'G 3 'F 4 1 state) 
                ;; (moveStack 'G 3 'F 4 1 state) 
                ;; (moveStack 'G 3 'F 4 1 state) 
                ;; (moveStack 'G 3 'F 4 1 state) 
                ;; za field size 2, depth 4

            )))
                (setq *numOfNodes* (+ *numOfNodes* (length outcomes)))
                outcomes
            )
            
            
        )
        ((= player 1)
            (list (moveStack 'B 8 'C 7 1 state) (moveStack 'B 6 'C 5 1 state))
        )
    )
)

(defun outcomesToNodes (list)
    "Maps outcomes (list of states) to list of Nodes."
    (cond 
        ((null list) ())
        (T (cons (make-node :state (car list)) (outcomesToNodes (cdr list))))
    )
)

(defun deepen (list n)
    (cond 
        ((or (null list) (= n 0)) ())
        ((listp list)
            (deepen (car list) n) (deepen (cdr list) n)
        )        
        ((node-p list)
            (cond 
                ((isQuasiTerminal list)
                    ;; (setf (node-children list) (outcomesToNodes (generatePossibleOutcomes (node-state list) 0)))
                    (cond 
                        ((= n 0) list)
                        ;; (T (deepen (node-children list) (- n 1)))
                        (T (deepen (outcomesToNodes (generatePossibleOutcomes (node-state list) 0)) (- n 1)))
                    )
                )
                (T
                    (deepen (node-children list) n)
                )
            )
        )
    )
)

(defun printPretty (node indent last)
    (write-line "")
    (write-string indent)
    (cond 
        (last (write-string "L ") (setq indent (concatenate 'string indent "     ")))
        (T
            (write-string "|-") (setq indent (concatenate 'string indent "|    "))
        )
    )
    
    (write (node-state node))    
    (let* ((n (- (length (node-children node)) 1)))
        (loop for i from 0 to n do
            (let ((child (nth i (node-children node))))
                (printPretty child indent (= i n))
            )
        )
    )   
)

(defun main ()

    (write-line "")
    (write-line "Enter field size:")

    (setq *numOfNodes* 1)
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
                (printBoard *state* T)
                (setq *state* (playMove *state*))
            )
            ((= action 4)           
                ;; (setq root (make-node :state *state* :value 'HV :children '()))
                (write-line "Depth:")
                (let ((num (read )))
                    (time 
                        ;; (deepen (list root) num)
                        (deepen (list (make-node :state *state* :value 'HV :children '())) num)
                    )
                    (print *numOfNodes*)
                    (setq *numOfNodes* 1)
                    ;; (printPretty root "" '())
                    ;; (room )
                )
                
            
            )
            ((= action 5)
                ;; (setq outcomes (generatePossibleOutcomes *state* 0))
                ;; (print outcomes)
                ;; (setq nodes (generateChildren outcomes))
                ;; (print nodes)
                ;; (printBoard (node-state (nth 0 nodes)) T)
                ;; (printBoard (node-state (nth 1 nodes)) T)
                (room )
            )
            (T (write-line "Please choose correct action."))

            
        )
        (write-line "")
        (write-line "Choose another action:")
        (write-line "")
        (setq action (read ))
    )
)

(main )