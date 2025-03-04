(defconstant *numberByLetter* '((A 1) (B 2) (C 3) (D 4) (E 5) (F 6) (G 7) (H 8) (I 9) (J 10) (S -1)))
(defconstant *letterByNumber* '((1 A) (2 B) (3 C) (4 D) (5 E) (6 F) (7 G) (8 H) (9 I) (10 J)))

(defstruct node environment (value 'HV) (children '()))
(defstruct environment state (stackWinners '()) (checker 'X) (xPositions '()) (oPositions '()))

(defvar DEV-nodeCount 1) ;; help variable for development

(defun generatePlayingField (current n)
    ;; Generates whole playing field by generating rows."
    (cond 
        ((= current 0) ())
        (T
            (append (generatePlayingField (- current 1) n) (list (generateRow current n)))
        )
    )
)

(defun generateRow (rowIndex n)
    ;; Initiates full row stack generation."
    (let* (
    (type (mod rowIndex 2)) 
    (start (- n type))
    )
        (cons 
            rowIndex
            (list (generateStacks start
                (cond 
                    ((or (= rowIndex 1) (= rowIndex n))
                        ()
                    )
                    ((= type 1)
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

(defun generateStacks (curr data)
    ;; Generates all stacks of one row based on type (odd/even) and puts given data on stack."
    (cond 
        ((<= curr 0) ())
        (T (append (generateStacks (- curr 2) data) (list (cons curr (list data)))))
    )
)

(defun initializeAllPositions (nonEmpty env)
    ;; Initializes all help position variables in environment (X and O positions)
    (cond 
        ((null nonEmpty) '())
        (T
            (if (= (mod (caar nonEmpty) 2) 0)
                (initializePosition 'O (car nonEmpty) '(0) env)
                (initializePosition 'X (car nonEmpty) '(0) env)
            )
            (initializeAllPositions (cdr nonEmpty) env)
        )
    )
)


(defun initializePosition (checker rcPair levels env)
    ;; Initializes single position in help variables
    (let ((positions (if (equal checker 'X) (environment-xPositions env) (environment-oPositions env))))
        (cond 
            ((equal checker 'X)
                (setf (environment-xPositions env) (append positions (list (list rcPair levels))))
            )
            (T 
                (setf (environment-oPositions env) (append positions (list (list rcPair levels))))
            )
        )
    )
)

(defun setPosition (checker rcPair levels env)
    ;; Changes single position in help variable
    (let* ((positions (if (equal checker 'X) (environment-xPositions env) (environment-oPositions env))) (pos (assoc rcPair positions :test #'equal)))
        (cond 
            ((null pos) 
                (initializePosition checker rcPair levels env)
            )
            (T
                (setf (cdr pos) (list levels))
            )
        )
    )
)

(defun removePosition (checker rcPair env)
    ;; Removes all positions in help variable on givem rcPair
    (let* (
    (positions (if (equal checker 'X) (environment-xPositions env) (environment-oPositions env)))
    (pos (assoc rcPair positions :test #'equal))
    )
        (setf (if (equal checker 'X) (environment-xPositions env) (environment-oPositions env)) (remove pos positions))
    )
)

(defun getLevels (stack checker)
    ;; Gets all indexes of a given checker in stack
    (indexes (copy-tree stack) checker)
)

(defun indexes (list e)
    ;; Finds indexes of e in list
    (cond 
        ((= (frequency e list) 0) '())
        (T
            (let* ((pos (position e list)))
                (setf (nth pos list) 'Y)
                (cons pos (indexes list e))
            )
        )
    )
)

(defun writeNumbers (n)
    ;; Prints column numbers."
    (cond 
        ((= n 0) (write-string " "))
        (T 
            (writeNumbers (- n 1))
            (write-string (format nil "~4D   " n))
        )
    )
)

(defun printBoard (field first n)
    ;; Initiaces full board printing"
    (cond 
        ((= (length field) 0) ())
        (first 
            (writeNumbers (length field))
            (write-line "")
            (printBoard field () n)
        )
        (T 
            (printRow (car field) 1 n n)
            (write-line "")
            (printBoard (cdr field) () n)
        )
    )
)

(defun printRow (row type curr n)
    ;; Prints row in three parts. Calls it self 2 times with different type."
    (cond 
        ((= curr -1)
            (cond 
                ((= type 3) ())
                (T
                    (write-line "")
                    (printRow row (+ type 1) n n)
                )
            )
        )
        (T
            (if (and (= type 2) (= curr n)) 
                (write (cadr (assoc (car row) *letterByNumber*)))

                (if (= curr n) 
                    (write-string " ")
                )
            )
           
            (let ((square (assoc (+ (- n curr) 1) (cadr row))))
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
            
            (printRow row type (- curr 1) n)
        )
    )
)

(defun printSquare (square part)
    ;; Prints one third of stacked checkers on given square."
    ;; "@param {(X O ...)} square: list of checkers"
    ;; "@param {int} part: top, middle or bottom 3"
    (printChecker (nth (-  (-  8 (*  (-  part 1) 3)) 0) (cadr square)))
    (printChecker (nth (-  (-  8 (*  (-  part 1) 3)) 1) (cadr square)))
    (printChecker (nth (-  (-  8 (*  (-  part 1) 3)) 2) (cadr square)))
)

(defun printChecker (checker)
    ;; Prints checkers one by one."
    ;; "@param {X, O or NULL} checker: checker"
    (cond 
        ((null checker) (write-string "- "))
        (T (write checker) (write-string " "))
    )
)

(defun getSquare (row column state)
    ;; Returns square on given row/column alongside column key."
    ;; "@param {int} row: row"
    ;; "@param {int} column: column"
    ;; "@param {( (row (col ())) )} state: memory representation of the game board"
    (assoc column (cadr (assoc row state)))
)

(defun getStack (row column state)
    ;; Returns stack on given row/column."
    ;; "@param {int} row: row"
    ;; "@param {int} column: column"
    ;; "@param {( (row (col ())) )} state: memory representation of the game board"
    (cadr (getSquare row column state))
)

(defun getRowNonEmpty (rowIndex row)
    ;; Returns all non empty squares in one row. (row column)"
    ;; "@param {int} rowIndex: row index"
    ;; "@param {( (col (X O ...)) )} row: list of valid squares in a row"
    (cond 
        ((null row) ())
        ((not (null (cadar row))) (cons (list rowIndex (caar row)) (getRowNonEmpty rowIndex (cdr row))))
        (T (getRowNonEmpty rowIndex (cdr row)))
    )
)

(defun getNonEmtpy (state)
    ;; Returns non empty squares. List of (row column)."
    ;; "@param {( (row (col ())) )} state: memory representation of the game board"
    (cond 
        ((null state) ())
        (T (append (getRowNonEmpty (caar state) (cadar state)) (getNonEmtpy (cdr state))))
    )
)

(defun distance (row col oRow oCol iRow iCol)
    ;; Returns distance between 2 stacks, unless it's second stack is ignored stack."
    ;; "@param {int} row: source row"
    ;; "@param {int} col: source column"
    ;; "@param {int} oRow: destination row"
    ;; "@param {int} oCol: destination column"
    ;; "@param {int} iRow: ignored row"
    ;; "@param {int} iCol: ignored column"
    (cond 
        ((and (= oRow iRow) (= oCol iCol)) 9999)
        (T (max (abs (- row oRow)) (abs (- col oCol))))
    )
   
)

(defun minDistance (row col others distance iRow iCol)
    ;; Return minimum distance between one and every other stack."
    ;; "@param {int} row: source row"
    ;; "@param {int} col: source column"
    ;; "@param {((row col))} others: list of other stack"
    ;; "@param {int} distance: minimal distance"
    ;; "@param {int} iRow: ignored row"
    ;; "@param {int} iCol: ignored column"
    (cond 
        ((null others) distance)
        (T
            (let* (
            (first (car others)) 
            (oRow (car first)) 
            (oCol (cadr first)) 
            (current (distance row col oRow oCol iRow iCol))
            )
                (if (< current distance)
                    (setq distance current)
                )
                (minDistance row col (cdr others) distance iRow iCol)
            )
        )
    )
)

(defun isClosestMove (sRow sCol dRow dCol state)
    ;; Checks if moving is towards the closest (or one of the closest) stack."
    ;; "@param {int} sRow: source row"
    ;; "@param {int} sCol: source column"
    ;; "@param {int} dRow: destination row"
    ;; "@param {int} dCol: destination column"
    ;; "@param {( (row (col ())) )} state: memory representation of the game board"
    (let* (
    (others (getNonEmtpy state)) 
    (dDist (minDistance dRow dCol others 9999 sRow sCol)) 
    (sDist (minDistance sRow sCol others 9999 sRow sCol))
    )
        (< dDist sDist)
    )
)

(defun checkValid (src dest level state)
    ;; Checks if move from src (@level) to dest is valid."
    ;; "@param {( (row col) )} src: source stack"
    ;; "@param {( (row col) )} dest: destination stack"
    ;; "@param {int} level: stack level"
    ;; "@param {( (row (col ())) )} state: memory representation of the game board"
    (let* (
    (sRow (car src))
    (sCol (car (last src)))
    (dRow (car dest))
    (dCol (car (last dest))))
        (and 
            (getSquare sRow sCol state) 
            (getSquare dRow dCol state) 
            
            (= (abs (- sRow dRow)) 1)
            (= (abs (- sCol dCol)) 1)

            (cond 
                ((null (getStack dRow dCol state))
                    (and (isClosestMove sRow sCol dRow dCol state) (= level 0))
                )
                (T T)
            )
        )
    )
)

(defun splitStack (dest src level)
    ;; Splits 'src' stack and moves top part to 'dest' stack."
    ;; "@param {( X O ... )} src: source stack"
    ;; "@param {( X O ... )} dest: destination stack"
    ;; "@param {int} level: source stack level"
    (list (append dest (nthcdr level src)) (subseq src 0 level))
)

(defun moveStack (source destination level env forceMove)
    ;; Moves one stack to another or to an empty place. Takes top of stack by level."
    ;; "@param {( row col )} source: source stack position"
    ;; "@param {( row col )} destination: destination stack position"
    ;; "@param {int} level: source stack level"
    ;; "@param {environment} env: environment"
    (cond 
        ((checkValid source destination level (environment-state env))
            (let* (
            (newEnv (copy-environment env))
            (newState (copy-tree (environment-state newEnv)))
            (newStackWinners (copy-tree (environment-stackWinners newEnv)))
            (sRow (car source))
            (sCol (car (last source)))
            (dRow (car destination))
            (dCol (car (last destination)))
            (src (getStack sRow sCol newState))
            (dest (getStack dRow dCol newState))
            (list (splitStack dest src level))
            (sDest (car list))
            (sSrc (cadr list)))
                (setf (cadr (getSquare sRow sCol newState)) sSrc)

                (let (
                (sXLevels (getLevels sSrc 'X))
                (sOLevels (getLevels sSrc 'O))
                (dXLevels (getLevels sDest 'X))
                (dOLevels (getLevels sDest 'O))
                )
                    (cond 
                        ((> (length sDest) 8)
                            (cond 
                                (forceMove
                                    (write-line "")
                                    (write-line "") 
                                    (write-line "Invalid move. You cannot make stack larger than 8.") 
                                    (playMove env)
                                )
                                (T env)
                            )
                        )
                        ((= (length sDest) 8)
                            (setf (cadr (getSquare dRow dCol newState)) '())
                            (setq newStackWinners (append (last sDest) newStackWinners)) 
                            (setf (environment-state newEnv) newState)

                            (setf (environment-stackWinners newEnv) newStackWinners)
                            (setf (environment-xPositions newEnv) (copy-tree (environment-xPositions newEnv)))
                            (setf (environment-oPositions newEnv) (copy-tree (environment-oPositions newEnv)))

                            (setPosition 'X source sXLevels newEnv)
                            (setPosition 'O source sOLevels newEnv)

                            (removePosition 'X destination newEnv)
                            (removePosition 'O destination newEnv)

                            (setf (environment-checker newEnv) (toggle (environment-checker newEnv)))

                            newEnv
                        )
                        (T
                            (setf (cadr (getSquare dRow dCol newState)) sDest)
                            (setf (environment-state newEnv) newState)
                           
                            (setf (environment-stackWinners newEnv) newStackWinners)
                            (setf (environment-xPositions newEnv) (copy-tree (environment-xPositions newEnv)))
                            (setf (environment-oPositions newEnv) (copy-tree (environment-oPositions newEnv)))

                            (if (null sXLevels) (removePosition 'X source newEnv) (setPosition 'X source sXLevels newEnv))
                            (if (null sOLevels) (removePosition 'O source newEnv) (setPosition 'O source sOLevels newEnv))

                            (setPosition 'X destination dXLevels newEnv)
                            (setPosition 'O destination dOLevels newEnv)

                            (setf (environment-checker newEnv) (toggle (environment-checker newEnv)))

                            newEnv
                        )
                    )        
                )
            )
        )
        (forceMove
            (write-line "")
            (write-line "") 
            (write-line "Invalid move.") 
            (playMove env)
        )
        (T env)
    )
)

(defun playMove (env)
    ;; Allows player to choose a move to play and returns a new environment."
    ;; "@param {environment} env: environment"
    (cond 
        ((> (length (generatePossibleOutcomes env)) 0)
            (write-line "")
            (write-line "Enter source row (A B C ...)")
            (let* ((sRow (cadr (assoc (read ) *numberByLetter*))))
                (cond 
                    ((= sRow -1) env)
                    (T 
                        (write-line "Enter source column (1 2 3 ...):")
                        (let ((sColumn (read )))
                            (write-line "Enter destination row (A B C ...):")
                            (let ((dRow (cadr (assoc (read ) *numberByLetter*))))
                                (write-line "Enter destination column (1 2 3 ...):")
                                (let ((dColumn (read )))
                                    (write-line "Enter source level (0 1 2 ...):")
                                    (let* (
                                    (level (read ))
                                    (stackOwner (nth level (getStack sRow sColumn (environment-state env))))
                                    )
                                        (cond 
                                            ((equal stackOwner (environment-checker env)) 
                                                (moveStack (list sRow sColumn) (list dRow dColumn) level env T)
                                            )
                                            (T
                                                (write-line "")
                                                (write-line "") 
                                                (write-line "You are not stack owner!")
                                                (playMove env)
                                            )
                                            
                                        )
                                    )
                                )
                            )
                        )
                    )
                )
            )
        )
        (T
            (write (environment-checker env))
            (write-string " doesn't have valid moves. Skipping.")
            (write-line "")
            (setf (environment-checker env) (toggle (environment-checker env)))
        )
    )
    
)

(defun isQuasiTerminal (node)
    ;; Checks if node is quasi terminal (no children)."
    ;; "@param {node} node: node"
    (null (node-children node))
)

(defun generateMovesByLevel (rcPair level env)
    ;; Generates all valid moves on given level for one stack for all adjacent squares
    (let* (
    (row (car rcPair))
    (column (cadr rcPair))
    (move1 (moveStack rcPair (list (+ row 1) (+ column 1)) level env '()))
    (move2 (moveStack rcPair (list (+ row 1) (- column 1)) level env '()))
    (move3 (moveStack rcPair (list (- row 1) (- column 1)) level env '()))
    (move4 (moveStack rcPair (list (- row 1) (+ column 1)) level env '()))
    (ret '())
    )
        (if (not (equal move1 env)) (setq ret (cons move1 ret)))
        (if (not (equal move2 env)) (setq ret (cons move2 ret)))
        (if (not (equal move3 env)) (setq ret (cons move3 ret)))
        (if (not (equal move4 env)) (setq ret (cons move4 ret)))

        ret
    )  
)

(defun generateMoves (position env)
    ;; Calls generateMovesByLevel for a given position
    (cond 
        ((or (null (cdr position)) (null (caadr position))) '())
        (T
            (append (generateMovesByLevel (car position) (caadr position) env) (generateMoves (cons (car position) (cddr position)) env))
        )
    )
)

(defun generateAllMoves (positions env)
    ;; Calls generateMoves for all given positions
    (cond 
        ((null positions) '())
        (T
             (append (generateMoves (car positions) env) (generateAllMoves (cdr positions) env))
             ;; (generateMoves (car positions) env)
        )
    )
)

(defun generatePossibleOutcomes (env)
    ;; Generates possible outcomes (next states) by given environment"

    (let* (
    (checker (environment-checker env))
    (positions (if (equal checker 'X) (environment-xPositions env) (environment-oPositions env)))
    (outcomes (generateAllMoves positions env))
    )
        (setq DEV-nodeCount (+ DEV-nodeCount (length outcomes)))
        outcomes
    )
)

(defun outcomesToNodes (list)
    ;; Maps outcomes (list of states) to list of Nodes."
    (cond 
        ((null list) ())
        (T 
            (let ((first (car list)) (value (random 1000)))
                (cons (make-node :environment first :value value ) (outcomesToNodes (cdr list)))
            )
        )
    )
)

(defun deepen (list n)
    ;; Generates children of last (quasi-terminal) nodes."
    (cond 
        ((or (null list) (= n 0)) ())
        ((listp list)
            (deepen (car list) n) (deepen (cdr list) n)
        )        
        ((node-p list)
            (cond 
                ((isQuasiTerminal list)
                    (setf (node-children list) (outcomesToNodes (generatePossibleOutcomes (node-environment list))))
                    (cond 
                        ((= n 0) list)
                        (T (deepen (node-children list) (- n 1)))
                        ;; (T (deepen (outcomesToNodes (generatePossibleOutcomes (node-environment list))) (- n 1)))
                    )
                )
                (T
                    (deepen (node-children list) n)
                )
            )
        )
    )
    list
)

(defun printPretty (node indent last)
    ;; Prints pretty tree starting from given node."
    (write-line "")
    (write-string indent)
    (cond 
        (last (write-string "L ") (setq indent (concatenate 'string indent "     ")))
        (T
            (write-string "|-") (setq indent (concatenate 'string indent "|    "))
        )
    )
    
    (write (environment-state (node-environment node))) 
    (write (environment-checker (node-environment node))) 
    (write (node-value node)) 
    (let* ((n (- (length (node-children node)) 1)))
        (loop for i from 0 to n do
            (let ((child (nth i (node-children node))))
                (printPretty child indent (= i n))
            )
        )
    )   
)

(defun printStackWinners (stackWinners)
    ;; Prints list of stack winners."
    ;; "@param {(X O X)} stackWinners: list of checkers"
    (cond 
        ((not (null stackWinners))
            (write stackWinners)
            (write-line "")
            (write stackWinners)
        )
        (T
            (write-line "No won stacks.")
        )
    )
)

(defun checkWinner (stackWinners half)
    ;; Checks and returns winner if there is enough stack winners to end the game."
    ;; "@param {(X O X)} stackWinners: list of checkers"
    ;; "@param {float} half: needed amount for win"
    (cond 
        ((>= (frequency 'X stackWinners) half) 'X)
        ((>= (frequency 'O stackWinners) half) 'O)
        (T ())
    )
)

(defun frequency (el list)
    ;; Returns count of occurence frequency of element in list. Not nested."
    ;; "@param {any} el: search element"
    ;; "@param {( any )} list: list"
    (cond 
        ((null list) 0)
        ((equal el (car list)) (+ 1 (frequency el (cdr list))))
        (T (frequency el (cdr list)))
    )
)

(defun toggle (checker)
    ;; Returns other checker based on input."
    ;; "@param {X or O} checker: element to toggle"
    (cond 
        ((equal checker 'X) 'O)
        (T 'X)
    )
)

(defun isTerminal (node)
    (isQuasiTerminal node)
)


(defun alphabeta (node A B)
    (cond 
        ((isTerminal node)
            (node-value node)
        )
    )
    (if (equal (environment-checker (node-environment node)) 'X)
        (let ((value -999999))
            (dolist (child (node-children node))
                (setf value (max value (alphabeta child A B)))
                (setf A (max A value))
                (if (>= A B) (return ))
            )
            value
        )
        (let ((value +999999))
            (dolist (child (node-children node))
                (setf value (min value (alphabeta child A B)))
                (setf A (min A value))
                (if (>= A B) (return ))
            )
            value
        )
    )
)

(defun main ()
    ;; Main function."
    (write-line "")
    (write-line "Enter field size (8 or 10, default 8):")
    (let* (
    (inputSize (read ))
    (n (cond 
        ((not (or (= inputSize 8) (= inputSize 10))) 8)
        (T inputSize)
    ))
    (env (make-environment :state (generatePlayingField n n)))
    (minForWin (/ (* n (- n 2)) 32))
    )
        (initializeAllPositions (getNonEmtpy (environment-state env)) env)
        (write-line "AI should play? (X, O or NO)")
        (let* (
        (AI (read ))
        (player (if (or (equal (string-upcase AI) "NO")) 'X (toggle AI)))
        (AI (not (equal (string-upcase AI) "NO")))
        )
            (printBoard (environment-state env) T n)

            (write-string "1: print board, 2: write state, 3: play a move, 4: generate possible outcomes, 5: alphabeta,  -1: exit")
            (write-line "")
            (let ((action (read )))
                (loop while (not (<= action -1)) do
                    (cond 
                        ((= action 1)
                            (write-line "")
                            (printBoard (environment-state env) T n)
                            (write-string "Won stacks: ")
                            (printStackWinners (environment-stackWinners env))

                            (write-string "Now plays: ")
                            (write (environment-checker env))
                        )
                        ((= action 2)
                            (write-line "")
                            (write (environment-state env))
                        )
                        ((= action 3)
                            (write-string "Now plays: ")
                            (write (environment-checker env))
                            (cond 
                                ((equal player (environment-checker env))
                                    (setq env (playMove env))
                                    (printBoard (environment-state env) T n)
                                )
                                (T
                                    (cond 
                                        (AI (write-line "") (write-string "AI PLAYS ASTONISHING MOVE!"))
                                        (T
                                            (setq env (playMove env))
                                            (printBoard (environment-state env) T n)
                                        )
                                    )
                                )
                            )

                            (let ((winner (checkWinner (environment-stackWinners env) minForWin)))
                                (cond 
                                    ((not (null winner))
                                        (write-string "Winner is ")
                                        (write winner)
                                        (write-string "!")
                                        (exit )
                                    )
                                )
                            )              
                        )
                        ((= action 4)
                            (write-line "Would you like to print possible outcomes? (0: no, 1: yes)")
                            (let ((outcomes (generatePossibleOutcomes env)) (print (read )))
                                (if (= print 1)
                                    (loop for oc in outcomes do
                                        (write-line "")
                                        (write-line "")
                                        (write-line "")
                                        (printBoard (environment-state oc) T n)
                                    )
                                )
                                
                                (write-line "Number of possible outcomes: ")
                                (write (length outcomes))
                                (setq DEV-nodeCount 1)
                            )
                            
                        )
                        ((= action 5)
                            (write-line "Depth:")
                            (let ((num (read )))
                                (time 
                                    (let ((a (deepen (list (make-node :environment env :value 55 :children '())) num)))
                                        (printPretty (first a) "" '())
                                        (print DEV-nodeCount)
                                        (setq DEV-nodeCount 1)
                                        (print (alphabeta (first a) -999999 +999999))
                                    )
                                )
                                
                            )
                        )
                        (T 
                            (write-line "Please choose correct action.")
                        )
                    )
                    (write-line "")
                    (write-line "1: print board, 2: write state, 3: play a move, 4: generate possible outcomes, 5: alphabeta,  -1: exit")
                    (write-line "Choose another action:")
                    (write-line "")
                    (setq action (read ))
                )
            )
        )
    )
)

(main )