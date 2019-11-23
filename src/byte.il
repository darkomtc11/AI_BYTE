defconstant(*numberByLetter* '((A 1) (B 2) (C 3) (D 4) (E 5) (F 6) (G 7) (H 8) (I 9) (J 10) (K 11) (L 12) (M 13) (N 14) (O 15) (P 16) (Q 17) (R 18) (S 19) (T 20)))
defconstant(*letterByNumber* '((1 A) (2 B) (3 C) (4 D) (5 E) (6 F) (7 G) (8 H) (9 I) (10 J) (11 K) (12 L) (13 M) (14 N) (15 O) (16 P) (17 Q) (18 R) (19 S) (20 T)))
defconstant(*allowed1* '(1 3 5 7 9 11 13 15 17 19))
defconstant(*allowed2* '(2 4 6 8 10 12 14 16 18 20))

defstruct(node environment (value 'HV) (children '()))
defstruct(environment state (stackWinners '()) (checker 'X))

defvar(DEV-nodeCount 1) // help variable for development

defun(generatePlayingField (current n)
    "Generates whole playing field by generating rows."
    cond(
        (=(current 0) ())
        (T
            cons(generateRow(current n) generatePlayingField(-(current 1) n))
        )
    )
)

defun(generateRow (rowIndex n)
    "Initiates full row stack generation."
    let(((index +(-(n rowIndex) 1)) (size /(n 2)) (type mod(rowIndex 2)))
        cons(
            index
            list(generateStacks(type size
                cond(
                    (or(=(rowIndex 1) =(rowIndex n))
                        ()
                    )
                    (=(type 0)
                        '(X)
                    )
                    (T
                        '(O)
                    )
                ) n
            ))
        )
    )
)

defun(generateStacks (type curr data n)
    "Generates all stacks of one row based on type (odd/even) and puts given data on stack."
    let(((index -(/(n 2) curr)) (next -(curr 1)))
        cond(
            (=(curr 0) ())
            (=(mod(type 2) 0) // gore levo - 0: CRNO, 1: BELO
                cons(cons(nth(index *allowed1*) list(data)) generateStacks(type next data n))
            )
            (T cons(cons(nth(index *allowed2*) list(data)) generateStacks(type next data n)))
        )
    )
)

defun(writeNumbers (n)
    "Prints column numbers."
    cond(
        (=(n 0) write-string(" "))
        (T 
            writeNumbers(-(n 1))
            write-string(format(nil "~4D   " n))
        )
    )
)

defun(printBoard (field first n)
    "Initiaces full board printing"
    cond(
        (=(length(field) 0) ())
        (first 
            writeNumbers(length(field))
            write-line("")
            printBoard(field () n)
        )
        (T 
            printRow(car(field) 1 n n)
            write-line("")
            printBoard(cdr(field) () n)
        )
    )
)

defun(printRow (row type curr n)
    "Prints row in three parts. Calls it self 2 times with different type."
    cond(
        (=(curr -1)
            
            cond(
                (=(type 3) ())
                (T
                    write-line("")
                    printRow(row +(type 1) n n)
                )
            )
        )
        (T
            if(and(=(type 2) =(curr n)) 
                write(cadr(assoc(car(row) *letterByNumber*)))

                if(=(curr n) 
                    write-string(" ")
                )
            )
           
            let(((square assoc(+(-(n curr) 1) cadr(row))))
                cond(
                    (null(square)
                        write-string("       ")
                    )
                    (T
                        write-string(" ")
                        printSquare(square type)
                    )
                )
            )
            
            printRow(row type -(curr 1) n)
        )
    )
)

defun(printSquare (square part)
    "Prints one third of stacked checkers on given square."
    cond(
        (=(part 1)
            printChecker(nth(8 cadr(square)))
            printChecker(nth(7 cadr(square)))
            printChecker(nth(6 cadr(square)))
        )
        (=(part 2)
            printChecker(nth(5 cadr(square)))
            printChecker(nth(4 cadr(square)))
            printChecker(nth(3 cadr(square)))
        )
        (=(part 3)
            printChecker(nth(2 cadr(square)))
            printChecker(nth(1 cadr(square)))
            printChecker(nth(0 cadr(square)))
        )
    )
)

defun(printChecker (checker)
    "Prints checkers one by one."
    cond(
        (null(checker) write-string("- "))
        (T write(checker) write-string(" "))
    )
)

defun(checkSquare (row column)
    "Checks is given row/column returns valid (dark) square."
    =(mod(+(row column) 2) 0)
)

defun(getStack (row column state)
    "Returns stack on given row/column."
    cadr(assoc(column cadr(assoc(row state))))
)

defun(getRowNonEmpty (rowIndex row)
    "Retruns all non empty squares in one row. (row column)"
    cond(
        (null(row) ())
        (not(null(cadar(row))) cons(list(rowIndex caar(row)) getRowNonEmpty(rowIndex cdr(row))))
        (T getRowNonEmpty(rowIndex cdr(row)))
    )
)

defun(getNonEmtpy (state)
    "Returns non empty squares. List of (row column)."
    cond(
        (null(state) ())
        (T append(getRowNonEmpty(caar(state) cadar(state)) getNonEmtpy(cdr(state))))
    )
)

defun(getSquare (row column state)
    "Returns square on given row/column alongside column key."
    assoc(column cadr(assoc(row state)))
)

defun(distance (row col oRow oCol iRow iCol)
    "Returns distance between 2 stacks, unless it's second stack is ignored stack."
    cond(
        (and(=(oRow iRow) =(oCol iCol)) 9999)
        (T max(abs(-(row oRow)) abs(-(col oCol))))
    )
   
)

defun(minDistance (row col others distance iRow iCol)
    "Return minimum distance between one and every other stack."
    cond(
        (null(others) distance)
        (T
            let*(((first car(others)) (oRow car(first)) (oCol cadr(first)) (current distance(row col oRow oCol iRow iCol)))
                if(<(current distance)
                    setq(distance current)
                )
                minDistance(row col cdr(others) distance iRow iCol)
            )
        )
    )
)

defun(isClosestMove (sRow sCol dRow dCol state)
    "Checks if moving is towards the closest (or one of the closest) stack."
    let*(((others getNonEmtpy(state)) (dDist minDistance(dRow dCol others 9999 sRow sCol)) (sDist minDistance(sRow sCol others 9999 sRow sCol)))
        <(dDist sDist)
    )
)

defun(checkValid (src dest level state)
    "Checks if move from src (@level) to dest is valid."
    let*((
    (sRow car(src))
    (sCol car(last(src)))
    (dRow car(dest))
    (dCol car(last(dest))))
        and(
            checkSquare(sRow sCol) 
            
            =(abs(-(sRow dRow)) 1)
            =(abs(-(sCol dCol)) 1)

            cond(
                (null(getStack(dRow dCol state))
                    and(isClosestMove(sRow sCol dRow dCol state) =(level 0))
                )
                (T T)
            )
        )
    )
)

defun(splitStack (stay move level)
    "Splits 'move' list and moves top part to 'stay' list."
    list(append(stay nthcdr(level move)) subseq(move 0 level))
    // list(append(stay nthcdr(-(level 1) move)) subseq(move 0 -(level 1)))
)

defun(moveStack (source destination level env)
    "Moves one stack to another or to an empty place. Takes top of stack by level."
    cond(
        (checkValid(source destination level environment-state(env))
            let*((
            (newEnv copy-environment(env))
            (newState copy-tree(environment-state(newEnv)))
            (newStackWinners copy-tree(environment-stackWinners(newEnv)))
            (sRow car(source))
            (sCol car(last(source)))
            (dRow car(destination))
            (dCol car(last(destination)))
            (src getStack(sRow sCol newState))
            (dest getStack(dRow dCol newState))
            (list splitStack(dest src level))
            (sDest car(list))
            (sSrc cadr(list)))
                // print(sSrc)
                // print(sDest)
                setf(cadr(getSquare(sRow sCol newState)) sSrc)
                cond(
                    (=(length(sDest) 8)
                        setf(cadr(getSquare(dRow dCol newState)) '())
                        setq(newStackWinners append(last(sDest) newStackWinners))  
                    )
                    (T
                        setf(cadr(getSquare(dRow dCol newState)) sDest)
                    )
                )        
                setf(environment-state(newEnv) newState)
                setf(environment-stackWinners(newEnv) newStackWinners)
                newEnv
            )
        )
        (T 
            write-line("")
            write-line("") 
            write-line("Invalid move.") 
            playMove(env)
        )
    )
)

defun(playMove (env)
    "Allows player to choose a move to play and returns a new environment."
    write-line("")
    write-line("Enter source row (A B C ...):")
    let(((sRow cadr(assoc(read() *numberByLetter*))))
        write-line("Enter source column (1 2 3...):")
        let(((sColumn read()))
            write-line("Enter destination row (A B C ...):")
            let(((dRow cadr(assoc(read() *numberByLetter*))))
                write-line("Enter destination column (1 2 3...):")
                let(((dColumn read()))
                    write-line("Enter source level:")
                    let*((
                    (level read())
                    (stackOwner nth(level getStack(sRow sColumn environment-state(env))))
                    )
                        cond(
                            (equal(stackOwner environment-checker(env)) 
                                moveStack(list(sRow sColumn) list(dRow dColumn) level env)
                            )
                            (T
                                write-line("")
                                write-line("") 
                                write-line("You are not stack owner!")
                                playMove(env)
                            )
                            
                        )
                       
                        
                    )
                )
            )
        )
    )
)

defun(isQuasiTerminal (node)
    "Checks if node is quasi terminal (no children)."
    null(node-children(node))
)

defun(generatePossibleOutcomes (env player)
    "Generates possible outcomes (next states) by state and player."
    cond(
        (=(player 0)
            let(((outcomes list(
                moveStack(list(7 1) list(6 2) 1 env) 
                moveStack(list(7 3) list(6 4) 1 env) 
                moveStack(list(7 3) list(6 4) 1 env) 
                moveStack(list(7 3) list(6 4) 1 env) 
                moveStack(list(7 3) list(6 4) 1 env) 
                moveStack(list(7 3) list(6 4) 1 env) 
                moveStack(list(7 3) list(6 4) 1 env) 
                moveStack(list(7 3) list(6 4) 1 env) 
                moveStack(list(7 3) list(6 4) 1 env) 
                moveStack(list(7 3) list(6 4) 1 env) 
                moveStack(list(7 3) list(6 4) 1 env) 
                moveStack(list(7 3) list(6 4) 1 env) 
                moveStack(list(7 3) list(6 4) 1 env) 
                moveStack(list(7 3) list(6 4) 1 env) 
                moveStack(list(7 3) list(6 4) 1 env) 
                moveStack(list(7 3) list(6 4) 1 env) 
                moveStack(list(7 3) list(6 4) 1 env) 
                moveStack(list(7 3) list(6 4) 1 env) 
                moveStack(list(7 3) list(6 4) 1 env) 
                moveStack(list(7 3) list(6 4) 1 env) 
                moveStack(list(7 3) list(6 4) 1 env) 
                moveStack(list(7 3) list(6 4) 1 env) 
                moveStack(list(7 3) list(6 4) 1 env) 
                moveStack(list(7 3) list(6 4) 1 env)
                moveStack(list(7 3) list(6 4) 1 env) 
                moveStack(list(7 3) list(6 4) 1 env) 
                moveStack(list(7 3) list(6 4) 1 env) 
                moveStack(list(7 3) list(6 4) 1 env) 
                moveStack(list(7 3) list(6 4) 1 env) 
                moveStack(list(7 3) list(6 4) 1 env)
                moveStack(list(7 3) list(6 4) 1 env) 
                moveStack(list(7 3) list(6 4) 1 env) 
                moveStack(list(7 3) list(6 4) 1 env) 
                moveStack(list(7 3) list(6 4) 1 env) 
                moveStack(list(7 3) list(6 4) 1 env)
                // 35
            )))
                setq(DEV-nodeCount +(DEV-nodeCount length(outcomes)))
                outcomes
            )
            
            
        )
        (=(player 1)
            list(moveStack(list(2 8) list(3 7) 1 env) moveStack(list(2 6) list(3 5) 1 env))
        )
    )
)

defun(outcomesToNodes (list)
    "Maps outcomes (list of states) to list of Nodes."
    cond(
        (null(list) ())
        (T cons(make-node(:environment car(list)) outcomesToNodes(cdr(list))))
    )
)

defun(deepen (list n)
    "Generates children of last (quasi-terminal) nodes."
    cond(
        (or(null(list) =(n 0)) ())
        (listp(list)
            deepen(car(list) n) deepen(cdr(list) n)
        )        
        (node-p(list)
            cond(
                (isQuasiTerminal(list)
                    // setf(node-children(list) outcomesToNodes(generatePossibleOutcomes(node-state(list) 0)))
                    cond(
                        (=(n 0) list)
                        // (T deepen(node-children(list) -(n 1)))
                        (T deepen(outcomesToNodes(generatePossibleOutcomes(node-environment(list) 0)) -(n 1)))
                    )
                )
                (T
                    deepen(node-children(list) n)
                )
            )
        )
    )
)

defun(printPretty (node indent last)
    "Prints pretty tree starting from given node."
    write-line("")
    write-string(indent)
    cond(
        (last write-string("L ") setq(indent concatenate('string indent "     ")))
        (T
            write-string("|-") setq(indent concatenate('string indent "|    "))
        )
    )
    
    write(environment-state(node-environment(node)))  
    let*(((n -(length(node-children(node)) 1)))
        loop(for i from 0 to n do
            let(((child nth(i node-children(node))))
                printPretty(child indent =(i n))
            )
        )
    )   
)

defun(printStackWinners (stackWinners)
    "Prints list of stack winners."
    cond(
        (not(null(stackWinners))
            write-line("")
            write(stackWinners)
        )
        (T
            write-line("No won stacks.")
        )
    )
)

defun(checkWinner (stackWinners half)
    "Checks and returns winner if there is enough stack winners to end the game."
    cond(
        (>=(frequency('X stackWinners) half) 'X)
        (>=(frequency('O stackWinners) half) 'O)
        (T ())
    )
)

defun(frequency (el list)
    "Returns counts of occurence frequency of element in list. Non recursive."
    cond(
        (null(list) 0)
        (equal(el car(list)) +(1 frequency(el cdr(list))))
        (T frequency(el cdr(list)))
    )
)

defun(toggle (checker)
    "Returns other checker based on input."
    cond(
        (equal(checker 'X) 'O)
        (checker 'X)
    )
)

defun(main ()
    "Main function."
    write-line("")
    write-line("Enter field size:")
    let*((
    (n read())
    (env make-environment(:state generatePlayingField(n n)))
    (minForWin /(*(n -(n 2)) 32))
    )
        write-line("AI should play? (X, O or NO)")
        let*((
        (AI read())
        (player if(or(equal(string-upcase(AI) "NO")) 'X toggle(AI)))
        (AI not(equal(string-upcase(AI) "NO")))
        )


            write-string("1: print board, 2: write state, 3: play a move, -1: exit")
            write-line("")
            let(((action read()))
                loop(while not(<=(action -1)) do
                    cond(
                        (=(action 1)
                            write-line("")
                            printBoard(environment-state(env) T n)
                            write-string("Won stacks: ")
                            printStackWinners(environment-stackWinners(env))

                            write-string("Now plays: ")
                            write(environment-checker(env))
                        )
                        (=(action 2)
                            write-line("")
                            write(environment-state(env))
                        )
                        (=(action 3)
                            write-string("Now plays: ")
                            write(environment-checker(env))
                            cond(
                                (equal(player environment-checker(env))
                                    setq(env playMove(env))
                                    printBoard(environment-state(env)T n)
                                )
                                (T
                                    cond(
                                        (AI write-string("AI PLAYS ASTONISHING MOVE!"))
                                        (T
                                            setq(env playMove(env))
                                            printBoard(environment-state(env) T n)
                                        )
                                    )
                                )
                            )

                            let(((winner checkWinner(environment-stackWinners(env) minForWin)))
                                cond(
                                    (not(null(winner))
                                        write-string("Winner is ")
                                        write(winner)
                                        write-string("!")
                                        exit()
                                    )
                                )
                            )
                            
                            setf(environment-checker(env) toggle(environment-checker(env)))              
                        )
                        (=(action 4)
                            write-line("Depth:")
                            let(((num read()))
                                time(
                                    deepen(list(make-node(:environment env :value 'HV :children '())) num)
                                )
                                print(DEV-nodeCount)
                                setq(DEV-nodeCount 1)
                                // printPretty(root "" '())
                                // room()
                            )
                        )
                        (=(action 5)
                            print(getNonEmtpy(environment-state(env)))
                        )
                        (T 
                            write-line("Please choose correct action.")
                        )
                    )
                    write-line("")
                    write-line("1: print board, 2: write state, 3: play a move, -1: exit")
                    write-line("Choose another action:")
                    write-line("")
                    setq(action read())
                )
            )
        )
    )
)

main()