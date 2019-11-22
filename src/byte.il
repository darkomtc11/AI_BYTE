defconstant(*numberByLetter* '((A 1) (B 2) (C 3) (D 4) (E 5) (F 6) (G 7) (H 8) (I 9) (J 10) (K 11) (L 12) (M 13) (N 14) (O 15) (P 16) (Q 17) (R 18) (S 19) (T 20)))
defconstant(*letterByNumber* '((1 A) (2 B) (3 C) (4 D) (5 E) (6 F) (7 G) (8 H) (9 I) (10 J) (11 K) (12 L) (13 M) (14 N) (15 O) (16 P) (17 Q) (18 R) (19 S) (20 T)))
defconstant(*allowed1* '(1 3 5 7 9 11 13 15 17 19))
defconstant(*allowed2* '(2 4 6 8 10 12 14 16 18 20))

defstruct(node environment (value 'HV) (children '()))
defstruct(environment state (stackWinners '()))

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

defun(getSquareStack (row column state)
    "Returns square stack on given row/column."
    cadr(assoc(column cadr(assoc(row state))))
)

defun(getSquare (row column state)
    "Returns square on given row/column alongside column key."
    assoc(column cadr(assoc(row state)))
)

defun(moveStack (sRow sColumn dRow dColumn level env)
    "Moves one stack to another or to an empty place. Takes top of stack by level."
    cond(
        (and(checkSquare(sRow sColumn) checkSquare(dRow dColumn))
            let*((
            (newEnv copy-environment(env))
            (newState copy-tree(environment-state(newEnv)))
            (newStackWinners copy-tree(environment-stackWinners(newEnv)))
            (src getSquareStack(sRow sColumn newState))
            (dest getSquareStack(dRow dColumn newState))
            (list splitStack(dest src level))
            (sDest car(list))
            (sSrc cadr(list)))
                // print(sSrc)
                // print(sDest)
                setf(cadr(getSquare(sRow sColumn newState)) sSrc)
                cond(
                    (=(length(sDest) 8)
                        setf(cadr(getSquare(dRow dColumn newState)) '())
                        setq(newStackWinners append(last(sDest) newStackWinners))  
                    )
                    (T
                        setf(cadr(getSquare(dRow dColumn newState)) sDest)
                    )
                )        
                setf(environment-state(newEnv) newState)
                setf(environment-stackWinners(newEnv) newStackWinners)
                newEnv
            )
        )
        (T write-line("Invalid move") env)
    )
)

defun(splitStack (stay move count)
    "Splits 'move' and moves top part to 'stay'"
    list(append(stay nthcdr(-(count 1) move)) subseq(move 0 -(count 1)))
)

defun(playMove (state)
    "Allows player to choose a valid move to play and returns a new game state."
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
                    let(((level read()))
                        moveStack(sRow sColumn dRow dColumn level state)
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
                moveStack(7 1 6 2 1 env) 
                moveStack(7 3 6 4 1 env) 
                moveStack(7 3 6 4 1 env) 
                moveStack(7 3 6 4 1 env) 
                moveStack(7 3 6 4 1 env) 
                moveStack(7 3 6 4 1 env) 
                moveStack(7 3 6 4 1 env) 
                moveStack(7 3 6 4 1 env) 
                moveStack(7 3 6 4 1 env) 
                moveStack(7 3 6 4 1 env) 
                moveStack(7 3 6 4 1 env) 
                moveStack(7 3 6 4 1 env) 
                moveStack(7 3 6 4 1 env) 
                moveStack(7 3 6 4 1 env) 
                moveStack(7 3 6 4 1 env) 
                moveStack(7 3 6 4 1 env) 
                moveStack(7 3 6 4 1 env) 
                moveStack(7 3 6 4 1 env) 
                moveStack(7 3 6 4 1 env) 
                moveStack(7 3 6 4 1 env) 
                moveStack(7 3 6 4 1 env) 
                moveStack(7 3 6 4 1 env) 
                moveStack(7 3 6 4 1 env) 
                moveStack(7 3 6 4 1 env)
                moveStack(7 3 6 4 1 env) 
                moveStack(7 3 6 4 1 env) 
                moveStack(7 3 6 4 1 env) 
                moveStack(7 3 6 4 1 env) 
                moveStack(7 3 6 4 1 env) 
                moveStack(7 3 6 4 1 env)
                moveStack(7 3 6 4 1 env) 
                moveStack(7 3 6 4 1 env) 
                moveStack(7 3 6 4 1 env) 
                moveStack(7 3 6 4 1 env) 
                moveStack(7 3 6 4 1 env)
                // 35
            )))
                setq(DEV-nodeCount +(DEV-nodeCount length(outcomes)))
                outcomes
            )
            
            
        )
        (=(player 1)
            list(moveStack(2 8 3 7 1 env) moveStack(2 6 3 5 1 env))
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
    if(not(null(stackWinners)) write(stackWinners))
)

defun(checkWinner (stackWinners)
    cond(
        (=(frequency('X stackWinners) 2) 'X)
        (=(frequency('O stackWinners) 2) 'O)
        (T ())
    )
)

defun(frequency (el list)
    cond(
        (null(list) 0)
        (equal(el car(list)) +(1 frequency(el cdr(list))))
        (T frequency(el cdr(list)))
    )
)

defun(checkValid (src dest level state)
    
)

defun(toggle (checker)
    cond(
        (equal(checker 'X) 'O)
        (checker 'X)
    )
)

defun(main ()

    write-line("")
    write-line("Enter field size:")
    let*((
    (n read())
    (env make-environment(:state generatePlayingField(n n)))
    (checker 'X)
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
                            printStackWinners(environment-stackWinners(env))
                        )
                        (=(action 2)
                            write-line("")
                            write(environment-state(env))
                        )
                        (=(action 3)
                            cond(
                                (equal(player checker)
                                    setq(env playMove(env))
                                    printBoard(environment-state(env)T n)
                                )
                                (T
                                    cond(
                                        (AI write-string("AI PLAYS ASTONISHING MOVE!"))
                                        (T
                                            setq(env playMove(env))
                                            printBoard(environment-state(env)T n)
                                        )
                                    )
                                )
                            )

                            let(((winner checkWinner(environment-stackWinners(env))))
                                cond(
                                    (not(null(winner))
                                        write-line("Winner is ")
                                        write(winner)
                                    )
                                )
                            )
                            
                            setq(checker toggle(checker))              
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
                        (T 
                            write-line("Please choose correct action.")
                        )
                    )
                    write-line("")
                    write-line("Choose another action:")
                    write-line("")
                    setq(action read())
                )
            )
        )
    )
)

main()