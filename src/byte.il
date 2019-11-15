(defvar letters '(A B C D E F G H I J))
(defvar allowed1 '(1 3 5 7 9))
(defvar allowed2 '(2 4 6 8 10))

defun(generateState (size)
    generateField(size size)
)

defun(generateField (size current)
    cond(
        (=(current 0) ())
        (T
            cons(generateRow(current size) generateField(size -(current 1)))
        )
    )
)

defun(generateRow (type size)
    cond(
        (or(=(type 1) =(type size))
            cons(nth(-(size type) letters) list(generateStacks(type /(size 2) /(size 2) ())))
        )
        (=(mod(type 2) 0)
            cons(nth(-(size type) letters) list(generateStacks(type /(size 2) /(size 2) '(O))))
        )
        (T
            cons(nth(-(size type) letters) list(generateStacks(type /(size 2) /(size 2) '(X))))
        )
    )
)

defun(generateStacks (type size curr data)
    cond(
        (=(curr 0) ())
        (=(mod(type 2) 0)
            cons(cons(nth(-(size curr) allowed1) list(data)) generateStacks(type size -(curr 1) data))
        )
        (T cons(cons(nth(-(size curr) allowed2) list(data)) generateStacks(type size -(curr 1) data)))
    )
)

defun(writeNumbers (n)
    cond(
        (=(n 0) write-string("#"))
        (T 
            writeNumbers(-(n 1))
            write-string("   ")
            write(n)
        )
    )
)

defun(printPretty (field size first)
    cond(
        (=(length(field) 0) ())
        (first 
            writeNumbers(length(field))
            write-line("")
            printPretty(field size ())
        )
        (T 
            printRow(car(field) size "top")
            write-line("")
            printPretty(cdr(field) size ())
        )
    )
)

defun(printRow (row size curr)
    ;;write-line("")
    
    cond(
        (equal(curr "top")
            write-string("   ")

            loop(for i from 1 to size do
                setq(stack assoc(i cadr(row)))
                cond(
                    (null(stack)
                        write-string("    ")
                    )
                    (T
                        printStack(stack "top")
                        write-string(" ")
                    )
                )
            )
            write-line("")
            printRow(row size "middle")
        )
        (equal(curr "middle")
            write(car(row))
            write-string("  ")

            loop(for i from 1 to size do
                setq(stack assoc(i cadr(row)))
                cond(
                    (null(stack)
                        write-string("    ")
                    )
                    (T
                        printStack(stack "middle")
                        write-string(" ")
                    )
                )
            )
            write-line("")
            printRow(row size "bottom")
        )
        (equal(curr "bottom")
            write-string("   ")

            loop(for i from 1 to size do
                setq(stack assoc(i cadr(row)))
                cond(
                    (null(stack)
                        write-string("    ")
                    )
                    (T
                        printStack(stack "bottom")
                        write-string(" ")
                    )
                )
            )
        )
    )

    ;;write(row)
)

defun(printStack (stack type)
    cond(
        (equal(type "top")
            printChecker(nth(8 cadr(stack)))
            printChecker(nth(7 cadr(stack)))
            printChecker(nth(6 cadr(stack)))
        )
        (equal(type "middle")
            printChecker(nth(5 cadr(stack)))
            printChecker(nth(4 cadr(stack)))
            printChecker(nth(3 cadr(stack)))
        )
        (equal(type "bottom")
            printChecker(nth(2 cadr(stack)))
            printChecker(nth(1 cadr(stack)))
            printChecker(nth(0 cadr(stack)))
        )
    )
)

defun(printChecker (checker)
    cond(
        (null(checker) write-string("-"))
        (T write(checker))

    )
)

print(cadr(assoc(2 cadr(assoc('B generateState(8))))))
write-line("")
write-line("")
printPretty(generateState(10) 10 T)

;;write(generateState(10))