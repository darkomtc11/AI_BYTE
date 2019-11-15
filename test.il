defun(getStack (row column)
    setq(key concatenate('string row write-to-string(column)))
    cadr(assoc(key flowers :test #'equalp))
)

defun(setassoc (sym val L)
  "Associates the value with the symbol in the list L."
  setcdr(assoc(sym L) val)
)

setq(key concatenate('string "a" write-to-string(1)))

setq(flowers '((key (W B B W)) ((b 2) (W B B W)) ((A 3) ())))
setq(row "a")
setq(column 1)

(setcdr flowers '(cat dog))

print(getStack(row column))
print(flowers)

;; defun(printConc (row column)
;;    print(concatenate('string row write-to-string(column)))
;; )

;; printConc(row column)