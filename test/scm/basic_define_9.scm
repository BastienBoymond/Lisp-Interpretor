
(define variable 1)

(define isEven
    (if (eq? (mod variable 2) 0)
        #t
        #f
    )
)

isEven