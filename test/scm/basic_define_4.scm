(define foo 42)
(if (< foo 10)
    (- foo 3)
    (if (< foo 10)
        (* foo 3)
        (* foo 2)))
