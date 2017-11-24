; Return #t if obj is a listdiff, #f otherwise
(define (listdiff? obj)
	(cond
		; A listdiff is a pair
		[(not (pair? obj)) #f]
		; The car of a listdiff is L, which is a pair
		[(not (pair? (car obj))) #f]
		; The cdr of a listdiff is eq? to L, or to cdr L, or to cdr cdr L, etc
		[(eq? (cdr obj) (car obj)) #t]
		[else (listdiff? (cons (cdr (car obj)) (cdr obj)))]
		))
