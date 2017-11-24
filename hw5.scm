; Return #t if obj is an empty listdiff, #f otherwise.
(define (null-ld? obj)
	; An empty listdiff must be a listdiff and its car must be eq? to its cdr
	(and (listdiff? obj) (eq? (car obj) (cdr obj)))
	)

; Return #t if obj is a listdiff, #f otherwise.
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

; Return a listdiff whose first element is obj and whose remaining elements are
; listdiff. The last argument cannot be an arbitrary object; it must be a
; listdiff.
(define (cons-ld obj listdiff)
	(cons (cons obj (car listdiff)) (cdr listdiff)
		))

; Return the first element of listdiff. It is an error if listdiff has no
; elements.
(define (car-ld listdiff)
	(cond
		; Error if not a listdiff
		[(not (listdiff? listdiff)) (display "error\n")]
		; Error if empty listdiff
		[(null-ld? listdiff) (display "error\n")]
		[else (car (car listdiff))]
		))
