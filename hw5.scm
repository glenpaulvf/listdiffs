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

; Return a listdiff containing all but the first element of listdiff. It is an
; error if listdiff has no elements.
(define (cdr-ld listdiff)
	(cond
		; Error if not a listdiff
		[(not (listdiff? listdiff)) (display "error\n")]
		; Error if empty listdiff
		[(null-ld? listdiff) (display "error\n")]
		[else (cons (cdr (car listdiff)) (cdr listdiff))]
		))

; Return a newly allocated listdiff of its arguments.
(define (listdiff . obj)
	(let ((car-obj (list (car obj)))) ; Ensure same object
		(cons 
			(append
				(cons (car obj) (cdr obj))
				car-obj)
			car-obj)
		))

; Return the length of listdiff.
(define (length-ld listdiff)
	(cond
		; Error if not a listdiff
		[(not (listdiff? listdiff)) (display "error\n")]
		; 0 if empty listdiff
		[(null-ld? listdiff) 0]
		; Count
		[else
			(let length-ld-count ([listdiff listdiff] [count 0])
				(cond
					[(eq? (car listdiff) (cdr listdiff)) count]
					[else (length-ld-count (cdr-ld listdiff) (+ count 1))]))]
		))

; Return a listdiff consisting of the elements of the first listdiff followed
; by the elements of the other listdiffs. The resulting listdiff is always
; newly allocated, except that it shares structure with the last argument.
(define (append-ld . listdiff)
	(cond
		[(= (length (cdr listdiff)) 1)
			(cons
				(append
					(listdiff->list (car listdiff))
					(car (car (cdr listdiff))))
				(cdr (car (cdr listdiff))))]
		[else
			(append-ld
				(cons
					(append
						(listdiff->list (car listdiff))
						(car (car (cdr listdiff))))
					(cdr (car (cdr listdiff))))
				(car (cdr (cdr listdiff))))]
		))

; Return listdiff, except with the first k elements omitted. If k is zero,
; return listdiff. It is an error if k exceeds the length of listdiff.
(define (list-tail-ld listdiff k)
	(cond
		; Error when k is less than 0
		[(< k 0) (display "error\n")]
		; Listdiff when k is 0
		[(= k 0) listdiff]
		; Error when k is greater than listdiff length
		[(< (length-ld listdiff) k) (display "error\n")]
		[else (cons (car-ld listdiff) (list-tail-ld (cdr-ld listdiff) (- k 1)))]
		))

; Return a listdiff that represents the same elements as list.
(define (list->listdiff list)
	(append
		(list (append list (list (car list))))
		(list (car list))
		))

; Return a list that represents the same elements as listdiff.
(define (listdiff->list listdiff)
	(cond
		; Error if not a listdiff
		[(not (listdiff? listdiff)) (display "error\n")]
		; Count
		[else
			(let ld->l ([listdiff listdiff] [l '()])
				(cond
					[(eq? (car listdiff) (cdr listdiff)) l]
					[else (ld->l (cdr-ld listdiff) (append l (list (car (car listdiff)))))]))]
		))

