
; *********************************************
; *  314 Principles of Programming Languages  *
; *  Fall 2015                                *
; *  Student Version                          *
; *********************************************

;; contains "ctv", "A", and "reduce" definitions
(load "include.ss")

;; contains simple dictionary definition
(load "test-dictionary.ss")

;; -----------------------------------------------------
;; HELPER FUNCTIONS

;; *** CODE FOR ANY HELPER FUNCTION GOES HERE ***

(define getKey
  (lambda (w) 
    (if (= (length w) 1) (+ (* 31 5387) (ctv(car w))) (+ (* 31 (getKey(cdr w))) (ctv (car w))))
    )
  )

(define mapping
  (lambda (dictonary hMap)
    (flatten (map (lambda (n) (append (map (lambda (m) (m n)) hMap))) dictonary))
    )
  )



;; -----------------------------------------------------
;; KEY FUNCTION

(define key
  (lambda (w)
    (getKey (reverse w))
    )
  )
;; -----------------------------------------------------
;; EXAMPLE KEY VALUES
;;   (key '(h e l l o))     = 154238504134
;;   (key '(w a y))         = 160507203 
;;   (key '(r a i n b o w)) = 148230379423562

;; -----------------------------------------------------
;; HASH FUNCTION GENERATORS

;; value of parameter "size" should be a prime number
(define gen-hash-division-method
  (lambda (size) ;; range of values: 0..size-1
 		(lambda (k)
			(modulo (key k) size)
		)
    )
)

;; value of parameter "size" is not critical
;; Note: hash functions may return integer values in "real"
;;       format, e.g., 17.0 for 17

(define gen-hash-multiplication-method
	(lambda (size)
		(lambda (k)
			(floor (* size (- (* (key k) A) (floor (* (key k) A)))))
		)	
	)
)


;; -----------------------------------------------------
;; EXAMPLE HASH FUNCTIONS AND HASH FUNCTION LISTS

(define hash-1 (gen-hash-division-method 70111))
(define hash-2 (gen-hash-division-method 89997))
(define hash-3 (gen-hash-multiplication-method 700224))
(define hash-4 (gen-hash-multiplication-method 900))

(define hashfl-1 (list hash-1 hash-2 hash-3 hash-4))
(define hashfl-2 (list hash-1 hash-3))
(define hashfl-3 (list hash-2 hash-3))

;; -----------------------------------------------------
;; EXAMPLE HASH VALUES
;;   to test your hash function implementation
;;
;;  (hash-1 '(h e l l o))     ==> 53236
;;  (hash-1 '(w a y))         ==> 23124 
;;  (hash-1 '(r a i n b o w)) ==> 17039 
;;
;;  (hash-2 '(h e l l o))     ==> 25588 
;;  (hash-2 '(w a y))         ==> 42552 
;;  (hash-2 '(r a i n b o w)) ==> 70913 
;;
;;  (hash-3 '(h e l l o))     ==> 415458.0 
;;  (hash-3 '(w a y))         ==> 390702.0 
;;  (hash-4 '(r a i n b o w)) ==> 503286.0 
;;
;;  (hash-4 '(h e l l o))     ==> 533.0
;;  (hash-4 '(w a y))         ==> 502.0
;;  (hash-4 '(r a i n b o w)) ==> 646.0


;; -----------------------------------------------------
;; SPELL CHECKER GENERATOR

(define gen-checker
  (lambda (hList dict)
    (let ((b (flattenHash dict hList)))
      (lambda (w)
        (let ((w (flattenHash (list w) hList)))
          (if (andmap (lambda (mapedW) (member mapedW b)) w) #t #f)
          )))))


;; -----------------------------------------------------
;; EXAMPLE SPELL CHECKERS

(define checker-1 (gen-checker hashfl-1 dictionary))
(define checker-2 (gen-checker hashfl-2 dictionary))
(define checker-3 (gen-checker hashfl-3 dictionary))

;; EXAMPLE APPLICATIONS OF A SPELL CHECKER
;;
;;  (checker-1 '(a r g g g g)) ==> #f
;;  (checker-2 '(h e l l o)) ==> #t

