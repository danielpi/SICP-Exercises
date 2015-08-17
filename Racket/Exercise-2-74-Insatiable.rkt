#lang racket

; Exercise 2.74
; Insatiable Enterprises, Inc., is a highly decentralized conglomerate company
; consisting of a large number of independent divisions located all over the
; world. The company's computer facilities have just been interconnected by
; means of a clever network-interfacing scheme that makes the entire network
; appear to any user to be a single computer. Insatiable's president, in her
; first attempt to exploit the ability of the network to extract administrative
; information from division files, is dismayed to discover that, although all
; the divisions files have been implemented as data structures in Scheme, the
; particular data structure used varies from division to division. A meeting of
; division managers is hastily called to search for a strategy to integrate
; the files that will satisfy headquarters' needs while preserving the existing
; autonomy of the divisions.

; Show how such a strategy can be implemented with data-directed programming.
; As an example, suppose that each division's personnel records consist of a
; single file, which contains a set of records keyed on employees' names. The
; structure of the set varies from division to division. Furthermore, each
; employee's record is itself a set (structured differently from division to
; division) that contains information keyed under identifiers such as address
; and salary.


; First up lets recreate the actual issue. We need to be able to create the data
; from two divisions that use different data structures to store their records

; Engineering
(define (make-entry key value)
  (list key value))
(define (key entry)
  (car entry))
(define (value entry)
  (cadr entry))

(define (element-of-set? x set)
  (cond ((null? set) false)
        ((equal? x (car set)) true)
        (else (element-of-set? x (cdr set)))))

(define (adjoin-set x set)
  (if (element-of-set? x set)
      set
      (cons x set)))

(define (lookup given-key set-of-records)
  (cond ((null? set-of-records) false)
        ((equal? given-key (key (car set-of-records)))
         (car set-of-records))
        (else (lookup given-key (cdr set-of-records)))))

; Employee Record
; Create
(define (create-employee-record name address salary)
  (adjoin-set (make-entry 'salary salary)
              (adjoin-set (make-entry 'address address)
                          (adjoin-set (make-entry 'name name) '()))))
; Read
(define (name record)
  (value (lookup 'name record)))
(define (address record)
  (value (lookup 'address record)))
(define (salary record)
  (value (lookup 'salary record)))

; Division file
; create,
(define (add-employee employee-record database)
  (adjoin-set (make-entry (name employee-record) employee-record) database))
; read, 
(define (lookup-employee name database)
  (value (lookup name database)))
; update, delete
   
(define engineering-file (add-employee (create-employee-record "Daniel" "Brisbane" "$100") 
             (add-employee (create-employee-record "Sarah" "Sydney" "$50")
             (add-employee (create-employee-record "Jane" "Perth" "$150")
             (add-employee (create-employee-record "Robert" "Darwin" "$50") '())))))

;engineering-file 
;(salary (lookup-employee "Daniel" engineering-file))

(define production-file (add-employee (create-employee-record "Daniel" "Mackay" "$47") 
             (add-employee (create-employee-record "Simon" "Melbourne" "$500")
             (add-employee (create-employee-record "Jamie" "Ballarat" "$154")
             (add-employee (create-employee-record "Robin" "Adelaide" "$12") '())))))
;production-file

; In particular
; a. Implement for headquarters a get-record procedure that retreives a 
;    specified personnel file. The procedure should be applicable to any
;    division's file. Explain how the individual divisions' files should 
;    be structured. In particular, what type information must be supplied?

; b. Implement for headquarters a get-salary procedure that returns the
;    salary information from a given employee's record from any division's
;    personnel file. How should the record be structured in order to make
;    this operations work?

(define global-array '())

(define (put op type item)
  (define (put-helper k array)
    (cond ((null? array) (list(make-entry k item)))
          ((equal? (key (car array)) k) array)
          (else (cons (car array) (put-helper k (cdr array))))))
  (set! global-array (put-helper (list op type) global-array)))

(define (get op type)
  (define (get-helper k array)
    (cond ((null? array) #f)
          ((equal? (key (car array)) k) (value (car array)))
          (else (get-helper k (cdr array)))))
  (get-helper (list op type) global-array))

(define (attach-tag type-tag contents)
  (cons type-tag contents))
(define (type-tag datum)
  (if (pair? datum)
      (car datum)
      (error "Bad tagged datum: TYPE-TAG" datum)))
(define (contents datum)
  (if (pair? datum)
      (cdr datum)
      (error "Bad tagged datum: CONTENTS" datum)))

(define (apply-generic op name file)
  (let ((division-name (type-tag file)))
    (let ((proc (get op division-name)))
      (if proc 
          (proc name file)
          (error "no result")))))

(define (apply-generic1 op record)
  (let ((division-name (type-tag record)))
    (let ((proc (get op division-name)))
      (if proc 
          (proc record)
          (error "no result")))))


; Each division needs to write a package that will install their functions
(define (install-engineering-package)
  (define (get-record name file)
    (if (lookup name (cdr file))
        (tag (lookup name (cdr file)))
        false))
        
  (define (get-name record)
    (value (lookup 'name record)))
  (define (get-salary record)
    (value (lookup 'salary (caddr record))))
    
  (define (tag x) (attach-tag 'engineering x))
  
  (put 'get-record 'engineering get-record)
  (put 'get-name 'engineering get-name)
  (put 'get-salary 'engineering get-salary)
  
  'done)

(define (install-production-package)
  (define (get-record name file)
    (if (lookup name (cdr file))
        (tag (lookup name (cdr file)))
        false))
  (define (get-name record)
    (value (lookup 'name record)))
  (define (get-salary record)
    (value (lookup 'salary (caddr record))))
    
  (define (tag x) (attach-tag 'production x))
  
  (put 'get-record 'production get-record)
  (put 'get-name 'production get-name)
  (put 'get-salary 'production get-salary)
  
  'done)

(define (get-record name file)
  (apply-generic 'get-record name file))
(define (get-salary record)
  (apply-generic1 'get-salary record))



; Install the packages
(display "Install the packages")(newline)
(install-engineering-package)
(install-production-package)

; Tag the seperate divisions files
(define tagged-engineering-file (attach-tag 'engineering engineering-file))
(define tagged-production-file (attach-tag 'production production-file))

(display "Find Daniel in Engineering")(newline)
(get-record "Daniel" tagged-engineering-file)
(get-salary (get-record "Daniel" tagged-engineering-file))

(display "Find Robin in Production")(newline)
(get-record "Robin" tagged-production-file)
(get-salary (get-record "Robin" tagged-production-file))



; c. Implement for headquarters a find-employee-record procedure. This 
;    should search all the divisions' files for the record of a given
;    employee and return the record. Assume that this procedure takes
;    as arguments an employee's name and a list of all the divisions'
;    files. 

(define (find-employee-record name list-of-files)
  (filter (lambda (result) result) (map (lambda (file) (get-record name file)) list-of-files)))

(display "Find all Daniel's at the company")(newline)
(find-employee-record "Daniel" (list tagged-engineering-file tagged-production-file))
                      
; d. When Insatiable takes over a new company, what changes must be made
;    in order to incorporate the new personnel information into the central
;    system?

; We need a new install package function to be written