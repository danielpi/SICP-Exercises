#lang racket

; 3.1 Assignment and Local State

; We ordinarily view the world as populated by independent objects, each of which has
; a state that changes over time. An object is said to "have state" if its behavior is
; influenced by its history. A bank account, for example, has state in that the answer
; to the question "Can I withdraw $100?" depends upon the history of deposit and
; withdrawal transactions. We can characterize an object's state by one or more state
; variables, which among them maintain enough information about history to determine
; the object's current behavior. In a simple banking system, we could characterize the
; state of an account by an account by a current balance rather than by remembering
; the entire history of account transactions.

; In a system composed of many objects, the objects are rarely completely independent.
; Each may influencethe states of others through interactions, which serve to couple
; the state variables of one object to those of other objects. Indeed, the view that a
; system is composed of separate objects is most useful when the state variables of the
; system can be grouped into closely coupled subsystems that are only loosely coupled
; to other subsystems.

; This view of a system can be a powerful framework for organising computational models
; of the system. For such a model to be modular, it should be decomposed into computational
; objects that model the actual objects in the system. Each computational object must have
; its own local state variables describing the actual object's state. Since the states of
; objects in the system being modeled change over time, the state variables of the
; corresponding computaional objects must also change. If we choose to model the flow of
; time in the system by the elapsed time in the computer, then we must have a way to
; construct computational objects whose behaviors change as our programs run. In particular,
; if we wish to model state variables by ordinary symbolic names in the programming
; language, then the language must provide an assignment operator to enable us to change
; the value associated with a name.

; 3.1.1 Local State Variables
; To illustrate what we mean by having a computational object with time varying state,
; let us model the situation of withdrawing money from a bank account. We will do this
; using a procedure withdraw, which takes as argument an amount to be withdrawn. If there
; is enough money in the account to accomodate the withdrawal, then withdraw should
; return the balance remaining after the withdrawal. Otherwise, withdraw should return the
; message Insufficient funds. For example, if we begin with $100 in the account, we should
; obtain the following sequence of responses using withdraw:

; (withdraw 25)
; 75
; (withdraw 25)
; 50
; (withdraw 60)
; "Insufficient funds"
; (withdraw 15)
; 35

; Observe that the expression (withdraw 25), evaluated twice, yeilds different values.
; This is a new kind of behavior for a procedure. Until now, all out procedures could
; be viewed as specifications for computing mathematical functions. A call to a procedure
; computed the value of the function applied to the given arguments, and two calls to the
; same procedure with the same arguments always produced the same result.

; To implement withdraw, we can use a variable balance to indicate the balance of money in
; the account and define withdraw as a procedure that accesses balance. The withdraw
; procedure checks to see if balance is at least as large as the requested amount. If so,
; withdraw decrements balance by amount and returns the new value of balance. Otherwise,
; withdraw returns the Insufficient funds message. Here are the difinitions of balance
; and withdraw:

(define balance 100)
(define (withdraw amount)
  (if (>= balance amount)
      (begin (set! balance (- balance amount))
             balance)
      "Insufficient funds"))

; Decrementing balance is accomplished by the expression
; (set! balance (- balance amount))


; This uses the set! special form, whose syntax is
; (set! <name> <new-value>)

; Here <name> is a symbol and <new-value. is any expression. set! changes <name> so that
; its value is the result obtained by evaluating <new-value>. In the case at hand, we are
; changing balance so that its new value will be the result of subtracting amount from the
; previous value of balance.

; withdraw also uses the begin special form to cause two expressions to be evaluated in the
; case where the if test is true: first decrementing balance and then returning the value
; of balance. In general, evaluating the expression

; (begin <exp1> <exp2> ... <expk>)

; cause the expressions <exp1> through <expk> to be evaluated in sequence and the value of
; the final expression <expk> to be returned as the value of the entire begin form.

; Although withdraw works as desired, the variable balance presents a problem. As specified
; above, balance is a name defined in the global environment and is freely accessible to be
; examined or modified by any procedure. It would be much better if we could somehow make
; balance internal to withdraw, so that withdraw would be the only procedure that could
; access balance directly and any other procedure could access balance only indirectly
; (through calls to withdraw). This would more accurately model the notion that balance is a
; local state variable used by withdraw to keep track of the state of the account.

; We can make balance internal to withdraw by reqriting the definition as follows:

(define new-withdraw
  (let ((balance 100))
    (lambda (amount)
      (if (>= balance amount)
          (begin (set! balance (- balance amount))
                 balance)
          "Insufficient funds"))))

; What we have done here is use let to establish an environment with a local variable balance,
; bound to the initial value 100. Within this local environment, we use lambda to create a
; procedure that takes amount as an argument and behaves like our previous withdraw procedure.
; This procedure-returned as the result of evaluating the let expression-is new-withdraw, which
; behaves in precisely the same way as withdraw but whose variable balance is not accessible
; by any other procedure.

; Combinint set! with local variables is the general programming technique we will use for
; constructing computational objects with local state. Unfortunately, using this technique
; raises a serious problem: When we first introduced procedures, we also introduced the
; substitution model of evaluation (Section 1.1.5) to provide an interpretation of what
; procedure application means. We said that applying a procedure should be interpreted as
; evaluating the body of the procedure with the formal parameters replaced by their values.
; The trouble is that, as soon as we introduce assignment into our language, substitution
; is no longer an adequate model of procedure application. (We will see why this is so in
; Section 3.1.3.) As a consequence, we technically have at this point no way to understand
; why the new-withdraw procedure behaves as clained above. In order to really understand
; a procedure such as new-withdraw, we will need to develop a new model of procedure
; application. In Section 3.2 we will introduce such a model, together with an explanation
; of set! and local variables. First, however, we examine some variations on the theme
; established by new-withdraw.

; The following procedure, make-withdraw, creates "withdrawal processor." The formal
; parameter balance in make-withdraw specifies the initial amount of money in the account.

(define (make-withdraw balance)
  (lambda (amount)
    (if (>= balance amount)
        (begin (set! balance (- balance amount))
               balance)
        "Insufficient funds")))

; make-withdraw can be used as follows to create two objects W1 and W2:

(define W1 (make-withdraw 100))
(define W2 (make-withdraw 100))

(W1 50) ; 50
(W2 70) ; 30
(W2 40) ; "Insufficient funds"
(W1 40) ; 10

; Observe that W1 and W2 are completely independent objects, each with its own local state
; variable balance. Withdrawals from one do not affect the other.

; We can also create objects that handle deposits as well withdrawals, and thus we can
; represent simple bank accounts. Here is a procedure that returns a "bank-account object"
; with a specified initial balance:

(define (make-account balance)
  (define (withdraw amount)
    (if (>= balance amount)
        (begin (set! balance (- balance amount))
               balance)
        "Insufficent funds"))
  (define (deposit amount)
    (set! balance (+ balance amount))
    balance)
  (define (dispatch m)
    (cond ((eq? m 'withdraw) withdraw)
          ((eq? m 'deposit) deposit)
          (else (error "Unkown request: MAKE-ACCOUNT"
                       m))))
  dispatch)

; Each call to make-account sets up an environment







