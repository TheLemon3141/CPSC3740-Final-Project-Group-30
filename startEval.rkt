#lang racket

;Interpreter for a subset of racket.

(define (startEval prog)
  (if (pair? prog);If it's a pair, execute as normal...
;+ operation
  (if (equal? (car prog) '+)                                       
      (+ (list-ref prog 1) (list-ref prog 2));if true
      ;if false, do the rest of the program
;- operation
  (if (equal? (car prog) '-)                                       
      (- (list-ref prog 1) (list-ref prog 2));if true
      ;if false, do the rest of the program
;* operation
  (if (equal? (car prog) '*)
      (* (list-ref prog 1) (list-ref prog 2));if true
      ;if false, do the rest of the program
;/ operation (with divide-by-zero checking
  (if (equal? (car prog) '/)                                       
      (if (equal? (list-ref prog 2) 0);If they're trying to divide by 0
          '"FOOL! YOU CAN'T DIVIDE BY ZERO"
          (/ (list-ref prog 1) (list-ref prog 2));if true
      );End of if list-ref 2 = 0
;/ #t
  (if (equal? (car prog) '#t)
      #t
      ;If not, do the rest of the program
;/ #f
  (if (equal? (car prog) '#f)
      #f
      ;If not, do the rest of the program
;/ equal?
  (if (equal? (car prog) 'equal?)
      (if (equal? (list-ref prog 1) (list-ref prog 2))
          (startEval #t)
          (startEval #f)
       )
      ;If not, do the rest of the program
;/ if (needs work)
  (if (equal? (car prog) 'if)
      (if (car(cdr prog))
          (startEval (list-ref (cdr prog)) 1)
          (startEval (list-ref (cdr prog)) 2)
      )
;/ ' (needs work)
  (if (equal? (car prog) '\')   ;This one's not quite working
  (quote (prog))
 (cons '"FOOL! THE CHAR " (cons (car prog) '" WASN'T DEFINED!"));If the first character wasn't any of the defined functions
);end of car is '
);End of car is if
);End of car is equal?
);End of car is #f
);End of car is #t
);end of car is /
);end of car is *
);end of if car is -   
);end of if car is +
  prog ;If it's not a pair, return the program using default Racket.
);End of if prog is a pair
);End of whole function


;Empty frame syntax: (startEval '())
