;; Exercise 1.1. Below is a sequence of expressions. What is the result printed by the interpreter in response to each expression? Assume that the sequence is to be evaluated in the order in which it is presented.

10 ;Value: 10
(+ 5 3 4) ;Value: 12
(- 9 1) ;Value: 8
(/ 6 2) ;Value: 3
(+ (* 2 4) (- 4 6)) ;Value: 6
(define a 3) ;Value: a
(define b (+ a 1)) ;Value: b
(+ a b (* a b)) ;Value: 19
(= a b) ;Value: #f
(if (and (> b a) (< b (* a b)))
    b
    a) ;Value: 4
(cond ((= a 4) 6)
      ((= b 4) (+ 6 7 a))
      (else 25)) ;Value: 16
(+ 2 (if (> b a) b a)) ;Value: 6
(* (cond ((> a b) a)
         ((< a b) b)
         (else -1))
   (+ a 1)) ;Value: 16

;; Exercise 1.2. Translate the following expression into prefix form.

(/ (+ 5 4 (- 2 (- 3 (+ 6 (/ 4 3))))) (* 3 (- 6 2) (- 2 7))) ;Value: -23/90

;; Exercise 1.3.  Define a procedure that takes three numbers as arguments and returns the sum of the squares of the two larger numbers.

(define (squire x)
  (* x x))

(define (sum-squire-two-larger x y z)
  (if (> x y)
      (if (> y z)
          (+ (squire x) (squire y))
          (+ (squire x) (squire z)))
      (if (> x z)
          (+ (squire y) (squire x))
          (+ (squire y) (squire z)))))
(sum-squire-two-larger 0 5 1)

;; Exercise 1.4.  Observe that our model of evaluation allows for combinations whose operators are compound expressions. Use this observation to describe the behavior of the following procedure:

(define (a-plus-abs-b a b) ;; Можно выбирать какую операцию применить в зависимости от условия, то есть функции можно рассматривать как объекты
  ((if (> b 0) + -) a b))

(a-plus-abs-b 5 -6)

;; Exercise 1.5.  Ben Bitdiddle has invented a test to determine whether the interpreter he is faced with is using applicative-order evaluation or normal-order evaluation. He defines the following two procedures:


(define (p) (p)) ;; scheme работает по applicative-order evaluation
;; при попытке вычислить (test 0 (p)) интерпритатор пытается сначала
;; посчитать агрументы функции test, что приводит к бесконечному рекурсивному вызову (define (p) (p))
;; если мы это была специальная форма, а не обычный define, то нам не пришлось бы вычислять (p) и результатом был бы 0
(define (test x y)
  (if (= x 0)
      0
      y))

(test 0 (p))


;; Exercise 1.6.
;; sqrt Методом Ньютона.
(define (average x y)
  (/ (+ x y) 2))

(define (improve guess x)
  (average guess (/ x guess)))

(define (good-enough? guess x)
  (< (abs (- (square guess) x)) 0.001)) ;; приближение не для всех чисел, но пока сойдет

(define (sqrt-iter guess x)
  (if (good-enough? guess x)
      guess
      (sqrt-iter (improve guess x)
                 x)))

(define (sqrt x)
  (sqrt-iter 1.0 x))
(sqrt 2) ;Value: 1.4142156862745097

(define (new-if predicate then-clause else-clause)
  (cond (predicate then-clause)
        (else else-clause)))
(new-if (= 2 3) 0 5)
(new-if (= 1 1) 0 5)

(define (sqrt-iter guess x)
  (new-if (good-enough? guess x)
          guess
          (sqrt-iter (improve guess x)
                     x)))
(sqrt 2)

;; Exercise 1.7. Ограничение приведенное в книге не подходит для малых чисел а так же для больших. Так же в реальном мире, арифметические операции всегда выполняются с погрешностью. Это делает наш тест неподходящим для больших чисел. Поясните данное утверждение, приведите примеры когда исходный вариант программы работает плохо. В качестве альтернативной реализации напишиете программу которая считает результат подходящим когда изменение предположения между двумя итерациями много меньше предведущего предполагаемого решения, то есть мы будем получать очень малое уточнение за все последующие операции. Попробуйте получить результат с помощью новой процедуры.

(define (sqrt x)
  (define (good-enough? guess x)
    (< (abs (- (square guess) x)) 0.001))
  (define (average x y)
    (/ (+ x y) 2))
  (define (improve guess x)
    (average guess (/ x guess)))
  (define (sqrt-iter guess x)
    (if (good-enough? guess x)
        guess
        (sqrt-iter (improve guess x) x)))
  (sqrt-iter 1.0 x))

(sqrt 0.01) ;Value: .10032578510960605 Ok
(sqrt 0.001) ;Value: .04124542607499115 Работает плохо "реальное значение" 0.03162277660168379

(sqrt 10000000) ;Value: 3162.277660168379, Ok "реальное значение" 3162.2776601683795
(sqrt 12345678901234567) ;; Слишком много итераций, программа зависает.


;; новая версия

(define (good-enough? guess prev-guess)
  (< (abs (- guess prev-guess)) 0.001))
(good-enough? 1.0 1.0001)


(define (sqrt x)
  (define (good-enough? guess prev-guess)
    (< (abs (- guess prev-guess)) 0.001)) ;; изменение guess менее десятой доли процента
  (define (average x y)
    (/ (+ x y) 2))
  (define (improve guess x)
    (average guess (/ x guess)))
  (define (sqrt-iter g prev-g x)
    (if (good-enough? g prev-g)
        g
        (sqrt-iter (improve g x) g x)))
  (sqrt-iter 1.0 1.002 x))

(sqrt 2)
(sqrt 12345678901234567) ;Value: 111111110.6111111
(sqrt 0.001) ;Value: .03162278245070105

Видим что стало работать быстро на больших числах и точно на любых с заданной погрешностью.


;; Exercise 1.8. Метод Ньютона для кубического корня основан на факте что приближенное значение можно искать по формуле


;; Use this formula to implement a cube-root procedure analogous to the square-root procedure. (In section 1.3.4 we will see how to implement Newton's method in general as an abstraction of these square-root and cube-root procedures.)

(define (cube-root x)
  (define (good-enough? guess prev-guess)
    (< (abs (- guess prev-guess)) 0.001))
  (define (average x y)
    (/ (+ x y) 2))
  (define (improve guess)
    (average guess (/ (+ (/ x (square guess)) (* 2 guess)) 3)))
  (define (sqrt-iter g prev-g)
    (if (good-enough? g prev-g)
        g
        (sqrt-iter (improve g) g)))
  (sqrt-iter 1.0 1.002))

(cube-root 27)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 1.2  Procedures and the Processes They Generate
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (factorial n)
  (if (= n 1)
      1
      (* n (factorial (- n 1)))))
(factorial 4)

(define (factorial n)
  (fact-iter 1 1 n))

(define (fact-iter product counter max-count)
  (if (> counter max-count)
      product
      (fact-iter (* counter product)
                 (+ counter 1)
                 max-count)))
(factorial 4)

;;  Exercise 1.9

(define (+ a b)
  (if (= a 0)
      b
      (inc (+ (dec a) b))))

(+ 6 5)
(inc (+ 5 5))
(inc (inc (+ 4 5)))
(inc (inc (inc (+ 3 5))))
(inc (inc (inc (inc (+ 2 5)))))
(inc (inc (inc (inc (inc (+ 1 5))))))
(inc (inc (inc (inc (inc (inc (+ 0 5))))))) ; 5 + 6inc 11


(define (+ a b)
  (if (= a 0)
      b
      (+ (dec a) (inc b))))
(+ 6 5)
(+ 5 6)
(+ 4 7)
(+ 3 8)
(+ 2 9)
(+ 1 10)
(+ 0 11) ; 11

;; Ex 1.10

(define (A x y)
  (cond ((= y 0) 0)
        ((= x 0) (* 2 y))
        ((= y 1) 2)
        (else (A (- x 1)
                 (A x (- y 1))))))

(A 1 10) ;Value: 1024
(A 2 4) ;Value: 65536
(A 3 3) ;Value: 65536

(A 2 4) ;
;Aborting!: out of memory
;GC #203 05:56:04: took:   0.20 (100%) CPU,   0.20  (87%) real; free: 16769793
;GC #204 05:56:04: took:   0.10  (33%) CPU,   0.20  (39%) real; free: 16773551
;GC #205 05:56:04: took:   0.20  (40%) CPU,   0.20  (40%) real; free: 16773551

(A 2 1)
(A 2 2)
(A 2 3)
(A 2 4)
(A 2 5)

(define (f n) (A 0 n))

(define (g n) (A 1 n))

(define (h n) (A 2 n))

(define (k n) (* 5 n n))


Дайте краткое математическое определение функций f, g, h. К примеру k(n) = 5*n*n

(f n) = (A 0 n) = (* 2 n)
(g n) = (A 1 n) = (A 0 (A 1 (- n 1))) = (* 2 (A 1 (- n 1))) = это будет 2 в степени n
(h n) = (A 2 n) = (A 1 (A 2 (- n 1))) = 2 в степени (A 2 (- n 1)) = 2 ** 2 в cтепени (A 2 (-n 2), пока
(A 2 4) ;;  = 2 ** 2 ** 2 ** 2 ;; 65536
(A 2 5) ;;  = 2 ** 2 ** 2 ** 2 ** 2;;


;; 1.2.2  Tree Recursion

(define (fib n)
  (cond ((= n 0) 0)
        ((= n 1) 1)
        (else (+ (fib (- n 1))
                 (fib (- n 2)))))) ;; bad code

(define (fib n)
  (fib-iter 1 0 n))

(define (fib-iter a b count)
  (if (= count 0)
      b
      (fib-iter (+ a b) a (- count 1))))


(fib 6)

;; Example: Counting change
;; Сколькими способами мы можем внесни изменение в доллар США
;; если у нас есть монеты 50 25 10 5 и 1 цент
;; Есть простое решение через рекурсивную процедуру
;; если a = 0 1 способ внести изменение
;; если a < 0 0 способов
;; если n = 0, 0 способов внести изменение
(define (count-change amount)
  (cc amount 5))
(define (first-denomination kinds-of-coins)
  (cond ((= kinds-of-coins 1) 1)
        ((= kinds-of-coins 2) 5)
        ((= kinds-of-coins 3) 10)
        ((= kinds-of-coins 4) 25)
        ((= kinds-of-coins 5) 50)))

(define (cc amount kinds-of-coins)
  (cond ((= amount 0) 1)
        ((or (< amount 0) (= kinds-of-coins 0)) 0)
        (else (+ (cc amount
                     (- kinds-of-coins 1)) ;; изменение берем разные монетки
                 (cc (- amount
                        (first-denomination kinds-of-coins)) ;; изменяем сумму на значение монетки
                     kinds-of-coins)))))
(count-change 100)
(cc 100 5)
(+ (cc 100 4) (cc 99 5 ))
(+ (+ (cc 100 3) (cc 75 4)) (+ (cc 99 4) (cc 98 4)))
;; ... и так проссумирруем все варианты


;; Ex. 1.11
(define (f n)
  (cond ((< n 3) n)
        ((>= n 3) (+ (f (- n 1)) (* 2 (f (- n 2))) (* 3 (f (- n 3)))))))
(f 2)
(f 3)
(f 5) ;; = (+ (f 4) (f 3) (f 2))
(f 4) ; (+ (f 3) (f 2) (f 1)) ;Value: 6
(>= 5 3)

;; a b c
;; b c (+ c (* 2 b) (* 3 a))
(define (f n)
  (define (iter a b c count)
    (if (= count 0)
        a
        (iter b c (+ c (* 2 b) (* 3 a)) (- count 1))))
  (iter 0 1 2 n))


f5
(+ f4 f3 f2)
f4
(+ f3 2f2 3f1)  (+ (f2 2f1 2f0) 2f2 3f1)
f3 = (f2 2f1 2f0)


(2 + 2*1 3*0) 2 1  ;; f3 4 (4 2 1)
f4 (f3 2f2 3f1)



2 1 0

(define (f n)
   (define (f-iter a b c count)
     (if (< count 3)
         a
         (f-iter (+ a (* 2 b) (* 3 c)) a b (- count 1))))
   (if (< n 3)
       n
       (f-iter 2 1 0 n)))



(define (f n)
  (cond ((< n 3) n)
        ((>= n 3) (+ (f (- n 2)) (* 3 (f (- n 2))) (* 4 (f (- n 3)))))))

(f 5)
(f 6)

(define (f n)
  (define (iter a b c count)
    (if (= count 0)
        a
        (iter b c (+ b (* 3 b) (* 4 a)) (- count 1))))
  (iter 0 1 2 n))


(f 5)
(f 6)

(f 3)
(f 4)
(f 5)

a b c
a = a + b + c
b = b
c = c
count = (n - 3 = 0)

(define (f n)
  (f-iter n 0 0 (-n 3)))
(define (f-iter a b c count)
  (if (< count 0)
      a
      (f-iter (+ a b c) (+ b 2) (+ c 3) (- count 1))))
(f 4)


;; Ex 1.12

в строке с номером n
в строке первое число равно 1
2-ое число равно n
третье число равно треугольному числу T n − 1 = n ( n − 1 ) 2 {\displaystyle \textstyle T_{n-1}={\frac {n(n-1)}{2}}} \textstyle T_{{n-1}}={\frac {n(n-1)}{2}}, что также равно сумме номеров предшествующих строк.


(define (factorial n)
  (if (= n 1)
      n
      (* n (factorial (- n 1)))))
(factorial 5)


(define (pascal n m)
  (if (or (= n 0) (= m n) (= m 0))
      1
      (/ (factorial n) (* (factorial m) (- n m)))))

(pascal 1 0)
(pascal 2 0)
(pascal 2 1)
(pascal 2 2)
(pascal 3 1)
(pascal 4 2)
(pascal 5 2)
         1                   0
       1   1                 1
      1  2  1                2
    1  3  3  1               3
   1  4  6  4  1             4
 1  5  10 10  5 1            5
1  6 15  20 15  6 1          6
(define pascal (n)



(define (pascal y x)
  (cond ((or (< x 0) (< y 0) (> x y)) #f)
        ((or (= x 0) (= x y)) 1)
        (else (+ (pascal (- y 1) (- x 1))
                 (pascal (- y 1) x)))))

(1 + x)**3
(1 + 2x + xx) (1 + x)
1 + 2x + xx + x + 2xx + xxx
1 + 3x + 3xx + xxx

(pascal 4 -1)
(pascal 0 0)
(pascal 1 0)
(pascal 1 1)
(pascal 2 0)
(pascal 2 1)
(pascal 2 2)
(pascal 3 1)
(pascal 4 1)
(pascal 4 2)

;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 1.3
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (p) (p))
(define (test x y)
  (if (= x 0)
      0
      y))

(test 0 (p))

(define dx 0.00001)
(define (deriv g) ;; производная
  (lambda (x)
    (/ (- (g (+ x dx)) (g x))
       dx)))

(define (cube x) (* x x x))
((deriv cube) 5) ;; производная от куба в точке 5 ;Value: 75.00014999664018; совсем неплохо и рядом

(define tolerance 0.00001)
(define (fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) tolerance))
  (define (try guess)
    (let ((next (f guess)))
      (if (close-enough? guess next)
          next
          (try next))))
  (try first-guess))
(fixed-point cos 1.0)

(define (newton-transform g)
  (lambda (x)
    (- x (/ (g x) ((deriv g) x)))))
(define (newtons-method g guess)
  (fixed-point (newton-transform g) guess))

(define (sqrt x)
  (newtons-method (lambda (y) (- (square y) x))
                  1.0))

(define (cube-root x)
  (newtons-method (lambda (y) (- (cube y) x))
                  1.0))
(sqrt 2)
(cube-root 27)
