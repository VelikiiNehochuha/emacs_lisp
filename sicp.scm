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
(count-change 1000)
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
;;; 1.2.3  Порядок роста
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; 1.14

;; (cc 11 5)
;; (cc 11 4) | (cc -39 5)
;; (cc 11 3) (cc -14 4) | 0
;; (cc 11 2) (cc 1 3) | 0
;; (cc 11 1) (cc 6 2) | (cc 1 2) (cc -9 3)
;; (cc 11 0) (cc 10 1) | (cc 6 1) (cc 1 1) | (cc 1 1) (cc -4 2) | 0
;; 0 | (cc 10 0) (cc 9 0) | (cc 6 0) (cc 5 1) | (cc 1 0) (cc 0 1) | (cc 1 0) (cc 0 1) | 0
;; 0 | 0 | 0 | (cc 5 0) (cc 4 1) | 0 | 0 | 0 | 0
;; 0 | 0 | (cc 4 0) (cc 3 1)
;; 0 | (cc 3 0) (cc 2 1)
;; 0 | (cc 2 0) (cc 1 1)
;; 0 | (cc 1 0) (cc 0 1)
;; 0 | 0


;; Exercise 1.15.  The sine of an angle (specified in radians) can be computed by making use of the approximation sin x x if x is sufficiently small, and the trigonometric identity

(define (cube x) (* x x x))
(define (p x) (- (* 3 x) (* 4 (cube x))))

(define (sine angle)
   (if (not (> (abs angle) 0.1))
       angle
       (p (sine (/ angle 3.0)))))

;; How many times is the procedure p applied when (sine 12.15) is evaluated?

;; 1.2.4  Exponentiation

(define (expt b n)
  (if (= n 0)
      1
      (* b (expt b (- n 1)))))

(define (expt b n)
  (define expt-iter (r count)
    (if (= count 0)
        r
        (expt-iter (* b r) (- count 1))
    ))
  (expt-iter (1 n)))
(expt 3 3)

;; если вычислять квадраты а не каждый раз умножать на b то можно получить время log n

(define (even? n)
  (= (remainder n 2) 0))
(even? 2)
(even? 7)

(define (fast-expt b n)
  (cond ((= n 0) 1)
        ((even? n) (square (fast-expt b (/ n 2))))
        (else (* b (fast-expt b (- n 1))))))
(fast-expt 5 10)

;; разработать быструю экспоненту с итеративным подходом

(define (even? n)
  (= (remainder n 2) 0))

;; 2 4
;; 4 2
;; 8 1

(define (expt b n)
  (define (expt-iter a count)
    (cond ((= count 1) a)
          ((= count 0) a)
          ((>= count (- n 1)) (expt-iter (square b) (/ count 2)))
          (else (expt-iter (square a) (/ count 2)))))
  (if (even? n)
      (expt-iter 1 n)
      (* b (expt-iter 1 (- n 1)))))

(expt 2 4)
(expt 2 0)
(expt 3 150)

;; Ex 1.17
;; (define (* a b)
;;   (if (= b 0)
;;       0
;;       (+ a (* a (- b 1)))))
;; (* 10 4)
;; (+ 10 (* 10 3))
;; (+ 10 (+ 10 (* 10 3)))
;; (+ 10 (+ 10 (+ 10 (* 10 1))))
;; (+ 10 (+ 10 (+ 10 (+ 10 (* 10 0)))))

(define (double x)
  (* x 2))
(double 2)

(define (halve x)
  (/ x 2))
(halve 4)

(define (even? n)
  (= (remainder n 2) 0))
(even? 6)

;; Using these, design a multiplication procedure analogous to fast-expt that uses a logarithmic number of steps.

(define (fast? a b)
  (cond ((or (= b 0) (= a 0)) 0)
        ((= b 1) a)
        ((even? b) (double (fast? a (halve b))))
        (else (+ a (double (fast? a (halve (- b 1))))))))
(fast? 10 0)
(fast? 0 10)
(fast? 10 11)


(define (fast? a b)
  (define (fast-iter asum bsum count)
    (cond ((= bsum 0) asum)
          ((= bsum 1) asum)
          ((even? bsum) (fast-iter (double asum) (halve bsum)))
          (else (fast-iter (+ asum asum) (- bsum 1)))))
  (cond (or (= b 0) (= a 0) 0)
        ((= b 1) a)
        (else (fast-iter a b))))
(fast? 16 18)

;; 1.19 fib-iter log n


(define (fib-iter a b count)
  (if (= count 0)
      b
      (fib-iter (+ a b) a (- count 1))))
(define (fib n)
  (fib-iter 1 0 n))
;; Перезапись a <- a + b, b <- a, назовем это трансформацией T
;; заметим что применение T n раз даст пару Fib(n + 1) и Fib(n)
;; иными словами числа генерируются применением T^n
;; Теперь рассмотрим частный случай T p = 0 q = 1 в преобразовании Tpq.
;; где Tpq преобразует пару (a, b) в a <- b*q + a*q + a*p и b <- b*p + a*q

;; Покажите что если применить такую трансформацию Tpq дважды
;; результат будет тем же как и от единственной трансформации Tp'q' такой же формы, для которой p' и q' выраженны через p и q.

;; Это даст нам возможность ускорить процедуру, как мы делали в fast-exp.
;; Собрав все это вместе закончите процедуру приведенную ниже.


(define (fib n)
  (fib-iter 1 0 0 1 n))
(define (fib-iter a b p q count)
  (cond ((= count 0) b)
        ((even? count)
         (fib-iter a
                   b
                   (+ (square p) (square q))      ; compute p'
                   (+ (* 2 p q) (square q))      ; compute q'
                   (/ count 2)))
        (else (fib-iter (+ (* b q) (* a q) (* a p))
                        (+ (* b p) (* a q))
                        p
                        q
                        (- count 1)))))

(fib 7)


;;; 1.20
;; при нормальном порядке выполнения
(define (gcd a b)
  (if (= b 0)
      a
      (gcd b (remainder a b))))

(remainder 206 40)
;; нормальный порядок
(gcd 206 40)
(if (= 40 0))
(gcd 40 (remainder 206 40))
(if (= (remainder 206 40) 0)) ; 6 + 1 раз
(if (= 6 0))
(gcd (remainder 206 40) (remainder 40 (remainder 206 40)))
(if (= (remainder 40 (remainder 206 40)) 0)) ; 4 + 2 раза
(gcd (remainder 40 (remainder 206 40)) (remainder (remainder 206 40) (remainder 40 (remainder 206 40))))
(if (= (remainder (remainder 206 40) (remainder 40 (remainder 206 40))) 0)) ; 2 + 4 раза
(gcd (remainder (remainder 206 40) (remainder 40 (remainder 206 40))) (remainder (remainder 40 (remainder 206 40)) (remainder (remainder 206 40) (remainder 40 (remainder 206 40)))))
(if (= (remainder (remainder 40 (remainder 206 40)) (remainder (remainder 206 40) (remainder 40 (remainder 206 40)))) 0)); 0; + 7 раз
(if (= 0 0) ...)
(remainder (remainder 206 40) (remainder 40 (remainder 206 40))) ;; 4 раза
;; 18 вычислений remainder
;; при апликативной форме этих вычислений всего 4

;; Теорема Ламе: Если алгоритм Евклида требует k шагов для вычисления НОД, тогда меньшее число в паре должно быть больше или равно k-ому числу Фибоначи

;; Мы можем использовать эту теорему что бы оценить порядок роста для алгоритма Евклида. Пусть n меньшее число из пары. Если процесс занял k шагов, тогда n >= Fib(k) что приближенно \phi^k/\sqrt(5). Поэтому число шагов растет лагорифмически (по основанию \phi) от n. То есть порядок роста \theta(logn)

;; 1.2.6  Пример: тест на простоту
;; эта секция описывает методы проверки на простоту целого n. Один с порядком роста корень из n, второй (вероятнотснтый) log n. Упражнения в конце раздела предполагают использование этих алгоритмов.
(define (smallest-divisor n)
  (find-divisor n 2))
(define (next n)
  (if (= n 2)
      3
      (+ n 2)))
(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
        ((divides? test-divisor n) test-divisor)
        (else (find-divisor n (next test-divisor)))))
(define (divides? a b)
  (= (remainder b a) 0))
(smallest-divisor 10)
(smallest-divisor 199)
(smallest-divisor 1999)
(smallest-divisor 19999)
(define (prime? n)
  (= n (smallest-divisor n)))
(smallest-divisor 11)

;; сложность \sqrt(n)

;; тест ферма
;; Малая теорема Ферма: Если n простое и существует положительно число меньшее n не делящееся на n то а в степени p сопоставимо с a mod p.
;; два числа называются сопоставимыми по модулю n, если оба имеют одинаковый остаток от деления на n
;; 7 2
;; 2**7
;; Если n не простое, тогда для большинство a < n условте не выполняется.
;; Это приводит к следующему алгоритму проверки простоты.
;; Пусть задано n, выбираем a < n вычисляем reaminder a^n modulo n
;; если результат не равен a. то p определнно не простое число.
;; потом выбираем другое a и тестируем его тем же методом. Это и есть тест ферма.

(define (expmod base exp m)
  (cond ((= exp 0) 1)
        ((even? exp)
         (remainder (square (expmod base (/ exp 2) m))
                    m))
        (else
         (remainder (* base (expmod base (- exp 1) m))
                    m))))

(define (fermat-test n)
  (define (try-it a)
    (= (expmod a n n) a))
  (try-it (+ 1 (random (- n 1)))))

(fermat-test 10)

(define (fast-prime? n times)
  (cond ((= times 0) true)
        ((fermat-test n) (fast-prime? n (- times 1)))
        (else false)))

(fast-prime? 117 20)
;; try it 6 ; expmod 6 10 10

;; Существуют числа которые проходят тест ферма, но не являются простыми.
;; 561, 1105, 1729, 2465, 2821, and 6601
;; Числа Кармайкла

(expmod 10 10 10)



;; Ex 1.22, 1.23

(define (timed-prime-test n)
  (newline)
  (display n)
  (start-prime-test n (runtime)))
(define (start-prime-test n start-time)
  (if (prime? n)
      (report-prime start-time (runtime))))
(define (report-prime start-time end-time)
  (newline)
  (display " *** ")
  (display start-time)
  (display "; ")
  (display end-time)
  (display "; ")
  (display (- end-time start-time)))
(define (prime? n)
  (= n (smallest-divisor n)))

(timed-prime-test 10007)
(runtime)

(define (search-iter from number start-time)
  (if (not (= number 0))
      (if (prime? from)
          (report-and-continue from number start-time (runtime))
          (search-iter (+ from 2) number start-time))))

(define (report-and-continue from number start-time end-time)
  (newline)
  (display from)
  (newline)
  (report-prime start-time end-time)
  (search-iter (+ from 2) (- number 1) start-time))

(define (search-for-primes from number)
  (if (even? from)
      (search-iter (+ from 1) number (runtime))
      (search-iter (from 1) number (runtime))))
(define (prime? n)
  (= n (smallest-divisor n)))
(search-for-primes 1000 3)
(search-for-primes 10000 3)
(search-for-primes 100000 3)
(search-for-primes 1000000 3)
(search-for-primes 10000000 3)
(search-for-primes 100000000 3)
(search-for-primes 1000000000 3)

;; Ex. 1.24

(define (timed-prime-test n)
  (newline)
  (display n)
  (start-prime-test n (runtime)))
(define (start-prime-test n start-time)
  (if (prime? n)
      (report-prime (- (runtime) start-time))))
(define (report-prime elapsed-time)
  (display " *** ")
  (display elapsed-time))

(timed-prime-test 10000019); 1.0000000000001563e-2
(timed-prime-test 10000079); 1.0000000000001563e-2
(timed-prime-test 10000103); 1.0000000000001563e-2

(timed-prime-test 100000007); 1.9999999999999574e-2
(timed-prime-test 100000037); 2.0000000000003126e-2
(timed-prime-test 100000039); 2.0000000000003126e-2

(timed-prime-test 1000000007); .05000000000000071
(timed-prime-test 1000000009); .05999999999999872
(timed-prime-test 1000000021); .07000000000000028


(define (fast-prime? n times)
  (cond ((= times 0) true)
        ((fermat-test n) (fast-prime? n (- times 1)))
        (else false)))

(define (timed-prime-test n)
  (newline)
  (display n)
  (start-prime-test n (runtime)))
(define (start-prime-test n start-time)
  (if (fast-prime? n 1000)
      (report-prime (- (runtime) start-time))))
(define (report-prime elapsed-time)
  (display " *** ")
  (display elapsed-time))

; c 10 проверками
(timed-prime-test 10000019); 0.
(timed-prime-test 10000079); 0.
(timed-prime-test 10000103); .00999999999999801

(timed-prime-test 100000007); 0.
(timed-prime-test 100000037); .00999999999999801
(timed-prime-test 100000039); .00999999999999801

(timed-prime-test 1000000007); 1.0000000000001563e-2
(timed-prime-test 1000000009); 1.0000000000001563e-2
(timed-prime-test 1000000021); 1.0000000000001563e-2

; с 1000 проверками
(timed-prime-test 10000019); .1999999999999993
(timed-prime-test 10000079); .20000000000000284
(timed-prime-test 10000103); .21999999999999886

(timed-prime-test 100000007); .23000000000000043
(timed-prime-test 100000037); .23999999999999844
(timed-prime-test 100000039); .240000000000002

(timed-prime-test 1000000007); .2699999999999996
(timed-prime-test 1000000009); .26000000000000156
(timed-prime-test 1000000021); .259999999999998


(fast-prime? 117 20)

;; 1.25

;; метод который используем в fast-prime?

;; Два числа называются сопоставимыми по модулю n если оба имеют одинаковый остаток при делении на n.
(define (expmod base exp m)
  (cond ((= exp 0) 1)
        ((even? exp)
         (remainder (square (expmod base (/ exp 2) m))
                    m))
        (else
         (remainder (* base (expmod base (- exp 1) m))
                    m))))

expmod (100 1000000007 1000000007)
remainder (100 * [expmod(100 1000000006 1000000007)]) 1000000007
;; тут мы будем умножать на остаток []
[] = remainder (square ([expmod 100 500000003 1000000007])) 1000000007)
;; возводить в квадрат и перемножать мы будем остаток от деления
;; мы сделаем это лагорифмическое число раз
remainder (square (100 * expmod(100 1000000006/2 1000000007))

;; метод Алисы
(define (fast-expt b n)
  (cond ((= n 0) 1)
        ((even? n) (square (fast-expt b (/ n 2))))
        (else (* b (fast-expt b (- n 1))))))
(define (expmod base exp m)
  (remainder (fast-expt base exp) m))
;; тут мы сначала вычисляем экспоненту, с числами навроде 100**1000000007
;; умножение и возведение в квадрат на таких больших числах приводит к росту времени, алгоритм у нас по прежнему имеет лагорифмическую оценку по подсчету экспоненты, но порядок цифр таков, что обычное умножение требует больгих ресурсов компьютера.

допустим проверяем число 1000000007, пусть a = 100
тогда нам нужно подсчитать expmod (100 1000000007 1000000007)

;; 2 7 7
;; 2 <- 2 * (2 6 7)
;; 1 <- square (2 3 7)
;; 1 <- 2 * (2 2 7)
;; 4 <- sqaure (2 1 7)
;; 2 <- 2 * (2 0 7)


;; 1.27

(define (expmod base exp m)
  (cond ((= exp 0) 1)
        ((even? exp)
         (remainder (square (expmod base (/ exp 2) m))
                    m))
        (else
         (remainder (* base (expmod base (- exp 1) m))
                    m))))

(define (fermat-test-all n)
  (define (iter-test a)
    (cond ((>= a n) (display "passed full Ferma test"))
          ((= (expmod a n n) a) (iter-test (+ a 1)))
          (else (display "Ferma test failed"))))
  (iter-test 2))

(fermat-test-all 561)
(fermat-test-all 1105)
(fermat-test-all 1729)
(fermat-test-all 2465)
(fermat-test-all 2821)
(fermat-test-all 6601)

561, 1105, 1729, 2465, 2821, and 6601

;; 1.28

(define (expmod-miller-rabin base exp m)
  (cond ((= exp 0) 1)
        ((even? exp)
         (let ((x (expmod-miller-rabin base (/ exp 2) m)))
           (cond ((or (= x 1) (= x (- m 1))) (remainder (square x) m))
                 ((= (remainder (square x) m) 1) 0)
                 (else (remainder (square x) m)))))
        (else (remainder (* base (expmod-miller-rabin base (- exp 1) m))
                         m))))

(define (miller-rabin-test-all n)
  (define (iter-test a)
    (cond ((>= a n) (display "passed full miller-rabin test"))
          ((= (expmod-miller-rabin a n n) a) (iter-test (+ a 1)))
          (else (display "miller-rabin test failed"))))
  (iter-test 2))

(miller-rabin-test-all 1999)
(miller-rabin-test-all 561)

;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 1.3
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; тут речь пойдет о процедурах высшего порядка, то есть когда процедура принимает процедуру в качестве параметра.

(define (cube x) (* x x x))
(define (sum-integers a b)
  (if (> a b)
      0
      (+ a (sum-integers (+ a 1) b))))
(define (sum-cubes a b)
  (if (> a b)
      0
      (+ (cube a) (sum-cubes (+ a 1) b))))

;; limit -> pi/8
(define (pi-sum a b)
  (if (> a b)
      0
      (+ (/ 1.0 (* a (+ a 2))) (pi-sum (+ a 4) b))))

;; паттерн объединяющий все три процедуры

(define (<name> a b)
  (if (> a b)
      0
      (+ (<term> a)
         (<name> (<next> a) b))))

(define (sum term a next b)
  (if (> a b)
      0
      (+ (term a)
         (sum term (next a) next b))))

(define (inc n) (+ n 1))
(define (sum-cubes a b)
  (sum cube a inc b))
(define (identity x) x)
(define (sum-integers a b)
  (sum identity a inc b))
(sum-cubes 1 3)
(sum-integers 1 3)

(define (pi-sum a b)
  (define (pi-term x)
    (/ 1.0 (* x (+ x 2))))
  (define (pi-next x)
    (+ x 4))
  (sum pi-term a pi-next b))
(* 8 (pi-sum 1 1000))

(define (integral f a b dx)
  (define (add-dx x) (+ x dx))
  (* (sum f (+ a (/ dx 2.0)) add-dx b)
     dx))

(integral cube 0 1 0.01)
(integral cube 0 1 0.001)

;; Ex 1.29
;; h = dx

(define (sum-simpson term a next b controln k)
  (if (> a b)
      0
      (+ (* (controln k)(term a))
         (sum-simpson term (next a) next b controln (+ k 1)))))

(define (integral-simpson f a b n)
  (define (getdx start end steps)
    (/ (- end start) steps))
  (let ((dx (getdx a b n)))
    (define (controln step-number)
      (cond ((= step-number 0) 1)
            ((even? step-number) 2)
            (else 4)))

    (define (next-a x)
      (+ x dx))
  ;; ;; (define (add-dx x) (+ x (getdx dx)))
    (* (sum-simpson f a next-a b controln 0)
       (/ (getdx a b n) 3))
    ))

(integral-simpson cube 0 1 100);;19/75 0.25333333333333335
(integral-simpson cube 0 1 1000);; 751/3000 0.25033333333333335


;; Ex. 1.30

(define (sum term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (+ result (term a)))))
  (iter a 0))
(sum identity 1 inc 5)

;; Ex. 1.31

(define (product term a next b)
  (if (> a b)
      1
      (* (term a)
         (product term (next a) next b))))

(define (producti term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (* result (term a)))))
  (iter a 1))

(define (product-factorial n)
  (product identity 1 inc 5))
(product-factorial 5)

(define (multiplicate-pi x)
  (square (/ (+ 4 x) (+ 5 x))))

(define (product-pi n)
  (define (sub-pi x)
    (* (/ x (+ 1 x)) (/ (+ x 2) (+ x 1))))
  (define (next-x x)
    (+ x 2))
  (* 4 (product sub-pi 2 next-x n)))
(product-pi 1000) ; 3.1431607055322663


;; 1.32
(define (accumulate combiner null-value term a next b)
  (if (> a b)
      null-value
      (combiner (term a)
                (accumulate combiner null-value term (next a) next b))))

(define (accumulate combiner null-value term a next b)
  (define (iter a result)
    (if (> a b)
      result
      (iter (next a) (combiner result (term a)))))
  (iter a null-value))


(define (product term a next b)
  (accumulate * 1 term a next b))
(define (sum term a next b)
  (accumulate + 0 term a next b))
(define (accumulate-factorial n)
  (product identity 1 inc 5))
(accumulate-factorial 5) ; 120
(define (accumulate-sum a b)
  (sum identity a inc b))
(accumulate-sum 1 5) ; 15

;; 1.33
;; рекурсивный
(define (filtered-accumulate combiner null-value term a next b filter)
  (if (> a b)
      null-value
      (combiner (if (filter a b) (term a) null-value)
                (filtered-accumulate combiner null-value term (next a) next b filter))))

;; итерационный
(define (filtered-accumulate combiner null-value term a next b filter)
  (define (iter a result)
    (if (> a b)
      result
      (iter (next a) (if (filter a b)
                         (combiner result (term a))
                         result)
            )))
  (iter a null-value))

;; cумма квадратов целых в диапазоне
;; передаем в фильтр два параметра a b
(define (filter-prime? x end)
  (if (prime? x)
      1
      false))
(define (sum-square-primes a b)
  (filtered-accumulate + 0 square a inc b filter-prime?))
(sum-square-primes 1 5) ; 14

;; перемножение всех взаимнопростых с n в диапазане [m n]
(define (filter-relative-prime? x end)
  (if (= (gcd x end) 1)
      1
      false))

(define (product-relative-primes a b)
  (filtered-accumulate * 1 identity a inc b filter-relative-prime?))
(product-relative-primes 1 6); 5

;; 1.3.2 Конструируем процедуры используя Lambda

;; 1.34
(define (f g)
  (g 2))
(f square)
(f (lambda (z) (* z (+ z 1))))
(f f)
(2 2)

;; 1.3.3 Процедуры как обобщащие методы.

;; Нахождение корней метом полуинтервалов
(define (search f neg-point pos-point)
  (let ((midpoint (average neg-point pos-point)))
    (if (close-enough? neg-point pos-point)
        midpoint
        (let ((test-value (f midpoint)))
          (cond ((positive? test-value)
                 (search f neg-point midpoint))
                ((negative? test-value)
                 (search f midpoint pos-point))
                (else midpoint))))))
(define (close-enough? x y)
  (< (abs (- x y)) 0.001))

(define (half-interval-method f a b)
  (let ((a-value (f a))
        (b-value (f b)))
    (cond ((and (negative? a-value) (positive? b-value))
           (search f a b))
          ((and (negative? b-value) (positive? a-value))
           (search f b a))
          (else
           (error "Values are not of opposite sign" a b)))))
(half-interval-method sin 2.0 4.0)
(half-interval-method (lambda (x) (- (* x x x) (* 2 x) 3))
                      1.0
                      2.0)

;; Нахождение фиксированных точек функции
;; Число x называется фиксированной точкой функции если f(x) x. Для некоторых функций мы можем найти эти точки выбрав изначальное предположение и потом  повторяя f. f(x), f(f(x)), f(f(f(x))) пока значение не меняется сильно.
;; Используя эту идею мы можем сделать процедуру fixed-point которая принимает в качестве аргументов функции и предположение и производит приближение к фиксированной точке функции. Мы применяем функцию до тек пор пока не найдем два подходящих значения разница между которыми ниже чем допуск.

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
(fixed-point (lambda (y) (+ (sin y) (cos y)))
             1.0)

(define (f x y)
  ((lambda (a b)
     (+ (* x (square a))
        (* y b)
        (* a b)))
   (+ 1 (* x y))
   (- 1 y)))
(define (f x y)
  (let ((a (+ 1 (* x y)))
        (b (- 1 y)))
    (+ (* x (square a))
       (* y b)
       (* a b))))

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
    (newline)
    (display guess)
    (let ((next (f guess)))
      (if (close-enough? guess next)
          next
          (try next))))
  (try first-guess))
(fixed-point cos 1.0)

;; пример с квадратным корнем к сожалению не работает, бесконечный цикл
(define (sqrt x)
  (fixed-point (lambda (y) (/ x y))
               1.0))
;; помогает усреднение
(define (sqrt x)
  (fixed-point (lambda (y) (average y (/ x y)))
               1.0))
(sqrt 2)
;; один из способов контроля этого зацикливания это брать среденее

;; Ex. 1.35

(define (phi x)
  (fixed-point (lambda (y) (+ 1 (/ 1 y)))
               1.0))

(phi 1)

;; Ex. 1.36

(define (xx1000)
  (fixed-point (lambda (y) (average y (/ (log 1000) (log y))))
               2.0))
(xx1000)
(log 1000)


;; Ex 1.37

(define (cont-frac n d k)
  (define (revert-index n d i)
    (if (> i k)
        (/ (n i) (+ (d i) 0.0))
        (/ (n i) (+ (d i) (revert-index n d (+ i 1))))))
  (revert-index n d 0))

(cont-frac (lambda (i) 1.0)
           (lambda (i) 1.0)
           100) ;Value: .6180339887498948


(define (cont-frac n d k)
  (define (iter i result)
    (newline)
    (display result)
    (if (= i 0)
        (/ (n i) (+ (d i) result))
        (iter (- i 1) (/ (n i) (+ (d i) result)))))
  (iter k 0.0))

(cont-frac (lambda (i) 1.0)
           (lambda (i) 1.0)
           100) ;Value: .6180339887498948

;; Ex 1.38

(define (e-approximation k)
  (+ 2 (cont-frac (lambda (i) 1.0)
                  (lambda (i) (cond ((= i 0) 1.0)
                               ((= i 1) 2.0)
                               ((<= i 3) 1.0)
                               ((= (remainder (- i 1) 3) 0) (* 2 (/ (- i 1) 3)))
                               (else 1.0)))
                  k)))

(e-approximation 100) ;Value: .6180339887498948
(define (line x)
  (cond ((= x 0) 1)
        ((= x 1) 2)
        ((<= x 3) 1)
        ((= (remainder (- x 1) 3) 0) (* 2 (/ (- x 1) 3)))
        (else 1)))
(line 0)
(line 1)
(line 2)
(line 3)
(line 4)
(line 5)
(line 6)
(line 7)
(line 8)
(line 9)
(line 10)
(/ 6 2)


;; Ex 1.39

(define (cont-frac-params n d k x)
  (define (iter i result)
    (newline)
    (display result)
    (if (= i 0)
        (/ (n i x) (- (d i x) result))
        (iter (- i 1) (/ (n i x) (- (d i x) result)))))
  (iter k 0.0))


(define (tan-cf x k)
  (cont-frac-params (lambda (i j) (cond ((= i 0) j)
                          (else (square j))))
           (lambda (i x) (+ 1 (* 2 i)))
           100 x))

(tan-cf 2 100);Value: -2.185039863261519

;; 1.34 Процедуры как возвращаемые значения

(define (average-damp f)
  (lambda (x) (average x (f x))))

((average-damp square) 10)

;; используя это перепишем процедуру квадратного корня

(define (sqrt x)
  (fixed-point (average-damp (lambda (y) (/ x y)))
               1.0))
(sqrt 2)

(define (cube-root x)
  (fixed-point (average-damp (lambda (y) (/ x (square y))))
               1.0))

;; Метод ньютона

;; используется идея
;; f(x) = x - g(x)/Dg(x)
;; f(x) = x - фиксированная точка, я является решением g(x)=0

;; это просто по определениею производной
(define dx 0.00001)
(define (deriv g)
  (lambda (x)
    (/ (- (g (+ x dx)) (g x))
       dx)))
(define (cube x) (* x x x))
((deriv cube) 5)

(define (newton-transform g)
  (lambda (x)
    (- x (/ (g x) ((deriv g) x)))))


(define (newtons-method g guess)
  (fixed-point (newton-transform g) guess))

(define (sqrt x)
  (newtons-method (lambda (y) (- (square y) x))
                  1.0))

;; Ex. 140

(define (cubic a b c)
  (lambda (x)
    (+ (* x x x) (* a x x) (* b x) c)))

(newtons-method (cubic 1 1 1) 1)
;; Ex. 141

(define (double f)
  (lambda (x)
    (f (f x))))
((double inc) 5)
(((double (double double)) inc) 0);; 21

;; Ex. 1.42
(define (compose f g)
  (lambda (x) (f (g x))))
((compose square inc) 6) ; 49

;; Ex. 1.43

(define (repeated f n)
  (if (= n 1)
      (lambda (x) (f x))
      (compose f (repeated f (- n 1)))))

(define (repeated f n)
  (if (= n 1)
      (lambda (x) (f x))
      (lambda (x) (f ((repeated f (- n 1)) x)))))

((repeated square 2) 5)

;; Ex. 1.44
(define dx 0.00001)
(define (smooth f)
  (lambda (x) (average (f (- x dx)) (f (+ x dx)))))

(define (smooth-n f n)
  ((repeated smooth n) f))
((smooth square) 2)
((smooth-n square 5) 2)

;; Ex. 1.45
;; 1 ; 1 0
;; 2 ; 2 1
     ; 3 1
;; 4 ; 4 2
     ; 5 2
     ; 6 2
     ; 7 2
;; 8 ; 8 3
     ; 15 3
     ; 16 4
     ; 32 4
     ; 33 5
(define (nth-root x n)
  (define (get-ad-count n i)
    (if (< n (fast-expt 2 i))
        (- i 1)
        (get-ad-count n (+ i 1))))
  (display (get-ad-count n 1))
  (fixed-point ((repeated average-damp (get-ad-count n 1)) (lambda (y) (/ x (fast-expt y (- n 1)))))
               1.0))
(nth-root 27 64)

(define (get-average-damp-count n)
  (if (= i 1 2)))


1 + 2 + 4 + 8 + 16


1*2^(2-1)
(define (get-ad-count n i)
  (if (< n (fast-expt 2 i))
      (- i 1)
      (get-i n (+ i 1))))

(get-i 16 1)



i = 8

;; 1 ; 1 0
;; 2 ; 2 1
     ; 3 1
;; 4 ; 4 2
     ; 5 2
     ; 6 2
     ; 7 2

;; 8 ; 8 3
     ; 15 3

     ; 16 4
     ; 32 4

     ; 33 5




;; Ex. 1.46

(define tolerance 0.00001)
(define (close-enough? v1 v2)
  (< (abs (- v1 v2)) tolerance))

(define (iterative-improve good-enough? improve)
  (define (try guess x)
    (let ((next (improve guess)))
      (if (close-enough? guess next)
          next
          (try next x))))
  (lambda (x guess) (try guess x)))

(define (sqrt x)
  (define (improve guess)
    (define (average x y)
      (/ (+ x y) 2))
    (average guess (/ x guess)))
  ((iterative-improve close-enough? improve) x 1.0))

(sqrt 2) ;Value: 1.4142156862745097


(define (fixed-point f)
  ((iterative-improve close-enough? f) 1.0 1.0))
(fixed-point cos) ;Value: .7390822985224024


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Создание абстракций с данными
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Введение в абстракции данных

(define (linear-combination a b x y)
  (+ (* a x) (* b y)))

(define (linear-combination a b x y)
  (add (mul a x) (mul b y)))

;; 2.1.1 арифметика над рациональными числами

;; (make-rat <n> <d>)
;; (numer <x>)
;; (denom <x>)
(define (add-rat x y)
  (make-rat (+ (* (numer x) (denom y))
               (* (numer y) (denom x)))
            (* (denom x) (denom y))))
(define (sub-rat x y)
  (make-rat (- (* (numer x) (denom y))
               (* (numer y) (denom x)))
            (* (denom x) (denom y))))
(define (mul-rat x y)
  (make-rat (* (numer x) (numer y))
            (* (denom x) (denom y))))
(define (div-rat x y)
  (make-rat (* (numer x) (denom y))
            (* (denom x) (numer y))))
(define (equal-rat? x y)
  (= (* (numer x) (denom y))
     (* (numer y) (denom x))))

(define x (cons 1 2))
(car x)
(cdr x)
(define (make-rat n d) (cons n d))
(define (numer x) (car x))
(define (denom x) (cdr x))
(define (print-rat x)
  (newline)
  (display (numer x))
  (display "/")
  (display (denom x)))
(define one-half (make-rat 1 2))
(print-rat one-half)

(define one-third (make-rat 1 3))
(print-rat (add-rat one-half one-third))

;; Ex. 2.1 Определите make-rat которая будет определять знак.
(define (make-rat n d)
  (let ((g (gcd n d))
        (sign (cond (or ((and (> n 0) (> d 0)) (and (< n 0) (< d 0))) 1)
                        (else 0))))
    (cons
     (if (> sign 0) (abs (/ n g)) (* -1 (abs (/ n g))))
     (abs (/ d g)))))
(print-rat (add-rat one-third one-third))

(print-rat (make-rat 1 -1))

;; Ex. 2.2
(define (average a b) (/ (+ a b) 2))

(define (make-point x y) (cons x y))
(define (x-point p) (car p))
(define (y-point p) (cdr p))

(define (print-point p)
  (newline)
  (display "(")
  (display (x-point p))
  (display ",")
  (display (y-point p))
  (display ")"))
(define (average-points a b)
   (make-point (average (x-point a) (x-point b))
               (average (y-point a) (y-point b))))
(define (distance-points a b)
  (sqrt (+ (square (- (x-point a) (x-point b)))
           (square (- (y-point a) (y-point b))))))
(print-point (make-point 1 2))

(define (make-segment start end) (cons start end))
(define (start-segment s) (car s))
(define (end-segment s) (cdr s))

(define (midpoint-segment s) (average-points (start-segment s) (end-segment s)))
(print-point (midpoint-segment (make-segment (make-point 0 0) (make-point 6 0))))

;; Ex. 2.3

(define (segment-lenght s)
  (distance-points (start-segment s) (end-segment s)))
;; (segment-lenght (make-segment (make-point 0 0) (make-point 6 0)))

(define (make-rect seg1 seg2) (cons seg1 seg2))
(define (rect-base r) (car r))
(define (rect-side r) (cdr r))

(define (rect-perimeter r)
  (* 2.0 (+ (segment-lenght (rect-base r)) (segment-lenght (rect-side r)))))
(define (react-area r)
  (* (segment-lenght (rect-base r)) (segment-lenght (rect-side r))))

(rect-perimeter (make-rect
                 (make-segment (make-point 0 0) (make-point 6 0))
                 (make-segment (make-point 0 0) (make-point 0 6))
                 ))

(react-area (make-rect
                 (make-segment (make-point 0 0) (make-point 6 0))
                 (make-segment (make-point 0 0) (make-point 0 6))
                 ))


;; другое представление прямоугольника например тремя точками
(define (make-rect point1 point2 point3) (cons (cons point1 point2) (cons point2 point3)))
(rect-perimeter (make-rect
                 (make-point 6 0) (make-point 0 0) (make-point 0 6)
                 ))

(react-area (make-rect
             (make-point 6 0) (make-point 0 0) (make-point 0 6)
             ))


;; What is meant by Data?

(define (cons x y)
  (define (dispatch m)
    (cond ((= m 0) x)
          ((= m 1) y)
          (else (error "Argument not 0 or 1 -- CONS" m))))
  dispatch)

(define (car z) (z 0))
(define (cdr z) (z 1))

;; Ex. 2.4

(define (cons x y)
  (lambda (m) (m x y)))

(cons 1 2)

(define (car z)
  (z (lambda (p q) p)))

;; (cons 1 3)
;; (car (cons 1 3))
;; (car (lambda (m) (m x y)))
;; ((lambda (m) (m x y)) (lambda (p q) p))
;; (m x y) это как раз вызвать процедуру которая в параметрах с параметрами x y
;; но следующая проуедура как раз возвращает первый параметр p, то есть все выражение
;; вернет x
(car (cons 1 2))

(define (cdr z)
  (z (lambda (p q) q)))

(cdr (cons 1 2))

;; Ex. 2.5
(define logB
    (lambda (x B)
      (/ (log x) (log B))))

(define (cons a b)
  (* (fast-expt 2 a) (fast-expt 3 b)))

(cons 1 4)

(define (cdr r)
  (if (> (remainder r 2) 0)
      (logB r 3)
      (cdr (/ r 2))))

(define (car r)
  (if (> (remainder r 3) 0)
      (logB r 2)
      (car (/ r 3))))
(car 162)
(cdr 162)
(gcd 54 2)
(logB 27 3)
(remainder 162 3)
(/ 162 3)
(/ 54 3)
(/ 18 3)
(/ 6 3)
(/ 2 3)


;; Ex. 2.6
(define zero (lambda (f) (lambda (x) x)))

(define (add-1 n)
  (lambda (f) (lambda (x) (f ((n f) x)))))

((zero 1) 1)
((((add-1 zero) 1)) 1)

;; (add-1 zero)
;; (add-1 (lambda (f) (lambda (x) x)))
;; ((n f) x) будет просто x, так как n это zero
;;
;; (lambda (x) (f x))
(define (one f)
  (lambda (x) (f x)))
;; (add-1 one)
;;
(define (two f)
  (lambda (x) (f (f x))))
(define (add m n)
  (lambda (f) (lambda (x) ((m f) ((n f) x)))))
;; пусть m=one, n=one
;; ((n f) x) = (f x)
;; ((m f) (f x))
;; (f (f x))
;; (lambda (x) (f (f x))) а это не что иное как два
(add one one)

(define (print-church n)
  (display ((n inc) 0)) (newline))

(print-church zero)
(print-church one)
(print-church two)
(print-church (add two two))

;; 2.1.4 Расширенное упражнение: интервальная арифметика.

(define (add-interval x y)
  (make-interval (+ (lower-bound x) (lower-bound y))
                 (+ (upper-bound x) (upper-bound y))))


(define (mul-interval x y)
  (let ((p1 (* (lower-bound x) (lower-bound y)))
        (p2 (* (lower-bound x) (upper-bound y)))
        (p3 (* (upper-bound x) (lower-bound y)))
        (p4 (* (upper-bound x) (upper-bound y))))
    (make-interval (min p1 p2 p3 p4)
                   (max p1 p2 p3 p4))))

(define (div-interval x y)
  (cond ((= (upper-bound y) 0) (error "Error upper-bound = 0" y))
        ((= (lower-bound y) 0) (error "Error lower-bound = 0" y))
        (else
         (mul-interval x
                       (make-interval (/ 1.0 (upper-bound y))
                                      (/ 1.0 (lower-bound y)))))))

(define (make-interval a b) (cons a b))

(define (upper-bound i) (max (car i) (cdr i)))
(define (lower-bound i) (min (car i) (cdr i)))

(define (sub-interval x y)
  (make-interval (- (lower-bound x) (upper-bound y))
                 (- (upper-bound x) (lower-bound y))))

(define (sub-interval x y)
  (add-interval x (make-interval (- (upper-bound y)) (- (lower-bound y)))))

;; Ex. 2.9

(define (width-interval i)
  (/ (- (upper-bound i) (lower-bound i)) 2.0))
(width-interval (make-interval 4.0 6.0)) ;; 1
(width-interval (make-interval 10 14)) ;; 2
(width-interval (add-interval (make-interval 4 6)
                              (make-interval 10 14)));; 3

(width-interval (sub-interval (make-interval 4 6)
                              (make-interval 10 14)));; 3
(width-interval (mul-interval (make-interval 10 14)
                              (make-interval 4 6)));; 22

(width-interval (div-interval (make-interval 10.0 14.0)
                              (make-interval 0 6)));; 0.916


;; Ex. 2.11

(define (mul-interval x y)
  (let ((p1 (* (lower-bound x) (lower-bound y)))
        (p2 (* (lower-bound x) (upper-bound y)))
        (p3 (* (upper-bound x) (lower-bound y)))
        (p4 (* (upper-bound x) (upper-bound y))))
    ;; (display p1)
    ;; (newline)
    ;; (display p2)
    ;; (newline)
    ;; (display p3)
    ;; (newline)
    ;; (display p4)
    ;; (newline)
    (make-interval (min p1 p2 p3 p4)
                   (max p1 p2 p3 p4))))


;; Если lx >= 0 и ly >= 0 то у нас все положительно, достаточно одного перемножения
;; Если Ux <= 0 и Uy <= 0 то у нас все отрицательное, достаточно одного перемножения
;; Lx >= 0 Ly < 0 Uy >= 0, x положительный, а y содержит 0
;; max Ux * Uy
;; min Ux * Ly
;; Lx >= 0 Ly < 0 Uy < 0, x положительный, а y весь отрицательный
;; min Ux * Ly
;; max Lx * Uy !!
;; Ly >= 0 Lx < 0 Ux >= 0, y положительный, а x содержит 0
;; min Uy * Lx
;; max Uy * Ux
;; Ly >= 0 Lx < 0 Ux < 0, y положительный, а x весь отрицательный
;; min Uy * Lx
;; max Ly * Lx
;; Ux < 0 Ly < 0 Uy > 0; x отрицательный y пересекает 0
;; min Lx * Uy
;; max Lx * Ly
;; Uy < 0 Lx < 0 Ux > 0; y отрицательный x пересекает 0
min Ly * Ux
max Ly * Lx
;; x и y пересекают 0



(define (mul-interval x y)
  ;; Если все положительно
  (cond ((and (>= (lower-bound x) 0) (>= (lower-bound y) 0)) (make-interval
                                                              (* (lower-bound x) (lower-bound y))
                                                              (* (upper-bound x) (upper-bound y))))
        ;; Если все отрицательное
        ((and (<= (upper-bound x) 0) (<= (upper-bound y) 0)) (make-interval
                                                              (* (upper-bound x) (upper-bound y))
                                                              (* (lower-bound x) (lower-bound y))))
        ;; x положительный, а y содержит 0
        ((and (>= (lower-bound x) 0) (< (lower-bound y) 0) (>= (upper-bound y) 0)) (make-interval
                                                                                    (* (upper-bound x) (upper-bound y))
                                                                                    (* (upper-bound x) (lower-bound y))))
        ;; x положительный, а y весь отрицательный
        ((and (>= (lower-bound x) 0) (< (upper-bound y) 0)) (make-interval
                                                             (* (upper-bound x) (lower-bound y))
                                                             (* (lower-bound x) (upper-bound y))))
        ;; y положительный, а x содержит 0
        ((and (>= (lower-bound y) 0) (< (lower-bound x) 0) (>= (upper-bound x) 0)) (make-interval
                                                             (* (upper-bound y) (lower-bound x))
                                                             (* (upper-bound x) (upper-bound y))))
        ;; y положительный, а x весь отрицательный
        ((and (>= (lower-bound y) 0) (< (lower-bound x) 0) (>= (upper-bound x) 0)) (make-interval
                                                             (* (upper-bound y) (lower-bound x))
                                                             (* (lower-bound x) (lower-bound y))))
        ;; x отрицательный y пересекает 0
        ((and (< (upper-bound x) 0) (< (lower-bound y) 0) (>= (upper-bound y) 0)) (make-interval
                                                             (* (lower-bound x) (upper-bound y))
                                                             (* (lower-bound x) (lower-bound y))))
        ;; y отрицательный x пересекает 0
        ((and (< (upper-bound y) 0) (< (lower-bound x) 0) (>= (upper-bound x) 0)) (make-interval
                                                             (* (lower-bound y) (upper-bound x))
                                                             (* (lower-bound x) (lower-bound y))))
        ;; иначе у нас и x и y пересекают 0, в этом случае и нужно перемножать значения
        (else (let ((p1 (* (lower-bound x) (lower-bound y)))
                    (p2 (* (lower-bound x) (upper-bound y)))
                    (p3 (* (upper-bound x) (lower-bound y)))
                    (p4 (* (upper-bound x) (upper-bound y))))
                (make-interval (min p1 p2 p3 p4)
                               (max p1 p2 p3 p4))))))


(define (mul-interval x y)
  (let ((p1 (* (lower-bound x) (lower-bound y)))
        (p2 (* (lower-bound x) (upper-bound y)))
        (p3 (* (upper-bound x) (lower-bound y)))
        (p4 (* (upper-bound x) (upper-bound y))))
    (make-interval (min p1 p2 p3 p4)
                   (max p1 p2 p3 p4))))


;; Тесты
;; 1) x, y >
(mul-interval (make-interval 10 14)
              (make-interval 0 6))
;; 2) x, y <
(mul-interval (make-interval -14 -10)
              (make-interval -6 -4))
;; 3) x> y<
(mul-interval (make-interval 14 10)
              (make-interval -6 -4))
;; 4) x> y 0
(mul-interval (make-interval 14 10)
              (make-interval -2 2))
;; 5) y> x 0
(mul-interval (make-interval -2 2)
              (make-interval 14 10))
;; 6) y> x<
(mul-interval (make-interval -4 -2)
              (make-interval 14 10))
;; 7) x< y 0
(mul-interval (make-interval -4 -2)
              (make-interval -2 6))
;; 8) y< x 0
(mul-interval (make-interval -2 6)
              (make-interval -4 -2))
;; 9) x, y 0
(mul-interval (make-interval -6 -3)
              (make-interval -4 -2))

;; После отладки Алиса показала её потенциальному пользователю, на что получила ответ что программа решает не ту задачу. Он хочет программу которая имеет дело с числами представленными как сентральная точка +- погрешность. Алиса вернулась к рабочему столу и написала альтернативный конструктор и селекторы.

(define (make-center-width c w)
  (make-interval (- c w) (+ c w)))
(define (center i)
  (/ (+ (lower-bound i) (upper-bound i)) 2))
(define (width i)
  (/ (- (upper-bound i) (lower-bound i)) 2))

;; К сожалению большинство пользователей программы инженеры. Реальные инженерные ситуации обычно включают измерения с небольшой неопределенностью, измеряемой как отношения ширины интервала, к центральной точке интервала. Инженеры обычно указывают процентные допуски на параметрах устройств, как указано в характеристиках резисторов, приведенных ранее.


(define (make-center-percent c p)
  (make-interval (- c  (* c (/ p 100.0))) (+ c (* c (/ p 100.0)))))

(define (percent i)
  (let ((c (center i)))
    (/ (* (- (upper-bound i) c) 100) c)))
(make-center-percent 5 10)
(percent (make-center-percent 5 10))


(let ((A (make-center-percent 100 3)) (B (make-center-percent 80 4)))
  (define (par1 r1 r2)
    (div-interval (mul-interval r1 r2)
                  (add-interval r1 r2)))
  (define (par2 r1 r2)
    (let ((one (make-interval 1 1)))
      (div-interval one
                    (add-interval (div-interval one r1)
                                  (div-interval one r2)))))
  ;; Подтверждение утверждения Лема
  ;; (display (width A))
  ;; (newline)
  ;; (display (width B))
  ;; (newline)
  ;; (display (par1 A B))
  ;; (newline)
  ;; (display (center (par1 A B)))
  ;; (newline)
  ;; (display (percent (par1 A B)))
  ;; (newline)
  ;; (display (par2 A B))
  ;; (newline)
  ;; (display (center (par2 A B)))
  ;; (newline)
  ;; (display (percent (par2 A B)))
  ;; (newline)

  (display "****B****")
  (display B)
  (newline)
  (display (center B))
  (newline)
  (display (percent B))
  (newline)
  (display "****B*(B/B)****")
  (display (mul-interval B (div-interval B B)))
  (newline)
  (display (center (mul-interval B (div-interval B B))))
  (newline)
  (display (percent (mul-interval B (div-interval B B))))
  (newline)
  )

;; получается что при деление умножении мы увеличиваем % погрешности, а центр смещается на малую величину.


;; Ex. 2.15
;; Другой пользователь так же заметил различные результаты подсчета интервалов при эквивалентных алгебраических выражениях. Она говорит, что формула для вычисления интервалов с использованием сисетмы Алисы даст более жесткие границы ошибок, если её можно записать в такой форме, что никакая переменная, представляющая неопределенное число, не повторяется. Поэтому par2 "лучше" par1. Права ли она? Почему?
;; "лучше" потому что погрешность получается меньше. она получается меньше, потому что в случае par2 у интервала 1 нет погрешности, поэтому все действия с этим интервалом не приводят к росту погрешности вцелом, напротив в первом случае мы вводим дополнительную погрешность при каждом действии с интервалом.


;; 2.2. Иерархические данные и свойство замыкания (the closure property).

;; в общем если при помощи cons можно создать пару, а применим cons к двум таким объектам можно создать пару пар, то const удовлетворяет свойству замыкания.


(define nil ())
(cons 1
      (cons 2
            (cons 3
                  (cons 4 nil))))

(define (list-ref items n)
  (if (= n 0)
      (car items)
      (list-ref (cdr items) (- n 1))))
(define squares (list 1 4 9 16 25))

(list-ref squares 3)

(define (length items)
  (if (null? items)
      0
      (+ 1 (length (cdr items)))))
(define odds (list 1 3 5 7))
(length odds)

(define (length items)
  (define (length-iter a count)
    (if (null? a)
        count
        (length-iter (cdr a) (+ 1 count))))
  (length-iter items 0))
(append squares odds)

(define (append list1 list2)
  (if (null? list1)
      list2
      (cons (car list1) (append (cdr list1) list2))))

;; Ex. 2.17 n
(define (last-pair list1)
  (if (null? (cdr list1))
      list1
      (last-pair (cdr list1))))
(last-pair odds)

;; Ex. 2.18
(define (reverse list1)
  (if (null? list1)
      list1
      (append (reverse (cdr list1)) (list (car list1)))))


(define (reverse list1)
  (define (iter l result)
    (if (null? l)
        result
        (iter (cdr l) (cons (car l) result))))
  (iter list1 nil))

odds

(const 1 (cons 2 ()))
(reverse odds)

(cons (car odds) 2)

;; Ex. 2.19

(define (count-change amount)
  (cc amount 5))
(define (cc amount kinds-of-coins)
  (cond ((= amount 0) 1)
        ((or (< amount 0) (= kinds-of-coins 0)) 0)
        (else (+ (cc amount
                     (- kinds-of-coins 1))
                 (cc (- amount
                        (first-denomination kinds-of-coins))
                     kinds-of-coins)))))
(define (first-denomination kinds-of-coins)
  (cond ((= kinds-of-coins 1) 1)
        ((= kinds-of-coins 2) 5)
        ((= kinds-of-coins 3) 10)
        ((= kinds-of-coins 4) 25)
        ((= kinds-of-coins 5) 50)))

(cc 100 5)

(define us-coins (list 1 25 10 5 50))
(define uk-coins (list 100 50 20 10 5 2 1 0.5))
(define (except-first-denomination list1)
  (cdr list1))
(define (first-denomination list1)
  (car list1))
(define (no-more? list1)
  (if (null? list1)
      list1
      false))
(define (cc amount coin-values)
  (cond ((= amount 0) 1)
        ((or (< amount 0) (no-more? coin-values)) 0)
        (else
         (+ (cc amount
                (except-first-denomination coin-values))
            (cc (- amount
                   (first-denomination coin-values))
                coin-values)))))
(cc 100 us-coins)


;; Ex. 2.20

(define (same-parity x . l)
  (define (filter list1 same?)
    (if (null? list1)
        list1
        (if (same? (car list1))
            (cons (car list1) (filter (cdr list1) same?))
            (filter (cdr list1) same?)
            )))
  (define same-even-odd (lambda (i) (if (or (and (not (even? x)) (not (even? i))) (and (even? x) (even? i))) 1 false)))
  (cons x (filter l same-even-odd)))

(same-parity 2 3 4 5 6 7 8)

;; map list

(define (scale-list items factor)
  (if (null? items)
      nil
      (cons (* (car items) factor)
            (scale-list (cdr items) factor))))
(scale-list (list 1 2 3 4 5) 10)

(define (map proc items)
  (if (null? items)
      nil
      (cons (proc (car items))
            (map proc (cdr items)))))
(map abs (list -10 2.5 -11.6 17))

(map (lambda (x) (* x x))
     (list 1 2 3 4))

(define (scale-list items factor)
  (map (lambda (x) (* x factor))
       items))

(scale-list (list 1 2 3 4 5) 10)


(define (square-list items)
  (if (null? items)
      nil
      (cons (square (car items)) (square-list (cdr items)))))


(define (square-list items)
  (map (lambda (x) (square x)) items))
(square-list (list 1 2 3))


(define (square-list items)
  (define (iter things answer)
    (if (null? things)
        answer
        (iter (cdr things) 
              (cons (square (car things))
                    answer))))
  (iter items nil))

(square-list (list 1 2 3))

(list 1 2 3) nil
iter (list 2 3) (cons 1 nil)
iter (list 3) (cons 4 (cons 1 nil))
iter () (cons 9 (cons 4 (cons 1 nil)))
return (cons 9 (cons 4 (cons 1 nil)))

(define (square-list items)
  (define (iter things answer)
    (if (null? things)
        answer
        (iter (cdr things)
              (cons answer
                    (square (car things))))))
  (iter items nil))

nil 1
cons (nil 1) 4

(square-list (list 1 2 3))

(define (square-list items)
  (define (iter things answer)
    (if (null? things)
        answer
        (cons (square (car things)) (iter (cdr things)))))
  (iter items))

(square-list (list 1 2 3))

(define (for-each proc items)
  (if (not (null? items))
      (proc (car items)))
  (if (not (null? items))
      (for-each proc (cdr items)))
  true)

(for-each (lambda (x) (newline) (display x))
          (list 57 321 88))

;; 2.2.2 Иерархические стурктуры


Рекурсия естественное средство для работы с древовидными структурами, так как мы можем свести операции над деревом к операции над веткой, которая будет вызываться рекурсивно. В качестве примера сравните процедурой length (из раздела 2.2.1) с процедурой count-leaves, которая возвращает количество листьев в дереве.

\begin{codelisting}{scheme}
(define x (cons (list 1 2) (list 3 4)))

(length x)
3
(count-leaves x)
4

(list x x)
(((1 2) 3 4) ((1 2) 3 4))

(length (list x x))
2

(count-leaves (list x x))
8
\end{codelisting}

;; Для реализации count-leaves, напомним рекурсивный план для вычисления длины.
;; * Lenght списка x равна 1 + Length списка (cdr x)
;; * Length пустого списка 0.

;; Count-leaves похожа. Значение пустого списка такое же.
;; * Count-leaves пустого списка - 0.
;; Но на этапе сокращения, когда мы убираем элемент car из списка, мы должны учесть что car в свою очередь является списком. Таким образом
;; * Count-leaves дерева x это count-leaves car x плюс count-leaves cdr x.

;; Наконец мы достигаем фактических листьев
;; * Count-leaves от одного листа равно 1.

;; Что бы помочь написанию рекурсивных обходов деревьев, Scheme представляет примитив pair?, которая проверяет является ли её аргумент парой.

(define (count-leaves x)
  (cond ((null? x) 0)
        ((not (pair? x)) 1)
        (else (+ (count-leaves (car x))
                 (count-leaves (cdr x))))))



(list 1 (list 2 (list 3 4)))

(define (left-norrow)
  (display "------->"))
(define (bottom-norrow)
  (newline)
  (display " | ")
  (newline)
  (display " v ")
  (newline))
(define (print-pointer x)
  (display "[ ") (display x) (display " ]")
  )
(print-leaf 4)

(define (right-from x)
  (if (and (not (null? x)) (pair? x))
      (print-pair x false))
  (if (and (not (null? x)) (not (pair? x)))
      (print-pointer x)))

(define (print-pair x center)
  (if (center)
      (newline))
  (display "[[[ ") (display x) (display " ]]]")
  (if (center)
      (left-norrow))
  (if (center)
      (right-from (cdr x)))
  (if (center)
      (bottom-norrow)
      )
  )
(print-pair 1)

(define (print-box-and-pointer x)
  (if (and (not (null? x)) (pair? x))
      (print-pair x 1))
  (if (and (not (null? x)) (not (pair? x)))
      (print-pointer x))

  (if (and (not (null? x)) (pair? x))
      (print-box-and-pointer (car x)))
  (if (and (not (null? x)) (pair? x))
      (print-box-and-pointer (cdr x))))
(print-box-and-pointer (list 1 (list 2 (list 3 4))))


;; Ex. 2.25

(car (cdr (car (cdr (cdr (list 1 3 (list 5 7) 9))))))


(car (car (list (list 7))))

(define list3 (list 1 (list 2 (list 3 (list 4 (list 5 (list 6 7)))))))
(car (cdr (car (cdr (car (cdr (car (cdr (car (cdr (car (cdr list3))))))))))))

(define x (list 1 2 3))
(define y (list 4 5 6))
(append x y)
(cons x y)
(list x y)

;; Ex. 2.27


(define (deep-reverse list1)
  (define (iter l result)
    (if (null? l)
        result
        (iter (cdr l) (cons (deep-reverse (car l)) result))))
  (if (pair? list1)
      (iter list1 nil)
      list1))

(define x (list (list 1 2) (list 3 4)))
(deep-reverse x)


;; Ex. 2.28

(define (fringe x)
  (if (null? x)
      x
      (if (pair? (car x))
          (append (fringe (car x)) (fringe (cdr x)))
          x)))


;; если это пара
(cons (fridge left) (fridge right))

(define x (list (list 1 2) (list 3 4)))
(fringe x)
(fringe (list x x))
(cons 1 (cons 2 nil))

(cons (cons 1 3) 3)

;; Ex. 2.29

(define (make-mobile left right)
  (list left right))

(define (make-branch length structure)
  (list length structure))
(define (left-branch m)
  (car m))
(define (right-branch m)
  (car (cdr m)))
(define (branch-length b)
  (car b))
(define (branch-structure b)
  (car (cdr b)))


(define l0 (make-branch 3 10))
(define r0 (make-branch 3 10))
(define m0 (make-mobile l0 r0))

(define l1 (make-branch 10 10))
(define r1 (make-branch 5 m0))
(define m1 (make-mobile l1 r1))

;; общий вес
(define (structure-weight s)
  (if (pair? s)
      (total-weight s)
      s))
(define (branch-weight b)
  (structure-weight (branch-structure b)))

(define (total-weight m)
  (+ (branch-weight (left-branch m)) (branch-weight (right-branch m))))

(total-weight m1)

;; балансировка
(define (balanced? m)
  (= (* (branch-weight (left-branch m)) (branch-length (left-branch m)))
     (* (branch-weight (right-branch m)) (branch-length (right-branch m)))))

(balanced? m1)
(balanced? m0)
(define (make-mobile left right)
  (cons left right))
(define (make-branch length structure)
  (cons length structure))

(define (left-branch m)
  (car m))
(left-branch m0)
(define (right-branch m) ;; поменял метод
  (cdr m))
(right-branch m0)
(define (branch-length b)
  (car b))
(define (branch-structure b) ;; поменял метод
  (cdr b))

;; Map применительно к деревьям.

(define (scale-tree tree factor)
  (cond ((null? tree) nil)
        ((not (pair? tree)) (* tree factor))
        (else (cons (scale-tree (car tree) factor)
                    (scale-tree (cdr tree) factor)))))
(scale-tree (list 1 (list 2 (list 3 4) 5) (list 6 7))
            10)

(define (scale-tree tree factor)
  (map (lambda (sub-tree)
         (if (pair? sub-tree)
             (scale-tree sub-tree factor)
             (* sub-tree factor)))
       tree))

(scale-tree (list 1 (list 2 (list 3 4) 5) (list 6 7))
            10)

;; Ex. 2.30

(define (square-tree tree)
  (cond ((null? tree) nil)
        ((not (pair? tree)) (square tree))
        (else (cons (square-tree (car tree))
                    (square-tree (cdr tree))))))

(define (square-tree tree)
  (map (lambda (sub-tree)
         (if (pair? sub-tree)
             (square-tree sub-tree)
             (square sub-tree)))
       tree))
(square-tree (list 1 (list 2 (list 3 4) 5) (list 6 7)))

;; Ex. 2.31

(define (tree-map proc tree)
  (map (lambda (sub-tree)
         (if (pair? sub-tree)
             (tree-map proc sub-tree)
             (proc sub-tree)))
       tree))

(define (square-tree tree) (tree-map square tree))
(square-tree (list 1 (list 2 (list 3 4) 5) (list 6 7)))


;; Ex. 2.32

(lambda (set) (cons (car s) set))

(define (subsets s)
  (if (null? s)
      (list nil)
      (let ((rest (subsets (cdr s))))
        (append rest (map (lambda (set) (cons (car s) set)) rest)))))

(subsets (list 1 2 3))

;; 2.2.3 Последовательности как обычные интерфейсы.

(define (sum-odd-squares tree)
  (cond ((null? tree) 0)
        ((not (pair? tree))
         (if (odd? tree) (square tree) 0))
        (else (+ (sum-odd-squares (car tree))
                 (sum-odd-squares (cdr tree))))))

(define tree0 (list 1 (list 3 4) (list 5 6)))
(sum-odd-squares tree0)

(define (even-fibs n)
  (define (next k)
    (if (> k n)
        nil
        (let ((f (fib k)))
          (if (even? f)
              (cons f (next (+ k 1)))
              (next (+ k 1))))))
  (next 0))
(even-fibs 10)

;; Несмотря на то что эти две функции выглядят сильно отличными
;; более абстрактное толкование показывает схожесть

;; 1 перечисляет список листьев.
;; фильтруте их выбирая нечетны.
;; возводит в квадрат каждый выбранный элемент
;; накапливает результат суммируя все полученные значения начиная с 0.

;; Вторая программа
;; перечисляет целые от 0 до n.
;; Вычисляет число фибоначи для каждого целого
;; фильтрует их выбирая четные.
;; накапливает результат используя cons начиная с пустого списка.

;; к сожалению мы видем что перечисление и фильтрация выполняются не последовательно, а перемешиваются. Если мы сможем оргвнизовать код таким образом что бы эти операции выполнялись последовательно, мы сможем применить общий паттерн.

;; enumerate ---> filter odd? ---> map: square ---> accumulates: +, 0
;; enumerate ---> map fib ---> filter even? ---> accumulates: cons, ()

(map square (list 1 2 3 4 5))

(define (filter predicate sequence)
  (cond ((null? sequence) nil)
        ((predicate (car sequence))
         (cons (car sequence)
               (filter predicate (cdr sequence))))
        (else (filter predicate (cdr sequence)))))
(filter odd? (list 1 2 3 4 5))

(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))
(accumulate + 0 (list 1 2 3 4 5))
(accumulate * 1 (list 1 2 3 4 5))
(accumulate cons nil (list 1 2 3 4 5))

;; осталось реализовать enumerate
(define (enumerate-interval low high)
  (if (> low high)
      nil
      (cons low (enumerate-interval (+ low 1) high))))
(enumerate-interval 2 7)

;; это по факту процедура fringe
(define (enumerate-tree tree)
  (cond ((null? tree) nil)
        ((not (pair? tree)) (list tree))
        (else (append (enumerate-tree (car tree))
                      (enumerate-tree (cdr tree))))))

(enumerate-tree (list 1 (list 2 (list 3 4)) 5))

;; теперь мы можем переформулировать sum-odd-squares even-fibs как на диаграмме сигнал-поток


(define (sum-odd-squares tree)
  (accumulate + 0 (map square (filter odd? (enumerate-tree tree)))))

(sum-odd-squares (list 1 (list 2 (list 3 4)) 5))


(define (even-fibs n)
  (accumulate cons () (filter even? (map fib (enumerate-interval 0 n)))))
(even-fibs 9)

;; Выражая программы как последовательность операций мы делаем их более модульными легко изменяемыми.


;; В реальных приложениях обработки сигналов разработчики регулярно создают системы с помощью каскадных элементов, выбранных из стандартизированных семейств фильтров и преобразователей.

(define (salary-of-highest-paid-programmer records)
  (accumulate max
              0
              (map salary
                   (filter programmer? records))))

;; Упр. 2.33

(define (map p sequence)
  (accumulate (lambda (x y) (cons (p x) y)) nil sequence))
(map square (list 1 2 3))

;; (define (accumulate op initial sequence)
;;   (if (null? sequence)
;;       initial
;;       (op (car sequence)
;;           (accumulate op initial (cdr sequence)))))

(define (append seq1 seq2)
  (accumulate cons seq2 seq1))
(append (list 1 2) (list 3 4))

(define (length sequence)
  (accumulate (lambda (x y) (+ 1 y)) 0 sequence))
.
(length (list 1 2 3))

((lambda (x y) (display x) ) (list 1 2))

;; Упр. 2.34

(define (horner-eval x coefficient-sequence)
  (accumulate (lambda (this-coeff higher-terms) (+ this-coeff (* x higher-terms)))
              0
              coefficient-sequence))

(horner-eval 2 (list 1 3 0 5 0 1))

;; Упр. 2.35

(define (count-leaves x)
  (cond ((null? x) 0)
        ((not (pair? x)) 1)
        (else (+ (count-leaves (car x))
                 (count-leaves (cdr x))))))

(define (count-leaves t)
  (accumulate + 0
              (map (lambda (sub-tree)
                     (if (pair? sub-tree)
                         (count-leaves sub-tree)
                         1)) t)))

(count-leaves (list 1 (list 1 2) (list 3 4)))

(map (lambda (sub-tree) (enumerate-tree sub-tree)) (list (list 1 2) (list 3 4)))
(map (lambda (sub-tree) (enumerate-tree sub-tree)) (list (list 1 2) (list 3 4)))

;; Ex 2.36

(define (accumulate-n op init seqs)
  (if (null? (car seqs))
      nil
      (cons (accumulate op init (map (lambda (seq) (car seq)) seqs))
            (accumulate-n op init (map (lambda (seq) (cdr seq)) seqs)))))

(define s (list (list 1 2 3) (list 4 5 6) (list 7 8 9) (list 10 11 12)))
(accumulate-n + 0 s)


;; 12 Scheme standardly provides a map procedure that is more general than the one described here. This more general map takes a procedure of n arguments, together with n lists, and applies the procedure to all the first elements of the lists, all the second elements of the lists, and so on, returning a list of the results. For example:

;; (map + (list 1 2 3) (list 40 50 60) (list 700 800 900))
;; (741 852 963)

;; (map (lambda (x y) (+ x (* 2 y)))
;;      (list 1 2 3)
;;      (list 4 5 6))
;; (9 12 15)

;; Упр. 2.37

(map * (list 1 2 3) (list 1 2 3))

(map (lambda (x) 0) (list 1 2 3))

(define (dot-product v w)
  (accumulate + 0 (map * v w)))
(dot-product (list 1 2 3) (list 1 2 3))


(define (matrix-*-vector m v)
  (map (lambda (w) (dot-product w v)) m))

(matrix-*-vector (list (list 1 2 3) (list 1 2 3) (list 1 2 3)) (list 1 2 3))


(define (transpose mat)
  (accumulate-n cons () mat))

(transpose (list (list 1 2 3) (list 1 2 3) (list 1 2 3)))


(define (matrix-*-matrix m n)
  (let ((cols (transpose n)))
    (map (lambda (row) (matrix-*-vector cols row)) m)))


(matrix-*-matrix (list (list 2 0 -1) (list 0 -2 2))
                 (list (list 4 1 0) (list 3 2 1) (list 0 1 0)))

;; Упр 2.38

(define (fold-left op initial sequence)
  (define (iter result rest)
    (if (null? rest)
        result
        (iter (op result (car rest))
              (cdr rest))))
  (iter initial sequence))

(accumulate / 1 (list 1 2 3))
(accumulate list nil (list 1 2 3))
(fold-left / 1 (list 1 2 3))
(fold-left list nil (list 1 2 3))

;; 1 (1 2 3)
;; 1/1 (2 3)
;; 1 / 2 (3)
;; 0.5 / 3 nil
;; 0.16

;; Упр 2.39
(define (reverse sequence)
  (accumulate (lambda (first part-reversed) (append part-reversed (list first))) nil sequence))
(reverse (list 1 2 3))


(append (list 1 2 3) (list 2))
(reverse (list 1 2 3))


(define (reverse sequence)
  (fold-left (lambda (result last) (cons last result)) nil sequence))
(reverse (list 1 2 3))


;; Вложенные отображения (mappings)

(accumulate append
            nil
            (map (lambda (i)
                   (map (lambda (j) (list i j))
                        (enumerate-interval 1 (- i 1))))
                 (enumerate-interval 1 n)))

;; комбинация mapping и accumulate с append так распространена в таком типе программ что мы выделим её в отдельную процедуру.

(cons 1 nil)
(define (flatmap proc seq)
  (accumulate append nil (map proc seq)))

(define (prime-sum? pair)
  (prime? (+ (car pair) (cadr pair))))

(define (make-pair-sum pair)
  (list (car pair) (cadr pair) (+ (car pair) (cadr pair))))


(define (prime-sum-pairs n)
  (map make-pair-sum
       (filter prime-sum?
               (flatmap
                (lambda (i)
                  (map (lambda (j) (list i j))
                       (enumerate-interval 1 (- i 1))))
                (enumerate-interval 1 n)))))

(prime-sum-pairs 6)

;; Упр. 2.40

(define (unique-pairs n)
  (flatmap
   (lambda (i)
     (map (lambda (j) (list i j))
          (enumerate-interval 1 (- i 1))))
   (enumerate-interval 1 n)))

(unique-pairs 5)


(define (prime-sum-pairs n)
  (map make-pair-sum
       (filter prime-sum?
               (unique-pairs n))))
(prime-sum-pairs 6)

(pairs 4)


(define (unique-pairs n)
  (flatmap
   (lambda (i)
     (map (lambda (j)
            (lol)) (enumerate-interval 1 (- i 1)))
       (map (lambda (k)
         (list i j k)) (enumerate-interval 1 (- j 1)))
       (enumerate-interval 1 (- i 1))))
   (enumerate-interval 1 n)))


;; Ex. 2.41

(define (not-end-with-1? pair)
  (not (= (cadr pair) 1)))

(define (uniq-3 n)
  (flatmap
   (lambda (pair)
     (map (lambda (k) (list (car pair) (cadr pair) k))
          (enumerate-interval 1 (- (cadr pair) 1))))
   (filter not-end-with-1? (unique-pairs n))))

(uniq-3 5)
(filter not-end-with-1? (unique-pairs 5))


(define (uniq-tuples-m n m)
  (cond ((< n m) nil)
        ((= m 1) (map list (enumerate-interval 1 n)))
        (else (flatmap
               (lambda (tuple)
                 (map (lambda (k) (cons k tuple))
                      (enumerate-interval (+ (car tuple) 1) n)))
               (uniq-tuples-m (- n 1) (- m 1))))))

(uniq-tuples-m 5 3)

;; Ex. 2.42


(define empty-board ())

(define (adjoin-position new-row k rest-of-queens)
  (cons new-row rest-of-queens))

(define (include-row? item positions)
  (if (null? positions)
      false
      (or (= item (car positions)) (include? item (cdr positions)))))


(define (include-diagonale? item positions k)
  (define (positive item positions k)
    (if (null? positions)
        false
        (or (= (+ item 1) (car positions)) (positive (+ item 1) (cdr positions) k))
        ))
  (define (negative item positions k)
    (if (or (null? positions) (< (- item 1) 1))
        false
        (or (= (- item 1) (car positions)) (negative (- item 1) (cdr positions) k))
        ))
  (or (negative item positions k) (positive item positions k)))

(include? 1 (list 2 3 3))
(include-diagonale? 2 (list 1 1 1) 4)


(define (safe? k positions)
  (and
   (not (include-row? (car positions) (cdr positions))) ;; нет совпадений по строкам
   (not (include-diagonale? (car positions) (cdr positions) k)))) ;; не совпадает по диагоналям

(safe? 2 (list 1 3))

(define (queens board-size)
  (define (queen-cols k)
    (if (= k 0)
        (list empty-board)
        (filter
         (lambda (positions) (safe? k positions))
         (flatmap
          (lambda (rest-of-queens)
            (map (lambda (new-row)
                   (adjoin-position new-row k rest-of-queens))
                 (enumerate-interval 1 board-size)))
          (queen-cols (- k 1))))))
  (queen-cols board-size))

(queens 8)
(length (queens 8))


;;rest-of-queens = (list 1 2 3 4 5 6) (list 2 3 4 5 6)
;; Ex. 2.43


(flatmap
 (lambda (rest-of-queens)
   (map (lambda (new-row)
          (adjoin-position new-row k rest-of-queens))
        (enumerate-interval 1 board-size)))
 (queen-cols (- k 1)))


;; в предведущем варианте было есть варианты к каждому добавляем число в начала получаем порядок роста N
;; если nxn
;; позиций из 1-ого столбца на первой n*n 8*8
;; позиций из 2х столбцов 8*8*8 без фильтрации с фильтрации сильно меньше.
;; и так далее
;; то есть мы для каждой позиции добавляем  вначала от 1 до 8 у нас увеличивается количество в 8 раз

;; во втором случае
(flatmap
 (lambda (new-row)
   (map (lambda (rest-of-queens)
          (adjoin-position new-row k rest-of-queens))
        (queen-cols (- k 1))))
 (enumerate-interval 1 board-size))


(define (interval-with-print n)
  (display "Call")
  (newline)
  (enumerate-interval 1 n))


(flatmap
 (lambda (i)
   (map
    (lambda (j)
      (list i j))
    (interval-with-print 8)))
 (enumerate-interval 1 8))



;; восемь раз для всех предведущих позиций мы добавляем по одной записи.
;; то есть если оценивать сложность относительно количества позиций, то оно n^8 а для предведущего 8*n

;; 2.2.4 Example: A Picture Language

(define wave2 (beside wave (flip-vert wave)))
(define wave4 (below wave2 wave2))

(define (flipped-pairs painter)
  (let ((painter2 (beside painter (flip-vert painter))))
    (below painter2 painter2)))

(define wave4 (flipped-pairs wave))

(define (right-split painter n)
  (if (= n 0)
      painter
      (let ((smaller (right-split painter (- n 1))))
        (beside painter (below smaller smaller)))))


(define (up-split painter n)
  (if (= n 0)
      painter
      (let ((smaller (up-split painter (- n 1))))
        (below painter (beside smaller smaller)))))


(define (corner-split painter n)
  (if (= n 0)
      painter
      (let ((up (up-split painter (- n 1)))
            (right (right-split painter (- n 1))))
        (let ((top-left (beside up up))
              (bottom-right (below right right))
              (corner (corner-split painter (- n 1))))
          (beside (below painter top-left)
                  (below bottom-right corner))))))



(define (flipped-pairs painter)
  (let ((painter2 (beside painter (flip-vert painter))))
    (below painter2 painter2)))

(define (flipped-pairs painter)
  (let ((combine4 (square-of-four identity flip-vert
                                  identity flip-vert)))
    (combine4 painter)))

(define (square-limit painter n)
  (let ((combine4 (square-of-four flip-horiz identity
                                  rotate180 flip-vert)))
    (combine4 (corner-split painter n))))

(define (square-of-four tl tr bl br)
  (lambda (painter)
    (let ((top (beside (tl painter) (tr painter)))
          (bottom (beside (bl painter) (br painter))))
      (below bottom top))))

;; Ex. 2.45

(define (right-split painter n)
  (if (= n 0)
      painter
      (let ((smaller (right-split painter (- n 1))))
        (beside painter (below smaller smaller)))))


(define (up-split painter n)
  (if (= n 0)
      painter
      (let ((smaller (up-split painter (- n 1))))
        (below painter (beside smaller smaller)))))

(define (split fs ss)
  (lambda (painter, n)
    (if (= n 0)
        painter
        (let ((smaller ((split fs ss) painter (- n 1))))
          (fs painter (ss smaller smaller))))))
(define right-split (split beside below))
(define up-split (split below beside))

;; Frames

(define (frame-coord-map frame)
  (lambda (v)
    (add-vect
     (origin-frame frame)
     (add-vect (scale-vect (xcor-vect v)
                           (edge1-frame frame))
               (scale-vect (ycor-vect v)
                           (edge2-frame frame))))))

((frame-coord-map a-frame) (make-vect 0 0))
;; return
(origin-frame a-frame)

;; Ex. 2.46
(define (make-vect x y)
  (cons x y))
(define (xcor-vect v)
  (car v))
(define (ycor-vect v)
  (cdr v))
(ycor-vect (make-vect 1 2))

(define (add-vect v1 v2)
  (make-vect (+ (xcor-vect v1) (xcor-vect v2)) (+ (ycor-vect v1) (ycor-vect v2))))

(define (sub-vect v1 v2)
  (make-vect (- (xcor-vect v1) (xcor-vect v2)) (- (ycor-vect v1) (ycor-vect v2))))
(define (scale-vect n v)
  (make-vect (* (xcor-vect v) n) (* (ycor-vect v) n)))

;; Ex. 2.46

(define (origin-frame frame)
  (car frame))
(define (edge1-frame frame)
  (card frame))
(define (edge1-frame frame)
  (card (cdr frame)))

(define (origin-frame frame)
  (car frame))
(define (edge1-frame frame)
  (car (cdr frame)))
(define (edge1-frame frame)
  (cdr (cdr frame)))


;; в общем идея в том что рисовать что-то только в пределах фрейма.

(define (segments->painter segment-list)
  (lambda (frame)
    (for-each
     (lambda (segment)
       (draw-line
        ((frame-coord-map frame) (start-segment segment))
        ((frame-coord-map frame) (end-segment segment))))
     segment-list)))

;; Ex. 2.48

(define (make-segment v1 v2)
  (cons v1 v2))
(define (start-segment seg)
  (car seg))
(define (end-segment seg)
  (cdr seg))


;; Ex 2.49
(define (for-each proc items)
  (if (not (null? items))
      (proc (car items)))
  (if (not (null? items))
      (for-each proc (cdr items)))
  true)
;; frame
(define (make-frame origin edge1 edge2)
  (list origin edge1 edge2))

(define (origin-frame frame)
  (car frame))
(define (edge1-frame frame)
  (cadr frame))
(define (edge2-frame frame)
  (cadr (cdr frame)))

;; segment
(define (make-segment v1 v2)
  (cons v1 v2))
(define (start-segment seg)
  (car seg))
(define (end-segment seg)
  (cdr seg))

;; vector
(define (make-vect x y)
  (cons x y))
(define (xcor-vect v)
  (car v))
(define (ycor-vect v)
  (cdr v))

(define (add-vect v1 v2)
  (make-vect (+ (xcor-vect v1) (xcor-vect v2)) (+ (ycor-vect v1) (ycor-vect v2))))

(define (sub-vect v1 v2)
  (make-vect (- (xcor-vect v1) (xcor-vect v2)) (- (ycor-vect v1) (ycor-vect v2))))
(define (scale-vect v n)
  (make-vect (* (xcor-vect v) n) (* (ycor-vect v) n)))

;; a

(define (segments->painter segment-list)
  (lambda (frame)
    (for-each
     (lambda (segment)
       (draw-line
        ((frame-coord-map frame) (start-segment segment))
        ((frame-coord-map frame) (end-segment segment))))
     segment-list)))

(define frame1 (make-frame (make-vect 0 0) (make-vect 0 1) (make-vect 1 0)))

(define segment-list-a (list (make-segment (make-vect 0 0) (make-vect 0 1))
                            (make-segment (make-vect 0 1) (make-vect 1 1))
                            (make-segment (make-vect 1 1) (make-vect 1 0))
                            (make-segment (make-vect 1 0) (make-vect 0 0))
                            ))

((segments->painter segment-list-a) frame1)

(define segment-list-b (list (make-segment (make-vect 0 0) (make-vect 1 1))
                            (make-segment (make-vect 0 1) (make-vect 1 0))
                            ))

((segments->painter segment-list-b) frame1)


(define segment-list-c (list (make-segment (make-vect 0.5 0) (make-vect 1 0.5))
                            (make-segment (make-vect 1 0.5) (make-vect 0.5 1))
                            (make-segment (make-vect 0.5 1) (make-vect 0 0.5))
                            (make-segment (make-vect 0 0.5) (make-vect 0.5 0))
                            ))

((segments->painter segment-list-c) frame1)


(define segment-list-d (list (make-segment (make-vect .25 0) (make-vect .35 .5))
                             (make-segment (make-vect .35 .5) (make-vect .3 .6))
                             (make-segment (make-vect .3 .6) (make-vect .15 .4))
                             (make-segment (make-vect .15 .4) (make-vect 0 .65))
                             (make-segment (make-vect 0 .65) (make-vect 0 .85))
                             (make-segment (make-vect 0 .85) (make-vect .15 .6))
                             (make-segment (make-vect .15 .6) (make-vect .3 .65))
                             (make-segment (make-vect .3 .65) (make-vect .4 .65))
                             (make-segment (make-vect .4 .65) (make-vect .35 .85))
                             (make-segment (make-vect .35 .85) (make-vect .4 1))
                             (make-segment (make-vect .4 1) (make-vect .6 1))
                             (make-segment (make-vect .6 1) (make-vect .65 .85))
                             (make-segment (make-vect .65 .85) (make-vect .6 .65))
                             (make-segment (make-vect .6 .65) (make-vect .75 .65))
                             (make-segment (make-vect .75 .65) (make-vect 1 .35))
                             (make-segment (make-vect 1 .35) (make-vect 1 .15))
                             (make-segment (make-vect 1 .15) (make-vect .6 .45))
                             (make-segment (make-vect .6 .45) (make-vect .75 0))
                             (make-segment (make-vect .75 0) (make-vect .6 0))
                             (make-segment (make-vect .6 0) (make-vect .5 .3))
                             (make-segment (make-vect .5 .3) (make-vect .4 0))
                             (make-segment (make-vect .4 0) (make-vect .25 0))
                             ))

((segments->painter segment-list-d) frame1)


;; Преобразования на плоскости.

;; flip-vert разворачивает рамку а не сегменты.

(define (transform-painter painter origin corner1 corner2)
  (lambda (frame)
    (let ((m (frame-coord-map frame)))
      (let ((new-origin (m origin)))
        (painter
         (make-frame new-origin
                     (sub-vect (m corner1) new-origin)
                     (sub-vect (m corner2) new-origin)))))))
(define (flip-vert painter)
  (transform-painter painter
                     (make-vect 0.0 1.0)   ; new origin
                     (make-vect 1.0 1.0)   ; new end of edge1
                     (make-vect 0.0 0.0))) ; new end of edge2

(define (shrink-to-upper-right painter)
  (transform-painter painter
                     (make-vect 0.5 0.5)
                     (make-vect 1.0 0.5)
                     (make-vect 0.5 1.0)))

;; Ex. 2.50

(define (flip-horiz painter)
  (transform-painter painter
                     (make-vect 1.0 .0)
                     (make-vect .0 .0)
                     (make-vect 1.0 1.0)))

(define (rotate90 painter)
  (transform-painter painter
                     (make-vect 1.0 0.0)
                     (make-vect 1.0 1.0)
                     (make-vect 0.0 0.0)))

(define (rotate270 painter)
  ((repeated rotate90 3) painter))

(define (rotate180 painter)
  ((repeated rotate90 2) painter))

;; Ex 2.51



(define (below painter1 painter2)
  (let ((split-point (make-vect 0.0 0.5)))
    (let ((paint-bottom
           (transform-painter painter1
                              (make-vect 0.0 0.0)
                              (make-vect 1.0 0.0)
                              split-point))
          (paint-top
           (transform-painter painter2
                              split-point
                              (make-vect 1.0 0.5)
                              (make-vect 0.0 1.0))))
      (lambda (frame)
        (paint-bottom frame)
        (paint-top frame)))))


(define (below-2 painter1 painter2)
   (rotate90 (beside (rotate270 painter1) (rotate270 painter2))))


;; Уровни языка для надежного дизайна
;; Подход многослойного дизайна, сложная система должна быть спроектирована как последовательность уровней, которые описывают с использованием последовательности языков.
;; Каждый уровень создается путем объединения частей, которые рассматриваются как примитивные на этом уровне и части сконструированные (объединенные) на каждом уровне используются как примитивы для следующего уровня. Язык используемый на каждом уровне, имеет примитивы, средства комбинирования и средства построения абстракций, соответствующие этому уровню детализации.

;; robost.

(define (corner-split painter n)
  (if (= n 0)
      painter
      (let ((up (up-split painter (- n 1)))
            (right (right-split painter (- n 1))))
        (let ((top-left (beside up up))
              (bottom-right (below right right))
              (corner (corner-split painter (- n 1))))
          (beside (below painter top-left)
                  (below bottom-right corner))))))

(define (square-of-four tl tr bl br)
  (lambda (painter)
    (let ((top (beside (tl painter) (tr painter)))
          (bottom (beside (bl painter) (br painter))))
      (below bottom top))))

(define (square-limit painter n)
  (let ((combine4 (square-of-four flip-horiz identity
                                  rotate180 flip-vert)))
    (combine4 (corner-split painter n))))


(cdr (list (list 1 1) (list 2 2) (list 3 3)))



(define frame1 (make-frame (make-vect 0 0) (make-vect 0 1) (make-vect 1 0)))

((frame-coord-map frame1) (cons 1 2))


(define (corner-split painter n)
  (if (= n 0)
      painter
      (let ((up (up-split painter (- n 1)))
            (right (right-split painter (- n 1))))
        (let ((top-left (beside up up))
              (bottom-right (below right right))
              (corner (corner-split painter (- n 1))))
          (beside (below painter top-left)
                  (below painter corner))))))


(define (square-limit painter n)
   (let ((combine4 (square-of-four flip-vert rotate180
                                   identity flip-horiz)))
     (combine4 (corner-split painter n))))
;; (define (corner-split painter n)
;;   (if (= n 0)
;;       painter
;;       (let ((up (up-split painter (- n 1)))
;;             (right (right-split painter (- n 1))))
;;         (let ((top-left (beside up up))
;;               (bottom-right (below right right))
;;               (corner (corner-split painter (- n 1))))
;;           (beside (below painter top-left)
;;                   (below bottom-right corner))))))

;; раздел 2.3
(cdr (cdr '(x (apple sauce) y apple pear)))

;; Ex. 2.53

(list 'a 'b 'c)

(list (list 'george))
(cdr '((x1 x2) (y1 y2)))

(cadr '((x1 x2) (y1 y2)))
(pair? (car '(a short list)))
(memq 'red '((red shoes) (blue socks)))

(memq 'red '(red shoes blue socks))
(eq? 'a 'a)
(define (equal? a b)
  (if (not (and (pair? a) (pair? b)))
      (eq? a b)
      (and (equal? (car a) (car b)) (equal? (cdr a) (cdr b)))))
(equal? '(this is a list) '(this is a list))
(equal? '(this is a list) '(this (is a) list))
(equal? 1 '(this is))

(equal? '(this (is a) list) '(this (is a) list))
(eq? 1 1)
(car ''asdfdsffd)
'''asdfsfsf

;; дефференцирование на абстрактных данных

(variable? e) ;; Is e a variable?
(same-variable? v1 v2) ;; Are v1 and v2 the same variable?

(sum? e) ;; Is e a sum?
(addend e) ;; Addend of the sum e.
(augend e) ;; Augend of the sum e.
(make-sum a1 a2) ;; Construct the sum of a1 and a2.

(product? e) ;; Is e a product?
(multiplier e) ;; Multiplier of the product e.
(multiplicand e) ;; Multiplicand of the product e.
(make-product m1 m2) ;; Construct the product of m1 and m2.

(define (deriv exp var)
  (cond ((number? exp) 0)
        ((variable? exp)
         (if (same-variable? exp var) 1 0))
        ((sum? exp)
         (make-sum (deriv (addend exp) var)
                   (deriv (augend exp) var)))
        ((product? exp)
         (make-sum
           (make-product (multiplier exp)
                         (deriv (multiplicand exp) var))
           (make-product (deriv (multiplier exp) var)
                         (multiplicand exp))))
        (else
         (error "unknown expression type -- DERIV" exp))))

;; представление алгебраических выражений

(define (variable? x) (symbol? x))
(define (same-variable? v1 v2)
  (and (variable? v1) (variable? v2) (eq? v1 v2)))

(define (make-sum a1 a2) (list '+ a1 a2))
(define (make-product m1 m2) (list '* m1 m2))

(define (sum? x)
  (and (pair? x) (eq? (car x) '+)))
(define (addend s) (cadr s))
(define (augend s) (caddr s))
(define (product? x)
  (and (pair? x) (eq? (car x) '*)))
(define (multiplier p) (cadr p))
(define (multiplicand p) (caddr p))

(deriv '(+ x 3) 'x)
(deriv '(* x y) 'x)
(deriv '(* (* x y) (+ x 3)) 'x)

;; сокращаем вывод при умножении на 0
(number? 1)
(define (=number? exp num)
  (and (number? exp) (= exp num)))

(define (make-sum a1 a2)
  (cond ((=number? a1 0) a2)
        ((=number? a2 0) a1)
        ((and (number? a1) (number? a2)) (+ a1 a2))
        (else (list '+ a1 a2))))
(define (make-product m1 m2)
  (cond ((or (=number? m1 0) (=number? m2 0)) 0)
        ((=number? m1 1) m2)
        ((=number? m2 1) m1)
        ((and (number? m1) (number? m2)) (* m1 m2))
        (else (list '* m1 m2))))

;; Ex 2.56

(define (exponentiation? x)
  (and (pair? x) (eq? (car x) '**)))
(define (base exponentiation) (cadr exponentiation))
(define (exponent exponentiation) (caddr exponentiation))

(define (make-exponentiation b1 e2)
  (cond ((=number? e2 0) 1)
        ((=number? e2 1) b1)
        ((=number? b1 0) 1)
        ((and (number? b1) (number? e2)) (expt b1 e2))
        (else (list '** b1 e2))))

(define (deriv exp var)
  (cond ((number? exp) 0)
        ((variable? exp)
         (if (same-variable? exp var) 1 0))
        ((sum? exp)
         (make-sum (deriv (addend exp) var)
                   (deriv (augend exp) var)))
        ((product? exp)
         (make-sum
           (make-product (multiplier exp)
                         (deriv (multiplicand exp) var))
           (make-product (deriv (multiplier exp) var)
                         (multiplicand exp))))
        ((exponentiation? exp)
         (make-product (make-product (exponent exp)
                                     (make-exponentiation (base exp)
                                                          (make-sum (exponent exp)
                                                                    -1)))
                       (deriv (base exp) var)))
        (else
         (error "unknown expression type -- DERIV" exp))))

(deriv '(** x 4) 'x)

;; Ex. 2.57

(define (make-sum a1 a2 . an)
  (cond ((and (number? a1) (number? a2)) (append (list '+ (+ a1 a2)) an))
        (else (append (list '+ a1 a2) an))))

(make-sum 1 2 3 4)

(append (list '+ 1 2) (list 'an))


(define (make-product m1 m2 . mn)
  (cond ((or (=number? m1 0) (=number? m2 0)) 0)
        ;; ((=number? m1 1) m2)
        ;; ((=number? m2 1) m1)
        ((and (number? m1) (number? m2)) (append (list '+ (* m1 m2)) mn))
        (else (append (list '* m1 m2) mn))))

(make-product 1 2 3 4)

(define (addend s) (cadr s))
(define (augend s)
  (if (= (length s) 3)
      (caddr s)
      (append (list '+) (cdr (cdr s)))))

(addend (make-sum 1 2 3 4))
(augend (make-sum 1 2 3 4))
(define (multiplier p) (cadr p))
(define (multiplicand p)
  (if (= (length p) 3)
      (caddr p)
      (append (list '*) (cdr (cdr p)))))
;; (caddr p))

(multiplier (make-product 'x 'y 3))
(multiplicand (make-product 'x 'y 3))

(cdr (cdr (list 1 2 3)))
(deriv '(* x y (+ x 3)) 'x)

;; Ex. 2.58

(define (variable? x) (symbol? x))
(define (same-variable? v1 v2)
  (and (variable? v1) (variable? v2) (eq? v1 v2)))

(define (make-sum a1 a2) (append (list a1) (list '+) (list a2)))


(define (sum? x)
  (and (pair? x) (eq? (cadr x) '+)))
(define (addend s) (car s))
(define (augend s) (caddr s))
(addend (make-sum 1 2))
(augend (make-sum 1 2))

(define (make-product m1 m2) (append (list m1) (list '*) (list m2)))
(define (product? x)
  (and (pair? x) (eq? (cadr x) '*)))
(define (multiplier p) (car p))
(define (multiplicand p) (caddr p))
(make-product 1 2)
(product? (make-product 1 2))
(multiplier (make-product 1 2))
(multiplicand (make-product 1 2))


(define (deriv exp var)
  (cond ((number? exp) 0)
        ((variable? exp)
         (if (same-variable? exp var) 1 0))
        ((sum? exp)
         (make-sum (deriv (addend exp) var)
                   (deriv (augend exp) var)))
        ((product? exp)
         (make-sum
           (make-product (multiplier exp)
                         (deriv (multiplicand exp) var))
           (make-product (deriv (multiplier exp) var)
                         (multiplicand exp))))
        ((exponentiation? exp)
         (make-product (make-product (exponent exp)
                                     (make-exponentiation (base exp)
                                                          (make-sum (exponent exp)
                                                                    -1)))
                       (deriv (base exp) var)))
        (else
         (error "unknown expression type -- DERIV" exp))))

(deriv '(x + (3 * (x + (y + 2)))) 'x)

;; b
(deriv '(x + 3 * (x + y + 2)) 'x)

(augend '(x + 3 * (x + y + 2)))
(product? '(x + 3 * (x + y + 2)))


(define (search-first-operand-position op exp n)
  (if (or (not (pair? exp)) (null? exp) (< (length exp) 3))
      0
      (if (eq? (cadr exp) op)
          (+ n 1)
          (search-first-operand-position op (cdr (cdr exp)) (+ n 1)))))

(cadr (cdr (cdr '(3 * (x + y + 2)))))
(length '((x + y + 2)))
(search-first-operand-position '+ '(3 + (x + y + 2)) 0)
(search-first-operand-position '+ '((x + y + 2)) 0)
(define (make-sum a1 a2) (append (list a1) (list '+) (list a2)))
(define (sum? s)
  (let ((fop (search-first-operand-position '+ s 0)))
    (> fop 0)))
(product? '(3 * (x + y + 2)))
(define (addend s)
  (let ((rev (reverse s)))
    (let ((fop (search-first-operand-position '+ rev 0)) (addends ()))
      (define (iter-shift ss n)
        (if (= n 0)
            ss
            (iter-shift (cdr (cdr ss)) (- n 1))))
      (define addends (iter-shift rev fop))
      (if (= (length addends) 1)
          (car addends)
          (reverse addends)))))

(define (augend s)
  (let ((fop (search-first-operand-position '+ s 0)) (aug ()))
    (define (iter-shift ss n)
      (if (= n 0)
          ss
          (iter-shift (cdr (cdr ss)) (- n 1))))
    (define aug (iter-shift s fop))
    (if (= (length aug) 1)
        (car aug)
        aug)))

(sum? '(x * (x + y + 2) + 3))
(augend '(x * 3 * (x + y + 2) + 3))
(addend '(x * 3 * (x + y + 2) + 3))


(sum? '(x + 3 * (x + y + 2)))
(augend '(x + 3 * (x + y + 2)))
(addend '(x + 3 * (x + y + 2)))

(define (deriv exp var)
  (cond ((number? exp) 0)
        ((variable? exp)
         (if (same-variable? exp var) 1 0))
        ((sum? exp)
         (make-sum (deriv (addend exp) var)
                   (deriv (augend exp) var)))
        ((product? exp)
         (make-sum
           (make-product (multiplier exp)
                         (deriv (multiplicand exp) var))
           (make-product (deriv (multiplier exp) var)
                         (multiplicand exp))))
        ((exponentiation? exp)
         (make-product (make-product (exponent exp)
                                     (make-exponentiation (base exp)
                                                          (make-sum (exponent exp)
                                                                    -1)))
                       (deriv (base exp) var)))
        (else
         (error "unknown expression type -- DERIV" exp))))

(deriv '(x + 3 * (x + y + 2)) 'x)

(make-sum
 (deriv (addend '(x + 3 * (x + y + 2))) 'x)
 (deriv (augend '(x + 3 * (x + y + 2))) 'x))
(addend '(x + 3 * (x + y + 2)))
(make-sum
           (make-product (multiplier '(3 * (x + y + 2)))
                         (deriv (multiplicand '(3 * (x + y + 2))) 'x))
           (make-product (deriv (multiplier '(3 * (x + y + 2))) 'x)
                         (multiplicand '(3 * (x + y + 2)))))


(product? '(3 * (x + y + 2)))
(make-sum
 (deriv (multiplier '(3 * (x + y + 2))) 'x)
 (deriv (multiplicand '(3 * (x + y + 2))) 'x))


(deriv '(x + 3 * (x + y + 2) + (x * x)) 'x)


;; 2.3.3  Пример: Представление множеств (Sets)

(define (union-set ))
(define (intersection-set ))
(define (element-of-set? x set))
(define (adjoin-set ))

;; Сеты как не сортированные списки

(define (element-of-set? x set)
  (cond ((null? set) false)
        ((equal? x (car set)) true)
        (else (element-of-set? x (cdr set)))))

(define (adjoin-set x set)
  (if (element-of-set? x set)
      set
      (cons x set)))

(define (intersection-set set1 set2)
  (cond ((or (null? set1) (null? set2)) '())
        ((element-of-set? (car set1) set2)
         (cons (car set1)
               (intersection-set (cdr set1) set2)))
        (else (intersection-set (cdr set1) set2))))

(define (union-set set1 set2)
  (cond ((null? set2) set1)
        ((element-of-set? (car set2) set1) (union-set set1 (cdr set2)))
        (else (cons (car set2) (union-set set1 (cdr set2))))))
(union-set (list 1 2 3) (list 4 2 5))


;; с повторами

(define (element-of-set? x set)
  (cond ((null? set) false)
        ((equal? x (car set)) true)
        (else (element-of-set? x (cdr set)))))

(define (adjoin-set x set)
  (cons x set))

(define (intersection-set set1 set2)
  (cond ((or (null? set1) (null? set2)) '())
        ((element-of-set? (car set1) set2)
         (cons (car set1)
               (intersection-set (cdr set1) set2)))
        (else (intersection-set (cdr set1) set2))))


(define (union-set set1 set2)
  (append set1 set2))

(element-of-set? 1 (list 2 2 3 1))
(adjoin-set 1 (list 2 2 3 1))
(intersection-set (list 1 1 1 2 2) (list 3 4 2))
(union-set (list 1 1 1 2 2) (list 3 4 2))

;; как сортированные списки


;; по прежнему n, но иногда спасает и позволяет не делать лишние шаги
(define (element-of-set? x set)
  (cond ((null? set) false)
        ((= x (car set)) true)
        ((< x (car set)) false)
        (else (element-of-set? x (cdr set)))))

;; тут получается порядок роста n вместо n^2 для несортированного списка
(define (intersection-set set1 set2)
  (if (or (null? set1) (null? set2))
      '()
      (let ((x1 (car set1)) (x2 (car set2)))
        (cond ((= x1 x2)
               (cons x1
                     (intersection-set (cdr set1)
                                       (cdr set2))))
              ((< x1 x2)
               (intersection-set (cdr set1) set2))
              ((< x2 x1)
               (intersection-set set1 (cdr set2)))))))

(define (adjoin-set x set)
  (cond ((null? set) (list x))
        ((= x (car set)) set)
        ((< x (car set)) (cons x set))
        (else (cons (car set) (adjoin-set x (cdr set))))))

(adjoin-set 5 (list 2 3))

(define (union-set set1 set2)
  (cond ((null? set1) set2)
        ((null? set2) set1)
        (else (let ((x1 (car set1)) (x2 (car set2)))
                (cond ((= x1 x2)
                       (cons x1
                             (union-set (cdr set1)
                                        (cdr set2))))
                      ((< x1 x2)
                       (cons x1 (union-set (cdr set1) set2)))
                      ((< x2 x1)
                       (cons x2 (union-set set1 (cdr set2)))))))))
(union-set (list 1 3 7) (list 2 5 8))

;; Сеты как бинарные деревья
(define (entry tree) (car tree))
(define (left-branch tree) (cadr tree))
(define (right-branch tree) (caddr tree))
(define (make-tree entry left right)
  (list entry left right))


;; log n
(define (element-of-set? x set)
  (cond ((null? set) false)
        ((= x (entry set)) true)
        ((< x (entry set))
         (element-of-set? x (left-branch set)))
        ((> x (entry set))
         (element-of-set? x (right-branch set)))))

;; тоже log n
(define (adjoin-set x set)
  (cond ((null? set) (make-tree x '() '()))
        ((= x (entry set)) set)
        ((< x (entry set))
         (make-tree (entry set) 
                    (adjoin-set x (left-branch set))
                    (right-branch set)))
        ((> x (entry set))
         (make-tree (entry set)
                    (left-branch set)
                    (adjoin-set x (right-branch set))))))
;; но если будем добавлять с 1 по 7 получим дерево 1->2->3->4->5->6->7-> то есть разбалансированное.

(define (tree->list-1 tree)
  (if (null? tree)
      '()
      (append (tree->list-1 (left-branch tree))
              (cons (entry tree)
                    (tree->list-1 (right-branch tree))))))
(define (tree->list-2 tree)
  (define (copy-to-list tree result-list)
    (if (null? tree)
        result-list
        (copy-to-list (left-branch tree)
                      (cons (entry tree)
                            (copy-to-list (right-branch tree)
                                          result-list)))))
  (copy-to-list tree '()))


(define tree1 (make-tree 7
                         (make-tree 3
                                    (make-tree 1 () ())
                                    (make-tree 5 () ()))
                         (make-tree 9
                                    ()
                                    (make-tree 11 () ()))))

tree1
(tree->list-1 tree1)
(tree->list-2 tree1)

(define tree2 (make-tree 3
                         (make-tree 7
                                    (make-tree 5 () ())
                                    (make-tree 9
                                               ()
                                               (make-tree 11 () ())))
                         (make-tree 1
                                    ()
                                    ())))

tree2
(tree->list-1 tree2)
(tree->list-2 tree2)

(define tree3 (make-tree 5
                         (make-tree 3
                                    (make-tree 1 () ())
                                    ())
                         (make-tree 9
                                    (make-tree 7 () ())
                                    (make-tree 11 () ()))))
tree3
(tree->list-1 tree3)
(tree->list-2 tree3)

;; строим сбалансированное дерево

(quotient 8 2)

(define (list->tree elements)
  (car (partial-tree elements (length elements))))

(define (partial-tree elts n)
  (if (= n 0)
      (cons '() elts)
      (let ((left-size (quotient (- n 1) 2)))
        (let ((left-result (partial-tree elts left-size)))
          (let ((left-tree (car left-result))
                (non-left-elts (cdr left-result))
                (right-size (- n (+ left-size 1))))
            (let ((this-entry (car non-left-elts))
                  (right-result (partial-tree (cdr non-left-elts)
                                              right-size)))
              (let ((right-tree (car right-result))
                    (remaining-elts (cdr right-result)))
                (cons (make-tree this-entry left-tree right-tree)
                      remaining-elts))))))))

(partial-tree (list 1 3) 2)

;; (1 3 5 7 9 11)
;; car (partial-tree (1 3 5 7 9 11) 6)
;; left-size = 2
;; left-result (partial-tree (1 3 5 7 9 11) 2)
;; left-tree (car (partial-tree (1 3 5 7 9 11) 2))
;; non-left-elts ()

;; Ex. 2.65
c
;; (define (entry tree) (car tree))
;; (define (left-branch tree) (cadr tree))
;; (define (right-branch tree) (caddr tree))

(define (union-tree-set set1 set2)
  (let ((orderedlist1 (tree->list-2 set1)) (orderedlist2 (tree->list-2 set2)))
    (define (union-set set1 set2)
      (cond ((null? set1) set2)
            ((null? set2) set1)
            (else (let ((x1 (car set1)) (x2 (car set2)))
                    (cond ((= x1 x2)
                           (cons x1
                                 (union-set (cdr set1)
                                            (cdr set2))))
                          ((< x1 x2)
                           (cons x1 (union-set (cdr set1) set2)))
                          ((< x2 x1)
                           (cons x2 (union-set set1 (cdr set2)))))))))

    (list->tree (union-set orderedlist1 orderedlist2))))
(tree->list-2 (list->tree (list 1 3 4 5 9 15)))
(union-tree-set (list->tree (list 1 3 4 5 9 15))
                (list->tree (list 2 3 6 9 15)))

(define (intersection-tree-set set1 set2)
  (let ((orderedlist1 (tree->list-2 set1)) (orderedlist2 (tree->list-2 set2)))
    (define (intersection-set set1 set2)
      (if (or (null? set1) (null? set2))
          '()
          (let ((x1 (car set1)) (x2 (car set2)))
            (cond ((= x1 x2)
                   (cons x1
                         (intersection-set (cdr set1)
                                           (cdr set2))))
                  ((< x1 x2)
                   (intersection-set (cdr set1) set2))
                  ((< x2 x1)
                   (intersection-set set1 (cdr set2)))))))
    (list->tree (intersection-set orderedlist1 orderedlist2))))

(intersection-tree-set (list->tree (list 1 3 4 5 9 15))
                       (list->tree (list 2 3 6 9 15)))


;; Ex 2.66
;; (define (entry tree) (car tree))
;; (define (left-branch tree) (cadr tree))
;; (define (right-branch tree) (caddr tree))
(define (lookup given-key tree)
  (cond ((null? tree) false)
        ((equal? given-key (entry tree)) (entry tree))
        ((> given-key (entry tree)) (lookup given-key (right-branch tree)))
        ((< given-key (entry tree)) (lookup given-key (left-branch tree)))))

(list->tree (list 1 3 4 5 9 15))

(lookup 5 (list->tree (list 1 3 4 5 9 15)))

;; Кодирование Хафмана

(define (make-leaf symbol weight)
  (list 'leaf symbol weight))
(define (leaf? object)
  (eq? (car object) 'leaf))
(define (symbol-leaf x) (cadr x))
(define (weight-leaf x) (caddr x))

(define (make-code-tree left right)
  (list left
        right
        (append (symbols left) (symbols right))
        (+ (weight left) (weight right))))

(define (left-branch tree) (car tree))

(define (right-branch tree) (cadr tree))
(define (symbols tree)
  (if (leaf? tree)
      (list (symbol-leaf tree))
      (caddr tree)))
(define (weight tree)
  (if (leaf? tree)
      (weight-leaf tree)
      (cadddr tree)))


(define (decode bits tree)
  (define (decode-1 bits current-branch)
    (if (null? bits)
        '()
        (let ((next-branch
               (choose-branch (car bits) current-branch)))
          (if (leaf? next-branch)
              (cons (symbol-leaf next-branch)
                    (decode-1 (cdr bits) tree))
              (decode-1 (cdr bits) next-branch)))))
  (decode-1 bits tree))
(define (choose-branch bit branch)
  (cond ((= bit 0) (left-branch branch))
        ((= bit 1) (right-branch branch))
        (else (error "bad bit -- CHOOSE-BRANCH" bit))))

(define (adjoin-set x set)
  (cond ((null? set) (list x))
        ((< (weight x) (weight (car set))) (cons x set))
        (else (cons (car set)
                    (adjoin-set x (cdr set))))))

(define (make-leaf-set pairs)
  (if (null? pairs)
      '()
      (let ((pair (car pairs)))
        (display pair)
        (newline)
        (adjoin-set (make-leaf (car pair)    ; symbol
                               (cadr pair))  ; frequency
                    (make-leaf-set (cdr pairs))))))

(make-leaf 'A 1)
(make-leaf-set (list (list 'A 4) (list 'B 2) (list 'C 1) (list 'D 1)))
(list (list 'A 4) (list 'B 2) (list 'C 1) (list 'D 1))


;; Ex. 2.67

(define sample-tree
  (make-code-tree (make-leaf 'A 4)
                  (make-code-tree
                   (make-leaf 'B 2)
                   (make-code-tree (make-leaf 'D 1)
                                   (make-leaf 'C 1)))))

(define sample-message '(0 1 1 0 0 1 0 1 0 1 1 1 0))
(decode sample-message sample-tree)


(define (in-list? symbol symbols)
  (if (null? symbols)
      false
      (if (equal? symbol (car symbols))
          true
          (in-list? symbol (cdr symbols)))))
(in-list? 'A '(b c a))

(define (encode-symbol symbol tree)
  (let ((l-branch (left-branch tree))
        (r-branch (right-branch tree)))
    (cond ((in-list? symbol (symbols l-branch)) (if (leaf? l-branch)
                                                    (cons '0 ())
                                                    (cons '0 (encode-symbol symbol l-branch))))
          ((in-list? symbol (symbols r-branch)) (if (leaf? r-branch)
                                                    (cons '1 ())
                                                    (cons '1 (encode-symbol symbol r-branch))))
          (else
           (error "not exist symbol -- ENCODE-SYMBOL" symbol)))))

(define (encode message tree)
  (if (null? message)
      '()
      (append (encode-symbol (car message) tree)
              (encode (cdr message) tree))))

(encode '(a d a b b c a) sample-tree) ;Value: (0 1 1 0 0 1 0 1 0 1 1 1 0)
(in-list? 'b (symbols (right-branch sample-tree)))
(leaf? (left-branch sample-tree))
(encode-symbol 'b sample-tree)
(cons '0 (cons '1 ()))


;; Ex. 2.69
;; вставка отсортированная по весу
(define (adjoin-set x set)
  (cond ((null? set) (list x))
        ((< (weight x) (weight (car set))) (cons x set))
        (else (cons (car set)
                    (adjoin-set x (cdr set))))))
;; получаем сортированный список листов.
(define (make-leaf-set pairs)
  (if (null? pairs)
      '()
      (let ((pair (car pairs)))
        (adjoin-set (make-leaf (car pair)    ; symbol
                               (cadr pair))  ; frequency
                    (make-leaf-set (cdr pairs))))))

(make-leaf-set (list (list 'A 4) (list 'B 2) (list 'C 1) (list 'D 1)))
;;Value: ((leaf d 1) (leaf c 1) (leaf b 2) (leaf a 4))

(define (generate-huffman-tree pairs)
  (successive-merge (make-leaf-set pairs)))

(define (successive-merge opairs)
  (if (< (length opairs) 2)
      (car opairs)
      (let ((merged (make-code-tree (car opairs) (cadr opairs))))
        (display merged)
        (newline)
        (successive-merge (adjoin-set merged (cdr (cdr opairs)))))))

(define os (make-leaf-set (list (list 'A 4) (list 'B 2) (list 'C 1) (list 'D 1))))
os
(make-code-tree (list 'leaf 'd 1) (list 'leaf 'c 1))
(symbols (make-code-tree (list 'leaf 'd 1) (list 'leaf 'c 1)))
(weight (make-code-tree (list 'leaf 'd 1) (list 'leaf 'c 1)))
(adjoin-set (make-code-tree (list 'leaf 'd 1) (list 'leaf 'c 1)) (cdr (cdr os)))

(define gentree (generate-huffman-tree (list (list 'A 4) (list 'B 2) (list 'C 1) (list 'D 1))))
gentree
(symbols gentree)

(encode '(a d a b b c a) gentree)

;; Ex. 2.70

(define pairs (list (list 'A 2) (list 'BOOM 1) (list 'GET 2) (list 'JOB 2) (list 'NA 16) (list 'SHA 3) (list 'YIP 9) (list 'WAH 1)))

(define lyric-tree (generate-huffman-tree pairs))
(define song '(Get a job Sha na na na na na na na na Get a job Sha na na na na na na na na Wah yip yip yip yip yip yip yip yip yip Sha boom))
song

(length (encode song lyric-tree))
(* 3 (length song))

;; Ex. 2.71

;; n = 5
;; 1,2,4,8,16
;; (1,2, 3)
;; (1,2,4 7)
;; (1,2,4,8 15)
;; (1,2,4,8,16 31)
(define pairs (list (list 'A 1) (list 'B 2) (list 'C 4) (list 'D 8) (list 'E 16)))
(define bin-tree (generate-huffman-tree pairs))
(encode '(A) bin-tree)


;; (1,2,4,8,16,32 63)

;; n = 10
;; 1,2,4,8,16
;; (1,2, 3)
;; (1,2,4 7)
;; (1,2,4,8 15)
;; (1,2,4,8,16 31)
;; (1,2,4,8,16,32 63)
;; (1,2,4,8,16,32,64 127)
;; (1,2,4,8,16,32,64,128 255)
;; (1,2,4,8,16,32,64,128,256 511)
;; (1,2,4,8,16,32,64,128,256,512 1023)

;; чтобы закодировать самый частый символ 1 бит
;; чтобы закодировать самый редкий символ для n=5, 

;; 1,2,4,8,16,32,64,128,256,512
(define pairs (list (list 'A 1) (list 'B 2) (list 'C 4) (list 'D 8) (list 'E 16) (list 'F 32) (list 'G 64) (list 'H 128) (list 'I 256) (list 'J 512)))
(define bin-tree (generate-huffman-tree pairs))
bin-tree
(encode '(A) bin-tree)

;; получается n-1 бит


;; Глава 2.4 речь про две системы представления комплексных чисел и общих операций над ними.


(define (add-complex c1 c2)
  )
(define (sub-complex c1 c2)
  )
(define (mul-complex c1 c2)
  )
(define (div-complex c1 c2)
  )

(make-from-real-imag (real-part z) (imag-part z))
(make-from-mag-ang (magnitude z) (angle z))

(define (add-complex z1 z2)
  (make-from-real-imag (+ (real-part z1) (real-part z2))
                       (+ (imag-part z1) (imag-part z2))))

(define (sub-complex z1 z2)
  (make-from-real-imag (- (real-part z1) (real-part z2))
                       (- (imag-part z1) (imag-part z2))))

(define (mul-complex z1 z2)
  (make-from-mag-ang (* (magnitude z1) (magnitude z2))
                     (+ (angle z1) (angle z2))))

(define (div-complex z1 z2)
  (make-from-mag-ang (/ (magnitude z1) (magnitude z2))
                     (- (angle z1) (angle z2))))

;; представим как пару реальная мнимая часть
(define (real-part z) (car z))
(define (imag-part z) (cdr z))
(define (magnitude z)
  (sqrt (+ (square (real-part z)) (square (imag-part z)))))
(define (angle z)
  (atan (imag-part z) (real-part z)))
(define (make-from-real-imag x y) (cons x y))
(define (make-from-mag-ang r a) 
  (cons (* r (cos a)) (* r (sin a))))

;; представим в полярных координатах храним магнитуду и угол.
(define (real-part z)
  (* (magnitude z) (cos (angle z))))
(define (imag-part z)
  (* (magnitude z) (sin (angle z))))
(define (magnitude z) (car z))
(define (angle z) (cdr z))
(define (make-from-real-imag x y) 
  (cons (sqrt (+ (square x) (square y)))
        (atan y x)))
(define (make-from-mag-ang r a) (cons r a))

;; ``principle of least commitment.'' принцип наименьшего преступления

;; решили использовать оба типа, цепляем теги
(define (attach-tag type-tag contents)
  (cons type-tag contents))
(define (type-tag datum)
  (if (pair? datum)
      (car datum)
      (error "Bad tagged datum -- TYPE-TAG" datum)))
(define (contents datum)
  (if (pair? datum)
      (cdr datum)
      (error "Bad tagged datum -- CONTENTS" datum)))

;; добавляем проверку типов
(define (rectangular? z)
  (eq? (type-tag z) 'rectangular))
(define (polar? z)
  (eq? (type-tag z) 'polar))

(define (real-part-rectangular z) (car z))
(define (imag-part-rectangular z) (cdr z))
(define (magnitude-rectangular z)
  (sqrt (+ (square (real-part-rectangular z))
           (square (imag-part-rectangular z)))))
(define (angle-rectangular z)
  (atan (imag-part-rectangular z)
        (real-part-rectangular z)))
(define (make-from-real-imag-rectangular x y)
  (attach-tag 'rectangular (cons x y)))
(define (make-from-mag-ang-rectangular r a) 
  (attach-tag 'rectangular
              (cons (* r (cos a)) (* r (sin a)))))

(define (real-part-polar z)
  (* (magnitude-polar z) (cos (angle-polar z))))
(define (imag-part-polar z)
  (* (magnitude-polar z) (sin (angle-polar z))))
(define (magnitude-polar z) (car z))
(define (angle-polar z) (cdr z))
(define (make-from-real-imag-polar x y) 
  (attach-tag 'polar
               (cons (sqrt (+ (square x) (square y)))
                     (atan y x))))
(define (make-from-mag-ang-polar r a)
  (attach-tag 'polar (cons r a)))


;; generic selectors
(define (real-part z)
  (cond ((rectangular? z) 
         (real-part-rectangular (contents z)))
        ((polar? z)
         (real-part-polar (contents z)))
        (else (error "Unknown type -- REAL-PART" z))))
(define (imag-part z)
  (cond ((rectangular? z)
         (imag-part-rectangular (contents z)))
        ((polar? z)
         (imag-part-polar (contents z)))
        (else (error "Unknown type -- IMAG-PART" z))))
(define (magnitude z)
  (cond ((rectangular? z)
         (magnitude-rectangular (contents z)))
        ((polar? z)
         (magnitude-polar (contents z)))
        (else (error "Unknown type -- MAGNITUDE" z))))
(define (angle z)
  (cond ((rectangular? z)
         (angle-rectangular (contents z)))
        ((polar? z)
         (angle-polar (contents z)))
        (else (error "Unknown type -- ANGLE" z))))

(define (add-complex z1 z2)
  (make-from-real-imag (+ (real-part z1) (real-part z2))
                       (+ (imag-part z1) (imag-part z2))))

(define (make-from-real-imag x y)
  (make-from-real-imag-rectangular x y))
(define (make-from-mag-ang r a)
  (make-from-mag-ang-polar r a))

;; дата направленное программирование

;; если у нас много типов представлений то с добавлением еще одного нового типа могут возникнуть сложности


;; не нужно беспокоиться и конфликте имен для разных представлений
(define (install-rectangular-package)
  ;; internal procedures
  (define (real-part z) (car z))
  (define (imag-part z) (cdr z))
  (define (make-from-real-imag x y) (cons x y))
  (define (magnitude z)
    (sqrt (+ (square (real-part z))
             (square (imag-part z)))))
  (define (angle z)
    (atan (imag-part z) (real-part z)))
  (define (make-from-mag-ang r a) 
    (cons (* r (cos a)) (* r (sin a))))
  ;; interface to the rest of the system
  (define (tag x) (attach-tag 'rectangular x))
  (put 'real-part '(rectangular) real-part)
  (put 'imag-part '(rectangular) imag-part)
  (put 'magnitude '(rectangular) magnitude)
  (put 'angle '(rectangular) angle)
  (put 'make-from-real-imag 'rectangular 
       (lambda (x y) (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang 'rectangular 
       (lambda (r a) (tag (make-from-mag-ang r a))))
  'done)

(define (install-polar-package)
  ;; internal procedures
  (define (magnitude z) (car z))
  (define (angle z) (cdr z))
  (define (make-from-mag-ang r a) (cons r a))
  (define (real-part z)
    (* (magnitude z) (cos (angle z))))
  (define (imag-part z)
    (* (magnitude z) (sin (angle z))))
  (define (make-from-real-imag x y) 
    (cons (sqrt (+ (square x) (square y)))
          (atan y x)))
  ;; interface to the rest of the system
  (define (tag x) (attach-tag 'polar x))
  (put 'real-part '(polar) real-part)
  (put 'imag-part '(polar) imag-part)
  (put 'magnitude '(polar) magnitude)
  (put 'angle '(polar) angle)
  (put 'make-from-real-imag 'polar
       (lambda (x y) (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang 'polar 
       (lambda (r a) (tag (make-from-mag-ang r a))))
  'done)


(define (apply-generic op . args)
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc
          (apply proc (map contents args))
          (error
            "No method for these types -- APPLY-GENERIC"
            (list op type-tags))))))

(define (real-part z) (apply-generic 'real-part z))
(define (imag-part z) (apply-generic 'imag-part z))
(define (magnitude z) (apply-generic 'magnitude z))
(define (angle z) (apply-generic 'angle z))

(define (make-from-real-imag x y)
  ((get 'make-from-real-imag 'rectangular) x y))
(define (make-from-mag-ang r a)
  ((get 'make-from-mag-ang 'polar) r a))

;; Ex. 2.73

;; https://stackoverflow.com/questions/5499005/how-do-i-get-the-functions-put-and-get-in-sicp-scheme-exercise-2-78-and-on

(define get 2d-get)
(define put 2d-put!)

(define (deriv exp var)
   (cond ((number? exp) 0)
         ((variable? exp) (if (same-variable? exp var) 1 0))
         (else ((get 'deriv (operator exp)) (operands exp)
                var))))
(define (operator exp) (car exp))
(define (operands exp) (cdr exp))

;; для произведения
(define (install-deriv-mul-package)
  ;; internal procedures
  (define (multiplier opds) (car opds))
  (define (multiplicand opds) (cadr opds))
  (define (make-sum a1 a2) (list '+ a1 a2))
  (define (make-product m1 m2) (list '* m1 m2))
  (define (deriv-mul operands var)
    (make-sum
     (make-product (multiplier operands)
                   (deriv (multiplicand operands) var))
     (make-product (deriv (multiplier operands) var)
                   (multiplicand operands))))
  ;; interface to the rest of the system
  (put '* 'deriv deriv-mul)
  'done)
(install-deriv-mul-package)
;; для суммы
(define (install-deriv-sum-package)
  ;; internal procedures
  (define (addend opds) (car opds))
  (define (augend opds) (cadr opds))
  (define (make-sum a1 a2) (list '+ a1 a2))
  (define (deriv-sum opds var)
    (make-sum (deriv (addend opds) var) (deriv (augend opds) var)))
  ;; interface to the rest of the system
  (put '+ 'deriv  deriv-sum)
  'done)
(install-deriv-sum-package)

;; для экспоненты
(define (install-deriv-exp-package)
  ;; internal procedures
  (define (base opds) (car opds))
  (define (exponent opds) (cadr opds))
  (define (make-sum a1 a2) (list '+ a1 a2))
  (define (make-product m1 m2) (list '* m1 m2))
  (define (make-exponentiation b1 e2)
    (cond ((=number? e2 0) 1)
          ((=number? e2 1) b1)
          ((=number? b1 0) 1)
          ((and (number? b1) (number? e2)) (expt b1 e2))
          (else (list '** b1 e2))))
  (define (deriv-exp opds var)
    (make-product (make-product (exponent opds)
                                (make-exponentiation (base opds)
                                                     (make-sum (exponent opds) -1)))
                  (deriv (base opds) var)))
  ;; interface to the rest of the system
  (put '** 'deriv deriv-exp)
  'done)

(install-deriv-exp-package)
(deriv '(** (+ x 3) 5) 'x)

;; изменили порядок вызова

(define (deriv exp var)
   (cond ((number? exp) 0)
         ((variable? exp) (if (same-variable? exp var) 1 0))
         (else ((get (operator exp) 'deriv) (operands exp) var))))

(deriv '(* x (+ x 3)) 'x)

(define (test a b)
  (display a)
  (newline)
  (display b)
  (newline))
(put 'deriv '+ test)
(get 'deriv '+)
(get 'deriv '*)
((get 'deriv '+) '(1 2) 'x)
(car '(x 1))
;; (define (deriv z) (apply-generic 'real-part z))


;; Ex. 2.74

(define (install-department-1-package)
  (define TYPE 'department1)
  (define allemployees (list '(Каблуков 100.0 "Достоевского 5, кв. 20" 1)
                             '(Коровин 70.0 "Луговая 2" 2)
                             '(Тихонов 75.0 "Лунина 13, кв. 156" 3)))
  (define (last-name record)
    (car record))
  (define (salary record)
    (cadr record))
  (define (address record)
    (caddr record))
  (define (id record)
    (cadddr record))
  (define (get-record employee-id)
    (define (iter-search pk records)
      (if (null? records)
          false
          (if (= pk (id (car records)))
              (car records)
              (iter-search pk (cdr records)))))
    (iter-search employee-id allemployees))
  (define (search-by-name employee-name)
    (define (iter-search name records)
      (if (null? records)
          false
          (if (equal? name (last-name (car records)))
              (car records)
              (iter-search name (cdr records)))))
    (iter-search employee-name allemployees))
  ;; interface
  (define (tag x) (attach-tag TYPE x))
  (put TYPE 'get-record
       (lambda (x) (tag (get-record x))))
  (put TYPE 'get-salary
       (lambda (rec) (tag (salary rec))))
  (put TYPE 'find-employee-record
       (lambda (x) (tag (search-by-name x))))
  TYPE)

(define dp1 (install-department-1-package))

(define (install-department-2-package)
  (define TYPE 'department2)
  (define allemployees (list (list 4 (list 'Щавелев 105.0 "Пушкина 5, кв. 20"))
                             (list 5 (list 'Коровин 71.0 "Пушкина 2"))
                             (list 6 (list 'Куравлев 74.0 "Лунина 13, кв. 156"))))
  (define (last-name record)
    (car (cadr record)))
  (define (salary record)
    (cadr (cadr record)))
  (define (address record)
    (caddr (cadr record)))
  (define (id record)
    (car record))
  (define (get-record employee-id)
    (define (iter-search pk records)
      (if (null? records)
          false
          (if (= pk (id (car records)))
              (car records)
              (iter-search pk (cdr records)))))
    (iter-search employee-id allemployees))
  (define (search-by-name employee-name)
    (define (iter-search name employees)
      (if (null? employees)
          false
          (if (equal? name (last-name (car employees)))
              (car employees)
              (iter-search name (cdr employees)))))
    (iter-search employee-name allemployees))
  ;; interface
  (define (tag x) (attach-tag TYPE x))
  (put TYPE 'get-record
       (lambda (x) (tag (get-record x))))
  (put TYPE 'get-salary
       (lambda (rec) (tag (salary rec))))
  (put TYPE 'find-employee-record
       (lambda (x) (tag (search-by-name x))))
  TYPE)
(define dp2 (install-department-2-package))

(define (get-record employee-id department-id)
  ((get department-id 'get-record) employee-id))

(define (apply-generic op . args)
  (let ((type-tags (map type-tag args)))
    (let ((proc (get (car type-tags) op)))
      (if proc
          (apply proc (map contents args))
          (error
            "No method for these types -- APPLY-GENERIC"
            (list type-tags op))))))

(define (get-salary record)
  (apply-generic 'get-salary record))
(define (find-employee-record employee-name deps)
  (define (iter-department-search name deps res)
    (when (not (null? deps))
        (let ((record ((get (car deps) 'find-employee-record) name)))
          (display record)
          (if (eq? record false)
              (iter-department-search name (cdr deps) res)
              (iter-department-search name (cdr deps) (cons record res)))))
    res)
  (iter-department-search employee-name deps '()))
dp1
dp2
(equal? 'Li 'Li)
(get dp2 'find-employee-record)
((get dp2 'find-employee-record) 'Li)
(find-employee-record 'Коровин (list dp1 dp2))
(define rec1 (get-record 1 dp1))
rec1
(get-salary rec1)
(define rec5 (get-record 5 dp2))
rec5
(get-salary rec5)

(define record1 (get-salary (get-record 1 'dp1)))


;; Message passing

;; проход сообщения
;; Ограничение такого подхода, разрешение общих процедур одного аргумента.

(define (make-from-real-imag x y)
  (define (dispatch op)
    (cond ((eq? op 'real-part) x)
          ((eq? op 'imag-part) y)
          ((eq? op 'magnitude)
           (sqrt (+ (square x) (square y))))
          ((eq? op 'angle) (atan y x))
          (else
           (error "Unknown op -- MAKE-FROM-REAL-IMAG" op))))
  dispatch)

(define (apply-generic op arg) (arg op))

(define (make-from-mag-ang m a)
  (define (dispatch op)
    (cond ((eq? op 'real-part) (* m (cos a)))
          ((eq? op 'imag-part) (* m (sin a)))
          ((eq? op 'magnitude) m)
          ((eq? op 'angle) a)
          (else
           (error "Unknown op -- MAKE-FROM-MAG-ANG" op))))
  dispatch)


(define (real-part z) (apply-generic 'real-part z))
(define (imag-part z) (apply-generic 'imag-part z))
(define (magnitude z) (apply-generic 'magnitude z))
(define (angle z) (apply-generic 'angle z))

(make-from-real-imag 1 1)
(define (add-complex z1 z2)
  (make-from-real-imag (+ (real-part z1) (real-part z2))
                       (+ (imag-part z1) (imag-part z2))))
(add-complex (make-from-real-imag 1 1) (make-from-real-imag 2 2))


;; 2.5.1 Общие арифметические операции
(define *op-table* (make-hash-table))

(define (put op type proc)
  (hash-table/put! *op-table* (list op type) proc))

(define (get op type)
  (hash-table/get *op-table* (list op type) #f))


(define (apply-generic op . args)
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc
          (apply proc (map contents args))
          (error
            "No method for these types -- APPLY-GENERIC"
            (list op type-tags))))))

;; общие операции
(define (add x y) (apply-generic 'add x y))
(define (sub x y) (apply-generic 'sub x y))
(define (mul x y) (apply-generic 'mul x y))
(define (div x y) (apply-generic 'div x y))

;; пакет работы с обычными числами
(define (install-scheme-number-package)
  (define (tag x)
    (attach-tag 'scheme-number x))    
  (put 'add '(scheme-number scheme-number)
       (lambda (x y) (tag (+ x y))))
  (put 'sub '(scheme-number scheme-number)
       (lambda (x y) (- x y)))
  (put 'mul '(scheme-number scheme-number)
       (lambda (x y) (* x y)))
  (put 'div '(scheme-number scheme-number)
       (lambda (x y) (/ x y)))
  'done)
(install-scheme-number-package) ;; установка пакета

;; конструктор для обычных чисел
(define (make-scheme-number n)
  ((get 'make 'scheme-number) n))

(add (make-scheme-number 1) (make-scheme-number 2))


;; пакет - класс для работы с рациональными цислами

(define (install-rational-package)
  ;; internal procedures
  (define (numer x) (car x))
  (define (denom x) (cdr x))
  (define (make-rat n d)
    (let ((g (gcd n d)))
      (cons (/ n g) (/ d g))))
  (define (add-rat x y)
    (make-rat (+ (* (numer x) (denom y))
                 (* (numer y) (denom x)))
              (* (denom x) (denom y))))
  (define (sub-rat x y)
    (make-rat (- (* (numer x) (denom y))
                 (* (numer y) (denom x)))
              (* (denom x) (denom y))))
  (define (mul-rat x y)
    (make-rat (* (numer x) (numer y))
              (* (denom x) (denom y))))
  (define (div-rat x y)
    (make-rat (* (numer x) (denom y))
              (* (denom x) (numer y))))
  ;; interface to rest of the system
  (define (tag x) (attach-tag 'rational x))
  (define (real-tag x) (attach-tag 'scheme-real x))
  (put 'gcos '(rational)
       (lambda (x) '()
  
  (put 'add '(rational rational)
       (lambda (x y) (tag (add-rat x y))))
  (put 'sub '(rational rational)
       (lambda (x y) (tag (sub-rat x y))))
  (put 'mul '(rational rational)
       (lambda (x y) (tag (mul-rat x y))))
  (put 'div '(rational rational)
       (lambda (x y) (tag (div-rat x y))))

  (put 'make 'rational
       (lambda (n d) (tag (make-rat n d))))
  (put 'equ? '(rational rational)
       (lambda (x y) (or
                 (and (= (numer x) (numer y)) (= (denom x) (denom y)))
                 (and (= (numer x) (numer y)) (= (numer x) 0)))))
  (put '=zero? '(rational)
       (lambda (x) (= (numer x) 0)))
  'done)
(install-rational-package)
(define (make-rational n d)
  ((get 'make 'rational) n d))

;; пакет для комплексных чисел

(install-rectangular-package)
(install-polar-package)

(define (install-complex-package)
  ;; imported procedures from rectangular and polar packages
  (define (make-from-real-imag x y)
    ((get 'make-from-real-imag 'rectangular) x y))
  (define (make-from-mag-ang r a)
    ((get 'make-from-mag-ang 'polar) r a))
  ;; internal procedures
  (define (add-complex z1 z2)
    (make-from-real-imag (+ (real-part z1) (real-part z2))
                         (+ (imag-part z1) (imag-part z2))))
  (define (sub-complex z1 z2)
    (make-from-real-imag (- (real-part z1) (real-part z2))
                         (- (imag-part z1) (imag-part z2))))
  (define (mul-complex z1 z2)
    (make-from-mag-ang (* (magnitude z1) (magnitude z2))
                       (+ (angle z1) (angle z2))))
  (define (div-complex z1 z2)
    (make-from-mag-ang (/ (magnitude z1) (magnitude z2))
                       (- (angle z1) (angle z2))))
  ;; interface to rest of the system
  (define (tag z) (attach-tag 'complex z))
  (put 'add '(complex complex)
       (lambda (z1 z2) (tag (add-complex z1 z2))))
  (put 'sub '(complex complex)
       (lambda (z1 z2) (tag (sub-complex z1 z2))))
  (put 'mul '(complex complex)
       (lambda (z1 z2) (tag (mul-complex z1 z2))))
  (put 'div '(complex complex)
       (lambda (z1 z2) (tag (div-complex z1 z2))))
  (put 'make-from-real-imag 'complex
       (lambda (x y) (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang 'complex
       (lambda (r a) (tag (make-from-mag-ang r a))))
  (put 'real-part '(complex) real-part)
  (put 'imag-part '(complex) imag-part)
  (put 'magnitude '(complex) magnitude)
  (put 'angle '(complex) angle)
  (put 'equ? '(complex complex)
       (lambda (x y) (and (= (real-part x) (real-part y)) (= (imag-part x) (imag-part y)))))
  (put '=zero? '(complex)
       (lambda (x) (= (magnitude x) 0)))
  'done)
(install-complex-package)

(define (make-complex-from-real-imag x y)
  ((get 'make-from-real-imag 'complex) x y))
(define (make-complex-from-mag-ang r a)
  ((get 'make-from-mag-ang 'complex) r a))
(real-part (make-complex-from-real-imag 1 2))
(add (make-complex-from-mag-ang 10 0.5) (make-complex-from-real-imag 1 2))

;; Тут мы имеем двух уровневую чичтему тегов (классов) то есть мы работаем с комплексными числами, но при выполнении операций над числами при извлечении магнитуды или другого свойства опираемся на вложенный тип.

;; не работает почему? обращаемся к первому тегу 'complex и в нем нет интерфейса для данного метода
(trace (magnitude (make-complex-from-mag-ang 10 0.5)))


;; Ex 2.78

(define (install-scheme-number-package)
  (put 'add '(scheme-number scheme-number)
       (lambda (x y) (+ x y)))
  (put 'sub '(scheme-number scheme-number)
       (lambda (x y) (- x y)))
  (put 'mul '(scheme-number scheme-number)
       (lambda (x y) (* x y)))
  (put 'div '(scheme-number scheme-number)
       (lambda (x y) (/ x y)))
  (put 'equ? '(scheme-number scheme-number)
       (lambda (x y) (= x y)))
  (put '=zero? '(scheme-number)
       (lambda (x) (= x 0)))
  (put 'make 'scheme-number
       (lambda (x) (tag x)))
  ;; following added to Scheme-number package
  (define (tag x) (attach-tag 'scheme-number x))
  (put 'exp '(scheme-number scheme-number)
       (lambda (x y) (tag (expt x y)))) ; using primitive expt
  'done)
(install-scheme-number-package) ;; установка пакета

(define (attach-tag type-tag contents)
  (if (number? contents)
      (cons 'scheme-number contents)
      (cons type-tag contents)))
(define (type-tag datum)
  (cond ((number? datum) 'scheme-number)
        ((pair? datum) (car datum))
        (else (error "Bad tagged datum -- TYPE-TAG" datum))))
(define (contents datum)
  (cond ((number? datum) datum)
        ((pair? datum) (cdr datum))
        (else (error "Bad tagged datum -- CONTENTS" datum))))


(add 1 2)

;; Ex 2.79

(define (equ? x y) (apply-generic 'equ? x y))
(define (=zero? x) (apply-generic '=zero? x))
(=zero? (make-number 1))
(=zero? (make-number 0))
(equ? 1 2)
(equ? 1 1)
(=zero? (make-complex-from-mag-ang 10 1))
(equ? (make-complex-from-mag-ang 0 1) (make-complex-from-real-imag 0 0))
(equ? (make-complex-from-real-imag 1 1) (make-complex-from-real-imag 1 1))
(equ? (make-rational 2 6) (make-rational 1 3))
(=zero? (make-rational 0 6))

;; Общие операции с различными типами данных
;; важно не нарушать барьер абстракции, поэтому для каждого типа операции нужно в пакетах предусмотреть методы взаимодействия разных типов

;; это работает но выглядит грамоздко
;; цена введения нового типа очень высока из-за большого числа сочетаний разных типов, плюс к этому не получится разрабатывать пакеты отдельно от других, теряется важное свойство примесности.
;; to be included in the complex package
(define (add-complex-to-schemenum z x)
  (make-from-real-imag (+ (real-part z) x)
                       (imag-part z)))
(put 'add '(complex scheme-number)
     (lambda (z x) (tag (add-complex-to-schemenum z x))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; приведение типов
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; когда работаем с разными типами, чаще всего мы можем сделать преобразование от одного типа к другому чтобы выполнить операцию над разными типами, такой процесс называется coercion (принудительное преобразование типов)

(define (scheme-number->complex n)
  (make-complex-from-real-imag (contents n) 0))

;; отдельная таблица для приведения типов

(define *op-coercion-table* (make-hash-table))

(define (put-coercion type1 type2 proc)
  (hash-table/put! *op-coercion-table* (list type1 type2) proc))

(define (get-coercion type1 type2)
  (hash-table/get *op-coercion-table* (list type1 type2) #f))

(put-coercion 'scheme-number 'complex scheme-number->complex)
;; (get-coercion 'scheme-number 'complex)


(define (apply-generic op . args)
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc
          (apply proc (map contents args))
          (if (= (length args) 2)
              (let ((type1 (car type-tags))
                    (type2 (cadr type-tags))
                    (a1 (car args))
                    (a2 (cadr args)))
                (if (equal? type1 type2)
                    (error "No method for these types"
                           (list op type-tags))
                    (let ((t1->t2 (get-coercion type1 type2))
                          (t2->t1 (get-coercion type2 type1)))
                      (cond (t1->t2
                             (apply-generic op (t1->t2 a1) a2))
                            (t2->t1
                             (apply-generic op a1 (t2->t1 a2)))
                            (else
                             (error "No method for these types"
                                    (list op type-tags)))))))
                (error "No method for these types"
                       (list op type-tags)))))))
(add 3 (make-complex-from-real-imag 1 1))


(define (scheme-number->scheme-number n) n)
(define (complex->complex z) z)
(put-coercion 'scheme-number 'scheme-number
              scheme-number->scheme-number)
(put-coercion 'complex 'complex complex->complex)
(add 1 2)
(add (make-complex-from-real-imag 1 1) (make-complex-from-real-imag 1 1))

(define (exp x y) (apply-generic 'exp x y))

;; рекурсивный вызов apply-generic
(exp (make-complex-from-real-imag 1 1) (make-complex-from-real-imag 1 1))
(exp 2 4)


;; Ex. 2.82


(define (map-args args type-tags cdr-type-tags)
  (define (try-coercion args type-list target-type res)
    (if (null? type-list)
        res
        (if (equal? (car type-list) target-type)
            (try-coercion
             (cdr args)
             (cdr type-list)
             target-type
             (cons (car args) res))
            (let ((t2->t1 (get-coercion (car type-list) target-type)))
              (if (not t2->t1)
                  false
                  (try-coercion
                   (cdr args)
                   (cdr type-list)
                   target-type (cons (t2->t1 (car args)) res)))))))
  (cond ((null? cdr-type-tags) (error "No method for these types"))
        (else
         (let ((mapped-args (try-coercion args type-tags (car cdr-type-tags) '())))
           (if (not mapped-args)
               (map-args args type-tags (cdr cdr-type-tags))
               mapped-args)))))


(define (apply-generic op . args)
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if (not proc)
          (let ((mapped-args (map-args args type-tags type-tags)))
            (if (not mapped-args)
                (error "No method for these types" (list op type-tags))
                (let ((mapped-type-tags (map type-tag mapped-args)))
                  (let ((proc (get op mapped-type-tags)))
                    (if (not proc)
                        (error "No method for these types" (list op type-tags))
                        (apply proc (map contents mapped-args)))))))
          (apply proc (map contents args))
          ))))

(get 'add '(complex complex))
(define (add x y) (apply-generic 'add x y))
(make-number 1)
(make-complex-from-real-imag 1 1)
(add (make-number 1) (make-number 1))
(add (make-number 1) (make-complex-from-real-imag 1 1))

;; Ex. 2.83.

;; можно использовать таблицу

;; тут поменял что бы только целые можно было создавать без make

(define (attach-tag type-tag contents)
  (cons type-tag contents))

(define (type-tag datum)
  (cond ((pair? datum) (car datum))
        (else (error "Bad tagged datum -- TYPE-TAG" datum))))
(define (contents datum)
  (cond ((pair? datum) (cdr datum))
        (else (error "Bad tagged datum -- CONTENTS" datum))))

;; добавляем пакет с реальными числами
(define (install-scheme-real-package)
  (define (tag x) (attach-tag 'scheme-real x))
  (put 'add '(scheme-real scheme-real)
       (lambda (x y) (tag (+ x y))))
  (put 'sub '(scheme-real scheme-real)
       (lambda (x y) (tag (- x y))))
  (put 'mul '(scheme-real scheme-real)
       (lambda (x y) (tag (* x y))))
  (put 'div '(scheme-real scheme-real)
       (lambda (x y) (tag (/ x y))))
  (put 'equ? '(scheme-real scheme-real)
       (lambda (x y) (= x y)))
  (put '=zero? '(scheme-real)
       (lambda (x) (= x 0)))
  (put 'make 'scheme-real
       (lambda (x) (tag x)))
  ;; following added to Scheme-number package
  (put 'exp '(scheme-real scheme-real)
       (lambda (x y) (tag (expt x y))))
  'done)
(install-scheme-real-package) ;; установка пакета

(define (install-scheme-number-package)
  (define (tag x) (attach-tag 'scheme-number x))
  (put 'add '(scheme-number scheme-number)
       (lambda (x y) (tag (+ x y))))
  (put 'sub '(scheme-number scheme-number)
       (lambda (x y) (tag (- x y))))
  (put 'mul '(scheme-number scheme-number)
       (lambda (x y) (tag (* x y))))
  (put 'div '(scheme-number scheme-number)
       (lambda (x y) (tag (/ x y))))
  (put 'equ? '(scheme-number scheme-number)
       (lambda (x y) (= x y)))
  (put '=zero? '(scheme-number)
       (lambda (x) (= x 0)))
  (put 'make 'scheme-number
       (lambda (x) (tag x)))
  ;; following added to Scheme-number package
  (put 'exp '(scheme-number scheme-number)
       (lambda (x y) (tag (expt x y)))) ; using primitive expt
  (put 'negation ('scheme-number)
       (lambda (x) (tag (- x))))
  'done)
(install-scheme-number-package) ;; установка пакета
(- 1)
(define x (- 1))
(- x)
(define (install-coercion-package)
  (define (numer x) (car x))
  (define (denom x) (cdr x))
  (define (make-rational n d)
    ((get 'make 'rational) n d))
  (define (make-complex-from-real-imag x y)
    ((get 'make-from-real-imag 'complex) x y))
  (define (make-real x)
    ((get 'make 'scheme-real) x))
  ;; interface
  (put 'raise '(scheme-number)
       (lambda (x) (make-rational x 1)))
  (put 'raise '(rational)
       (lambda (x) (make-real (/ (numer x) (denom x)))))
  (put 'raise '(scheme-real)
       (lambda (x) (make-complex-from-real-imag x 0)))
  (put 'raise '(complex)
       (lambda (x) (error "complex can't raise")))
  ;; project
  (put 'project '(rational)
       (lambda (x) (make-number (round (/ (number x) (denom x))))))
  (put 'project '(scheme-real)
       (lambda (x) (make-number (round x))))
  (put 'project '(complex)
       (lambda (x) (make-real (real-part x))))
  'done)

(install-coercion-package)
(define (raise x)
  (apply-generic 'raise x))
(define (project x)
  (apply-generic 'project x))

(define (drop x)
  (let ((pproc (get 'project (map type-tag (list x)))))
    (if (not pproc)
        x
        (let ((p (project x)))
          (if (equ? (raise p) x)
              p
              x)))))
(define (drop-max x)
  (let ((type-before (type-tag x))
        (new-value (drop x)))
    (if (equal? type-before (type-tag new-value))
        x
        (drop-max new-value))))


(drop-max (make-real 1.))
(drop-max (make-complex-from-real-imag 1 0))

(drop (make-real 1.0))


(raise (raise (raise (make-number 1))))
(raise (make-number 1))
(raise 1)
(define (make-number x)
  ((get 'make 'scheme-number) x))
(define (make-real x)
  ((get 'make 'scheme-real) x))
(add (make-real 1.5) (make-real 2.2))

(add (make-real 1) (make-number 2))


;; Упр 2.84

;; табличка с сравнением типов
(define *op-compare-types-table* (make-hash-table))
(define (put-type-value type value)
  (hash-table/put! *op-compare-types-table* (list type) value))
(define (get-type-value type)
  (hash-table/get *op-compare-types-table* (list type) #f))
(define (install-compare-types-package)
  (put-type-value 'scheme-number 1)
  (put-type-value 'rational 5)
  (put-type-value 'scheme-real 10)
  (put-type-value 'complex 15)
  'done)
(install-compare-types-package)

;; процедура сравнения типов
(define (type1>type2 type1 type2)
  (let ((value1 (get-type-value type1))
        (value2 (get-type-value type2)))
    (> value1 value2)))


(define (map-args args type-tags)
  (define (find-max-value-type types)
    (define (iter-types types value)
      (if (null? types)
          value
          (if (type1>type2 (car types) value)
              (iter-types (cdr types) (car types))
              (iter-types (cdr types) value))))
    (iter-types types (car types)))
  (define (raise-until-type arg target-type)
    (if (equal? (type-tag arg) target-type)
        arg
        (raise-until-type (raise arg) target-type)))
  (define (try-coercion args type-list target-type res)
    (if (null? args)
        res
        (if (equal? (car type-list) target-type)
            (try-coercion (cdr args) (cdr type-list) target-type (cons (car args) res))
            (try-coercion (cdr args) (cdr type-list) target-type (cons (raise-until-type (car args) target-type) res)))))
  (let ((target-type (find-max-value-type type-tags)))
    (let ((mapped-args (try-coercion args type-tags target-type '())))
      (if (not mapped-args)
          (error "No coercion for these types")
          mapped-args))))


(define (apply-generic op . args)
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if (not proc)
          (let ((mapped-args (map-args args type-tags)))
            (if (not mapped-args)
                (error "No method for these types" (list op type-tags))
                (let ((mapped-type-tags (map type-tag mapped-args)))
                  (let ((proc (get op mapped-type-tags)))
                    (if (not proc)
                        (error "No method for these types" (list op type-tags))
                        (apply proc (map contents mapped-args)))))))
          (apply proc (map contents args))
          ))))

;; Ex. 2.85

(define (install-coercion-package)
  (define (numer x) (car x))
  (define (denom x) (cdr x))
  (define (make-rational n d)
    ((get 'make 'rational) n d))
  (define (make-complex-from-real-imag x y)
    ((get 'make-from-real-imag 'complex) x y))
  (define (make-real x)
    ((get 'make 'scheme-real) x))
  (define (make-number x)
    ((get 'make 'scheme-number) x))
  ;; interface
  (put 'raise '(scheme-number)
       (lambda (x) (make-rational x 1)))
  (put 'raise '(rational)
       (lambda (x) (make-real (/ (numer x) (denom x)))))
  (put 'raise '(scheme-real)
       (lambda (x) (make-complex-from-real-imag x (make-number 0))))
  (put 'raise '(complex)
       (lambda (x) (error "complex can't raise")))
  ;; project
  (put 'project '(rational)
       (lambda (x) (make-number (round (/ (numer x) (denom x))))))
  (put 'project '(scheme-real)
       (lambda (x) (make-number (round x))))
  (put 'project '(complex)
       (lambda (x) (make-real (real-part x))))
  'done)

(install-coercion-package)

(define (apply-generic op . args)
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc
          (apply proc (map contents args))
          (error
            "No method for these types -- APPLY-GENERIC"
            (list op type-tags))))))
(define (real-part z)
  (apply-generic 'real-part z))
(define (imag-part z) (apply-generic 'imag-part z))
(define (magnitude z) (apply-generic 'magnitude z))
(define (angle z) (apply-generic 'angle z))
(define (raise x)
  (apply-generic 'raise x))
(define (project x)
  (apply-generic 'project x))


(define (drop x)
  (define (raise-until-type arg target-type)
    (if (equal? (type-tag arg) target-type)
        arg
        (raise-until-type (raise arg) target-type)))

  (let ((pproc (get 'project (map type-tag (list x)))))
    (if (not pproc)
        x
        (let ((p (project x)))
          (if (equ? (raise-until-type p (type-tag x)) x)
              p
              x)))))
(define (drop-max x)
  (let ((type-before (type-tag x))
        (new-value (drop x)))
    (if (equal? type-before (type-tag new-value))
        x
        (drop-max new-value))))


(define (map-args args type-tags)
  (define (find-max-value-type types)
    (define (iter-types types value)
      (if (null? types)
          value
          (if (type1>type2 (car types) value)
              (iter-types (cdr types) (car types))
              (iter-types (cdr types) value))))
    (iter-types types (car types)))
  (define (raise-until-type arg target-type)
    (if (equal? (type-tag arg) target-type)
        arg
        (raise-until-type (raise arg) target-type)))
  (define (try-coercion args type-list target-type res)
    (if (null? args)
        res
        (if (equal? (car type-list) target-type)
            (try-coercion (cdr args) (cdr type-list) target-type (cons (car args) res))
            (try-coercion (cdr args) (cdr type-list) target-type (cons (raise-until-type (car args) target-type) res)))))
  (let ((target-type (find-max-value-type type-tags)))
    (let ((mapped-args (try-coercion args type-tags target-type '())))
      (if (not mapped-args)
          (error "No coercion for these types")
          mapped-args))))


(define (apply-generic-simplified op . args)
  (define (simplified-result res)
    (cond ((boolean? res) res)
          (else (drop-max res))))
  ;; (define (simplified-result res)
  ;;   res)

  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if (not proc)
          (let ((mapped-args (map-args args type-tags)))
            (if (not mapped-args)
                (error "No method for these types" (list op type-tags))
                (let ((mapped-type-tags (map type-tag mapped-args)))
                  (let ((proc (get op mapped-type-tags)))
                    (if (not proc)
                        (error "No method for these types" (list op type-tags))
                        (simplified-result (apply proc (map contents mapped-args))))))))
          (simplified-result (apply proc (map contents args)))
          ))))

(define (add x y) (apply-generic-simplified 'add x y))
(define (sub x y) (apply-generic-simplified 'sub x y))
(define (mul x y) (apply-generic-simplified 'mul x y))
(define (div x y) (apply-generic-simplified 'div x y))
(make-number 1)

(real-part (make-complex-from-real-imag 1 1))
(drop (make-complex-from-real-imag 1 0))
(drop (make-real 1))
(add (make-complex-from-real-imag 1 -1) (make-complex-from-real-imag 1 1))

;; Ex 2.86


(define (install-complex-package)
  ;; imported procedures from rectangular and polar packages
  (define (make-from-real-imag x y)
    ((get 'make-from-real-imag 'rectangular) x y))
  (define (make-from-mag-ang r a)
    ((get 'make-from-mag-ang 'polar) r a))
  ;; internal procedures
  (define (add-complex z1 z2)
    (make-from-real-imag (add (real-part z1) (real-part z2))
                         (add (imag-part z1) (imag-part z2))))
  (define (sub-complex z1 z2)
    (make-from-real-imag (sub (real-part z1) (real-part z2))
                         (sub (imag-part z1) (imag-part z2))))
  (define (mul-complex z1 z2)
    (display (magnitude z1))
    (display (mul (magnitude z1) (magnitude z2)))
    (newline)
    (display (add (angle z1) (angle z2)))
    (newline)
    (display (make-from-mag-ang (mul (magnitude z1) (magnitude z2))
                                (add (angle z1) (angle z2))))
    (make-from-mag-ang (mul (magnitude z1) (magnitude z2))
                       (add (angle z1) (angle z2))))
  (define (div-complex z1 z2)
    (make-from-mag-ang (div (magnitude z1) (magnitude z2))
                       (sub (angle z1) (angle z2))))
  ;; interface to rest of the system
  (define (tag z) (attach-tag 'complex z))
  (put 'add '(complex complex)
       (lambda (z1 z2) (tag (add-complex z1 z2))))
  (put 'sub '(complex complex)
       (lambda (z1 z2) (tag (sub-complex z1 z2))))
  (put 'mul '(complex complex)
       (lambda (z1 z2) (tag (mul-complex z1 z2))))
  (put 'div '(complex complex)
       (lambda (z1 z2) (tag (div-complex z1 z2))))
  (put 'make-from-real-imag 'complex
       (lambda (x y) (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang 'complex
       (lambda (r a) (tag (make-from-mag-ang r a))))
  (put 'real-part '(complex) real-part)
  (put 'imag-part '(complex) imag-part)
  (put 'magnitude '(complex) magnitude)
  (put 'angle '(complex) angle)
  (put 'equ? '(complex complex)
       (lambda (x y) (and (equ? (real-part x) (real-part y)) (equ? (imag-part x) (imag-part y)))))
  (put '=zero? '(complex)
       (lambda (x) (=zero? (magnitude x))))
  'done)
(install-complex-package)


(real-part (make-complex-from-real-imag 1 1))
(magnitude (make-complex-from-real-imag 1 1))


(define complex1 (make-complex-from-real-imag (make-rational 1 2) (make-rational 3 4)))
(define complex2 (make-complex-from-real-imag (make-rational 1 2) (make-rational 3 4)))
(real-part complex1)
(imag-part complex1)
(imag-part (make-from-real-imag (make-number 1) (make-rational 3 2)))
(add complex1 complex2)
(magnitude complex1)
(mul complex1 complex2)

(sqrt 1.2)

;; тут у нас чаще всего получаются scheme-real
;; поэтому мы приводим к этому типу
;; иначе во всех пакетах у нас перемешаются разные типы что плохо влияет на разработку пакетов отдельно

(define (operation-with-raise-to-real op x)
  (define (tag z) (attach-tag 'scheme-real z))
  (if (equal? (type-tag x) 'rational)
      (operation-with-raise-to-real op (raise x))
      (tag (op (contents x)))))

(define (gsqrt x)
  (operation-with-raise-to-real sqrt x))

(define (cosine x)
  (operation-with-raise-to-real cos x))

(define (sine x)
  (operation-with-raise-to-real sin x))

(define (atangens x)
  (operation-with-raise-to-real atan x))


(define (install-rectangular-package)
  ;; internal procedures
  (define (real-part z) (car z))
  (define (imag-part z) (cdr z))
  (define (make-from-real-imag x y) (cons x y))
  (define (magnitude z)
    (gsqrt (add (mul (real-part z) (real-part z))
                      (mul (imag-part z) (imag-part z)))))
  (define (angle z)
    (atangens (div (imag-part z) (real-part z))))
  (define (make-from-mag-ang r a) 
    (cons (mul r (sine a)) (mul r (sine a))))
  ;; interface to the rest of the system
  (define (tag x) (attach-tag 'rectangular x))
  (put 'real-part '(rectangular) real-part)
  (put 'imag-part '(rectangular) imag-part)
  (put 'magnitude '(rectangular) magnitude)
  (put 'angle '(rectangular) angle)
  (put 'make-from-real-imag 'rectangular 
       (lambda (x y) (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang 'rectangular 
       (lambda (r a) (tag (make-from-mag-ang r a))))
  'done)
(install-rectangular-package)


(define (install-rational-package)
  ;; internal procedures
  (define (numer x) (car x))
  (define (denom x) (cdr x))
  (define (make-rat n d)
    (let ((g (gcd n d)))
      (cons (/ n g) (/ d g))))
  (define (add-rat x y)
    (make-rat (+ (* (numer x) (denom y))
                 (* (numer y) (denom x)))
              (* (denom x) (denom y))))
  (define (sub-rat x y)
    (make-rat (- (* (numer x) (denom y))
                 (* (numer y) (denom x)))
              (* (denom x) (denom y))))
  (define (mul-rat x y)
    (make-rat (* (numer x) (numer y))
              (* (denom x) (denom y))))
  (define (div-rat x y)
    (make-rat (* (numer x) (denom y))
              (* (denom x) (numer y))))
  ;; interface to rest of the system
  (define (tag x) (attach-tag 'rational x))
  (define (real-tag x) (attach-tag 'scheme-real x))

  (put 'add '(rational rational)
       (lambda (x y) (tag (add-rat x y))))
  (put 'sub '(rational rational)
       (lambda (x y) (tag (sub-rat x y))))
  (put 'mul '(rational rational)
       (lambda (x y) (tag (mul-rat x y))))
  (put 'div '(rational rational)
       (lambda (x y) (tag (div-rat x y))))

  (put 'make 'rational
       (lambda (n d) (tag (make-rat n d))))
  (put 'equ? '(rational rational)
       (lambda (x y) (or
                 (and (= (numer x) (numer y)) (= (denom x) (denom y)))
                 (and (= (numer x) (numer y)) (= (numer x) 0)))))
  (put '=zero? '(rational)
       (lambda (x) (= (numer x) 0)))
  'done)
(install-rational-package)

(define (install-polar-package)
  ;; internal procedures
  (define (magnitude z) (car z))
  (define (angle z) (cdr z))
  (define (make-from-mag-ang r a) (cons r a))
  (define (real-part z)
    (mul (magnitude z) (cosine (angle z))))
  (define (imag-part z)
    (mul (magnitude z) (sine (angle z))))
  (define (make-from-real-imag x y) 
    (cons (gsqrt (add (mul x x) (mul y y)))
          (atan (div y x))))
  ;; interface to the rest of the system
  (define (tag x) (attach-tag 'polar x))
  (put 'real-part '(polar) real-part)
  (put 'imag-part '(polar) imag-part)
  (put 'magnitude '(polar) magnitude)
  (put 'angle '(polar) angle)
  (put 'make-from-real-imag 'polar
       (lambda (x y) (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang 'polar 
       (lambda (r a) (tag (make-from-mag-ang r a))))
  'done)
(install-polar-package)


(make-from-real-imag (make-number 1) (make-number 3))
(make-from-real-imag (make-number 2) (make-number 4))


;; 2.5.3 Пример. Символьная алгебра.

;; Подзадача работа с полниомами
;; Упражнение 2.87

(define (install-polynomial-package)
  ;; internal procedures
  ;; representation of poly
  (define (make-poly variable term-list)
    (cons variable term-list))
  (define (variable p) (car p))
  (define (term-list p) (cdr p))
  ;; <procedures same-variable? and variable? from section 2.3.2>
  (define (variable? x) (symbol? x))
  (define (same-variable? v1 v2)
    (and (variable? v1) (variable? v2) (eq? v1 v2)))
  ;; representation of terms and term lists
  ;; ((100 1) (2 2) (0 1)) = x^100 + 2x^2 + 1

  ;; procedurs on poly
  (define (add-poly p1 p2)
    (if (same-variable? (variable p1) (variable p2))
        (make-poly (variable p1)
                   (add (term-list p1)
                        (term-list p2)))
        (error "Polys not in same var -- ADD-POLY"
               (list p1 p2))))
  (define (mul-poly p1 p2)
    (if (same-variable? (variable p1) (variable p2))
        (make-poly (variable p1)
                   (mul (term-list p1)
                        (term-list p2)))
        (error "Polys not in same var -- MUL-POLY"
               (list p1 p2))))

  ;; interface to rest of the system
  (define (tag p) (attach-tag 'polynomial p))
  (put 'add '(polynomial polynomial)
       (lambda (p1 p2) (tag (add-poly p1 p2))))
  (put 'sub '(polynomial polynomial)
       (lambda (p1 p2) (tag (add-poly p1 (contents (negation (tag p2)))))))
  (put 'mul '(polynomial polynomial)
       (lambda (p1 p2) (tag (mul-poly p1 p2))))
  (put 'make 'polynomial
       (lambda (var terms) (tag (make-poly var terms))))
  (put '=zero? '(polynomial)
       (lambda (x) (=zero? (term-list x))))
  (put 'negation '(polynomial)
       (lambda (p) (tag (make-poly (variable p) (negation (term-list p))))))
  'done)

(install-polynomial-package)

;; выключаем теги для обычных чисел
(define (attach-tag type-tag contents)
  (if (number? contents)
      (cons 'scheme-number contents)
      (cons type-tag contents)))
(define (type-tag datum)
  (cond ((number? datum) 'scheme-number)
        ((pair? datum) (car datum))
        (else (error "Bad tagged datum -- TYPE-TAG" datum))))
(define (contents datum)
  (cond ((number? datum) datum)
        ((pair? datum) (cdr datum))
        (else (error "Bad tagged datum -- CONTENTS" datum))))

(define (make-polynomial var terms)
  ((get 'make 'polynomial) var terms))



(define (install-scheme-number-package)
  (define (tag x) (attach-tag 'scheme-number x))
  (put 'add '(scheme-number scheme-number)
       (lambda (x y) (+ x y)))
  (put 'sub '(scheme-number scheme-number)
       (lambda (x y) (- x y)))
  (put 'mul '(scheme-number scheme-number)
       (lambda (x y) (* x y)))
  (put 'div '(scheme-number scheme-number)
       (lambda (x y) (/ x y)))
  (put 'equ? '(scheme-number scheme-number)
       (lambda (x y) (= x y)))
  (put '=zero? '(scheme-number)
       (lambda (x) (= x 0)))
  (put 'make 'scheme-number
       (lambda (x) x))
  ;; following added to Scheme-number package
  (put 'exp '(scheme-number scheme-number)
       (lambda (x y) (expt x y))) ; using primitive expt
  (put 'negation '(scheme-number)
       (lambda (x) (- x)))
  'done)
(install-scheme-number-package) ;; установка пакета
(add 1 2)
(negation 1)
(define (negation x) (apply-generic 'negation x))
(define (=zero? x) (apply-generic '=zero? x))
(define (add x y) (apply-generic 'add x y))
(define (sub x y) (apply-generic 'sub x y))
(define (mul x y) (apply-generic 'mul x y))


;; Ex. 2.89, 2.90


;; ((100 1) (2 2) (0 1)) - x^100 + 2x^2 + 1

(define (install-polynomial-sparse-package)
  (define (tag x) (attach-tag 'sparse x))
  (define (the-empty-termlist) '())
  (define (first-term term-list) (car term-list))
  (define (rest-terms term-list) (cdr term-list))
  (define (empty-termlist? term-list) (null? term-list))
  (define (make-term order coeff) (list order coeff))
  (define (order term) (car term))
  (define (coeff term) (cadr term))
  (define (negation-term-list L1)
    (if (empty-termlist? L1)
        L1
        (cons (make-term (order (first-term L1)) (negation (coeff (first-term L1)))) (negation-term-list (rest-terms L1)))))

  (define (adjoin-term term term-list)
    (if (=zero? (coeff term))
        term-list
        (cons term term-list)))
  (define (add-terms L1 L2)
    (cond ((empty-termlist? L1) L2)
          ((empty-termlist? L2) L1)
          (else
           (let ((t1 (first-term L1)) (t2 (first-term L2)))
             (cond ((> (order t1) (order t2))
                    (adjoin-term
                     t1 (add-terms (rest-terms L1) L2)))
                   ((< (order t1) (order t2))
                    (adjoin-term
                     t2 (add-terms L1 (rest-terms L2))))
                   (else
                    (adjoin-term
                     (make-term (order t1)
                                (add (coeff t1) (coeff t2)))
                     (add-terms (rest-terms L1)
                                (rest-terms L2)))))))))

  (define (sub-terms L1 L2)
    (add-terms L1 (negation-term-list L2)))

  (define (mul-terms L1 L2)
    (if (empty-termlist? L1)
        (the-empty-termlist)
        (add-terms (mul-term-by-all-terms (first-term L1) L2)
                   (mul-terms (rest-terms L1) L2))))
  (define (mul-term-by-all-terms t1 L)
    (if (empty-termlist? L)
        (the-empty-termlist)
        (let ((t2 (first-term L)))
          (adjoin-term
           (make-term (+ (order t1) (order t2))
                      (mul (coeff t1) (coeff t2)))
           (mul-term-by-all-terms t1 (rest-terms L))))))


  (define (div-terms L1 L2)
    (if (empty-termlist? L1)
        (list (the-empty-termlist) (the-empty-termlist))
        (let ((t1 (first-term L1))
              (t2 (first-term L2)))
          (if (> (order t2) (order t1))
              (list (the-empty-termlist) L1)
              (let ((new-c (div (coeff t1) (coeff t2)))
                    (new-o (- (order t1) (order t2))))
                (let ((rest-of-result
                       (sub-terms L1 (mul-terms (list (make-term new-o new-c)) L2))))

                  (let ((result (div-terms rest-of-result L2)))
                    (list (add-terms (list (make-term new-o new-c)) (car result)) (cdr result)))))))))

  (define (all-zero? L1)
    (cond ((empty-termlist? L1) true)
          ((and (=zero? (coeff (first-term L1))) (all-zero? (rest-terms L1))) true)
          (else false)))

  (put 'negation '(sparse)
       (lambda (p) (tag (negation-term-list p))))
  (put 'make 'sparse
       (lambda (terms) (tag terms)))
  (put '=zero? '(sparse)
       (lambda (terms) (all-zero? terms)))
  (put 'add '(sparse sparse)
       (lambda (terms1 terms2) (tag (add-terms terms1 terms2))))
  (put 'sub '(sparse sparse)
       (lambda (terms1 terms2) (tag (sub-terms terms1 terms2))))
  (put 'mul '(sparse sparse)
       (lambda (terms1 terms2) (tag (mul-terms terms1 terms2))))
  (put 'div '(sparse sparse)
       (lambda (terms1 terms2) (tag (div-terms terms1 terms2))))
  'done)

(install-polynomial-sparse-package)
(put 'add-terms '(sparce sparce) false)
(define (add x y) (apply-generic 'add x y))
(define (mul x y) (apply-generic 'mul x y))

(define (make-sparse-terms terms)
  ((get 'make 'sparse) terms))

(define sparse-terms (make-sparse-terms '((2 1) (1 1) (0 1))))
sparce-terms
((get 'add '(sparse sparse)) '((2 1) (1 1) (0 1)) '((2 1) (1 1) (0 1)))
(add sparse-terms sparse-terms)
(sub sparse-terms sparse-terms)
(mul sparse-terms sparse-terms)
(div sparse-terms sparse-terms)
(div sparse-terms (make-sparse-terms '((1 1) (0 1))))
(negation (make-sparce-terms '((2 1) (1 1) (0 1))))
(=zero? sparce-terms)
(=zero? (make-sparse-terms '((1 0) (2 0) (0 0))))



;; (1 2 0 3 -2 -5)       - x^5 + 2x^4 + 0x^3 + 3x^2 - 2x -5


(define (install-polynomial-dense-package)
  (define (tag x) (attach-tag 'dense x))
  (define (the-empty-termlist) '())
  (define (first-term term-list) (car term-list))
  (define (rest-terms term-list) (cdr term-list))
  (define (empty-termlist? term-list) (null? term-list))
  ;; (define (make-term order coeff) (list order coeff))
  (define (first-coeff term-list) (car term-list))
  (define (first-order term-list) (- (length term-list) 1))
  (define (negation-term-list L1)
    (if (empty-termlist? L1)
        L1
        (cons (negation (first-term L1)) (negation-term-list (rest-terms L1)))))
  (define (add-terms L1 L2)
    (display L1)
    (define (sum-terms L1 L2)
      (cond ((empty-termlist? L1) L2)
            ((empty-termlist? L2) L1)
            (else
             (cons (+ (first-term L1) (first-term L2)) (sum-terms (rest-terms L1) (rest-terms L2))))))
    (reverse (sum-terms (reverse L1) (reverse L2))))

  (define (mul-terms L1 L2)
    (if (empty-termlist? L1)
        (the-empty-termlist)
        (add-terms (mul-term-by-all-terms L1 L2)
                   (mul-terms (rest-terms L1) L2))))

  (define (mul-term-by-all-terms L1 L2)
    (define (mul-coeff L constant)
      (if (empty-termlist? L)
          (the-empty-termlist)
          (cons (* (first-term L) constant) (mul-coeff (rest-terms L) constant))))
    (define (create-n-zeroes n)
      (if (> n 0)
          (cons 0 (create-n-zeroes (- n 1)))
          '()))
    (if (empty-termlist? L2)
        (the-empty-termlist)
        (let ((coeff (first-coeff L1))
              (order (first-order L1)))
          (append (mul-coeff L2 coeff) (create-n-zeroes order)))))

  (define (all-zero? L1)
    (cond ((empty-termlist? L1) true)
          ((and (=zero? (first-term L1)) (all-zero? (rest-terms L1))) true)
          (else false)))

  (put 'negation '(dense)
       (lambda (p) (tag (negation-term-list p))))
  (put 'make 'dense
       (lambda (terms) (tag terms)))
  (put '=zero? '(dense)
       (lambda (terms) (all-zero? terms)))
  (put 'add '(dense dense)
       (lambda (terms1 terms2) (tag (add-terms terms1 terms2))))
  (put 'mul '(dense dense)
       (lambda (terms1 terms2) (tag (mul-terms terms1 terms2))))
  'done)
(install-polynomial-dense-package)
(define (make-dense-terms terms)
  ((get 'make 'dense) terms))
(define dense-terms (make-dense-terms '(1 1 1)))
dense-terms
((get 'add '(dense dense)) '(3 0 2 1) '(3 0 2 1))
(add dense-terms dense-terms)
(mul dense-terms dense-terms)
(negation dense-terms)
(=zero? dense-terms)
(=zero? (make-dense-terms '(0 0 0 0)))
(define (create-n-zeroes n)
      (if (> n 0)
          (cons 0 (create-n-zeroes (- n 1)))
          '()))
(append (create-n-zeroes 1) (list 1 2))



(define pol1 (make-polynomial 'x (make-sparse-terms '((2 1) (1 1) (0 1)))))
(define pol2 (make-polynomial 'x (make-sparse-terms '((2 1) (1 1) (0 1)))))

(negation pol2)
(add pol1 pol2)
(add pol1 (negation pol2))
(sub pol1 pol2)
(mul pol1 pol2)

(=zero? 0)

(make-number 1)


;; Ex. 2.91

;; в последнем пакете для install-polynomial-sparse-package

;; Иерархия типов в символической алгебре

;; для полиномов от нескольких переменных, мы не можем привести их к башне, как это было с числами, потому что полином от x, может иметь коэффициенты которые являются полиномом от y. Также возможно иметь полином от y чьи коэффициенты полиномы от х. Ни один из этих типов не будет "выше" другого. Но также необходимо уметь складывать полиному из разных типов (c разными переменными).

;; одно из возможных решений привести полином к другому типу, что бы оба полинома были одного типа (по одной переменной). То есть мы создаем искусственную башню и приводим полиному к канонической форме. Эта стратегия работает хорошо, за исключением того что мы может зря расширить полином, делая его трудочитаемым.

;; Не должно удивлять что приведение типов серьёзная проблема в дизайне больших приложений.

;; Ex 2.92


(define (install-polynomial-package)
  ;; internal procedures
  ;; representation of poly
  (define (make-poly variable term-list)
    (cons variable term-list))
  (define (variable p) (car p))
  (define (term-list p) (cdr p))
  ;; <procedures same-variable? and variable? from section 2.3.2>
  (define (variable? x) (symbol? x))
  (define (same-variable? v1 v2)
    (and (variable? v1) (variable? v2) (eq? v1 v2)))
  ;; representation of terms and term lists
  ;; ((100 1) (2 2) (0 1)) = x^100 + 2x^2 + 1

  ;; procedurs on poly
  (define (add-poly p1 p2)
    (if (same-variable? (variable p1) (variable p2))
        (make-poly (variable p1)
                   (add (term-list p1)
                        (term-list p2)))
        (error "Polys not in same var -- ADD-POLY"
               (list p1 p2))))
  (define (mul-poly p1 p2)
    (if (same-variable? (variable p1) (variable p2))
        (make-poly (variable p1)
                   (mul (term-list p1)
                        (term-list p2)))
        (error "Polys not in same var -- MUL-POLY"
               (list p1 p2))))

  ;; interface to rest of the system
  (define (tag p) (attach-tag 'polynomial p))
  (put 'add '(polynomial polynomial)
       (lambda (p1 p2) (tag (add-poly p1 p2))))
  (put 'sub '(polynomial polynomial)
       (lambda (p1 p2) (tag (add-poly p1 (contents (negation (tag p2)))))))
  (put 'mul '(polynomial polynomial)
       (lambda (p1 p2) (tag (mul-poly p1 p2))))
  (put 'make 'polynomial
       (lambda (var terms) (tag (make-poly var terms))))
  (put '=zero? '(polynomial)
       (lambda (x) (=zero? (term-list x))))
  (put 'negation '(polynomial)
       (lambda (p) (tag (make-poly (variable p) (negation (term-list p))))))
  'done)

(install-polynomial-package)



;; надо уметь приводить к полиному по x



(define (install-coercion-polynomial-package)
  (define (tag p) (attach-tag 'polynomial p))
  (define (term-list pol)
    (cdr (cdr (cdr pol)))
    )
  (define (order term) (car term))
  (define (coeff term) (cadr term))

  (define (switch-variable pol)
    (define (switch-single-term single)
      ;; term here y and polinom x
      (
       make-polynomial
       'x
       (make-sparse-terms
        (map (lambda (term)
               (list
                (order term)
                (make-polynomial
                 'y
                 (make-sparse-terms (list (list (car single) (coeff term))))
                 )))
             (cadr single)))))

    (define (sum-list l)
      (if (= (length l) 2)
          (add (car l) (cadr l))
          (add (car l) (sum-list (cdr l)))))

    ;; пусть пока везде полиномы, в общем виде надо добавить условие
    (let ((mapterms (map (lambda (term) (cons (car term) (list (term-list (cadr term))))) (term-list pol))))
      (sum-list (map (lambda (item) (switch-single-term item)) mapterms))
      )
    )
  ;; interface
  (put 'raise '(polynomial)
       (lambda (pol) (switch-variable (tag pol))))
  'done)
(install-coercion-polynomial-package)

(define pol2variable (make-polynomial
                      'y
                      (make-sparse-terms (list (list 2 (make-polynomial 'x (make-sparse-terms '((2 1) (0 1)))))
                                               (list 1 (make-polynomial 'x (make-sparse-terms '((3 1) (2 2) (0 3)))))
                                               (list 0 (make-polynomial 'x (make-sparse-terms '((0 5)))))))))

pol2variable
(raise pol2variable)

(define pol1 (
              make-polynomial
              'x
              (make-sparse-terms (list (list 2 (make-polynomial 'y (make-sparse-terms '((2 1) (0 1)))))
                                       (list 1 (make-polynomial 'y (make-sparse-terms '((0 1)))))
                                       (list 0 (make-polynomial 'y (make-sparse-terms '((0 1)))))))))
pol1
(add pol1 (raise pol2variable))


;; Дополнительные упражнения, рациональные функции.

;; уберем из пакета рациональных, gcd в make-rat
;; и сделаем что бы операции были общими.

;; Ex. 2.93

(define (install-rational-polynomial-package)
  ;; internal procedures
  (define (numer x) (car x))
  (define (denom x) (cdr x))
  (define (make-rat n d)
    (let ((red (reduce n d)))
      (cons (car red) (cadr red))))

  (define (add-rat x y)
    (make-rat (add (mul (numer x) (denom y))
                 (mul (numer y) (denom x)))
              (mul (denom x) (denom y))))
  (define (sub-rat x y)
    (make-rat (sub (mul (numer x) (denom y))
                 (mul (numer y) (denom x)))
              (mul (denom x) (denom y))))
  (define (mul-rat x y)
    (make-rat (mul (numer x) (numer y))
              (mul (denom x) (denom y))))
  (define (div-rat x y)
    (make-rat (mul (numer x) (denom y))
              (mul (denom x) (numer y))))
  ;; interface to rest of the system
  (define (tag x) (attach-tag 'rational x))
  (define (real-tag x) (attach-tag 'scheme-real x))


  (put 'add '(rational rational)
       (lambda (x y) (tag (add-rat x y))))
  (put 'sub '(rational rational)
       (lambda (x y) (tag (sub-rat x y))))
  (put 'mul '(rational rational)
       (lambda (x y) (tag (mul-rat x y))))
  (put 'div '(rational rational)
       (lambda (x y) (tag (div-rat x y))))

  (put 'make 'rational
       (lambda (n d) (tag (make-rat n d))))
  ;; (put 'equ? '(rational rational)
  ;;      (lambda (x y) (or
  ;;                (and (= (numer x) (numer y)) (= (denom x) (denom y)))
  ;;                (and (= (numer x) (numer y)) (= (numer x) 0)))))
  (put '=zero? '(rational)
       (lambda (x) (=zero? (numer x))))
  'done)
(install-rational-polynomial-package)

(define rf1 (make-rational p1 p2))

(define (make-rational n d)
  ((get 'make 'rational) n d))


(define p1 (make-polynomial 'x (make-sparse-terms '((2 1)(0 1)))))
(define p2 (make-polynomial 'x (make-sparse-terms '((3 1)(0 1)))))
(define rf (make-rational p2 p1))

rf
(add rf rf)


(define (install-polynomial-sparse-package)
  (define (tag x) (attach-tag 'sparse x))
  (define (the-empty-termlist) '())
  (define (first-term term-list) (car term-list))
  (define (rest-terms term-list) (cdr term-list))
  (define (empty-termlist? term-list) (null? term-list))
  (define (make-term order coeff) (list order coeff))
  (define (order term) (car term))
  (define (coeff term) (cadr term))
  (define (negation-term-list L1)
    (if (empty-termlist? L1)
        L1
        (cons (make-term (order (first-term L1)) (negation (coeff (first-term L1)))) (negation-term-list (rest-terms L1)))))

  (define (adjoin-term term term-list)
    (if (=zero? (coeff term))
        term-list
        (cons term term-list)))
  (define (add-terms L1 L2)
    (cond ((empty-termlist? L1) L2)
          ((empty-termlist? L2) L1)
          (else
           (let ((t1 (first-term L1)) (t2 (first-term L2)))
             (cond ((> (order t1) (order t2))
                    (adjoin-term
                     t1 (add-terms (rest-terms L1) L2)))
                   ((< (order t1) (order t2))
                    (adjoin-term
                     t2 (add-terms L1 (rest-terms L2))))
                   (else
                    (adjoin-term
                     (make-term (order t1)
                                (add (coeff t1) (coeff t2)))
                     (add-terms (rest-terms L1)
                                (rest-terms L2)))))))))

  (define (sub-terms L1 L2)
    (add-terms L1 (negation-term-list L2)))

  (define (mul-terms L1 L2)
    (if (empty-termlist? L1)
        (the-empty-termlist)
        (add-terms (mul-term-by-all-terms (first-term L1) L2)
                   (mul-terms (rest-terms L1) L2))))
  (define (mul-term-by-all-terms t1 L)
    (if (empty-termlist? L)
        (the-empty-termlist)
        (let ((t2 (first-term L)))
          (adjoin-term
           (make-term (+ (order t1) (order t2))
                      (mul (coeff t1) (coeff t2)))
           (mul-term-by-all-terms t1 (rest-terms L))))))


  (define (div-terms L1 L2)
    (if (empty-termlist? L1)
        (list (the-empty-termlist) (the-empty-termlist))
        (let ((t1 (first-term L1))
              (t2 (first-term L2)))
          (if (> (order t2) (order t1))
              (list (the-empty-termlist) L1)
              (let ((new-c (div (coeff t1) (coeff t2)))
                    (new-o (- (order t1) (order t2))))
                (let ((rest-of-result
                       (sub-terms L1 (mul-terms (list (make-term new-o new-c)) L2))))

                  (let ((result (div-terms rest-of-result L2)))
                    (list (adjoin-term (make-term new-o new-c) (car result)) (cadr result)))))))))

  (define (all-zero? L1)
    (cond ((empty-termlist? L1) true)
          ((and (=zero? (coeff (first-term L1))) (all-zero? (rest-terms L1))) true)
          (else false)))


  (define (multiple-coeffs q integer-factor)
    (map (lambda (x) (list (order x) (* (coeff x) integer-factor))) q))

  (define (integerizing-factor p q)
    (let ((c (coeff (first-term q)))
          (o1 (order (first-term p)))
          (o2 (order (first-term q))))
      (expt c (+ 1 o1 (- o2)))))

  (define (remainder-terms a b)
    (cadr (div-terms a b)))

  (define (pseudoremainder-terms a b)
    (let ((ma (multiple-coeffs a (integerizing-factor a b))))
      (display (cadr (div-terms ma b)))
      (newline)
      (newline)
      (cadr (div-terms ma b))))

  (define (reduce-gcd-terms-coeff terms)

    (define (gcd-integer-list l)
      (if (< (length l) 2)
          (car l)
          (gcd-integer-list (cons (gcd (car l) (cadr l)) (cddr l)))))

    (let ((l (map (lambda (x) (coeff x)) terms)))
      (multiple-coeffs terms (/ 1 (gcd-integer-list l))))
    )

  (define (reduce-terms a b)
    (let ((gcd-ab (gcd-terms a b)))
      (list (tag (car (div-terms a gcd-ab))) (tag (car (div-terms b gcd-ab))))))


  (define (gcd-terms a b)
    (if (empty-termlist? b)
        (reduce-gcd-terms-coeff a)
        (gcd-terms b (pseudoremainder-terms a b))))

  ;; interface
  (put 'negation '(sparse)
       (lambda (p) (tag (negation-term-list p))))
  (put 'make 'sparse
       (lambda (terms) (tag terms)))
  (put '=zero? '(sparse)
       (lambda (terms) (all-zero? terms)))
  (put 'add '(sparse sparse)
       (lambda (terms1 terms2) (tag (add-terms terms1 terms2))))
  (put 'sub '(sparse sparse)
       (lambda (terms1 terms2) (tag (sub-terms terms1 terms2))))
  (put 'mul '(sparse sparse)
       (lambda (terms1 terms2) (tag (mul-terms terms1 terms2))))
  (put 'div '(sparse sparse)
       (lambda (terms1 terms2) (tag (div-terms terms1 terms2))))
  (put 'greatest-common-divisor '(sparse sparse)
       (lambda (terms1 terms2) (tag (gcd-terms terms1 terms2))))
  (put 'reduce '(sparse sparse)
       (lambda (terms1 terms2) (reduce-terms terms1 terms2)))
  'done)

(install-polynomial-sparse-package)
(greatest-common-divisor q1 q2)

(define (reduce term1 term2)
  (apply-generic 'reduce term1 term2))
;; (define (gcd-terms term1 term2)
;;   (apply-generic 'gcd-terms term1 term2))

;; (define p1 (make-polynomial 'x (make-sparse-terms '((2 1)(0 1)))))
;; (define p2 (make-polynomial 'x (make-sparse-terms '((3 1)(0 1)))))

(gcd-terms (make-sparse-terms '((4 1) (3 -1) (2 -2) (1 2)))
           (make-sparse-terms '((3 1) (1 -1)))
           )


(define (install-polynomial-package)
  ;; internal procedures
  ;; representation of poly
  (define (tag p) (attach-tag 'polynomial p))
  (define (make-poly variable term-list)
    (cons variable term-list))
  (define (variable p) (car p))
  (define (term-list p) (cdr p))
  ;; <procedures same-variable? and variable? from section 2.3.2>
  (define (variable? x) (symbol? x))
  (define (same-variable? v1 v2)
    (and (variable? v1) (variable? v2) (eq? v1 v2)))
  ;; representation of terms and term lists
  ;; ((100 1) (2 2) (0 1)) = x^100 + 2x^2 + 1

  ;; procedurs on poly
  (define (add-poly p1 p2)
    (if (same-variable? (variable p1) (variable p2))
        (make-poly (variable p1)
                   (add (term-list p1)
                        (term-list p2)))
        (error "Polys not in same var -- ADD-POLY"
               (list p1 p2))))
  (define (mul-poly p1 p2)
    (if (same-variable? (variable p1) (variable p2))
        (make-poly (variable p1)
                   (mul (term-list p1)
                        (term-list p2)))
        (error "Polys not in same var -- MUL-POLY"
               (list p1 p2))))

  (define (gcd-poly p1 p2)
    (if (same-variable? (variable p1) (variable p2))
        (make-poly (variable p1)
                   (greatest-common-divisor (term-list p1)
                                            (term-list p2)))
        (error "Polys not in same var -- GCD-POLY"
               (list p1 p2))))

  (define (reduce-poly p1 p2)
    (if (same-variable? (variable p1) (variable p2))
        (map (lambda (terms) (tag (make-poly (variable p1) terms)))
             (reduce (term-list p1)
                     (term-list p2)))
        (error "Polys not in same var -- REDUCE-POLY"
               (list p1 p2))))

  ;; interface to rest of the system
  (put 'add '(polynomial polynomial)
       (lambda (p1 p2) (tag (add-poly p1 p2))))
  (put 'sub '(polynomial polynomial)
       (lambda (p1 p2) (tag (add-poly p1 (contents (negation (tag p2)))))))
  (put 'mul '(polynomial polynomial)
       (lambda (p1 p2) (tag (mul-poly p1 p2))))
  (put 'make 'polynomial
       (lambda (var terms) (tag (make-poly var terms))))
  (put '=zero? '(polynomial)
       (lambda (x) (=zero? (term-list x))))
  (put 'negation '(polynomial)
       (lambda (p) (tag (make-poly (variable p) (negation (term-list p))))))
  (put 'greatest-common-divisor '(polynomial polynomial)
       (lambda (p1 p2) (tag (gcd-poly p1 p2))))
  (put 'reduce '(polynomial polynomial)
       (lambda (p1 p2) (reduce-poly p1 p2)))
  'done)

(install-polynomial-package)

(define (greatest-common-divisor p1 p2)
  (apply-generic 'greatest-common-divisor p1 p2))


(define terms1 (make-sparse-terms '((4 1) (3 -1) (2 -2) (1 2))))
(define terms2 (make-sparse-terms '((3 1) (1 -1))))

(define p1 (make-polynomial 'x terms1))
(define p2 (make-polynomial 'x terms2))
(greatest-common-divisor p1 p2)


;; Ex. 2.95

(define terms1 (make-sparse-terms '((2 1) (1 -2) (0 1))))
(define p1 (make-polynomial 'x terms1))
(define terms2 (make-sparse-terms '((2 11) (0 7))))
(define p2 (make-polynomial 'x terms2))
(define terms3 (make-sparse-terms '((1 13) (0 5))))
(define p3 (make-polynomial 'x terms3))
(define q1 (mul p1 p2))
(define q2 (mul p1 p3))
q1
q2

(greatest-common-divisor q1 q2)

;; видим что появляются дроби при нахождении НОД, для полиномов от одной переменной, можно ввести понятие псевдоделения, умножая делитель на константу  с^(1+O_1-O_2), где O_1 порядок p, O_2 порядок  



(define p1 (make-polynomial 'x (make-sparse-terms '((1 1)(0 1)))))
(define p2 (make-polynomial 'x (make-sparse-terms '((3 1)(0 -1)))))
(define p3 (make-polynomial 'x (make-sparse-terms '((1 1)))))
(define p4 (make-polynomial 'x (make-sparse-terms '((2 1)(0 -1)))))
p1
p2
(define rf1 (make-rational p1 p2))
(define rf2 (make-rational p3 p4))
rf2

rf1

(add rf1 rf2)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; 3.1  Назначение и локальное состояние.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define balance 100)

(define (withdraw amount)
  (if (>= balance amount)
      (begin (set! balance (- balance amount))
             balance)
      "Insufficient funds"))
(withdraw 75)
(withdraw 50)

(define new-withdraw
  (let ((balance 100))
    (lambda (amount)
      (if (>= balance amount)
          (begin (set! balance (- balance amount))
                 balance)
          "Insufficient funds"))))
(new-withdraw 75)


(define (make-withdraw balance)
  (lambda (amount)
    (if (>= balance amount)
        (begin (set! balance (- balance amount))
               balance)
        "Insufficient funds")))

(define W1 (make-withdraw 100))
(W1 10)

(define (make-account balance)
  (define (withdraw amount)
    (if (>= balance amount)
        (begin (set! balance (- balance amount))
               balance)
        "Insufficient funds"))
  (define (deposit amount)
    (set! balance (+ balance amount))
    balance)
  (define (dispatch m)
    (cond ((eq? m 'withdraw) withdraw)
          ((eq? m 'deposit) deposit)
          (else (error "Unknown request -- MAKE-ACCOUNT"
                       m))))
  dispatch)

(define acc (make-account 100))
((acc 'withdraw) 50)
((acc 'deposit) 50)


(define (make-accumulator sum)
  (lambda (x) (begin
           (set! sum (+ sum x))
           sum
           )))


(define A (make-accumulator 5))
(A 10)
15
(A 10)
25


;; Ex. 3.2

(define (make-monitor f)
  (define how-many-call? 0)
  (define (incretement)
    (begin
      (set! how-many-call? (+ how-many-call? 1))
      f))
  (define (dispatch m)
    (cond ((eq? m 'how-many-call?) how-many-call?)
          (else ((incretement) m))))
  dispatch)

(define s (make-monitor sqrt))
(s 100)
(s 'how-many-call?)


;; Ex 3.3, 3.4

(define (make-account balance password)
  (define (withdraw amount)
    (if (>= balance amount)
        (begin (set! balance (- balance amount))
               balance)
        "Insufficient funds"))
  (define (deposit amount)
    (set! balance (+ balance amount))
    balance)

  (define (permission-denied . m)
    (display "Incorrect password"))


  (define monitor-error-password (make-monitor permission-denied))
  (define (call-limit?)
    (when (= (monitor-error-password 'how-many-call?) 2)
        (display "Viu viu viu viu..")))

  (define (dispatch pass func)
    (cond ((not (eq? pass password)) (begin
                                       (call-limit?)
                                       monitor-error-password))
          ((eq? func 'withdraw) withdraw)
          ((eq? func 'deposit) deposit)
          (else (error "Unknown request -- MAKE-ACCOUNT"
                       func))))
  dispatch)

(define acc (make-account 100 'secret-password))
acc
((acc 'secret-password 'withdraw) 40)
((acc 'some-other-password 'deposit) 50)


(define rand
  (let ((x random-init))
    (lambda ()
      (set! x (rand-update x))
      x)))


(define (estimate-pi trials)
  (sqrt (/ 6 (monte-carlo trials cesaro-test))))
(define (cesaro-test)
   (= (gcd (rand) (rand)) 1))
(define (monte-carlo trials experiment)
  (define (iter trials-remaining trials-passed)
    (cond ((= trials-remaining 0)
           (/ trials-passed trials))
          ((experiment)
           (iter (- trials-remaining 1) (+ trials-passed 1)))
          (else
           (iter (- trials-remaining 1) trials-passed))))
  (iter trials 0))




;; Ex 3.5

;; окружность
(define (predicate x y)
  (<= (+ (square (- x 25)) (square (- y 25))) (square 25)))

;; x от 2 до 8
;; y от 4 до 10
(define (random-in-range low high)
  (let ((range (- high low)))
    (+ low (random range))))

(define (estimateintegral P min-x max-x min-y max-y trials)
  (define (test)
    (P
     (random-in-range min-x max-x)
     (random-in-range min-y max-y)))

  (* (monte-carlo trials test) (* (- max-x min-x) (- max-y min-y))))

(estimateintegral predicate 0 50 0 50 100000)
                                        ;Value: 16866/625 26.46

;; приближенное значение pi равно результат поделить на квадрат 3.


;; Ex. 3.6.

(define (rand m)
  (define init-value 2)

  (define (rand-update)
    (cond ((= init-value 0) 7)
          ((= init-value 2) 5)
          ((= init-value 5) 8)
          ((= init-value 8) 3)
          ((= init-value 3) 9)
          ((= init-value 9) 1)
          ((= init-value 1) 4)
          ((= init-value 4) 4)))

  (define (generate)
    (set! init-value (rand-update))
    init-value)

  (lambda (m) (cond ((eq? m 'reset) (lambda (x) (reset x)))
               ((eq? m 'generate) (generate))))
  
  )

(define rand
  (let ((x random-init))
    (lambda ()
      (set! x (rand-update x))
      x)))



(define rand
  (let ((random-init 2))
    (define (rand-update)
      (cond ((= random-init 0) 7)
            ((= random-init 2) 5)
            ((= random-init 5) 8)
            ((= random-init 8) 3)
            ((= random-init 3) 9)
            ((= random-init 9) 1)
            ((= random-init 1) 4)
            ((= random-init 4) 6)
            ((= random-init 6) 0)
            ((= random-init 7) 2)
            ))

    (define (generate)
      (set! random-init (rand-update))
      random-init)

    (lambda (m)
      (cond ((eq? m 'reset) (lambda (x)
                              (begin (set! random-init x)
                                     x)))
            ((eq? m 'generate) (generate))))
    ))

((rand 'reset) 8)
(rand 'generate)


;; Цена добавления присваиваивания

;; императивное программирование

;; Упрражнение 3.7

(define (make-monitor f)
    (define how-many-call? 0)
    (define (incretement)
      (begin
        (set! how-many-call? (+ how-many-call? 1))
        f))
    (define (dispatch m)
      (cond ((eq? m 'how-many-call?) how-many-call?)
            (else ((incretement) m))))
    dispatch)

(define (make-account balance password)
 
  (define (withdraw amount)
    (if (>= balance amount)
        (begin (set! balance (- balance amount))
               balance)
        "Insufficient funds"))
  (define (deposit amount)
    (set! balance (+ balance amount))
    balance)

  (define (permission-denied . m)
    "Incorrect password")


  (define monitor-error-password (make-monitor permission-denied))
  (define (call-limit?)
    (when (= (monitor-error-password 'how-many-call?) 7)
        (display "Viu viu viu viu..")))

  (define (dispatch pass func)
    (cond ((not (eq? pass password)) (begin
                                       (call-limit?)
                                       monitor-error-password))
          ((eq? func 'withdraw) withdraw)
          ((eq? func 'deposit) deposit)
          (else (error "Unknown request -- MAKE-ACCOUNT"
                       func))))
  dispatch)

(define peter-acc (make-account 100 'secret))
peter-acc
((peter-acc 'secret 'withdraw) 10)
((peter-acc 'some-other-password 'deposit) 50)
((peter-acc 'secret 'deposit) 50)


(define (make-joint account acc-pass new-pass)
  (display ((account acc-pass 'withdraw) 0))
  (when (equal? ((account acc-pass 'withdraw) 0) "Incorrect password")
    (error "Incorrect password"))

  (define (permission-denied . m)
    "Incorrect password")
  (define monitor-error-password (make-monitor permission-denied))
  (define (call-limit?)
    (when (= (monitor-error-password 'how-many-call?) 7)
        (display "Viu viu viu viu..")))

  (define (withdraw amount)
    ((account acc-pass 'withdraw) amount))

  (define (deposit amount)
    ((account acc-pass 'deposit) amount))

  (define (dispatch pass func)
    (cond ((not (eq? pass new-pass)) (begin
                                       (call-limit?)
                                       monitor-error-password))
          ((eq? func 'withdraw) withdraw)
          ((eq? func 'deposit) deposit)
          (else (error "Unknown request -- MAKE-ACCOUNT"
                       func))))
  dispatch
  )


(define paul-acc
  (make-joint peter-acc 'secret 'open-sesam))
((paul-acc 'open-sesam 'withdraw) 10)



;; Ex. 3.8

(define f (let ((y 0))
            (lambda (x) (if (= x 0)
                       (begin (set! y 1) x)
                       (- x y)))))

;; слева направо, сумма 0
(f 0) ;; 0
(f 1) ;; 0

;; справа налево 1
(f 1) ;; 1
(f 0) ;; 0


(+ (f 0) (f 1))



(define (make-withdraw initial-amount)
  (let ((balance initial-amount))
    (lambda (amount)
      (if (>= balance amount)
          (begin (set! balance (- balance amount))
                 (display initial-amount)
                 balance)
          "Insufficient funds"))))

(define W1 (make-withdraw 100))

(W1 50)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 3.2 Моделирование изменяемых данных
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define a (list (list 1 2) 2 3))
a
(define b (list 5 6))
(set-car! a b)


;; 3.12
(define (append x y)
  (if (null? x)
      y
      (cons (car x) (append (cdr x) y))))

(define (append! x y)
  (set-cdr! (last-pair x) y)
  x)
(define (last-pair x)
  (if (null? (cdr x))
      x
      (last-pair (cdr x))))
(define x (list 'a 'b))
(define y (list 'c 'd))
(define z (append x y))
z
x
y
(set-car! x 'y)
y
z
(cdr x)
(define w (append! x y))
w
(cdr x)
y
;; 3.13
(define (make-cycle x)
  (set-cdr! (last-pair x) x)
  x)

(define z (make-cycle (list 'a 'b 'c)))
z
(last-pair z)

;; 3.14
(define v (list 'a 'b 'c 'd))

(define (mystery x)
  (define (loop x y)
    (if (null? x)
        y
        (let ((temp (cdr x)))
          (set-cdr! x y)
          (loop temp x))))
  (loop x '()))
(define w (mystery v))
w
v
(set-car! v 'c)
v
w
;; (loop v '())
;; temp (list b c d)
;; v (a)
;; loop (list b c d) (a)
;; temp (list c d)
;; (b a) <- x
;; loop (list c d) (list b a))

;; 3.15

(define x (list 'a 'b))
(define z1 (cons x x))

(define z2 (cons (list 'a 'b) (list 'a 'b)))

(define (set-to-wow! x)
  (set-car! (car x) 'wow)
  x)

z1
z2

(set-to-wow! z1)

(set-to-wow! z2)

(define (count-pairs x)
  (if (not (pair? x))
      0
      (+ (count-pairs (car x))
         (count-pairs (cdr x))
         1)))


(define x (cons 1 '()))
(define y (cons 2 x))
(define z (cons 3 y))
(count-pairs z)


(define x (cons 1 '()))
(define y (cons x x))
(define z (cons y 99999))
(count-pairs z)


(define x (cons 1 '()))
(define y (cons x x))
(define z (cons y y))
(count-pairs z)


(define x (cons 1 '()))
(define y (cons 2 x))
(define z (cons 3 y))
(set-cdr! x z)
(count-pairs z)


;; Ex 3.17
(define l '())
(append 1 l)
l
(append  (list (cons 1 2)) '())
(append (list (cons 1 2)) (append  (list (cons 1 2)) '()))

(define (include? item positions)
  (if (null? positions)
      false
      (or (equal? item (car positions)) (include? item (cdr positions)))))

(define (count-uniq-pairs x checked)
  (if (or (not (pair? x)) (include? x checked))
      0
      (let ((checked (cons x checked)))
        (if (equal? (car x) (cdr x))
            (+ (count-uniq-pairs (car x) checked) 1)
            (+ (count-uniq-pairs (car x) checked)
               (count-uniq-pairs (cdr x) checked)
               1)))))


(define x (cons 1 '()))
(define y (cons 2 x))
(define z (cons 3 y))
(count-uniq-pairs z '())


(define x (cons 1 '()))
(define y (cons x x))
(define z (cons y 99999))
(count-uniq-pairs z '())


(define x (cons 1 '()))
(define y (cons x x))
(define z (cons y y))
(count-uniq-pairs z '())


(define x (cons 1 '()))
(define y (cons 2 x))
(define z (cons 3 y))
(set-cdr! x z)
(count-uniq-pairs z '())

;; 3.18

(define (make-cycle x)
  (set-cdr! (last-pair x) x)
  x)

(define z (make-cycle (list 'a 'b 'c)))
z
(define z1 (cons '999999 z))
z1
(cdr (cdr z1))

(cdr z)
(define temp (cdr z))
(set-cdr! z 'wowow)
z ;; (a)
temp ;; (b c a)

;; если все это проделать на списке без цикла

(define y (list 'a 'b 'c))
(define y1 (cons '999999 y))
y1
(define temp (cdr y))
(set-cdr! y '())
y ;; (a)
temp ;; value (b c)


(define z2 (make-cycle (list 'a 'b 'c))) ;; эквивалентен z только создается отдельно
(define (break-cycle l)
  (let ((temp (cdr l)))
    (set-cdr! l '())
    (cons l temp)))

;; если при сверке два раза не повторится temp, то это точно не цикл.
;; если он повторился, то возможно там все элементы с одинаковым значением.


(define (make-cycle x)
  (set-cdr! (last-pair x) x)
  x)

(define z (make-cycle (list 'a 'b 'c)))


(same? (cdr z) (cdr (cdr z)))
;; кролик бежит в два раза быстрее черепахи, находим точку когда их позиция повторится
(define (repeat-point? tl hl)
  (display (car tl))
  (display (car hl))
  (if (eq? (car tl) (car hl))
      hl
      (same-point? (cdr tl) (cdr (cdr hl)))))

(define hl (repeat-point? (cdr z) (cdr (cdr z))))
hl


(define z '(a a b c a a b c a a b c a a b c a a b c a a a))
(define z '(b c a b c a b c a b c a b c a b c a))

(define (start-repeat? tl hl index)
  (if (eq? (car tl) (car hl))
      (cons tl index)
      (start-repition?? (cdr tl) (cdr (hl) (+ index 1)))))
(define res2 (start-repeat? z hl 0))
(newline)
(display (cdr res2))

(define (period? tl hl period)
  (if (eq? (car tl) (car hl))
      period
      (period? tl (cdr hl) (+ period 1))))

(define period (period? (car res2) (cdr (car res2)) 1))
period
;; теперь они бегут с одинаковой скоростью, черепаха с начальной позиции
;; а кролик продолжает

;; # Find the position μ of first repetition.
;;     mu = 0
;;     tortoise = x0
;;     while tortoise != hare:
;;         tortoise = f(tortoise)
;;         hare = f(hare)   # Hare and tortoise move at same speed
;;         mu += 1

(define (floyed l)
  ;; кролик бежит в два раза быстрее черепахи
  ;; если цикл есть то он обагнав его на круг встретятся в некоторой точке
  ;; запоминаем позицию кролика
  (define (same-point? tl hl)
    (if (eq? (car tl) (car hl))
        (begin
          (display (car tl))
          (display (car hl))
          hl
          )
        (same-point? (cdr tl) (cdr (cdr hl)))))
  ;; далее бегут с равной минимальной скоростью
  ;; кролик начинает с позиции предведущей встречи, а черепаха сначала
  ;; как только их позиции совпадут это будет означать что 
  (define (start-repeat? tl hl index)
    (if (eq? (car tl) (car hl))
        (cons tl index)
        (start-repeat? (cdr tl) (cdr hl) (+ index 1))))
  ;; черепаха остается на месте а кролик бежит с минимальной скоростью
  (define (period? tl hl period)
    (if (eq? (car tl) (car hl))
        period
        (period? tl (cdr hl) (+ period 1))))

  (define hl (same-point? (cdr l) (cdr (cdr l))))
  ;; (display hl)
  ;; (c b a b c a c b)
  (define start-repeat (start-repeat? l hl 0))
  (define tl (car start-repeat))
  (define index (cdr start-repeat))

  (define period (period? tl (cdr tl) 1))

  (cons index period))

(define (make-cycle2 x)
  (set-cdr! (last-pair x) (cdr (cdr x)))
  x)

(define z (make-cycle (list 'a 'b 'c 'd 'e 'f 'g 'h)))
z ;Value: (a b . #0=(c d e f . #0#))
(floyed z)
;; бегут с разными скоростями
;; b c   b c
;; c e   c e
;; d c   d g
;; e e   e c
;;       f e
;;       g g
;; далее бегут с одной скоростью, как только они опять встрется эта точка будет означать начало цикла, потомучто

до этого кролик промежал расстояние до цикла + цикл + от начала цикла до точки встречи
а черепаха расстояние до цикла + от начала цикла до точки

можно показать что дистанция v в точке встречи кратна длине цикла.


так как они теперь идут с равными скоростями
то пока черепаха дойдет до начала цикла, кролик будут должен завершить еще один круг и тоже окажется в начале цикла


;; Ex 3.20

(define (cons x y)
  (define (dispatch m)
    (cond ((eq? m 'car) x)
          ((eq? m 'cdr) y)
          (else (error "Неопределенная операция -- CONS" m))))
  dispatch)
(define (car z) (z 'car))
(define (cdr z) (z 'cdr))

(define (cons x y)
  (define (set-x! v) (set! x v))
  (define (set-y! v) (set! y v))
  (define (dispatch m)
    (cond ((eq? m 'car) x)
          ((eq? m 'cdr) y)
          ((eq? m 'set-car!) set-x!)
          ((eq? m 'set-cdr!) set-y!)
          (else (error "Неопределенная операция -- CONS" m))))
  dispatch)

(define (car z) (z 'car))
(define (cdr z) (z 'cdr))
(define (set-car! z new-value)
  ((z 'set-car!) new-value)
  z)
(define (set-cdr! z new-value)
  ((z 'set-cdr!) new-value)
  z)

(define x (cons 1 2))
(car x)
(cdr x)
(define z (cons x x))
(car (car z))
(set-car! (cdr z) 17)
(car x) ;; 17


;; Queue FIFO

(define q (make-queue))
(insert-queue! q 'a) 	a
(insert-queue! q 'b) 	a b
(delete-queue! q) 	b
(insert-queue! q 'c) 	b c
(insert-queue! q 'd) 	b c d
(delete-queue! q) 	c d

(define (front-ptr queue) (car queue))
(define (rear-ptr queue) (cdr queue))
(define (set-front-ptr! queue item) (set-car! queue item))
(define (set-rear-ptr! queue item) (set-cdr! queue item))

(define (empty-queue? queue) (null? (front-ptr queue)))

(define (make-queue) (cons '() '()))

(define (front-queue queue)
  (if (empty-queue? queue)
      (error "FRONT called with an empty queue" queue)
      (car (front-ptr queue))))

(define (insert-queue! queue item)
  (let ((new-pair (cons item '())))
    (cond ((empty-queue? queue)
           (set-front-ptr! queue new-pair)
           (set-rear-ptr! queue new-pair)
           queue)
          (else
           (set-cdr! (rear-ptr queue) new-pair)
           (set-rear-ptr! queue new-pair)
           queue))))

(define (delete-queue! queue)
  (cond ((empty-queue? queue)
         (error "DELETE! called with an empty queue" queue))
        (else
         (set-front-ptr! queue (cdr (front-ptr queue)))
         queue)))


;; Ex. 3.21.

(define q1 (make-queue))
q1
(insert-queue! q1 'a)
((a) a)
(insert-queue! q1 'b)
((a b) b)
(delete-queue! q1)
((b) b)
(delete-queue! q1)
(() b)

(insert-queue! q1 'c)
(define (print-queue queue)
  (car queue))
(print-queue q1)
(delete-queue! q1)

(define a '(a))
(define l '(a b c))


(define c (append l a))
c
(set-cdr! a '(b))
a
c

;; Ex. 3.22

(define a '())
(define b '())

;; 
(define (front-ptr queue) (car queue))
(define (rear-ptr queue) (cdr queue))
(define (set-front-ptr! queue item) (set-car! queue item))
(define (set-rear-ptr! queue item) (set-cdr! queue item))
;; вот это мы заменили на локальные переменные

(define (empty-queue? queue) (null? (front-ptr queue)))


(define (make-queue)
  (let ((front-ptr '())
        (rear-ptr '()))

    (define (empty-queue?) (null? front-ptr))
    (define (front-queue)
      (if (empty-queue?)
          (error "FRONT вызвана с пустой очередью")
          (car front-ptr)))

    (define (insert item)
      (let ((new-pair (cons item '())))
        (cond ((empty-queue?)
               (set! front-ptr new-pair)
               (set! rear-ptr new-pair)
               front-ptr)
              (else
               (set-cdr! rear-ptr new-pair)
               (set! rear-ptr new-pair)
               front-ptr))))

    (define (delete)
      (cond ((empty-queue?)
             (error "DELETE! вызвана с пустой очередью"))
            (else
             (set! front-ptr (cdr front-ptr))
             front-ptr)))

    (define (dispatch m)
      (cond ((eq? m 'delete) delete)
            ((eq? m 'insert) insert)))
    dispatch))

(define q (make-queue))
((q 'insert) 'a)
((q 'insert) 'b)
((q 'insert) 'c)
((q 'delete))
q
q

(insert-queue! q 'a) 	a
(insert-queue! q 'b) 	a b
(delete-queue! q) 	b


;; Ex. 3.23

(define (make-deque)
  (let ((front-ptr '())
        (rear-ptr '()))

    (define (empty-queue?) (null? front-ptr))
    (define (front-queue)
      (if (empty-queue?)
          (error "FRONT вызвана с пустой очередью")
          (car front-ptr)))

    (define (rear-insert item)
      (let ((new-pair (cons item '())))
        (cond ((empty-queue?)
               (set! front-ptr new-pair)
               (set! rear-ptr new-pair)
               front-ptr)
              (else
               (set-cdr! rear-ptr new-pair)
               (set! rear-ptr new-pair)
               front-ptr))))

    (define (delete)
      (cond ((empty-queue?)
             (error "DELETE! вызвана с пустой очередью"))
            (else
             (set! front-ptr (cdr front-ptr))
             front-ptr)))

    (define (dispatch m)
      (cond ((eq? m 'delete) delete)
            ((eq? m 'insert) insert)))
    dispatch))



(define a (cons 'a '()))
a
(define tmp (cons '() '()))

(define b (cons 'b '()))


(set-cdr! a tmp)
(set-car! tmp b)
(set-cdr! tmp a)
(cdr (cdr a))

(set-cdr! a b)
a
b
(set-cdr! b a)
(car a)
(car (cdr a))
(car (cdr (cdr a)))


(define l (cons 'a b))
l
(set-cdr! b a)
l


(cons 'a (cons '() (cons 'b '())))
(

(define l (list a tmp b))
l

(cdr b)

(set-cdr! tmp a)
(set-cdr! a tmp)
(set-car! tmp 'b)
a

(define a (list 'a '() '()))

(define b (list 'b '() '()))

(set-cdr! a b)



(define (make-deque)
  )
(define (empty-deque? d)
  )
(define (front-deque d)
  )
(define (rear-deque d)
  )
(define (front-insert-deque! d item)
  )
(define (rear-insert-deque! d item)
  )



(define (make-deque)
  (let ((front-ptr '())
        (rear-ptr '()))

    (define (empty-queue?) (or (null? front-ptr) (null? rear-ptr)))
    (define (front-queue)
      (if (empty-queue?)
          (error "FRONT вызвана с пустой очередью")
          (car front-ptr)))

    (define (rear-insert item)
      (let ((new-list (cons (cons item '()) '())))
        (cond ((empty-queue?)
               (set! front-ptr new-list)
               (set! rear-ptr front-ptr)
               front-ptr)
              (else
               (set-cdr! (car new-list) rear-ptr)
               (set-cdr! rear-ptr new-list)
               (set! rear-ptr new-list)
               front-ptr))))

    (define (front-insert item)
      (let ((new-list (cons (cons item '()) '())))
        (cond ((empty-queue?)
               (set! front-ptr new-list)
               (set! rear-ptr front-ptr)
               front-ptr)
              (else
               (set-cdr! new-list front-ptr)
               (set-cdr! (car front-ptr) new-list)
               (set! front-ptr new-list)
               front-ptr))))

    (define (front-delete)
      (cond ((empty-queue?)
             (error "DELETE! вызвана с пустой очередью"))
            (else
             (set! front-ptr (cdr front-ptr))
             (when (not (null? front-ptr))
               (set-cdr! (car front-ptr) '()))
             front-ptr)))

    (define (rear-delete)
      (cond ((empty-queue?)
             (error "DELETE! вызвана с пустой очередью"))
            (else
             (set! rear-ptr (cdr (car rear-ptr)))
             (when (not (null? rear-ptr))
               (set-cdr! rear-ptr '()))
             rear-ptr)))

    (define (rear)
      rear-ptr)
    (define (front)
      front-ptr)

    (define (dispatch m)
      (cond ((eq? m 'front-delete) front-delete)
            ((eq? m 'rear-delete) rear-delete)
            ((eq? m 'rear) rear)
            ((eq? m 'front) front)
            ((eq? m 'rear-insert) rear-insert)
            ((eq? m 'front-insert) front-insert)
            ))
    dispatch))

(define q (make-deque))
((q 'rear-insert) 'a)
((q 'rear-insert) 'b)
((q 'rear-insert) 'c)
((q 'rear-insert) 'd)

q

(define re ((q 'rear)))
(define fr ((q 'front)))
((q 'front))
re
fr
(cdr re)

((q 'front-delete))

((q 'rear-delete))


(define a (cons (cons 'a '()) '()))

(define b (cons (cons 'b '()) '()))
(define c (cons (cons 'c '()) '()))
(define d (cons (cons 'd '()) '()))

;; (set-cdr! (car newlist) (rear-ptr deque))
;; (set-cdr! (rear-ptr deque) newlist)

a
(set-cdr! (car b) a)
(set-cdr! a b)
a
b

(set-cdr! (car c) b)
(set-cdr! b c)
(car c)
c
(set-cdr! (car c) '())
(car c)

(set-cdr! (car d) b)
(set-cdr! b d)
(cdr (car d))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; tables
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (lookup key table)
  (let ((record (assoc key (cdr table))))
    (if record
        (cdr record)
        false)))
(define (assoc key records)
  (cond ((null? records) false)
        ((equal? key (caar records)) (car records))
        (else (assoc key (cdr records)))))

(define (insert! key value table)
  (let ((record (assoc key (cdr table))))
    (if record
        (set-cdr! record value)
        (set-cdr! table
                  (cons (cons key value) (cdr table)))))
  'ok)

(define (make-table)
  (list '*table*))

(define table (make-table))
(insert! 'a 1 table)
table


;; Ex 3.24

(define (assoc key records)
  (cond ((null? records) false)
        ((equal? key (caar records)) (car records))
        (else (assoc key (cdr records)))))

(define (make-table)
  (let ((local-table (list '*table*)))

    (define (lookup key-1)
      (let ((record (assoc key-1 (cdr local-table))))
        (if record
            (cdr record)
            false)))

    (define (insert! key-1 value)
      (let ((record (assoc key-1 (cdr local-table))))
        (if record
            (set-cdr! record value)
            (set-cdr! local-table
                      (cons (cons key-1 value) (cdr local-table))))
        'ok))
    (define (dispatch m)
      (cond ((eq? m 'lookup-proc) lookup)
            ((eq? m 'insert-proc!) insert!)
            (else (error "Unknown operation -- TABLE" m))))
    dispatch
    )
  )

(define tolerance 0.1)

(define operation-table (make-table))
(define get (operation-table 'lookup-proc))
(define put (operation-table 'insert-proc!))

(put 1 1.2)
(put 1.05 3)
(put 'b 3)
(get 'b)
operation-table


;; Ex 3.25


(define (make-table)
  (let ((local-table (list '*table*)))
    (define (print)
      (display local-table))

    (define (assoc key records)
      (cond ((null? records) false)
            ((equal? key (caar records)) (car records))
            (else (assoc key (cdr records)))))

    (define (lookup key . keys)
      (define (sub-lookup table key keys)
        (let ((record (assoc key (cdr table))))
          (if record
              (if (null? keys)
                  (cdr record)
                  (sub-lookup record (car keys) (cdr keys)))
              false))

        )
      (sub-lookup local-table key keys)
      )

    (define (insert! value key . keys)
      (define (sub-insert table value key keys)
        (let ((record (assoc key (cdr table))))
          (if record
              (if (null? keys)
                  (set-cdr! record value)
                  (sub-insert record value (car keys) (cdr keys)))
              (if (null? keys)
                  (set-cdr! table (cons (cons key value) (cdr table)))
                  (let ((sub-table (list key)))
                    (set-cdr! table (cons sub-table (cdr table)))
                    (sub-insert sub-table value (car keys) (cdr keys))
                    )
                  )
              ))
          )

      (sub-insert local-table value key keys)
      'ok)

    (define (dispatch m)
      (cond ((eq? m 'lookup-proc) lookup)
            ((eq? m 'insert-proc!) insert!)
            ((eq? m 'print) print)
            (else (error "Unknown operation -- TABLE" m))))
    dispatch
    )
  )


(define operation-table (make-table))
(define get (operation-table 'lookup-proc))
(define put (operation-table 'insert-proc!))
(define print (operation-table 'print))
(print)
(put 1 'test)
(put 1 'test)
(put 2 'test 'two)
(get 'test)
(get 'test 'two)

;; Ex 3.26

;; делаем дерево которое будет хранить помимо веса вершины еще значение
(define (entry tree) (car tree))
(define (left-branch tree) (cadr tree))
(define (right-branch tree) (caddr tree))
(define (value-tree tree) (cadddr tree))
(define (make-tree entry left right value)
  (list entry left right value))


;; нужно переписать на новую структуру дерева
(define (partial-tree elts n)
  (if (= n 0)
      (cons '() elts)
      (let ((left-size (quotient (- n 1) 2)))
        (let ((left-result (partial-tree elts left-size)))
          (let ((left-tree (car left-result))
                (non-left-elts (cdr left-result))
                (right-size (- n (+ left-size 1))))
            (let ((this-entry (car non-left-elts))
                  (right-result (partial-tree (cdr non-left-elts)
                                              right-size)))
              (let ((right-tree (car right-result))
                    (remaining-elts (cdr right-result)))
                (cons (make-tree this-entry left-tree right-tree)
                      remaining-elts))))))))

(define (lookup given-key tree)
  (cond ((null? tree) false)
        ((equal? given-key (entry tree)) (entry tree))
        ((> given-key (entry tree)) (lookup given-key (right-branch tree)))
        ((< given-key (entry tree)) (lookup given-key (left-branch tree)))))


(define (make-table)
  ;; добавляем тег 'bin
  (define (empty-bin)
    (list '*bin*))
  (define (sub-bin? record)
    (equal? (car (value-tree record)) '*bin*))
  (define (adjoin-set x set value)
    (cond ((null? set) (make-tree x '() '() value))
          ((= x (entry set)) set)
          ((< x (entry set))
           (make-tree (entry set)
                      (adjoin-set x (left-branch set) value)
                      (right-branch set)
                      (value-tree set)
                      ))
          ((> x (entry set))
           (make-tree (entry set)
                      (left-branch set)
                      (adjoin-set x (right-branch set) value)
                      (value-tree set)))))

  (let ((local-bin (empty-bin))) ;; вместо списка бинарное дерево
    (define (print)
      (display local-bin))

    (define (assoc-bin given-key tree)
      (cond ((null? tree) false)
            ((equal? given-key (entry tree)) tree)
            ((> given-key (entry tree)) (assoc-bin given-key (right-branch tree)))
            ((< given-key (entry tree)) (assoc-bin given-key (left-branch tree)))))

    (define (lookup key . keys)
      (define (sub-lookup bin key keys)
        (let ((record (assoc-bin key (cdr bin))))
          (if record
              (if (null? keys)
                  (value-tree record)
                  (if (sub-bin? record)
                      (sub-lookup (value-tree record) (car keys) (cdr keys))
                      false))
              false))

        )
      (sub-lookup local-bin key keys)
      )

    (define (insert! value key . keys)
      (define (sub-insert bin value key keys)
        (let ((record (assoc-bin key (cdr bin))))
          (display record)
          (if record
              (if (null? keys)
                  (set-cdr! record (list (left-branch record)
                                         (right-branch record)
                                         value))
                  (sub-insert (cadddr record) value (car keys) (cdr keys)))
              (if (null? keys)
                  (set-cdr! bin (adjoin-set key (cdr bin) value))
                  (let ((sub-bin (empty-bin)))
                    (set-cdr! bin (adjoin-set key (cdr bin) sub-bin))
                    (sub-insert sub-bin value (car keys) (cdr keys))
                    )
                  )
              ))
          )
      (sub-insert local-bin value key keys)
      'ok)

    (define (dispatch m)
      (cond ((eq? m 'lookup-proc) lookup)
            ((eq? m 'insert-proc!) insert!)
            ((eq? m 'print) print)
            (else (error "Unknown operation -- TABLE" m))))
    dispatch
    )
  )


(define operation-table (make-table))
(define get (operation-table 'lookup-proc))
(define put (operation-table 'insert-proc!))
(define print (operation-table 'print))
(print)
(put 'a 1 2)
(put 'b 1 3)
(put 'b 2)
(put 1 'test)
(put 2 'two)
(get 1)
(get 2)
(get 1 2)
(get 1 3)
(get 2)


;; Ex. 3.27.

(define (lookup key table)
  (let ((record (assoc key (cdr table))))
    (if record
        (cdr record)
        false)))
(define (assoc key records)
  (cond ((null? records) false)
        ((equal? key (caar records)) (car records))
        (else (assoc key (cdr records)))))
(define (insert! key value table)
  (let ((record (assoc key (cdr table))))
    (if record
        (set-cdr! record value)
        (set-cdr! table
                  (cons (cons key value) (cdr table)))))
  'ok)
(define (make-table)
  (list '*table*))



(define (fib n)
  (cond ((= n 0) 0)
        ((= n 1) 1)
        (else (+ (fib (- n 1))
                 (fib (- n 2))))))

(define memo-fib
  (memoize (lambda (n)
             (cond ((= n 0) 0)
                   ((= n 1) 1)
                   (else (+ (memo-fib (- n 1))
                            (memo-fib (- n 2))))))))

(define (memoize f)
  (let ((table (make-table)))
    (display table)
    (lambda (x)
      (let ((previously-computed-result (lookup x table)))
        (or previously-computed-result
            (let ((result (f x)))
              (display result)
              (newline)
              (insert! x result table)
              result))))))

(fib 6)
(memo-fib 6)
((memoize fib) 6)


;; 3.3.4. Имитация цифровых схем

(define (half-adder a b s c)
  (let ((d (make-wire)) (e (make-wire)))
    (or-gate a b d)
    (and-gate a b c)
    (inverter c e)
    (and-gate d e s)
    'ok))

(define (full-adder a b c-in sum c-out)
  (let ((s (make-wire))
        (c1 (make-wire))
        (c2 (make-wire)))
    (half-adder b c-in s c1)
    (half-adder a s sum c2)
    (or-gate c1 c2 c-out)
    'ok))


(define (inverter input output)
  (define (invert-input)
    (let ((new-value (logical-not (get-signal input))))
      (after-delay inverter-delay
                   (lambda ()
                     (set-signal! output new-value)))))
  (add-action! input invert-input)
  'ok)
(define (logical-not s)
  (cond ((= s 0) 1)
        ((= s 1) 0)
        (else (error "Invalid signal" s))))


(define (and-gate a1 a2 output)
  (define (and-action-procedure)
    (let ((new-value
           (logical-and (get-signal a1) (get-signal a2))))
      (after-delay and-gate-delay
                   (lambda ()
                     (set-signal! output new-value)))))
  (add-action! a1 and-action-procedure)
  (add-action! a2 and-action-procedure)
  'ok)

(define (logical-and s1 s2)
  (if (and (= s1 1) (= s2 1))
      1
      0))

;; Ex. 3.28.

(define (or-gate a1 a2 output)
  (define (or-action-procedure)
    (let ((new-value
           (logical-or (get-signal a1) (get-signal a2))))
      (after-delay or-gate-delay
                   (lambda ()
                     (set-signal! output new-value)))))
  (add-action! a1 or-action-procedure)
  (add-action! a2 or-action-procedure)
  'ok)

(define (logical-or s1 s2)
  (if (or (= s1 1) (= s2 1))
      1
      0))

;; Ex. 3.29.

(or a1 a2) = (and (not a1) (not a2))

0 0 0      0 1       0 0 0
0 1 1      1 0       0 1 0
1 0 1                1 0 0
1 1 1                1 1 1

1
1
1
0
(not (and (not a1) (not a2)))
0
1
1
1

(define (or-gate a1 a2 output)
  (let ((na1 (make-wire))
        (na2 (make-wire))
        (andnot (make-wire)))
    (inverter a1 na1)
    (inventer a2 na2)
    (and-gate na1 na2 andnot)
    (inventer andnot output)
    'ok))

;; время задержки будет равно сумме
inventer-delay + inventer-delay + and-gate-delay + and-gate-delay + inventer-delay


;; Ex. 3.30.


(define (full-adder a b c-in sum c-out)
  (let ((s (make-wire))
        (c1 (make-wire))
        (c2 (make-wire)))
    (half-adder b c-in s c1)
    (half-adder a s sum c2)
    (or-gate c1 c2 c-out)
    'ok))


(define (riple-carry-adder A B S Cn)
  (define (iter-carry-adder A B S Cn res)
    (if (null? A)
        (cons res Cn)
        (begin
          (let ((Sn (car S)))
            (full-adder (car A) (car B) Cn Sn Cn)
            (iter-carry-adder (cdr A) (cdr B) (cdr S) Cn Cn (cons Sn res))
          ))))

  (iter-carry-adder A B S Cn '())
  )

;; время задержки равно n * full-adder
;; время задержки full-adder = half-adder + half-adder + or-gate
;; время задержки half-adder = 2 and + 1 or + 1 not


;; Представление проводов

(define (make-wire)
  (let ((signal-value 0) (action-procedures '()))
    (define (set-my-signal! new-value)
      (if (not (= signal-value new-value))
          (begin (set! signal-value new-value)
                 (call-each action-procedures))
          'done))
    (define (accept-action-procedure! proc)
      (set! action-procedures (cons proc action-procedures))
      (proc))
    (define (dispatch m)
      (cond ((eq? m 'get-signal) signal-value)
            ((eq? m 'set-signal!) set-my-signal!)
            ((eq? m 'add-action!) accept-action-procedure!)
            (else (error "Unknown operation -- WIRE" m))))
    dispatch))

(define (call-each procedures)
  (if (null? procedures)
      'done
      (begin
        ((car procedures))
        (call-each (cdr procedures)))))

(define (get-signal wire)
  (wire 'get-signal))
(define (set-signal! wire new-value)
  ((wire 'set-signal!) new-value))
(define (add-action! wire action-procedure)
  ((wire 'add-action!) action-procedure))

(define (after-delay delay action)
  (add-to-agenda! (+ delay (current-time the-agenda))
                  action
                  the-agenda))

(define (propagate)
  (if (empty-agenda? the-agenda)
      'done
      (let ((first-item (first-agenda-item the-agenda)))
        (first-item)
        (remove-first-agenda-item! the-agenda)
        (propagate))))

(define (probe name wire)
  (add-action! wire
               (lambda ()
                 (newline)
                 (display name)
                 (display " ")
                 (display (current-time the-agenda))
                 (display "  New-value = ")
                 (display (get-signal wire)))))


;; Реализация agenda
(define (make-agenda) (list 0))
(define (current-time agenda) (car agenda))
(define (set-current-time! agenda time)
  (set-car! agenda time))
(define (segments agenda) (cdr agenda))
(define (set-segments! agenda segments)
  (set-cdr! agenda segments))
(define (first-segment agenda) (car (segments agenda)))
(define (rest-segments agenda) (cdr (segments agenda)))
(define (empty-agenda? agenda)
  (null? (segments agenda)))


(define (add-to-agenda! time action agenda)
  (define (belongs-before? segments)
    (or (null? segments)
        (< time (segment-time (car segments)))))
  (define (make-new-time-segment time action)
    (let ((q (make-queue)))
      (insert-queue! q action)
      (make-time-segment time q)))
  (define (add-to-segments! segments)
    (if (= (segment-time (car segments)) time)
        (insert-queue! (segment-queue (car segments))
                       action)
        (let ((rest (cdr segments)))
          (if (belongs-before? rest)
              (set-cdr!
               segments
               (cons (make-new-time-segment time action)
                     (cdr segments)))
              (add-to-segments! rest)))))
  (let ((segments (segments agenda)))
    (if (belongs-before? segments)
        (set-segments!
         agenda
         (cons (make-new-time-segment time action)
               segments))
        (add-to-segments! segments))))
(define (remove-first-agenda-item! agenda)
  (let ((q (segment-queue (first-segment agenda))))
    (delete-queue! q)
    (if (empty-queue? q)
        (set-segments! agenda (rest-segments agenda)))))
(define (first-agenda-item agenda)
  (if (empty-agenda? agenda)
      (error "Agenda is empty -- FIRST-AGENDA-ITEM")
      (let ((first-seg (first-segment agenda)))
        (set-current-time! agenda (segment-time first-seg))
        (front-queue (segment-queue first-seg)))))


;; test

(define the-agenda (make-agenda))
(define inverter-delay 2)
(define and-gate-delay 3)
(define or-gate-delay 5)

(define input-1 (make-wire))
(define input-2 (make-wire))
(define sum (make-wire))
(define carry (make-wire))
(probe 'sum sum)
(probe 'carry carry)

(half-adder input-1 input-2 sum carry)
