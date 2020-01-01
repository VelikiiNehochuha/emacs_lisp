;;--------------------------------------------------------------------------------
;; Lisp Processing
;;--------------------------------------------------------------------------------
fill-column ;; 70
(fill-column) ;; void function fill-column
(+ 2 2) ;; cusror after + CxCe void-variable +, because it is function
(+ 2 'hello) ;; wrong type argument
(concat "abc" "def")
(substring "The fox into room" 4 7) ;; fox
(+ 2 fill-column)
(number-to-string (+ 2 fill-column))
(concat "The " (number-to-string (+ 2 fill-column)) " red foxes.")
(+) ;; 0
(*) ;; 1
(+ 3) ;; 3
(* 4) ;; 4
(+ 3 4 5) ;; 12
(* 3 4 5) ;; 60
'hello
(+ 2 'hello)
(message "The message appears in the echo area!")
(message "The name of the buffer is: %s" (buffer-name))
(message "The integer into string: %d" fill-column)
(message "He saw %d %s"
         (- fill-column 32)
         (concat "red "
                 (substring
                  "The quick brown foxes jumped" 16 21)
                 " leaping"))

(set 'flowers '(rose violet daisy buttercup))
flowers
(setq carnivores '(lion tiger leopard))
carnivores
(setq trees '(pine fir oak maple)
      herbivores '(gazelle antelope zebra))
trees
herbivores

(setq counter 0)
counter
(setq counter (+ counter 1))
counter

;; Exercises

;; 1) сгенерируйте сообщение об ошибке выполнив подходящий символ без скобок
+ ; void-variable +

;; 2) сгенерируйте сообщение об ошибке выполнив подходящий символ который внутри скобок
(fill-column) ; void-function fill-column

;; 3) создать счетчик который увеличивает на два за итерацию
(setq counter 0)
(setq counter (+ counter 2))
;; 4) напечатать сообщение в облать эхо
(message "Message %d" counter)

;;--------------------------------------------------------------------------------
;; Practicing Evaluation
;;--------------------------------------------------------------------------------

(buffer-name)
(buffer-file-name)
buffer-name ;; error


;; C-u C-x C-e show result into scratch buffer itself, (buffer-name)"*scratch*"

(current-buffer)
(other-buffer)
(switch-to-buffer (other-buffer))
(switch-to-buffer (other-buffer (current-buffer) t))
(buffer-size)
(point)
(point-min)
(point-max)
;; Exercises

;; Find a file with which you are working and move towards its middle. Find its buffer
;; name, file name, length, and your position in the file.
(goto-char (/ (buffer-size) 2)) ;; middle position (by char)
(goto-line (/ (buffer-s) 2))
(buffer-name)
(buffer-file-name)
(buffer-size)
(point)
(line-move -5)

;;--------------------------------------------------------------------------------
;; 3 How To Write Function Definitions
;;--------------------------------------------------------------------------------
(mark-whole-buffer)

(defun multiple-by-seven (number)
  "multiple NUMBER by seven."
  (* 7 number))
(multiple-by-seven 7) ;; 49

;; interactive function
(defun multiple-by-seven (number)
  "interactive multiple NUMBER by seven"
  (interactive "p")
  (message "The result is %d" (* 7 number)))
; C-u 8 M-x multiple-by-seven (or key)

;; Exercises
