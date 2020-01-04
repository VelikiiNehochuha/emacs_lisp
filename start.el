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

;; let expression

(let ((zebra "stripes")
      (tiger "fierce"))
  (message "One kind of animal has %s and another is %s."
           zebra tiger))

;; uninitialized variables in a let statement

(let ((birch 3)
      pine
      fir
      (oak 'some))
  (message
   "Here are %d variables with %s, %s, and %s value."
   birch pine fir oak))

;; the if special form

(if (> 5 4)
    (message "True."))

(defun type-of-animal (characteristic)
  "print message depending on CHARACTERISTIC.
If the characteristic is the string \"fierce\",
then warn of a tiger."
  (if (equal characteristic "fierce")
      (message "It is a tiger!") ;; if then
    (message "It is not fierce"))) ;; else condition

(type-of-animal "fierce")
(type-of-animal "stripes")

;; if else

(if (> 4 5)
    (message "4 > 5!")
  (message "wrong 4 < 5!"))

;; nil in emacs it is false, it is empty list
;; is not nil == true
(if 4
    'true
  'false)

(if nil
    'true
  'false)

;; save-excursion
;; C-SPC (set-mark-command)
;; C-x C-x (exchange-point-and-mark)
;; C-u C-SPC jump the cusror to a saved mark

(let (a)
  (save-excursion
    (goto-char (/ (buffer-size) 2))
    ))

(message "We are %d charecters into this buffer."
         (- (point)
            (save-excursion
              (goto-char (point-min))
              (point)
              )))

;; eval-last-sexp C-x C-e


;; Exercises
(defun double-number (number)
  "Double NUMBER."
  (* number 2))
(double-number 3)
(defun double-number-interactive (number)
  "Double NUMBER."
  (interactive "p")
  (message "The result is %d" (* number 2)))

(defun compare-fill-column (number)
  "Compare NUMBER with (fill-column)."
  (if (> number fill-column)
      (message "Number greater than fill-column.")
    (message "Number lower than fill-column.")))
(compare-fill-column 83)
fill-column ;; 70

;; 4 A Few Buffer-Related Functions

;; M-. (xref-find-definitions) M-, return to previous buffer

; C-h p emacs docs by topics

;;
;; M-< M-> beginning-of-buffer
(defun simplified-beginning-of-buffer ()
  "Move point to the beginnging of the buffer;
Leave mark at the previous position."
  (interactive)
  (push-mark)
  (goto-char (point-min)))

;; transient-mark-mode (select enable disable)
(transient-mark-mode nil)

;; old mark-whole-buffer emacs 22
(defun custom-mark-whole-buffer ()
  "try avoid push mark into real code."
  (interactive)
  (push-mark (point))
  (push-mark (point-max) nil t)
  (goto-char (point-min)))
;; now function def differ from emacs 22 check it (mark-whole-buffer)

;; 4.4 the definition of append-to-buffer (old emacs)

(defun custom-append-to-buffer (buffer start end)
  "Append to specified buffer the text of the region.
It is inserted into that buffer before its point.

When calling from a program, give three arguments:
BUFFER (or buffer name), START and END.
START and END specify the portion of the current buffer to be copied."
  (interactive
   (list (read-buffer "Append to buffer: " (other-buffer
                                            (current-buffer) t))
         (region-beginning) (region-end)))
  (let ((oldbuf (current-buffer)))
    (save-excursion
      (let* ((append-to (get-buffer-create buffer))
             (windows (get-buffer-window-list append-to t t))
             point)
        (set-buffer append-to)
        (setq point (point))
        (barf-if-buffer-read-only)
        (insert-buffer-substring oldbuf start end)
        (dolist (window windows)
          (when (= (window-point window) point)
            (set-window-point window (point))))))))

(append-to-buffer)
;; 4.6 Exercises

;; Write your own simplified-end-of-buffer function definition; then test it
;; to see whether it works.
(defun simplified-end-of-buffer ()
  "Go to the end of buffer."
  (interactive)
  (push-mark)
  (goto-char (point-max)))

(end-of-buffer)

;; Use if and get-buffer to write a function that prints a message telling you
;; whether a buffer exists.
(defun is-buffer-exist (buffer)
  "Check that buffer with BUFFER as name exist."
  (if (get-buffer buffer)
      (message "Buffer %s exists." buffer)
    (message "Buffer %s does not exist." buffer)))

(is-buffer-exist "does_not_exist.el")
(is-buffer-exist "start.el")

;; Using xref-find-definitions, find the source for the copy-to-buffer function.
(xref-find-definitions 'copy-to-buffer)


;; 5.1 The Definition of copy-to-buffer
(defun custom-copy-to-buffer (buffer start end)
  "Replace text into BUFFER with current buffer (region)."
  (interactive "BCopy to buffer: \nr")
  (let ((oldbuf (current-buffer)))
    (with-current-buffer (get-buffer-create buffer)
      (barf-if-buffer-read-only)
      (erase-buffer)
      (save-excursion
        (insert-buffer-substring oldbuf start end)))))

(copy-to-buffer)

;; 5.2 The Definition of insert-buffer

(insert-buffer)

;; old version
(defun custom-insert-buffer (buffer)
  "Insert after point the contents of BUFFER.
Puts mark after inserted text.
BUFFER may be a buffer or buffer name."
  (interactive "*bInsert buffer: ")
  ;; (or (bufferp buffer)
  ;;     (setq buffer (get-buffer buffer)))
  (if (not (bufferp buffer)) ;; true if argument is buffer, false if it is name of the buffer
      (setq buffer (get-buffer buffer)))
  (let (start end newmark)
        (save-excursion
          (save-excursion
            (set-buffer buffer)
            (setq start (point-min) end (point-max)))
          (insert-buffer-substring buffer start end)
          (setq newmark (point)))
        (push-mark newmark)))

;; alternative defun insert text into push-mark
;; avoid create additional variables
;; (push-mark
;;   (save-excursion
;;     (insert-buffer-substring (get-buffer buffer))
;;     (point)))
;;  nil

;; 5.3 Complete Definition of beginning-of-buffer

;; (defun beginning-of-buffer (&optional arg)
;;   "documentation..."
;;   (interactive "P")
;;   (or (is-the-argument-a-cons-cell arg)
;;       (and are-both-transient-mark-mode-and-mark-active-true)
;;       (push-mark))
;;   (let (determine-size-and-set-it)
;;     (goto-char
;;      (if-there-is-an-argument
;;         figure-out-where-to-go
;;       else-go-to
;;       (point-min))))
;;     do-nicety
(beginning-of-buffer)

(not (consp arg)) ;; забыли казать С-u 6 оставили ячейку пустой, это значит consp arg = nil

;; 5.5 optional Argument Exercise
;; Write an interactive function with an optional argument that tests whether its
;; argument, a number, is greater than or equal to, or else, less than the value of
;; fill-column, and tells you which, in a message. However, if you do not pass an
;; argument to the function, use 56 as a default value.

(defun compare-5-5 (&optional arg)
  "docs comparator with optional ARG
if arg missiong compare with 56, another case compare with fill column
"
  (interactive "p")
  (or (not (consp arg))
      (setq arg 56))
  (if (>= fill-column arg)
      (message "%s >= %s" fill-column arg)
    (message "%s < %s" fill-column arg)))

(compare-5-5 55)
(compare-5-5 70)
(compare-5-5) ;; optional only into interactive session

;; http://ergoemacs.org/emacs/elisp_optional_params.html

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 6 Narrowing and Widening
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(The key binding for narrow-to-region is
C-x n n.)
(The key binding for widen is C-x n w.)
(what-line)
(count-lines)

;; 6.1 The save-restriction Special Form
(save-excursion
  (save-restriction ;; зазграничивает скрытие
    body...))

(save-restriction
  (widen)
  (save-excursion
    body...))

; 6.2 what-line
(defun what-line ()
  "Print current line number (in the buffer) of point."
  (interactive)
  (save-restriction
    (widen)
    (save-excursion
      (beginning-of-line)
      (message "Line %d"
               (1+ (count-lines 1 (point)))))))

(what-line)
(beginning-of-line)
(count-lines 1 (point))

;; 6.3 Exercise with Narrowing
;; Write a function that will display the first 60 characters of the current buffer, even
;; if you have narrowed the buffer to its latter half so that the first line is inaccessible.
;; Restore point, mark, and narrowing. For this exercise, you need to use a whole pot-
;; 70
;;  Chapter 6: Narrowing and Widening
;; pourri of functions, including save-restriction, widen, goto-char, point-min,
;; message, and buffer-substring.
;; (buffer-substring is a previously unmentioned function you will have to inves-
;; tigate yourself; or perhaps you will have to use buffer-substring-no-properties
;; or filter-buffer-substring . . . , yet other functions. Text properties are a fea-
;; ture otherwise not discussed here. See Section “Text Properties” in The GNU
;; Emacs Lisp Reference Manual.)
;; Additionally, do you really need goto-char or point-min? Or can you write
;; the function without them?

(defun show-first-60 ()
  "Show first 60."
  (interactive)
  (save-restriction
    (widen)
    (save-excursion
      (message (filter-buffer-substring 1 60)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 7 car, cdr, cons: Fundamental Functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 7.1 car and cdr
(car '(rose violet daisy buttercup)) ; [0]
; Clearly, a more reasonable name for the car function would be first and this
; is often suggested.
(cdr '(rose violet daisy buttercup)); [1:]
; Clearly, a more reasonable name for cdr would be rest.
(car '((lion tiger cheetah)
       (gazelle antelope zebra)
       (whale dolphin seal)))
(cdr '((lion tiger cheetah)
       (gazelle antelope zebra)
       (whale dolphin seal)))

                                        ; 7.2 cons

(cons 'pine '(fir oak maple)) ; cons mean construct
; cons does not change an existing list, but creates a new one.

(cons 'buttercup ())
(cons 'daisy '(buttercup))
(cons 'violet '(daisy buttercup))

; 7.2.1 Find the Length of a List: length
(length '(buttercup))
(length '(daisy buttercup))
(length (cons 'violet '(daisy buttercup)))
; 7.3 nthcdr
(nthcdr 0 '(pine fir oak maple))
(nthcdr 3 '(pine fir oak maple))

;; Thus, if it were not defined in C for speed, the definition of nth would be:
(defun nth (n list)
  "Returns the Nth element of LIST.
N counts from zero. If LIST is not that long, nil is returned."
  (car (nthcdr n list)))

;; извлечь n-ый элемент списка
(nth 1 '("one" "two" "three"))

; 7.5 setcar
(setq animals '(antelope giraffe lion tiger))
(setcar animals 'cat) ; replace first element
animals
(setcdr animals '(tiger))
animals

;; 7.7 Exercise
;; Construct a list of four birds by evaluating several expressions with cons. Find out
;; what happens when you cons a list onto itself. Replace the first element of the list
;; of four birds with a fish. Replace the rest of that list with a list of other fish.

(setq birds (cons 'bird1 '(bird2 bird3 bird4)))
birds
(cons birds birds)

(setcar birds 'fish1)
birds
(setcdr birds '(fish2 fish3 fish4))
birds

;; 8 Cutting and Storing Text
;; 8.1 zap-to-char

(zap-to-char) ; interactive remove text from point to char and push it to kill-ring

(defun test-zap-to-char (arg char)
  "Kill up to and including ARG'th occurence of the CHR
Case is ignored if `case-fold-search' is non-nil in the current buffer.
Goes backward if ARG is negative; error if CHAR not found."
  (interactive "p\ncZap to chat: ")
  (if (char-table-p translation-table-for-input)
      (setq char (or (aref translation-table-for-input char) char)))
  (kill-region (point) (progn
                         (search-forward (char-to-string char)
                                         nil nil arg)
                         (point))))

; 8.1.4 The progn Special Form
(progn
  body...) ;; вернут результат последнего выражения

;; обработка ошибок, в случае исключения отрабатывает error-handler
(condition-case
    var
    bodyform
  error-handler...)

;; 8.2.2 Lisp macro
when it is if without else-part

;; 8.3 copy-region-as-kill

(defun copy-region-as-kill (beg end)
  "Save the region as if killed, but don't kill it.
In Transient Mark mode, deactivate the mark.
If `interprogram-cut-function' is non-nil, also save the text for a window
system cut and paste."
  (interactive "r")
  (if (eq last-command 'kill-region)
      (kill-append (filter-buffer-substring beg end) (< end beg))
    (kill-new (filter-buffer-substring beg end)))
  (if transient-mark-mode
      (setq deactivate-mark t)))

;; 8.7 Searching Exercises
;; Write an interactive function that searches for a string. If the search finds the
;; string, leave point after it and display a message that says “Found!”. (Do not
;; use search-forward for the name of this function; if you do, you will overwrite
;; the existing version of search-forward that comes with Emacs. Use a name
;; such as test-search instead.)

(defun test-search (arg word)
  "Search the WORD and move point and print 'Found'!"
  (interactive "p\nsSearch word: ")
  (condition-case nil
      (progn
        (search-forward word nil nil arg)
        (message "Found!"))
    (error (message "Not Found!"))))

;; Write a function that prints the third element of the kill ring in the echo area,
;; if any; if the kill ring does not contain a third element, print an appropriate
;; message.

kill-ring
(nth 1 kill-ring)

(defun third-el-kill-ring ()
  "print third el into kill ring"
  (interactive)
  (let (value)
    (setq value (nth 15 kill-ring))
    (if value
        (message "%s" value)
      (message "Empty"))))


;; 9 How Lists are Implemented
