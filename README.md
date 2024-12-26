<p align="center"><b>МОНУ НТУУ КПІ ім. Ігоря Сікорського ФПМ СПіСКС</b></p>
<p align="center">
<b>Звіт з лабораторної роботи 4</b><br/>
"Функції вищого порядку та замикання"<br/>
дисципліни "Вступ до функціонального програмування"
</p>
<p align="right"><b>Студент(-ка)</b>: Гнатюк Софія Валентинівна КВ-12</p>
<p align="right"><b>Рік</b>: 2024</p>

## Загальне завдання
Завдання складається з двох частин:
1. Переписати функціональну реалізацію алгоритму сортування з лабораторної роботи 3 з такими змінами: використати функції вищого порядку для роботи з послідовностями (де це доречно);
додати до інтерфейсу функції (та використання в реалізації) два ключових параметра: key та test , що працюють аналогічно до того, як працюють параметри з такими назвами в функціях, що працюють з послідовностями. При цьому key має виконатись мінімальну кількість разів.
2. Реалізувати функцію, що створює замикання, яке працює згідно із завданням за варіантом (див. п 4.1.2). Використання псевдо-функцій не забороняється, але, за можливості, має бути мінімізоване.
## Варіант першої частини <5>
Алгоритм сортування обміном №2 (із використанням прапорця) за незменшенням.
## Лістинг реалізації першої частини завдання
```lisp
(defun bubble-sort-functional (list &key (key #'identity) (test #'>=))
  (let* ((compare (lambda (a b) (funcall test (funcall key a) (funcall key b)))))
    (labels ((bubble-pass (lst last-index)
               (cond
                 ((or (null lst) (null (cdr lst))) lst)
                 ((<= last-index 0) lst)
                 (t
                  (let ((head (first lst))
                        (tail (cdr lst)))
                    (if (funcall compare head (first tail))
                        (cons (first tail) (bubble-pass (cons head (cdr tail)) (1- last-index)))
                        (cons head (bubble-pass tail (1- last-index))))))))
             (sort-helper (lst last-index)
               (if (or (null lst) (<= last-index 0))
                   lst
                   (sort-helper (bubble-pass lst last-index) (1- last-index)))))
      (sort-helper list (1- (length list))))))
```
### Тестові набори та утиліти першої частини
```lisp
(defun check-result (name function input expected)
  (let ((result (funcall function input)))
    (format t "~:[FAILED~;passed~]... ~a: Expected ~a, got ~a~%"
            (equal result expected) name expected result)))

(defun test-functions ()
  ;; Тести для bubble-sort-functional
  (check-result "Bubble Sort 1"
                (lambda (input) (bubble-sort-functional input))
                '(3 1 4 2)
                '(1 2 3 4))
  (check-result "Bubble Sort 2"
                (lambda (input) (bubble-sort-functional input :key #'abs :test #'<))
                '(-3 -1 4 -2)
                '(4 -3 -2 -1))
  (check-result "Bubble Sort 3"
                (lambda (input) (bubble-sort-functional input))
                '(1 1 4 3)
                '(1 1 3 4))
  (check-result "Bubble Sort 4"
                (lambda (input) (bubble-sort-functional input :key #'abs :test #'<))
                '(1 1 4 3)
                '(4 3 1 1))
```
### Тестування першої частини
```lisp
passed... Bubble Sort 1: Expected (1 2 3 4), got (1 2 3 4)
passed... Bubble Sort 2: Expected (4 -3 -2 -1), got (4 -3 -2 -1)
passed... Bubble Sort 3: Expected (1 1 3 4), got (1 1 3 4)
passed... Bubble Sort 4: Expected (4 3 1 1), got (4 3 1 1)
```
## Варіант другої частини <5>
Написати функцію propagator-fn , яка має один ключовий параметр — функцію comparator . propagator-fn має повернути функцію, яка при застосуванні в якості першого аргументу mapcar разом з одним списком-аргументом робить наступне: якщо елемент не "кращий" за попередній згідно з comparator , тоді він заміняється на значення попереднього, тобто "кращого", елемента. Якщо ж він "кращий" за попередній елемент згідно comparator, тоді заміна не відбувається. Функція comparator за замовчуванням має значення #'> .
```lisp
CL-USER> (mapcar (propagator-fn) '(1 2 3))
(1 2 3)
CL-USER> (mapcar (propagator-fn) '(3 1 4 2))
(3 3 4 4)
CL-USER> (mapcar (propagator-fn :comparator #'<) '(1 2 3))
(1 1 1)
```
## Лістинг реалізації другої частини завдання
```lisp
(defun propagator-fn (&key (comparator #'>))
  (let ((previous-value nil))
    (lambda (current-value)
      (if (or (null previous-value) (funcall comparator current-value previous-value))
          (setf previous-value current-value)
          previous-value))))
```
### Тестові набори та утиліти другої частини
```lisp
  (check-result "Propagator 1"
                (lambda (input) (mapcar (propagator-fn) input))
                '(3 1 4 2)
                '(3 3 4 4))
  (check-result "Propagator 2"
                (lambda (input) (mapcar (propagator-fn) input))
                '(1 2 3)
                '(1 2 3))
  (check-result "Propagator 3"
                (lambda (input) (mapcar (propagator-fn :comparator #'<) input))
                '(1 2 3)
                '(1 1 1)))
(test-functions)
```
### Тестування другої частини
```lisp
passed... Propagator 1: Expected (3 3 4 4), got (3 3 4 4)
passed... Propagator 2: Expected (1 2 3), got (1 2 3)
passed... Propagator 3: Expected (1 1 1), got (1 1 1)
```
