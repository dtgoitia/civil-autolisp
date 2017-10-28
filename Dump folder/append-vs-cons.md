# `append` vs `cons`

## Summary

`(cons)` is much faster.
Just keep in mind this:
```autolisp
Command: (cons "a" (list 1 2 3))           ; returns list
("a" 1 2 3)
Command: (cons (list 1 2 3) "a")           ; returns doted pair
((1 2 3) . "a")
Command: (cons (list 1 2 3) (list 1 2 3))  ; returns list
((1 2 3) 1 2 3)
```

## Comment

When you `cons` two atoms, the function returns a dotted pair. If you `cons` an atom and a list, the function returns a list with the atom in the first position. Adding item in the front of a list, leads to lists which are created backwards, but this can be easily corrected using `reverse` (when it's necessary).

Interestingly enough, it is not possible to cons a list and an atom (in this particular order) because then the list will be treated as an atom and the function will build a dotted pair...

## Original article
Source: [Autodesk Community | ppend versus cons (09-28-2009)](https://forums.autodesk.com/t5/visual-lisp-autolisp-and-general/append-versus-cons/td-p/2563384)

I'm seeing a lot of people using append to add to lists. although append is a fine function it really eats up time

witness - `append`

```autolisp
(setq ctr 1 l1 (list) t1 (seconds))
(repeat 20000
  (setq
    l1 (append l1 (list ctr))
    ctr (1+ ctr)
  )
)
```

versus `cons` with a final `reverse`

```autolisp
(setq ctr 1)
(repeat 20000
  (setq
    l1 (cons ctr l1)
    ctr (1+ ctr)
    )
)
(setq l1 (reverse l1))
```

the time cost for append is astonishing
```autolisp
Command: (progn
(_> (defun seconds (/ s)
((_> (setq s (getvar "DATE" ))
((_> (* 86400.0 (- s (fix s)))
((_> )
(_> ; begin timing for append
(_> (setq ctr 1 l1 (list) t1 (seconds))
(_> (repeat 20000
((_> (setq l1 (append l1 (list ctr))
(((_> ctr (1+ ctr)
(((_> )
((_> )
(_> (setq t1 (- (seconds) t1))
(_> (setq l1 nil ctr 1)
(_> ; begin timing for cons with reverse
(_> (setq t2 (seconds) )
(_> (repeat 20000
((_> (setq l1 (cons ctr l1)
(((_> ctr (1+ ctr)
(((_> )
((_> )
(_> (setq l1 (reverse l1))
(_> (setq t2 (- (seconds) t2))
(_> (princ (strcat "\n Appending Requires " (rtos t1 2 5) " seconds"))
(_> (princ (strcat "\n Cons with reverse Requires " (rtos t2 2 15) "
seconds"))
(_> )

Appending Requires 37.73301 seconds
Cons with reverse Requires 0.093019008636475 seconds
```
it takes a lot of repeats to get a second for cons with reverse
```autolisp
Command: (progn
(_> (setq l1 nil ctr 1)
(_> ; begin timing for cons with reverse
(_> (setq t2 (seconds) )
(_> (repeat 2000000
((_> (setq l1 (cons ctr l1)
(((_> ctr (1+ ctr)
(((_> )
((_> )
(_> (setq l1 (reverse l1))
(_> (setq t2 (- (seconds) t2))
(_> (princ (strcat "\n Cons with reverse Requires " (rtos t2 2 15) "
seconds"))
(_> )

Cons with reverse Requires 0.984022021293640 seconds
```
there you have, 20,000 repeats with append requires over 37 seconds while
200,000 repeats with cons takes less thant over a second or 380+/- to 1
speed advanatge.

Jamie

## Comment to above

When you "cons" two atoms, the function returns a dotted pair. If you "cons"
an atom and a list, the function returns a list with the atom in the first
position. Adding item in the front of a list, leads to lists which are
created backwards, but this can be easily corrected using reverse (when it's
necessary).

Interestingly enough, it is not possible to cons a list and an atom (in this
particular order) because then the list will be treated as an atom and the
function will build a dotted pair..

Regards,
