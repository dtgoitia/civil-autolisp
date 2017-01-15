# `(mapcar)` and `(lambda)` functions

## `(mapcar)` function

### `(mapcar)` in its most basic form

The `(mapcar)` function executes `something` at the given arguments:

```lisp
(mapcar something argument1 argument2 argument3...)
```
Just remember 2 things:
 1. `something` needs to be passed without being evaluated.
 2. `argument1`, `argument2`, etc. need to be lists.

Easy right? Let's se an example with pseudocode:
```lisp
(mapcar something (list 3))
```
In the example above, `mapcar` function will execute `something` at the `(list 3)`. And... what can this `something` be? It can be anything really. Any function you want.

Let's suppose we use the command `(princ)` to print a message in the command line. To do so, we need to pass the function symbol to `mapcar` without being _evaluated_, and for this we will use the `(quote)` function or the apostrophe symbol `'`. Both are equivalent. This functions supress the evaluation of the code. Read Lee Mac's article about _[The Apostrophe and the Quote Function](http://www.lee-mac.com/quote.html)_ for further reference, as the concept of _evaluation_ can be confusing if you are a beginer.
```lisp
(mapcar 'princ (list 3))
```
The above code will return a `3(3)` printed on the command line. In another words, the above code will return something very similar to having executed `(princ 3)`.

NOTE: I have used the apostrophe for convenience, but I could have used `(mapcar (quote princ) 3)` and AutoCAD would return the same.



### `(mapcar)` with multiple arguments
As said, `mapcar` can take multiple arguments. Actually, it can take as many arguments as the function `something` can take. Let's see an example:

We will create a function called `myFunction` which will take 2 arguments (`arg1` and `arg2`), multiply them, and return the result. Once `myFunction` defined, we will pass it to `mapcar` without being evaluated (as `'myFunction` or `(quote myFunction)`) and provide the 2 arguments `myFunction` needs to take. Remember, arguments needs to be lists.
```lisp
(defun myFunction ( arg1 arg2 )
  (* arg1 arg2)
)
(mapcar 'myFunction (list 3) (list 2))
```
If everything went right, the code above will return `(6)`. So the code above is equivalent to:
```lisp
(list (myFunction 3 2))
```
Okey. Right. And? What's your point? Be patient, you will understand everything on the next part.

### `(mapcar)`'s power
`(mapcar)` useful when you need to process big lists of data. Let's imagine we have a list with names (`listA`) and another list with surnames (`listB`) of 4 people and we want to create a unique list with the names and surnames together. We could run a `(foreach)`, or a `(while)`...but makes sense to use `(mapcar)` as you will see now:
:
```lisp
(defun joinNameAndSurname ( name surname )
  (strcat name " " surname)
)
(setq
  listA (list "Jane" "Ben" "Tim" "Anna")
  listB (list "Smith" "Jones" "Williams" "Taylor")
)
(mapcar 'joinNameAndSurname listA listB)
```
The command line will return us `("Jane Smith" "Ben Jones" "Tim Williams" "Anna Taylor")`. Obviously, you could have done this with thousands of people, or point coordinates... ;)

Everything clear? Let's jump know into the `(lambda)` function -`(mapcar)`'s perfect teammate.

## `(lambda)` function

When you want to use a function, you need to define it first, so some memory is being used for that purpose. If the function will be reused many times... no problem, but what if you just need the function once? Is it worth the effort of declaring the name and arguments...such a waste of creativity! xD

Here is where `(lambda)` function comes into play. The concept of having a temporary function (_temporary_ in the sense that it will not remain allocated in memory all the session) is something common to many programming languages such as C#, python, etc.

As shown in the examples above, you need to reference a function within `(mapcar)` so that is applied to the arguments. Let's substitute this functions for a `(lambda)` function:
```lisp
(mapcar
  '(lambda (a) (alert a))               ; function
  (list "Hi!" "I am a cheecky lambda!") ; arguments
);END mapcar
```
The code above will pass a temporary function to `(mapcar)`. This temporary function takes a single argumen (`a`) and executes the code inside. The code inside, in this case is `(alert a)`. This code could be more complex, but for the time begin, let's keep it simple. And to finish we pass to `(mapcar)` the values we want we want to use as arguments (as always, in a list format): `(list "Hi!" "I am a cheecky lambda!")`.

Last update: 2017.01.15
