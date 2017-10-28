Dieter,

Try the vl-catch-all-apply function.
Some examples:
(/ 10 2) > 5
(/ 10 0) > ; error: divide by zero

(vl-catch-all-apply '/ '(10 2)) > 5
(vl-catch-all-apply '/ '(10 0)) > #<%catch-all-apply-error%>

But the program continues as displayed in the following code:
(progn
(print (vl-catch-all-apply '/ '(10 2)))
(print (vl-catch-all-apply '/ '(10 0)))
(print (vl-catch-all-apply '/ '(10 5)))
)

And now for grread:
(progn
(setq i 1)
(while (< (setq i (1+ i)) 300)
(print (vl-catch-all-apply 'grread '(T)))
)
)

BTW:
I use i to end the loop. Otherwise I couldn't break the while-loop.

Alex

; URL (vl-catch-all-apply) https://knowledge.autodesk.com/search-result/caas/CloudHelp/cloudhelp/2016/ENU/AutoCAD-AutoLISP/files/GUID-E08CC2A6-787A-422F-8BD3-18812996794C-htm.html
