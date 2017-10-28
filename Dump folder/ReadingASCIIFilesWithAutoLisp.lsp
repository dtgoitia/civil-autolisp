; Source: https://www.theswamp.org/index.php?topic=21047.0
; Date of collection: 2017.08.22

;; reading ASCII files with AutoLISP

;;; Opening
;;
;; first you must 'open' a file and specifiy if you want to 'r' read it, 'w'
;; write to it or 'a' append it. In the file path your have a choice of using
;; either a single forward slash ( / ) or double backslashes ( \\ ).

;; (setq fo (open "c:\\temp\\afile.txt" "r"))

(setq fo (open "c:/temp/afile.txt" "r"))

;; which will return something like this
;; #<file "c:/temp/afile.txt">

;; think of 'fo' as a sort of link to the file you are going to read.

;; the contents of 'afile.txt'
;;
;; 1. line one
;; 2. line two
;; 3. line three
;; 4. line four
;; #  comment line followed by an empty line
;;
;; 5. line five
;;

;;; Reading
;;
;; now using other autolisp functions we can read the the contents of the file.
;; We have a choice of one character (read-char) or the whole line (read-line).
;; Let's look at the later first.
;;
;; We can store the first line of our fine in a variable using the 'read-line'
;; function.

(setq aline (read-line fo))

;; 'aline' now contains [ "1. line one" ] a string. From this point you can
;; parse the string anyway you like. To get the next line from our file we
;; simply issue the same statement again.

(setq aline (read-line fo))

;; 'aline' now contains [ "2. line two" ].

;; Once you're done with the file it's a good idea to 'close' it.

(close fo)

;; Reading a file one line at a time is not usually what we want to do. Most
;; of the time you will want to read each line of a file and examine it in a
;; loop. For that we will use the 'while' function.

(setq fo (open "c:/temp/afile.txt" "r"))

(while (setq aline (read-line fo))
       (princ (strcat aline "\n")
       )
(princ)

(close fo)

;; which will produce something like this.
;;
;; 1. line one
;; 2. line two
;; 3. line three
;; 4. line four
;; #  comment line followed by an empty line
;;
;; 5. line five

;; Once we reached the end of our file 'read-line' returned 'nil' which
;; stopped out 'while' loop.

;; Let us take it one step further and examine each line using the 'if' and
;; 'wcmatch' function to look for a certain string within each line of our
;; file.

(setq fo (open "c:/temp/afile.txt" "r"))

(while (setq aline (read-line fo))
       (if (wcmatch aline "*line t*")
         (princ (strcat "Found a match: " aline "\n"))
         ); if
       ); while
(princ)

(close fo)

;; which should return something like this.
;;
;; Found a match: 2. line two
;; Found a match: 3. line three

;; One more example and then we will move on. Remember that '#' we have in our
;; file? This time we will read the file but ignore that line when we come to
;; it along with any blank lines.

(setq fo (open "c:/temp/afile.txt" "r"))

(while (setq aline (read-line fo))
       (if (/= (substr aline 1 1) "#")
         (if (/= aline "")
           (princ (strcat aline "\n"))
           )
         )
       )
(princ)

(close fo)

;; which should return something like this.
;;
;; 1. line one
;; 2. line two
;; 3. line three
;; 4. line four
;; 5. line five

;; Now let us look at the 'read-char' function. 'read-char' works like
;; 'read-line' only it returns the ASCII character code (inetger) not a
;; string, for every character in our file. In order to get the string you
;; have to use the 'chr' function.

(setq fo (open "c:/temp/afile.txt" "r"))

(setq charcnt 0)

(while (read-char fo)
  (setq charcnt (1+ charcnt))
  )

(princ (strcat "I counted " (itoa charcnt) " characters\n"))

(princ)

(close fo)

;; that should return something like
;; "I counted 107 characters"

;; what about creating a list of character codes in our file.

(setq fo (open "c:/temp/afile.txt" "r"))

(while (setq achr (read-char fo))
  (setq chrlst (cons achr chrlst))
  )

(close fo)

;; that should produce something like this.
;; (10 101 118 105 102 32 101 110 105 108 32 46 .... )
