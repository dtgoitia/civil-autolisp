(defun c:xx ()
  ; Trigger
  (DT:AutoLoadFileFromCivilTemp "AutolispDependencyAnalisys.lsp")
  (vl-load-com)
  (c:LspTree)
  ; v0.0 - 2017.08.22 - First issue
  ; Author: David Torralba
  ; Last revision: 2017.08.22
)
; all string
; 2 rounds:
; ROUND 1: find declared functions and it's dependecies and arguments
;   if "(":
;      T: pass all remaining string and
;         if "(defun "
;           get function name until " " is found
;           get arguments from "(" to "/" or ")"
;           if ends in "/" omit from "/" to ")"
;           if ends in ")" don't omit anything
;           carry on until new "(defun "
;         F: carry on with next character
;      F: carry on with next character

; ROUND 2: find wrongly called functions (too little/many arguments passed)

(defun c:LspTree ( / *error* directory outputFile data )
  (defun *error* (msg)
    (vl-bt)
  )

  (CleanCommandLine)
  ; Handle user input
  (setq directory "C:/Users/davidt/projects/civil-autolisp-TEMP/LspTreeTestDirectorySmall")
  (setq i 0)
  ; (setq directory "C:/Users/davidt/projects/civil-autolisp-TEMP/LspTreeTestDirectory")
  ; (setq directory "C:/Users/davidt/projects/civil-autolisp-TEMP")
  (setq outputFile "C:/Users/davidt/projects/civil-autolisp-TEMP/LspTreeReport.txt")

  ; Get info
  (setq data (DT:LspTree directory))
  (if data
    (princ (strcat "\ndata = " (vl-prin1-to-string data)))
    (DT:Error 'c:LspTree "data = nil")
  );END if
  (foreach fileData data
    (princ "\n.\n.\n")
    (princ (nth 0 fileData))
    (if (> (length fileData) 1)
      (foreach func fileData
        (princ "\n\t\t")
        (if (= 'list (type func))
          (foreach a func
            (princ "\n\t\t\t\t")
            (princ a)
          );END foreach
          (princ func)
        );END if
      );END foreach
    );END if
  );END foreach
  ; (foreach fileData data
  ;   (princ "\n.\n.\n")
  ;   (princ (nth 0 fileData))
  ;   (if (> (length fileData) 1)
  ;     (foreach func (nth 1 fileData)
  ;       (princ "\n\t\t")
  ;       (princ (nth 0 func))
  ;       (if (> (length func) 1)
  ;         (foreach a (nth 1 func)
  ;           (princ "\n\t\t\t")
  ;           (princ a)
  ;         );END foreach
  ;       );END if
  ;     );END foreach
  ;   );END if
  ; );END foreach


  ; Stringify data
  ;

  ; Export info
  ; write to outputFile

  (princ)
  ; v0.0 - 2017.08.22 - First issue
  ; Author: David Torralba
  ; Last revision: 2017.08.22
)
(defun DT:LspTree ( directory / fileList return )
  ; Get all lsp files' functions and its function dependencies

  ; Find all lsp files
  (setq fileList (DT:LspTreeGetLspFilePaths directory))

  ; Find file defined functions and its dependencies
  (setq
    return
    (mapcar
      'DT:LspTreeGetFunctionsAndDependencies
      fileList
    );END mapcar
  );END setq

  return
  ; v0.0 - 2017.08.22 - First issue
  ; Author: David Torralba
  ; Last revision: 2017.08.22
)
(defun DT:LspTreeGetLspFilePaths ( directory / fileList )
  ; Return a list of all the ".lsp" files within the directory
  (if (setq fileList (vl-directory-files directory "*.lsp"))
    (mapcar
      '(lambda (fileName) (strcat directory "/" fileName))
      fileList
    );END mapcar
  );END if

  ; v0.0 - 2017.08.22 - First issue
  ; Author: David Torralba
  ; Last revision: 2017.08.22
)
(defun DT:LspTreeGetFileContent ( filePath / fileHandle readLine fileContent )
  ; Return file content

  ; Open file (read mode)
  (setq fileHandle (open filePath "r"))

  ; Read every single file line and concatenate them
  (while (setq readLine (read-line fileHandle))
    (if fileContent
      (setq fileContent (strcat fileContent readLine "\n"))
      (setq fileContent (strcat readLine "\n"))
    );END if
  );END while

  ; Close file
  (close fileHandle)

  ; Return data
  fileContent

  ; v0.0 - 2017.08.22 - First issue
  ; Author: David Torralba
  ; Last revision: 2017.08.22
)
(defun DT:LspTreeGetFunctionsAndDependencies ( filePath / fileHandle readLine fileContent return )
  ; Return a list with:
  ; - ".lsp" file path
  ; - defined functions within file
  ; - each defined function's dependencies

  ; Get file content
  (setq fileContent (DT:LspTreeGetFileContent filePath))

  ; Get defined functions
  (setq definedFunctions (DT:LspTreeGetDefinedFunctions fileContent))

  ; Return data
  (setq return (list filePath definedFunctions))
  (DT:PrintVar 'return)
  return

  ; v0.0 - 2017.08.22 - First issue
  ; Author: David Torralba
  ; Last revision: 2017.08.22
)
; (defun DT:LspTreeGetDefinedFunctions ( fileContent / remainingString currentNestedLevel defunExpression dependencyExpression functionNameDeclaration functionDeclaration functionName currentFunctionName ch1 functionDeclarationNestedLevel functionList dependencyFunctionNameDeclaration dependencyList )
;   ; Return a list with functions declared within filePath file
;
;   (if fileContent
;     (progn
;       (setq remainingString fileContent)
;       (setq currentNestedLevel 0)
;       (setq defunExpression "")
;       (setq dependencyExpression "")
;       (setq functionNameDeclaration nil)
;       (setq functionDeclaration nil)
;       (setq functionName "")
;
;       ; While the remainingString to analize is longer than zero ("")
;       (while (< 0 (strlen remainingString))
;         ; Get the first character of remainingString
;         (setq ch1 (substr remainingString 1 1))
;
;         (cond
;           ; Get current nesting level
;           ((= ch1 "(") (setq currentNestedLevel (1+ currentNestedLevel)) )
;           ((= ch1 ")") (setq currentNestedLevel (1- currentNestedLevel)) )
;
;           ; Find "defun " string and set function declaration condition
;           ((and (not functionNameDeclaration) (or (= ch1 "d")(= ch1 "e")(= ch1 "f") (= ch1 "u")(= ch1 "n")(= ch1 " ") ) )
;             (cond
;               ((= ch1 "d") (setq defunExpression (strcat defunExpression ch1)) )
;               ((and (= defunExpression "d")     (= ch1 "e") ) (setq defunExpression (strcat defunExpression ch1)) )
;               ((and (= defunExpression "de")    (= ch1 "f") ) (setq defunExpression (strcat defunExpression ch1)) )
;               ((and (= defunExpression "def")   (= ch1 "u") ) (setq defunExpression (strcat defunExpression ch1)) )
;               ((and (= defunExpression "defu")  (= ch1 "n") ) (setq defunExpression (strcat defunExpression ch1)) )
;               ((and (= defunExpression "defun") (= ch1 " ") )
;                 (setq defunExpression (strcat defunExpression ch1))
;                 (setq functionNameDeclaration T)
;                 (setq functionDeclaration T)
;                 (setq functionDeclarationNestedLevel currentNestedLevel)
;               );END and
;               ; If first character is "d", "e", "f", "u", "n" or " " but doesn't match, remove defunExpression history
;               (t (setq defunExpression "") )
;             );END cond
;           );END subcond
;
;           ; Collect function name while functionNameDeclaration is true
;           (functionNameDeclaration
;             (if (or (= " " ch1)(= "(" ch1))
;               ; If " " or "(" is found:
;               ; - finalize function name storing
;               ; - and add the function name to the list to be returned
;               (progn
;                 (setq functionNameDeclaration nil)
;                 (setq currentFunctionName functionName)
;                 (setq functionName "")
;               );END progn
;               ; Continue storing function name
;               (setq functionName (strcat functionName ch1))
;             );END if
;           );END subcond
;         );END cond
;
;         ; Collect dependent "DT:" function name if it's within declaration
;         ; if functionDeclaration is true and "(DT:" is found
;         (if functionDeclaration
;           (cond
;             ; Finalize function declaration
;             ( (and (= ")" ch1) (= functionDeclarationNestedLevel (+ currentNestedLevel 1)) )
;               (setq functionDeclaration nil)
;               (setq functionList (append functionList (if dependencyList
;                     (list (list currentFunctionName dependencyList))
;                     (list (list currentFunctionName))
;               )))
;               (setq dependencyList nil)
;             );END subcond
;
;             ; Capture "(DT:" functions
;             ( (and
;                 (not dependencyFunctionNameDeclaration)
;                 (or (= ch1 "(")(= ch1 "D")(= ch1 "T")(= ch1 ":"))
;               );END and
;               (cond
;                 ((= ch1 "(") (setq dependencyExpression (strcat dependencyExpression ch1)) )
;                 ((and (= dependencyExpression "(")   (= ch1 "D") ) (setq dependencyExpression (strcat dependencyExpression ch1)) )
;                 ((and (= dependencyExpression "(D")  (= ch1 "T") ) (setq dependencyExpression (strcat dependencyExpression ch1)) )
;                 ((and (= dependencyExpression "(DT") (= ch1 ":") )
;                   (setq dependencyExpression (strcat dependencyExpression ch1))
;                   (setq dependencyFunctionNameDeclaration T)
;                 );END subcond
;               );END cond
;             );END subcond
;
;             (dependencyFunctionNameDeclaration
;               ; If " " or ")" found, close the dependency expression and add it to the list
;               (if (or (= " " ch1) (= ")" ch1))
;                 (progn
;                   (setq dependencyExpression (substr dependencyExpression 2) )
;                   (setq dependencyList (append dependencyList (list dependencyExpression)))
;                   (setq dependencyExpression "")
;                   (setq dependencyFunctionNameDeclaration nil)
;                 );END progn
;                 (progn
;                   (setq dependencyExpression (strcat dependencyExpression ch1))
;                 );END progn
;               );END if
;             );END subcond
;
;             (t (setq dependencyExpression ""))
;           );END cond
;         );END if
;
;         ; Step by step printing
;         ; (princ "\nch=")(prin1 ch1)
;         ; (princ "   n=")(prin1 currentNestedLevel)
;         ; (princ "   nf=")(prin1 functionDeclarationNestedLevel)
;         ; ; (princ "   defun=")(prin1 defunExpression)
;         ; (princ "   foo=")(prin1 functionDeclaration)
;         ; (princ "   fNameT=")(prin1 functionNameDeclaration)
;         ; (princ "   fName=")(prin1 functionName)
;         ; (princ "   dE=")(prin1 dependencyExpression)
;         ; (princ "   dfT=")(prin1 dependencyFunctionNameDeclaration)
;         ; (getint)
;
;         ; Remove first character from remainingString
;         (setq remainingString (substr remainingString 2))
;       );END while
;
;       ; Return function name list
;       functionList
;
;     );END progn
;     nil
;   );END if
;
;   ; v0.0 - 2017.08.22 - First issue
;   ; Author: David Torralba
;   ; Last revision: 2017.08.22
; )
; (defun DT:LspTreeGetDefinedFunctions (
;   stringToParse /
;   remainingString exitParenthesis currentNestedLevel
;   argumentParenthesisFound
;   ch1
;   )
;   ; Recursive function to find function dependencies
;   ; Returns a list with the following structure:
;   ; (
;   ;   (
;   ;     function1Name
;   ;     (function1Argument1 function1Argument2 function1Argument3 ... )
;   ;     (function1Dependency1 function1Dependency2 ...)
;   ;   )
;   ;   (
;   ;     function2Name
;   ;     (function2Argument1 function2Argument2 function2Argument3 ... )
;   ;     (function2Dependency1 function2Dependency2 ...)
;   ;   )
;   ; )
;   ;
;   ; If a function has nested function definitions, the return will look like so:
;   ; (
;   ;   (
;   ;     functionName
;   ;     (functionArgument1 functionArgument2 functionArgument3 ... )
;   ;     (
;   ;       ( subFunction1Name (subFunction1Argument1 subFunction1Argument2 ...) (subFunction1Dependency1 subFunction1Dependency2 ... ) )
;   ;       ( subFunction2Name (subFunction2Argument1 subFunction2Argument2 ...) (subFunction2Dependency1 subFunction2Dependency2 ... ) )
;   ;       ...
;   ;     )
;   ;   )
;   ; )
;   (princ (strcat "\n--------------------------------------------- ITERATION " (itoa i) " -------------"))
;   (DT:PrintVar 'stringToParse)
;   (getint (strcat "\nPress ENTER TO CONTINUE: "))
;   (if (DT:Arg 'DT:LspTreeGetDefinedFunctions '((stringToParse 'str)))
;     (progn
;       (setq i (1+ i))
;       (setq remainingString stringToParse)
;       (setq exitParenthesis nil) ; will be T when defun closes parenthesis
;       (setq currentNestedLevel 0)
;       (setq argumentParenthesisFound nil)
;       (while (and (< 0 (strlen remainingString)) (not exitParenthesis) )
;         (setq ch1 (substr remainingString 1 1))
;         (DT:PrintVar 'ch1)
;
;         (cond
;           ((= ch1 "(")
;             ; Get current nesting level
;             (princ "\n--------------------------------------------- \"(\" found!")
;             (setq currentNestedLevel (1+ currentNestedLevel))
;             (if (= currentNestedLevel 1) (setq argumentParenthesisFound T))
;             (if (DT:LspTreeFindDefun remainingString)
;               ; Nested function declaration, pass remainingString
;               (progn
;                 (princ "\n--------------------------------------------- \"(defun \" found!")
;                 (DT:PrintVar 'remainingString)
;                 (DT:PrintVar 'exitParenthesis)
;                 (DT:PrintVar 'currentNestedLevel)
;                 (DT:PrintVar 'argumentParenthesisFound)
;                 (getint "\nDT:LspTreeGetDefinedFunctions will be now called")
;                 (DT:LspTreeGetDefinedFunctions (substr remainingString 2))
;               );END progn
;               ; to avoid reparsing the same, this function needs to return the length of the string
;               ; it has parsed, so that it can skip this part
;             );END if
;           );END subcond
;           ((= ch1 ")")
;             (setq currentNestedLevel (1- currentNestedLevel))
;             (princ "\n--------------------------------------------- \")\" found")
;             (DT:PrintVar 'argumentParenthesisFound)
;             (DT:PrintVar 'currentNestedLevel)
;             (if (and argumentParenthesisFound (= currentNestedLevel -1))
;               (progn
;                 (setq exitParenthesis T)
;                 (princ "\n--------------------------------------------- Setting exitParenthesis=T to exit finish (while) loop")
;                 (getint)
;               );END progn
;             );END if
;           );END subcond
;         );END cond
;         (setq remainingString (substr remainingString 2))
;       );END while
;     );END progn
;   );END if
;   (princ "\n---------------------------------------------END of DT:LspTreeGetDefinedFunctions reached!")
;   ; v0.0 - 2017.08.24 - First issue
;   ; Author: David Torralba
;   ; Last revision: 2017.08.24
; )
(defun DT:LspTreeFindDefun ( remainingString )
  ; Return true if the string "(defun " if found within remainingString

  (if (= "(defun " (substr remainingString 1 7))
    T
    nil
  );END if

  ; v0.0 - 2017.08.24 - First issue
  ; Author: David Torralba
  ; Last revision: 2017.08.24
)
(defun DT:LspTreeCleanString ( string / cleanString return )
  ; Return "string" without:
  ;   - new line characters "\n"
  (if (DT:Arg 'DT:LspTreeCleanString '((string 'str)))
    (progn
      ; Remove "\n"
      (setq cleanString (DT:RemovePattern string "\n"))

      ; Return clean string
      cleanString
    );END progn
  );END if

  ; v0.0 - 2017.08.24 - First issue
  ; Author: David Torralba
  ; Last revision: 2017.08.24
)
(defun DT:RemovePattern ( string pattern / cleanString )
  ; Remove pattern from string and return new string
  ; if pattern not found, return string anyway
  (if (DT:Arg 'DT:RemovePattern '((string 'str)(pattern 'str)))
    (progn
      (setq cleanString string)
      (while (vl-string-search pattern cleanString)
        (setq cleanString (vl-string-subst "" pattern cleanString))
      );END while
      cleanString
    );END progn
  );END if
  cleanString

  ; v0.0 - 2017.08.24 - First issue
  ; Author: David Torralba
  ; Last revision: 2017.08.24
)
(defun DT:LspTreeSplit ( stringTostringToParse / remainingString exitWhile currentNestedLevel capturedString return )
  ; Return a list with functions declared add current nesting level

  (setq remainingString stringTostringToParse)
  (setq currentNestedLevel 0)
  (setq capturedString "")
  (while (and (< 0 (strlen remainingString)) (not exitWhile) )
    (setq ch1 (substr remainingString 1 1))
    (if (= "(" ch1) (setq currentNestedLevel (1+ currentNestedLevel)) )
    (if (< 0 currentNestedLevel)
      (setq capturedString (strcat capturedString ch1))
    );END if
    (if (= ")" ch1)
      (progn
        ; If ")" found, update current nested level
        (setq currentNestedLevel (1- currentNestedLevel))
        ; If captured string if
        (if (and (= 0 currentNestedLevel) (> (strlen capturedString) 0))
          (progn
            (setq return (append return (list capturedString)))
            (setq capturedString "")
          );END progn
        );END if
      );END progn
    )
    (if (> (strlen remainingString) 1)
      (setq remainingString (substr remainingString 2))
      (progn
        (setq exitWhile T)
      );END progn
    );END if
  );END while
  return

  ; v0.0 - 2017.08.24¶ - First issue
  ; Author: David Torralba
  ; Last revision: 2017.08.24¶
)
(defun DT:LspTreeGetDefinedFunctions ( stringToParse / cleanedString chunckedString return )

  ; Clean string to be parsed
  (setq cleanedString (DT:LspTreeCleanString stringToParse))

  ; Chunk functions at current level
  (setq chunckedString (DT:LspTreeSplit cleanedString))

  ; Parse and split in chunks, parse and split in chunks... you know
  chunckedString
)
(defun DT:LspTreeSplitAndParseRecursive ( stringToParse / chunckedString parsedString )
  ; Split and parse stringToParse

  ; Split the string to parse
  (if (setq chunckedString (DT:LspTreeSplit stringToParse))
    (setq parsedString (DT:LspTreeParse chunckedString))
  );END if

  ; v0.0 - 2017.08.24 - First issue
  ; Author: David Torralba
  ; Last revision: 2017.08.24
)
(defun DT:LspTreeParse ( chunckedString )
  ; BUILD THISS!!
  (alert "BUILD DT:LspTreeParse")
  chunckedString

  ; v0.0 - 2017.08.24 - First issue
  ; Author: David Torralba
  ; Last revision: 2017.08.24
)
