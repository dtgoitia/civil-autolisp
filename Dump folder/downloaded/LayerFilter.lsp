;; EXPFLTR & IMPFLTR (gile)
;; Export and import property layer filters

;; EXPFLTR
;; Exports a layer filter datas from the current drawint to a file (.flt)

(defun c:ExpFltr (/		layerDict     layerFilters
		  filterList	filterName    layerFilter
		  dataType	dataValue     filterDatas
		  fileName	file
		 )
  (vl-load-com)
  (or *acdoc*
      (setq *acdoc* (vla-get-ActiveDocument (vlax-get-acad-object)))
  )
  (if
    (and
      (setq layerDict
	     (vla-getExtensionDictionary (vla-get-Layers *acdoc*))
      )
      (setq layerFilters (GetItem layerDict "ACAD_LAYERFILTERS"))
      (vlax-for	f layerFilters
	(setq filterList (cons (vla-get-Name f) filterList))
      )
    )
     (if (setq filterName
		(ListBox "Export a filter"
			 "Choose the filter to export"
			 (mapcar '(lambda (x) (cons x x))
				 (reverse filterList)
			 )
			 0
		)
	 )
       (if
	 (setq
	   fileName (getfiled "Create an exportation file"
			      filterName
			      "lft"
			      1
		    )
	 )
	  (progn
	    (setq file (open fileName "w"))
	    (write-line
	      "//Layer filter export, DO NOT MODIFY."
	      file
	    )
	    (mapcar
	      (function
		(lambda (x) (write-line (vl-prin1-to-string x) file))
	      )
	      (getFilterDatas *acdoc* filterName)
	    )
	    (close file)
	  )
       )
     )
     (alert "None property layer filter in this drawing")
  )
  (princ)
)

;; IMPFLTR
;; Imports a layer filter in the current drawing from an export file (.flt) or a drawing (.dwg/.dwt)

(defun c:ImpFltr (/	       fileName	    file	 dataList
		  filterName   source	    odbx	 layerDict
		  layerFilters filterList
		 )
  (vl-load-com)
  (or *acdoc*
      (setq *acdoc* (vla-get-ActiveDocument (vlax-get-acad-object)))
  )
  (if (setq fileName (getfiled "Import a filter" "" "lft;dwg;dwt" 0))
    (cond
      ((= (strcase (vl-filename-extension fileName)) ".LFT")
       (setq file (open fileName "r"))
       (read-line file)
       (repeat 4
	 (setq dataList (cons (read (read-line file)) dataList))
       )
       (close file)
       (setq dataList	(reverse dataList)
	     filterName	(caadr dataList)
       )
       (if (not (addLayerFilter *acdoc* dataList))
	 (alert
	   (strcat "\nThe filter \"" filterName "\" already exists.")
	 )
       )
      )
      ((member (strcase (vl-filename-extension fileName))
	       '(".DWG" ".DWT")
       )
       (if
	 (not
	   (and
	     (setq
	       source (GetItem
			(vla-get-Documents (vlax-get-acad-object))
			(strcat (vl-filename-base fileName) ".dwg")
		      )
	     )
	     (= fileName (vla-get-FullName source))
	   )
	 )
	  (setq	source (OpenDrawingDBX filename)
		odbx   T
	  )
       )
       (if
	 (and
	   (setq layerDict (vla-getExtensionDictionary
			     (vla-get-Layers source)
			   )
	   )
	   (setq layerFilters (GetItem layerDict "ACAD_LAYERFILTERS"))
	   (vlax-for f layerFilters
	     (setq filterList (cons (vla-get-Name f) filterList))
	   )
	 )
	  (if (setq filterName
		     (ListBox
		       "Import afilter"
		       "Choose the filter to import"
		       (mapcar '(lambda (x) (cons x x)) (reverse filterList))
		       0
		     )
	      )
	    (if	(not (addLayerFilter
		       *acdoc*
		       (getFilterDatas source filterName)
		     )
		)
	      (alert
		(strcat "\nThe filter \"" filterName "\" already exists.")
	      )
	    )
	  )
	  (alert
	    "None property layer filter in the source drawing"
	  )
       )
       (and odbx (vlax-release-object source))
      )
      (T (alert "The choosen file is not valid."))
    )
  )
  (princ)
)

;;===========================================================;;

;; getFilterDatas (gile)
;; Get a property filter datas
;;
;; Arguments
;; sourceDoc: The document in which the filter is got  (vla-object)
;; filterName: the layer filter name (string)
;;
;; Return
;; a list of 4 sub lists countaining the filter XRecord datas or nil
;; if the filter does not exist in the document

(defun getFilterDatas (sourceDoc     filterName	   /
		       layerDict     layerFilters  aclyDict
		       layerFilter   dataType	   dataValue
		       tmpType	     tmpValue	   return
		      )

  (setq
    layerDict (vla-getExtensionDictionary (vla-get-Layers sourceDoc))
  )
  (if
    (and
      (setq layerFilters (GetItem layerDict "ACAD_LAYERFILTERS"))
      (setq aclyDict (GetItem layerDict "ACLYDICTIONARY"))
      (setq layerFilter (GetItem layerFilters filterName))
    )
     (progn
       (vlax-for xr aclyDict
	 (vla-GetXRecordData xr 'dataType 'dataValue)
	 (setq tmpType (vlax-safearray->list dataType))
	 (if (= 290 (car tmpType))
	   (setq tmpValue (mapcar 'vlax-variant-value
				  (cdr (vlax-safearray->list dataValue))
			  )
		 tmpType  (cdr tmpType)
	   )
	   (setq tmpValue (mapcar 'vlax-variant-value
				  (vlax-safearray->list dataValue)
			  )
	   )
	 )
	 (if (and (= (car tmpValue) "AcLyLayerFilter")
		  (member filterName tmpValue)
	     )
	   (setq return (list tmpType tmpValue))
	 )
       )
       (vla-GetXRecordData layerFilter 'dataType 'dataValue)
       (setq
	 return	(cons (vlax-safearray->list dataType)
		      (cons (mapcar 'vlax-variant-value
				    (vlax-safearray->list dataValue)
			    )
			    return
		      )
		)
       )
     )
  )
)

;;===========================================================;;

;; addLayerFilter (gile)
;; Add a property layer filter to the document
;;
;; Arguments
;; targetDoc: the target document (vla-object)
;; dataList: a list of 4 sub lists countaining the filter XRecord datas
;;
;; Retour: T or nil if the filter already exists in the document

(defun addLayerFilter (targetDoc    dataList	 /
		       filterName   layerDict	 layerFilters
		       aclyDict	    layerFilter	 n
		       aclyName	    aclyXRec
		      )

  (setq	filterName (caadr dataList)
	layerDict  (vla-getExtensionDictionary (vla-get-Layers targetDoc))
  )
  (or
    (setq layerFilters (GetItem layerDict "ACAD_LAYERFILTERS"))
    (and
      (vla-addObject
	layerDict
	"ACAD_LAYERFILTERS"
	"AcDbDictionary"
      )
      (setq layerFilters (vla-item layerDict "ACAD_LAYERFILTERS"))
    )
  )
  (or
    (setq aclyDict (GetItem layerDict "ACLYDICTIONARY"))
    (and
      (vla-addObject layerDict "ACLYDICTIONARY" "AcDbDictionary")
      (setq aclyDict (vla-item layerDict "ACLYDICTIONARY"))
    )
  )
  (if (not (GetItem layerFilters filterName))
    (progn
      (setq layerFilter
	     (vla-addXRecord layerFilters filterName)
      )
      (setq n 1)
      (not
	(while
	  (getitem aclyDict
		   (setq aclyName (strcat "*A" (itoa n)))
	  )
	   (setq n (1+ n))
	)
      )
      (setq aclyXRec (vla-addXRecord aclyDict aclyName))
      (vla-SetXRecordData
	layerFilter
	(vlax-make-variant
	  (vlax-safearray-fill
	    (vlax-make-safearray
	      vlax-vbInteger
	      (cons 0 (1- (length (car dataList))))
	    )
	    (car dataList)
	  )
	)
	(vlax-make-variant
	  (vlax-safearray-fill
	    (vlax-make-safearray
	      vlax-vbVariant
	      (cons 0 (1- (length (cadr dataList))))
	    )
	    (mapcar 'vlax-make-variant (cadr dataList))
	  )
	)
      )
      (vla-SetXRecordData
	aclyXRec
	(vlax-make-variant
	  (vlax-safearray-fill
	    (vlax-make-safearray
	      vlax-vbInteger
	      (cons 0 (1- (length (caddr dataList))))
	    )
	    (caddr dataList)
	  )
	)
	(vlax-make-variant
	  (vlax-safearray-fill
	    (vlax-make-safearray
	      vlax-vbVariant
	      (cons 0 (1- (length (cadddr dataList))))
	    )
	    (mapcar 'vlax-make-variant (cadddr dataList))
	  )
	)
      )
      T
    )
  )
)

;;===========================================================;;

;; ListBox (gile)
;; Dialog box for one or more choice(s)
;;
;; Arguments
;; title: the dialog title (string)
;; msg: message (string), "" or nil for none
;; keylab: an association list which type is ((key1 . label1) (key2 . label2) ...)
;; flag: 0 = popup list
;;        1 = single choice list box
;;        2 = multiple choice list box
;;
;; Return: the option key (flag = 0 or 1) or a list of the option keys (flag = 2)
;;
;; Example
;; (listbox "Layout" "Choose a layout" (mapcar 'list (layoutlist) (layoutlist)) 1)

(defun ListBox (title msg keylab flag / tmp file dcl_id choice)
  (setq	tmp  (vl-filename-mktemp "tmp.dcl")
	file (open tmp "w")
  )
  (write-line
    (strcat "ListBox:dialog{label=\"" title "\";")
    file
  )
  (if (and msg (/= msg ""))
    (write-line (strcat ":text{label=\"" msg "\";}") file)
  )
  (write-line
    (cond
      ((= 0 flag) "spacer;:popup_list{key=\"lst\";")
      ((= 1 flag) "spacer;:list_box{key=\"lst\";")
      (T "spacer;:list_box{key=\"lst\";multiple_select=true;")
    )
    file
  )
  (write-line "}spacer;ok_cancel;}" file)
  (close file)
  (setq dcl_id (load_dialog tmp))
  (if (not (new_dialog "ListBox" dcl_id))
    (exit)
  )
  (start_list "lst")
  (mapcar 'add_list (mapcar 'cdr keylab))
  (end_list)
  (action_tile
    "accept"
    "(or (= (get_tile \"lst\") \"\")
    (if (= 2 flag) (progn
    (foreach n (str2lst (get_tile \"lst\") \" \")
    (setq choice (cons (nth (atoi n) (mapcar 'car keylab)) choice)))
    (setq choice (reverse choice)))
    (setq choice (nth (atoi (get_tile \"lst\")) (mapcar 'car keylab)))))
    (done_dialog)"
  )
  (start_dialog)
  (unload_dialog dcl_id)
  (vl-file-delete tmp)
  choice
)

;;===========================================================;;

;;; GetItem (gile)
;; Returns the item vla-object if found in the collection
;;;
;;; Arguments
;;; col: a collection (vla-object)
;;; name: the object name (string) or its index (integer)
;;;
;;; Return: the vla-object or nil

(defun GetItem (col name / obj)
  (vl-catch-all-apply
    (function (lambda () (setq obj (vla-item col name))))
  )
  obj
)

;;===========================================================;;

;; OpenDrawingDBX
;; Get a closed drawing
;;
;; Argument: the filename (dwg)
;;
;; Return: the document (vla-object)

(defun OpenDrawingDBX (fileName / objDBX)
  ((lambda (release)
     (setq objDBX
	    (vlax-create-object
	      (if (< release 16)
		"ObjectDBX.AxDbDocument"
		(strcat "ObjectDBX.AxDbDocument." (itoa release))
	      )
	    )
     )
   )
    (atoi (getvar "ACADVER"))
  )
  (vla-open objDBX fileName)
  objDBX
)
