;;;; Grassi Marco 829664

(defparameter *classes-specs* (make-hash-table))

(defun add-class-spec (name class-spec)
  (setf (gethash name *classes-specs*) class-spec))

(defun get-class-spec (name)
  (gethash name *classes-specs*))

;;; An instance is a list '('oolinst Name class arg-value-list)

;;;; class-name is a Symbol, parents a list (might be null)
;;;; slot-value is a list where each element is an assoc-list
;;;; that might be slot-name(field) - value or
;;;; method-name - method expression
;;;; each of these parameters must be evaluated
;;;; a class is a list

;;;;ex i-list ('name "gianni" 'surname "bolla")
;;;;ex (method-name '(=> (arg) (form1) (form2) ....)
;;;; formats a flat list into an association list.
;;;; If the list contains methods these are processed.

(defun format-to-alist (i-list) 
  (if (null i-list) nil
      (cons (if (and (listp (second i-list))
                     (eq '=> (first (second i-list))))                 
                (cons (first i-list) (process-method (first i-list) (second i-list)))             
                (cons (first i-list) (second i-list)))
            (format-to-alist (rest (rest i-list))))))

;;;;checks if all the slot-names are symbols
;;;;checks if every slot-name has an associated slot-value
;;;;checks if slot-names are duplicated
(defun check-slots-validity (field-list) 
  (if (oddp (length field-list)) (error "Not all slots have a corresponding value"))  
  (if (not (null (mapcan (lambda (arg)
                           (not (symbolp (car arg))))
                         (format-to-alist field-list))))
      (error "At least a slot is not a symbol"))
  (if (not (= (length (get-slot-name-list field-list))
              (length (remove-duplicates (get-slot-name-list field-list)))))
      (error "duplicated slot names"))
  t)

;;;;return all of the slot-names in a slot-value list
(defun get-slot-name-list (field-list)
  (mapcar (lambda (x) (car x)) (format-to-alist field-list))) 


(defun check-class-validity (class-name parents field-list)
  (cond ((not (symbolp class-name)) (error "class-name arg is not a symbol"))
        ((null parents) (check-slots-validity field-list))
        ((not (listp parents)) (error "Parents is not a list"))                
        ((not (null (mapcan (lambda (arg) (not (symbolp arg))) parents)))
         (error "At least one parent arg is not a symbol"))
        ((null (mapcan 'get-class-spec parents))
         (error "parent class/es have not been defined"))
        ((not (null (member class-name parents))) (error "A class cannot be a child of itself"))
        (t (check-slots-validity field-list))))
          
;;;;defines a class and adds it to the global hash table.
;;;;if the class was already defined then the old one is
;;;;replaced.
;;;;if parents are duplicated only one "copy" is preserved.
(defun def-class (class-name parents &rest slot-value)
  (if (check-class-validity class-name (remove-duplicates parents) slot-value)
      (add-class-spec class-name (list :parents (remove-duplicates parents)
                                       :fields (format-to-alist slot-value))))
  class-name)
                      




;;;; class-name is a symbol
;;;; slot-value is an assoc-list (slot . value)
;;;; slot is a symbol value is whatever.
;;;; an instance looks like (:type 'oolinst :class-name class-name :fields (list of alists(slot value)
;;;; slot-value added must be defined in class hierarchy
(defun new (class-name &rest slot-value)
  (if (not (symbolp class-name)) (error "invalid class-name"))
  (if (null (get-class-spec class-name)) (error "class does not exist"))  
  (when (check-slots-validity slot-value)
  (let ((class-field-list (mapcar 'car (getf (get-class-spec class-name) :fields)))
        (instance-field-list (mapcar 'car (format-to-alist slot-value))))
    (if (subsetp instance-field-list class-field-list)
        (list :type 'oolinst :class-name class-name :fields (format-to-alist slot-value))
        (error "slot not defined in class")))))


;;;; returns a flat list made of all the superclasses of a class
;;;; order of appearence is defined by order of definition in class
(defun class-hierarchy-list (class)
  (cond ((null class) nil)
        ((null (get-class-spec class)) (error "Class does not exist"))
        ((and (atom class) (null (getf (get-class-spec class) :parents))) (list class))
        (t (append (if (atom class) (list class) class)
                   (append (class-hierarchy-list (first (getf (get-class-spec class) :parents)))
                           (class-hierarchy-list (rest (getf (get-class-spec class) :parents))))))))

(defun find-slot-in-hierarchy (hierarchy slot-name)
  (cond ((null hierarchy) (error "slot not present in class hierarchy"))
        ((not (null (assoc slot-name (getf (get-class-spec (first hierarchy)) :fields))))
         (cdr (assoc slot-name (getf (get-class-spec (first hierarchy)) :fields))))
        (t (find-slot-in-hierarchy (rest hierarchy) slot-name))))
         
                                        

;;;; an instance looks like (:type 'oolinst :class-name class-name :fields (list of alists(slot value))
;;;; returns value associated with slot-name in instance or class hierachy of instance
(defun getv (instance slot-name)
  (if (or (null instance)
          (not (eq (getf instance :type) 'oolinst)))
      (error "invalid instance"))
  (if (null (get-class-spec (getf instance :class-name)))
      (error "non-existent class for this instance"))
  (if (or (null slot-name)
          (not (symbolp slot-name)))
      (error "invalid slot-name"))
  (cond ((not (null (assoc slot-name (getf instance :fields)))) ;;if it's in the instance
         (cdr (assoc slot-name (getf instance :fields))))
        (t (find-slot-in-hierarchy (class-hierarchy-list (getf instance :class-name)) slot-name))))  

;;;; return value from a class by following a chain of slot-names
(defun getvx (instance slot-name)  
  (cond ((or (null slot-name)
             (not (listp slot-name)))
         (error "invalid slot-name"))
        ((or (null instance)
             (not (eq (getf instance :type) 'oolinst)))
         (error "invalid instance"))
        (t (cond ((and (symbolp (first slot-name))
                       (null (rest slot-name)))
                  (getv instance (first slot-name)))
                 (t (getvx (getv instance (first slot-name)) (rest slot-name)))))))

;;;; if a method is correctly defined associate it's name to a call to
;;;; the rewritten method (a lambda). An argument this is added so that
;;;; only appropriated instances can call the method.
(defun process-method (method-name method-spec)
  (if (not (symbolp method-name)) (error "method-name is not a symbol"))
  (if (not (eq (first method-spec) '=>)) (error "error in method-definition syntax"))
  (if (not (listp (second method-spec))) (error "invalid arg-list"))
  (if (not (eql (length (second method-spec))
                (length (remove-duplicates (second method-spec)))))
      (error "arg-list has some duplicated args"))

  (setf (fdefinition method-name)
        (lambda (this &rest args)          
          ;;;getv error if no method is found
          (apply (getv this method-name) (append (list this)
                                                 args))))
  (eval (rewrite-method-code method-name method-spec)))

;;;method-spec body is a list
;;; constructs a lambda from the method-spec passed,
;;; adds an additional argument this (always the first arg)
(defun rewrite-method-code (method-name method-spec)
  (append `(lambda ,(append '(this) (second method-spec)))
          (cdr (cdr method-spec))))
