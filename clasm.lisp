(defun asm (&rest rest)
  (mapcar (lambda (str) (progn (princ str)
                               (terpri)))
          rest)
  t)

(defun join-strings-with-comma (s1 s2)
  (concatenate 'string s1 "," s2))

(defun operands-from-list (l)
  (reduce #'join-strings-with-comma
          (mapcar #'princ-to-string l)))

(defmacro definstruction (name)
  `(defun ,name (&rest rest)
     (let ((operands (operands-from-list rest)))
       (concatenate 'string
                    "    "
                    ,(string-downcase (symbol-name name))
                    "    "
                    operands))))

(defmacro defregister (name)
  `(defvar ,name (concatenate 'string
                              "$"
                              ,(string-downcase (symbol-name name)))))
                                   

(defun label (name)
  (concatenate 'string name ":"))


; Sample x86 instructions and registers

(definstruction add)
(definstruction xor)
(definstruction cmp)
(definstruction jge)
(definstruction inc)
(definstruction jmp)

(defregister eax)
(defregister ebx)
(defregister ecx)
(defregister edx)

; Sample program

(defun sample-program ()
  (asm
   (label "start")
   (xor eax eax)
   (xor ebx ebx)
   (add ebx 100)
   (label "loop")
   (cmp eax ebx)
   (jge "end")
   (inc eax)
   (jmp "loop")
   (label "end")))
