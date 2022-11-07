(setq ll '( A 1 BB 2 CCC 3 DDD 4))


(defun aff_1(l)
  (loop for x in l
      do (print x))
)

(defun aff_2(l)
  (loop (if l (print (pop l)) (return NIL)))
)

(defun aff_3(l)
  (dolist (x l) (print x))
)

(defun aff_4(l)
  (mapcar #'(lambda (x) (print x)) l)
)

(defun aff_5(l)
  (mapcar 'print l)
)

(defun aff_6(l)
  (maplist #'(lambda (x) (print (car x))) l)
)

(defun aff_7(liste)
  (if liste
    (progn
      (print (car liste))
      (aff_7 (cdr liste))
    )
    NIL
  )
)

(aff_1 ll)
(aff_2 ll)
(aff_3 ll)
(aff_4 ll)
(aff_5 ll)
(aff_6 ll)
(aff_7 ll)





(defun make_html(html indent file)
  (if (listp html)
    (
      progn
      (format file "~V@t<~s>~&" indent (car html))
      (dolist (x (cdr html)) (make_html x (+ indent 3) file))
      (format file "~V@t</~s>~&" indent (car html))
    )
    (format file "~V@t~a~&" indent html)
  )
)


(setq *html* '(html
    (header
        (title
          "titre"
        )
    )
    (body
        (h1
          "titre"
        )
        (p
          "parag"
        )
     )
))


(with-open-file (file "test.html"
    :if-does-not-exist :create
    :if-exists :overwrite
    :direction :output)

    (make_html *html* 0 file)
)
