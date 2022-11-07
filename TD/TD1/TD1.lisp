23
(quote 23)
'23
(set x 32)
(setq x 32)
(list x x 32)
(cadr (list x x 32))
(setq x 'y)
(setq xx 5)
(setq y '(+ xx xx 32))
x
(eval x)
(eval y)
(cadr y)
(eval (list '+ (eval y)(cadr y)))
(setq z (+ (if x 2 0) (caddr y) (* y y)))
(setq y (list (setq z "Albert") x y))
z
y
(setq x (* x x))
x
