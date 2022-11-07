;; un seul terme
(defun deriv_terme (term var)
  (if (equal term var)
    1
    0
  )
)

;; addition et soustraction
(defun deriv_add (exp var)
  (list (car exp) (deriv (cadr exp) var) (deriv (caddr exp) var))
)

;; multiplication : (uv)' = (u' * v) + (u * v')
(defun deriv_mult (exp var)
  (list
    '+
    (list
      '*
      (deriv (cadr exp) var)
      (caddr exp)
    )
    (list
      '*
      (cadr exp)
      (deriv (caddr exp) var)
    )
  )
)

;; division : (u/v)' = ((u' * v) - (u * v')) / (v * v)
(defun deriv_div (exp var)
  (list
    '/
    (list
      '-
      (list
        '*
        (deriv (cadr exp) var)
        (caddr exp)
      )
      (list
        '*
        (cadr exp)
        (deriv (caddr exp) var)
      )
    )
    (list
      '*
      (caddr exp)
      (caddr exp)
    )
  )
)

;; exponentielle : exp(u)' = u' * exp(u)
(defun deriv_exp (exp var)
  (list
    '*
    (deriv (cadr exp) var)
    (list
      'EXP
      (cadr exp)
    )
  )
)

;; log : ln(u)' = ln(u'/u)
(defun deriv_log (exp var)
  (list
    'LN
    (list
      '/
      (deriv (cadr exp) var)
      (cadr exp)
    )
  )
)

;; sinus : sin(u)' = u' * cos(u)
(defun deriv_sin (exp var)
  (list
    '*
    (deriv (cadr exp) var)
    (list
      'cos
      (cadr exp)
    )
  )
)

;; cosinus : cos(u)' = -u' * sin(u)
;; on représente l'expression négative comme suit :
;; -expression = 0 - expression
;; ce qui nous permet d'utiliser la synthaxe déjà définie
(defun deriv_cos (exp var)
  (list
    '-
    0
    (list
      '*
      (deriv (cadr exp) var)
      (list
        'sin
        (cadr exp)
      )
    )
  )
)

;; tangente : tan(u)' = u' / (cos(u) * cos(u))
(defun deriv_tan (exp var)
  (list
    '/
    (deriv (cadr exp) var)
    (list
      '*
      (list 'cos (cadr exp))
      (list 'cos (cadr exp))
    )
  )
)

;; fonction générale
(defun deriv (exp var)
  (cond
    ((atom exp) (deriv_terme exp var))
    ((or (equal (car exp) '+) (equal (car exp) '-)) (deriv_add exp var))
    ((equal (car exp) '*) (deriv_mult exp var))
    ((equal (car exp) '/) (deriv_div exp var))
    ((equal (car exp) 'EXP) (deriv_exp exp var))
    ((equal (car exp) 'LOG) (deriv_log exp var))
    ((equal (car exp) 'SIN) (deriv_sin exp var))
    ((equal (car exp) 'COS) (deriv_cos exp var))
    ((equal (car exp) 'TAN) (deriv_tan exp var))
    (T 0)
  )
)

;; exemples d'utilisation
(deriv '(EXP X) 'x)
(deriv '(+ 1 (* 2 X)) 'x)
(deriv '(/ 1 X) 'x)
(deriv '(- x (* x y)) 'x)
(deriv '(log x) 'x)
(deriv '(log (* x x)) 'x)
(deriv '(cos x) 'x)
(deriv '(sin (* 2 x)) 'x)
(deriv '(tan (* x x)) 'x)
