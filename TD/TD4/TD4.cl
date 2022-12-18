
(setq laby
  '(
    (e 1)
    (1 e 2)
    (2 1 7)
    (3 6)
    (4 5)
    (5 4 12)
    (6 3 7)
    (7 2 6 8)
    (8 7 9)
    (9 7 10)
    (10 9 11 15)
    (11 10 12 14)
    (12 5 11)
    (13 20)
    (14 11)
    (15 16 10)
    (16 15 17)
    (17 16 18)
    (18 17 19)
    (19 20 18)
    (20 13 s)
    (s 20)
  )
)

(defun successeurs (etat laby)
  (cdr (assoc etat laby))
)

(defun successeurs_valides (etat laby chemin)
  (let ((succ (successeurs etat laby)))
    (dolist (x chemin succ) (setq succ (remove x succ)))
  )
)

(defun explore_prof (laby etat sortie chemin)
  (if (equal etat sortie)
    (list etat)
    (progn
      (setq chemin (cons etat chemin))
      (let
        ((valides (successeurs_valides etat laby chemin))(trouve NIL))
        (progn
          (while (and valides (equal trouve NIL))
            (progn
              (setq trouve (explore_prof laby (pop valides) sortie chemin))
              (if trouve
                (setq trouve (cons etat trouve))
                NIL
              )
            )
          )
          (if trouve
            (setq trouve trouve)
            (progn
              (setq chemin (remove etat chemin))
              NIL
            )
          )
        )
      )
    )
  )
)
