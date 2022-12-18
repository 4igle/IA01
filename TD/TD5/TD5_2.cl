(setq *basederegles*
  '(
      ((eq moyen voiture) ((>= d 5)) R1)
      ((eq moyen voiture) ((<= t 15)) R2)
      ((eq moyen a_pied) ((<= d 5)(> t 15)) R3)
      ((eq action taxi) ((eq moyen voiture)(eq arena ville)(eq action taxi)) R4)
      ((eq action voiture_perso) ((eq moyen voiture)(neq arena ville)) R5)
      ((eq action a_pied_avec_impermerable) ((eq moyen a_pied)(eq temps mauvais)) R6)
      ((eq action promenade) ((eq moyen a_pied)(eq temps beau)) R7)
  )
)


(defparameter *basedefaits* '((temps mauvais)(d 6)(t 20)))

(defun appartient (but bdf)
  (let ((element (assoc (cadr but) bdf)))
    (if element
      (funcall (car but) (cadr element) (caddr but))
      NIL
    )
  )
)


; (appartient '(>= d 5) *basedefaits*)


(defun CCL (regle)
  (car regle)
)

(defun premiere_regle (regle)
  (cadr regle)
)

(defun num_regle (regle)
  (caddr regle)
)

(defun regles_candidates (but bdr)
  (if bdr
    (if (equal (CCL (car bdr)) but)
      (cons (car bdr) (regles_candidates but (cdr bdr)))
      (regles_candidates but (cdr bdr))
    )
  )
)


(defun chainage_arriere (but bdf bdr &optional (i 0))
  (if (member but bdf)
    (progn
      (format t "~V@t-> ~s vrai car dans la bdf : ~s obtenu~&" i but but)
      T
    )
    (let
      ((cand (regles_candidates but bdr)) (ok NIL) (prem NIL) (X NIL) (nom NIL))
      (while (and cand (not ok))
        (setq nom (num_regle(car cand)))
        (format t "~V@t-> Obtention de ~s avec la regle ~s : ~s->~s~&" i but nom (premiere_regle(car cand)) but)
        (setq prem (premiere_regle (pop cand)))
        (setq ok T)
        (while (and prem ok)
          (setq X (pop prem))
          (setq ok (chainage_arriere X bdf bdr (+ i 3)))
          (if (not ok)
            (format t "~%~V@t-> Impossible d'obtenir ~s avec ~s : regle suivante~2%" i but nom)
          )
        )
      )
      (if ok
        (format t "~V@t-> ~s obtenu~&" i but)
        (format t "~V@t-> Impossible d'obtenir ~s : abandon~&" i but)
      )
      ok
    )
  )
)



(chainage_arriere '(eq action promenade) *basedefaits* *basederegles*)




;; test à exécuter de préférence directement en tant que commande pour éviter
;; un bug d'affichage
;; (chainage_arriere 'H '(B C) bdr)
