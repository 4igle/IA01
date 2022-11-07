(setq bdr
  '(
      (F (B D E) R1)
      (A (D G) R2)
      (A (C F) R3)
      (D (C) R4)
      (E (D) R5)
      (H (A) R6)
      (X (B) R7)
      (A (X C) R8)
  )
)

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

;; test à exécuter de préférence directement en tant que commande pour éviter
;; un bug d'affichage
;; (chainage_arriere 'H '(B C) bdr)
