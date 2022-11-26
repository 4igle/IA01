;; DEFINITION DES LABYRINTHES

(setq *laby1* '((A1 A2 B1)
                (A2 A3 A1)
                (A3 A4 A2)
                (A4 B4 A3)
                (B1 C1 A1)
                (B2)
                (B3)
                (B4 C4 A4)
                (C1 C2 D1 B1)
                (C2 C3 D2 C1)
                (C3 C4 C2)
                (C4 D4 C3 B4)
                (D1 D2 C1)
                (D2 D1 C2)
                (D3)
                (D4 C4)))

(setq *laby2* '((A1 A2 B1)
                (A2 A3 A1)
                (A3 A4 A2)
                (A4 B4 A3)
                (B1 C1 A1)
                (B2)
                (B3)
                (B4 C4 A4)
                (C1 C2 D1 B1)
                (C2 C1)
                (C3)
                (C4 D4 B4)
                (D1 E1 C1)
                (D2)
                (D3)
                (D4 E4 C4)
                (E1 E2 D1)
                (E2 E3 E1)
                (E3 E4 E2)
                (E4 D4 E3)))


;; DEFINITION ET TESTS DES FONCTIONS DE SERVICE

;==============================================================================
; successeurs


(defun successeurs (etat laby)
  (cdr (assoc etat laby)))


;(successeurs 'A2 *laby1*)
;(successeurs 'A5 *laby1*)
;(successeurs 'C4 *laby1*)

;==============================================================================
; successeursValides


(defun successeursValides (etat laby chem)
  (let ((succ (successeurs etat laby))(suivants nil) (c chem))
    (dolist (x succ suivants)
      (if (not (member x chem))
          (push x suivants)))))


; (successeursValides 'C4  *laby1* '(C3))

;==============================================================================
; distanceManhattan


(defun x(case)
  (- (char-int (char (symbol-name case) 0)) 64))

(defun y(case)
  (- (char-int (char (symbol-name case) 1)) 48))

(defun distanceManhattan (startingPoint endPoint)
  (+ (abs (- (x startingPoint) (x endPoint)))
     (abs (- (y startingPoint) (y endPoint)))))


;(distanceManhattan 'A1 'B1)
;(distanceManhattan 'A1 'B2)
;(distanceManhattan 'A1 'D4)

;==============================================================================
; sortBy


(defun sortBy (lst)
  (stable-sort lst #'< :key #'cadr))


; (setq lst '((1 3) (2 2) (3 2) (2 3) (2 2) (3 1) (1 3) (4 2) (3 5)))
; (sortby lst)

;==============================================================================













; ALGO GLOUTON

(defun glouton (laby depart arrivee)
  (let ((chemin NIL)
        (file (list (list depart (distanceManhattan depart arrivee))))
        (current NIL)
        (succ NIL))


    ; condition d'arret : l'arrivee est dans la file d'elements a traiter
    (while (not (assoc arrivee file))

      ; (print file) ; decommenter cette ligne pour voir l'evolution de la liste d'elements a traiter

      ; on enregistre l'etat dans le chemin
      (push (caar file) chemin)

      ; on enleve le premier element de la liste d'etats a traiter et on le traite
      (setq current (pop file))

      ; on recupere ses successeurs
      (setq succ (successeursValides (car current) laby chemin))

      ; pour chaque successeur, on construit un element et on l'ajoute a a liste d'elements a traiter
      (dolist (x succ)
        (push (list x (distanceManhattan x arrivee)) file)
      )

      ; on trie la liste en fonction de la plus faible distance
      (setq file (sortBy file))

    )

  ; pour le retour, on inverse la liste des etats parcourus pour avoir
  ; les etats dans le meme ordre qu'il ont ete parcourus. Cependant,
  ; cette liste peut aussi contenir des etats explores mais ne faisant
  ; pas partie du chemin final. Pour retirer ces etats, on appelle clean
  ; (definie plus bas)
  (clean (reverse (push arrivee chemin)) laby)

  )
)



; ALGO A*
; Etant donne que l'algo est quasiment identique a l'algo glouton,
; je ne vais que commenter la partie qui change

(defun A* (laby depart arrivee)
  (let ((chemin NIL)
        (file (list (list depart (distanceManhattan depart arrivee))))
        (current NIL)
        (succ NIL))

    (while (not (assoc arrivee file))

      ; (print file)
      (push (caar file) chemin)

      (setq current (pop file))
      (setq succ (successeursValides (car current) laby chemin))

      ; Ceci est la partie qui change, lors de la construction de nouveaux
      ; elements, nous devons maintenant rajouter le cout de deplacement
      (dolist (x succ)
        (push (list x (+
                        (+ 1 (- (cadr current) (distanceManhattan (car current) arrivee))) ; g(s) cout de deplacement
                        (distanceManhattan x arrivee))) ; h(s) distance de manhattan
                      file)
      )

      ; le cout de deplacement d'un successeur est egal au cout de deplacement
      ; de l'element actuel + 1, le + 1 vient donc de la.
      ; Pour recuperer le cout de deplacement de l'element actuel,
      ; on se sert du fait que f(s) = g(s) + h(s), donc g(s) = f(s) - h(s), d'ou le
      ; (- (cadr current) (distanceManhattan (car current) arrivee))

      (setq file (sortBy file))

    )

  (clean (reverse (push arrivee chemin)) laby)

  )
)



; FONCTION CLEAN
; Cette fonction sert a retirer les etats visites mais ne faisant pas partie
; du chemin final, cela permet un retour plus lisible dans les algos glouton et A*
; Le principe est le suivant :
; Pour que la liste ordonnee des etats parcourus soit egale au chemin final,
; il faut que pour chaque element de la liste (sauf le dernier),
; l'element suivant fasse partie de ses successeurs (sinon en toute logique on a pas un chemin)

; on itere donc sur la liste et on procede a cette verification avec l'element suivant, si l'element
; suivant ne fait pas partie des successeurs de l'element actuel, alors on retire l'element actuel de la liste
; et on recommence la verification en reprenant au debut de la liste
; si on arrive a parcourir toute la liste avec la condition verifiee, alors la liste des etats parcourus
; est notre chemin final

; etant donne que chaque element n'apparait qu'une fois dans la liste (grace a l'enregistrement des etats),
; on peut recuperer l'element suivant de "current" dans la liste "new" avec (cadr (member current new))

(defun clean (chemin laby)
  (let ((succ NIL)
        (new chemin)
        (termine NIL)
        (current NIL))

    (while (not termine)
      ; (format t "~&Liste actuelle : ~s~&" new) ; decommenter les lignes avec "format" pour voir les etapes
      (setq termine T)
      (setq current (car new))
      (while (and termine (not (equal (cadr (member current new)) (car (last new)))))
        (setq succ (successeurs current laby))

        (if (not (member (cadr (member current new)) succ))
          (progn
            ; (format t "~s -/-> ~s~&" current (cadr (member current new)))
            ; (format t "~s n'a pas pour successeur ~s~2&" current (cadr (member current new)))
            (setq termine NIL)
            (setq new (remove current new))
          )
          (progn
            ; (format t "~s ---> " current)
            (setq current (cadr (member current new)))
          )
        )

      )
    )
    (format t "~s~2&" (car (last new)))
    new
  )
)


; TESTS

;(glouton *laby1* 'B1 'A4)
;(glouton *laby2* 'B1 'C4)  ; passe par la droite, on trouve une solution mais elle n'est pas optimale
;(A* *laby1* 'B1 'A4)
;(A* *laby2* 'B1 'C4)   ; passe par la gauche, on trouve une solution optimale


; sans clean, l'appel (A* *laby2* 'B1 'C4) retournerait (B1 C1 C2 A1 A2 A3 A4 B4 C4),
; soit le chemin final + les etats parcourus C1 et C2, qui eux ne font pas partie du chemin final.
; En se servant de ce resultat et en DECOMMENTANT les lignes servant a l'affichage
; dans la fonction clean, on peut voir le detail de son fonctionnement :

; (clean '(B1 C1 C2 A1 A2 A3 A4 B4 C4) *laby2*)




