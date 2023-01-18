(setq Chiffres '((0 (1 1 1 1 1 1 1 0) 0) ;; nb / x0...x7 / C res attendu
                 (1 (1 0 1 1 0 0 0 0) 1)
                 (2 (1 1 1 0 1 1 0 1) 0)
                 (3 (1 1 1 1 1 0 0 1) 1)
                 (4 (1 0 1 1 0 0 1 1) 0)
                 (5 (1 1 0 1 1 0 1 1) 1)
                 (6 (1 1 0 1 1 1 1 1) 0)
                 (7 (1 1 1 1 0 0 0 0) 1)
                 (8 (1 1 1 1 1 1 1 1) 0)
                 (9 (1 1 1 1 1 0 1 1) 1)))

(setq Poids '(1 1 1 1 1 1 1 1))


(defun Affichage (W X Os) ; W = poid, X entrée, Os = sortie calcule (Affichage proposé par Joseph)
  (format t "----- ~%Nombre : ~s ~% représentation ~s ~% poids : ~s ~% sortie attendue : ~s ~% sortie calculee : ~s ~%" (car x) (cadr x) w (caddr x) Os)
  (if (equal Os (caddr x))
      (format t "Resultat ok ~%")
      (format t "Resultat contradictoire~%")))


(defun somme (chiffre poids)
  (let ((somme 0))
    (dotimes (i 8)
      (setq somme (+ somme (* (nth i (cadr chiffre)) (nth i poids)))))
    somme))


(defun prediction (chiffre poids)
  (let ((somme (somme chiffre poids)))
    (if (> somme 0) 1 0)))


(defun Verification (Chiffres Poids)
    (let ((valide T)(i 1))
        (while (and valide (< i 10))
            (if (not (equal (prediction (nth i Chiffres) Poids) (caddr (nth i Chiffres))))
                (setq valide nil))
            (setq i (+ i 1)))
        valide))

(defun modification_poids (chiffre anciens_poids pred)
    (let ((attendu (caddr chiffre))(nouv_poids NIL)(modelisation_chiffre (cadr chiffre)))
        (dotimes (i 8)
            (setq nouv_poids (cons (+ (nth i anciens_poids)(* (- attendu pred) (nth i modelisation_chiffre))) nouv_poids)))
        (reverse nouv_poids)))
            


(defun Perceptron (S)
    (let ((poids NIL)(termine NIL)(nouveaux_poids NIL)(pred NIL))

    ;; Initialisation des poids
    (dotimes (i (length (cadr (car S))))
        (setq poids (cons 1 poids)))
    
    (while (not termine)
        (setq termine T)
        (loop for chiffre in S do
            (setq pred (prediction chiffre poids))
            (affichage poids chiffre pred)
            (setq nouveaux_poids (modification_poids chiffre poids pred))
            (format t "Nouveaux poids : ~s ~%" nouveaux_poids)
            (if (not (equal poids nouveaux_poids))
                (setq termine NIL))
            (setq poids nouveaux_poids))
        (if termine
            (return poids)))))


(Perceptron Chiffres)

;; Exemple de modèles qui fonctionnent
(Verification Chiffres '(0 1 0 1 0 -2 -1 0))
(Verification Chiffres '(0 1 0 1 0 -1 -1 0))
(Verification Chiffres '(1 2 -1 3 2 -6 -3 0))
(Verification Chiffres '(0 1 0 1 0 -2 -1 0))
(Verification Chiffres '(1 2 -1 1 2 -6 -3 0))


;; Exemple de modèle qui ne fonctionnent pas
(Verification Chiffres '(1 2 -3 4 0 0 -3 2))