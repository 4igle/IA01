(defparameter *frames* NIL)

(setq ETRE '(ETRE
              (TYPE (VALUE CONCEPT))
              (NAME (IF-NEEDED ask-user))))
(pushnew ETRE *frames*)

(setq ELEPHANT
 '(ELEPHANT
       (TYPE (VALUE CONCEPT))
       (IS-A (VALUE ETRE))
       (COLOR (DEFAULT "grey"))
       (AGE (IF-NEEDED ask-user)
            (IF-ADDED check-age))
       (POIDS (IF-NEEDED computer-weight-from-age))
       (AFFICHAGE (IF-ADDED draw-elephant)
                  (IF-REMOVED erase-elephant))))
(pushnew ELEPHANT *frames*)



(defun make-individu (concept name caract)
  (let ((nom NIL)(slots_allowed NIL)(caracteristiques caract)(slot NIL)(value NIL)(demon NIL)(frame NIL))
    (if (assoc concept *frames*)
      (progn
        (setq slots_allowed (get_slots_allowed concept))
        (while caracteristiques
          (progn
            (setq slot (pop caracteristiques))
            (setq value (pop caracteristiques))
            (if (not (member slot slots_Allowed))
              (progn
                (format t "Erreur le slot \"~s\" n'est pas valide pour \"~s\"~&" slot concept)
                (return-from make-individu NIL))
              (progn
                (setq demon (get_demon slot 'IF-ADDED concept))
                (if demon
                  (if (eq (funcall demon value) NIL)
                    (progn
                      (format t "Erreur \"~s\" n'est pas une valeur valide pour \"~s\"~&" value slot)
                      (return-from make-individu NIL))))
                (push (list slot (list 'value value)) frame)))))

        (setq nom (gentemp name))
        (push (list 'is-a (list 'value concept)) frame)
        (push (list 'type (list 'value 'individu)) frame)
        (push nom frame)
        (pushnew frame *frames*))

      (progn
        (format t "Erreur, le concept \"~s\" n'est pas dans la liste des frames~&" concept)
        NIL))))


(defun get_slots_allowed (concept)
  (let ((liste_slots NIL)(contenu_concept (cdr (assoc concept *frames*)))(concept_pere NIL))
    (loop for slot in contenu_concept do
      (pushnew (car slot) liste_slots))

    (setq concept_pere (assoc 'IS-A contenu_concept))
    (if concept_pere
      (append liste_slots (get_slots_allowed (cadadr concept_pere)))
      liste_slots)))

(defun get_demon (slot_name type_demon concept)
  (let* ((contenu_concept (cdr (assoc concept *frames*)))(slot (cdr (assoc slot_name contenu_concept)))(demon_name NIL)(concept_pere NIL))
    (setq demon_name (cadr (assoc type_demon slot)))

    (if demon_name
      demon_name
      (progn
        (setq concept_pere (assoc 'IS-A contenu_concept))
        (if concept_pere
          (get_demon slot_name type_demon concept_pere))))))

; On aurait pu redemander une valeur en cas de donnee incorrecte
; mais ma fonction read ne fonctionne pas
(defun check-age (age)
  (and (< 0 age)(> 150 age)))

(defun computer-weight-from-age (frame)
  (let ((contenu_frame NIL) (contenu_slot NIL) (age NIL))
    (setq contenu_frame (cdr (assoc frame *frames*)))
    (if contenu_frame
      (progn
        (setq contenu_slot (cdr (assoc 'AGE contenu_frame)))
        (if contenu_slot
          (progn
            (setq age (cadr (assoc 'VALUE contenu_slot)))
            (if age
              (+ (* age 1.5) 6))))))))

(defun get_slot_value (frame slot frame_original)
  (let ((contenu_frame (cdr (assoc frame *frames*))) (value NIL) (herit NIL))
    (if contenu_frame
      (progn
        (setq value
          (cond
            ((not (assoc slot contenu_frame)) NIL)
            ((equal (caadr (assoc slot contenu_frame)) 'VALUE) (cadadr (assoc slot contenu_frame)))
            ((equal (caadr (assoc slot contenu_frame)) 'DEFAULT) (cadadr (assoc slot contenu_frame)))
            ((get_demon slot 'IF-NEEDED frame) (funcall (get_demon slot 'IF-NEEDED frame) frame_original))))
        (if value
          value
          (progn
            (setq herit (cadadr (assoc 'IS-A contenu_frame)))
            (if herit
              (get_slot_value herit slot frame_original)
              (progn
                ; S'il n'y a pas de concept pere et qu'on n'a toujours pas trouve de slot correspondant, alors il n'existe pas
                (format t "Erreur, slot \"~s\" inexistant pour \"~s\"~&" slot frame_original)
                (return-from get_slot_value NIL))))))
      (progn
        (format t "Erreur, la frame \"~s\" n'est pas dans la liste des frames~&" frame)
        (return-from get_slot_value NIL)))))



;;;;;;;;;;; Tests make-individu ;;;;;;;;;;;;;;
; (make-individu 'ELEPHANT "CLYDE" '(NAME "Clyde" COLOR "grey" AGE 5)) ; Appel correct
; (make-individu 'ELEPHANT "BABAR" '(NAME "BABAR" AGE 10)) ; Appel correct

; (make-individu 'CHIEN "CLYDE" '(NAME "Clyde" COLOR "grey" AGE 5)) ; Erreur nom de concept
; (make-individu 'ELEPHANT "CLYDE" '(NAME "Clyde" CHAUSSURE "grey" AGE 5)) ; Erreur nom de slot
; (make-individu 'ELEPHANT "CLYDE" '(NAME "Clyde" COLOR "grey" AGE 500)) ; Erreur value de slot



;;;;;;;;;; Tests get_slot_value ;;;;;;;;;;;;
; Le nom peut varier avec le gentemp
; (get_slot_value 'CLYDE0 'AGE 'CLYDE0) ; Appel correct, grace a value
; (get_slot_value 'BABAR1 'COLOR 'BABAR1) ; Appel correct, grace a default
; (get_slot_value 'CLYDE0 'POIDS 'CLYDE0) ; Appel correct, grace au demon

; (get_slot_value 'CLYDE0 'CHAUSSURE 'CLYDE0) ; Erreur, slot inexistant pour le frame et ses concepts peres
; (get_slot_value 'DUMBO2 'AGE 'DUMBO2) ; Erreur, pas dans la liste de frames

