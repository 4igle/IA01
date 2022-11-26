;; noeud : Ni ->(('etiquette ...)('type ...))

(defvar *nodes* NIL)
(defvar *arcs* NIL)

(defun defnode (type nom)
  (if (and (boundp nom) (eq 'concept type))
    NIL
    (let ((noeud (gentemp "N")))
      (set noeud (list (cons 'type type)(cons 'etiquette nom)))

      (cond
        ((and (boundp nom) (eq 'individu type))
          (set nom (cons noeud (eval nom))))
        (T
          (set nom (list noeud)))
      )

      (push noeud *nodes*)
      noeud
    )
  )
)


(defun defarc (type from_node to_node)
  (if (null from_node) (error "Pas de from_node"))
  (if (null to_node) (error "Pas de to_node"))
  (let ((arc (gentemp "A"))
    (from_node (car from_node))
    (to_node  (car to_node)))

    (set arc `((type ,type)(from_node ,from_node)(to_node ,to_node)))

    (unless (member type *arcs*) (push type *arcs*))

    (if (boundp type)
      (set type (cons arc (eval type)))
      (set type (list arc))
    )
    arc
  )
)

(defun mark-node (node marque)
  (if (boundp marque)
    (if (not (member node (eval marque)))
      (set marque (cons node (eval marque)))
    )
    (set marque (list node))
  )
)

(defun wave (current-node marque sens)
  (let
    ((current current-node)
    (autre-sens (cond ((eq sens 'TO_NODE) 'FROM_NODE)(t 'TO_NODE)))
    (next-node NIL))

    (if (boundp current)
      (progn
        (loop for node in (eval current) do
          (mark-node node marque)
        )
        (loop for arc in IS-A do
          (if (eq current (cdr (assoc 'ETIQUETTE (eval (cadr (assoc sens (eval arc)))))))
            (progn
              (setq next-node (cadr (assoc autre-sens (eval arc))))
              (mark-node next-node marque)
              (wave (cdr (assoc 'ETIQUETTE (eval next-node))) marque sens)
            )
          )
        )
      )
      NIL
    )
  )
)


(defun get-results (relation type1 type2)
  (let ((M1 (gentemp "M"))(M2 (gentemp "M"))(results NIL))
    (wave type1 M1 'TO_NODE)
    (wave type2 M2 'TO_NODE)

    (loop for arc in relation do
      (if (and
            (member (cadr (assoc 'FROM_NODE (eval arc))) (eval M1))
            (member (cadr (assoc 'TO_NODE (eval arc))) (eval M2)))
          (push (cadr (assoc 'TO_NODE (eval arc))) results)
      )
    )
    results
  )
)

;; tests

(defvar *nodes* NIL)
(defvar *arcs* NIL)

(defnode 'concept 'noble)
(defnode 'concept 'compte)
(defnode 'concept 'CDG)
(defnode 'concept 'soldat)
(defnode 'concept 'mondaine)

(defnode 'individu 'roxane)
(defnode 'individu 'cyrano)
(defnode 'individu 'christian)
(defnode 'individu 'deguiche)


(defarc 'aime deguiche roxane)
(defarc 'aime cyrano roxane)
(defarc 'aime christian roxane)
(defarc 'aime roxane christian)

(defarc 'prete-a-aider roxane CDG)


(defarc 'is-a CDG noble)
(defarc 'is-a compte noble)
(defarc 'is-a deguiche compte)
(defarc 'is-a cyrano CDG)
(defarc 'is-a christian CDG)
(defarc 'is-a cyrano soldat)
(defarc 'is-a christian soldat)
(defarc 'is-a roxane mondaine)



;(wave 'noble 'M1 'TO_NODE)
;(wave 'roxane 'M2 'TO_NODE)

(get-results AIME 'ROXANE 'NOBLE) ;; -> quels nobles aime roxane ?

