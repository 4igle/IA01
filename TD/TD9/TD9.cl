$1 ((nom "Albert")(type Personne)(age 26)(Frere $2 $3))
Personne ((type class)(rel Frere mere)(att nom age))
Etudiant ((type class)(is-a Personne)(rel univ)(attr n_etu))


Val par défaut, 2 solutions:
- Etudiant ((type class)(is-a Personne)(rel univ)(attr n_etu) (age (default 80)))
- age ((type class) (owner etudiant) (default 20))

Q5) 2 solutions :
  conditions -> méthodes qui vont vérifier
  classe mere abstraite avec classes filles homme/femme