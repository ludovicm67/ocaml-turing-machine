(* On créer les différents types nécessaires pour la suite *)
type direction   = G | D;;
type symbole     = char;;
type position    = int;;
type etat        = int;;
type ruban       = position -> symbole;;
type etat_global = {
  etatCourant:  etat;
  ruban:      ruban;
  positionTete: position;
};;
type regle = {
  etatDepart:    etat;
  etatArrive:    etat;
  symboleDepart: symbole;
  symboleArrive: symbole;
  dir:           direction;
};;
type programme = regle list;;
type tm = {
  etatInitial: etat;
  etatsFinaux: etat list;
  prog:        programme;
};;
