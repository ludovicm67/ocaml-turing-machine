(* Permet d'afficher le ruban actuel, avec la position du curseur *)
let affiche_etat_global (eg:etat_global) =

	(* On affiche les symboles du ruban *)
	for i = (eg.positionTete-15) to (eg.positionTete+15) do
		print_char(eg.ruban (i))
	done;

	print_newline();

	(* On affiche le curseur *)
	for j = eg.positionTete to eg.positionTete+15 do
		print_string(" ");
	done;
	print_string("^");
	print_newline()

;;

(* Permet de trouver la règle appropriée *)
let rec rechercheRegle (p:programme) (ec:etat) (s:symbole) =
	match p with
	| t::r -> if(t.etatDepart = ec && t.symboleDepart == s)
			  then t, true
			  else rechercheRegle r ec s
	| [] -> {
				etatDepart    = 0;
				etatArrive    = 0;
				symboleDepart = '#';
				symboleArrive = '#';
				dir           = G;
			}, false
;;

(* Permet de créer un nouveau ruban *)
let constructRuban (r:ruban) (p:position) (s:symbole) =
	let newRuban (pos:position) =
		if pos = p then s else r pos
	in newRuban
;;

(* Vaut -1 si on se déplace à gauche, 1 sinon *)
let dirValue (d:direction) =
	match d with
	| G -> -1
	| _ -> 1
;;

(* On exécute la règle *)
let executeRegle (rub:ruban) (r:regle) (eg:etat_global) =
	{
		etatCourant  = r.etatArrive;
		ruban 		 = constructRuban (rub) (eg.positionTete) (r.symboleArrive);
		positionTete = dirValue(r.dir)+eg.positionTete;
	}
;;

(* On éxécute la règle suivante *)
let suivant (m:tm) (eg:etat_global) =
	let re, b = rechercheRegle m.prog eg.etatCourant (eg.ruban eg.positionTete) in
	if not b then eg, false
	else match m.prog with
	| t::r -> executeRegle eg.ruban re eg, true
	| _ -> eg, false
;;

(* On lance la machine de Turing *)
let rec run (m:tm) (eg:etat_global) =
	let newEg, b = suivant m eg in
	match b with
	| true ->
		affiche_etat_global eg;
		if not (List.mem eg.etatCourant m.etatsFinaux) then
			run m newEg
		else ()
	| _ -> ()
;;
