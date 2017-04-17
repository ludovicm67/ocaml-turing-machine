(*
   On teste avec une machine qui supprime le '0' central
   qui ajoute deux '0' à la fin, et qui replace la tête
   de lecture au début du ruban (sur le premier '1')
*)
let testRuban (p:position) =
  match p with
  | -7 -> '0'
  | -6 -> '1'
  | -5 -> '1'
  | -4 -> '1'
  | -3 -> '1'
  | -2 -> '1'
  | -1 -> '0'
  | 0  -> '1'
  | 1  -> '1'
  | 2  -> '1'
  | 3  -> '1'
  | 4  -> '1'
  | 5  -> '1'
  | 6  -> '1'
  | 7  -> '0'
  | _  -> '#'
;;

let testEtatGlobal = {
  etatCourant  = 0;
  ruban        = testRuban;
  positionTete = 0;
};;

let testProgramme = [

  {
    etatDepart    = 0;
    etatArrive    = 1;
    symboleDepart = '1';
    symboleArrive = '1';
    dir           = G;
  };

  {
    etatDepart    = 1;
    etatArrive    = 2;
    symboleDepart = '0';
    symboleArrive = '1';
    dir           = D;
  };

  {
    etatDepart    = 2;
    etatArrive    = 2;
    symboleDepart = '1';
    symboleArrive = '1';
    dir           = D;
  };

  {
    etatDepart    = 2;
    etatArrive    = 3;
    symboleDepart = '0';
    symboleArrive = '0';
    dir           = G;
  };

  {
    etatDepart    = 3;
    etatArrive    = 4;
    symboleDepart = '1';
    symboleArrive = '0';
    dir           = G;
  };

  {
    etatDepart    = 4;
    etatArrive    = 5;
    symboleDepart = '1';
    symboleArrive = '0';
    dir           = G;
  };

  {
    etatDepart    = 5;
    etatArrive    = 5;
    symboleDepart = '1';
    symboleArrive = '1';
    dir           = G;
  };

  {
    etatDepart    = 5;
    etatArrive    = 6;
    symboleDepart = '0';
    symboleArrive = '0';
    dir           = D;
  };

];;

let testMachine = {
  etatInitial = 0;
  etatsFinaux = [6];
  prog        = testProgramme;
};;
