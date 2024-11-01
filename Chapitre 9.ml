Exercice 1 :
------------


1)

fonction (int -> int) -> (int -> bool)


let fzz = fun f a -> f (a + 1) = 0 ;;

fzz (fun x -> x * x) (3) ;;


2)

- Erreur de type

- g = fun (int -> bool)

- false


3)

a)
fonction int -> bool -> float


let virgule = fun

entier ->
	let flottant = float_of_int (entier)
	in
		fun
		
		| true -> flottant +. 0.5
		| _ -> flottant
;;


virgule (5) ;;

b)
fonction (int -> bool) -> float

let cinqMoitie = fun

f ->
	if f (5)
	then 5.5
	else 5.
;;


cinqMoitie (fun entier -> entier mod 2 = 0) ;;



Exercice 2 :
------------


2)

fonction (int -> int) -> int -> int


let sigma =
fun
f ->

	fun
		| borne
		when borne >= 0
		->
			let rec somme =
			fun
				(* Cas de base : n = 0 *)
				| 0 -> f (0)

				(* Cas récursif *)
				| iteration ->
					let appel = somme (iteration - 1)
					in
						appel + f (iteration)
			in
				somme (borne)
		
		(* Cas élémentaire : n < 0 *)
		| _ -> 0
;;


sigma (fun x -> x * x) (10) ;;
-> 385

sigma (fun x -> x * x) (-1) ;;
-> 0



Exercice 3 :
------------


1)

fonction ('_a -> bool) -> '_a list -> bool


let ilexiste =
fun
p ->
	let rec trouve =
	fun
		(* Cas récursif *)
		| (tete :: queue) ->
			p (tete)
			or trouve (queue)
			
		(* Cas de base : pas trouvé *)
		| _ -> false
	in
		trouve
;;


ilexiste (fun x -> x = 4) ([2; 4]) ;;
-> true

ilexiste (fun x -> x = 3) ([2; 4]) ;;
-> false


2)

fonction ('_a -> bool) -> '_a list -> bool


let qqsoit =
fun
p ->
	let rec tous =
	fun
		(* Cas récursif *)
		| (tete :: queue) ->
			p (tete)
			& tous (queue)
			
		(* Cas de base : [] *)
		| _ -> true
	in
		tous
;;


qqsoit (fun x -> x = 4) ([2; 4]) ;;
-> false

qqsoit (fun x -> x = 3) ([2; 4]) ;;
-> false

qqsoit (fun x -> x mod 2 = 0) ([2; 4]) ;;
-> true


3)

3.1)

fonction '_a -> '_a list -> bool


let estMembre =
fun
element ->
	fun
		liste ->
			ilexiste
				(fun x -> x = element)
				(liste)
;;


estMembre (4) ([1; 4; 2]) ;;
-> true

estMembre (3) ([1; 4; 2]) ;;
-> false

estMembre (0) ([]) ;;
-> false

3.2)

fonction '_a list -> '_a list -> bool


let estInclus =
fun
gauche ->
	fun
		droite ->
			qqsoit
			(fun x -> estMembre (x) (droite))
			(gauche)
;;


estInclus ([1; 4; 2]) ([2; 1; 5; 4]) ;;
-> true

estInclus ([1; 4; 2; 3]) ([2; 1; 4]) ;;
-> false

estInclus ([]) ([2; 1; 4]) ;;
-> true

estInclus ([2; 1; 4]) ([]) ;;
-> false

estInclus ([]) ([]) ;;
-> true


4)

fonction ('_a -> bool) -> '_a list -> '_a list


let filtrer =
fun
p ->
	let rec construire =
	fun
		(* Cas récursif *)
		| (tete :: queue) ->
			let appel = construire (queue)
			in
				if p (tete)
				then tete :: appel
				else appel
		
		(* Cas de base : [] *)
		| _ -> []
	in
		construire
;;


filtrer (fun x -> x mod 2 = 0) ([2; 1; 4]) ;;
-> [2; 4]

filtrer (fun x -> x mod 2 = 0) ([]) ;;
-> []


5)

fonction '_a list -> '_a list -> '_a list


let diffEns =
fun
	gauche ->
		fun
			droite ->
			
				(*
				Nous filtrons la liste gauche sur
				le prédicat de non appartenance à
				la liste droite.
				*)
				filtrer (
					fun
					x ->
						not (
							estMembre
							(x)
							(droite)
						)
				)
				(gauche)
;;


diffEns ([2; 1; 5; 4]) ([1; 4; 2]) ;;
-> [5]

diffEns ([1; 4; 2]) ([2; 1; 5; 4]) ;;
-> []



Exercice 4 :
------------


1)

type expression =
	| Const of int
	| Var of char
	| Add of expression * expression
	| Mult of expression * expression
	| Puiss of expression * expression
;;


1.1)

let e1 = Add (
	Const (1),
	Mult (
		Const (2),
		Puiss (
			Var (`x`),
			Const (3)
		)
	)
)
;;

let e2 = Add (
	Const (1),
	Puiss (
		Var (`a`),
		Const (2)
	)
)
;;


type liaison = {
	idL : char;
	valeur : int
} ;;

let envC = [
	{idL = `a`; valeur = 3};
	{idL = `b`; valeur = 4}
] ;;


1.2)

fonction char * liaison list -> int


let rec evalVar =
fun
	(* Cas de base : trouvée *)
	| (nom, tete :: queue)
	when nom = tete.idL
	->
		tete.valeur

	(* Cas récursif *)
	| (nom, _ :: queue) ->
		evalVar (nom, queue)
		
	(* Cas d'erreur : pas trouvée *)
	| _ -> failwith "Identificateur inconnu."
;;


evalVar (`a`, envC) ;;
-> 3

evalVar (`b`, envC) ;;
-> 4

evalVar (`x`, envC) ;;
-> Erreur


1.3)

1.3.1)

fonction int * int -> int


let rec puissanceNaivePos =
fun
	(* Cas de base : exposé nul *)
	| (exposant, 0) -> 1
	
	(* Cas récursif *)
	| (exposant, expose) ->
		let appel = puissanceNaivePos (exposant, expose - 1)
		in
			exposant * appel
;;


1.3.2)

fonction int * int -> int


let puissanceNaive =
fun
	(* Cas d'erreur : exposé négatif *)
	| (exposant, expose)
	when expose < 0
	-> failwith "La puissance donnée doit être postitive ou nulle."
	
	| (exposant, expose) ->
		puissanceNaivePos (exposant, expose)
;;


puissanceNaive (2, 5) ;;
-> 32

puissanceNaive (42, 0) ;;
-> 1

puissanceNaive (0, 42) ;;
-> 0

puissanceNaive (0, 1) ;;
-> 0

puissanceNaive (0, 0) ;;
-> 1


1.3.3)

fonction int * int -> int


let rec puissancePos =
fun
	(* Cas de base : exposé nul *)
	| (exposant, 0) -> 1
	
	(* Cas récursif : exposé imapair *)
	| (exposant, expose)
	when expose mod 2 = 1
	->
		let appel = puissancePos (exposant, expose - 1)
		in
			exposant * appel
		
	(* Cas récursif : exposé pair *)
	| (exposant, expose) ->
		puissancePos (exposant * exposant, expose / 2)
;;


1.3.4)

fonction int * int -> int


let puissance =
fun
	(* Cas d'erreur : exposé négatif *)
	| (exposant, expose)
	when expose < 0
	-> failwith "La puissance donnée doit être postitive ou nulle."
	
	| (exposant, expose) ->
		puissancePos (exposant, expose)
;;


puissance (2, 5) ;;
-> 32

puissance (42, 0) ;;
-> 1

puissance (0, 42) ;;
-> 0

puissance (0, 1) ;;
-> 0

puissance (0, 0) ;;
-> 1


1.4)

liaison list -> expression -> int


let evalExp =
fun
	environnement ->
		let rec calcul =
		fun
			(* Cas de base : constante *)
			| (Const (c)) -> c
			
			(* Cas de base : variable *)
			| (Var (v)) ->
				evalVar (v, environnement)
			
			(* Cas récursifs *)
			| (Add (gauche, droite)) ->
				calcul (gauche)
				+ calcul (droite)
			
			| (Mult (gauche, droite)) ->
				calcul (gauche)
				* calcul (droite)
			
			| (Puiss (exposant, expose)) ->
				puissance (
					calcul (exposant),
					calcul (expose)
				)
		in
			calcul
;;


evalExp (envC) (e1) ;;
-> Erreur

evalExp (envC) (e2) ;;
-> 10


2)

type definition =
{
	idD : char;
	exp : expression
}
;;


2.1)

liaison list -> definition -> liaison list


let ajoute =
fun
	environnement ->
		fun
			{
				idD = nom;
				exp = calcul
			}
			->
				{
					idL = nom;
					valeur = evalExp (environnement) (calcul)
				}
				:: environnement
;;


ajoute (envC) ({idD = `x`; exp = Add (Var `a`, Const (3))}) ;;


3)

type programme =
	| Elementaire of expression
	| DefGlobale of definition
	| DefLocale of definition * programme
;;


let p = DefLocale
(
	{
	idD = `x`;
	exp = Const (7)
	},
	DefLocale
	(
		{
			idD = `y`;
			exp = Add (Var (`x`), Const (3))
		},
		Elementaire (Add
		(
			Var (`x`),
			Mult
			(
				Const (3),
				Var (`y`)
			)
		)
		)
	)
)
;;


3.1)

fonction programme * liaison list -> int * liaison list


let rec evalProg =
fun
	(* Cas récursif *)
	| (DefLocale (loc, prog), environnement) ->
		let appel = evalProg
		(
			prog,
			ajoute (environnement) (loc)
		)
		in
			(
				fst (appel),
				environnement
			)
	
	(* Cas de base : calcul d'expression *)
	| (Elementaire (exp), environnement) ->
		(
			evalExp (environnement) (exp),
			environnement
		)
	
	(* Cas de base : calcul d'environnement *)
	| (DefGlobale (glo), environnement) ->
		(
			evalExp (environnement) (glo.exp),
			ajoute (environnement) (glo)
		)
;;


evalProg (Elementaire (e2), envC) ;;
-> 10, [{idL = `a`; valeur = 3}; {idL = `b`; valeur = 4}]

evalProg (DefGlobale ({idD = `x`; exp = e2}), envC) ;;
-> 10, [{idL = `x`; valeur = 10}; {idL = `a`; valeur = 3}; {idL = `b`; valeur = 4}]

evalProg (p, envC) ;;
-> 37, [{idL = `a`; valeur = 3}; {idL = `b`; valeur = 4}]
