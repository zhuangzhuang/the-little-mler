datatype meza = 
	Shrimp
	| Calamari
	| Escargots	
	| Hummus

datatype main = 
	Steak
	| Ravioli
	| Chicken	
	| Eggplant

datatype salad = 
	Green
	| Cucumber
	| Greek

datatype dessert = 
	Sundae
	| Mousse
	| Torte

val m0 = (Hummus, Steak, Green, Torte)


fun add_a_steak(Shrimp)
	= (Shrimp, Steak)
| add_a_steak(Calamari)
	= (Calamari, Steak)
| add_a_steak(Escargots)
	= (Escargots, Steak)
| add_a_steak(Hummus)
	= (Hummus, Steak);

fun add_a_steak(x: meza) : (meza * main)
	= (x, Steak);

fun eq_main(Steak, Steak)
	= true
| eq_main(Ravioli, Ravioli)
 	= true
| eq_main(Chicken, Chicken)
	= true
| eq_main(Eggplant, Eggplant)
	= true
| eq_main(a_main, anther_main)
	= false;

fun has_steak(a: meza, Steak, d: dessert) : bool
	= true
| has_steak(a: meza, ns, d: dessert): bool
	= false;