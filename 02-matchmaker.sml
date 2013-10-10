datatype shish_kebab = 
	Skewer
	| Onion of shish_kebab
	| Lamb  of shish_kebab
	| Tomato of shish_kebab

val s0 = Skewer
val s2 = Onion(Skewer)

(* 测试 *)
fun only_onions(Skewer)
	= true
| only_onions(Onion(x))
	= only_onions(x)
| only_onions(Lamb(x))
	= false
| only_onions(Tomato(x))
	= false;

val o1 = only_onions(Skewer)
val o2 = only_onions(Onion(Skewer))
val o3 = only_onions(Onion(Onion(Skewer)))

val o4 = only_onions(Onion(Lamb(Skewer)))

(*素食*)
fun is_vegetarian(Skewer)
	= true
| is_vegetarian(Onion(x))
	= is_vegetarian(x)
| is_vegetarian(Lamb(x))
    = false
| is_vegetarian(Tomato(x))
    = is_vegetarian(x);


 datatype 'a shish = 
 	Botton   of 'a
 	| Onion  of 'a shish
 	| Lamb   of 'a shish
 	| Tomato of 'a shish

 datatype rod = 
 	Dagger
 	| Fork
 	| Sword

val s0 = Onion(Tomato(Botton(Dagger))) 

(* 泛型了 Botton(x) *)
fun is_veggie(Botton(x))
	= true
| is_veggie(Onion(x))
	= is_veggie(x)
| is_veggie(Lamb(x))
	= false
| is_veggie(Tomato(x))
	= is_veggie(x);


(*获取最地下的*)
fun what_bottom(Botton(x))
	= x
| what_bottom(Onion(x))
	= what_bottom(x)
| what_bottom(Lamb(x))
	= what_bottom(x)
| what_bottom(Tomato(x))
	= what_bottom(x);