datatype 'a pizza = 
	Bottom
	| Topping of ('a * ('a pizza))

datatype fish = 
	Anchovy
	| Lox
	| Tuna

val t = Topping(Anchovy, Topping(Tuna, Bottom))
val t1 = Bottom


fun rem_anchovy(Bottom) 
	= Bottom
| rem_anchovy(Topping(Anchovy, p))
 	= rem_anchovy(p)
| rem_anchovy(Topping(Tuna, p))
	= Topping(Tuna, rem_anchovy(p))
| rem_anchovy(Topping(Lox, p))
	= Topping(Lox, rem_anchovy(p));

(*可以省略部分*)
fun rem_anchovy(Bottom) 
	= Bottom
| rem_anchovy(Topping(Anchovy, p))
 	= rem_anchovy(p)
| rem_anchovy(Topping(t, p))
	= Topping(t, rem_anchovy(p))


val r0 = rem_anchovy(Topping(Lox,
			 	Topping(Anchovy,
			 		Topping(Tuna,
			 			Topping(Anchovy,
			 				Bottom)))))

(*简单的方法*)
fun rem_fish(x, Bottom)
	= Bottom
| rem_fish(Tuna, Topping(Tuna, p))  (*Tuna*)
	= rem_fish(Tuna, p)
| rem_fish(Tuna, Topping(t, p))
	= Topping(t, rem_fish(Tuna, p))

| rem_fish(Anchovy, Topping(Anchovy, p)) (*Anchovy*)
	= rem_fish(Anchovy, p)
| rem_fish(Anchovy, Topping(t, p))
	= Topping(t, rem_fish(Anchovy, p))

| rem_fish(Lox, Topping(Lox, p)) (*Lox*)
	= rem_fish(Lox, p)
| rem_fish(Lox, Topping(t, p))
	= Topping(t, rem_fish(Lox, p));

(*完整 2^n + 1*)

(*简单 但是不合法*)
(*
fun rem_fish(x, Bottom)
	= Bottom
| rem_fish(x, Topping(x, p))
	= rem_fish(x, p)
| rem_fish(x, Topping(t, p))
	= Topping(t, rem_fish(x, p));
*)

fun eq_fish(Anchovy, Anchovy)
	= true
| eq_fish(Lox, Lox)
	= true
| eq_fish(Tuna, Tuna)
	= true
| eq_fish(a_fish, anther_fish)
	= false;

fun rem_fish(x, Bottom)
	= Bottom
| rem_fish(x, Topping(t, p))
	= if eq_fish(t, x)
		then rem_fish(x, p)
		else Topping(t, rem_fish(x, p))

fun eq_int(n: int, m: int) = (m = n);

fun rem_int(x, Bottom)
	= Bottom
| rem_int(x, Topping(t, p))
	= if eq_int(x, t)
		then rem_int(x, p)
		else Topping(t, rem_int(x, p));

fun subst_fish(n, a, Bottom)
	= Bottom
| subst_fish(n, a, Topping(t, p))
	= if eq_fish(t, a)
		then Topping(n, subst_fish(n, a, p))
		else Topping(t, subst_fish(n, a, p));

datatype num =
	Zero
	| One_mor_than of num

fun eq_num(Zero, Zero)		
	= true
| eq_num(One_mor_than(n), One_mor_than(m))
	= eq_num(n, m)
| eq_num(n, m)
	= false;