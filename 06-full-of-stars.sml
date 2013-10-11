datatype fruit = 
	Peach
	| Apple
	| Pear
	| Lemon
	| Fig

datatype tree =
	Bud
	| Flat  of fruit * tree
	| Split of tree * tree

fun flat_only(Bud) = 
	true
| flat_only(Flat(n, t)) = 
	flat_only(t)
| flat_only(Split(n, m)) = 
	false;

fun split_only(Bud)
	= true
| split_only(Flat(f, t))
	= false
| split_only(Split(s, t))
	= if split_only(s)
		then split_only(t)
		else false;

fun contains_fruit(Bud)		
	= false
| contains_fruit(Flat(f, t))
	= true
| contains_fruit(Split(s, t))
	= if contains_fruit(s)
		then true
		else contains_fruit(t);

(*直接复用*)
fun contains_fruit(x)
	= if split_only(x)
		then false
		else true;

fun contains_fruit(x) = not(split_only(x));

fun less_then(n: int, m: int) = n<m;
fun larger_of(n, m) 
	= if less_then(n, m)
		then m
		else n;

fun height(Bud)		
	= 0
| height(Flat(f, t))
	= 1 + height(t)
| height(Split(s, t))
	= 1 + larger_of(height(s), height(t));

fun eq_fruit(Peach, Peach)
	= true
| eq_fruit(Apple, Apple)
	= true
| eq_fruit(Pear, Pear)
	= true
| eq_fruit(Lemon, Lemon)
	= true
| eq_fruit(Fig, Fig)
	= true
| eq_fruit(a_fruit, another_fruit)
	= false;

fun subset_in_tree(n, a, Bud)
	= Bud
| subset_in_tree(n, a, Flat(f, t))
	= if eq_fruit(f, a)
		then Flat(n, subset_in_tree(n, a, t))
		else Flat(f, subset_in_tree(n, a, t))
| subset_in_tree(n, a, Split(s, t))
	= Split(
		subset_in_tree(n, a, s),
		subset_in_tree(n, a, t));

fun occurs(a, Bud)
	= 0
| occurs(a, Flat(f, t))
	= if eq_fruit(f, a)
		then 1 + occurs(a, t)
		else occurs(a, t)
| occurs(a, Split(s, t))
	= occurs(a, s) + occurs(a, t);

(* 模拟出了Scheme中的列表 *)
datatype 
	'a slist = 
		Empty
		| Scons of (('a sexp) * ('a slist))

and
	'a sexp = 
	An_atom of 'a
	| A_slist of ('a slist);

val s = Scons(An_atom(Fig), Empty)

fun occurs_in_slist(a, Empty)
	= 0
| occurs_in_slist(a, Scons(s, y))
	= occurs_in_sexp(a, s) +
	  occurs_in_slist(a, y)
and
	occurs_in_sexp(a, An_atom(b))
	= if eq_fruit(b, a)
		then 1
		else 0
	| occurs_in_sexp(a, A_slist(y))
		= occurs_in_slist(a, y);

fun subset_in_slist(n, a, Empty)
	= Empty
| subset_in_slist(n, a, Scons(s, y))
	= Scons(
		subset_in_sexp(n, a, s),
		subset_in_slist(n, a, y))
and
	subset_in_sexp(n, a, An_atom(b))
	= if eq_fruit(b, a)
		then An_atom(n)
		else An_atom(b)
| subset_in_sexp(n, a, A_slist(y))
	= A_slist(subset_in_slist(n, a, y));

fun rem_from_slist(a, Empty)
	= Empty
| rem_from_slist(a, Scons(s, y))
	= 
and
	rem_from_sexp(a, An_atom(b))
	= 
| rem_from_sexp(a, A_slist(y))
	= A_slist(rem_from_slist(a, y));