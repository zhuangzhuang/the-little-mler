datatype 'a list = 
	Empty
	| Cons of 'a * 'a list

datatype orapl = 
	Orange
	|Apple

fun eq_orapl(Orange, Orange)	
	= true
| eq_orapl(Apple, Apple)
	= true
| eq_orapl(one, onther)
	= false;

(*
fun subst_int(n, a, Empty)
	= Empty
| subst_int(n, a, Cons(e, t))
	= if eq_int(a, e)
		then Cons(n, subst_int(n, a, t))
		else Cons(e, subst_int(n, a, t));
*)

fun subst_orapl(n, a, Empty)
	= Empty
| subst_orapl(n, a, Cons(e, t))
	= if eq_orapl(a, e)
		then Cons(n, subst_orapl(n, a, t))
		else Cons(e, subst_orapl(n, a, t));

fun subst(rel, n, a, Empty)		
	= Empty
| subst(rel, n, a, Cons(e, t))
	= if rel(a, e)
		then Cons(n, subst(rel, n, a, t))
		else Cons(e, subst(rel, n, a, t));


fun less_then(n: int, m: int) = (n<m);

fun in_range((small, large), x)
	= if less_then(small, x)
		then less_then(x, large)
		else false;

fun subst_pred(pred, n, Empty)		
	= Empty
| subst_pred(pred, n, Cons(e, t))
	= if pred(e)
		then Cons(n, subst_pred(pred, n, t))
		else Cons(e, subst_pred(pred, n, t));

fun in_range_c(small, large)(x)
	= if less_then(small, x)
		then less_then(x, large)
		else false;

fun subst_c(pred)(n, Empty)
	= Empty
| subst_c(pred)(n, Cons(e,t))
	= if pred(e)
		then Cons(n, subst_c(pred)(n, t))
		else Cons(e, subst_c(pred)(n, t));

fun combine(Empty, l2)
	= l2
| combine(Cons(a, l1), l2)
	= Cons(a, combine(l1, l2));

fun combine_c(Empty)(l2)
	= l2
| combine_c(Cons(a, l1))(l2)
	= Cons(a, combine_c(l1)(l2));

fun base(l2)
	= l2;

fun combine_s(Empty) 
	= base
| combine_s(Cons(a, l1))
	= make_cons(a, combine_s(l1))
and
	make_cons(a, f)(l2)
		= Cons(a, f(l2));
		