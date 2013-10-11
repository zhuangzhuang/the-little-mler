datatype 'a list = 
	Empty
	| Cons of 'a * 'a list

datatype box = 
	Bacon
	| lx of int

fun is_bacon(Bacon)	
	= true
| is_bacon(lx(n))
	= false;

exception No_bacon of int

fun where_is(Empty)	
	= raise No_bacon(0)
| where_is(Cons(a_box, rest))
	= if is_bacon(a_box)
		then 1
		else 1 + where_is(rest);

val w=(where_is(
	Cons(lx(5),
		Empty))
	handle
		No_bacon(an_int)
			=> an_int)


exception Out_of_range

fun eq_int(n: int, m: int) = (n = m);

fun list_item(n, Empty)
	= raise Out_of_range
| list_item(n, Cons(abox, rest))
	= if eq_int(n, 1)
		then abox
		else list_item(n-1, rest);

fun find(n, boxes)	
	= (check(n, boxes, list_item(n, boxes))
		handle Out_of_range
		=> find(n div 2, boxes))
and	
	check(n, boxes, Bacon)
		= n
	| check(n, boxes, lx(i))
		= find(i, boxes);

fun path(n, boxes)		
	= Cons(n,
		(check(n, boxes, list_item(n, boxes))
			handle Out_of_range
			=> path(n div 2, boxes)))
and 
	check (n, boxes, Bacon)
		= Empty
	| check(n, boxes, lx(i))
		= path(i, boxes);