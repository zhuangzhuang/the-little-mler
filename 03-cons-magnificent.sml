datatype pizza = 
	Crust
	| Cheese of pizza
	| Onion of pizza
	| Anchovy of pizza
	| Sausage of pizza

fun remove_anchovy(Crust)	
	= Crust
| remove_anchovy(Cheese(x))
	= Cheese(remove_anchovy(x))
| remove_anchovy(Onion(x))
	= Onion(remove_anchovy(x))
| remove_anchovy(Anchovy(x))
	= remove_anchovy(x)
| remove_anchovy(Sausage(x))
	= Sausage(remove_anchovy(x));

val r1 = remove_anchovy(Crust)
val r2 = remove_anchovy(Cheese(Crust))
val r3 = remove_anchovy(Cheese(Anchovy(Onion(Crust))))


fun top_anchovy_with_cheese(Crust)
	= Crust
| top_anchovy_with_cheese(Cheese(x))
	= Cheese(top_anchovy_with_cheese(x))
| top_anchovy_with_cheese(Onion(x))
	= Onion(top_anchovy_with_cheese(x))
| top_anchovy_with_cheese(Anchovy(x))
	= Cheese(Anchovy(top_anchovy_with_cheese(x)))
| top_anchovy_with_cheese(Sausage(x))
	= Sausage(top_anchovy_with_cheese(x));

(*直接组合了一个函数*)
fun subset_anchowy_by_cheese(x)
	= remove_anchovy(top_anchovy_with_cheese(x));

(* 重新定义自己的方法 *)
fun subset_anchowy_by_cheese(Crust)
	= Crust
| subset_anchowy_by_cheese(Cheese(x))
	= Cheese(subset_anchowy_by_cheese(x))
| subset_anchowy_by_cheese(Onion(x))
	= Onion(subset_anchowy_by_cheese(x))
| subset_anchowy_by_cheese(Anchovy(x))
	= Cheese(subset_anchowy_by_cheese(x))
| subset_anchowy_by_cheese(Sausage(x))
	= Sausage(subset_anchowy_by_cheese(x));