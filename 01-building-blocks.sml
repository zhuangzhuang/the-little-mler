(* 定义普通类型 *)
datatype seasoning = 
	  Salt
	| Pepper

val s = Pepper

(* 递归定义类型*)
datatype num = 
	Zero
	| One_more_than of num

val n0 = Zero
val n1 = One_more_than(Zero)
val n2 = One_more_than(
			One_more_than(
				Zero))

(*类型泛型*)
datatype 'a open_faced_sandwich = 
	Bread of   'a
	| Slice of 'a open_faced_sandwich

(*自动推断出类型*)
val o0 = Bread(0)
val o1 = Bread(true)
val o2 = Bread(One_more_than(Zero))

(*自己的类型*)
val o3 = Bread(Bread(0))
val o4 = Bread(Bread(One_more_than(Zero)))