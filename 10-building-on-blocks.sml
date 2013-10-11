fun eq_int(n: int, m: int) = (n = m);

fun is_zero(n)
	= eq_int(n, 0);

exception Too_small

fun pred(n)
	= if eq_int(n, 0)
		then raise Too_small
		else n - 1

fun succ(n)
	= n + 1;

fun plus(n, m)
	= if is_zero(n)
		then m
		else succ(plus(pred(n), m));


(*自定义数字*)
datatype num = 
	Zero
	| One_more_than of num

fun is_zero(Zero)
	= true
| is_zero(not_zero)
	= false;

fun pred(Zero)
	= raise Too_small
| pred(One_more_than(n))
	= n;

fun succ(n)
	= One_more_than(n);

fun plus(n, m)
	= if is_zero(n)
		then m
		else succ(plus(pred(n), m));

signature N = 
	sig
		type number
		exception Too_small
		val succ: number -> number
		val pred: number -> number
		val is_zero: number -> bool
	end;	

functor NumberAsNumber() :>N = 
	struct
	    datatype num = 
	    	Zero
	    	| One_more_than of num
	    type number = num
	    exception Too_small
	    fun succ(n)
	    	= One_more_than(n)
	    fun pred(Zero)
	    	= raise Too_small
	    | pred(One_more_than(n))
	    	= n
	    fun is_zero(Zero)
	    	= true
	    | is_zero(a_num)
	    	= false
	end

functor NumberAsInt() :> N = 	
struct
    type number = int
    exception Too_small
    fun succ(n)
     	= n + 1
    fun pred(n)
    	= if eq_int(n, 0)
    		then raise Too_small
    	    else n - 1
   	fun is_zero(n)
   		= eq_int(n, 0)
end

structure IntStruct 
	= NumberAsInt();
structure NumStruct
	= NumberAsNumber();

signature P = 
	sig
	    type number
	    val plus: (number * number) -> number
	end;

functor PON(a_N: N) :> P where type number = a_N.number = 
struct
	type number = a_N.number
	fun plus(n, m)
		= if a_N.is_zero(n)
			then m
			else a_N.succ(
					plus(a_N.pred(n), m))
end;

(* 有问题
structure IntArith = 
	PON(structure a_N = IntStruct);
structure NumArith = 
	PON(structure a_N = NumStruct);
*)

signature N_C_R = 
	sig 
		type number
		exception Too_small
		val conceal: int    -> number
		val succ:    number -> number
		val pred:    number -> number
		val is_zero: number -> bool
		val reveal:  number -> int
	end;

(*reveal(conceal(x)) = x*)


functor NumberAsInt() :> N_C_R= 
struct
    type number = int
    exception Too_small
    fun conceal(n)
    	= n;
    fun succ(n)
    	= n + 1;
    fun  pred(n) 
    	= if eq_int(n, 0)
    		then raise Too_small
    		else n - 1;
    fun is_zero(n)
    	= eq_int(n, 0);
    fun reveal(n)
    	= n;
end;

functor  NumberAsNum() :> N_C_R = 
struct
    datatype num = 
	    	Zero
	    	| One_more_than of num
	    type number = num
	    exception Too_small
	    fun succ(n)
	    	= One_more_than(n);
	    fun pred(Zero)
	    	= raise Too_small
	    | pred(One_more_than(n))
	    	= n;
	    fun is_zero(Zero)
	    	= true
	    | is_zero(a_num)
	    	= false;

	    fun conceal(n)
	    	= if eq_int(n, 0)
	    		then Zero
	    		else One_more_than(
	    				conceal(n-1));
	    fun reveal(n)
	    	= if is_zero(n)
	    		then 0
	    		else 1 + reveal(pred(n));
end

structure IntStruct 
	= NumberAsInt();



