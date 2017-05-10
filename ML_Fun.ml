(* ML functions in order of oldest to newest from spring 2017 *)
(* moves the first value to the end ofthe list *)
fun cycle1(x) = tl(x)@(hd(x)::[]);

(* remove the second digit in the tuple *)
fun rem(x) = (hd(x)::[])@tl(tl(x));

(* reverses the order of the digits in the tuple *)
fun order(x,y) = if x < y
				 then (x,y)
				 else (y,x);
(*  cycles the list n times*)
fun cycle( L,0 ) = L
  | cycle( L,n ) = cycle( tl(L)@(hd(L)::nil), n-1);


(* flip adjacent values in a list *)
fun flip nil        = nil
  | flip(h::nil)    = [h]
  | flip(h::t::nil) = [t]@[h]
  | flip(h::s::t)   = [s,h] @ flip(t);

(* high value in list Feb 9th*)
fun high(a::nil)    = a
  | high(a::b::c)   = if a>b
  					  then high(a::c)
  					  else high(b::c);
(* zip Feb 9th*)
fun zip (nil,nil) = nil
  | zip (a,b)     = (hd a,hd b)::zip(tl a, tl b);

(* (((((((((((()))))))))))) DOESN'T WORK YET!!!!!(((((((((((()))))))))))) *)
(* delete kth value in list Feb 9th*)
fun del(L,0)  = hd L :: tl(tl L)
  | del(L,n)  = del(  (hd(L)::tl(L) ), n-1);
(* prefix Feb 9th*)
fun prefix(nil,b) = true
  | prefix(a,nil) = false
  | prefix(a,b)   = if (hd a = hd b)
  				    then prefix(tl a,tl b)
  				    else false;
(* suffix *)
fun suffix([],b) = true
  | suffix(a,[]) = false
  | suffix(a,b)  = (a=b) orelse suffix(a,tl b);
(* merge Feb 16th*)
fun merge(nil,b) = b
  | merge(a,nil) = a
  | merge(a,b)   = if(hd a < hd b)
                      then (hd a)::merge(b, tl a)
                      else (hd b)::merge(tl b, a);
(* turn all integers to reals Feb 16th*)
val realize = map (fn (x) => real(x));
(* High value in list v2 Feb 16th*)
fun max a = foldr (fn(a,b) => if a > b then a else b) (hd a) a;
(* We wrote a merge sortish thing in class it is really an insertion sort*)
fun mlergesort L = 
    let val singletons = map ( fn x => [x]) L
    in foldl merge nil singletons
    end;

(* ------------------------------------- *)
(* Turn this set in on the 23rd together *)
(* ------------------------------------- *)
          (* xor bool list Feb 23rd *)
fun xor L = foldr(fn(a,b) => 
            if (a = true andalso b = false) orelse (b = true andalso a = false) 
            then true 
            else false ) false L; 
          (* formerly negative intergers Feb 23rd *)
val wasNegative =
    let val onlyNeg = List.filter (fn n => n > 0)
    in  onlyNeg o map (fn x => ~1 *(x))
    end;
          (* Sum alternating values  Feb 23rd *)
          (* doesn't work *)
(* fun sum L = 
    let val foldr((a,b)) => 
    in 
    end;
          (* Unzip Feb 23rd *)
          (* doesn't work *)
fun unzip 
    let val lista = [];
        val listb = [];
        fun lists((a,b)) = (a::lista, b::listb)
    in map lists(hd L) 
    end; *)

(* ------------------------------------ *)
(* Turn this set in on the 2nd together *)
(* ------------------------------------ *)

(* first match from list Mar 2nd*)
fun first comp nil = NONE
  | first comp (x::xs) = 
  if comp(x)
        then SOME x
        else first comp xs;
(* last match from list Mar 2nd*)
fun last comp L =
  foldr(fn (x,y) => 
    if isSome(y)
    then y
    else if comp(x)
       then SOME x
       else NONE
    ) NONE L;

(* int to string Mar 2nd*)
fun intstr x = 
  let val toChar = (x mod 10) + 48
  in if x < 0 
  then "~" ^ intstr(x * ~1)
  else if x>=10
     then intstr(x div 10) ^ str(chr(toChar))
     else str(chr(toChar))
  end;

(* listify Mar 2nd *)
fun listify nil = nil
  |   listify [x] = [[x]]
  |   listify (x::y::z) =
      let val head::tail = listify (y::z)
    in if x <= y 
       then (x::head)::tail
       else [x]::head::tail
    end;

(* remove zero *)
fun remzero nil = nil
  | remzero L  = List.filter(fn x => x <> 0) L;