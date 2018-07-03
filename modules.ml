(* "Once upon a time, there was a university with a peculiar tenure policy. All
 faculty were tenured, and could only be dismissed for moral turpitude. What
 was peculiar was the definition of moral turpitude: making a false statement
 in class. Needless to say, the university did not teach computer science.
 However, it had a renowned department of mathematics.

 One Semester, there was such a large enrollment in complex variables that two
 sections were scheduled. In one section, Professor Descartes announced that a
 complex number was an ordered pair of reals, and that two complex numbers were
 equal when their corresponding components were equal. He went on to explain
 how to convert reals into complex numbers, what "i" was, how to add, multiply,
 and conjugate complex numbers, and how to find their magnitude.

 In the other section, Professor Bessel announced that a complex number was an
 ordered pair of reals the first of which was nonnegative, and that two complex
 numbers were equal if their first components were equal and either the first
 components were zero or the second components differed by a multiple of 2π. He
 then told an entirely different story about converting reals, "i", addition,
 multiplication, conjugation, and magnitude.

 Then, after their first classes, an unfortunate mistake in the registrar's
 office caused the two sections to be interchanged. Despite this, neither
 Descartes nor Bessel ever committed moral turpitude, even though each was
 judged by the other's definitions. The reason was that they both had an
 intuitive understanding of type. Having defined complex numbers and the
 primitive operations upon them, thereafter they spoke at a level of
 abstraction that encompassed both of their definitions.

 The moral of this fable is that:
   Type structure is a syntactic discipline for enforcing levels of
   abstraction."

 from:
 John C. Reynolds, "Types, Abstraction, and Parametric Polymorphism", IFIP83

 (optional) homework: read the rest of the introduction of the paper at
 https://people.mpi-sws.org/~dreyer/tor/papers/reynolds.pdf
 *)


(* Complex numbers are more complicated than natural numbers.
   Let's start with Nat! *)


(* Define a module type "NAT" that specifies the structure of a "natural
 numbers structure". It should have a carrier type, a function that tests for
 equality, a zero and a one element, addition, subtraction, and multiplication
 operations and conversion functions from and to OCaml's "int" type. *)
module type NAT = sig
    type t
    val eq   : t -> t -> bool
    val zero : t
    val one : t
	val add : t -> t -> t
	val sub : t -> t -> t
	val mul : t -> t -> t
	val int_to_nat : int -> t
	val nat_to_int : t -> int
  end

(* Write a module that implements the NAT signature, using OCaml's "int" type
   as carrier.

   Trick : until you're done implementing Nat_int, it won't have the required
   signature. You can add stubs with `failwith "later"' to make the compiler
   happy and leave a note for yourself. *)

module NAT_int : NAT = struct
  type t = int
  let eq = (=)
  let zero = 0
  let one = 1
  let add a b = a + b
  let sub a b = if a < b
		then zero
		else a - b
  let mul a b = a * b
  let int_to_nat a = if a < 0
	then 0
	else a
  let nat_to_int a = a
end


(* Write another implementation of NAT, taking inspiration from the Peano
 axioms: https://en.wikipedia.org/wiki/Peano_axioms
 - The carrier type is given by a variant type that says that a natural number
   is either zero or the successor of another natural number.
 - Equality of k and l is decided by recursion on both k and l. The base case
   is that Zero = Zero.
 - All the other functions are also defined by structural recursion (c.f.
   Wikipedia).
 *)


module NAT_P : NAT = struct
  type t = Zero | Succesor of t
  let rec eq a b =
	match a, b with
	| (Zero, Zero) -> true
	| (Zero, Succesor _) -> false
	| (Succesor _, Zero) -> false
	| (Succesor x, Succesor y) -> eq x y

  let zero = Zero

  let one = Succesor Zero

  let rec add a b =
	match a, b with
	| (x, Zero) -> x
	| (x, Succesor y) -> add (Succesor x) y

  let rec sub a b =
	match a, b with
	| (Zero, Succesor _) -> Zero
	| (x, Zero) -> x
	| (Succesor x, Succesor y) -> sub x y

  let rec mul a b =
	match a, b with
	| (_, Zero) | (Zero, _) -> Zero
	| (x, Succesor y) -> add x (mul x y)

  let int_to_nat a = if a < 0
	then Zero
	else
	let rec inting num acc =
		match num with
		| 0 -> acc
		| x -> inting (x-1) (Succesor acc)
	in inting a Zero

  let nat_to_int a =
	let rec natting nat acc =
		match nat with
		| Zero -> acc
		| Succesor x -> natting x (acc + 1)
	in natting a 0
end

(* For those wishing to reenact the glory of 17th century mathematics:
   Follow the fable told by John Reynolds in the introduction. *)

(* Define the signature of a module of complex numbers.
   We will need a carrier type, a test for equality, zero, one, i, negation and
   conjugation, addition, multiplication, division, and taking the inverse.
 *)
module type COMPLEX = sig
    type t
    val eq : t -> t -> bool
    val zero : t
    val one : t
    val i : t
    val neg : t -> t
    val con : t -> t
    val add : t -> t -> t
    (*val sub : t -> t -> t*)
    val mul : t -> t -> t
    val div : t -> t -> t
    val inv : t -> t
  end


(* Write an implementation of Professor Descartes's complex numbers. Reminder:
 this should be the cartesian representation (latin_of_french "Descartes" =
 "Cartesius").

  Recommendation: implement a few basic parts of the module but leave division
  for later. It's relatively messy.
 *)

module Cartesian : COMPLEX = struct

  type t = {re : float; im : float}

  let eq x y = x.re = y.re && x.im = y.im

  let zero = {re=0.0; im=0.0}
  let one = {re=1.0; im=0.0}
  let i = {re=0.0; im=1.0}

  let neg x = {re = (-1.0) *. x.re; im = (-1.0) *. x.im}
  let con x = {re =  x.re; im = (-1.0) *. x.im}

  let add x y = {re = x.re +. y.re; im = x.im +. y.im}
  (*let sub x y = {re = x.re -. y.re; im = x.im -. y.im}*)
  let mul x y = {re = x.re *. y.re -. x.im *. y.im;
                im = x.re *. y.im +. x.im *. y.re}
  let div x y = {re = (x.re *. y.re +. x.im *. y.im) /. (y.re *. y.re +. y.im *. y.im);
                im = (y.re *. x.im -. x.re *. y.im) /. (y.re *. y.re +. y.im *. y.im)}
  let inv x = {re = x.re /. (x.re *. x.re +. x.im *.x.im);
              im = x.im /. (x.re *. x.re +. x.im *.x.im)}

end



(* Now implement Professor Bessel's complex numbers. The carrier this time
   will be a polar representation, with a magnitude and an argument for each
   complex number.

   Recommendation: First implement equality, the constants, negation, and
   multiplication. Then the rest except for addition. So far, so pleasant.
   Finally implement addition. Now form an opinion on why nobody likes polar
   coordinates. *)

module Polar : COMPLEX = struct

  type t = {magn : float; arg : float}

  let pi = 2. *. acos 0.
  let rad_of_deg deg = (deg /. 180.) *. pi
  let deg_of_rad rad = (rad /. pi) *. 180.

  let zero = {magn=0.0; arg=0.0}
  let one = {magn=1.0; arg=0.0}
  let i = {magn=1.0; arg = pi /. 2.0}

  let eq x y = x.magn = y.magn && x.arg = y.arg

  let neg x = {magn = x.magn; arg = if x.arg >= pi
                                    then x.arg -. pi
                                    else x.arg +. pi}
  let con x = {magn = x.magn; arg = 2.0 *. pi -. x.arg}

  let add x y = {magn = sqrt (x.magn *. x.magn +. y.magn *. y.magn +. 2. *. x.magn *. y.magn *. cos (if x.arg -. y.arg < -1. *. pi then 2. *. pi +. x.arg -. y.arg else x.arg -. y.arg));
                arg = atan ((x.magn *. sin x.arg +. y.magn *. sin y.arg) /. (x.magn *. cos x.arg +. y.magn *. cos y.arg))}
  (*let sub x y = failwith "later" *)
  let mul x y = {magn = x.magn *. y.magn;
                arg = if x.arg +. y.arg >= 2.0 *. pi
                      then x.arg +. y.arg -. 2.0 *. pi
                      else x.arg +. y.arg}
  let div x y = {magn = if y.magn <> 0.
                        then x.magn /. y.magn
                        else 0.0;
                arg = if x.arg -. y.arg < 0.0
                      then x.arg -. y.arg +. 2.0 *. pi
                      else x.arg -. y.arg}
  let inv x = {magn = 1.0 /. x.magn; arg = (-1.0) *. x.arg}

end
