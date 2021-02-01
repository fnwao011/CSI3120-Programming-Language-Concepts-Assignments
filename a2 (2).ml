(*** CSI 3120 Assignment 2 ***)
(*** Fabian Nwaoha ***)
(*** 300063406 ***)
(*** 4.10.0 ***)
(* If you use the version available from the lab machines via VCL, the
   version is 4.05.0 ***)

(*************)
(* PROBLEM 1 *)
(*************)

(* For each part of problem 1, explain in the given string why the code
   will not typecheck, then follow the instructions for that part to
   change the code so that it typechecks while keeping the same values
   as intended by the erroneous code. Once you have done so, uncomment
   the fixed expression for submission.
*)

(* Problem 1a - Give your explanation in exp1a and then fix the
   right-hand-side of the assignment to match the listed type.
   (Do not change the left-hand-side.)
*)


let exp1a : string = "The type is supposed to be a string int char typle within a list"
let prob1a : (string * int * char) list = [("7", 8, '9');];;


(* Problem 1b - Give your explanation in exp1b and then fix the type
   of variable prob1b to match the type of the expression on the
   right-hand-side of the assignment. (Do not change the
   right-hand-side.)
 *)

(
let exp1b : string = "The type is supposed to be of a tuple containing a string list and int list"
let prob1b : (string list * int list) = (["apples";"bananas";"carrots"],[3;2;1]);;


(* Problem 1c - Give your explanation in exp1c and then fix the
   right-hand-side of the expression to match the variable prob1c's
   listed type.  (Do not change the left-hand-side.)
 *)


let exp1c : string = " The first 2 elements were not of type string list and the last element was not of type string list list"
let prob1c : string list list = ["2"; "b"] :: ["or"; "not"; "2b"] :: [["that is"; "the" ; "question"]]


(*************)
(* PROBLEM 2 *)
(*************)

(* Fill in expressions to satisfy the following types:
 *
 * NOTE: for option, list, and function types, you must
 * provide a nontrivial answer. For a list that means a
 * non-empty one, for an option type that means a Some
 * construction, and for a function, that means using
 * its arguments to generate the return value.
 * example problems:
 *   let x : int option = ???
 *   let y : int list = ???
 *   let f (x: int) (y: int) : int = ???
 * incorrect answers:
 *   let x : int option = None
 *   let y : int list = []
 *   let f (x: int) (y: int) : int = 7
 * possible correct answers:
 *   let x : int option = Some 1
 *   let y : int list = [1]
 *   let y : int list = [1; 2]
 *   let y : int list = 1 :: [2]
 *   let f (x: int) (y: int) : int = x + y
 *   let f (x: int) (y: int) : int =
 *         String.length  ((string_of_int x) ^ (string_of_int y))
 *)

(* Problem 2a *)

let prob2a : (int * ((string * float) option list)) list = [5,[Some("F",3.01)]]



(* Problem 2b *)
(* a pet is a (name, animal_type, age option) tuple *)

type pet = string * string * int option


let prob2b : string * pet list option = ("S",Some[("Leo","Dog",Some 7)])



(* Problem 2c *)
(* Fill in a valid function call to f to make prob2c typecheck *)


let prob2c =
  let rec f arg =
    match arg with
    | (a, b) :: xs -> if a then (b ^ (f xs)) else f xs
    | _ -> ""
  in 
  f([(true,"test");(true,"part2")])



(*************)
(* PROBLEM 3 *)
(*************)

(* Problem 3a.  You have been asked to write a text filter,
   where you want to find all search characters in your text
   if they appear the right order.

   Write a function text_filter that takes two lists of characters
   and checks to see if all the characters in the first list are
   included in the second list AND in the same order, BUT possibly
   with other characters in between.  For example

   text_filter ['a';'m';'z'] ['1';'a';'2';'m';'3';'z'] = true
   text_filter ['a';'m';'z'] ['1';'a';'3';'z'] = false
   text_filter ['a';'m';'z'] ['1';'z';'2';'m';'3';'a'] = false

 *)

 let rec text_filter (xs:char list) (ys:char list) : bool =
   match xs, ys with
   |([],_)->true
   |(x::y,[]) -> false
   |(x::y, xx::yy)-> if x == xx then text_filter y yy else text_filter xs yy

(* Problem 3b. Rewrite the function above so that is is polymorphic,
   i.e., it should work on lists whose elements are any types.  Give
   at least one test case (call your function at least once) with a
   type that is different from chars. *)


(*************)
let rec text_filter (xs:'a list) (ys:'b list) : bool =
   match xs, ys with
   |([],_)->true
   |(x::y,[]) -> false
   |(x::y, xx::yy)-> if x == xx then text_filter y yy else text_filter xs yy

   let test() : bool =
      text_filter(["a","b"],["a","z","b"])

(* PROBLEM 4 *)
(*************)

(* Write a function (int_to_whole) that converts an integer
   into a whole number if one exists
   (a whole number is 1, 2, 3, ...).
   Use an option type because not all integer inputs can
   be converted. *)

type whole = One | Succ of whole

let int_to_whole (x:int) : (whole option) =
   if(x < 1) then None else
   let rec int_to_whole2 (x:int) : (whole) =
      match x with
      | 1 -> One
      | y -> (int_to_whole2 (y-1))
      in Some (int_to_whole2 x);;

