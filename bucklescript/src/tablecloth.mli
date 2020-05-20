(** *)

(** Functions for working with boolean ([true] or [false]) values. *)
module Bool : module type of Bool

module Char : module type of TableclothChar

module String : module type of TableclothString

(** Fixed precision integers *)
module Int : module type of Int

module Float : module type of Float

module Array : module type of TableclothArray

module List : module type of TableclothList

(**
  This module implements the [Result] type, which has a variant for
  successful results (['ok]), and one for unsuccessful results (['error]).
*)
module Result : sig
  (**
    [type] is the type constructor for a [Result] type. You specify
    the type of the [Error] and [Ok] variants, in that order.


    Here is how you would annotate a [Result] variable whose [Ok]
    variant is an integer and whose [Error] variant is a string:

    {[
    let x: (string, int) Tablecloth.Result.t = Ok 3
    let y: (string, int) Tablecloth.Result.t = Error "bad"
    ]}

  *)
  type ('err, 'ok) t = ('ok, 'err) Belt.Result.t

  val succeed : 'ok -> ('err, 'ok) t
  (**
    [Result.succeed(value)] returns [Ok(value)]. Use this to construct a successful
    result without having to depend directly on Belt or Base.

    Not only can you use [Result.succeed] whenever you would use the type constructor,
    but you can also use it when you would have wanted to pass the constructor
    itself as a function.

    {[
    Result.succeed 3 = Ok 3
    Tablecloth.List.map [1; 2; 3] ~f:Result.succeed = [Ok 1; Ok 2; Ok 3]
    ]}
  *)

  val fail : 'err -> ('err, 'ok) t
  (**
    [Result.fail(value)] returns [Error(value)]. Use this to construct a failing
    result without having to depend directly on Belt or Base.

    (Similar to {!Result.succeed})

    Not only can you use [Result.fail] whenever you would use the type constructor,
    but you can also use it when you would have wanted to pass the constructor
    itself as a function.

    {[
    Result.fail 3 = Error 3
    Tablecloth.List.map [1; 2; 3] ~f:Result.fail =
      [Error 1; Error 2; Error 3]
    ]}
  *)

  val withDefault : default:'ok -> ('err, 'ok) t -> 'ok
  (**
    Same as {!Result.with_default}.
  *)

  val with_default : default:'ok -> ('err, 'ok) t -> 'ok
  (**
    [Result.with_default ~default:defaultValue, result], when given an [Ok value], returns
    [value]; if given an [Error errValue ], returns [defaultValue].

    {[
    Result.with_default ~default:0 (Ok 12) = 12
    Result.with_default ~default:0 (Error "bad") = 0
    ]}
  *)

  val map2 : f:('a -> 'b -> 'c) -> ('err, 'a) t -> ('err, 'b) t -> ('err, 'c) t
  (**
    [Result.map2 ~f:fcn result_a result_b] applies
    [fcn], a function taking two non-[Result] parameters and returning a
    non-[Result] result to two [Result] arguments [result_a] and [result_b] as follows:

    If [result_a] and [result_b] are of the form [Ok a] and [OK b],
    the return value is [Ok (f a b)].

    If only one of [result_a] and [result_b] is of the form [Error err],
    that becomes the return result.  If both are [Error] values,
    [Result.map2] returns [result_a].


    {[
    let sum_diff x y = (x + y) * (x - y)
    Result.map2 ~f:sum_diff (Ok 7) (Ok 3) = Ok 40
    Result.map2 ~f:sum_diff (Error "err A") (Ok 3) = Error "err A"
    Result.map2 ~f:sum_diff (Ok 7) (Error "err B") = Error "err B"
    Result.map2 ~f:sum_diff (Error "err A") (Error "err B") = Error ("err A")
    ]}

  *)

  val combine : ('x, 'a) t list -> ('x, 'a list) t
  (**
    [Result.combine results] takes a list of [Result] values. If all
    the elements in [results] are of the form [Ok x], then [Result.combine]
    creates a list [xs] of all the values extracted from their [Ok]s, and returns
    [Ok xs]

    If any of the elements in [results] are of the form [Error err],
    the first of them is returned as the result of [Result.combine].

    {[
    Result.combine [Ok 1; Ok 2; Ok 3; Ok 4] = Ok [1; 2; 3; 4]
    Result.combine [Ok 1; Error "two"; Ok 3; Error "four"] = Error "two"
    ]}
  *)

  val map : f:('ok -> 'value) -> ('err, 'ok) t -> ('err, 'value) t
  (**
    [Result.map f r] applies a function [f], which
    takes a non-[Result] argument and returns a non-[Result] value, to
    a [Result] variable [r] as follows:

    If [r] is of the form [Ok x] ([Ok(x) in ReasonMl), [Result.map]
    returns [Ok (f x)]. Otherwise, [r] is an [Error]
    value and is returned unchanged.

    {[
    Result.map (fun x -> x * x) (Ok 3) = Ok 9
    Result.map (fun x -> x * x) (Error "bad") = Error "bad"
    ]}
  *)

  val fromOption : error:'err -> 'ok option -> ('err, 'ok) t
  (**
    Same as {!Result.from_option}.

  *)

  val from_option : error:'err -> 'ok option -> ('err, 'ok) t
  (**
    Map a [Option] to a [Result] value where [None] becomes [Error] and [Some]
    becomes [Ok].

    Useful for interacting with code that primarily uses [Result]s.
  *)

  val toOption : ('err, 'ok) t -> 'ok option
  (**
    Same as {!Result.to_option}.

  *)

  val to_option : ('err, 'ok) t -> 'ok option
  (**
    [Result.to_option r] converts a [Result] value [r] to an [Option] value as follows:
    a value of [Ok x] becomes [Some x]; a value of [Error err] becomes [None].

    {[
    Result.to_option (Ok 42) = Some 42
    Result.to_option (Error "bad") = None
    ]}
  *)

  val andThen : f:('ok -> ('err, 'value) t) -> ('err, 'ok) t -> ('err, 'value) t
  (**
    Same as {!Result.and_then}.
  *)

  val and_then :
    f:('ok -> ('err, 'value) t) -> ('err, 'ok) t -> ('err, 'value) t
  (**
    [Result.and_then ~f:fcn r] applies function [fcn], which takes a non-[Result]
    parameter and returns a [Result], to a [Result] variable [r].

    If [r] is of the form [Ok x], [Result.and_then] returns [f x];
    otherwise [r] is an [Error], and is returned unchanged.

    {[
    let recip (x:float) : (string, float) Tablecloth.Result.t = (
      if (x = 0.0) then
        Error "Divide by zero"
      else
        Ok (1.0 /. x)
    )

    Result.and_then ~f:recip (Ok 4.0) = Ok 0.25
    Result.and_then ~f:recip (Error "bad") = Error "bad"
    Result.and_then ~f:recip (Ok 0.0) = Error "Divide by zero"
    ]}

    [Result.and_then] is usually used to implement a chain of function
    calls, each of which returns a [Result] value.

    {[
    let root (x:float) : (string, float) Tablecloth.Result.t = (
      if (x < 0.0) then
        Error "Cannot be negative"
      else
        Ok (sqrt x)
    )

    root 4.0 |> Result.and_then ~f:recip = Ok 0.5
    root (-2.0) |> Result.and_then ~f:recip = Error "Cannot be negative"
    root(0.0) |> Result.and_then ~f:recip = Error "Divide by zero"
    ]}
  *)

  val pp :
       (Format.formatter -> 'err -> unit)
    -> (Format.formatter -> 'ok -> unit)
    -> Format.formatter
    -> ('err, 'ok) t
    -> unit
end
(**
    [Result.pp errFormat okFormat destFormat result]“pretty-prints”
    the [result], using [errFormat] if the [result] is an [Error] value or
    [okFormat] if the [result] is an [Ok] value. [destFormat] is a formatter
    that tells where to send the output.

    The following example will print [<ok: 42><error: bad>].


    {[
    let good: (string, int) Tablecloth.Result.t = Ok 42
    let not_good: (string, int) Tablecloth.Result.t = Error "bad"
    Result.pp Format.pp_print_string Format.pp_print_int Format.std_formatter good
    Result.pp Format.pp_print_string Format.pp_print_int Format.std_formatter not_good
    Format.pp_print_newline Format.std_formatter ();
    ]}

  *)

(**
  This module provides functions to work with the [option] type,
  which has a variant for  valid values (['Some]), and one for
  invalid values (['None]).
*)
module Option : sig
  type 'a t = 'a option

  val some : 'a -> 'a option
  (**
    [Option.some(value)] returns [Some(value)]. Use this to construct the Some branch
    of an option whenever you need a function to do so.

    {[
    Tablecloth.List.map [1; 2; 3] ~f:Option.some = [Some 1; Some 2; Some 3]
    ]}
  *)

  val andThen : f:('a -> 'b option) -> 'a option -> 'b option
  (**
    Same as {!Option.and_then}.
  *)

  val and_then : f:('a -> 'b option) -> 'a option -> 'b option
  (**
    [Option.and_then ~f:fcn opt] applies function [fcn], which takes a non-[Option]
    parameter and returns an [Option], to an [Option] variable [opt].

    If [opt] is of the form [Some x], [and_then] returns [f x];
    otherwise it returns [None].

    {[
    let recip (x:float) : float option = (
      if (x == 0.0) then
        None
      else
        Some (1.0 /. x)
    )

    Option.and_then ~f:recip (Some 4.0) = Some 0.25
    Option.and_then ~f:recip None = None
    Option.and_then ~f:recip (Some 0.0) = None
    ]}

    [Option.and_then] is usually used to implement a chain of function
    calls, each of which returns an [Option] value.

    {[
    let root (x:float) : float option = (
      if (x < 0.0) then
        None
      else
        Some (sqrt x)
    )

    root 4.0 |> Option.and_then ~f:recip = Some 0.5
    root (-2.0) |> Option.and_then ~f:recip = None
    root(0.0) |> Option.and_then ~f:recip = None
    ]}
  *)

  val or_ : 'a option -> 'a option -> 'a option
  (**
    [Option.or_ opt_a opt_b] returns
    [opt_a] if it is of the form [Some x];
    otherwise, it returns [opt_b].

    Unlike the built in or operator, the [Option.or_] function
    does not short-circuit. When you call [Option.or_], both arguments
    are evaluated before being passed to the function.

    {[
    Option.or_ (Some 11) (Some 22) = Some 11
    Option.or_ None (Some 22) = Some 22
    Option.or_ (Some 11) None = Some 11
    Option.or_ None None = None
    ]}
  *)

  val orElse : 'a option -> 'a option -> 'a option
  (**
    Same as {!Option.or_else}.
  *)

  val or_else : 'a option -> 'a option -> 'a option
  (**
    [Option.or_else opt_a opt_b] returns [opt_b] if it is of the form [Some x];
    otherwise, it returns [opt_a].

    {[
    Option.orElse (Some 11) (Some 22) = Some 22
    Option.orElse None (Some 22) = Some 22
    Option.orElse (Some 11) None = Some 11
    Option.orElse None None = None
    ]}
  *)

  val map : f:('a -> 'b) -> 'a option -> 'b option
  (**
    [Option.map ~f:fcn opt] returns
    [fcn x] if [opt] is of the form
    [Some x]; otherwise, it returns [None].

    {[
    Option.map ~f:(fun x -> x * x) (Some 9) = Some 81
    Option.map ~f:(fun x -> x * x) None = None
    ]}
  *)

  val withDefault : default:'a -> 'a option -> 'a
  (**
    Same as {!Option.with_default}.
  *)

  val with_default : default:'a -> 'a option -> 'a
  (**
    [Option.with_default(~default: def_val, opt)] If [opt] is of the form [Some x],
    this function returns [x]. Otherwise, it returns the default value [def_val].

    {[
    Option.with_default ~default:99 (Some 42) = 42
    Option.with_default ~default:99 None = 99
    ]}
  *)

  val values : 'a option list -> 'a list
  (**
    [Option.values xs] takes a list of [option] values and creates
    a new list consisting of the values wrapped in [Some x].

    {[
    Option.values [Some 1; None; Some 3; None] = [1; 3]
    Option.values [None; None] = [ ]
    ]}
  *)

  val toList : 'a option -> 'a list
  (**
    Same as {!Option.to_list}.
  *)

  val to_list : 'a option -> 'a list
  (**
    [Option.to_list opt] returns the list [[x]] if [opt] is of the form [Some x];
    otherwise, it returns the empty list.
  *)

  val isSome : 'a option -> bool
  (**
    Same as {!Option.is_some}.
  *)

  val is_some : 'a option -> bool
  (**
    [Option.is_some opt] returns [true] if [opt] is a [Some] value, [false] otherwise.
  *)

  val toOption : sentinel:'a -> 'a -> 'a option
  (**
    Same as {!Option.to_option}.
  *)

  val to_option : sentinel:'a -> 'a -> 'a option
  (**
    [Option.to_option ~sentinel:s, x] returns [Some x] unless [x] equals the sentinel
    value [s], in which case [Option.to_option] returns [None].
  *)

  val getExn : 'a option -> 'a
  (**
    Same as {!Option.get_exn}
  *)

  val get_exn : 'a option -> 'a
  (** [get_exn optional_value]
    Returns [value] if [optional_value] is [Some value], otherwise raises [get_exn]

    @example {[
      get_exn (Some 3) = 3;;
      get_exn None (* Raises get_exn error *)
    ]}
  *)
end

module Tuple2 : module type of Tuple2

module Tuple3 : module type of Tuple3

module IntSet : sig
  type t = Belt.Set.Int.t

  type value = int

  val fromList : value list -> t

  val from_list : value list -> t

  val member : value:value -> t -> bool

  val diff : t -> t -> t

  val isEmpty : t -> bool

  val is_empty : t -> bool

  val toList : t -> value list

  val to_list : t -> value list

  val ofList : value list -> t

  val of_list : value list -> t

  val union : t -> t -> t

  val remove : value:value -> t -> t

  val add : value:value -> t -> t

  val set : value:value -> t -> t

  val has : value:value -> t -> bool

  val empty : t

  val pp : Format.formatter -> t -> unit
end

module StrSet : sig
  type t = Belt.Set.String.t

  type value = string

  val fromList : value list -> t

  val from_list : value list -> t

  val member : value:value -> t -> bool

  val diff : t -> t -> t

  val isEmpty : t -> bool

  val is_empty : t -> bool

  val toList : t -> value list

  val to_list : t -> value list

  val ofList : value list -> t

  val of_list : value list -> t

  val union : t -> t -> t

  val remove : value:value -> t -> t

  val add : value:value -> t -> t

  val set : value:value -> t -> t

  val has : value:value -> t -> bool

  val empty : t

  val pp : Format.formatter -> t -> unit
end

module StrDict : sig
  type key = Belt.Map.String.key

  type 'value t = 'value Belt.Map.String.t

  val toList : 'a t -> (key * 'a) list

  val to_list : 'a t -> (key * 'a) list

  val empty : 'a t

  val fromList : (key * 'value) list -> 'value t

  val from_list : (key * 'value) list -> 'value t

  val get : key:key -> 'value t -> 'value option

  val insert : key:key -> value:'value -> 'value t -> 'value t

  val keys : 'a t -> key list

  val update :
    key:key -> f:('value option -> 'value option) -> 'value t -> 'value t

  val map : 'a t -> f:('a -> 'b) -> 'b t

  val toString : 'a t -> string

  val to_string : 'a t -> string

  val pp :
    (Format.formatter -> 'value -> unit) -> Format.formatter -> 'value t -> unit

  val merge :
    f:(key -> 'v1 option -> 'v2 option -> 'v3 option) -> 'v1 t -> 'v2 t -> 'v3 t
end

module IntDict : sig
  type key = Belt.Map.Int.key

  type 'value t = 'value Belt.Map.Int.t

  val toList : 'a t -> (key * 'a) list

  val to_list : 'a t -> (key * 'a) list

  val empty : 'a t

  val fromList : (key * 'value) list -> 'value t

  val from_list : (key * 'value) list -> 'value t

  val get : key:key -> 'value t -> 'value option

  val insert : key:key -> value:'value -> 'value t -> 'value t

  val update :
    key:key -> f:('value option -> 'value option) -> 'value t -> 'value t

  val keys : 'a t -> key list

  val map : 'a t -> f:('a -> 'b) -> 'b t

  val toString : 'a t -> string

  val to_string : 'a t -> string

  val pp :
    (Format.formatter -> 'value -> unit) -> Format.formatter -> 'value t -> unit

  val merge :
    f:(key -> 'v1 option -> 'v2 option -> 'v3 option) -> 'v1 t -> 'v2 t -> 'v3 t
end

(** Functions for working with functions. *)
module Fun : module type of Fun
