open Stdint
open Classfile

module Frame : sig
  type t = Callable of Cp_info.t * Cp_info.t * Cp_info.t | String of string

  val to_java_primitive : t -> Java_libs.primitive
end

module Runtime_data_area : sig
  type t

  val create : Cp_info.t array -> t
end

type t

val create : Classfile.t -> t
val entry_point : t -> Attribute_info.t

(* access constant_pool *)
val get_constant : t -> int -> Cp_info.t
val get_constant_16 : t -> Uint16.t -> Cp_info.t
val get_constant_8 : t -> Uint8.t -> Uint8.t -> Cp_info.t

(* resolution *)
val field_resolution : t -> Cp_info.t -> Frame.t

(* operator of stack *)
val stack_push : t -> Frame.t -> unit
val stack_pop : t -> int -> Frame.t list
