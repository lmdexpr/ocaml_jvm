open Classfile

type t_local =
  | Null
  | Boolean of bool
  | Byte of char
  | Char of char
  | Short of int
  | Int of int
  | Float of float
  | Reference of int
  | Return_address of int

let local_to_string = function
  | Null -> "null"
  | Boolean b -> "boolean " ^ string_of_bool b
  | Byte c -> "byte " ^ String.make 1 c
  | Char c -> "char " ^ String.make 1 c
  | Short i -> "short " ^ string_of_int i
  | Int i -> "int " ^ string_of_int i
  | Float f -> "float " ^ string_of_float f
  | Reference i -> "reference " ^ string_of_int i
  | Return_address i -> "return address " ^ string_of_int i

type t_operand =
  | Callable of Cp_info.t * Cp_info.t * Cp_info.t
  | String of string
  | Int of int

let operand_to_string = function
  | String s -> "string " ^ s
  | Int i -> "int " ^ string_of_int i
  | Callable _ -> "callable"

type t =
  { locals : t_local array
  ; stack : t_operand Stack.t
        (* dynamic linking*)
        (* normal method invocation completion*)
        (* abrupt method invocation completion*)
  }

let to_java_primitive = function
  | String s -> Java_libs.String s
  | Int i -> Java_libs.Int i
  | _ -> Java_libs.Not_implemented

let create size_of_locals : t =
  { locals = Array.make size_of_locals Null; stack = Stack.create () }

let stack_push frame value = Stack.push value frame.stack
let stack_pops frame n = List.init n (fun _ -> Stack.pop frame.stack)
let stack_pop frame = Stack.pop frame.stack
