(** {2 Functors types to construct error value implementations } *)
module type SimpleError = sig
  (** The name of the error class *)
  val component : string

  type error

  (** Catches exn and transform it to this error type *)
  val of_exn : exn -> error

  (** Display errors in human readable form *)
  val show : Format.formatter -> error -> unit
end

module type Error = sig
  include SimpleError

  module Public : SimpleError

  (** Convert private error to a suitble reprensation to be sent to external world *)
  val to_public_error : error -> Public.error option
end

(** An error type that is public *)
module PublicError (E : SimpleError) : Error with type error = E.error and type Public.error = E.error

(** An error type that should not be exposed to public and would be removed *)
module PrivateError (E : SimpleError) : Error with type error = E.error and type Public.error = E.error

(** Errors representated as human readable strings *)
module StringError (C : sig
  val component : string
end) : SimpleError with type error = string

(** Context specific value to add to error messages, like actual request url, or database address *)
module type Context = sig
  type context

  (** Display context in human readable form *)
  val show : Format.formatter -> context -> unit

  (** Display the context in a way that is suitable for public use *)
  val show_public : Format.formatter -> context -> unit
end

(** {2 Main error types } *)
module type ErrorWithContext = sig
  include Error

  module Context : Context
end

(** Error values *)
type ('error, 'public) t

type any_error = AnyError : ('error, 'public) t -> any_error

(** {2 Generic api to construct errors } *)

(** {3 Core error api } *)

(** Wraps an error value into an error *)
val pure : (module Error with type error = 'error and type Public.error = 'public) -> 'error -> ('error, 'public) t

(** Unwraps the error value contained in the error *)
val as_error : ('error, _) t -> 'error

(** Constructs an error from an exception *)
val of_exn : (module Error with type error = 'error and type Public.error = 'public) -> exn -> ('error, 'public) t

(** Adds a context value to an error *)
val context : (module Context with type context = 'ctx) -> 'ctx -> ('error, 'public) t -> ('error, 'public) t

(** Adds and wraps an error as cause of the error *)
val caused_by : cause:(_, _) t -> ('error, 'public) t -> ('error, 'public) t

(** {3 high level api } *)

(** Wraps an error value into an error and an optional cause *)
val error
  :  (module Error with type error = 'error and type Public.error = 'public) ->
  ?cause:(_, _) t ->
  'error ->
  ('error, 'public) t

(** Wraps an error value into an error and an optional cause from a result value
  [with_error (module E) ?cause r] is [Result.map_err (error (module E) ?cause) r] *)
val with_error
  :  (module Error with type error = 'error and type Public.error = 'public) ->
  ?cause:(_, _) t ->
  ('ok, 'error) result ->
  ('ok, ('error, 'public) t) result

(** Adds a context value to a result when the result is an error.
  [with_context (module C) ctx r] is [Result.map_err (context (module C) ctx)] *)
val with_context
  :  (module Context with type context = 'ctx) ->
  'ctx ->
  ('ok, ('error, 'public) t) result ->
  ('ok, ('error, 'public) t) result

(** [guard_result (module E) ?cause f] runs f and catches exceptions.
    Exn is converted to an error using [E.of_exn] and return [result] of [f] is converted to an error trace result *)
val guard_result
  :  (module Error with type error = 'error and type Public.error = 'public) ->
  ?cause:(_, _) t ->
  (unit -> ('ok, 'error) result) ->
  ('ok, ('error, 'public) t) result

(** [guard_exn (module E) ?cause f] runs f and catches exceptions.
    Exn is converted to an error using [E.of_exn] *)
val guard_exn
  :  (module Error with type error = 'error and type Public.error = 'public) ->
  ?cause:(_, _) t ->
  (unit -> 'ok) ->
  ('ok, ('error, 'public) t) result

(** [bubble_error e r] wraps error case of [r] as cause of error [e] in case [r] is an [Error] *)
val bubble_error : ('error, 'public) t -> ('ok, (_, _) t) result -> ('ok, ('error, 'public) t) result

(** {2 Transformations on error traces *)

(** Constructs the backtraces of all causes *)
val backtrace : (_, _) t -> any_error list

(** Recursively cleans error trace from any sensitive data and simplify it to public type *)
val to_public_error : ('e, 'public) t -> ('public, 'public) t option

(** {2 Display error messages } *)

(** Displays error as single line *)
val show_compact : Format.formatter -> (_, _) t -> unit

(** Displays error as multi line *)
val show_verbose : Format.formatter -> (_, _) t -> unit

(** display human readable string of error *)
val to_string : (_, _) t -> string

(** display verbose human readable string of error *)
val to_string_verbose : (_, _) t -> string

(** {2 Instanciated api for convenience } *)

module Make (E : Error) : sig
  val pure : E.error -> (E.error, E.Public.error) t

  val as_error : ('a, 'b) t -> 'a

  val of_exn : exn -> (E.error, E.Public.error) t

  val caused_by : cause:('a, 'b) t -> (E.error, E.Public.error) t -> (E.error, E.Public.error) t

  val error : ?cause:('a, 'b) t -> E.error -> (E.error, E.Public.error) t

  val with_error : ?cause:('a, 'b) t -> ('ok, E.error) result -> ('ok, (E.error, E.Public.error) t) result

  val guard_result : ?cause:(_, _) t -> (unit -> ('ok, E.error) result) -> ('ok, (E.error, E.Public.error) t) result

  val guard_exn : ?cause:(_, _) t -> (unit -> 'ok) -> ('ok, (E.error, E.Public.error) t) result
end

module MakeContext (E : ErrorWithContext) : sig
  val pure : E.error -> (E.error, E.Public.error) t

  val as_error : ('a, 'b) t -> 'a

  val of_exn : exn -> (E.error, E.Public.error) t

  val caused_by : cause:('a, 'b) t -> (E.error, E.Public.error) t -> (E.error, E.Public.error) t

  val error : ?cause:('a, 'b) t -> E.error -> (E.error, E.Public.error) t

  val with_error : ?cause:('a, 'b) t -> ('ok, E.error) result -> ('ok, (E.error, E.Public.error) t) result

  val guard_result : ?cause:(_, _) t -> (unit -> ('ok, E.error) result) -> ('ok, (E.error, E.Public.error) t) result

  val guard_exn : ?cause:(_, _) t -> (unit -> 'ok) -> ('ok, (E.error, E.Public.error) t) result

  val context : E.Context.context -> (E.error, E.Public.error) t -> (E.error, E.Public.error) t

  val with_context
    :  E.Context.context ->
    ('ok, (E.error, E.Public.error) t) result ->
    ('ok, (E.error, E.Public.error) t) result
end
