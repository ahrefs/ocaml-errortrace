open Devkit
open Containers

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

  val to_public_error : error -> Public.error option
end

module PublicError (E : SimpleError) = struct
  include E
  module Public = E

  let to_public_error e = Some e
end

module PrivateError (E : SimpleError) = struct
  include E
  module Public = E

  let to_public_error _e = None
end

module StringError (C : sig
  val component : string
end) =
struct
  include C

  type error = string

  let of_exn = Exn.to_string

  let show = Format.string
end

module type Context = sig
  type context

  (** Display context in human readable form *)
  val show : Format.formatter -> context -> unit

  (** Display the context in a way that is suitable for public use *)
  val show_public : Format.formatter -> context -> unit
end

module type ErrorWithContext = sig
  include Error

  module Context : Context
end

type error_context =
  | Context : {
    context : 'context;
      context_kind : (module Context with type context = 'context);
    }
      -> error_context

type ('error, 'public) t = {
  error : 'error;
  kind : (module Error with type error = 'error and type Public.error = 'public);
  context : error_context option;
  cause : any_error option;
}

and any_error = AnyError : ('error, 'public) t -> any_error

(** {3 Core error api } *)

let pure (type e public) (module E : Error with type error = e and type Public.error = public) e =
  { error = e; kind = (module E); cause = None; context = None }

let as_error e = e.error

let of_exn (type e public) (module E : Error with type error = e and type Public.error = public) exn =
  pure (module E) (E.of_exn exn)

let context (type ctx) (module Ctx : Context with type context = ctx) context e =
  { e with context = Some (Context { context; context_kind = (module Ctx) }) }

let caused_by ~cause e = { e with cause = Some (AnyError cause) }

(** {3 high level api } *)

let with_context (type ctx) (module Ctx : Context with type context = ctx) c r =
  Result.map_err (context (module Ctx) c) r

let error (type e public) (module E : Error with type error = e and type Public.error = public) ?cause e =
  pure (module E) e |> Option.map_or ~default:id (fun cause -> caused_by ~cause) cause

let with_error (type e public) (module E : Error with type error = e and type Public.error = public) ?cause r =
  Result.map_err (error (module E) ?cause) r

let guard_result (type e public) (module E : Error with type error = e and type Public.error = public) ?cause f =
  Result.guard (fun () -> f () |> Result.map_err (error (module E) ?cause))
  |> Result.map_err (error (module E) ?cause $ E.of_exn)
  |> Result.join

let guard_exn (type e public) (module E : Error with type error = e and type Public.error = public) ?cause f =
  Result.guard f |> Result.map_err (error (module E) ?cause $ E.of_exn)

let bubble_error e r = Result.map_err (fun cause -> caused_by ~cause e) r

let rec cause_to_public = function
  | AnyError cause ->
    let module Cause = (val cause.kind) in
    ( match to_public_error cause with
    | None -> None
    | Some cause -> Some (AnyError cause)
    )

and to_public_error : type e public. (e, public) t -> (public, public) t option =
 fun e ->
  let module E = (val e.kind : Error with type error = e and type Public.error = public) in
  let error = E.to_public_error e.error in
  match error with
  | None -> None
  | Some error ->
  match e.cause with
  | None -> Some { error; cause = None; context = e.context; kind = (module PublicError (E.Public)) }
  | Some cause ->
    let cause = cause_to_public cause in
    Some { error; cause; context = e.context; kind = (module PublicError (E.Public)) }

let backtrace e =
  let rec to_list acc = function
    | Some cause -> to_list (cause :: acc) e.cause
    | None -> acc
  in
  to_list [] e.cause

type ('a, 'b) error = ('a, 'b) t

include struct
  open Format

  let show_error (type e public) out (e : (e, public) error) =
    let module E = (val e.kind : Error with type error = e and type Public.error = public) in
    E.show out (as_error e)

  let show_any_error out = function
    | AnyError e -> show_error out e

  let show_compact out e =
    pp_open_hbox out ();
    hbox show_error out e;
    fprintf out "@ caused by:@ ";
    hbox (list ~sep:(const string "@ ↣ ") show_any_error) out (List.rev (backtrace e));
    pp_close_box out ()

  let show_verbose out e =
    pp_open_box out 0;
    hbox show_error out e;
    fprintf out "@,caused by:@,";
    vbox ~i:2 (list ~sep:(const string "@,↳ ") show_any_error) out (List.rev (backtrace e));
    pp_close_box out ()

  let to_string_verbose e = to_string show_verbose e

  let to_string e = to_string show_compact e
end

module Make (E : Error) = struct
  let pure e = pure (module E) e

  let as_error e = as_error e

  let of_exn e = of_exn (module E) e

  let caused_by ~cause e = caused_by ~cause e

  let error ?cause e = error (module E) ?cause e

  let with_error ?cause r = with_error (module E) ?cause r

  let guard_result ?cause f = guard_result (module E) ?cause f

  let guard_exn ?cause f = guard_exn (module E) ?cause f
end

module MakeContext (E : ErrorWithContext) = struct
  include Make (E)

  let context ctx e = context (module E.Context) ctx e

  let with_context c r = with_context (module E.Context) c r
end
