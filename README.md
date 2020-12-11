This is an attempt at creating an error library that allows easily to "stack" errors layer by layer by providing composable error types based on first class modules. The second goal is to be able to distinguish between "public" error values and "private" internal only errors.

The idea is that each components having a consistent set of errors creates an `Error` that defines printers and a way to convert to public error. The framework then furnishes helpers to convert a trace of errors to public trace, and to display nice error messages (not yet implemented :p)

the intention is to use it like this:
```ocaml
module Error = StringError(struct let component = "my_request")

let handle_request foo =
match build_es_query foo with
| Error e -> Lwt.return_error (caused_by ~cause:e (with_context StringContext foo  (pure "cannot build es query")))
| Ok query ->
match%lwt do_es_query query with
| Ok ret -> Lwt.return_ok ret
| Error e -> Lwt.return_error (caused_by ~cause:e (with_context StringContext foo  (pure "failed to execute es query")))
```

In the end the display would show :
```
failed to execute es query with <my_request input> caused by :
    request <some truncated json> timeouted after 10s
```
