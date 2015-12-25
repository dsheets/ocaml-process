## Easy process control in OCaml

```ocaml
let stdout_lines = Process.read_stdout "ls" [||] in
List.iter print_endline stdout_lines
```

It's as easy as that!

Exit status, stdin, and stderr are also available.

`process` makes it easy to use commands like functions.
