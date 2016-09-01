(*
 * Copyright (c) 2015 David Sheets <sheets@alum.mit.edu>
 *
 * Permission to use, copy, modify, and distribute this software for any
 * purpose with or without fee is hereby granted, provided that the above
 * copyright notice and this permission notice appear in all copies.
 *
 * THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
 * WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
 * ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
 * WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
 * ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
 * OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
 *
 *)

module Signal = struct
  type t =
    | SIGABRT
    | SIGALRM
    | SIGFPE
    | SIGHUP
    | SIGILL
    | SIGINT
    | SIGKILL
    | SIGPIPE
    | SIGQUIT
    | SIGSEGV
    | SIGTERM
    | SIGUSR1
    | SIGUSR2
    | SIGCHLD
    | SIGCONT
    | SIGSTOP
    | SIGTSTP
    | SIGTTIN
    | SIGTTOU
    | SIGVTALRM
    | SIGPROF
    | Unknown of int

  let of_int = Sys.(function
    | x when x = sigabrt   -> SIGABRT
    | x when x = sigalrm   -> SIGALRM
    | x when x = sigfpe    -> SIGFPE
    | x when x = sighup    -> SIGHUP
    | x when x = sigill    -> SIGILL
    | x when x = sigint    -> SIGINT
    | x when x = sigkill   -> SIGKILL
    | x when x = sigpipe   -> SIGPIPE
    | x when x = sigquit   -> SIGQUIT
    | x when x = sigsegv   -> SIGSEGV
    | x when x = sigterm   -> SIGTERM
    | x when x = sigusr1   -> SIGUSR1
    | x when x = sigusr2   -> SIGUSR2
    | x when x = sigchld   -> SIGCHLD
    | x when x = sigcont   -> SIGCONT
    | x when x = sigstop   -> SIGSTOP
    | x when x = sigtstp   -> SIGTSTP
    | x when x = sigttin   -> SIGTTIN
    | x when x = sigttou   -> SIGTTOU
    | x when x = sigvtalrm -> SIGVTALRM
    | x when x = sigprof   -> SIGPROF
    | x                    -> Unknown x
  )

  let to_string = function
    | SIGABRT -> "SIGABRT"
    | SIGALRM -> "SIGALRM"
    | SIGFPE -> "SIGFPE"
    | SIGHUP -> "SIGHUP"
    | SIGILL -> "SIGILL"
    | SIGINT -> "SIGINT"
    | SIGKILL -> "SIGKILL"
    | SIGPIPE -> "SIGPIPE"
    | SIGQUIT -> "SIGQUIT"
    | SIGSEGV -> "SIGSEGV"
    | SIGTERM -> "SIGTERM"
    | SIGUSR1 -> "SIGUSR1"
    | SIGUSR2 -> "SIGUSR2"
    | SIGCHLD -> "SIGCHLD"
    | SIGCONT -> "SIGCONT"
    | SIGSTOP -> "SIGSTOP"
    | SIGTSTP -> "SIGTSTP"
    | SIGTTIN -> "SIGTTIN"
    | SIGTTOU -> "SIGTTOU"
    | SIGVTALRM -> "SIGVTALRM"
    | SIGPROF -> "SIGPROF"
    | Unknown k -> "SIG"^(string_of_int k)

end

module Exit = struct
  type t =
    | Exit of int
    | Kill of Signal.t
    | Stop of Signal.t

  type error = {
    cwd     : string;
    command : string;
    args    : string array;
    status  : t;
  }
  exception Error of error

  let of_unix = Unix.(function
    | WEXITED k   -> Exit k
    | WSIGNALED k -> Kill (Signal.of_int k)
    | WSTOPPED k  -> Stop (Signal.of_int k)
  )

  let to_string = function
    | Exit k -> Printf.sprintf "exit %d" k
    | Kill k -> Printf.sprintf "kill %s" (Signal.to_string k)
    | Stop k -> Printf.sprintf "stop %s" (Signal.to_string k)

  let error_to_string { cwd; command; args; status } =
    let args = Array.map (Printf.sprintf "%S") args in
    let args_s = String.concat "; " (Array.to_list args) in
    Printf.sprintf "%s [|%s|] in %s: %s" command args_s cwd (to_string status)

  let check ?(exit_status=[0]) command args = function
    | Exit k when List.mem k exit_status -> ()
    | status ->
      raise (Error { cwd = Unix.getcwd (); command; args; status })

end

module Output = struct
  type t = {
    exit_status : Exit.t;
    stdout : string list;
    stderr : string list;
  }
end

module type S = sig
  type 'a io

  val run :
    ?stdin:Bytes.t -> ?exit_status:int list -> string -> string array
    -> Output.t io

  val read_stdout :
    ?stdin:Bytes.t -> ?exit_status:int list -> string -> string array
    -> string list io
end

module Blocking : S with type 'a io = 'a = struct
  type 'a io = 'a

  let quote = Printf.sprintf "\"%s\""

  let string_of_prog_args prog args =
    prog ^ (
      if Array.length args > 0 then
        " " ^ (String.concat " " Array.(to_list (map quote args)))
      else ""
    )

  let rec waitpid_retry flags pid =
    try Unix.waitpid flags pid
    with Unix.Unix_error (Unix.EINTR,"waitpid","") ->
      waitpid_retry flags pid

  let io_from_fd fds fn fd =
    let closed = fn fd in
    if closed
    then List.filter ((<>) fd) fds
    else fds

  let select_io
      ~input_stdout ~stdout
      ~input_stderr ~stderr
      ~output_stdin ~stdin
      ~read_fds ~write_fds =
    let rec loop ~read_fds ~write_fds =
      if read_fds <> [] || write_fds <> []
      then
        let ready_read, ready_write, _ready_exn =
          Unix.select read_fds write_fds [] ~-.1.
        in
        match ready_read with
        | fd::_ when fd = stdout ->
          let read_fds = io_from_fd read_fds input_stdout fd in
          loop ~read_fds ~write_fds
        | fd::_ when fd = stderr ->
          let read_fds = io_from_fd read_fds input_stderr fd in
          loop ~read_fds ~write_fds
        | _::_ -> failwith "unexpected read fd" (* TODO: ? *)
        | [] -> match ready_write with
          | fd::_ ->
            let write_fds = io_from_fd write_fds output_stdin fd in
            loop ~read_fds ~write_fds
          | [] -> failwith "select failed" (* TODO: ? *)
    in
    try
      let sigpipe = Sys.(signal sigpipe Signal_ignore) in
      loop ~read_fds ~write_fds;
      Sys.(set_signal sigpipe) sigpipe
    with Invalid_argument _ ->
      (* Can't ignore the pipe broken signal on Windows. *)
      loop ~read_fds ~write_fds

  let execute prog args ~output_stdin ~input_stdout ~input_stderr =
    let in_fd, stdin = Unix.pipe () in
    let stdout, out_fd = Unix.pipe () in
    let stderr, err_fd = Unix.pipe () in
    Unix.set_close_on_exec stdin;
    Unix.set_close_on_exec stdout;
    Unix.set_close_on_exec stderr;
    let args = Array.append [|prog|] args in
    let pid = Unix.create_process prog args in_fd out_fd err_fd in
    Unix.close in_fd;
    Unix.close out_fd;
    Unix.close err_fd;
    select_io
      ~input_stdout ~stdout
      ~input_stderr ~stderr
      ~output_stdin ~stdin
      ~read_fds:[ stdout; stderr ] ~write_fds:[ stdin ];
    (* stdin is closed when we run out of input *)
    Unix.close stdout;
    Unix.close stderr;
    let status = snd (waitpid_retry [Unix.WUNTRACED] pid) in
    Exit.of_unix status

  let rindex_from buf i c =
    try Some (Bytes.rindex_from buf i c) with Not_found -> None

  let rec lines buf i acc =
    match rindex_from buf i '\n' with
    | Some 0 -> Bytes.empty :: (Bytes.sub buf 1 i) :: acc
    | Some j -> lines buf (j - 1) (Bytes.sub buf (j + 1) (i - j) :: acc)
    | None -> Bytes.sub buf 0 (i + 1) :: acc

  let read_lines buf len into fd =
    (* The EPIPE case covers an odd behavior on Windows.
       See <http://caml.inria.fr/mantis/view.php?id=7342>.
    *)
    let n = Unix.(try read fd buf 0 len with Unix_error (EPIPE, _, _) -> 0) in
    if n = 0
    then true (* closed *)
    else
      let ls = lines buf (n - 1) [] in
      begin match !into with
        | [] -> into := List.rev ls
        | partial_line::rest -> match ls with
          | [] -> ()
          | first::more ->
            let first = Bytes.cat partial_line first in
            into := List.rev_append more (first :: rest)
      end;
      false (* not closed *)

  let run ?(stdin=Bytes.empty) ?exit_status prog args =
    let out_lines = ref [] in
    let err_lines = ref [] in
    let len = 4096 in
    let buf = Bytes.create len in
    let input_stdout = read_lines buf len out_lines in
    let input_stderr = read_lines buf len err_lines in
    let stdin_len = Bytes.length stdin in
    let stdin_off = ref 0 in
    let output_stdin i_fd =
      let off = !stdin_off in
      let len = stdin_len - off in
      if len = 0
      then begin
        Unix.close i_fd;
        true (* closed, we have nothing more to write *)
      end
      else
        try
          let n = Unix.single_write i_fd stdin off len in
          stdin_off := off + n;
          false (* not closed *)
        with
        | Unix.Unix_error (Unix.EPIPE, "single_write", _) -> true (* closed *)
    in
    let exit_status' =
      execute prog args ~output_stdin ~input_stdout ~input_stderr
    in
    (match exit_status with
     | None -> ()
     | Some exit_status -> Exit.check ~exit_status prog args exit_status'
    );
    let exit_status = exit_status' in
    let stdout = List.rev_map Bytes.to_string !out_lines in
    let stderr = List.rev_map Bytes.to_string !err_lines in
    Output.({ exit_status; stdout; stderr; })

  let read_stdout ?stdin ?exit_status prog args =
    let exit_status = match exit_status with None -> [0] | Some v -> v in
    (run ?stdin ~exit_status prog args).Output.stdout

end

include Blocking
