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

  let execute prog args ~stdin input_fn =
    let input = stdin in
    let in_fd, stdin = Unix.pipe () in
    let stdout, out_fd = Unix.pipe () in
    let stderr, err_fd = Unix.pipe () in
    Unix.set_close_on_exec stdin;
    Unix.set_close_on_exec stdout;
    Unix.set_close_on_exec stderr;
    let args = Array.append [|prog|] args in
    let pid = Unix.create_process prog args in_fd out_fd err_fd in
    Unix.close in_fd;
    let len = Bytes.length input in
    let n = Unix.write stdin input 0 len in
    assert (n = len);
    Unix.close stdin;
    Unix.close out_fd;
    Unix.close err_fd;
    input_fn stdout stderr;
    Unix.close stdout;
    Unix.close stderr;
    let status = snd (waitpid_retry [Unix.WUNTRACED] pid) in
    Exit.of_unix status

  let rec input_all_ lst ic =
    let line = try Some (input_line ic) with End_of_file -> None in
    match line with Some l -> input_all_ (l::lst) ic | None -> List.rev lst
  let input_all = input_all_ []

  let run ?(stdin=Bytes.empty) ?exit_status prog args =
    let out_lines = ref [] in
    let err_lines = ref [] in
    let input_fn o_fd e_fd =
      let oic = Unix.in_channel_of_descr o_fd in
      out_lines := input_all oic;
      let eic = Unix.in_channel_of_descr e_fd in
      err_lines := input_all eic
    in
    let exit_status' = execute prog args ~stdin input_fn in
    (match exit_status with
     | None -> ()
     | Some exit_status -> Exit.check ~exit_status prog args exit_status'
    );
    let exit_status = exit_status' in
    Output.({ exit_status; stdout = !out_lines; stderr = !err_lines; })

  let read_stdout ?stdin ?exit_status prog args =
    let exit_status = match exit_status with None -> [0] | Some v -> v in
    (run ?stdin ~exit_status prog args).Output.stdout

end

include Blocking
