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

module Signal : sig
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

  val of_int : int -> t

  val to_string : t -> string

end

module Exit : sig
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

  val of_unix : Unix.process_status -> t

  val to_string : t -> string

  val error_to_string : error -> string
end

module Output : sig
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

include S with type 'a io = 'a
