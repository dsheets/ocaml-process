(*
 * Copyright (c) 2016 David Sheets <sheets@alum.mit.edu>
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

let try_test f () =
  try f ()
  with Process.Exit.Error e -> failwith (Process.Exit.error_to_string e)

module Pipes = struct

  let full_pipe () =
    let megabyte = Bytes.make (1024 * 1024) ' ' in
    Process.read_stdout
      ~stdin:megabyte "./test_megabyte_writer.native" [||] |> ignore

  let broken_pipe () =
    let megabyte = Bytes.make (1024 * 1024) ' ' in
    Process.read_stdout
      ~stdin:megabyte "./test_kilobyte_writer.native" [||] |> ignore

  let tests = [
    "full_pipe",   `Quick, try_test full_pipe;
    "broken_pipe", `Quick, try_test broken_pipe;
  ]
end

module Reading = struct

  let empty () =
    let output = Process.read_stdout "./test_empty_out.native" [||] in
    Alcotest.(check (list string)) "empty output" [] output

  let nl () =
    let output = Process.read_stdout "./test_nl_out.native" [||] in
    Alcotest.(check (list string)) "newline output" ["";""] output

  let trail_nl () =
    let output = Process.read_stdout "./test_trail_nl_out.native" [||] in
    Alcotest.(check (list string)) "trailing newline output"
      ["hello, world";""] output

  let start_nl () =
    let output = Process.read_stdout "./test_start_nl_out.native" [||] in
    Alcotest.(check (list string)) "starting newline output"
      [""; "hello, world"] output

  let interleave () =
    (* TODO: test the stderr, too *)
    let output = Process.read_stdout "./test_interleave_err_out.native" [||] in
    Alcotest.(check (list string)) "interleaved err and out output"
      ["longish"; "longerer"; "short"; ""] output

  let tests = [
    "empty",      `Quick, try_test empty;
    "nl",         `Quick, try_test nl;
    "trail_nl",   `Quick, try_test trail_nl;
    "start_nl",   `Quick, try_test start_nl;
    "interleave", `Quick, try_test interleave;
  ]
end

let tests = [
  "pipes",   Pipes.tests;
  "reading", Reading.tests;
]

;;
Alcotest.run "process" tests
