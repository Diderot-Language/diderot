(* log.sml
 *
 * Support for logging internal messages to a log file.
 *
 * COPYRIGHT (c) 2016 John Reppy (http://cs.uchicago.edu/~jhr)
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy
 * of this software and associated documentation files (the "Software"), to deal
 * in the Software without restriction, including without limitation the rights
 * to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
 * copies of the Software, and to permit persons to whom the Software is
 * furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in all
 * copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
 * AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
 * SOFTWARE.
 *
 * This file is part of the SML Compiler Utilities, which can be found at
 *
 *      https://github.com/JohnReppy/sml-compiler-utils
 *)

structure Log : sig

    val enabled : unit -> bool

    val init : string -> unit

    val logFile : unit -> TextIO.outstream

    val msg : string list -> unit

    val msg' : (unit -> string list) -> unit

  (* conditional dumping and checking of IR *)
    val after : {
            dumpCtl : bool Controls.control,
            checkCtl : bool Controls.control,
            output : TextIO.outstream * string * 'prog -> unit,
            checkIR : string * 'prog -> bool
          } -> string * 'prog -> 'prog

    val reportTiming : PhaseTimer.timer -> unit

  end = struct

    val enabledFlg = ref false
    val logStrm : TextIO.outstream option ref = ref NONE

    fun enabled () = !enabledFlg

    fun init file = (case !logStrm
           of NONE => let
                val outS = TextIO.openOut file
                in
                  enabledFlg := true;
                (* turn off buffering *)
                  TextIO.StreamIO.setBufferMode (TextIO.getOutstream outS, IO.NO_BUF);
                  logStrm := SOME outS
                end
            | SOME strm => raise Fail "multiple initialization of log file"
          (* end case *))

    fun logFile () = (case !logStrm
           of NONE => (init "/dev/null"; enabledFlg := false; logFile())
            | SOME outS => outS
          (* end case *))

    fun msg s = if !enabledFlg then TextIO.output(logFile(), String.concat s) else ()

    fun msg' msgFn = if !enabledFlg then TextIO.output(logFile(), String.concat(msgFn())) else ()

    fun after {dumpCtl, checkCtl, output, checkIR} (phase, prog) = let
          fun dump () = output(logFile(), "after "^phase, prog)
          in
            if Controls.get dumpCtl
              then dump()
              else ();
            if Controls.get checkCtl andalso checkIR("after " ^ phase, prog)
              then (
                if not(Controls.get dumpCtl)  (* avoid duplication *)
                  then dump()
                  else ();
                TextIO.output(TextIO.stdErr, concat [
                    "***** Internal error after ", phase, ": see log file for details\n"
                  ]);
                OS.Process.exit OS.Process.failure)
              else prog
          end

    fun reportTiming timer = PhaseTimer.report (logFile(), timer)

  end

