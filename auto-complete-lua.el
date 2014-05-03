;;; auto-complete-lua.el --- auto-complete.el source for lua builtin functions  -*- lexical-binding: t; -*-

;; Copyright (C) 2014  Rolando Pereira

;; Author: Rolando Pereira <finalyugi@sapo.pt>
;; Keywords: convenience

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Provide an auto-complete-source for the builtin lua functions.
;;
;; To install add this file in your load-path and then add the
;; following to your .emacs:
;;
;;    (require 'auto-complete-lua)
;;    (add-hook 'lua-mode-hook '(lambda ()
;;                                (setq ac-sources '(ac-source-lua))
;;                                (auto-complete-mode 1)))

;;; Code:

(defvar auto-complete-lua--builtin-functions
  '(("assert" . "assert (v [, message])")
     ("collectgarbage" . "collectgarbage ([opt [, arg]])")
     ("dofile" . "dofile ([filename])")
     ("error" . "error (message [, level])")
     ("getfenv" . "getfenv ([f])")
     ("getmetatable" . "getmetatable (object)")
     ("ipairs" . "ipairs (t)")
     ("load" . "load (func [, chunkname])")
     ("loadfile" . "loadfile ([filename])")
     ("loadstring" . "loadstring (string [, chunkname])")
     ("next" . "next (table [, index])")
     ("pairs" . "pairs (t)")
     ("pcall" . "pcall (f, arg1, ...)")
     ("print" . "print (...)")
     ("rawequal" . "rawequal (v1, v2)")
     ("rawget" . "rawget (table, index)")
     ("rawset" . "rawset (table, index, value)")
     ("select" . "select (index, ...)")
     ("setfenv" . "setfenv (f, table)")
     ("setmetatable" . "setmetatable (table, metatable)")
     ("tonumber" . "tonumber (e [, base])")
     ("tostring" . "tostring (e)")
     ("type" . "type (v)")
     ("unpack" . "unpack (list [, i [, j]])")
     ("xpcall" . "xpcall (f, err)")
     ("coroutine.create" . "coroutine.create (f)")
     ("coroutine.resume" . "coroutine.resume (co [, val1, ...])")
     ("coroutine.running" . "coroutine.running ()")
     ("coroutine.status" . "coroutine.status (co)")
     ("coroutine.wrap" . "coroutine.wrap (f)")
     ("coroutine.yield" . "coroutine.yield (...)")
     ("module" . "module (name [, ...])")
     ("require" . "require (modname)")
     ("package.loadlib" . "package.loadlib (libname, funcname)")
     ("package.seeall" . "package.seeall (module)")
     ("string.byte" . "string.byte (s [, i [, j]])")
     ("string.char" . "string.char (...)")
     ("string.dump" . "string.dump (function)")
     ("string.find" . "string.find (s, pattern [, init [, plain]])")
     ("string.format" . "string.format (formatstring, ...)")
     ("string.gmatch" . "string.gmatch (s, pattern)")
     ("string.gsub" . "string.gsub (s, pattern, repl [, n])")
     ("string.len" . "string.len (s)")
     ("string.lower" . "string.lower (s)")
     ("string.match" . "string.match (s, pattern [, init])")
     ("string.rep" . "string.rep (s, n)")
     ("string.reverse" . "string.reverse (s)")
     ("string.sub" . "string.sub (s, i [, j])")
     ("string.upper" . "string.upper (s)")
     ("table.concat" . "table.concat (table [, sep [, i [, j]]])")
     ("table.insert" . "table.insert (table, [pos,] value)")
     ("table.maxn" . "table.maxn (table)")
     ("table.remove" . "table.remove (table [, pos])")
     ("table.sort" . "table.sort (table [, comp])")
     ("math.abs" . "math.abs (x)")
     ("math.acos" . "math.acos (x)")
     ("math.asin" . "math.asin (x)")
     ("math.atan" . "math.atan (x)")
     ("math.atan2" . "math.atan2 (y, x)")
     ("math.ceil" . "math.ceil (x)")
     ("math.cos" . "math.cos (x)")
     ("math.cosh" . "math.cosh (x)")
     ("math.deg" . "math.deg (x)")
     ("math.exp" . "math.exp (x)")
     ("math.floor" . "math.floor (x)")
     ("math.fmod" . "math.fmod (x, y)")
     ("math.frexp" . "math.frexp (x)")
     ("math.ldexp" . "math.ldexp (m, e)")
     ("math.log" . "math.log (x)")
     ("math.log10" . "math.log10 (x)")
     ("math.max" . "math.max (x, ...)")
     ("math.min" . "math.min (x, ...)")
     ("math.modf" . "math.modf (x)")
     ("math.pow" . "math.pow (x, y)")
     ("math.rad" . "math.rad (x)")
     ("math.random" . "math.random ([m [, n]])")
     ("math.randomseed" . "math.randomseed (x)")
     ("math.sin" . "math.sin (x)")
     ("math.sinh" . "math.sinh (x)")
     ("math.sqrt" . "math.sqrt (x)")
     ("math.tan" . "math.tan (x)")
     ("math.tanh" . "math.tanh (x)")
     ("io.close" . "io.close ([file])")
     ("io.flush" . "io.flush ()")
     ("io.input" . "io.input ([file])")
     ("io.lines" . "io.lines ([filename])")
     ("io.open" . "io.open (filename [, mode])")
     ("io.output" . "io.output ([file])")
     ("io.popen" . "io.popen (prog [, mode])")
     ("io.read" . "io.read (...)")
     ("io.tmpfile" . "io.tmpfile ()")
     ("io.type" . "io.type (obj)")
     ("io.write" . "io.write (...)")
     ;; These ones can't be parsed because "file" could be anything
     ;; ("file:close" . "file:close ()")
     ;; ("file:flush" . "file:flush ()")
     ;; ("file:lines" . "file:lines ()")
     ;; ("file:read" . "file:read (...)")
     ;; ("file:seek" . "file:seek ([whence] [, offset])")
     ;; ("file:setvbuf" . "file:setvbuf (mode [, size])")
     ;; ("file:write" . "file:write (...)")
     ("os.clock" . "os.clock ()")
     ("os.date" . "os.date ([format [, time]])")
     ("os.difftime" . "os.difftime (t2, t1)")
     ("os.execute" . "os.execute ([command])")
     ("os.exit" . "os.exit ([code])")
     ("os.getenv" . "os.getenv (varname)")
     ("os.remove" . "os.remove (filename)")
     ("os.rename" . "os.rename (oldname, newname)")
     ("os.setlocale" . "os.setlocale (locale [, category])")
     ("os.time" . "os.time ([table])")
     ("os.tmpname" . "os.tmpname ()")
     ("debug.debug" . "debug.debug ()")
     ("debug.getfenv" . "debug.getfenv (o)")
     ("debug.gethook" . "debug.gethook ([thread])")
     ("debug.getinfo" . "debug.getinfo ([thread,] function [, what])")
     ("debug.getlocal" . "debug.getlocal ([thread,] level, local)")
     ("debug.getmetatable" . "debug.getmetatable (object)")
     ("debug.getregistry" . "debug.getregistry ()")
     ("debug.getupvalue" . "debug.getupvalue (func, up)")
     ("debug.setfenv" . "debug.setfenv (object, table)")
     ("debug.sethook" . "debug.sethook ([thread,] hook, mask [, count])")
     ("debug.setlocal" . "debug.setlocal ([thread,] level, local, value)")
     ("debug.setmetatable" . "debug.setmetatable (object, table)")
     ("debug.setupvalue" . "debug.setupvalue (func, up, value)")
     ("debug.traceback" . "debug.traceback ([thread,] [message [, level]])")))



(defun auto-complete-lua--get-lua-builtin-functions ()
  "Return a list containing the list of built-in functions in Lua."
  (mapcar #'car auto-complete-lua--builtin-functions))

(defun auto-complete-lua--get-documentation (lua-symbol)
  "Return a string containing the documention of LUA-SYMBOL.

LUA-SYMBOL should be a string representing the name of a builtin
lua function that exists in `auto-complete-lua--builtin-functions'

Return `nil' if LUA-SYMBOL doesn't exists in
`auto-complete-lua--builtin-functions'."
  (let ((lua-symbol-string (substring-no-properties lua-symbol)))
    (cdr (assoc lua-symbol-string auto-complete-lua--builtin-functions))))

(defvar ac-source-lua
  '((candidates . auto-complete-lua--get-lua-builtin-functions)
     (document . auto-complete-lua--get-documentation)
     (cache))
  "A auto-complete.el source for the builtin lua functions.")


(provide 'auto-complete-lua)
;;; auto-complete-lua.el ends here
