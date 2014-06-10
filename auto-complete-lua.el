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
  '(
     ("bit32.arshift" . "Returns the number `x` shifted `disp` bits to the right. The number `disp` may be any
representable integer. Negative displacements shift to the left.

This shift operation is what is called arithmetic shift.
Vacant bits on the left are filled with copies of the higher bit of `x`;
vacant bits on the right are filled with zeros. In particular, displacements with
absolute values higher than 31 result in zero or `0xFFFFFFFF`
(all original bits are shifted out).

@function [parent=#bit32] arshift
@param #number x value to shift
@param #number disp the number of shift, may an positive or negative integer
@return #number shifted number
")
     ("bit32.band" . "Returns the bitwise `and` of its operands.
@function [parent=#bit32] band
@param #number ... operands
@return #number bitwise `and` of operands
")
     ("bit32.bnot" . "Returns the bitwise negation of `x`. For any integer `x`, the following identity holds:

		assert(bit32.bnot(x) == (-1 - x) % 2^32)

@function [parent=#bit32] bnot
@param #number x Number to proceed
@return #number bitwise negation
")
     ("bit32.bor" . "Returns the bitwise `or` of its operands.
@function [parent=#bit32] bor
@param #number ... operands
@return #number bitwise `or` of operands
")
     ("bit32.btest" . "Returns a boolean signaling whether the bitwise `and` of its operands is different from zero.
@function [parent=#bit32] btest
@return #boolean true if the bitwise `and` of its operands is different from zero
")
     ("bit32.bxor" . "Returns the bitwise exclusive `or` of its operands.
@function [parent=#bit32] bxor
@param #number ... operands
@return #number bitwise exclusive `or` of its operands.
")
     ("bit32.extract" . "Returns the unsigned number formed by the bits `field` to `field + width - 1` from `n`.
Bits are numbered from `0` (least significant) to `31` (most significant).
All accessed bits must be in the range `[0, 31]`.

The default for `width` is `1`.

@function [parent=#bit32] extract
@param #number n input number
@param #number field bit field to apply
@param #number width the number of bit to take in account (optional, 1 by default)
@return #number extracted number
")
     ("bit32.replace" . "Returns a copy of `n` with the bits `field` to `field + width - 1` replaced by the value `v`.
See `bit32.extract` for details about field and width.
@function [parent=#bit32] replace
@param #number n the number to copy
@param #number v the value v
@param #number field bits field to apply
@param #number width the number of bit to take in account (optional, 1 by default)
@return #number replaced number
")
     ("bit32.lrotate" . "Returns the number `x` rotated `disp` bits to the left. The number `disp` may be
any representable integer.
For any valid displacement, the following identity holds:

    assert(bit32.lrotate(x, disp) == bit32.lrotate(x, disp % 32))

In particular, negative displacements rotate to the right.
@function [parent=#bit32] lrotate
@param #number x original number
@param #number disp number of rotate
@return #number rotated number
")
     ("bit32.lshift" . "Returns the number `x` shifted `disp` bits to the left. The number `disp` may be any representable integer.
Negative displacements shift to the right. In any direction, vacant bits are filled with zeros.
In particular, displacements with absolute values higher than `31` result in zero (all bits are shifted out).
For positive displacements, the following equality holds:

     assert(bit32.lshift(b, disp) == (b * 2^disp) % 2^32)

@function [parent=#bit32] lshift
@param #number x original number
@param #number disp the number of shift
@return #number shifted number
")
     ("bit32.rrotate" . "Returns the number `x` rotated `disp` bits to the right. The number `disp` may be any representable integer.
For any valid displacement, the following identity holds:

     assert(bit32.rrotate(x, disp) == bit32.rrotate(x, disp % 32))

In particular, negative displacements rotate to the left.
@function [parent=#bit32] rrotate
@param #number x original number
@param #number disp number of bits to rotate
@return #number rotated number
")
     ("string.byte" . "Returns the internal numerical codes of the characters `s[i], s[i+1], ..., s[j]`. The default value
for i is 1; the default value for j is i. These indices are corrected following the same rules of
function `string.sub`.

Numerical codes are not necessarily portable across platforms.

@function [parent=#string] byte
@param #string s string to handle.
@param #number i start index, default value is 1.
@param #number j end index, default value is `i`.
@return the internal numerical codes of the characters `s[i]`, `s[i+1]`,..., `s[j]`
")
     ("string.char" . "Receives zero or more integers. Returns a string with length equal to
the number of arguments, in which each character has the internal numerical
code equal to its corresponding argument.

Note that numerical codes are not necessarily portable across platforms.
@function [parent=#string] char
@param ... zero or more integers.
@return #string a string with length equal to
the number of arguments, in which each character has the internal numerical
code equal to its corresponding argument.
")
     ("string.dump" . "Returns a string containing a binary representation of the given function,
so that a later `load` on this string returns a copy of the function (but with new upvalues).
@function [parent=#string] dump
@param f the function to dump.
@return #string a string representation of the given function.
")
     ("string.find" . "Looks for the first match of `pattern` in the string `s`. If it finds a
match, then `find` returns the indices of `s` where this occurrence starts
and ends; otherwise, it returns **nil**. A third, optional numerical argument
`init` specifies where to start the search; its default value is 1 and
can be negative. A value of **true** as a fourth, optional argument `plain`
turns off the pattern matching facilities, so the function does a plain
\"find substring\" operation, with no characters in `pattern` being considered
\"magic\". Note that if plain is given, then init must be given as well.

If the pattern has captures, then in a successful match the captured values
are also returned, after the two indices.
@function [parent=#string] find
@param #string s string to handle.
@param #string pattern pattern to search.
@param #number init index where to start the search. (default value is 1)
@param #boolean plain set to true to search without pattern matching. (default value is false)
@return #number, #number start and end indices of first occurrence.
@return #nil if pattern not found.
")
     ("string.format" . "Returns a formatted version of its variable number of arguments following the description given
in its first argument (which must be a string). The format string follows the same rules as the
ANSI C function `sprintf`. The only differences are that the options/modifiers `*`, `h`, `L`, `l`, `n`, and
`p` are not supported and that there is an extra option, `q`. The `q` option formats a string between
double quotes, using escape sequences when necessary to ensure that it can safely be read back
by the Lua interpreter. For instance, the call

    string.format('%q', 'a string with \"quotes\" and \\n new line')

will produce the string:

    \"a string with \\\"quotes\\\" and \\
        new line\"

Options `A` and `a` (when available), `E`, `e`, `f`, `G`, and `g` all expect a number as argument.
Options `c`, `d`, `i`, `o`, `u`, `X`, and `x` also expect a number, but the range of that number
may be limited by the underlying C implementation. For options `o`, `u`, `X`, and `x`, the number cannot
be negative. Option `q` expects a string; option `s` expects a string without embedded zeros.
If the argument to option `s` is not a string, it is converted to one following the same rules of `tostring`.
@function [parent=#string] format
@param #string formatstring the string template.
@param ... arguments could be strings or numbers.
@return #string the formatted string.
")
     ("string.gmatch" . "Returns an iterator function that, each time it is called, returns the
next captures from `pattern` over string `s`. If `pattern` specifies no
captures, then the whole match is produced in each call.
 As an example, the following loop will iterate over all the words from string s, printing one per line:

    s = \"hello world from Lua\"
    for w in string.gmatch(s, \"%a+\") do
      print(w)
    end

The next example collects all pairs key=value from the given string into a table:

    t = {}
    s = \"from=world, to=Lua\"
    for k, v in string.gmatch(s, \"(%w+)=(%w+)\") do
      t[k] = v
    end

For this function, a '`^`' at the start of a pattern does not work as an
anchor, as this would prevent the iteration.
@function [parent=#string] gmatch
@param #string s string to handle.
@param #string pattern pattern to search.
@return Iterator of captures.
")
     ("string.gsub" . "Returns a copy of `s` in which all (or the first `n`, if given) occurrences of
the pattern have been replaced by a replacement string specified by `repl`,
which can be a string, a table, or a function. `gsub` also returns, as its second
value, the total number of matches that occurred. The name `gsub` comes from
`Global SUBstitution`.

If `repl` is a string, then its value is used for replacement. The character
`%` works as an escape character: any sequence in `repl` of the form `%n`,
with *n* between 1 and 9, stands for the value of the *n*-th captured
substring (see below). The sequence `%0` stands for the whole match. The
sequence `%%` stands for a single `%`.

If `repl` is a table, then the table is queried for every match, using the first capture as the key.

If `repl` is a function, then this function is called every time a match occurs,
with all captured substrings passed as arguments, in order.

If the value returned by the table query or by the function call is a string or
a number, then it is used as the replacement string; otherwise, if it is **false** or **nil**,
then there is no replacement (that is, the original match is kept in the string).

Here are some examples:

    x = string.gsub(\"hello world\", \"(%w+)\", \"%1 %1\")
    --> x=\"hello hello world world\"

    x = string.gsub(\"hello world\", \"%w+\", \"%0 %0\", 1)
    --> x=\"hello hello world\"

    x = string.gsub(\"hello world from Lua\", \"(%w+)%s*(%w+)\", \"%2 %1\")
    --> x=\"world hello Lua from\"

    x = string.gsub(\"home = $HOME, user = $USER\", \"%$(%w+)\", os.getenv)
    --> x=\"home = /home/roberto, user = roberto\"

    x = string.gsub(\"4+5 = $return 4+5$\", \"%$(.-)%$\", function (s)
          return loadstring(s)()
        end)
    --> x=\"4+5 = 9\"

    local t = {name=\"lua\", version=\"5.1\"}
    x = string.gsub(\"$name-$version.tar.gz\", \"%$(%w+)\", t)
    --> x=\"lua-5.2.tar.gz\"

@function [parent=#string] gsub
@param #string s string to handle.
@param #string pattern pattern to search.
@param repl replacement could be a string, a table or a function.
@param #number n number of occurences to replace, default is nil which means all occurences.
@return #string a modified copy of `s`.
")
     ("string.len" . "Receives a string and returns its length. The empty string `\"\"` has
length 0. Embedded zeros are counted, so `\"a\\000bc\\000\"` has length 5.
@function [parent=#string] len
@param #string s string to handle.
@return #number the lenght of `s`.
")
     ("string.lower" . "Receives a string and returns a copy of this string with all uppercase
letters changed to lowercase. All other characters are left unchanged. The
definition of what an uppercase letter is depends on the current locale.
@function [parent=#string] lower
@param #string s string to handle.
@return #string a lower case version of `s`.
")
     ("string.match" . "Looks for the first *match* of `pattern` in the string `s`. If it
finds one, then `match` returns the captures from the pattern; otherwise
it returns **nil**. If `pattern` specifies no captures, then the whole match
is returned. A third, optional numerical argument `init` specifies where
to start the search; its default value is 1 and can be negative.
@function [parent=#string] match
@param #string s string to handle.
@param #string pattern pattern to search.
@param #number init index where to start the search. (default value is 1)
@return #string the captures from the pattern; otherwise it returns nil. If pattern specifies no captures, then the whole match is returned.
")
     ("string.rep" . "Returns a string that is the concatenation of `n` copies of the string `s`.
@function [parent=#string] rep
@param #string s string to handle.
@param #number n number of repetition.
@return #string the concatenation of `n` copies of the string `s`.
")
     ("string.reverse" . "Returns a string that is the string `s` reversed.
@function [parent=#string] reverse
@param #string s string to handle.
@return #string the string `s` reversed.
")
     ("string.sub" . "Returns the substring of `s` that starts at `i` and continues until
`j`; `i` and `j` can be negative. If `j` is absent, then it is assumed to
be equal to -1 (which is the same as the string length). In particular,
the call `string.sub(s,1,j)` returns a prefix of `s` with length `j`, and
`string.sub(s, -i)` returns a suffix of `s` with length `i`.
If, after the translation of negative indices, i is less than 1, it is corrected to 1.

If `j` is greater than the string length, it is corrected to that length. If, after these
corrections, `i` is greater than `j`, the function returns the empty string.

@function [parent=#string] sub
@param #string s string to handle.
@param #number i start index.
@param #number j end index. (default value is -1, which is the same as the string lenght)
@return #string the substring of `s` that starts at `i` and continues until `j`.
")
     ("package.loadlib" . "Dynamically links the host program with the C library `libname`.
Inside this library, looks for a function `funcname` and returns this
function as a C function.
(So, `funcname` must follow the protocol (see `lua_CFunction`)).

This is a low-level function. It completely bypasses the package and module
system. Unlike `require`, it does not perform any path searching and does
not automatically adds extensions. `libname` must be the complete file name
of the C library, including if necessary a path and extension. `funcname`
must be the exact name exported by the C library (which may depend on the
C compiler and linker used).

This function is not supported by ANSI C. As such, it is only available
on some platforms (Windows, Linux, Mac OS X, Solaris, BSD, plus other Unix
systems that support the `dlfcn` standard).
@function [parent=#package] loadlib
@param #string libname the complete file name of the C library.
@param #string funcname the name of a function defined in the C library named `libname`.
")
     ("io.close" . "Equivalent to `file:close`. Without a `file`, closes the default output file.
@function [parent=#io] close
@param #file file file to close (optional).
@return A status code which is system-dependent (optional).
")
     ("io.flush" . "Equivalent to `io.output():flush()`.
@function [parent=#io] flush

")
     ("io.input" . "When called with a file name, it opens the named file (in text mode),
and sets its handle as the default input file. When called with a file
handle, it simply sets this file handle as the default input file. When
called without parameters, it returns the current default input file.

In case of errors this function raises the error, instead of returning an
error code.
@function [parent=#io] input
@param file a filename or a file handle which will used as default input. (optional)
@return #file the default input file handle.
")
     ("io.lines" . "Opens the given file name in read mode and returns an iterator function that
works like file:lines(...) over the opened file. When the iterator function
detects the end of file, it returns **nil** (to finish the loop) and automatically
closes the file.

The call `io.lines()` (with no file name) is equivalent to `io.input():lines()`;
that is, it iterates over the lines of the default input file. In this
case it does not close the file when the loop ends.

In case of errors this function raises the error, instead of returning an error code.
@function [parent=#io] lines
@param filename a filename or a file handle. (default value is `io.input()`)
@return iterator function on lines
")
     ("io.open" . "This function opens a file, in the mode specified in the string `mode`. It
returns a new file handle, or, in case of errors, **nil** plus an error message.
The `mode` string can be any of the following:

 * _\"r\"_: read mode (the default);
 * _\"w\"_: write mode;
 * _\"a\"_: append mode;
 * _\"r+\"_: update mode, all previous data is preserved;
 * _\"w+\"_: update mode, all previous data is erased;
 * _\"a+\"_: append update mode, previous data is preserved, writing is only
  allowed at the end of file.

The `mode` string can also have a '`b`' at the end, which is needed in
some systems to open the file in binary mode.
@function [parent=#io] open
@param #string filename path to the file.
@param #string mode used to specify the open mode (optional).
@return #file a file handle.
")
     ("io.output" . "Similar to `io.input`, but operates over the default output file.
@function [parent=#io] output
@param file a filename or a file handle which will used as default output. (optional)
@return #file the default ouput file handle.
")
     ("io.popen" . "Starts program `prog` in a separated process. Returns a file handle
that you can use to read data from this program (if `mode` is `\"r\"`,
the default) or to write data to this program (if `mode` is `\"w\"`).

This function is system dependent and is not available on all platforms.
@function [parent=#io] popen
@param #string prog the program to start.
@param #string mode used to specify the open mode.
@return #file a file handle used to read from or write to the program `prog`.
")
     ("io.read" . "Equivalent to `io.input():read`.
@function [parent=#io] read
@param format _\"*n\"_, _\"*a\"_, _\"*l\"_ or a number.
@return A string (or a number) with the characters read
@return #nil if it cannot read data with the specified format.
")
     ("io.tmpfile" . "Returns a handle for a temporary file. This file is opened in update
mode and it is automatically removed when the program ends.
@function [parent=#io] tmpfile
@return #file a file handle for a temporary file.
")
     ("io.type" . "Checks whether `obj` is a valid file handle. Returns the string `\"file\"`
if `obj` is an open file handle, `\"closed file\"` if `obj` is a closed file
handle, or **nil** if `obj` is not a file handle.
@function [parent=#io] type
@param obj
")
     ("io.write" . "Equivalent to `io.output():write`.
@function [parent=#io] write
@param ... must be strings or numbers.
@return #file the standard output
@return #nil, #string an error message, if it failed.
")
     ("file.close" . "Closes `file`. Note that files are automatically closed when their
handles are garbage collected, but that takes an unpredictable amount of
time to happen.

When closing a file handle created with `io.popen`, `file:close` returns the
same values returned by `os.execute`.
@function [parent=#file] close
@param self
@return A status code which is system-dependent (optional).
")
     ("file.flush" . "Saves any written data to `file`.
@function [parent=#file] flush
@param self
")
     ("file.lines" . "Returns an iterator function that, each time it is called, reads the file
according to the given formats. When no format is given, uses \"`*l`\" as a
default. As an example, the construction

    for line in file:lines() do body end

will iterate over all lines of the file. Unlike `io.lines`, this function
does not close the file when the loop ends.
In case of errors this function raises the error, instead of returning an error code.
@function [parent=#file] lines
@param self
@param ... reading formats (optional, \"`*l`\" by default)
@return iterator function over the file
")
     ("file.read" . "Reads the file `file`, according to the given formats, which specify
what to read. For each format, the function returns a string (or a number)
with the characters read, or nil if it cannot read data with the specified
format. When called without formats, it uses a default format that reads
the entire next line (see below).
The available formats are

  * _\"*n\"_: reads a number; this is the only format that returns a number
  instead of a string.
  * _\"*a\"_: reads the whole file, starting at the current position. On end of
  file, it returns the empty string.
  * _\"*l\"_: reads the next line (skipping the end of line), returning **nil** on
  end of file. This is the default format.
  * _number_: reads a string with up to this number of characters, returning
  **nil** on end of file. If number is zero, it reads nothing and returns an
  empty string, or **nil** on end of file.
@function [parent=#file] read
@param self
@param format _\"*n\"_, _\"*a\"_, _\"*l\"_ or a number.
@return A string (or a number) with the characters read
@return #nil if it cannot read data with the specified format
")
     ("file.seek" . "Sets and gets the file position. It is measured from the beginning of the
file, to the position given by `offset` plus a base specified by the string
`whence`, as follows:

 * _\"set\"_: base is position 0 (beginning of the file);
 * _\"cur\"_: base is current position;
 * _\"end\"_: base is end of file;

In case of success, function `seek` returns the final file position,
measured in bytes from the beginning of the file. If this function fails,
it returns **nil**, plus a string describing the error.
The default value for `whence` is `\"cur\"`, and for `offset` is 0. Therefore,
the call `file:seek()` returns the current file position, without changing
it; the call `file:seek(\"set\")` sets the position to the beginning of the
file (and returns 0); and the call `file:seek(\"end\")` sets the position
to the end of the file, and returns its size.
@function [parent=#file] seek
@param self
@param #string whence  `\"set\"`, `\"cur\"` or `\"end\"` (optional, default value is `\"cur\"`)
@param #number offset offset of end position. (optional, default value is `0`)
@return #number the final file position in bytes, if it succeed.
@return #nil, #string an error message, if it failed.
")
     ("file.setvbuf" . "Sets the buffering mode for an output file. There are three available
modes:

 * `\"no\"` : no buffering; the result of any output operation appears immediately.
 * `\"full\"` : full buffering; output operation is performed only when the
  buffer is full or when you explicitly `flush` the file (see `io.flush`).
 * `\"line\"` : line buffering; output is buffered until a newline is output or
  there is any input from some special files (such as a terminal device).

For the last two cases, `size` specifies the size of the buffer, in
bytes. The default is an appropriate size.
@function [parent=#file] setvbuf
@param self
@param #string mode the buffering mode : `\"no\"`, `\"full\"` or `\"line\"`.
@param #number size the size of the buffer.(default value is an appropriate size)

")
     ("math.abs" . "Returns the absolute value of `x`.
@function [parent=#math] abs
@param #number x
@return #number
")
     ("math.acos" . "Returns the arc cosine of `x` (in radians).
@function [parent=#math] acos
@param #number x
@return #number
")
     ("math.asin" . "Returns the arc sine of `x` (in radians).
@function [parent=#math] asin
@param #number x
@return #number
")
     ("math.atan" . "Returns the arc tangent of `x` (in radians).
@function [parent=#math] atan
@param #number x
@return #number
")
     ("math.atan2" . "Returns the arc tangent of `y/x` (in radians), but uses the signs
of both parameters to find the quadrant of the result. (It also handles
correctly the case of `x` being zero.)
@function [parent=#math] atan2
@param #number y
@param #number x
@return #number
")
     ("math.ceil" . "Returns the smallest integer larger than or equal to `x`.
@function [parent=#math] ceil
@param #number x
@return #number
")
     ("math.cos" . "Returns the cosine of `x` (assumed to be in radians).
@function [parent=#math] cos
@param #number x
@return #number
")
     ("math.cosh" . "Returns the hyperbolic cosine of `x`.
@function [parent=#math] cosh
@param #number x
@return #number
")
     ("math.deg" . "Returns the angle `x` (given in radians) in degrees.
@function [parent=#math] deg
@param #number x
@return #number
")
     ("math.exp" . "Returns the value *e^x*.
@function [parent=#math] exp
@param #number x
@return #number
")
     ("math.floor" . "Returns the largest integer smaller than or equal to `x`.
@function [parent=#math] floor
@param #number x
@return #number
")
     ("math.fmod" . "Returns the remainder of the division of `x` by `y` that rounds the
quotient towards zero.
@function [parent=#math] fmod
@param #number x
@param #number y
@return #number
")
     ("math.frexp" . "Returns `m` and `e` such that *x = m2^e*, `e` is an integer and the
absolute value of `m` is in the range *[0.5, 1)* (or zero when `x` is zero).
@function [parent=#math] frexp
@param #number x
@return #number
")
     ("math.ldexp" . "Returns *m2^e* (`e` should be an integer).
@function [parent=#math] ldexp
@param #number m
@param #number e
@return #number
")
     ("math.log" . "Returns the logarithm of `x` in the given base. The default for `base` is `e`
(so that the function returns the natural logarithm of `x`).
@function [parent=#math] log
@param #number x
@param #number base
@return #number
")
     ("math.max" . "Returns the maximum value among its arguments.
@function [parent=#math] max
@param #number x first number
@param #number ... other numbers
@return #number
")
     ("math.min" . "Returns the minimum value among its arguments.
@function [parent=#math] min
@param #number x first number
@param #number ... other numbers
@return #number
")
     ("math.modf" . "Returns two numbers, the integral part of `x` and the fractional part of
`x`.
@function [parent=#math] modf
@param #number x
@return #number
")
     ("math.pow" . "Returns *x^y*. (You can also use the expression `x^y` to compute this
value.)
@function [parent=#math] pow
@param #number x
@param #number y
@return #number
")
     ("math.rad" . "Returns the angle `x` (given in degrees) in radians.
@function [parent=#math] rad
@param #number x
@return #number
")
     ("math.random" . "This function is an interface to the simple pseudo-random generator
function `rand` provided by ANSI C. (No guarantees can be given for its
statistical properties.)

When called without arguments, returns a uniform pseudo-random real
number in the range *[0,1)*. When called with an integer number `m`,
`math.random` returns a uniform pseudo-random integer in the range *[1,
m]*. When called with two integer numbers `m` and `n`, `math.random`
returns a uniform pseudo-random integer in the range *[m, n]*.
@function [parent=#math] random
@param #number m
@param #number n
@return #number
")
     ("math.randomseed" . "Sets `x` as the \"seed\" for the pseudo-random generator: equal seeds
produce equal sequences of numbers.
@function [parent=#math] randomseed
@param #number x
@return #number
")
     ("math.sin" . "Returns the sine of `x` (assumed to be in radians).
@function [parent=#math] sin
@param #number x
@return #number
")
     ("math.sinh" . "Returns the hyperbolic sine of `x`.
@function [parent=#math] sinh
@param #number x
@return #number
")
     ("math.sqrt" . "Returns the square root of `x`. (You can also use the expression `x^0.5`
to compute this value.)
@function [parent=#math] sqrt
@param #number x
@return #number
")
     ("math.tan" . "Returns the tangent of `x` (assumed to be in radians).
@function [parent=#math] tan
@param #number x
@return #number
")
     ("os.clock" . "Returns an approximation of the amount in seconds of CPU time used by
the program.
@function [parent=#os] clock
@return #number the amount in seconds of CPU time used by
the program.
")
     ("os.date" . "Returns a string or a table containing date and time, formatted according
to the given string `format`.

If the `time` argument is present, this is the time to be formatted
(see the `os.time` function for a description of this value). Otherwise,
`date` formats the current time.

If `format` starts with '`!`', then the date is formatted in Coordinated
Universal Time. After this optional character, if `format` is the string
\"`*t`\", then `date` returns a table with the following fields:

  * `year` (four digits)
  * `month` (1--12)
  * `day` (1--31)
  * `hour` (0--23)
  * `min` (0--59)
  * `sec` (0--61)
  * `wday` (weekday, Sunday is 1)
  * `yday` (day of the year)
  * `isdst` (daylight saving flag, a boolean).

If `format` is not \"`*t`\", then `date` returns the date as a string,
formatted according to the same rules as the C function `strftime`.
When called without arguments, `date` returns a reasonable date and time
representation that depends on the host system and on the current locale
(that is, `os.date()` is equivalent to `os.date(\"%c\")`).
On non-Posix systems, this function may be not thread safe because of its
reliance on C function gmtime and C function localtime.
@function [parent=#os] date
@param #string format format of date (optional).
@param #number time time to format (optional, default value is current time)
@return #string a formatted string representation of `time`.
")
     ("os.difftime" . "Returns the number of seconds from time `t1` to time `t2`. In POSIX,
Windows, and some other systems, this value is exactly `t2`*-*`t1`.
@function [parent=#os] difftime
@param #number t2
@param #number t1
@return #number the number of seconds from time `t1` to time `t2`.
")
     ("os.execute" . "This function is equivalent to the ANSI C function `system`. It passes command
to be executed by an operating system shell. Its first result is **true** if
the command terminated successfully, or **nil** otherwise. After this first
result the function returns a string and a number, as follows:

* _\"exit\"_: the command terminated normally; the following number is
the exit status of the command.
* _\"signal\"_: the command was terminated by a signal; the following number
is the signal that terminated the command.

When called without a command, os.execute returns a boolean that is true
if a shell is available.
@function [parent=#os] execute
@param #string command command to be executed (optional).
@return A status code which is system-dependent.
@return #boolean true if a shell is available
")
     ("os.exit" . "Calls the ANSI C function exit to terminate the host program. If code is **true**,
the returned status is `EXIT_SUCCESS`; if code is **false**, the returned status is
`EXIT_FAILURE`; if code is a number, the returned status is this number.
The default value for code is `true`.

If the optional second argument `close` is **true**, closes the Lua state before exiting.
@function [parent=#os] exit
@param #number code an exit code. (optional, **true** by default)
@param #boolean close indicate if the Lua state have to be closed (optional, **false** by default)
")
     ("os.getenv" . "Returns the value of the process environment variable `varname`, or
**nil** if the variable is not defined.
@function [parent=#os] getenv
@param #string varname an environment variable name.
@return The value of the process environment variable `varname`
@return #nil if the variable is not defined.
")
     ("os.remove" . "Deletes the file (or empty directory, on POSIX systems) with the given name.
If this function fails, it returns **nil**, plus a string describing the error
and the error code.
@function [parent=#os] remove
@param filename the path to the file or directory to delete.
@return #nil, #string an error message if it failed.
")
     ("os.rename" . "Renames file or directory named `oldname` to `newname`. If this function
fails, it returns **nil**, plus a string describing the error.
@function [parent=#os] rename
@param oldname the path to the file or directory to rename.
@param newname the new path.
@return #nil, #string an error message if it failed.
")
     ("os.setlocale" . "Sets the current locale of the program. `locale` is a string specifying
a locale; `category` is an optional string describing which category to
change: `\"all\"`, `\"collate\"`, `\"ctype\"`, `\"monetary\"`, `\"numeric\"`, or
`\"time\"`; the default category is `\"all\"`. The function returns the name
of the new locale, or **nil** if the request cannot be honored.

If `locale` is the empty string, the current locale is set to an
implementation-defined native locale. If `locale` is the string \"`C`\",
the current locale is set to the standard C locale.

When called with nil as the first argument, this function only returns
the name of the current locale for the given category.
@function [parent=#os] setlocale
@param #string locale a string specifying a locale.
@param #string category `\"all\"`, `\"collate\"`, `\"ctype\"`, `\"monetary\"`, `\"numeric\"`, or
`\"time\"`; the default category is `\"all\"`.
@return #string the current locale.
")
     ("os.time" . "Returns the current time when called without arguments, or a time representing
the date and time specified by the given table. This table must have fields
`year`, `month`, and `day`, and may have fields `hour` (default is `12`),
`min` (default is `0`), `sec` (default is `0`), and `isdst` (default is **nil**).
For a description of these fields, see the `os.date` function.

The returned value is a number, whose meaning depends on your system. In POSIX,
Windows, and some other systems, this number counts the number of seconds since
some given start time (the \"epoch\"). In other systems, the meaning is not specified,
and the number returned by `time` can be used only as an argument to `os.date`
and `os.difftime`.
@function [parent=#os] time
@param #table table A table describing the time (optional)
@return #number timestamp formatted according to the current system.
")
     ("debug.debug" . "Enters an interactive mode with the user, running each string that
the user enters. Using simple commands and other debug facilities,
the user can inspect global and local variables, change their values,
evaluate expressions, and so on. A line containing only the word `cont`
finishes this function, so that the caller continues its execution.

Note that commands for `debug.debug` are not lexically nested within any
function, and so have no direct access to local variables.
@function [parent=#debug] debug
")
     ("debug.gethook" . "Returns the current hook settings of the thread, as three values: the
current hook function, the current hook mask, and the current hook count
(as set by the `debug.sethook` function).
@function [parent=#debug] gethook
@param #thread thread thread to handle (optional).
")
     ("debug.getinfo" . "Returns a table with information about a function. You can give the
function directly, or you can give a number as the value of `func`,
which means the function running at level `func` of the call stack
of the given thread: level 0 is the current function (`getinfo` itself);
level 1 is the function that called `getinfo`; and so on. If `function`
is a number larger than the number of active functions, then `getinfo`
returns **nil**.

The returned table can contain all the fields returned by `lua_getinfo`,
with the string `what` describing which fields to fill in. The default for
`what` is to get all information available, except the table of valid
lines. If present, the option '`f`' adds a field named `func` with
the function itself. If present, the option '`L`' adds a field named
`activelines` with the table of valid lines.

For instance, the expression `debug.getinfo(1,\"n\").name` returns a table
with a name for the current function, if a reasonable name can be found,
and the expression `debug.getinfo(print)` returns a table with all available
information about the `print` function.
@function [parent=#debug] getinfo
@param #thread thread thread to handle (optional).
@param func the function or a number which means the function running at level `func`.
@param #string what used to precise information returned (optional).
@return #table with information about the function `func`.
")
     ("debug.getlocal" . "This function returns the name and the value of the `local` variable with index
local of the function at level `f` of the stack. This function accesses not only
xplicit local variables, but also parameters, temporaries, etc.

The first parameter or local variable has index `1`, and so on, until the last
active variable. Negative indices refer to vararg parameters; `-1` is the first
vararg parameter. The function returns **nil** if there is no variable with the
given index, and raises an error when called with a level out of range.
(You can call `debug.getinfo` to check whether the level is valid.)

Variable names starting with '(' (open parenthesis) represent internal
variables (loop control variables, temporaries, varargs, and C function locals).

The parameter `f` may also be a function. In that case, getlocal returns
only the name of function parameters.
@function [parent=#debug] getlocal
@param #thread thread thread which owns the local variable (optional).
@param f the stack level or a function
@param #number local the index of the local variable.
@return #string The name and the value of the local variable with
index `local` of the function at level `level` of the stack.
@return #nil if no variable was found
")
     ("debug.getmetatable" . "Returns the metatable of the given `value` or **nil** if it does not have
a metatable.
@function [parent=#debug] getmetatable
@param value value to handle.
@return #table the metatable of the given `object` or nil if it does not have
a metatable.
")
     ("debug.getregistry" . "Returns the registry table.
@function [parent=#debug] getregistry
@return #table The registry table
")
     ("debug.getupvalue" . "This function returns the name and the value of the upvalue with index
`up` of the function `f`. The function returns **nil** if there is no
upvalue with the given index.
@function [parent=#debug] getupvalue
@param f function which owns the upvalue.
@param #number up index of upvalue.
@return The name and the value of the upvalue of the function `f`.
@return #nil no upvalue found.
")
     ("debug.getuservalue" . "Returns the Lua value associated to `u`. If `u` is not a userdata, returns **nil**.
@function [parent=#debug] getuservalue
@param #userdata u userdata
@return the value of the userdata
@return #nil no userdata found
")
     ("debug.sethook" . "Sets the given function as a hook. The string `mask` and the number
`count` describe when the hook will be called. The string mask may have
the following characters, with the given meaning:

  * `\"c\"`: the hook is called every time Lua calls a function;
  * `\"r\"`: the hook is called every time Lua returns from a function;
  * `\"l\"`: the hook is called every time Lua enters a new line of code.

With a `count` different from zero, the hook is called after every `count`
instructions.

When called without arguments, `debug.sethook` turns off the hook.

When the hook is called, its first parameter is a string describing
the event that has triggered its call: `\"call\"`, `\"return\"` (or `\"tail
return\"`, when simulating a return from a tail call), `\"line\"`, and
`\"count\"`. For line events, the hook also gets the new line number as its
second parameter. Inside a hook, you can call `getinfo` with level 2 to
get more information about the running function (level 0 is the `getinfo`
function, and level 1 is the hook function).
@function [parent=#debug] sethook
@param #thread thread thread on which the hook is set (optional).
@param hook a function which takes two argument : event as string and line number.
@param #string mask could be `\"c\"`, `\"r\"` or `\"l\"`.
@param #number count the hook is called after every `count` instructions (optional).
")
     ("debug.setlocal" . "This function assigns the value `value` to the local variable with
index `local` of the function at level `level` of the stack. The function
returns **nil** if there is no local variable with the given index, and raises
an error when called with a `level` out of range. (You can call `getinfo`
to check whether the level is valid.) Otherwise, it returns the name of
the local variable.
See debug.getlocal for more information about variable indices and names.
@function [parent=#debug] setlocal
@param #thread thread thread which owns the local variable (optional).
@param #number level the stack level.
@param #number local the index of the local variable.
@param value the new value.
@return #string the name of the variable if it succeed.
@return #nil no local variable with the given index.
")
     ("debug.setmetatable" . "Sets the metatable for the given `value` to the given `table` (which
can be **nil**). Returns `value`.
@function [parent=#debug] setmetatable
@param value value to handle.
@param #table table the metatable for `object`.
@return the given value
")
     ("debug.setupvalue" . "This function assigns the value `value` to the upvalue with index `up`
of the function `f`. The function returns **nil** if there is no upvalue
with the given index. Otherwise, it returns the name of the upvalue.
@function [parent=#debug] setupvalue
@param func function which owns the upvalue.
@param #number up index of the upvalue.
@param value the new value.
@return #string the name of the upvalue if it succeed.
@return #nil if there no upvalue with the given index.
")
     ("debug.traceback" . "If `message` is present but is neither a `#string` nor **nil**, this function returns
message without further processing. Otherwise, it returns a string with
a traceback of the call stack. An optional `message` string is appended at
the beginning of the traceback. An optional `level` number tells at which
level to start the traceback (default is 1, the function calling traceback).
@function [parent=#debug] traceback
@param #thread thread thread which owns the local variable (optional).
@param #string message  original message (optional).
@param #number level (1 by default, optional).
@return #string message with additional traceback informations.

")
     ("debug.upvalueid" . " Returns an unique identifier (as a light userdata) for the upvalue `n`
 from the given function.

These unique identifiers allow a program to check whether different closures
share upvalues. Lua closures that share an upvalue (that is, that access a same
external local variable) will return identical ids for those upvalue indices.
@function [parent=#debug] upvalueid
@param f function which owns the upvalue.
@param #number n index of the upvalue.
@return #userdata id of the upvalue

")
     ("table.concat" . "Given a list where all elements are strings or numbers, returns the string
`list[i]..sep..list[i+1] ... sep..list[j]`. The default value for `sep` is
the empty string, the default for `i` is `1`, and the default for `j` is `#list`.
If `i` is greater than `j`, returns the empty string.
@function [parent=#table] concat
@param #table list list to handle.
@param #string sep the separator (optional, empty string by default).
@param #number i start index (optional, 1 by default).
@param #number j end index (optional, `list` length by default)
@return #string the concatenated list.
@return #string empty string if `i` is greater than `j`
")
     ("table.insert" . "Inserts element `value` at position `pos` in `list`, shifting up
other elements to open space, if necessary. The default value for `pos` is
`n+1`, where `n` is the length of the list, so that a call
`table.insert(t,x)` inserts `x` at the end of list `t`.
@function [parent=#table] insert
@param #table list list to modify.
@param #number pos index of insertion (optional, insert at the end of the table by default)
@param value value to insert.
")
     ("table.pack" . "Returns a new table with all parameters stored into keys 1, 2, etc. and with
a field `n` with the total number of parameters. Note that the resulting
table may not be a sequence.
@function [parent=#table] pack
@param ... items to pack
@return #table the created table from given parameters
")
     ("table.remove" . "Removes from list the element at position `pos`, shifting down the elements
`list[pos+1], list[pos+2], ..., list[#list]` and erasing element `list[#list]`.
Returns the value of the removed element. The default value for pos is `#list`,
so that a call `table.remove(t)` removes the last element of list `t`.
@function [parent=#table] remove
@param #table list list to modify.
@param #number pos index of deletion (optional, length of the list by default)
")
     ("table.sort" . "Sorts list elements in a given order, in-place, from `list[1]` to `list[#list]`.
If `comp` is given, then it must be a function that receives two list elements
and returns true when the first element must come before the second in the
final order (so that `not comp(list[i+1],list[i]`) will be true after the sort).
If `comp` is not given, then the standard Lua operator < is used instead.

The sort algorithm is not stable; that is, elements considered equal by the
given order may have their relative positions changed by the sort.
@function [parent=#table] sort
@param #table list list to sort.
@param comp a function which take two lists and returns true when the first is less than the second (optional).
")
     ("table.unpack" . " Returns the elements from the given table. This function is equivalent to

    return list[i], list[i+1], ..., list[j]

By default, `i` is 1 and `j` is `#list`.
@function [parent=#table] unpack
@param #table list list to unpack
@param #number i start index (optional, 1 by default).
@param #number j end index (optional, length of the list by default).
@return Return each table elements as separated values
")
     ("global.assert" . "Issues an error when the value of its argument `v` is false (i.e.,
**nil** or **false**); otherwise, returns all its arguments. `message` is an error
message; when absent, it defaults to *\"assertion failed!\"*.
@function [parent=#global] assert
@param v if this argument is false an error is issued.
@param #string message an error message (optional, *\"assertion failed\"* by default)
@return All its arguments.
")
     ("global.collectgarbage" . "This function is a generic interface to the garbage collector. It performs
different functions according to its first argument, `opt`:

* _\"collect\"_: performs a full garbage-collection cycle. This is the default option.
* _\"stop\"_: stops automatic execution of the garbage collector. The collector will
run only when explicitly invoked, until a call to restart it.
* _\"restart\"_: restarts automatic execution of the garbage collector.
* _\"count\"_: returns the total memory in use by Lua (in Kbytes) and a second
value with the total memory in bytes modulo `1024`. The first value has a fractional
part, so the following equality is always true:
(The second result is useful when Lua is compiled with a non floating-point type for numbers.)

        k, b = collectgarbage(\"count\")
        assert(k*1024 == math.floor(k)*1024 + b)

* _\"step\"_: performs a garbage-collection step. The step \"size\" is controlled by
arg (larger values mean more steps) in a non-specified way. If you want to control
the step size you must experimentally tune the value of arg. Returns **true** if
the step finished a collection cycle.
* _\"setpause\"_: sets `arg` as the new value for the pause of the collector.
Returns the previous value for pause.
* _\"setstepmul\"_: sets `arg` as the new value for the step multiplier of the collector.
Returns the previous value for step.
* _\"isrunning\"_: returns a boolean that tells whether the collector is running
(i.e., not stopped).
* _\"generational\"_: changes the collector to generational mode. This is an experimental feature.
* _\"incremental\"_: changes the collector to incremental mode. This is the default mode.
@function [parent=#global] collectgarbage
@param #string opt the command to send (optional, \"collect\" by default)
@param arg the argument of the command (optional).
")
     ("global.dofile" . "Opens the named file and executes its contents as a Lua chunk. When
called without arguments,
`dofile` executes the contents of the standard input (`stdin`). Returns
all values returned by the chunk. In case of errors, `dofile` propagates
the error to its caller (that is, `dofile` does not run in protected mode).
@function [parent=#global] dofile
@param #string filename the path to the file. (optional)
@return values returned by the chunk
")
     ("global.error" . "Terminates the last protected function called and returns `message`
as the error message. Function `error` never returns.

Usually, `error` adds some information about the error position at the
beginning of the message. The `level` argument specifies how to get the
error position.
With level 1 (the default), the error position is where the
`error` function was called.
Level 2 points the error to where the function
that called `error` was called; and so on.
Passing a level 0 avoids the addition of error position information to the message.
@function [parent=#global] error
@param #string message an error message.
@param #number level specifies how to get the error position (optional, `1` by default).
")
     ("global.getmetatable" . "If `object` does not have a metatable, returns **nil**. Otherwise, if the
object's metatable has a `\"__metatable\"` field, returns the associated
value. Otherwise, returns the metatable of the given object.
@function [parent=#global] getmetatable
@param object
@return #table the metatable of object.
@return #nil if no metatable was found
")
     ("global.ipairs" . "If t has a metamethod __ipairs, calls it with t as argument and returns the
first three results from the call.
Otherwise, returns three values: an iterator function, the table `t`, and `0`,
so that the construction

     for i,v in ipairs(t) do body end

will iterate over the pairs `(1,t[1]), (2,t[2]), ...,` up to the first integer
key absent from the table.
@function [parent=#global] ipairs
@param #table t a table by index.
@return iterator function, table `t`, the value `0`
")
     ("global.load" . "Loads a chunk.
If `ld` is a string, the chunk is this string. If `ld` is a function, load calls
it repeatedly to get the chunk pieces. Each call to `ld` must return a string
that concatenates with previous results. A return of an empty string, **nil**, or
no value signals the end of the chunk.

If there are no syntactic errors, returns the compiled chunk as a function;
otherwise, returns **nil** plus the error message.

If the resulting function has upvalues, the first upvalue is set to the value
of `env`, if that parameter is given, or to the value of the global environment.
(When you load a main chunk, the resulting function will always have exactly one
upvalue, the `_ENV` variable. When you load a binary chunk created from
a function (see `string.dump`), the resulting function can have arbitrary upvalues.)

`source` is used as the source of the chunk for error messages and debug information.
When absent, it defaults to `ld`, if `ld` is a string, or to `\"=(load)\"` otherwise.

The string mode controls whether the chunk can be text or binary
(that is, a precompiled chunk). It may be the string `\"b\"` (only binary chunks),
`\"t\"` (only text chunks), or `\"bt\"` (both binary and text). The default is `\"bt\"`.
@function [parent=#global] load
@param ld string or function representing the chunk.
@param #string source used as source code (optional, by default `ld` if `ld` is a
string, or to `\"=(load)\"`otherwise.
@param #string mode `\"b\"` for only binary chunk, `\"t\"` for only text chunks,
`bt` for both binary and text (optional, \"bt\" by default).
@param env environment where to set the first upvalue if any.
@return compiled chunk as a function
@return #nil, #string error message
")
     ("global.loadfile" . "Similar to `load`, but gets the chunk from file `filename` or from the
standard input, if no file name is given.
@function [parent=#global] loadfile
@param #string filename the path to the file. (optional)
@param #string mode `\"b\"` for only binary chunk, `\"t\"` for only text chunks,
`bt` for both binary and text (optional, \"bt\" by default).
@param env environment where to set the first upvalue if any.
@return compiled chunk as a function
@return #nil, #string error message
")
     ("global.next" . "Allows a program to traverse all fields of a table. Its first argument is
a table and its second argument is an index in this table. `next` returns
the next index of the table and its associated value.

When called with **nil**
as its second argument, `next` returns an initial index and its associated
value. When called with the last index, or with nil in an empty table, `next`
returns nil.

If the second argument is absent, then it is interpreted as
nil. In particular, you can use `next(t)` to check whether a table is empty.
The order in which the indices are enumerated is not specified, *even for
numeric indices*. (To traverse a table in numeric order, use a numerical **for**.)

The behavior of `next` is undefined if, during the traversal, you assign
any value to a non-existent field in the table. You may however modify
existing fields. In particular, you may clear existing fields.
@function [parent=#global] next
@param #table table table to traverse.
@param index initial index (optional).
@return index, value
@return #nil if called on the last index or on an empty table
")
     ("global.pairs" . "If t has a metamethod `__pairs`, calls it with t as argument and returns the
first three results from the call.

Otherwise, returns three values: the `next` function, the table t, and nil,
so that the construction

     for k,v in pairs(t) do body end

will iterate over all key–value pairs of table `t`.
See function next for the caveats of modifying the table during its traversal.
@function [parent=#global] pairs
@param #table t table to traverse.
@return iterator function, table `t`, the value `0`
")
     ("global.pcall" . "Calls function `f` with the given arguments in *protected mode*. This
means that any error inside `f` is not propagated; instead, `pcall` catches
the error and returns a status code. Its first result is the status code (a
boolean), which is true if the call succeeds without errors. In such case,
`pcall` also returns all results from the call, after this first result. In
case of any error, `pcall` returns **false** plus the error message.
@function [parent=#global] pcall
@param f function to be call in *protected mode*.
@param ... function arguments.
@return #boolean true plus the result of `f` function if its call succeeds without errors.
@return #boolean,#string false plus the error message in case of any error.
")
     ("global.print" . "Receives any number of arguments and prints their values to `stdout`, using the
`tostring` function to convert each argument to a string. print is not intended
for formatted output, but only as a quick way to show a value, for instance for
debugging. For complete control over the output, use `string.format` and `io.write`.
@function [parent=#global] print
@param ... values to print to `stdout`.
")
     ("global.rawequal" . "Checks whether `v1` is equal to `v2`, without invoking any
metamethod. Returns a boolean.
@function [parent=#global] rawequal
@param v1 first operand
@param v2 second operand
@return #boolean true if `v1` is equal to `v2`.
")
     ("global.rawget" . "Gets the real value of `table[index]`, without invoking any
metamethod. `table` must be a table; `index` may be any value.
@function [parent=#global] rawget
@param #table table table to looking for
@param index index in the table
@return The real value of `table[index]`, without invoking any
metamethod.
")
     ("global.rawlen" . "Returns the length of the object `v`, which must be a table or a string, without
invoking any metamethod. Returns an integer number.
@function [parent=#global] rawlen
@param v table or a string
@return #number length of `v`
")
     ("global.rawset" . "Sets the real value of `table[index]` to `value`, without invoking any
metamethod. `table` must be a table, `index` any value different from nil,
and `value` any Lua value.
This function returns `table`.
@function [parent=#global] rawset
@param #table table
@param index any value different from nil.
@param value any Lua value.
@return #table the given table
")
     ("global.select" . "If `index` is a number, returns all arguments after argument number
`index`. Otherwise, `index` must be the string `\"#\"`, and `select` returns
the total number of extra arguments it received.
@function [parent=#global] select
@param index a number or the string `\"#\"`
@param ...
@return all arguments after argument number `index`
@return total number of extra arguments
")
     ("global.setmetatable" . "Sets the metatable for the given table. (You cannot change the metatable
of other types from Lua, only from C.) If `metatable` is nil, removes the
metatable of the given table. If the original metatable has a `\"__metatable\"`
field, raises an error.
This function returns `table`.
@function [parent=#global] setmetatable
@param #table table
@param #table metatable
@return #table The first argument `table`.
")
     ("global.tonumber" . "When called with no base, tonumber tries to convert its argument to a number.
If the argument is already a number or a string convertible to a number,
then tonumber returns this number; otherwise, it returns **nil**.

When called with base, then e should be a string to be interpreted as an
integer numeral in that base. The base may be any integer between `2` and `36`,
inclusive. In bases above `10`, the letter 'A' (in either upper or lower case)
represents `10`, 'B' represents `11`, and so forth, with 'Z' representing `35`.
If the string `e` is not a valid numeral in the given base,
the function returns **nil**.
@function [parent=#global] tonumber
@param e a number or string to convert to a number.
@param #number base the base to interpret the numeral, any integer between `2` and `36` (optional, `10` by default).
@return #number converted number
@return #nil if convertion fail.
")
     ("global.tostring" . "Receives an argument of any type and converts it to a string in a
reasonable format. (For complete control of how numbers are converted, use
`string.format`.)

If the metatable of `v` has a `\"__tostring\"` field, then `tostring` calls
the corresponding value with `v` as argument, and uses the result of the
call as its result.
@function [parent=#global] tostring
@param v an argument of any type.
@return #string a string in a reasonable format.
")
     ("global.type" . "Returns the type of its only argument, coded as a string. The possible
results of this function are \"
`nil`\" (a string, not the value **nil**), \"`number`\", \"`string`\", \"`boolean`\",
\"`table`\", \"`function`\", \"`thread`\", and \"`userdata`\".
@function [parent=#global] type
@param v any value.
@return #string the type of `v`.
")
     ("coroutine.create" . "Creates a new coroutine, with body f. f must be a Lua function. Returns this new coroutine,
an object with type \"thread\".
@function [parent=#coroutine] create
@param f a function used as coroutine body.
@return #thread a new coroutine.
")
     ("coroutine.resume" . "Starts or continues the execution of coroutine `co`. The first time
you resume a coroutine, it starts running its body. The values `val1`,
... are passed as the arguments to the body function. If the coroutine
has yielded, `resume` restarts it; the values `val1`, ... are passed
as the results from the yield.

If the coroutine runs without any errors, `resume` returns true plus any
values passed to `yield` (if the coroutine yields) or any values returned
by the body function (if the coroutine terminates). If there is any error,
`resume` returns `false` plus the error message.
@function [parent=#coroutine] resume
@param #thread co coroutine to start or resume.
@param ... arguments passed to the body function or as result of yield call.
@return #boolean true plus any values passed to `yield` (if the coroutine yields) or any values returned
by the body function (if the coroutine terminates)
@return #boolean false plus an error message.
")
     ("coroutine.running" . "Returns the running coroutine plus a boolean, true when the running coroutine is the main one.
@function [parent=#coroutine] running
@return #thread the running coroutine, or nil when called by the main thread.
")
     ("coroutine.status" . "Returns the status of coroutine `co`. as a string. `\"running\"`, if
the coroutine is running (that is, it called `status`); `\"suspended\"`, if
the coroutine is suspended in a call to `yield`, or if it has not started
running yet; `\"normal\"` if the coroutine is active but not running (that
is, it has resumed another coroutine); and `\"dead\"` if the coroutine has
finished its body function, or if it has stopped with an error.
@function [parent=#coroutine] status
@param #thread co a coroutine
@return #string the status : `\"running\"`, `\"suspended\"`, `\"normal\"` or `\"dead\"`.
")
     ("coroutine.wrap" . "Creates a new coroutine, with body `f`. `f` must be a Lua
function. Returns a function that resumes the coroutine each time it is
called. Any arguments passed to the function behave as the extra arguments to
`resume`. Returns the same values returned by `resume`, except the first
boolean. In case of error, propagates the error.
@function [parent=#coroutine] wrap
@param f a function used as coroutine body.
@param ... arguments passed to the body function or as result of yield call.
@return Any values passed to `yield` (if the coroutine yields) or any values returned
by the body function (if the coroutine terminates).
")
     ("debug.setfenv" . "debug.setfenv (object, table)")
     ("debug.getfenv" . "debug.getfenv (o)")
     ("os.tmpname" . "os.tmpname ()")
     ("math.tanh" . "math.tanh (x)")
     ("math.log10" . "math.log10 (x)")
     ("table.maxn" . "table.maxn (table)")
     ("string.upper" . "string.upper (s)")
     ("package.seeall" . "package.seeall (module)")
     ("require" . "require (modname)")
     ("module" . "module (name [, ...])")
     ("coroutine.yield" . "coroutine.yield (...)")
     ("xpcall" . "xpcall (f, err)")
     ("unpack" . "unpack (list [, i [, j]])")
     ("type" . "type (v)")
     ("tostring" . "tostring (e)")
     ("tonumber" . "tonumber (e [, base])")
     ("setmetatable" . "setmetatable (table, metatable)")
     ("setfenv" . "setfenv (f, table)")
     ("select" . "select (index, ...)")
     ("rawset" . "rawset (table, index, value)")
     ("rawget" . "rawget (table, index)")
     ("rawequal" . "rawequal (v1, v2)")
     ("print" . "print (...)")
     ("pcall" . "pcall (f, arg1, ...)")
     ("pairs" . "pairs (t)")
     ("next" . "next (table [, index])")
     ("loadstring" . "loadstring (string [, chunkname])")
     ("loadfile" . "loadfile ([filename])")
     ("load" . "load (func [, chunkname])")
     ("ipairs" . "ipairs (t)")
     ("getmetatable" . "getmetatable (object)")
     ("getfenv" . "getfenv ([f])")
     ("error" . "error (message [, level])")
     ("dofile" . "dofile ([filename])")
     ("collectgarbage" . "collectgarbage ([opt [, arg]])")
     ("assert" . "assert (v [, message])")
     ))



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

(defun auto-complete-lua--calculate-beginning-of-word ()
  (let ((word-start-regexp "\\<"))      ; The regexp that detects the
                                        ; beginning of a word

    ;; The `lua-mode' syntax table considers the "." character as a
    ;; puntuation character. This causes `word-start-regexp' to assume
    ;; "format" is the start of a word, however, to correctly perform
    ;; the auto-complete it should think that the word starts at
    ;; "string.format" instead. As a workaround this buffer's
    ;; syntax-table is temporarily modified so it considers "." as
    ;; belonging to word. This makes `word-start-regexp' return think
    ;; that the word starts at "string.format" instead of "format".
    (with-syntax-table (copy-syntax-table)
      (modify-syntax-entry ?. "w")
      ;; Move point to the start of the current word, unless point is
      ;; already there because then moving the point would mean that
      ;; auto-complete would try to auto complete the word *behind*
      ;; the word where the pointer is.
      (unless (looking-at word-start-regexp)
        (backward-word))
      ;; Point is now at the first character of word
      (point))))

(defvar ac-source-lua
  '((candidates . auto-complete-lua--get-lua-builtin-functions)
     (prefix . auto-complete-lua--calculate-beginning-of-word)
     (document . auto-complete-lua--get-documentation)
     (cache))
  "A auto-complete.el source for the builtin lua functions.")


(provide 'auto-complete-lua)
;;; auto-complete-lua.el ends here
