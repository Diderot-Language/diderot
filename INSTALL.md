# Build instructions for the Diderot compiler

## PREREQUISITES

You must have TEEM installed, which you can get from

>
>  <http://teem.sourceforge.net/download/index.html>
>

You also must have SML/NJ (version 110.80 or later) installed, which you can
get from

>
>  <http://smlnj.org/dist/working/index.html>
>

Diderot is currently supported on Linux and Mac OS X.  It requires a system that is
capable of running 64-bit executables.  Note that SML/NJ is a 32-bit program, so
on Linux systems you must also have the 32-bit compatibility libraries installed
(the exact libraries/packages required will depend on your distribution).

## SOURCE CHECKOUT

You can checkout the stable version of the Diderot source tree from
<github.com> with the command:

````bash
  % git clone https://github.com/Diderot-Language/diderot.git
````

or you can browse the source code

>
>  <https://github.com/Diderot-Language/diderot>
>

## CONFIGURATION

Run autoheader:

````Bash
  % autoheader -Iconfig
````

Run autoconf:

````bash
  % autoconf -Iconfig
````

Configure the makefiles etc.

````bash
  % ./configure --with-teem=/path/to/teem
````

where `/path/to/teem` is the full path of the directory
containing the "lib" and "include" directories containing
`libteem.{a,so,dylib}` and `teem/*.h`, respectively.
Note that if you have teem installed in the default location
(`/usr/local/teem`), then you do not need to specify its location.

## INSTALLATION

From the root of the Diderot tree, run

````bash
  % make local-install
````

This command will build the Diderot compiler (`diderotc`) and
runtime support.  Assuming that `$ROOT` is the root of the Diderot tree,
the compiler will be located at `$ROOT/bin/diderotc`.
