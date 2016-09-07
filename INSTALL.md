# Build instructions for the Diderot compiler

## PREREQUISITES

### TEEM

You must have TEEM installed, which you can get from

>
>  <http://teem.sourceforge.net/download/index.html>
>

Make sure that your TEEM installation is revision **r6294** or later.

### Standard ML

You also must have SML/NJ (version 110.80 or later) installed, which you can
get from

>
>  <http://smlnj.org/dist/working/index.html>
>

Diderot is currently supported on Linux and Mac OS X.  It requires a system that is
capable of running 64-bit executables.  Note that SML/NJ is a 32-bit program, so
on Linux systems you must also have the 32-bit compatibility libraries installed
(the exact libraries/packages required will depend on your distribution; details
are available in the SML/NJ [INSTALL](http://www.smlnj.org/dist/working/110.80/INSTALL)
notes).

### C++

The Diderot runtime system is written in C++11 and the code generator also produces
C++ code, so you will need to have a modern C++ compiler installed.

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
  % ./configure
````

The `configure` script will look for Standard ML in your path.  If necessary,
you can specify its location to `configure` using the command

````bash
  % SMLNJ_CMD=/usr/local/smlnj/bin/sml ./configure
````

assuming that `/usr/local/smlnj` is the installation directory for SML/NJ.

The `configuration` script looks for TEEM in either `/usr/local` or
`/usr/local/teem`.  If TEEM is installed elsewhere, use the command

````bash
  % ./configure --with-teem=/path/to/teem
````

where `/path/to/teem` is the full path of the directory
containing the TEEM `lib` and `include` directories.

## INSTALLATION

From the root of the Diderot tree, run

````bash
  % make local-install
````

This command will build the Diderot compiler (`diderotc`) and
runtime support.  Assuming that `$ROOT` is the root of the Diderot tree,
the compiler will be located at `$ROOT/bin/diderotc`.
