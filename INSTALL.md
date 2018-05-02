# Build instructions for the Diderot compiler

These instructions are organized into two parts.  First is a quick
guide to installing the Diderot system (including the prerequisite
software).  Following the quick overview are detailed instructions
for acquiring and installing the prerequisite software.

## QUICK GUIDE

### Prerequisite software

#### Software tools

To build *Teem*, you will need to have *cmake* installed.  Instructions
for getting *cmake* are [below](#cmake-details).

The Diderot runtime system is written in C++11 and the code generator
also produces C++ code, so you will need to have a modern C++ compiler
installed (*e.g.*, either version 4.8.1 or later of **g++**, or
version 3.3 or later of **clang++**).

#### Teem

You must have **Teem** installed, which you can get from

>
>  <http://teem.sourceforge.net/download/index.html>
>

Make sure that your **Teem** installation is revision **r6294** or later.
More details about installing **Teem** can be found [below](#teem-details).

#### Standard ML of New Jersey

You also must have **Standard ML of New Jersey** (SML/NJ) installed,
which you can get from

>
>  <http://smlnj.org/dist/working/index.html>
>

Make sure that you get version 110.82 or later.  More details about
installing **SML/NJ** can be found [below](#smlnj-details).

### Getting the Diderot source

You can checkout the stable version of the Diderot source tree from
<github.com> with the command:

````bash
  % git clone https://github.com/Diderot-Language/diderot.git
````

or you can browse the source code

>
>  <https://github.com/Diderot-Language/diderot>
>

### Configuration

Once you have downloaded the **Diderot** source code, go to the root of
the source tree and configure the build for your machine.

````bash
    ./configure
````

The `configure` script will try to find your **Teem** installation and
the **sml** command.  You may need to specify these locations as follows.

The `configuration` script looks for **Teem** in either `/usr/local` or
`/usr/local/teem`.  If **Teem** is installed elsewhere, use the command

````bash
    ./configure --with-teem=/path/to/teem
````

where `/path/to/teem` is the full path of the directory containing the
**Teem** `lib` and `include` directories.

If the **sml** command is in your `PATH`, then you are set.  Otherwise,
you can specify its location to `configure` using the command

````bash
    SMLNJ_CMD=/usr/local/smlnj/bin/sml ./configure
````

assuming that `/usr/local/smlnj` is the installation directory for
**SML/NJ**.

### Building and installing the Diderot system

From the root of the Diderot source tree, run the command

````bash
    make local-install
````

This command will build the Diderot compiler (`diderotc`) and runtime
support.  Assuming that `$ROOT` is the root of the Diderot tree, the
compiler will be located at `$ROOT/bin/diderotc`.

## DETAILS

### CMake <a name="cmake-details"></a>

[CMake](https://cmake.org) is required to build **Teem**.
These utilities can be obtained via `apt-get` on Ubuntu/Debian
Linux, or via [Homebrew `brew`](http://brew.sh) or
[Mac Ports `port`](https://www.macports.org) on **macOS**.

To get CMake:
* **Linux**: CMake can be installed for Ubuntu/Debian using
  the command `sudo apt-get install cmake`.
* **macOS**: CMake can be installed from [Homebrew](http://brew.sh)
  using the command `brew install cmake` or from
  [Mac Ports](https://www.macports.org) using the command
  `sudo port install cmake`.
* Alternatively, the [CMake download](https://cmake.org/download/)
  page includes "Binary distributions" that have the executable
  `cmake` you will need, as well as source-code distributions.

### Teem <a name="teem-details"></a>

The Diderot run-time depends on [**Teem**](http://teem.sourceforge.net).
**Teem** is overdue for a release, but in the mean time you should
build it from source with **CMake**, because **Diderot** requires
the current source (revision **r6294** or later).

It is best to build a **Teem** for Diderot that has *none* of the
optional libraries (PNG, zlib, etc) enabled. Experience has shown
that additional library dependencies from **Teem** will complicate
the linking that the Diderot compiler must do to create executables.

Create a directory for **Teem**; assume that `$TEEMDIR` specifies the path to
that directory.  Then run the following shell commands:

````bash
    cd $TEEMDIR
    svn co https://svn.code.sf.net/p/teem/code/teem/trunk teem-src
````

The next step is to create a build directory and to configure the build
using **CMake**.  This step requires specifying a bunch of flags to
disable dependency on various libraries.

**COMMENT: for macOS, I do not think that turing off shared libraries
is required.  Also, it is not clear that one need to disable PNG, etc.
on macOS (at least when using shared libraries). -- JHR**

````bash
    mkdir teem-build
    cd teem-build
    TEEMBUILD=`pwd`
    cmake -Wno-dev \
      -D BUILD_EXPERIMENTAL_APPS=OFF -D BUILD_EXPERIMENTAL_LIBS=OFF \
      -D BUILD_SHARED_LIBS=OFF -D BUILD_TESTING=OFF \
      -D CMAKE_BUILD_TYPE=Release \
      -D Teem_BZIP2=OFF -D Teem_FFTW3=OFF -D Teem_LEVMAR=OFF -D Teem_PTHREAD=OFF \
      -D Teem_PNG=OFF -D Teem_ZLIB=OFF \
      -D CMAKE_INSTALL_PREFIX:PATH=$TEEMDIR \
      ../teem-src
````

Lastly, you can build and install **Teem**.

````bash
    make
    make install
````

At this point you will have a `$TEEMDIR/bin`, `$TEEMDIR/lib`, and
`$TEEMDIR/include` directories that contain the various parts of
**Teem**.

To make sure your build works, try:

````bash
    $TEEMDIR/bin/unu --version
````

Note that we do **not** recommend adding this `teem-ddro/bin` to your path;
it's not very useful.

### SML/NJ <a name="smlnj-details"></a>

#### Installing on Linux

On Ubuntu or Debian Linux, `apt-get` may work to install a sufficiently recent
version.  `apt-cache policy smlnj` reports what version you can get;
if that's at or above version 110.82, you can:

````bash
    sudo apt-get install smlnj
    sudo apt-get install ml-lpt
````

The second `apt-get` to get `ml-lpt` is required because without it, the later compilation
of the Diderot compiler (with the `sml` from `apt-get`) will stop with an error message
like `driver/sources.cm:16.3-16.18 Error: anchor $ml-lpt-lib.cm not defined`.

#### Installing on macOS

On **macOS** systems, the easiest way to install **SML/NJ** is by
downloading the signed installer package from <smlnj.org>.  Running the
installer will place the system in `/usr/local/smlnj` and the path to the
**sml** command will be `/usr/local/smlnj/bin/sml`.

As an alternative you can install SML/NJ from [Homebrew](https://brew.sh)
using the following commands:

It is also possible to get SML/NJ from [Homebrew](https://brew.sh) for macOS.
Assuming that `brew info smlnj` mentions version 110.82 or higher, then

````bash
    brew install smlnj
````

(possibly followed by `brew link smlnj`) should work.

#### Installing from source ####

It is also possible to install **SML/NJ** using the installation script
provided as part of the source downloads.

First, create a directory to hold the downloaded files and the results
of the installation; assume that `$SMLDIR` denotes the path to that
directory.  Then run the following command to download and unbundle
the `config` directory:

```` bash
    curl -O http://smlnj.org/dist/working/110.82/config.tgz
    tar -xzf config.tgz
````

Then you can build the system using the command

```` bash
    config/install.sh
````

This command will download the necessary source and precompiled files from
<smlnj.org> and build the **SML/NJ** runtime, compiler, and libraries.
The path to the **sml** command will be `$SMLDIR/bin/sml`.

Note that SML/NJ is a 32-bit program, so on Linux systems you must also
have the 32-bit compatibility libraries installed (the exact libraries/packages
required will depend on your distribution; details are available in the
SML/NJ [INSTALL](http://www.smlnj.org/dist/working/110.82/INSTALL)
notes).
