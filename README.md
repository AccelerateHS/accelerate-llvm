<div align="center">
<img width="450" src="https://github.com/AccelerateHS/accelerate/raw/master/images/accelerate-logo-text-v.png?raw=true" alt="henlo, my name is Theia"/>

# LLVM backends for the Accelerate array language

[![CI](https://github.com/AccelerateHS/accelerate-llvm/actions/workflows/ci.yml/badge.svg)](https://github.com/AccelerateHS/accelerate-llvm/actions/workflows/ci.yml)
[![Gitter](https://img.shields.io/gitter/room/nwjs/nw.js.svg)](https://gitter.im/AccelerateHS/Lobby)
[![Hackage](https://img.shields.io/hackage/v/accelerate-llvm.svg)](https://hackage.haskell.org/package/accelerate-llvm)

</div>

This package compiles Accelerate code to LLVM IR, and executes that code on
multicore CPUs as well as NVIDIA GPUs. This avoids the need to go through
`nvcc` or write C++ code. For details on Accelerate, refer to the [main
repository][GitHub].

We love all kinds of contributions, so feel free to open issues for missing
features as well as report (or fix!) bugs on the [issue tracker][Issues].

  [GitHub]:  https://github.com/AccelerateHS/accelerate
  [Issues]:  https://github.com/AccelerateHS/accelerate/issues


 * [Dependencies](#dependencies)
   * [macOS](#macos)
   * [Debian/Ubuntu](#debianubuntu)
   * [Arch Linux](#archlinux)
   * [Windows](#windows)


Dependencies
------------

Haskell dependencies are available from Hackage, but there are several external
library dependencies that you will need to install as well:

- if using `accelerate-llvm-native` for multicore CPU:
  [`libFFI`](http://sourceware.org/libffi/)
- if using `accelerate-llvm-ptx` for GPU:
  [`CUDA`](https://developer.nvidia.com/cuda-downloads);
  [Note that not all versions of CUDA support all NVIDIA GPUs](https://en.wikipedia.org/wiki/CUDA#GPUs_supported)
- [`clang`](https://clang.llvm.org/) (if using `accelerate-llvm-ptx`: version
  16 or higher, built with support for the `nvptx` backend). `accelerate-llvm`
  uses the command-line tool as a way to be compatible with many different LLVM
  versions, not to compile C code. (Accelerate passes LLVM IR to `clang`.)

Below are some OS-specific instructions. If anything here is wrong or out of
date, please file an issue.

## macOS

To get `libFFI`, run `brew install libffi`. `clang` is already provided with
macOS (you may need to `xcode-select --install`), and CUDA is not supported on
macOS.

## Debian/Ubuntu

For `clang`:
- On Ubuntu 24.04 (noble) / Debian trixie or higher: `sudo apt install clang`.
- Otherwise, if you need only the CPU backend (`accelerate-llvm-native`):
  `sudo apt install clang` will give you an old version of `clang`, but the CPU
   backend is likely to work fine.
- If you are on an older distro and need the GPU backend
  (`accelerate-llvm-ptx`): `clang` version 16 or higher is required.
  Add the apt source from [apt.llvm.org](https://apt.llvm.org/). The neatest
  way to do this is to create a file `/etc/apt/sources.list.d/llvm.list` (the
  precise file name does not matter) and put in it, for Ubuntu (change "jammy"
  as appropriate):

      deb http://apt.llvm.org/jammy/ llvm-toolchain-jammy main
      deb-src http://apt.llvm.org/jammy/ llvm-toolchain-jammy main

  or for Debian (change "bookworm" as appropriate):

      deb http://apt.llvm.org/bookworm/ llvm-toolchain-bookworm main
      deb-src http://apt.llvm.org/bookworm/ llvm-toolchain-bookworm main

  and `sudo apt update; sudo apt install clang`. This gets you the latest
  version of `clang`; different sources are also available for specific
  versions (see [apt.llvm.org](https://apt.llvm.org)).

To use the CPU backend (`accelerate-llvm-native`), install `libFFI` using
`sudo apt install libffi-dev`.

To use the GPU backend (`accelerate-llvm-ptx`), install CUDA from
[here](https://developer.nvidia.com/cuda-downloads?target_os=Linux)
("deb (network)" is smoother than the "deb (local)" option).

## Arch Linux

Run `sudo pacman -S clang`. To use the CPU backend (`accelerate-llvm-native`),
additionally run `sudo pacman -S libffi`. To use the GPU backend
(`accelerate-llvm-ptx`), additionally run `sudo pacman -S cuda`.

## Windows

We recommend WSL2 (not WSL1, WSL2!) and following the Ubuntu instructions
above. The remainder of this text attemps to give you a working system on
Windows native.

Install `clang`; you have two options:
1. Using
   [WinGet](https://learn.microsoft.com/en-us/windows/package-manager/winget/):
   `winget install LLVM.LLVM`
2. By downloading the installer directly (WinGet just runs the same installer)
   from [here](https://github.com/llvm/llvm-project/releases) (choose
   "LLVM-<version>-win64.exe" from the latest release; you may need to click
   "Show all 57 assets").
This will also give you `libFFI`.

<details><summary>Optionally, add <code>clang</code> (and more) to your system path. Click to see how.</summary>

Accelerate should be able to find `clang` automatically even if you do not do
this. However, for easy access to `clang` and all other LLVM executables, add
`C:\Program Files\LLVM\bin` to the system path as follows:
1. Search for "environment variables" in the start menu
2. Click "Edit the system environment variables"
3. Click on "Environment Variables..."
4. Double-click on the user variable called "Path"
5. And add a new entry containing `C:\Program Files\LLVM\bin`.

Note that if you add an entry here manually, it is a good idea to clean it up
again if you uninstall LLVM/clang. (Leaving it there is not very harmful,
however.)

You may find that the LLVM/clang installer has already added the Path entry
automatically (it did not for us); if so, no need to add a second entry.

&mdash;&mdash;
</details>

You may additionally need the VS Build Tools, if you have not yet installed and
set up Visual Studio otherwise. You need this if `clang` complains that it is
`unable to find a Visual Studio installation; try running Clang from a developer command prompt`.

1. If you already have the Visual Studio Installer on your system, open it and
   check if you already have Visual Studio (Community) installed. Note that
   this is completely unrelated to VS _Code_.
   - If you already have VS (Community): inside the Visual Studio Installer,
     click on "Modify" in the VS (Community) box. This should get you a screen
     with "workloads" you can select.
   - If you do not yet have VS (Community), install the VS Build Tools: go to
     https://visualstudio.microsoft.com/downloads, scroll down to "All
     Downloads", open "Tools for Visual Studio", and select "Build Tools for
     Visual Studio". If you run the installer, you should get a screen with
     "workloads" you can select.
2. Under the Workloads tab, choose the "Desktop development with C++" workload.
   If you want to save a bit of disk space (not much), keep only the following
   two options selected:
   - "MSVC v143 - VE 2022 C++ x64/x86 build tools (Latest)"
   - "Windows 11 SDK (…)" (choose the latest option). The attentive reader may
     note that the wizard also offers Clang; we recommend a separate Clang
     install for Accelerate because the one from VS somehow doesn’t seem to
     work properly with Accelerate. If you find out why, please let us know.
3. Install that. This takes a while.

It turns out that having both Visual Studio and the Build Tools installed
results in Clang getting confused between the two (it appears that Visual
Studio is 64-bit (x64) and the Build Tools are 32-bit (x86)). If Clang
complains about the bit-ness of your system libraries, double-check that you
haven’t installed both simultaneously.

The GPU backend (`accelerate-llvm-ptx`) probably doesn't work on Windows; in
any case, it is untested.
