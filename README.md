# arch-hs

[![GitHub CI](https://github.com/berberman/arch-hs/workflows/CI/badge.svg)](https://github.com/berberman/arch-hs/actions)
[![Build Status](https://travis-ci.com/berberman/arch-hs.svg?branch=master)](https://travis-ci.com/berberman/arch-hs)
[![Hackage](https://img.shields.io/hackage/v/arch-hs.svg?logo=haskell)](https://hackage.haskell.org/package/arch-hs)
[![MIT license](https://img.shields.io/badge/license-MIT-blue.svg)](LICENSE)

A program generating PKGBUILD for hackage packages. Special thanks to [felixonmars](https://github.com/felixonmars/).

**Notice that `arch-hs` will always support only the latest GHC version.**


## Introduction

Given the name of a package in hackage, `arch-hs` can generate its corresponding PKGBUILD draft.
It has a naive built-in dependency solver, which can fetch all dependencies we need to produce a archlinux package. 
During the dependency calculation, all version constraints will be discarded due to the arch haskell packaging strategy,
and packages already exist in the [community](https://www.archlinux.org/packages/) will be excluded.

## Prerequisite

`arch-hs` is just a PKGBUILD text file generator, which is not integrated with `pacman`, depending on nothing than:

* Pacman database (`community.db`), ~~i.e., archlinux system.~~ the db file can be specified manually for now. 

* Hackage database tarball (`01-index.tar`, or `00-index.tar` previously), usually provided by `cabal-install`.

## Installation

### Install the latest release

```
# pacman -S arch-hs
```

`arch-hs` is available in [community](https://www.archlinux.org/packages/community/x86_64/arch-hs/), so you can install it using `pacman`.

### Install the development version

```
# pacman -S arch-hs-git
```

The `-git` version is available in [archlinxcn](https://github.com/archlinuxcn/repo), following the latest git commit.

### Build from source (for development)

```
$ git clone https://github.com/berberman/arch-hs
```

Then build and install it via stack or cabal.

#### Stack
```
$ stack build
```

#### Cabal (dynamic)
```
$ cabal configure --disable-library-vanilla --enable-shared --enable-executable-dynamic --ghc-options=-dynamic 
$ cabal build
```

## Usage

Just run `arch-hs` in command line with options and a target. Here is an example:
we will create the archlinux package of `dhall-lsp-server`:

```
$ arch-hs -o "/home/berberman/Desktop/test/" dhall-lsp-server

......

  ⓘ Recommended package order (from topological sort):
1. haskell-lsp-types
2. haskell-lsp
3. dhall-lsp-server

  ⓘ Detected flags from targets (their values will keep default unless you specify):
haskell-lsp
    ⚐ demo:
      description: Build the lsp-hello demo executable
      default: False
      isManual: False

  ⓘ Write file: /home/berberman/Desktop/test/dhall-lsp-server/PKGBUILD
  ⓘ Write file: /home/berberman/Desktop/test/haskell-lsp/PKGBUILD
  ⓘ Write file: /home/berberman/Desktop/test/haskell-lsp-types/PKGBUILD
  ✔ Success!

```

This message tells that in order to package `dhall-lsp-server`, we must package `haskell-lsp-types`
and `haskell-lsp` sequentially, because they don't present in archlinux community repo.

```
/home/berberman/Desktop/test
├── dhall-lsp-server
│   └── PKGBUILD
├── haskell-lsp
│   └── PKGBUILD
└── haskell-lsp-types
    └── PKGBUILD
```

`arch-hs` will generate PKGBUILD for each packages. Let's see what we have in `./haskell-lsp/PKGBUILD`:

``` bash
# This file was generated by arch-hs, please check it manually.
# Maintainer: Your Name <youremail@domain.com>

_hkgname=haskell-lsp
pkgname=haskell-lsp
pkgver=0.22.0.0
pkgrel=1
pkgdesc="Haskell library for the Microsoft Language Server Protocol"
url="https://github.com/alanz/haskell-lsp"
license=("custom:MIT")
arch=('x86_64')
depends=('ghc-libs' 'haskell-aeson' 'haskell-async' 'haskell-attoparsec' 'haskell-data-default' 'haskell-hashable' 'haskell-lsp-types' 'haskell-hslogger' 'haskell-lens' 'haskell-network-uri' 'haskell-rope-utf16-splay' 'haskell-sorted-list' 'haskell-temporary' 'haskell-unordered-containers' 'haskell-vector')
makedepends=('ghc' 'haskell-quickcheck' 'haskell-hspec' 'haskell-hspec-discover' 'haskell-quickcheck-instances')
source=("https://hackage.haskell.org/packages/archive/$_hkgname/$pkgver/$_hkgname-$pkgver.tar.gz")
sha256sums=('SKIP')

prepare(){
  cd $_hkgname-$pkgver
}

build() {
  cd $_hkgname-$pkgver    

  runhaskell Setup configure -O --enable-shared --enable-executable-dynamic --disable-library-vanilla \
    --prefix=/usr --docdir=/usr/share/doc/$pkgname --enable-tests \
    --dynlibdir=/usr/lib --libsubdir=\$compiler/site-local/\$pkgid \
    --ghc-option=-optl-Wl\,-z\,relro\,-z\,now \
    --ghc-option='-pie'

  runhaskell Setup build
  runhaskell Setup register --gen-script
  runhaskell Setup unregister --gen-script
  sed -i -r -e "s|ghc-pkg.*update[^ ]* |&'--force' |" register.sh
  sed -i -r -e "s|ghc-pkg.*unregister[^ ]* |&'--force' |" unregister.sh
}

check() {
  cd $_hkgname-$pkgver
  runhaskell Setup test
}

package() {
  cd $_hkgname-$pkgver

  install -D -m744 register.sh "$pkgdir"/usr/share/haskell/register/$pkgname.sh
  install -D -m744 unregister.sh "$pkgdir"/usr/share/haskell/unregister/$pkgname.sh
  runhaskell Setup copy --destdir="$pkgdir"
  install -D -m644 "LICENSE" "${pkgdir}/usr/share/licenses/${pkgname}/LICENSE"
  rm -f "${pkgdir}/usr/share/doc/${pkgname}/LICENSE"
}
```

`arch-hs` will collect the information from hackage db, and apply it into a fixed template after some processing steps
including renaming, matching license, and filling out dependencies etc.
If the package doesn't have test suits, `check()` will be removed. However, packaging haven't been done so far.
`arch-hs` may does well statically, but we should guarantee that this package can be built by ghc with the latest dependencies;
hence some patchs may be required in `prepare()`.


## Options

### Output
```
$ arch-hs -o "/home/berberman/Desktop/test/" termonad
```

This will generate a series of PKGBUILD including `termonad` with its dependencies into the output dir.

### Flag Assignments
```
$ arch-hs -f inline-c:gsl-example:true termonad
```

Using `-f` can pass flags, which may affect the results of rsolving.  

### AUR Searching
```
$ arch-hs -a termonad
```

Using `-a` can regard AUR as another package provider. `arch-hs` will try to search missing packages in AUR.

### Skipping Components
```
$ arch-hs -s termonad-test termonad
```

Using `-s` can force skip runnable components in dependency rsolving.
This is useful when a package doesn't provide flag to disable its runnables, which will be built by default but are trivial in system level packaging.
Notice that this only makes sense in the lifetime of `arch-hs`, whereas generated PKGBUILD and actual build processes will not be affected.

### Extra Cabal Files

```
$ arch-hs -e /home/berberman/arch-hs/arch-hs.cabal arch-hs
```

Using `-e` can can include extra `.cabal` files as supplementary. Useful when the target like `arch-hs` hasn't been released to hackage. 

### Help

```
$ arch-hs --help
arch-hs - a program generating PKGBUILD for hackage packages.

Usage: arch-hs [-h|--hackage PATH] [-c|--community PATH] [-o|--output PATH] 
               [-f|--flags package_name:flag_name:true|false,...] 
               [-s|--skip component_name,...] [-e|--extra PATH_1,...] [-a|--aur]
               TARGET
  Try to reach the TARGET QAQ.

Available options:
  -h,--hackage PATH        Path to hackage index
                           tarball (default: "~/.cabal/packages/YOUR_HACKAGE_MIRROR/01-index.tar | 00-index.tar")
  -c,--community PATH      Path to
                           community.db (default: "/var/lib/pacman/sync/community.db")
  -o,--output PATH         Output path to generated PKGBUILD files (empty means
                           dry run)
  -f,--flags package_name:flag_name:true|false,...
                           Flag assignments for packages - e.g.
                           inline-c:gsl-example:true (separated by ',')
  -s,--skip component_name,...
                           Skip a runnable component (executable, test suit, or
                           benchmark) in dependency calculation
  -e,--extra PATH_1,...    Extra cabal files' path - e.g.
                           /home/berberman/arch-hs/arch-hs.cabal
  -a,--aur                 Enable AUR searching
  -h,--help                Show this help text

```

For all available options, have a look at the help message.

## Diff

`arch-hs` also provides a component called `arch-hs-diff`. `arch-hs-diff` can show differences of information used in PKGBUILD between two versions of a hackage package.
This is useful in the subsequent maintenance of a package. Example:

```
$ arch-hs-diff HTTP 4000.3.14 4000.3.15
  ▶ You didn't pass -f, different flag values may make difference in dependency resolving.
  ⓘ Start running...
  ⓘ Downloading cabal file from https://hackage.haskell.org/package/HTTP-4000.3.14/revision/0.cabal...
  ⓘ Downloading cabal file from https://hackage.haskell.org/package/HTTP-4000.3.15/revision/0.cabal...
Package: HTTP
Version: 4000.3.14  ⇒  4000.3.15
Synopsis: A library for client-side HTTP
URL: https://github.com/haskell/HTTP
Depends: 
    base  >=4.3.0.0 && <4.14
    time  >=1.1.2.3 && <1.10
    array  >=0.3.0.2 && <0.6
    bytestring  >=0.9.1.5 && <0.11
    mtl  >=2.0 && <2.3
    network-uri  ==2.6.*
    network  >=2.6 && <3.2
    parsec  >=2.0 && <3.2
--------------------------------------
    base  >=4.3.0.0 && <4.15
    time  >=1.1.2.3 && <1.11
    array  >=0.3.0.2 && <0.6
    bytestring  >=0.9.1.5 && <0.11
    mtl  >=2.0 && <2.3
    network-uri  ==2.6.*
    network  >=2.6 && <3.2
    parsec  >=2.0 && <3.2
MakeDepends: 
    deepseq  >=1.3.0.0 && <1.5
    HUnit  >=1.2.0.1 && <1.7
    httpd-shed  >=0.4 && <0.5
    mtl  >=1.1.1.0 && <2.3
    pureMD5  >=0.2.4 && <2.2
    split  >=0.1.3 && <0.3
    test-framework  >=0.2.0 && <0.9
    test-framework-hunit  >=0.3.0 && <0.4
Flags:
  HTTP
    ⚐ mtl1:
      description: Use the old mtl version 1.
      default: False
      isManual: False
    ⚐ warn-as-error:
      description: Build with warnings-as-errors
      default: False
      isManual: True
    ⚐ conduit10:
      description: Use version 1.0.x or below of the conduit package (for the test suite)
      default: False
      isManual: False
    ⚐ warp-tests:
      description: Test against warp
      default: False
      isManual: True
    ⚐ network-uri:
      description: Get Network.URI from the network-uri package
      default: True
      isManual: False


  ✔ Success!
```

`arch-hs-diff` does not require hackage db, it downloads cabal files from hackage server instead. 

## Limitations

* The dependency solver will **ONLY** expand the dependencies of *executables* and *libraries* recursively, because
circular dependency lies ubiquitously involving *test suites*, *benchmarks*, and their *buildTools*.

* Currently, `arch-hs`'s functionality is limited to dependency processing, whereas necessary procedures like
file patches, loose of version constraints, etc. are need to be done manually, so **DO NOT** give too much trust in generated PKGBUILD files.

## ToDoList

- [ ] **Standardized pretty printing**.

- [ ] Structuralized PKGBUILD template.

- [x] AUR support.

- [ ] Logging system.

- [ ] A watchdog during dependency calculation.

- [x] Working with given `.cabal` files which haven't been released to hackage.

- [ ] Using `hackage-security` to manage hackage index tarball.


## Contributing

Issues and PRs are always welcome. **\_(:з」∠)\_**
