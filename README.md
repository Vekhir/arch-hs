# arch-hs

[![GitHub CI](https://github.com/berberman/arch-hs/workflows/CI/badge.svg)](https://github.com/berberman/arch-hs/actions)
[![Build Status](https://travis-ci.com/berberman/arch-hs.svg?branch=master)](https://travis-ci.com/berberman/arch-hs)
[![MIT license](https://img.shields.io/badge/license-MIT-blue.svg)](LICENSE)

A program generating PKGBUILD for hackage packages. Special thanks to [felixonmars](https://github.com/felixonmars/).


## Introduction

Given the name of a package in hackage, `arch-hs` can generate its corresponding PKGBUILD draft.
It has a naive built-in dependency solver, which can fetch all dependencies we need to produce a archlinux package. 
During the dependency calculation, all version constraints will be discarded due to the arch haskell packaging strategy,
and packages already exist in the [community](https://www.archlinux.org/packages/) will be excluded.

## Prerequisite

`arch-hs` is just a PKGBUILD text file generator, which is not integrated with `pacman`, depending on nothing than:

* Pacman database (`community.db`), ~~i.e., archlinux system.~~ the db file can be specified manually for now. 

* Hackage database tarball (`00-index.tar`), usually provided by `cabal-install`.

## Installation

`arch-hs` has not been released currently, thus it is required to build from source.
`arch-hs` only supports the latest GHC version.

### Install from AUR

```
❯ yay -S arch-hs-git
```

This is the **recommended** way, since it doesn't require `cabal` or `stack`, using system level ghc and haskell packages instead.

### Install from source (development)

```
❯ git clone https://github.com/berberman/arch-hs
```

Then build it via stack or cabal.

#### Stack
```
❯ stack install
```

#### Cabal (dynamic)
```
❯ cabal configure --disable-library-vanilla --enable-shared --enable-executable-dynamic --ghc-options=-dynamic 
❯ cabal install
```

## Usage

Just run `arch-hs` in command line with options and a target. Here is an example:
we will create the archlinux package of `dhall-lsp-server`:

```
❯ arch-hs -o "/home/berberman/Desktop/test/" dhall-lsp-server

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
❯ arch-hs -o "/home/berberman/Desktop/test/" termonad
```

This will generate a series of PKGBUILD including `termonad` with its dependencies into the output dir.

### Flag Assignments
```
❯ arch-hs -f inline-c:gsl-example:true termonad
```

Using `-f` can pass flags, which may affect the results of solving.  

### AUR Searching
```
❯ arch-hs -a termonad
```

Using `-a` can regard AUR as another package provider. `arch-hs` will try to search missing packages in AUR.

### Skipping Components
```
❯ arch-hs -s termonad-test termonad
```

Using `-s` can force skip runnable components in dependency solving.
This is useful when a package doesn't provide flag to disable its runnables, which will be built by default but are trivial in system level packaging.
Notice that this only makes sense in the lifetime of `arch-hs`, whereas generated PKGBUILD and actual build processes will not be affected.

### Extra Cabal Files

```
❯ arch-hs -e /home/berberman/arch-hs/arch-hs.cabal arch-hs
```

Using `-e` can can include extra `.cabal` files as supplementary. Useful when the target like `arch-hs` hasn't been released to hackage. 

### Help

```
❯ arch-hs --help
arch-hs - a program generating PKGBUILD for hackage packages.

Usage: arch-hs [-h|--hackage PATH] [-c|--community PATH] [-o|--output PATH] 
               [-f|--flags package_name:flag_name:true|false,...] 
               [-s|--skip component_name,...] [-e|--extra PATH_1,...] [-a|--aur]
               TARGET
  Try to reach the TARGET QAQ.

Available options:
  -h,--hackage PATH        Path to
                           00-index.tar (default: "~/.cabal/packages/YOUR_HACKAGE_MIRROR/00-index.tar")
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

## Limitations

* The dependency solver will **ONLY** expand the dependencies of *executables* and *libraries* recursively, because
circular dependency lies ubiquitously involving *test suites*, *benchmarks*, and their *buildTools*.

* Currently, `arch-hs`'s functionality is limited to dependency processing, whereas necessary procedures like
file patches, loose of version constraints, etc. are need to be done manually, so **DO NOT** give too much trust in generated PKGBUILD files.

## ToDoList

- [ ] **Standardized pretty printing**.

- [ ] Structuralized PKGBUILD template.

- [x] ~~Ability to switch *buildable* of a component in the beginning.~~ Skipping specific components as alternative.

- [x] AUR support.

- [ ] Logging system.

- [ ] A watchdog during dependency calculation.

- [x] Working with given `.cabal` files which havn't been released to hackage.


## Contributing

Issues and PRs are always welcome. **\_(:з」∠)\_**
