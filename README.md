# Neil: Workflow Utilities [![Hackage version](https://img.shields.io/hackage/v/neil.svg?style=flat)](http://hackage.haskell.org/package/neil) [![Build Status](http://img.shields.io/travis/ndmitchell/neil.svg?style=flat)](https://travis-ci.org/ndmitchell/neil)

A tool for performing common actions run by Neil Mitchell. Many of these commands enhance specific aspects of my workflow. While other people are welcome to use this tool, it is not supported.

This documentation is highly likely to be out of date, and I recommend running `neil --help` to get a correct list.

## Darcs Enhancements

All these commands can either be run from a darcs repo, or from a directory containing darcs repos below it.

* `neil whatsnew` - says if there are patches to pull, patches to push, or local changes.
* `neil pull` - standard darcs pull.
* `neil push` - standard darcs push.
* `neil send` - standard darcs send, but to a tarball if there are multiple repos around.

## Cabal Enhancements

* `neil sdist` - `cabal sdist`, plus some checks for well-formedness.
