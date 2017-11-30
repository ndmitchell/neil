# Neil: Workflow Utilities [![Hackage version](https://img.shields.io/hackage/v/neil.svg?label=Hackage)](https://hackage.haskell.org/package/neil) [![Linux Build Status](https://img.shields.io/travis/ndmitchell/neil.svg?label=Linux%20build)](https://travis-ci.org/ndmitchell/neil) [![Windows Build Status](https://img.shields.io/appveyor/ci/ndmitchell/neil.svg?label=Windows%20build)](https://ci.appveyor.com/project/ndmitchell/neil)

A tool for performing common actions run by Neil Mitchell. Many of these commands enhance specific aspects of my workflow. While other people are welcome to use this tool, it is not supported. Use `neil --help` to see what is available. Some uses:

* `neil docs` can be used to upload documentation to Hackage, as per [this blog post](http://neilmitchell.blogspot.com/2014/10/fixing-haddock-docs-on-hackage.html).

## Contributions

These guidelines apply to all my projects, not just this one. I am pretty relaxed, but as guidelines to my preferences:

* I welcome and appreciate contributions. If you've contributed to my code, and we meet in real life, I'll buy you a beer.
* GitHub pull requests are the preferred mechanism for sending me code.
* If writing the code will take a significant amount of time, please raise a ticket first, so we can discuss possible approaches and whether I agree with the direction. I'd rather give advice before people use their valuable time, than after.
* That said, if you have a pull request that fixes something, don't bother raising a separate ticket - just the pull request is fine.
* If you go down a few dead ends and have a few junk commits in there, don't worry, as long as they don't stray too far outside the area you were working on. No need to squash the history.
* If I am not responding after a few days, ping me again on the ticket, or via email. I'm probably just busy or lost the email in a pile. I certainly intended to thank you for contribution and give some feedback.
* If your preferences are vehemently different, then follow yours, I'm really not all that fussed - but sometimes it's easier for people to know what you do like rather than to spend effort thinking about what you like, or doing something they think you will like but you actually don't.
* After I've merged your patch, if it would be useful to have an immediate release containing the patch (e.g. because you are working on downstream code or can only deploy official releases in production) let me know. Usually I am happy to release after any change, but for smaller changes might wait to collect a bunch of things together.
* I develop on `master` and only update the `.cabal` version just before a release.

### Quirks

There are a few places where my opinion on coding differs from other people, so I document them here so as not to cause surprise:

* I tend to be conservative in my use of extensions. If the project doesn't already use type functions/GADTs, then unless they add compelling benefits, I'm probably not going to like them.
* I am conservative in my use of additional packages, since each additional package complicates a project. I tend to not use upper bounds unless I think they are particularly valuable, but do so on a package-by-package basis.
* I avoid CPP where possible, trying other techniques to work around API changes through versions.
* Where I do use CPP, I make sure the code still loads in `ghci` alone, if all `MIN_VERSION_foo` macros have not been defined.
* I don't use explicit imports, since I consider the benefits small, but the cost of continually editing imports while developing to be a significant distraction to the flow of coding.
* I stick to ASCII characters, since I appreciate the ease of typing and complete editor/font compatibility.
* I avoid literate Haskell and boot files, both of which offer significant complexity.
* I don't add stack.yml files if they would be identical to those produced by stack init. I do test that `stack init` works and builds correctly.
* I try and push my testing to the limit, checking things like timing properties and the copyright year etc that are inherently fragile. As a result, sometimes the tests will fail. That tends to be a transient state, but don't overly worry if tests fail after a PR.
