# Neil: Workflow Utilities [![Hackage version](https://img.shields.io/hackage/v/neil.svg?label=Hackage)](https://hackage.haskell.org/package/neil) [![Linux Build Status](https://img.shields.io/travis/ndmitchell/neil.svg?label=Linux%20build)](https://travis-ci.org/ndmitchell/neil) [![Windows Build Status](https://img.shields.io/appveyor/ci/ndmitchell/neil.svg?label=Windows%20build)](https://ci.appveyor.com/project/ndmitchell/neil)

A tool for performing common actions run by Neil Mitchell. Many of these commands enhance specific aspects of my workflow. While other people are welcome to use this tool, it is not supported. Use `neil --help` to see what is available. Some uses:

* `neil docs` can be used to upload documentation to Hackage, as per [this blog post](http://neilmitchell.blogspot.com/2014/10/fixing-haddock-docs-on-hackage.html).

## Contributions

These guidelines apply to all my projects, not just this one. I am pretty relaxed, but as guidelines to my preferences:

* GitHub pull requests are the preferred mechanism for sending me code.
* If writing the code will take a significant amount of time, please raise a ticket first, so we can discuss possible approaches and whether I agree with the direction. I'd rather give advice before people use their valuable time, than after.
* That said, if you have a pull request that fixes something, don't bother raising a separate ticket - just the pull request is fine.
* If you go down a few dead ends and have a few junk commits in there, don't worry, as long as they don't stray too far outside the area you were working on. No need to squash the history.
* If I am not responding after a few days, ping me again on the ticket, or via email. I'm probably just busy or lost the email in a pile. I certainly intended to thank you for contribution and give some feedback.
* If your preferences are vehemently different, then follow yours, I'm really not all that fussed - but sometimes it's easier for people to know what you do like rather than to spend effort thinking about what you like, or doing something they think you will like but you actually don't.

### Quirks

There are a few places where my opinion on coding differs from other people, so I document them here so as not to cause surprise:

* I tend to be conservative in my use of extensions. If the project doesn't already use type functions/GADTs, then unless they add compelling benefits, I'm probably not going to like them.
* I am conservative in my use of additional packages, since each additional package complicates a project. I tend to not use upper bounds unless I think they are particularly valuable, but do so on a package-by-package basis.
* I avoid CPP where possible, trying other techniques to work around API changes through versions.
* I don't use explicit imports, since I consider the benefits small, but the cost of continually editing imports while developing to be a significant distraction to the flow of coding.
