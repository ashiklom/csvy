Contributions to **csvy** are welcome from anyone and are best sent as pull requests on [the GitHub repository](https://github.com/leeper/csvy/). This page provides some instructions to potential contributors about how to add to the package.

 1. Contributions can be submitted as [a pull request](https://help.github.com/articles/creating-a-pull-request/) on GitHub by forking or cloning the [repo](https://github.com/leeper/csvy/), making changes and submitting the pull request.
 
 2. Pull requests should involve only one commit per substantive change. This means if you change multiple files (e.g., code and documentation), these changes should be committed together. If you don't know how to do this (e.g., you are making changes in the GitHub web interface) just submit anyway and the maintainer will clean things up.
 
 3. All contributions must be submitted consistent with the package license ([GPL-2](http://www.gnu.org/licenses/old-licenses/gpl-2.0.en.html)).
 
 4. All contributions need to be noted in the `Authors@R` field in the [DESCRIPTION](https://github.com/leeper/csvy/blob/master/DESCRIPTION). Just follow the format of the existing entries to add your name (and, optionally, email address). Substantial contributions should also be noted in [`inst/CITATION`](https://github.com/leeper/csvy/blob/master/inst/CITATION).
 
 5. Please run `R CMD BUILD csvy` and `R CMD CHECK csvy_VERSION.tar.gz` before submitting the pull request to check for any errors.
 
Some specific types of changes that you might make are:

 1. Documentation-only changes (e.g., to Rd files, README, vignettes). This is great! All contributions are welcome.
 
 2. Changes requiring a new package dependency should be discussed on the GitHub issues page before submitting a pull request.
 
 3. Message translations. These are very appreciated! The format is a pain, but if you're doing this I'm assuming you're already familiar with it.

Any questions you have can be opened as GitHub issues or directed to thosjleeper (at) gmail.com.

