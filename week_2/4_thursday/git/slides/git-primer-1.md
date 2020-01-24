---
theme: "solarized"
#theme: "black"
transition: "zoom"
highlightTheme: "solarized-dark"
separator: "^\n\n\n"
verticalSeparator: "^\n\n"
#margin: 0.1
#minScale: 0.2
#maxScale: 1.5
#fig_width: 7
#fig_height: 6
title: "git primer"
author: Maximilian Konzack
date: February 20, 2020
output: revealjs::revealjs_presentation
---

# Introduction to git


## Agenda
1. Understanding version control
2. How to use basic git commands
3. Hands-on with git
4. What's to learn after that


## Who has experience with version control?
- Dropbox, Nextcloud
- google drive, iCloud, OneDrive
- CVS, SVN
- git, mercurial, darcs



# What is version control?


## Filenames as version control

![blah](img/draft_mess.png)


## Local version control

<img src="img/pro-git-book/local.png" height="500">


## Centralized VCS

<img src="img/pro-git-book/centralized.png" height="500">


## Distributed VCS

<img src="img/pro-git-book/distributed.png" height="500">



# What is git?



## A distributed version control system!
- work offline
- undo errors
- small changes
- keep history
- working with others
- don't panic


## git in a nutshell


## delta-based version control system

<img src="img/pro-git-book/deltas.png">


## Stream of snapshots

<img src="img/pro-git-book/snapshots.png">


## Summary
```git```
1. keeps a timeline
2. handles most operations locally
3. leaves unchanged files as they are
4. maintains "deltas"/diffs of a changed file
5. has three states that a file can have
   - ```modified```
   - ```staged```
   - ```committed```


## Three stages

![bam](img/pro-git-book/areas.png)



# ```git``` primer


## Install git
- [x] Mac: already installed
- [ ] Linux: consult your package manager
- [ ] Windows: download

https://git-scm.com/download/win


## Setup git



# git clients


## GitHub Desktop

<img src="img/github-desktop.png" height="500">


## GitKraken

<img src="img/gitkraken.png" height="500">


## git in RStudio

<img src="img/rstudio-git.png" height="500">


## git in the shell

<img src="img/bash-git.png" height="500">




# Beyond ```git``` basics


## Branching in a nutshell


## Branching
<img src="img/pro-git-book/lr-branches-2.png" height="500">


## Create a branch
<img src="img/pro-git-book/head-to-testing.png" height="500">


## Commit in a branch
<img src="img/pro-git-book/advance-testing.png" height="500">



# Literature


##
https://git-scm.com/book/en/v2

<img src="img/pro-git-book/progit2.png" height="500">


## GitHub resources
- https://try.github.io/
- https://github.github.com/training-kit/downloads/de/github-git-cheat-sheet/


## iDiv GSU
- https://idiv-biodiversity.github.io/git-cheat-sheet/
- [Git Basics course](https://www.idiv.de/de/ydiv/lehrveranstaltungen/git-basics-for-beginner-level-git-users.html)

Thank you, Dirk and Christian
