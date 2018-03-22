# microbio
A set of R functions, tools, utilities and wrappers to perform bionformatic analyses applied to Microbiology.

[How to] (https://github.com/abrozzi/microbio#how-to)

* [Git and RStudio](#git-and-rstudio)
* [Install pyclone] (#install-pyclone)

How to
====

### Git and RStudio 

1 - Create by RStudio a project to build a package:

> ~/ab/mypack

2 - Create a repository on GitHub

3 - Move to the local package directory and:

> cd ~/ab/mypack

> git init
  
> git add .

> git commit -m "first commit"

> git remote add origin https://github.com/ab/mypack.git

> git push -u origin master

4 - In Rstudio you can do the same by Git menu: commit -> push-

5 - If you need proxy add in your home a file .Renviron with the following lines:

> http_proxy="http://myproxy.net:port"

> https_proxy="https://myproxy.net:2011"

### Install pyclone
This is fo Mac users.

1 - Make sure you have rigth proxy set up. In a terminal:

> export http_proxy=http://myproxy.net:port

> export https_proxy=https://myproxy.net:port

2 - Download and install Miniconda:

> wget https://repo.continuum.io/miniconda/Miniconda-latest-MacOSX-x86_64.sh

> bash Miniconda-latest-MacOSX-x86_64.sh

3 - Install numpy:

> /Users/ab/miniconda2/bin/./conda install numpy

4- Install pyclone:

> /Users/ab/miniconda2/bin/./conda install pyclone -c aroth85

You might use sudo
