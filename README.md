# SplitstRee

A set of R functions to work with SplitsTree

[How to] (<https://github.com/abrozzi/SplitstRee#how-to>)

-   [Git and RStudio](#git-and-rstudio)

# How to

### Set SSh key Mac for github

I had some troubles with git push. Basically everytime I was doing a push a pop-up window saied "Username for <https://github.com>". Next message was to insert password, but nothing worked. Surfing a bit the web,I found the solution which worked for me.

1.  Open terminal and type:

`ssh-keygen -t ed25519 -C "myemail@something.com"`

ed25519 is an algorithm.

2.  Create a file:

`nano ~/.ssh/config`

3.  Insert in the file

```         
Host github.com
 AddKeysToAgent yes
 UseKeychain yes
 IdentityFile ~/.ssh/id_ed25519
```

4.  In the terminal

`ssh-add --apple-use-keychain ~/.ssh/id_ed25519`

5.  Finally in the project Settings tab of github.com add a Deploy Key:

`cat /Users/abrozzi/.ssh/id_ed25519.pub`

which is something like:

`ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAILFBLSrDAAI2U21mDcSddnQjgouGfGXfr8AfQ8SQNzJb alessandro.brozzi@gmail.com`

copy paste and add a title in github.com.

### Git and RStudio {#git-and-rstudio}

1 - Create by RStudio a project to build a package:

> \~/ab/mypack

2 - Create a repository on GitHub

3 - Move to the local package directory and:

> cd \~/ab/mypack

> git init

> git add .

> git commit -m "first commit"

> git remote add origin <https://github.com/ab/mypack.git>

> git push -u origin master

4 - In Rstudio you can do the same by Git menu: commit -\> push-

5 - If you need proxy add in your home a file .Renviron with the following lines:

> http_proxy="[http://myproxy.net:port](http://myproxy.net:port){.uri}"

> https_proxy="<https://myproxy.net:2011>"
