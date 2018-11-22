# How to install xmonad

Install xmonad and some handy tools
Install xmonad and dmenu:
```shell
sudo apt-get install xmonad suckless-tools xscreensaver xmobar feh zsh
```

# How to use this repo

Create a directory to hold your git database (the .git dir):

```shell
$ mkdir ~/.dotfiles/
$ cd ~/.dotfiles/
~/.dotfiles$ git init
``` 
Create a .gitifnore file that will ignore everything. You can be more conservative here and only ignore things you don't want in git. I like to pick and choose exactly which things I'll add, so I ignore everything by default and then add it later.
```shell
~/.dotfiles$ echo "*" > .gitignore
~/.dotfiles$ git add -f .gitignore 
~/.dotfiles$ git commit -m "gitignore"
``` 

Now we've got a repository set up for our files. It's out of the way of our home directory, so the .git directory won't cause any conflicts with other repositories in your home directory. Here comes the magic part that lets us use this repository to keep our home directory in. Add the dgit alias to your .bashrc or .profile, whichever you prefer:

```shell
~/.dotfiles$ echo "alias dgit='git --git-dir ~/.dotfiles/.git --work-tree=\$HOME'" >> ~/.bashrc
``` 
​You'll have to log out and in again, or just copy-paste the alias defnition in your current shell. We can now the repository out in our home directory with the dgit command:

```shell
~/.dotfiles$ cd ~
$ dgit reset --hard
HEAD is now at 642d86f gitignore
``` 
Now the repository is checked out in our home directory, and it's ready to have stuff added to it. The dgit reset --hard command might seem spooky (and I do suggest you make a backup before running it), but since we're ignoring everything, it'll work just fine.

## Using it
Everything we do now, we do with the dgit command instead of normal git. In case you forget to use dgit, it simply won't work, so don't worry about that.

A dgit status shows nothing, since we've gitignored everything:
```shell
$ dgit status
On branch master
nothing to commit, working directory clean
```
We add things by overriding the ignore with -f:
```shell
$ dgit add -f .profile 
$ dgit commit -m "Added .profile"
[master f437f9f] Added .profile
 1 file changed, 22 insertions(+)
 create mode 100644 .profile
 ```
We can push our configuration files to a remote repository:
```shell
$ dgit remote add origin ssh://git@github.com:PabloPavan/xmonad_dotfiles.git
$ dgit push origin master
 * [new branch]      master -> master
 ```
## And easily deploy them to a new machine:
```shell
$ ssh someothermachine
$ git clone ssh://git@github.com:PabloPavan/xmonad_dotfiles.git
$ alias dgit='git --git-dir ~/.dotfiles/.git --work-tree=$HOME'
$ dgit reset --hard
HEAD is now at f437f9f Added .profile
``` 
Please note that any files that exist in your home directory will be overwritten by the files from your repository if they're present.
