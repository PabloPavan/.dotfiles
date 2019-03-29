# How to install emacs 

```shell
sudo apt-get install emacs25 emacs-goodies-el org-mode org-mode-doc ess r-base texlive auctex xclip gfortran libc6-dev g++ libssl-dev libcurl4-openssl-dev libxml2-dev -y
```

# How to install xmonad

```shell
sudo apt-get install xmonad suckless-tools xscreensaver xmobar feh kde-spectacle
```
cloning the repo to deploy in your machine:
```shell
$ git clone git@github.com:PabloPavan/.dotfiles.git
$ alias dgit='git --git-dir ~/.dotfiles/.git --work-tree=$HOME'
$ dgit reset --hard
``` 
Please note that any files that exist in your home directory will be overwritten by the files from your repository if they're present.

## How to configure xscreensaver 

```shell
$ xscreensaver-demo 
``` 
## How to use multiple screens

Use the xrandr command, following the example:

```shell
$ xrandr

Screen 0: minimum 320 x 200, current 3840 x 1080, maximum 8192 x 8192
HDMI-1 connected primary 1920x1080+0+0 (normal left inverted right x axis y axis) 477mm x 268mm
   1920x1080     60.00*+  50.00    59.94  
   1920x1080i    60.00    50.00    59.94  
   1680x1050     59.88  
   1280x1024     75.02    60.02  
   1152x864      75.00  
   1280x720      60.00    50.00    59.94  
   1024x768      75.03    60.00  
   800x600       75.00    60.32  
   720x576       50.00  
   720x480       60.00    59.94  
   640x480       75.00    60.00    59.94  
   720x400       70.08  
DP-1 disconnected (normal left inverted right x axis y axis)
HDMI-2 disconnected (normal left inverted right x axis y axis)
DP-2 connected 1920x1080+1920+0 (normal left inverted right x axis y axis) 477mm x 268mm
   1920x1080     60.00*+
   1680x1050     59.95  
   1280x1024     75.02    60.02  
   1152x864      75.00  
   1024x768      75.03    60.00  
   800x600       75.00    60.32  
   640x480       75.00    59.94  
   720x400       70.08 

```

Select the connected screens and use the command: Obs. Select **--right-of** to be the main screen.

```shell
$ xrandr --output HDMI-1 --auto --output DP-2 --auto --right-of HDMI-1 
``` 

# EXTRA
## How to config your dotfiles git

Create a directory to hold your git database (the .git dir):

```shell
$ mkdir ~/.dotfiles/
$ cd ~/.dotfiles/
~/.dotfiles$ git init
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

### Add files
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
$ dgit remote add origin your@githublink
$ dgit push origin master
 * [new branch]      master -> master
 ```
 ### And easily deploy them to a new machine:
 
```shell
$ git clone your@githublink
$ alias dgit='git --git-dir ~/.dotfiles/.git --work-tree=$HOME'
$ dgit reset --hard
HEAD is now at f437f9f Added .profile
``` 
 
