# Created by newuser for 5.4.2


#/bin/shell
# Created by newuser for 5.0.7
# function untargz {
#     tar xzvf "$@"
# }

# https://github.com/rpellerin/dotfiles/blob/master/.aliases
# Extract any archive
function extract() {
    if [ -f $1 ] ; then
        case $1 in
            *.tar.bz2) tar xjf $1 ;;
            *.tar.gz) tar xzf $1 ;;
            *.bz2) bunzip2 $1 ;;
            *.rar) rar x $1 ;;
            *.gz) gunzip $1 ;;
            *.tar) tar xf $1 ;;
            *.tbz2) tar xjf $1 ;;
            *.tgz) tar xzf $1 ;;
            *.zip) unzip $1 ;;
            *.Z) uncompress $1 ;;
            *) echo "'$1' cannot be extracted via extract()" ;;
        esac
    else
        echo "'$1' is not a valid file"
    fi
}


# Open a file with the appropriate application
function open {
    while [ "$1" ] ; do
        xdg-open $1 &> /dev/null
        shift # shift décale les param
    done
}


# A reminder
function githelp {
    echo "-------------------------------------------------------------------------------"
    echo "git clone http://... [repo-name]"
    echo "git init [repo-name]"
    echo "-------------------------------------------------------------------------------"
    echo "git add -A <==> git add . ; git add -u # Add to the staging area (index)"
    echo "-------------------------------------------------------------------------------"
    echo "git commit -m 'message' -a"
    echo "git commit -m 'message' -a --amend"
    echo "-------------------------------------------------------------------------------"
    echo "git status"
    echo "git log --stat # Last commits, --stat optional"
    echo "git ls-files"
    echo "git diff HEAD~1..HEAD"
    echo "-------------------------------------------------------------------------------"
    echo "git push origin master"
    echo "git push origin master:master"
    echo "-------------------------------------------------------------------------------"
    echo "git remote add origin http://..."
    echo "git remote set-url origin git://..."
    echo "-------------------------------------------------------------------------------"
    echo "git stash"
    echo "git pull origin master"
    echo "git stash list ; git stash pop"
    echo "-------------------------------------------------------------------------------"
    echo "git submodule add /absolute/path repo-name"
    echo "git submodule add http://... repo-name"
    echo "-------------------------------------------------------------------------------"
    echo "git checkout -b new-branch <==> git branch new-branch ; git checkout new-branch"
    echo "git merge old-branch"
    echo "git branch local_name origin/remote_name # Associate branches"
    echo "-------------------------------------------------------------------------------"
    echo "git update-index --assume-unchanged <file> # Ignore changes"
    echo "git rm --cached <file> # Untrack a file"
    echo "-------------------------------------------------------------------------------"
    echo "git reset --hard HEAD # Repair what has been done since last commit"
    echo "git revert HEAD # Repair last commit"
    echo "git checkout [file] # Reset a file to its previous state at last commit"
    echo "-------------------------------------------------------------------------------"
    echo "git tag # List"
    echo "git tag v0.5 # Lightwieght tag"
    echo "git tag -a v1.4 -m 'my version 1.4' # Annotated tag"
    echo "git push origin v1.4 # Pushing"
    echo "-------------------------------------------------------------------------------"
    echo "HOW TO RENAME A BRANCH LOCALLY AND REMOTELY"
    echo "git branch -m old_name new_name"
    echo "git push origin new_name"
    echo "git push origin :old_name"
    echo "------"
    echo "Each other client of the repository has to do:"
    echo "git fetch origin ; git remote prune origin"
    echo "-------------------------------------------------------------------------------"
}


# Alias
alias ls='ls --color=auto'
alias l="ls  --color=auto -la"
alias ll="ls --color=auto -la"
alias grep='grep -i --color=auto'
alias rm='rm --interactive --verbose'
alias mv='mv --interactive --verbose'
alias cp='cp --verbose'

# bindkey '~' beginning-of-line # Home (console)
# bindkey '9~' '~' # Home (console)

domain=$(cat /etc/resolv.conf | grep domain)
if [[ "$domain" = "domain ca.cite-u.univ-nantes.prive" ]]; then
    source ~/proxy/citeu.sh
    echo ">>> Cité U proxy environment"
fi
search=$(cat /etc/resolv.conf | grep search)
if [[ "$search" = "search jof.wifi.univ-nantes.prive" ]]; then
    source ~/proxy/iut-wifi.sh
    echo ">>> IUT wifi proxy environment"
fi


UTILISATEUR=%n
# THIS_DIRECTORY=%~
THIS_DIRECTORY=%c

PROMPT="${UTILISATEUR} | ${THIS_DIRECTORY} $ "
alias dgit='git --git-dir ~/.dotfiles/.git --work-tree=$HOME'
