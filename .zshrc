# Path to your oh-my-zsh installation.
export ZSH="$HOME/.oh-my-zsh"

# Set the theme to load.
ZSH_THEME="robbyrussell"

# Enable command auto-correction.
ENABLE_CORRECTION="true"

# Plugins.
plugins=(git thefuck zsh-autosuggestions zsh-syntax-highlighting)

source $ZSH/oh-my-zsh.sh

# Save my life from typos.
alias ls='ls -G'
alias lss='ls'
alias lsss='ls'
alias claer='clear'
alias mkdri='mkdir'
alias dls='cd ~/Downloads/'
alias say='espeak'
alias td='e ~/wmarcoyu/TODO.org'
alias tg='e ~/wmarcoyu/TOGO.org'
alias tw='e ~/wmarcoyu/watch-list.org'
alias op='open'
alias lv='open ~/wmarcoyu/pepTalk'
alias life='cd ~/Downloads/La\ Vida/'
alias day='e ~/wmarcoyu/my-day.org'
alias cv='open ~/Downloads/Yu_Wang_CV.pdf'
alias opsa='open -a Safari'
alias brv='open -a "Brave Browser"'
alias we='open -a WeChat'
alias conf='e ~/.zshrc'
alias ck='open -a "Activity Monitor"'
alias getlatextemplate='cp ~/wmarcoyu/math/template.tex template.tex'
alias mkae='make'
alias af='e ~/wmarcoyu/dotFiles/.always-forget.txt'
alias saf='cat ~/wmarcoyu/dotFiles/.always-forget.txt'
alias notes='e ~/wmarcoyu/cs-notes.md'
alias cppcompile='g++ -std=c++17 -Wall -Werror -pedantic -g -fsanitize=address -fsanitize=undefined'

# Git shortcuts
alias gs='git status'
alias ga.='git add .'
alias ga='git add'
alias gc='git commit -m'
alias gps='git push'
alias gpl='git pull'
alias gb='git branch'
alias gco='git checkout'
alias gl='git log'
alias gsw='git switch'
alias getgitignore='cp ~/wmarcoyu/dotfiles/.gitignore .gitignore'
alias venv='source env/bin/activate'

# Emacs
e ()
{
  emacs "$@" &
}

# Colorize ls output.
LSCOLORS=ga
export LSCOLORS

# Suppress Malloc warning:
# nano zone abandoned due to inability to reserve vm space.
export MallocNanoZone=0

# Custom environment variables.
export CAEN='wmarcoyu@login.engin.umich.edu'
export E583A='wmarcoyu@eecs583a.eecs.umich.edu'
export E583B='wmarcoyu@eecs583b.eecs.umich.edu'

# University of Michigan CAEN account.
alias caen='ssh wmarcoyu@login.engin.umich.edu'

# Project specifics.
alias run='./bin/starchaser_run'
alias js='npx webpack --watch'
alias getmyip='http ipinfo.io/ip'
alias whereami='http ipinfo.io/loc'
alias ec2app='ssh -i credentials/starchasers.pem ec2-user@ec2-50-17-5-132.compute-1.amazonaws.com'
alias ec2data='ssh -i credentials/starchasers-data.pem ec2-user@ec2-3-238-88-126.compute-1.amazonaws.com'
# >>> xmake >>>
test -f "/Users/marco.w/.xmake/profile" && source "/Users/marco.w/.xmake/profile"
# <<< xmake <<<