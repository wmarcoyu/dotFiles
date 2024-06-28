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
alias dcm='cd ~/Documents/'
alias prj='cd ~/Projects/'
alias td='ec ~/wmarcoyu/TODO.org'
alias tg='ec ~/wmarcoyu/TOGO.org'
alias tw='ec ~/wmarcoyu/watch-list.org'
alias op='open'
alias lv='ec ~/wmarcoyu/pepTalk.txt'
alias life='cd ~/Downloads/La-Vida/'
alias day='ec ~/wmarcoyu/my-day.org'
alias cv='open ~/Downloads/Yu_Wang_CV.pdf'
alias opsa='open -a Safari'
alias brv='open -a "Brave Browser"'
alias we='open -a WeChat'
alias conf='ec ~/.zshrc'
alias ck='open -a "Activity Monitor"'
alias getlatextemplate='cp ~/wmarcoyu/latex/template.tex template.tex'
alias mkae='make'
alias af='ec ~/wmarcoyu/dotFiles/.always-forget.txt'
alias saf='cat ~/wmarcoyu/dotFiles/.always-forget.txt'
alias notes='ec ~/wmarcoyu/cs-notes.md'
alias cppcompile='g++ -std=c++17 -Wall -Werror -pedantic -g \
-fsanitize=address -fsanitize=undefined'
alias hola='cd ~/wmarcoyu/Spanish/'
alias getgitignore='cp ~/wmarcoyu/dotfiles/.gitignore .gitignore'
alias venv='source env/bin/activate'
alias cafe='caffeinate'

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

# Emacs
e ()
{
  emacs "$@" &
}

ec ()
{
  emacsclient -c "$@" &
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
alias ec2app='ssh -i credentials/starchasers.pem \
ec2-user@ec2-50-17-5-132.compute-1.amazonaws.com'
alias ec2data='ssh -i credentials/starchasers-data.pem \
ec2-user@ec2-3-238-88-126.compute-1.amazonaws.com'

# LLVM-CFG-ANNOTATOR.
alias ir='clang \
--sysroot=/Applications/Xcode.app/Contents/Developer/Platforms/MacOSX.platform/Developer/SDKs/MacOSX.sdk \
-emit-llvm -S -Xclang -disable-O0-optnone'
alias noalloca='opt -passes="mem2reg" -S'
alias gendot='opt -disable-output -passes="dot-cfg"'
alias genpdf='cat .main.dot | dot -Tpdf > main.pdf'
alias run='opt -disable-output \
-load-pass-plugin=./build/ProgramVariable/ProgramVariable.dylib \
-passes="program-variable"'

test -e "${HOME}/.iterm2_shell_integration.zsh" && source "${HOME}/.iterm2_shell_integration.zsh"

export PATH="/opt/homebrew/opt/postgresql@16/bin:$PATH"

# >>> xmake >>>
test -f "/Users/yuwang/.xmake/profile" && source "/Users/yuwang/.xmake/profile"
# <<< xmake <<<
