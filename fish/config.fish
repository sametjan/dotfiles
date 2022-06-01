# Make sure brew is is on PATH
eval (/opt/homebrew/bin/brew shellenv)

# Setup ASDF for programming languages
source /opt/homebrew/opt/asdf/asdf.fish

# Source autojump
[ -f /opt/homebrew/share/autojump/autojump.fish ]; and source /opt/homebrew/share/autojump/autojump.fish
# Created by `pipx` on 2021-07-24 12:58:23
set PATH $PATH /Users/sametjan/.local/bin

# tabtab source for packages
# uninstall by removing these lines
[ -f ~/.config/tabtab/fish/__tabtab.fish ]; and . ~/.config/tabtab/fish/__tabtab.fish; or true
