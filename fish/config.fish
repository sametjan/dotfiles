# Make sure brew is is on PATH
eval (/opt/homebrew/bin/brew shellenv)

# Setup ASDF for programming languages
source /opt/homebrew/opt/asdf/asdf.fish

# Source autojump
[ -f /opt/homebrew/share/autojump/autojump.fish ]; and source /opt/homebrew/share/autojump/autojump.fish