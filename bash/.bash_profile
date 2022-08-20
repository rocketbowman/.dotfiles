# My hack to read bash_profile exactly once
export READ_PROFILE=True

# Environment Variables
export PATH="$HOME/.emacs.d/bin:$PATH"
export GUIX_PROFILE="$HOME/.guix-profile"
. "$GUIX_PROFILE/etc/profile"

# Aliases
alias emc="emacsclient -c -a 'vim' &"

