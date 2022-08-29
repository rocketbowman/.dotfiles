# My hack to read bash_profile exactly once
export READ_PROFILE=True

# Environment Variables
export PATH="$HOME/.emacs.d/bin:$PATH"

GUIX_PROFILE="$HOME/.config/guix/current"
. "$GUIX_PROFILE/etc/profile"

GUIX_PROFILE="$HOME/.guix-profile"
. "$GUIX_PROFILE/etc/profile"

# Doom requires Git.
# Doom ignores git config unless you explicitly
# set it. This is a hacky workaround. 
# (I think I only need it for the installation.)
#export DOOMGITCONFIG="$HOME/.gitconfig"

# Aliases
alias emc="emacsclient -c -a 'vim' &"

