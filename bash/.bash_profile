## Read Once
# Supposedly, .bash_profile is sourced whenever you log in. But I don't use a
# login shell. I use a login manager. From my tests, it didn't seem like the
# login manager sourced the .bash_profile script. Some people recommend .bashrc
# because .bashrc is sourced each time you start a shell (even if it isn't a
# login server). But a common pattern is to define an environment variable VAR
# like `export VAR=$VAR:addition`. If you open a shell within a shell or
# something, you end up variables with duplicate values. This line is a flag
# to set a global environment variable that indicates the .bash_profile has
# been read. In my .bashrc, it sources the .bash_profile only if READ_PROFILE
# is undefined.
export READ_PROFILE=True

## Path
export PATH="$HOME/.emacs.d/bin:$PATH"

## Source Guix Profiles
# Set appropriate Guix profiles. The important part is to source each
# ./etc/profile script. That sets the appropriate environment variables that
# Guix uses. The $HOME/.config profile is the default profile for Guix
# itself. It controls what version of guix you are using and what set of package
# repositories you are using. The $HOME/.guix-profile profile controls the set
# of packages that are installed in a profile.
GUIX_PROFILE="$HOME/.config/guix/current"
. "$GUIX_PROFILE/etc/profile"
GUIX_PROFILE="$HOME/.guix-profile"
. "$GUIX_PROFILE/etc/profile"

## Set CA Certificates
# Set CA Certificate environement variables so we can use TLS. First, install
# the guix package `nss-certs` to get the certificates. Why do we need to set
# environment variables? Most SSL clients use a location in root. But Guix
# is oriented for unprivileged package management. In order to use
# user-installed certificates, SSL clients allow use specific environment
# variables to override the default certificate location. More information about
# Guix SSL certs can be found here:
# https://guix.gnu.org/manual/en/html_node/X_002e509-Certificates.html#X_002e509-Certificates
export SSL_CERT_DIR="$HOME/.guix-profile/etc/ssl/certs"
export SSL_CERT_FILE="$HOME/.guix-profile/etc/ssl/certs/ca-certificates.crt"
export GIT_SSL_CAINFO="$SSL_CERT_FILE"

## Doom Git hack (might be deprecated after CA-cert addition)
# Doom requires Git. Doom ignores git config unless you explicitly
# set it. This is a hacky workaround. 
# (I think I only need it for the installation.)
#export DOOMGITCONFIG="$HOME/.gitconfig"

# Aliases
alias emc="emacsclient -c -a 'vim' &"

