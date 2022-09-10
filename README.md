# Overview
This is my attempt at a systematized approach to dotfiles. This README is a set 
of reminders for myself. I didn't write it with generality in mind.

The basic strategy is as follows:
1. Keep all configurations in a centralized dotfiles directory. 
2. Use GNU Stow to create symlinks to where your OS expects to find the dotfiles. 
3. Use version control on your centralized dotfiles directory.

# Instructions
I followed the instructions in the "Manage Your Dotfiles Like a Superhero" 
video. The corresponding blog can be found here: [Manage Your Dotfiles Like a 
Superhero](https://www.jakewiesler.com/blog/managing-dotfiles). Here is a 
summary:

1. Create the required directory structure.
2. Move your configurations into the directory structure.
3. Use GNU Stow to create symlinks in the target directory.
4. Commit dotfile changes to version control.

## Terminology:

* A *package* is a directory (in .dotfiles) that contains dotfiles related to one 
another.
* A *stow directory* is a directory that contains one or more packages.
* A *target directory* is the location to which a packages content is symlinked. 

My target directory is the home directory. My stow directory is the ~/.dotfiles 
directory. Inside the stow directory are a bunch of packages that contain 
the actual dotfiles. 

## Organizing Packages

The main concept of organizing a package is that it must mimic the structure of 
the target directory. Here are two examples:

1. If your target is ~/.bashrc then you should store your actual .bashrc at 
~/.dotfiles/bash/.bashrc.
2. If your target is ~/.config/.whateverrc then you should store your actual 
.whateverrc at ~/.dotifles/whatever/.config/.whateverrc. Note the .config 
directory within the whatever package.

## GNU Stow Command
The relevant stow command for me is:

`stow -d <stow-directory> -t <target-directory> -S <package-name>`

`-S <package-name>` stows the package.
`-D <package-name>` deletes the stowed package.

If unspecified, <stow-directory> is `.` and <target-directory> is `..`.

# Future Plans
* Use org-mode + org-babel to create literate config files.
* Create an automated Shell+Guix installation process.

