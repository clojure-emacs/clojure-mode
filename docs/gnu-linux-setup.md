Emacs Setup on GNU/Linux
========================

Emacs should be available via your OS's package manager.  For example,
on Debian-based distros you would typically install Emacs like so:

    sudo apt-get install emacs

We recommend you install the current stable release of Emacs, version
24. Since v24 was released only very recently, the above command may
get you v23 instead. Use `apt-cache show emacs` to see which version
it will provide.



Ubuntu-based distros
--------------------

To get Emacs version 24 on an Ubuntu-based distribution, you currently
need to do the following:

    sudo add-apt-repository ppa:cassou/emacs
    sudo apt-get update
    sudo apt-get install emacs-snapshot



Config Files and Directory
==========================

Your main Emacs config file is "~/.emacs". You may create
this file if it does not already exist.

Your main Emacs config and package directory is "~/.emacs.d".
Create this directory if it does not already exist.



Other Optional Configuration
============================

A nice monospace font to use is Inconsolata. To install
that on modern Debian-based distros:

    sudo apt-get install fonts-inconsolata

You can then select it from within Emacs by using the "Options → Set
Default Font..." menu item, and then save that setting your ~/.emacs
file by using the "Options → Save Options" menu item.
