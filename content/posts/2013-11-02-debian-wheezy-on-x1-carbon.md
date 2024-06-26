---
author: Carlo Hamalainen

date: "2013-11-02T00:00:00Z"
format: image
title: Debian Wheezy on X1 Carbon
url: /2013/11/02/debian-wheezy-on-x1-carbon/
---
This would be a chef/puppet recipe if I had to do this kind of installation  more frequently. 

Kernel configuration: <https://github.com/carlohamalainen/dotfiles/blob/master/config-3.11.6-for-x1-carbon> 

Installation notes: <https://github.com/carlohamalainen/dotfiles/blob/master/x1-carbon-debian-wheezy-install.txt>. The copy on github will be the latest, not this blog post. 

```
# Notes for building Debian Wheezy from scratch
# on my Lenovo Carbon X1 ultrabook.

# Put a copy of the iwlwifi package on a USB stick
# so that the installation program can get onto the
# network: http://packages.debian.org/wheezy/firmware-iwlwifi

# Do a normal install using the amd64 DVD-1 ISO image, copied
# to a USB stick using dd. Set the boot methods in the BIOS
# to "legacy only". Beware that changing this option can break
# an existing installation!

################################################################################

echo 'source $HOME/work/bin/bash-aliases-x1' >> $HOME/.bashrc

# edit /etc/apt/sources.list as follows:

deb http://ftp.se.debian.org/debian stable main contrib non-free
deb http://ftp.debian.org/debian/ wheezy-updates main contrib non-free
deb http://security.debian.org/ wheezy/updates main contrib non-free
deb http://download.virtualbox.org/virtualbox/debian wheezy contrib

# Virtualbox key.
wget -q http://download.virtualbox.org/virtualbox/debian/oracle_vbox.asc -O- | sudo apt-key add -

# Commonly used packages.
sudo apt-get update
sudo apt-get install unison2.32.52-gtk meld vim-gtk build-essential git colordiff ghc rsync sshfs xinput virtualbox-4.3 ipython python-nose python-networkx python-setuptools python-numpy python-scipy graphviz ecryptfs-utils cryptsetup pavucontrol screen durep baobab keepassx xinput htop transmission-gtk gnupg vlc mplayer gstreamer0.10-plugins-base gstreamer0.10-plugins-good gstreamer0.10-fluendo-mp3 gstreamer0.10-ffmpeg calibre conky-all gqview libjpeg-progs tcsh inkscape

sudo adduser carlo vboxusers

sudo apt-get install apt-file
sudo apt-file update

# Encrypt home directory if installer didn't do so.
ecryptfs-migrate-home -u carlo

# Google chrome.
sudo dpkg -i Downloads/google-chrome-stable_current_amd64.deb

# Skype.
sudo dpkg --add-architecture i386
sudo apt-get update
sudo dpkg -i Downloads/skype-debian_4.2.0.11-1_i386.deb
sudo apt-get -f install

# Skype headset configuration. Start a test call, set
# recording and output of the Skype application (in pavucontrol)
# to the Plantronics headset.
pavucontrol &
skype &

# Thunderbird binary tarball
sudo apt-get install ia32-libs-gtk # otherwise errors about xul library
~/opt/thunderbird-24.1.0/thunderbird

# Why I can't change this setting, I have no idea:
#
#     $ grep firefox .thunderbird/l3fwjt8m.default/*js
#     user_pref("network.protocol-handler.app.ftp", "/opt/firefox/firefox");
#     user_pref("network.protocol-handler.app.http", "/opt/firefox/firefox");
#     user_pref("network.protocol-handler.app.https", "/opt/firefox/firefox");
#
# so here's an ugly hack:

sudo mkdir -p /opt/firefox
sudo ln -s /usr/bin/google-chrome /opt/firefox/firefox

# Dropbox
sudo dpkg -i d/dropbox_1.6.0_amd64.deb

# GHC and Haskell Platform:

sudo apt-get install ghc libncurses5 libncurses5-dev
cd ~/opt
rm -fr ghc-7.6.3_build/*
cd ghc-7.6.3
make clean
./configure --prefix=$HOME/opt/ghc-7.6.3_build
make && make install
echo 'export PATH=$HOME/opt/ghc-7.6.3_build/bin:$PATH' >> $HOME/.bashrc
# Get a new shell or set the $PATH manually.
cd ..

sudo apt-get install libgl1-mesa-dev                    
                     libglc-dev                         
                     freeglut3-dev                      
                     libedit-dev                        
                     libglw1-mesa libglw1-mesa-dev
sudo apt-get install happy alex
sudo rm -fr haskell-platform-2013.2.0.0_build/*
cd haskell-platform-2013.2.0.0
make clean
./configure --prefix=$HOME/opt/haskell-platform-2013.2.0.0_build/
make && make install
echo 'export PATH=$HOME/opt/haskell-platform-2013.2.0.0_build/bin:$PATH' >> $HOME/.bashrc
# get a new shell or set the $PATH manually

# Cabal:
cabal update
cabal install cabal-install
echo 'export PATH=$HOME/.cabal/bin:$PATH' >> ~/.bashrc
# get a new shell...

# XMonad
sudo apt-get install libxrandr-dev trayer
cabal update
cabal install xmonad xmonad-contrib xmonad-extras xmonad-utils

# Edit /usr/share/xsessions/xmonadcarlo.desktop

[Desktop Entry]
Encoding=UTF-8
Name=xmonadcarlo
Comment=This session starts xmonad
Exec=/home/carlo/.cabal/bin/xmonad
Type=Application

# As normal user:
xmonad --recompile

# Newer kernel:
sudo apt-get install libncurses5 libncurses5-dev kernel-package
cd ~/opt
wget -c https://www.kernel.org/pub/linux/kernel/v3.x/linux-3.11.6.tar.xz
cd /usr/src
sudo su -
tar Jxf /home/carlo/opt/linux-3.11.6.tar.xz
cd linux-3.11.6
make mrproper
make clean
wget https://raw.github.com/carlohamalainen/dotfiles/master/config-3.11.6-for-x1-carbon -O .config
make oldconfig
make-kpkg --rootcmd fakeroot --config menuconfig --initrd --us --uc -j 4 kernel_image
cd ..
dpkg -i linux-image-3.11.6_3.11.6-10.00.Custom_amd64.deb

# Reboot, check that the built-in webcam works.

#####################

## minc toolkit
# git clone FIXME
sudo apt-get install cmake cmake-curses-gui
cd minc-toolkit
rm -fr build
mkdir build
cd build
ccmake ..  # hit 'c'
           # go down to MT_BUILD_SHARED_LIBS, hit enter to turn 'ON'
           # hit 'c'
           # hit 'g'
           #
make
sudo make install


## pyminc
# git clone FIXME
cd pyminc
sudo python setup.py install

## nipype
sudo pip install nibabel
sudo pip install traits traitsui
cd nipype
sudo rm -fr build && sudo python setup.py install

# Python things

wget https://raw.github.com/pypa/pip/master/contrib/get-pip.py
sudo python get-pip.py
sudo pip install youtube-dl
```
