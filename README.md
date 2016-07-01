# mucel
minimalist multicellular simulator

This inbstructions were tested on Debian Jessie, but should work on any GNU-Linux distro.
Just replace "sudo apt-get" by your distro equivalent command and change the respective package names when needed.

Download mucel.
```bash
sudo apt-get install git 
git clone https://github.com/davips/mucel.git
```

Download and install local stack.
```bash
sudo apt-get install curl
curl -sSL https://get.haskellstack.org/ | sh
echo "export PATH=$PATH:$HOME/.local/bin" >> $HOME/.bashrc
source $HOME/.bashrc
```

Alternatively, download and install system-wide stack.
```bash
sudo echo "deb http://download.fpcomplete.com/debian jessie main" >> /etc/apt/sources.list
sudo apt-get update
sudo apt-get install stack
```

Setup stack (this can take 5min. depending on your internet connection).
Stack avoids to interfere with your system, it also needs more recent packages than distros usually supply,
so the Haskell compiler and libraries will be downloaded locally.
```bash
cd mucel
stack setup
```
Install necessary OpenGL libraries and build mucel (this can take 15min. depending on your internet connection and hardware).
```bash
sudo apt-get install freeglut3 freeglut3-dev
stack build
```