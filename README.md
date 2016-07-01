mucel - minimalist multicellular simulator
==================================
These instructions were tested on Debian Jessie, but should work on any GNU-Linux distro.
Just replace "sudo apt-get" by your distro equivalent command and change the respective package names when needed.

Install local stack
--------------------------------
```bash
echo "export PATH=$PATH:$HOME/.local/bin" >> $HOME/.bashrc
sudo apt-get install curl
curl -sSL https://get.haskellstack.org/ | sh
source $HOME/.bashrc
```

Alternatively, install system-wide stack.
```bash
sudo echo "deb http://download.fpcomplete.com/debian jessie main" >> /etc/apt/sources.list
sudo apt-get update
sudo apt-get install stack
```

Install mucel
--------------------------
Install git and OpenGL when needed.
```bash
sudo apt-get install git freeglut3 freeglut3-dev
```

Build mucel (this can take 5 or more minutes depending on your internet connection and hardware).
Stack avoids to interfere with your system, it also needs more recent packages than usually supplied by distros,
so the Haskell compiler and libraries will be downloaded locally.
```bash
git clone https://github.com/davips/mucel.git
cd mucel
stack setup
stack build
```

Run
---
```bash
stack exec mucel
```

Features / To do list
-----
1. [x] 2D particle collision simulator
2. [x] visualize according to real elapsed time
3. [x] exact preemptory collision calculation
4. [ ] rigid bodies
5. [ ] rotation
6. [ ] different types of cells
*    [x] structure
*    [ ] sensor
*    [ ] bulb
*    [ ] motor
*    [ ] wire
7. [ ] reproduction
8. [ ] mobile version
9. [ ] distributed version
10. [ ] friction?
11. [ ] 3D version?
