https://gist.github.com/samuelematias/3b122e2e8964ec6e438739e44120a791
- this is the original gist link, the following is what i stripped off version 
  that i care about, so look at the above link if the following doesn't work

1. Optionally prettify the MSYS2 console `mintty` with `~/.minttyrc` to make it more pleasing to eyes. Thanks to this awesome [theme](https://github.com/mavnn/mintty-colors-solarized)!
    - So if msys64 was installed in C, then its root path would be 'C:\msys64\home\smpl'
    and this is where the above guide is telling to paste the .bashrc and .minttyrc config
    - so paste the following in the .minttyrc file whose full path is basically at 'C:\msys64\home\smpl\.minttyrc'

  ```sh
  ForegroundColour=131,148,150
  BackgroundColour=0,43,54
  CursorColour=192,192,192
  Black=7,54,66
  BoldBlack=0,43,54
  Red=220,50,47
  BoldRed=203,75,22
  Green=133,153,0
  BoldGreen=88,110,117
  Yellow=181,137,0
  BoldYellow=101,123,131
  Blue=38,139,210
  BoldBlue=131,148,150
  Magenta=211,54,130
  BoldMagenta=108,113,196
  Cyan=42,161,152
  BoldCyan=147,161,161
  White=238,232,213
  BoldWhite=253,246,227
  BoldAsFont=yes
  Font=Consolas
  FontHeight=12
  Transparency=off
  OpaqueWhenFocused=no
  ScrollbackLines=200000
  FontSmoothing=full
  CursorType=underscore
  CursorBlinks=yes
  ScrollMod=off
  RightClickAction=paste
  ClickTargetMod=off
  
  ```

2. Paste the following in the .bashrc file

  ```sh
  # .bashrc
   
  # Source global definitions
  if [ -f /etc/bashrc ]; then
	  . /etc/bashrc
  fi
   
  ##############
  ### Basics ###
  ##############
   
  # Don't wait for job termination notification
  set -o notify
   
  # Enables UTF-8 in Putty.
  # See http://www.earth.li/~huggie/blog/tech/mobile/putty-utf-8-trick.html
  echo -ne '\e%G\e[?47h\e%G\e[?47l'
   
  # My pretty prompt (Yay!)
  PS1='\[\e[32;40m\]\u\[\e[0m\]\[\e[34;40m\]\H\[\e[0m\]\[\e[40;1m\]\w\[\e[0m\] '
   
  ############
  ### PATH ###
  ############
   
  # Remove '/c/*' from PATH if running under Msys to avoid possible 
  # interference from programs already installed on system. Removal 
  # with awk is copied from http://stackoverflow.com/a/370192.
  if [ $(uname -o) == 'Msys' ]; then
	  export PATH=`echo ${PATH} | awk -v RS=: -v ORS=: '/c\// {next} {print}' | sed 's/:*$//'`
  fi
   
  ###############
  ### Aliases ###
  ###############
   
   
  alias less='less -r'
  alias rm='rm -i'
  alias whence='type -a'
  alias ls='ls -F --color=auto'
  alias dir='ls --color=auto --format=long -L'
  alias vdir='ls --color=auto --format=long'
  alias ll='ls -l'
  alias la='ls -A'
  alias l='ls -CF'
  alias md='mkdir'
  alias pico='nano'
  alias cls='clear'
  
  ```

3. Install packages required to build Emacs.

  Follow instructions on [this page](http://sourceforge.net/p/msys2/wiki/MSYS2%20installation/) first.

  ```sh
  pacman -Syu
  pacman -Sy
  pacman -Sy\
      --needed \
      filesystem \
      msys2-runtime \
      bash \
      libreadline \
      libiconv \
      libarchive \
      libgpgme \
      libcurl \
      pacman \
      ncurses \
      libintl
      
  ```

  Close mintty and restart it, do this again.

  ```sh
  pacman -Su
  
  ```

  Then let install all required libs for building Emacs.

  ```sh
pacman -Su \
      autoconf \
      autogen \
      automake \
      automake-wrapper \
      diffutils \
      git \
      guile \
      libgc \
      libguile \
      libidn-devel \
      libltdl \
      libnettle-devel \
      libopenssl \
      libp11-kit-devel \
      libtasn1-devel \
      libunistring \
      make \
      mingw-w64-x86_64-binutils \
      mingw-w64-x86_64-bzip2 \
      mingw-w64-x86_64-cairo \
      mingw-w64-x86_64-crt-git \
      mingw-w64-x86_64-dbus \
      mingw-w64-x86_64-expat \
      mingw-w64-x86_64-fontconfig \
      mingw-w64-x86_64-freetype \
      mingw-w64-x86_64-gcc \
      mingw-w64-x86_64-gcc-libs \
      mingw-w64-x86_64-gdk-pixbuf2 \
      mingw-w64-x86_64-gettext \
      mingw-w64-x86_64-giflib \
      mingw-w64-x86_64-glib2 \
      mingw-w64-x86_64-gmp \
      mingw-w64-x86_64-gnutls \
      mingw-w64-x86_64-harfbuzz \
      mingw-w64-x86_64-headers-git \
      mingw-w64-x86_64-imagemagick \
      mingw-w64-x86_64-isl \
      mingw-w64-x86_64-jansson \
      mingw-w64-x86_64-libffi \
      mingw-w64-x86_64-libgccjit \
      mingw-w64-x86_64-libiconv \
      mingw-w64-x86_64-libidn2 \
      mingw-w64-x86_64-libjpeg-turbo \
      mingw-w64-x86_64-libpng \
      mingw-w64-x86_64-librsvg \
      mingw-w64-x86_64-libsystre \
      mingw-w64-x86_64-libtasn1 \
      mingw-w64-x86_64-libtiff \
      mingw-w64-x86_64-libunistring \
      mingw-w64-x86_64-libwinpthread-git \
      mingw-w64-x86_64-libxml2 \
      mingw-w64-x86_64-mpc \
      mingw-w64-x86_64-mpfr \
      mingw-w64-x86_64-nettle \
      mingw-w64-x86_64-p11-kit \
      mingw-w64-x86_64-pango \
      mingw-w64-x86_64-pixman \
      mingw-w64-x86_64-winpthreads \
      mingw-w64-x86_64-xpm-nox \
      mingw-w64-x86_64-xz \
      mingw-w64-x86_64-zlib \
      mingw-w64-x86_64-jbigkit \
      nano \
      openssl \
      pkgconf \
      tar \
      texinfo \
      wget
  ```

