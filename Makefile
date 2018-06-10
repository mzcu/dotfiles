OS := $(shell uname -s)

ifeq ($(OS),Darwin)
MACOS := true
endif

ifeq ($(OS),Linux)
LINUX := true
endif

install: install-xmonad install-bashrc install-vim install-readline install-tmux

install-xmonad:
ifdef LINUX:
	$(info installing xmonad config)
	rm -rf ~/.xmonad ~/.xmobarrc ~/.Xresources
	mkdir ~/.xmonad
	ln -s `pwd`/xmonad/bin ~/.xmonad/bin
	ln -s `pwd`/xmonad/xmonad.hs ~/.xmonad/
	ln -s `pwd`/xmonad/xmobarrc.hs ~/.xmobarrc
	ln -s `pwd`/xmonad/Xresources ~/.Xresources
	xrdb -merge ~/.Xresources
endif

install-bashrc:
ifdef LINUX
	$(info installing linux bash config)
	`pwd`/bash/bin/install
endif

vundle_home := ~/.vim/bundle/Vundle.vim

install-vim:
	$(info installing vim settings)
ifeq "" "$(wildcard $(vundle_home))"
	git clone https://github.com/VundleVim/Vundle.vim.git $(vundle_home)
endif
	ln -sf `pwd`/vim/vimrc ~/.vimrc
	vim +PluginInstall +qall

install-powerline-fonts:
ifdef LINUX
	$(info installing linux powerline fonts)
	git clone https://github.com/powerline/fonts.git /tmp/powerline-fonts
	/tmp/powerline-fonts/install.sh
endif

install-readline:
	$(info installing readline config)
	rm -f ~/.inputrc
	ln -s `pwd`/readline/inputrc ~/.inputrc

install-tmux:
	$(info installing tmux config)
	rm -f ~/.tmux.conf
	ln -s `pwd`/tmux/tmux.conf ~/.tmux.conf
