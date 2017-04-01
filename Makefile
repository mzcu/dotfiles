install: install-xmonad install-bashrc install-vim install-readline

install-xmonad:
	rm -rf ~/.xmonad ~/.xmobarrc ~/.Xresources
	mkdir ~/.xmonad
	ln -s `pwd`/xmonad/bin ~/.xmonad/bin
	ln -s `pwd`/xmonad/xmonad.hs ~/.xmonad/
	ln -s `pwd`/xmonad/xmobarrc.hs ~/.xmobarrc
	ln -s `pwd`/xmonad/Xresources ~/.Xresources
	xrdb -merge ~/.Xresources

install-bashrc:
	`pwd`/bash/bin/install

vundle_home := ~/.vim/bundle/Vundle.vim

install-vim:
ifeq "" "$(wildcard $(vundle_home))"
	git clone https://github.com/VundleVim/Vundle.vim.git $(vundle_home)
endif
	ln -sf `pwd`/vim/vimrc ~/.vimrc

install-powerline-fonts:
	git clone https://github.com/powerline/fonts.git /tmp/powerline-fonts
	/tmp/powerline-fonts/install.sh

install-readline:
	rm ~/.inputrc
	ln -s `pwd`/readline/inputrc ~/.inputrc
