OS := $(shell uname -s)

ifeq ($(OS),Darwin)
MACOS := true
endif

ifeq ($(OS),Linux)
LINUX := true
endif

install: install-xmonad install-bashrc install-vim install-ideavim install-readline install-tmux

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

install-ideavim:
	$(info installing idea vim plugin config)
	rm -f ~/.ideavimrc
	ln -s `pwd`/ideavimrc ~/.ideavimrc

install-powerline-fonts:
	$(info installing powerline fonts)
	git clone https://github.com/powerline/fonts.git /tmp/powerline-fonts
	/tmp/powerline-fonts/install.sh

install-readline:
	$(info installing readline config)
	rm -f ~/.inputrc
	ln -s `pwd`/readline/inputrc ~/.inputrc

install-tmux:
	$(info installing tmux config)
	rm -f ~/.tmux.conf
	ln -s `pwd`/tmux/tmux.conf ~/.tmux.conf

DOOM_CONFIG := ~/.doom.d/config.el
LOAD_EXPORT_DEFS := (after! (org-roam) (load! "mc-export-defs"))

install-emacs:
ifeq "" "$(wildcard $(DOOM_CONFIG))"
	$(error no $(DOOM_CONFIG) present, can't add custom emacs functions)
endif
	$(info installing emacs config)
	rm -f ~/.doom.d/mc-export-defs.el
	ln -s `pwd`/emacs/mc-export-defs.el ~/.doom.d/mc-export-defs.el
	grep -qxF '$(LOAD_EXPORT_DEFS)' $(DOOM_CONFIG) || echo '$(LOAD_EXPORT_DEFS)' >> $(DOOM_CONFIG)

