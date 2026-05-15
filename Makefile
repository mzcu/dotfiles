OS := $(shell uname -s)

ifeq ($(OS),Darwin)
MACOS := true
endif

ifeq ($(OS),Linux)
LINUX := true
endif

install: install-xmonad install-bashrc install-vim install-ideavim install-readline install-tmux

XMONAD_HOME := ~/.config/xmonad

install-xmonad:
ifdef LINUX
	$(info installing xmonad config)
	mkdir -p $(XMONAD_HOME)
	rm $(XMONAD_HOME)/*.hs ~/.xmobarrc ~/.Xresources
	ln -s `pwd`/xmonad/bin $(XMONAD_HOME)/bin
	ln -s `pwd`/xmonad/xmonad.hs $(XMONAD_HOME)
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
	rm -f ~/.config/ideavim/ideavimrc
	ln -s `pwd`/ideavimrc ~/.config/ideavim/ideavimrc

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
	rm -f ~/.config/tmux/tmux.conf
	mkdir ~/.config/tmux
	ln -s `pwd`/tmux/tmux.conf ~/.config/tmux/tmux.conf

setup-mac: setup-mac-screenshots

setup-mac-screenshots:
ifdef MACOS
	$(info configuring screenshots directory)
	mkdir -p ~/Pictures/Screenshots
	defaults write com.apple.screencapture location ~/Pictures/Screenshots
	killall SystemUIServer
	defaults write com.apple.dock persistent-others -array-add \
		'<dict><key>tile-data</key><dict><key>file-data</key><dict><key>_CFURLString</key><string>$(HOME)/Pictures/Screenshots</string><key>_CFURLStringType</key><integer>0</integer></dict></dict><key>tile-type</key><string>directory-tile</string></dict>'
	killall Dock
endif

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
