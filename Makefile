install: install-xmonad

install-xmonad:
	rm -rf ~/.xmonad ~/.xmobarrc ~/.Xresources
	mkdir ~/.xmonad
	ln -s `pwd`/xmonad/xmonad.hs ~/.xmonad/
	ln -s `pwd`/xmonad/xmobarrc.hs ~/.xmobarrc
	ln -s `pwd`/xmonad/Xresources ~/.Xresources
	xrdb -merge ~/.Xresources



