install: install-xmonad

install-xmonad:
	rm -rf ~/.xmonad ~/.xmobarrc
	mkdir ~/.xmonad
	ln -s `pwd`/xmonad/xmonad.hs ~/.xmonad/
	ln -s `pwd`/xmonad/xmobarrc.hs ~/.xmobarrc


