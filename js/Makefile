# Copyright (C) 2012, 2013 Tom Willemsen
#
#   This file is part of CLark
#
#   CLark is free software: you can redistribute it and/or modify it
#   under the terms of the GNU General Public License as published by
#   the Free Software Foundation, either version 3 of the License, or
#   (at your option) any later version.
#
#   CLark is distributed in the hope that it will be useful, but
#   WITHOUT ANY WARRANTY; without even the implied warranty of
#   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
#   General Public License for more details.
#
#   You should have received a copy of the GNU General Public License
#   along with CLark. If not, see <http://www.gnu.org/licenses/>.
DESTDIR ?= /usr/local

.PHONY: all install uninstall

all:
install: clark.js
	$(INSTALL_DATA) clark.js \
		"$(DESTDIR)/share/conkeror/modules/clark.js"

install-strip:
	$(MAKE) install

uninstall:
	rm -f "$(DESTDIR)/share/conkeror/modules/clark.js"
