# Copyright (C) 2013 Tom Willemsen
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
INSTALL_INFO = install-info
TEXI2PDF = texi2pdf
DVIPS = dvips

.PHONY: all html dvi pdf ps install uninstall install-dvi install-pdf \
		install-ps

all: clark.info

install: clark.info
	$(INSTALL_DATA) clark.info "$(DESTDIR)/share/info/clark.info"
	$(INSTALL_INFO) --dir-file="$(DESTDIR)/share/info/dir" \
		"$(DESTDIR)/share/info/clark.info"

install-strip:
	$(MAKE) install

uninstall:
	$(INSTALL_INFO) --dir-file="$(DESTDIR)/share/info/dir" \
		--delete "$(DESTDIR)/share/info/clark.info"
	rm -f "$(DESTDIR)/share/info/clark.info"
	rm -f "$(DESTDIR)/share/doc/clark/dvi/clark.dvi"
	rm -f "$(DESTDIR)/share/doc/clark/pdf/clark.pdf"
	rm -f "$(DESTDIR)/share/doc/clark/ps/clark.ps"
	rm -rf "$(DESTDIR)/share/doc/clark/html/"

install-dvi: clark.dvi
	$(INSTALL_DATA) clark.dvi \
		"$(DESTDIR)/share/doc/clark/dvi/clark.dvi"

install-pdf: clark.pdf
	$(INSTALL_DATA) clark.pdf \
		"$(DESTDIR)/share/doc/clark/pdf/clark.pdf"

install-ps: clark.ps
	$(INSTALL_DATA) clark.ps \
		"$(DESTDIR)/share/doc/clark/ps/clark.ps"

clark.info: clark.texi

html: clark.texi
	$(MAKEINFO) --html clark.texi -o html

dvi: clark.dvi
pdf: clark.pdf
ps: clark.ps

clark.dvi: clark.texi
	$(TEXI2DVI) clark.texi

clark.pdf: clark.texi
	$(TEXI2PDF) clark.texi

clark.ps: clark.dvi
	$(DVIPS) clark.dvi
