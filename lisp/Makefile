# Copyright (C) 2013 Tom Willemsen <tom at ryuslash dot org>
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
LISP ?= sbcl --disable-debugger

binaries = clark
install-binaries = $(addprefix install-,$(binaries))
uninstall-binaries = $(addprefix uninstall-,$(binaries))

.PHONY: all install $(install-binaries) uninstall $(uninstall-binaries) \
	install-strip

all: $(binaries)
install: $(install-binaries)
uninstall: $(uninstall-binaries)
install-strip:
	$(MAKE) INSTALL_PROGRAM='$(INSTALL_PROGRAM) -s' install

$(install-binaries): install-%: %
	$(INSTALL_PROGRAM) $* "$(DESTDIR)/bin/$*"

$(uninstall-binaries): uninstall-%:
	rm -f "$(DESTDIR)/bin/$*"

clark: clark.lisp queries.lisp make-image.lisp
	$(LISP) --load make-image.lisp
