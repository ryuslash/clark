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
INSTALL = install -D

export DESTDIR ?= /usr/local
export INSTALL_PROGRAM = $(INSTALL)
export INSTALL_DATA = $(INSTALL) -m 644

dirs = lisp doc
install-dirs = $(addprefix install-,$(dirs))
uninstall-dirs = $(addprefix uninstall-,$(dirs))
install-strip-dirs = $(addprefix installstrip-,$(dirs))

.PHONY: all $(dirs) install $(install-dirs) uninstall $(uninstall-dirs) \
	dvi install-dvi pdf install-pdf ps install-ps

all: $(dirs)
install: $(install-dirs)
uninstall: $(uninstall-dirs)
install-strip: $(install-strip-dirs)

$(dirs):
	$(MAKE) -C $@

$(install-dirs): install-%:
	$(MAKE) -C $*/ install

$(uninstall-dirs): uninstall-%:
	$(MAKE) -C $*/ uninstall

$(install-strip-dirs): installstrip-%:
	$(MAKE) -C $*/ install-strip

dvi:
	$(MAKE) -C doc/ dvi

pdf:
	$(MAKE) -C doc/ pdf

ps:
	$(MAKE) -C doc/ ps

install-dvi:
	$(MAKE) -C doc/ install-dvi

install-pdf:
	$(MAKE) -C doc/ install-pdf

install-ps:
	$(MAKE) -C doc/ install-ps
