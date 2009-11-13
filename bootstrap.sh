#!/bin/sh
# evol - bootstrap evolution
# Copyright (C) 2009  Alexander Kahl <e-user@fsfe.org>
# This file is part of evol.
# evol is free software; you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation; either version 3 of the License, or
# (at your option) any later version.
# 
# evol is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
# 
# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <http://www.gnu.org/licenses/>.

cwd=$(dirname $0)
cd $cwd

test -f evol && rm -f evol

sbcl --noinform --disable-debugger --no-userinit \
     --eval "(require 'asdf)" \
     --eval "(push \"${cwd}/\" asdf:*central-registry*)" \
     --eval "(push \"${HOME}/local/asdf/\" asdf:*central-registry*)" \
     --eval "(require 'evol)" \
     --eval "(evol:repl)"

test -f evol || { echo "building failed"; exit 1; }

mv evol{,~}
./evol~
rm evol~

ls -l evol
