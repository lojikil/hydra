## Overview
Testing repo for Eprime & hydra. All changes will eventually make their way back to 
the digamma main repo.

## Files
- Eprime.ss: the original PreDigamma compiler. Abandoned because it was really messy & difficult to update.
- enyalios.ss: the second PreDigamma compiler, clean, modular, outputs human-readable C (though it uses an IR and can output whatever).
- hydra.ss: the Hydra VM, a Digamma VM system. This fork contains fixes & is tuned to be compilable by enyalios.
- test\*.ss: test cases for Enyalios.
- t[0-9A-Z].ss: test drivers for Enyalios (different from the above in that the above are run _through_ Enyalios, where as these _drive_ it).
- compile.ss: a driver for Enyalios/Eprime; ./compile.ss file [(+/-)e] <output-file> <init>.
- mutual\_test.ss: another Eprime/Enyalios test case.
- test\_enyalios.ss: original test driver for enyalios.
- test\_hydra.ss: defines a simple test harness to check Hydra\'s VM compiler & execution.

## License

Unless otherwise specified, all code in this repository is released under the following license (ISC):

Copyright (c) 2006-2012, Stefan Edwards <saedwards.ecc@gmail.com>

Permission to use, copy, modify, and/or distribute this software for any purpose with or without fee is hereby
granted, provided that the above copyright notice and this permission notice appear in all copies.

THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES WITH REGARD TO THIS SOFTWARE INCLUDING
ALL IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY SPECIAL,
DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS,
WHETHER IN AN ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF OR IN CONNECTION WITH THE
USE OR PERFORMANCE OF THIS SOFTWARE
