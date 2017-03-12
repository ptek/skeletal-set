# Setoid

A Haskell implementation of [setoid](https://en.wikipedia.org/wiki/Setoid) - a
set equipped with an equivalence relation. Setoid is a useful data structure in
cases where equivalence is chosen not to be equality. One can use it to
influence the memberships of the elements more strictly, as opposed to sets, and
run computations on unions when conflicts between elements are found.

For more in-depth explanations and examples, please have a look at the
documentation on hackage.

### Copyright:
* [Global Access Internet Services GmbH](http://www.global.de)

### Authors:
* [Pavlo Kerestey](https://github.com/ptek)
* [Simon Zelazny](https://github.com/pzel)
* [Irek Jozwiak](https://github.com/irekjozwiak) - Author of the predecessor
  implementation of sets wich stricter guarantees and the idea of Setoid in
  Haskell.

## License

```text
    Copyright (c) 2017, Global Access Internet Services GmbH
    
    All rights reserved.
    
    Redistribution and use in source and binary forms, with or without
    modification, are permitted provided that the following conditions are met:
    
        * Redistributions of source code must retain the above copyright notice,
          this list of conditions and the following disclaimer.
    
        * Redistributions in binary form must reproduce the above copyright
          notice, this list of conditions and the following disclaimer in the
          documentation and/or other materials provided with the distribution.
    
        * Neither the name of Global Access Internet Services GmbH nor the names
          of other contributors may be used to endorse or promote products
          derived from this software without specific prior written permission.
    
    THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
    AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
    IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
    ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR CONTRIBUTORS BE
    LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
    CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
    SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
    INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
    CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
    ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
    POSSIBILITY OF SUCH DAMAGE.
```
