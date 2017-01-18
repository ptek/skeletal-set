# Setoid

This is a Haskell implementation of setoid
(https://en.wikipedia.org/wiki/Setoid) - a set equipped with an
equivalence relation. Usually equivalence is not equality. This makes
it more strict regarding membership of elements as compared to set. If
equivalence relation is equality then setoid has the same properties
as a set.

## Usage examples

When manipulating collections of objects in the real world, we often
use lists/arrays. Sometimes we need to represent some properties of
the relation between the elements though, and the lists do not provide
such possibility. The library not only provides the guarantee that a
setoid is correct by construction, but also that any manipulations
will not change its structure.

It is useful for things like time series of sampling data, collections
of users (who are unique by username or email) to keep the same
structure as the one which would be used in the database with unique
indexes.

### Apples and Oranges

To start with a simple example we will combine apples and oranges to a
Setoid of fruit names by color. We want one fruit per colour as a
result and don't care if its apple or an orange.

    import Data.Setoid (Setoid)
    import qualified Data.Setoid as Setoid

    data Colour = Red | Green | Blue deriving (Eq,Ord)

    instance EquivalenceBy Colour (Colour,String) where
      eqRel = fst

    apples, organges, fruits :: Setoid Int (Int,String)
    apples  = Setoid.fromList [(Green,"golden delicious"), (Orange,"honeycrunch")]
    oranges = Setoid.fromList [(Orange,"seville"), (Red,"blood orange")]

    fruits = apples `Setoid.union` oranges
    -- > [(Green,"golden delicious"),(Orange,"seville"),(Red,"blood orange")]

Of course one could use a list and then just `nubBy` but one would
have to do this every time the data is transformed. The default
implementation of deciding between two equivalent elements is taking
the maximum of both. Here the ordering of `"seville"` is bigger than
`"golden delicious"` thus `("Orange", "seville")` is chosen in the
result.

### Friends of friends and computation on union

Imagine that we want to get all the users of two different services F
and G. We are not interested in the different details, but want the
instance of the users to be unique.

    type Email = String
    data User = User {
      email :: Email,
      contacts :: Maybe Int,
      friends :: Maybe Int
      } deriving (Eq,Show)

    instance EquivalenceBy Email User where
	  eqRel u = email u

    usersF, usersG, allUsers :: Setoid Email User
    usersF <- getUsers F
    usersG <- getUsers G

    allUsers = Setoid.unionWith mergeContactDetails usersF usersG

    mergeContactDetails :: User -> User -> User
    ...

We assume that here are equivalent elements in both setoids - in this
case they have the same email adress. Thus we use `Setoid.unionWith`
to merge the other details of the contact. We could simply take the
maximum, as the default implementation, or the left element only. We
could also do computations and, for example, sum the number of
friends/contacts from bothe services.

    Setoid.size allUsers

Would give us the amount of all unique users in both services together.


## Peculiar details

### Union

Since `Setoid.union` is implemented as `Setoid.unionWith max` it makes the default union operation commutative and associative.

### Tuple Instance

TupleSetoid already has the instance we would need in the Example 1:

    instance EquivalenceBy eq (eq,val) where
      eqRel = fst

## Future Work

- There is an unproven hypothesis about a relation between setoids and
  Quotient Sets. It seems, that a `Setoid (a,b) (a,b,c)` is equivalent
  to a `QuotientSet a (Setoid b (a,b,c))`. This means that every
  QuotientSet can actually be represented as a setoid.

- Performance is another issue. Current implementation uses the
  `newtype Setoid x y = Setoid (Map x y)` which may be inefficient.

## Authors and Credits

We needed a data structure for time series which would provide strong
guarantees on insertion of an element. Originally we had an idea to
use a Set with a customized Monoid property which would allow to make
a decision if a newly added element already exists in the Set we want
to add it to. This was called a Monoid Set but the implementation
allowed for mistakes in its usage.

After more research, Irek Jozwiak came up with an idea to use
[setoids](https://en.wikipedia.org/wiki/Setoid) for this task. The
implementation by Pavlo Kerestey met all the properties we needed, and
we heavily rely on it in our production code up to date. Simon Zelazny
was indispensable in both research and implementation for his insights
and careful and precise judgements.

Hope this will find more contributors and users now.

### Authors:
*  [Pavlo Kerestey](https://github.com/ptek)
*  [Irek Jozwiak](https://github.com/irekjozwiak)
*  [Simon Zelazny](https://github.com/pzel)

### Copyright: [Global Access Internet Services GmbH](http://www.global.de)

## License

```
Copyright (c) 2017, Global Access Internet Services GmbH

All rights reserved.

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are met:

    * Redistributions of source code must retain the above copyright
      notice, this list of conditions and the following disclaimer.

    * Redistributions in binary form must reproduce the above
      copyright notice, this list of conditions and the following
      disclaimer in the documentation and/or other materials provided
      with the distribution.

    * Neither the name of Pavlo Kerestey nor the names of other
      contributors may be used to endorse or promote products derived
      from this software without specific prior written permission.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
"AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
(INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
```