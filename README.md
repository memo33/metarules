 MetaRules
===========

A meta language for RUL2 code, implemented as internal DSL in Scala.


 Getting Started
-----------------

Download the project using `git clone` and, if you plan to make any changes,
create a new branch, e.g. `git checkout -b feature-a`. You will also need to
install `sbt`.

Some important `sbt` commands are:
- `run` to execute the Main class to generate all the RUL2 code and write it
  to `target/output.txt`.
- `doc` to generate the Scaladocs. Open them at `target/scala-2.11/api/index.html`.
  Not everything is documented in the Scaladocs, but they help to get an
  overview.
- `test` to run the test suites.
- `compile`
- `clean`
- `console`, for an interactive console. This is very handy for trying out
  certain functionality.


 The Meta Syntax
-----------------

Every `Rule` consists of four `Tiles`. Each `Tile` consists of a set of
`Segments`. Each `Segment` consists of a `Network` and (four) `Flags`.

A network and a tuple of flags are joined with `~` to form a `Segment`, for
example `Mis~(-2,0,+2,0)`. A number of common flags can be imported from
`Flags._`, so the previous `Segment` would be the same as `Mis~WE`.

Mutliple segments are joined with `&` to form a `Tile`. The four tiles are
separated via `|`. A complete `Rule` would therefore look like:

    Mis~WE | Dirtroad~WE & Rail~NS | Mis~WE | Mis~WE & Rail~NS

There is a special syntax to reuse previous tiles in the third or fourth
position using `%`:

    Mis~WE | Dirtroad~WE & Rail~NS | % | Mis~WE & Rail~NS

There is an even shorter syntax that exchanges only specific networks in
segments (which is a common operation) using `~>` between two networks:

    Mis~WE | (Dirtroad ~> Mis)~WE & Rail~NS

All three of these Rules are equivalent. Moreover, there is a syntax for
flag variations (also a common operation):

    Mis~SE~ES | (Dirtroad ~> Mis)~WN~NW & Rail~NS

is shorthand for

    Mis~SE | (Dirtroad ~> Mis)~WN & Rail~NS
    Mis~ES | (Dirtroad ~> Mis)~NW & Rail~NS

which is about as compact as it can get, using the current syntax.


 Looking up IIDs
-----------------

If you launch the `sbt console` in the repository
https://github.com/NAMTeam/Network-Addon-Mod, you can interactively look up
IIDs for the IID schemes that have already been implemented there.
For example:

    scala> resolve.lift(Rhw4~NS)
    res0: Option[metarules.meta.IdTile] = Some(0x57030000,0,0)

    scala> resolve.lift(L1Mis~NS & Rail~NW)
    res1: Option[metarules.meta.IdTile] = Some(0x57124500,2,1)

    scala> resolve.lift(L1Rhw6c~ES & Rhw6cm~EN)
    res2: Option[metarules.meta.IdTile] = Some(0x5718B340,1,1)


 Adding A Custom Code Generator
--------------------------------

To implement a custom RUL code generator, you need to implement two
interfaces:

- `IdResolver` which is simply a (partial) function mapping from `Tile` to
  `IdTile`, i.e. it maps tiles from meta syntax to their respective
  IID,Rot,Flip representation.
- `RuleGenerator` which creates all the meta rules.

If you extend `RuleGenerator`, you need to implement the `start` method.
There you would create all the rules; you add them to an internal buffer
called `Rules` like this:

    Rules += Mis~SE~ES | (Dirtroad ~> Mis)~WN~NW & Rail~NS

Call `createRules()` in the end or in between to flush the buffer. The rules
in the buffer will then be translated to RUL2 code (using `RuleTransducer`)
and duplicates in the buffer will be removed.

Note that you do not need to add rules that are equivalent up to symmetries
(based on the flags of the tiles). For example, the two rules

    Rules += Mis~WE | (Dirtroad ~> Mis)~WE & Rail~NS
    Rules += Mis~EW | (Dirtroad ~> Mis)~EW & Rail~NS

are equivalent up to symmetries (they look the same if you flip everything
along the horizontal axis) and would therefore produce the exact same
output, so you only need to add one of them. (Check for example the
`transduce` function in the `console`.)


 A Complete Mini-Example
-------------------------

Open the Main class [src/main/scala/module/Main.scala](https://github.com/NAMTeam/Network-Addon-Mod/blob/master/src/main/scala/module/Main.scala) and replace the
definition of `resolve` and `generator` by the following chunk of code:

```
  import meta._, Network._, RotFlip._, Flags._, Implicits._

  val resolve = Map[Tile, IdTile](
    (Dirtroad~NS, IdTile(0x57000000, R0F0)),
    (Dirtroad~EW, IdTile(0x57000000, R1F0)),
    (Mis~NS,      IdTile(0x57020000, R0F0)),
    (Mis~EW,      IdTile(0x57020000, R1F0)),
    (Mis~SN,      IdTile(0x57020000, R2F0)),
    (Mis~WE,      IdTile(0x57020000, R3F0)),
    (L1Rhw2~NS,   IdTile(0x57100000, R0F0)),
    (L1Rhw2~EW,   IdTile(0x57100000, R1F0)))

  val generator = new RuleGenerator {
    val resolver = resolve
    def start(): Unit = {
      Rules += Mis~WE    | (Dirtroad ~> Mis)~WE
      Rules += L1Rhw2~WE | (Dirtroad ~> L1Rhw2)~WE
      createRules()
    }
  }
```

Now run `sbt run`. It produces the following RUL2 code in
`target/output.txt` as expected:

    0x57020000,3,0,0x57000000,1,0=0x57020000,3,0,0x57020000,3,0
    0x57020000,3,0,0x57000000,3,0=0x57020000,3,0,0x57020000,3,0
    0x57020000,1,0,0x57000000,3,0=0x57020000,1,0,0x57020000,1,0
    0x57020000,1,0,0x57000000,1,0=0x57020000,1,0,0x57020000,1,0
    0x57100000,1,0,0x57000000,1,0=0x57100000,1,0,0x57100000,1,0
    0x57100000,3,0,0x57000000,3,0=0x57100000,3,0,0x57100000,3,0

Note that, when implementing `IdResolver`, you should assert that it maps to
the `RotFlip` in a consistent manner, if symmetries are involved. For
example, `Dirtroad~NS` and `Dirtroad~SN` are equivalent and the `RotFlip`
must be `0,0` (not `2,0` even if it looks the same). The general rule for
this is: Pick the first matching `RotFlip` in the sequence
(0,0) (1,0) (2,0) (3,0) (0,1) (3,1) (2,1) (1,1).

Also note that listing all the IIDs and rotations explicitly, as in this
example, is not advisable in general, but should be replaced by a more
appropriate solution.


Adding new networks and flags
-----------------------------

Any new networks need to be added to the file [networks.scala](src/main/scala/meta/networks.scala).
Make sure that the existing metarule code in the NAM repository plays well with the networks you add.

If you need additional flags, these need to be added to the file [flags.scala](src/main/scala/meta/flags.scala).
For example, apart from the standard flags, this file already contains some additional flags
for multi-radius curves and FLEX-Fly curves.


Combining metarule syntax and RUL2 syntax
-----------------------------------------

Sometimes you may want to use raw IIDs in a metarule,
like you would in ordinary RUL2 code.
For instance, this can be useful for bridging the gap to existing RUL2 code
or for any scenario that cannot be conveniently expressed in terms of flags.

Flags are used by the metarule framework to deduce the symmetries of network tiles,
which are subsequently used for determining the necessary orientations
of tiles in the generated RUL2 code.
In order to use a tile with just an IID but without refering to flags,
you need to manually specify the symmetries of that tile,
so that the metarule framework can do its thing.

Here is an example for the RHW-6S R1 curve:

    val rule = Dirtroad~(0,0,0,0) | Rhw6s~(0,-2,0,+123) | IdTile(0x57945380,1,0,noSymmetries) | %

The flags of the tile `Rhw6s~(0,-2,0,+123)` are custom flags for the R1 curve.
The tile `IdTile(0x57945380,1,0,noSymmetries)` is an extra tile that is needed for RHW-6S due to its width,
but not for the other single-tile RHW networks, so there are not any flags to express this.
The `IdTile` consists of an IID, an orientation and its symmetries -- in this case `noSymmetries`.
If you call `transduce` with the above metarule, you obtain the following RUL2 output:

    scala> transduce(rule)
    Rule( 0x57000F00,0,0 | 0x57945080,1,0 | 0x57945380,1,0 | 0x57945080,1,0 )
    Rule( 0x57000F00,1,0 | 0x57945080,1,0 | 0x57945380,1,0 | 0x57945080,1,0 )
    Rule( 0x57000F00,2,0 | 0x57945080,1,0 | 0x57945380,1,0 | 0x57945080,1,0 )
    Rule( 0x57000F00,3,0 | 0x57945080,1,0 | 0x57945380,1,0 | 0x57945080,1,0 )
    Rule( 0x57000F00,2,0 | 0x57945080,3,1 | 0x57945380,3,1 | 0x57945080,3,1 )
    Rule( 0x57000F00,1,0 | 0x57945080,3,1 | 0x57945380,3,1 | 0x57945080,3,1 )
    Rule( 0x57000F00,0,0 | 0x57945080,3,1 | 0x57945380,3,1 | 0x57945080,3,1 )
    Rule( 0x57000F00,3,0 | 0x57945080,3,1 | 0x57945380,3,1 | 0x57945080,3,1 )

All of these generated lines correspond to the above metarule,
based on the symmetries derived from the flags.

When using an `IdTile` in the metarule syntax,
make sure to choose the smallest representing orientation of the tile
that fits with its symmetries.
Otherwise, you might get an error about unreachable rotations.
(In the future, the framework might handle this automatically for you.)


Symmetries
----------

The symmetries you can pass to the `IdTile` constructor are best understood by some examples.
Let us look at the symmetries of the orthogonal Road tile:

    scala> (Road~(0,2,0,2)).symmetries
    Dih2A((0,0), (2,0), (0,1), (2,1))

This lists all the orientations of the tile that look the same as the (0,0) rotation,
namely, you can rotate it by 180 degrees (2,0) or mirror it along the vertical axis (0,1)
or along the horizontal axis (2,1) without changing its appearance.

Here is the full list of valid symmetries together with some canonical examples:

    Cyc1  == (Road~(0,2,0,11)).symmetries     // `noSymmetries` is an alias for this
    Cyc2A == (Road~(0,11,0,11)).symmetries    // 180 degree rotational symmetry
    Cyc2B == (Road~(0,0,0,2)).symmetries      // mirror symmetry along vertical axis
    Cyc2C == (Road~(0,0,1,3)).symmetries      // mirror symmetry along diagonal
    Cyc2D == (Road~(0,0,2,0)).symmetries      // mirror symmetry along horizontal axis
    Cyc2E == (Road~(0,1,3,0)).symmetries      // mirror symmetry along other diagonal
    Cyc4  == (Road~(11,11,11,11)).symmetries  // 90 degree rotational symmetry
    Dih2A == (Road~(0,2,0,2)).symmetries      // vertical and horizontal mirror symmetry and 180 degree rotational symmetry
    Dih2B == (Road~(1,3,1,3)).symmetries      // mirror symmetry along both diagonals and 180 degree rotational symmetry
    Dih4  == (Road~(2,2,2,2)).symmetries      // all 8 symmetries

These are all the 10 subgroups of the [symmetry group of a square](https://en.wikipedia.org/wiki/Dihedral_group_of_order_8).
Thus, a correct definition of the orthogonal Road tile by IID would be:

    IdTile(0x00004B00,1,0,Dih2A)

As referring to the symmetry group names directly is error-prone,
it is better to obtain the symmetries from the flags of another tile
that more evidently possesses the desired symmetries.
So instead you could write:

    IdTile(0x00004B00, R1F0, (Road~(2,0,2,0)).symmetries)

Revisiting the example of the RHW-6S R1 curve from before, we could write each tile in terms of IIDs:

    IdTile(0x57000F00,0,0, (Road~(2,2,2,2)).symmetries) | IdTile(0x57945080,1,0,noSymmetries) | IdTile(0x57945380,1,0,noSymmetries) | %

This is already close to actual RUL2 code, but keeps some of the benefits of the metarule framework,
as calling `transduce` will produce the same 8 lines of output as before.
The advantages include the capabilities of a programming language, such as loops, variables, functions, abstraction.


Creating a Release
------------------

Bump the version number in `build.sbt`. Then run `sbt publishLocal` and upload the generated artifacts to GitHub.
(If the version number ends in `-SNAPSHOT`, you can run `sbt publishLocal` locally without uploading anything,
which is convenient if you make changes to the NAM repository at the same time.
Once your changes are finalized, you can remove the `-SNAPSHOT` from the version tag.)
