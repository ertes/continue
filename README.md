The ContinueT monad transformer
===============================

Quickstart tutorial
-------------------

This monad transformer allows you to set continuation spots in a
computation and reenter it at any of those spots possibly causing
different behavior:

    label name = continue_ (M.singleton name ())

    myComp = do
        label "first"
        liftIO (putStrLn "Hello")
        label "second"
        liftIO (putStrLn "World!")

This computation gives you two spots for reentry, "first" and "second".
If you reenter at "first", the whole computation will be run again.  If
you reenter at "second", only the second `putStrLn` computation will be
run again.


### Advanced reentering

The most general way to reenter is to perform a computation when
reentering:

    addCont (Right False) . M.singleton "abc" $ do
        liftIO (putStrLn "Reentered at spot abc.")
        return True

In the primary run this computation will just return `False`, but it
will also register a continuation spot.  When you reenter the
computation using that spot, it will return `True` instead.


### Suspension

A `ContinueT` monad also allows you to suspend a computation.  This
basically means that the computation is aborted at some spot to be
reentered somewhere else (or at the same spot) later.  You can do this
with the `empty` combinator as well as the `suspend` function.  Examples
TODO.


ContinueT explained
-------------------

`ContinueT` takes four arguments:

    newtype ContinueT e f m a

To form a monad, *e* has to be a monoid, *f* has to be a `Plus` functor
(as defined by the [semigroupoids] package) and *m* has to be a monad.


### The reentry functor

The most important parameter is the reentry functor *f*.  It represents
the type for maps of reentry points.  Example:

    ContinueT e (Map String) m a

A computation of this type may register reentry points indexed by
strings.  All registered reentry points are combined according to the
functor's `Plus` instance.


### The suspension monoid

Computations may suspend, in which case control is returned to a
controller, which may be the application loop or the `(<|>)` combinator:

    c1 <|> c2

This computation runs both `c1` and `c2` (note: even if neither
suspends) and returns the result of the left-most computation that did
not suspend.

When a computation suspends it may do so with a value of type *e* that
might communicate the reason for suspending.  This type must be a
monoid, because multiple branches of a `(<|>)` composition may suspend,
in which case the suspension values are combined according to the
`Monoid` instance of *e*.

A reasonable suspension monoid is `Last SomeException`, and since this
will likely be a very common choice this library defines a shorthand for
it:

    type LastEx = Last SomeException


[semigroupoids]: <http://hackage.haskell.org/package/semigroupoids>
