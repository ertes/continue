The Continue monad transformer
==============================

This monad transformer allows you to set continuation spots in a
computation and reenter it at any of those spots:

    label name = continue_ (M.singleton name ())

    do label "first"
       liftIO (putStrLn "Hello")
       label "second"
       liftIO (putStrLn "World!")

This computation gives you to spots for reentry, "first" and "second".
If you reenter at "first", the whole computation will be run again.  If
you reenter at "second", only the second `putStrLn` computation will be
run again.


Advanced reentering
-------------------

The most general way to reenter is to run perform a computation when
reentering:

    addCont False . M.singleton "abc" $ do
        liftIO (putStrLn "Reentered at spot abc.")
        return True

In the primary run this computation will just return `False`, but it
will also register a continuation spot.  When you reenter the
computation using that spot, it will return `True` instead.
