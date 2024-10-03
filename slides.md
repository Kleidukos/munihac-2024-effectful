---
css:
  - ./assets/css/variables.css
  - ./assets/css/styles.css
title: Effect Systems in Practice
subtitle:
introductory_notes: |
  Hello everyone,
  this talk about practical programming with effects in Haskell
light: true
#ratio43: true
overlay: MuniHac 2024
author:
  - name: Hécate
    desc:
      - Software Engineer / PM at Scrive </br> <span class="big-2">&</span>
      - Helping hand at the Haskell Foundation </br> <span class="big-2">&</span>
      - Trade Unionism at Solidaires Informatique

---

# Follow the presentation

:::jumbogroup
<img src="./assets/img/talk-qrcode.png" height=300>
:::

---

# Structure of this talk

* What this talk is not about
* The need for side-effect tracking: IO
* Semantic Effect Tracking
* Effect Systems, their implementation, their tragedies

---

# What this talk is not about

<div class="big-1">
This is not a deep dive into the theoretical implications of algebraic effects

➡️ Check out the talks of Alexis King and Ningning Xie
</div>

:::notes
If I think you'll receive a better answer from one of them, I will redirect you to their work
:::

---

# 🤡

:::jumbogroup
<img src="./assets/img/URL-equal-method.png" height=400>
:::

> Java™ Platform Standard Ed. 8, docs.oracle.com

---

# The need for side-effect tracking

## Determinism, Referential transparency

* > Determinism: The output of an expression stays the same if the input is the same
* > Referential transparency: You can replace a call to a function with its result in a safe and predictible manner

:::notes
Interactions with the outside world in non-reproducible ways is the burden of many programmers.
I want to clarify some notions here that make life much, much easier.
:::

---

# The need for side-effect tracking

## No side effects

* No alteration of the outside world in ways that matter to us
  * Missile strike? No!
  * Register alloation? Fairly essential.

::: notes
Meanwhile in an Eternal War alternate reality based on launching missiles: we do not allocate registers willy-nilly!
:::

---

# The need for side-effect tracking

## Side effects are arbitrary

> A function without any effect is called total and corresponds to mathematically total functions – a good place to be.
> Then we have effects for partial functions that can raise exceptions (exn), and potentially non-terminating functions as div (divergent).
> **The combination of exn and div is called pure as that corresponds to Haskell's notion of purity.**

– "The Koka Programming Language"

:::notes
Now, does that mean that IO is complete bullshit? No! We needed an analytical framework to understand and anticipate the results of programs, and IO was the option that was retained **at the time**
:::

---

# The need for side-effect tracking

## Enforcing sequentiality of computations

### When you're a pure lambda calculus girl living in a pure lambda calculus world, life in plastic _is_ indeed fantastic!

:::notes
You can do a lot with the generated code, like re-organise it, inline it, anticipate what it will do and replace function calls.
Inlining becomes easier as well, when you know for a fact that.
:::

---

# The need for side-effect tracking

## Enforcing sequentiality of computations

### But how about computations that depend on each-other?

::: notes
However you can't do much when it comes to interacting with the outside world, and at the level of code generation, there are things that are absolutely forbidden, like re-organising the order of computations that depend on each-other and that produce side-effects.
:::

---

# Interlude: Anatomy of a type signature

```haskell
fun :: a
```

```haskell
       ❗
      ┌──┐
fun :: IO a
```

``` haskell
            ❗
      ┌──────────────┐
fun :: (Constraint a) => IO a
```

---

# Semantic side-effect tracking

## These functions could be doing anything

```haskell
trace :: Text -> IO a -> IO a

fetchFromCache :: Int -> IO (Maybe Result)

log :: Text -> IO ()

fetchFromDB :: Int -> IO (Maybe Result)

throwError :: ServerError -> IO ()
```

::: notes
The signification of `IO` is, fundamentally: I give up.
We saw earlier that Haskell admits runtime exceptions as part of its model for purity (think about `undefined` or `error`).
:::

---

# Semantic side-effect tracking

<div class="big-3 horizontally-centered">
Semantic Opacity
</div>

```haskell
myComputation :: Int -> IO Result
myComputation number = trace "myComputation" $ do
  mCacheResult <- fetchFromCache number
  case mCacheResult of
    Just cacheResult -> do
      log "Found result from cache"
      pure cacheResult
    Nothing -> do
      log "No result in cache"
      mDatabaseResult <- fetchFromDB number
      case mDatabaseResult of
        Nothing -> do
          log "No result in database"
          throwError notFound
        Just dbResult -> do
          log "Found result from database"
          pure dbResult
```

::: notes
Putting every type of outside interaction into the 'IO' box is Semantic Opacity:
This leads us into a mental framework of pure versus impure, good versus evil, safe versus unsafe,
which is not only wrong but also steers us in the wrong direction for thinking about systems.

From an architectural point of view we need to be able to know what are the interactions
of our systems.
:::

---

# Semantic side-effect tracking

<div class="big-3 horizontally-centered">
Semantic clarity
</div>


```haskell
trace :: (Trace :> es) => Text -> Eff es a -> Eff es a

fetchFromCache :: (Cache :> es) => Int -> Eff es (Maybe Result)

log :: (Log :> es) -> Text -> Eff es ()

fetchFromDB :: (DB :> es) -> Int -> Eff es (Maybe Result)

throwError :: (Error ServerError :> es) -> ServerError -> Eff es ()
```

```haskell
myComputation :: Int -> Eff [Trace, Log, Cache, DB, Error ServerError] Result
```

<div class="horizontally-centered big-3">
 🎉 🎉 🎉
</div>

::: notes
We were blindly using IO to say "all bets are off", and now we are listing,
with precision, well-understood effects that denote interactions with various outside systems.

Lo and behold: suddenly `myComputation` makes a lot more sense!

Notice that we are using the syntax for constraints here. With the help of the compiler,
redundant constraints are signalled, and can tell you if you have mistakenly removed an
interaction with an outside system, because this interaction is labeled at the types level.
:::

---

# Effect Systems 

<div class="horizontally-centered big-3">
Their implementation 
</div>

<div class="horizontally-centered big-1">
Their tragedies
</div>

---

# Effect Systems

## Historically: Monad Transformers

* Newtypes around values with a custom `Monad` instance for sequentiality
* Instance of `MonadTrans` to use its `lift` method

---

### Monad Transformers Stacks

```haskell
ExceptT e m a
```

* `e`: The error type
* `m`: The monad on which the transformer is stacked
* `a`: The return value of the computation

### Stack 'Em Up!

```haskell
ReaderT AuthEnv (
  RedisReaderT Persistent (
    CryptoRNGT (
      DBT (
        LogT (
          TraceT Handler)))))
```

---

### Monad transformer constraints

```haskell
( Reader AuthEnv m
, RedisReader PersisTent m
, CryptoRNG m
, DB m
, Log m
, Trace m
) => m Handler
```

---

## Free Monads



---

# Now Introducing: Effectful

:::jumbogroup
<img src="./assets/img/effectful-logo.png" height=300>
:::

---

# Now Introducing: Effectful

> Created by Andrzej Rybczak in 2021

## Integrations
  * Drop-in solution in 🫵 your codebase
  * Integrations with `unliftio`, `exceptions`, `resourcet`

## Correctness
  * To hell with counter-intuitive interactions!

## Performance
  * `SmallMutableArray` + `RealWorld`
  * `PrimArray Int`
  * Strict `IORef`

:::notes
Effectful stems from the double need of replacing stacks of a dozen transformers in industrial
code bases. For this, it needs very good integration with libraries like `unliftio`, `exceptions`, `resourcet`, because it is made to be dropped into in an ecosystem of libraries.

However, it does not inherit types from the `transformers` library.
For the sake of correctness, Effectful re-implemented the State, Writer, and Except transformers. While ReaderT does not have any surprising behaviour,
the ways StateT and ExceptT interact together can be pretty counter-intuitive! Especially, dropping state updates is not cool, and the fact that
ExceptT does not raise a runtime exception means that resources are not freed. It's very easy to inadvertently trigger a space leak.

As such, the performance need not to be just decent but very good. It makes use of IORefs, and low-level Haskell constructs like mutable arrays.
:::

# Now Introducing: Effectful

## Ergonomics

From:

```haskell
webHandler
  :: ( MonadBaseControl IO m
     , MonadCatch
     , MonadDB m
     , MonadFileStorage m
     , MonadLog m
     , MonadMask m
     , MonadRandom m
     , MonadThrow
     , MonadTime m
     , MonadTrace m)
```

to

```haskell
webHandler
  :: ( FileStorage :> es
     , RNG :> es
     , Log :> es
     , Time :> es
     , Trace :> es
     , DB :> es)
```

::: notes
Offering good ergonomics to replace your bare ReaderT or your MTL constraints.

The Eff monad has instances for MonadThrow, MonadCatch, MonadMask, and `MonadBaseControl IO`, so you don't have to specify them manually.
:::

---

# My thanks go to

* Alexis King
* Andrzej Rybczak
* Ningning Xie
* Xiaoyan Ren

---

# Sources

* _"The Koka Programming Language", §2.2 "Effect Typing"_, <https://koka-lang.github.io/koka/doc/book.html#why-effects>
