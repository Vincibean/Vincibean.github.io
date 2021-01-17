---
title: How to combine tests in Hspec? 
site_title: Test
site_logo: Test
site_description: Test
author: Vincibean
author_image: Test
author_bio: Test
site_cover: ../assets/img/geometric.jpg
---

**TL;DR**

you can use `(>>)`:
```haskell
(1 `shouldBe` 1) >> ( 1 `shouldBe` 3)
```

or (in a more idiomatic way) you can use `do`:
```haskell
it "counterEx 1" $ do
  1 `shouldBe` 2
  1 `shouldBe` 1
```

---

I started to know (and, frankly, to appreciate) [behavior-driven development](https://en.wikipedia.org/wiki/Behavior-driven_development) when I started to write Scala code.

My favourite testing library in Scala is [specs2](https://etorreborre.github.io/specs2/). Using the [words](https://medium.com/@etorreborre_99063/becoming-reasonable-361d7f674ee0) of its creator: 
> One thing which sets specs2 apart from other test libraries is its data model. At its heart it is a stream of "fragments" where a `Fragment` is a "description" and an "execution" which can return a "result".

My typical specs2 test will look like this:
```scala
class HelloWorldSpec extends Specification { def is = s2"""

  This is a specification for the 'Hello world' string

  The 'Hello world' string should
    contain 11 characters                             $e1
    start with 'Hello'                                $e2
    end with 'world'                                  $e3
                                                      """
  def e1 = "Hello world" must haveSize(11)
  def e2 = "Hello world" must startWith("Hello")
  def e3 = "Hello world" must endWith("world")
```

However, in time, I started to appreciate the value of combining test results together, like this:
```scala
class HelloWorldSpec extends Specification { def is = s2"""

  This is a specification for the 'Hello world' string

  The 'Hello world' string should
    contain 11 greeting characters                      $e1
                                                        """
  def e1 = ("Hello world" must haveSize(11)).and("Hello world" must startWith("Hello"))
```

So it only seems natural that, while coding in Haskell, I would find myself trying to apply the same pattern. 

In particular, I want to combine test results in [Hspec](https://hspec.github.io/) just like specs2 (`test1.and(test2)`).

However, I couldn't find any specific function for that in Hspec's API.

Haskell _does_ have an `and` operator, but it has a [totally different meaning](https://hackage.haskell.org/package/base-4.14.0.0/docs/Prelude.html#v:and).

We could combine the test results in the most naive way, turning everything into `Bool`s and then "combining" them with `&&`:
```haskell
it "Bad" $ 
  (1 == 1) && ( 1 == 22 )
```
but the result we'd get would be:
```
Bad FAILED [1]
...
Failures:
  1) Ch15.Ch15, Combination test, Bad
```
The error message tells us that a failure occurred, but it doesn't even tell us what the error is. Yuk!

Let's dig a bit deeper into the library.

Hspec's `shouldBe` returns an [Expectation](https://hackage.haskell.org/package/hspec-expectations-0.8.2/docs/Test-Hspec-Expectations.html#v:shouldBe). 

`Expectation` is a [type alias](https://hackage.haskell.org/package/hspec-expectations-0.8.2/docs/Test-Hspec-Expectations.html#t:Expectation) of [HUnit](https://hackage.haskell.org/package/HUnit)'s `Assertion`.

`Assertion` is a [type alias](https://hackage.haskell.org/package/HUnit-1.5.0.0/docs/Test-HUnit-Lang.html#t:Assertion) of `IO ()`.

`IO` has an instance of [Monad](https://hackage.haskell.org/package/base-4.9.0.0/docs/Control-Monad.html#t:Monad).

Of all functions exposed by `Monad`, the best fit to me seems `(>>)`, whose documentation says:
```
(>>) :: forall a b. m a -> m b -> m

Sequentially compose two actions, discarding any value produced by the first, like sequencing operators (such as the semicolon) in imperative languages.
```
If we try it out in Hspec:
```haskell
it "Good" $ 
  (1 `shouldBe` 1) >> ( 1 `shouldBe` 1)
it "counterEx 1" $ 
  (1 `shouldBe` 2) >> ( 1 `shouldBe` 1)
it "counterEx 2" $ 
  (1 `shouldBe` 1) >> ( 1 `shouldBe` 3)
```
We get this result:
```
    Good
    counterEx 1 FAILED [2]
    counterEx 2 FAILED [3]
    ...
  1) Ch15.Ch15, Combination test, counterEx 1
       expected: 2
        but got: 1

  ...

  test/Ch15/Ch15Spec.hs:138:29: 
  2) Ch15.Ch15, Combination test, counterEx 2
       expected: 3
        but got: 1
```
Exactly what we want!

But there's more! Since we now know that we are dealing with `IO` and `Monad`, we can sugar things up a bit and use Haskell's [do notation](https://en.wikibooks.org/wiki/Haskell/do_notation):
```haskell
it "Good" $ do
  1 `shouldBe` 1
  1 `shouldBe` 1
it "counterEx 1" $ do
  1 `shouldBe` 2
  1 `shouldBe` 1
it "counterEx 2" $ do
  1 `shouldBe` 1
  1 `shouldBe` 3
```

The result, of course, it the same:
```
    Good
    counterEx 1 FAILED [2]
    counterEx 2 FAILED [3]
    ...
  1) Ch15.Ch15, Combination test, counterEx 1
       expected: 2
        but got: 1

  ...

  test/Ch15/Ch15Spec.hs:138:29: 
  2) Ch15.Ch15, Combination test, counterEx 2
       expected: 3
        but got: 1
```

In summary, we don't need something like my revered `and` operator for combining tests; the Haskell ecosystem already provides `(>>)` and `do notation`.