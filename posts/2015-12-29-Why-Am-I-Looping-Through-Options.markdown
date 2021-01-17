---
title: Why Am I Looping Through Options??
author: Vincibean
author_image: Test
author_bio: Test
---
For those like me who started with imperative programming, functional programming may seem like a minefield, 
especially when familiar concepts gain different meanings. Recently, I had the opportunity to learn a couple of things about Scala, 
and I fell in love with Options: they deal with the infamous [billion-dollar mistake](https://en.wikipedia.org/wiki/Tony_Hoare/Apologies_and_retractions)
(whose presence simply isn't justified in modern pointer-free languages); more importantly, by explicitly stating that a 
value may or may not be present, force developers to think about and deal with this 
possibility. In this way, Options are so different by those lurking nulls: there simply is no way you might accidentally rely on 
the presence of a value that is actually optional. This is well explained [here](http://danielwestheide.com/blog/2012/12/19/the-neophytes-guide-to-scala-part-5-the-option-type.html).
   
Yet, a familiar concept that gain a different meaning in Scala is the for loop, whenever it is associated with an Option, like in the following example:
      
      for {
         language <- Some("Scala")
         behavior <- Some("rocks")
      } yield s"$language $behavior"
  
Which will yield:
      
      res0: Option[String] = Some(Scala rocks)
  
While you may already be familiar with this, it quite puzzled me: what is the logical assumption behind this? Why should I be allowed to loop through an optional value?   
I had the opportunity to talk with [Stefano Baghino](https://github.com/stefanobaghino) about this. Stefano is always ready to answer my questions, 
and for this I wholeheartedly thank him.
  Stefano explained to me that there are two possible explanations behind this: a simpler yet incorrect one, and a more involved yet more rigorous one.   
  The first, simpler explanation (which is better exposed [here](http://danielwestheide.com/blog/2012/12/19/the-neophytes-guide-to-scala-part-5-the-option-type.html)) 
  simply considers an Option as a collection: a particular type of collection that contains either zero or one element. As a further evidence, you may even consider that 
  Option comes with all the Scala collections goodness you love, like map() and flatMap().   
  The second, more rigorous one deals with the concept of "for loop": in fact, we imperative programmers are used to consider the 
  "for" keyword as just a means to loop. Yet, in Scala, "for" can be much more: as it's well explained [here](http://docs.scala-lang.org/tutorials/FAQ/yield.html)
  and [here](http://nerd.kelseyinnis.com/blog/2013/11/12/idiomatic-scala-the-for-comprehension/), "for" has other uses, one being the so called "for comprehension" (or "for-yield").   
   You may be familiar with Python's (or Ruby's) list comprehension, the syntactic construct for creating a list based on existing lists. Scala's "for" can be viewed as
   a generalization of list comprehension. Think of it as syntactic sugar hiding the composition of multiple operations.   
   In this sense, for comprehension can be viewed 
   as an equivalent to [Haskell’s do notation](https://en.wikibooks.org/wiki/Haskell/do_notation), 
   allowing you to do simple looping, nested looping, list comprehension (thanks to our friend, yield) and, in general, composing multiple operations.   
   
   And you? What's your opinion about this?
