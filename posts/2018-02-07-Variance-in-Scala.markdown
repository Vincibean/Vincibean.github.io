---
title: Some Thoughts about Variance in Scala 
site_title: Test
site_description: Test
author: Vincibean
author_image: Test
author_bio: Test
---
Some time ago (so long ago!) I was asked by a fledgling (at the time) Scala developer to explain how covariance, 
contravariance and invariance work in Scala. Just like me, he came from Java, so getting all the technicalities of 
variance in Scala wasn't straightforward for him (just like it wasn't for me). The following is an adaptation of the 
answer I wrote to him.

## Section 1
~~In Java, generics are invariant~~ (this is wrong; see [this update](#update-18th-february-2018)). As a consequence, it is not possible to make an assignment like the following:
 ```java
ArrayList<Animal> dogs = new ArrayList<Dog>(); // Won't compile!
```

[Co | Contra | In] variance doesn't mean that there are Upper or Lower bounds; more than anything else, it defines the 
relationships of the "wrappers" (here: `ArrayList`) according to the type hierarchy (here: Animal and Dog). 
In this case, since there is no such relationship, it is not possible: in the eyes of the compiler, `ArrayList<Animal>` 
and `ArrayList<Dog>` are like two totally disconnected classes.

## Section 2 
In Scala, the `List` class is covariant, as you can see here:

```scala
val dogs: List[Animal] = List.empty[Dog]
```

Here the assignment works, since List is covariant. The basic idea is that, if a collection can store animals, then it 
can store dogs.

## Section 3
Let's define the following hierarchy:

```scala
class Living
class Animal extends Living
class Cat extends Animal
```

and then the various "container classes" using the upper and lower bounds:

```scala
class InvariantCage[T](val t: T)
class UpperBoundCage[T <: Animal](val t: T) // a cage where an upper bound is defined
class LowerBoundCage[T >: Animal](val t: T) // a cage where a lower bound is defined
```
 
For InvariantCage there is nothing else to mention.

For UpperBoundCage, this is what could happen:

```scala
val u = new UpperBoundCage(new Animal)
val u2 = new UpperBoundCage(new Living) // Will not compile
val u3 = new UpperBoundCage(new Cat)
```

This should not surprise: since `Living` is higher (with respect to the hierarchy) than the upper bound, the compiler 
reports an error.

For `LowerBoundCage`, the situation is a bit more complicated. Here's what happens:

```scala
val l = new LowerBoundCage(new Animal)
val l2 = new LowerBoundCage(new Living)
val l3 = new LowerBoundCage(new Cat)
```

Apparently, it seems that everything compiles, even `l3`; in fact, if we look at the type of `l3`, we can see that
something strange is happening:

```scala
l3: LowerBoundCage[Animal]
```

The compiler, to avoid errors, transforms ('upcasts') the `Cat` into an `Animal`. That's why it seems that everything is 
working fine. To return to the situation we want, we can 'force' the compiler to use the type we actually want, like this:

```scala
new LowerBoundCage[Cat](new Cat) // Will not compile!
```

Now the compiler knows what's the type we want to use, and reports an error. Obviously, we can force the correct type 
also in the other cases (those that compile): we will never have this error.

## Section 4 
Regarding the added value that upper and lower bounds can give us, I have found two nice articles 
([here](https://www.ibm.com/developerworks/library/j-jtp07018/) and 
[here](http://www.softwaregeek.net/2013/02/get-and-put-principle-in-java-generics.html)) where it is explained when it 
might make sense to use them... at the same time! The underlying principle is called '`get-put principle`', and it says:
```
Use an extends wildcard when you only get values ​​in a structure, and do not use a wildcard when you both get and put.
```

The example that is provided (and that I tried to rewrite in Scala) is the following: 

we try to write a method to pass data from one collection to another, trying to be as flexible as possible. 

A first attempt could be the following (to avoid the problem of the covariance of List, we use ListBuffer, which is crappy, 
but is also invariant):

```scala
def transfer1[T](from: ListBuffer[T], to: ListBuffer[T]): ListBuffer[T] = {
  for (f <- from)
    to += f
  to
}
```

**(N.B.: ListBuffer is part of Scala's mutable collections, so returning the filled ListBuffer is useless, it's for 
educational purposes only)**

Now let's try our function: transfer the contents of a `Cat` `ListBuffer` to an `Animal` `ListBuffer`.

```scala
transfer1(ListBuffer[Cat](new Cat(), new Cat(), new Cat()), ListBuffer.empty[Animal])  // Won't compile
```
It won't compile, for the usual problem about invariance. 

Let's try to modify the function by adding a Lower Bound (we are therefore in the "put" side of the `get-put principle`):

```scala
def transfer2[T](from: ListBuffer[T], to: ListBuffer[_ >: T]): ListBuffer[_ >: T] = {
  for (f <- from)
    to += f
  to
}
```

Now the example compiles! But let's try to pass him a `HelloKitty` `ListBuffer` (a subtype of `Cat`):

```scala
transfer2(ListBuffer(new HelloKitty(), new HelloKitty(), new HelloKitty()), ListBuffer.empty[Animal])   // Won't compile!
```

It doesn't compile again. Let's try to modify the function by adding an Upper Bound (here we are in the "get" side of the 
`get-put principle`):

```scala
def transfer3[T](from: ListBuffer[_ <: T], to: ListBuffer[_ >: T]): ListBuffer[_ >: T] = {
  for (f <- from)
    to += f
  to
}
```

Now the example compiles!

## Section 5
The example above has a pattern that seems familiar: on the one hand, a "producer" (from) that is better to make 
covariant, on the other a "consumer" (to) that is better to make contravariant. 
It looks like... the `trait` `Function1`! As you know, a lambda in Scala is syntactic sugar: at compile time, they are 
transformed into instances that implement the `FunctionN` type (where N is the number of arguments of the function); 
the `trait` `Function1` in particular, as you can see here, has a signature that, in general, looks like what we had before:
```scala
trait Function1 [-T1, +R]
```

We can try to implement our own function now:
```scala
val myF = new Function1[Cat, Animal] {
  override def apply(c: Cat): Animal = new Animal // Simple substitution
}
```
This function can be used in this way:
```scala
val animals: List[Animal] = List(new Cat).map(myF)
```
But we can also use it on different types:
```
val livings: List [Living] = List(new HelloKitty).map(myF)
```
Here we use as an argument of the function `HelloKitty` type (a subclass of `Cat`) and return instances of `Living` type 
(superclass of `Animal`).
So we have a good example of variance: our friendly neighborhood lambda!

# Update (18th February 2018)
This blog post ended up gaining a lot more interest than I expected, especially considering that it started as a simple 
email to explain a Scala concept in simple terms.

In particular, a reader highlighted an error: this blog post starts by stating that 
> In Java, generics are invariant

This is incorrect: in Java it is possible to achieve variance... through type-bounds!

But let's start with definitions

## Use-Site Variance & Declaration-Site Variance
In this blog post, I've been using the term 'variance' quite roughly. It turns out that variance can be classified into 
two categories:
- Use-Site Variance
- Declaration-Site Variance

Let's see them in detail.

With **use-site variance**, every time you use a generic class/interface you furthermore specify whether you want to 
access it invariantly, covariantly or contravariantly.

Conversely, with **declaration-site variance**, every time you declare a generic class/interface you furthermore 
specify whether it should be used invariantly, covariantly or contravariantly.

As stated before, in Java it is possible to achieve variance: in particular, it is possible to achieve use-site variance, 
and as previously stated the way to do this is through bound types.

Let's turn our `InvariantCage` into a Java class:
```java
public class InvariantCage<T> {
    
    private T t;
    
    public InvariantCage(T t) {
        this.t = t;
    }
    
    public T getT() {
        return t;
    }

}

```
We can take advantage of use-site variance in Java like this:
```java
InvariantCage<? extends Animal> cage = new InvariantCage<>(new Cat());
```
Yes: we are using an InvariantCage (i.e. a cage that was defined as invariant) in a covariant way by taking advantage of
use-site variance.

## Use-Site Variance? Is This Enough To Have Variance?
There's a way to prove that variance is actually taking place: the `Liskov Substitution Principle`.
[It states that](https://en.wikipedia.org/wiki/Liskov_substitution_principle) if S is a subtype of T, then objects of 
type T may be replaced with objects of type S (i.e. an object of type T may be substituted with any object of a subtype 
S) without altering any of the desirable properties of T (correctness, task performed, etc.)

We now have a tool to formally check if everything we said so far holds true.

Let's start with Scala. Scala allows declaration-site variance, which means that, in order to define a covariant cage, 
we can use the following notation:

```scala
class CovariantCage[+T](val t: T)
```

OK, now we need to see if the Liskov Substitution Principle (LSP) holds true, that is, we need to see if a 
`CovariantCage[Cat]` can be substituted to a `CovariantCage[Animale]`. The simplest form of "substitution" is the 
assignment.

```scala
val cd: CovariantCage[Animal] = new CovariantCage[Cat](new Cat())
``` 
That is, we are substituting (assigning) an object of type `CovariantCage[Cat]` to an object (reference) of type 
`CovariantCage[Cat]` without altering any of the 'desirable properties' (in fact, we are providing a `Cat`, which is a 
specialization over `Animal`).

Let's see if the same holds true for the Java case. Here, we'll have to resort to use-site variance:

```java
InvariantCage<? extends Animal> cage = new InvariantCage<Cat>(new Cat());
```

Again, the LSP tells us that what we wrote holds true.

# Is that all?
Well, no. There are still a couple of things that aren't quite clear to me, and this lurks into this blog post. 
I'll probably add another update once things are a bit clearer. 

Do you have any question so far? If this is the case, don't hesitate to [let me know](mailto:andrebessi00@gmail.com)

# Resources
- [https://medium.com/@sinisalouc/variance-in-java-and-scala-63af925d21dc](https://medium.com/@sinisalouc/variance-in-java-and-scala-63af925d21dc)
- [https://dzone.com/articles/covariance-and-contravariance](https://dzone.com/articles/covariance-and-contravariance)
- [Java Generics and Collections by Maurice Naftalin and Philip Wadler, ch.2](http://shop.oreilly.com/product/9780596527754.do)
- [Programming in Scala, Second Edition, by Martin Odersky, Lex Spoon and Bill Venners, ch. 19](https://www.amazon.com/Programming-Scala-Comprehensive-Step-Step/dp/0981531644)
- [The Liskov Substitiution Principle according to Wikipedia](https://en.wikipedia.org/wiki/Liskov_substitution_principle)