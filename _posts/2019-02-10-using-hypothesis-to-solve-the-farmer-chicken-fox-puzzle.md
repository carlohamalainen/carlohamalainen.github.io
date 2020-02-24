---
title: Using Hypothesis to solve the farmer-chicken-fox puzzle
date: 2019-02-10T23:55:18+00:00
author: Carlo Hamalainen
layout: post
permalink: /2019/02/10/using-hypothesis-to-solve-the-farmer-chicken-fox-puzzle/
---
The farmer-chicken-fox puzzle goes something like this: a farmer is at a shop, having bought a chicken, fox, and a bag of corn. The farmer would like to get to her house on the other side of the river using a small boat. For some reason she can take at most one item at a time. If the chicken is left alone with the corn, it will eat the corn. If the fox is left alone with the chicken, it will eat the chicken. How does the farmer get across the river without losing any item?

A straightforward way to solve this is to use a breadth-first search, enumerating all valid moves from a set of states. The initial state would be

    shop: {farmer, chicken, fox, corn}
    house: {}

One valid first move is to go to the other side with the chicken, giving the new state:

    shop: {fox, corn}
    house: {farmer, chicken}

Another way is to use the [Hypothesis](https://hypothesis.works/) library to simulate a finite state machine, and assert that no sequence of rules leads to the state where everyone is on the house side of the river. This is an adaptation of the [Die Hard water jugs problem](https://hypothesis.works/articles/how-not-to-die-hard-with-hypothesis/).

We model the state using two sets of strings:

{% highlight python %}
class FarmerChickenFox(RuleBasedStateMachine):

    def __init__(self):
        self.shop  = set(['farmer', 'chicken', 'fox', 'corn'])
        self.house = set([])
        super().__init__()
{% endhighlight %}

Here's the rule to take the chicken. We work out which side the farmer is in, and then move the farmer and the chicken to the other side. If this transition results in an invalid state (e.g. the fox left alone with the chicken) we undo the state-change.

{% highlight python %}
@rule()
    def take_chicken(self):
        self.save_state()

        if 'farmer' in self.shop and 'chicken' in self.shop:
            self.shop.remove('farmer')
            self.shop.remove('chicken')
            self.house.add('farmer')
            self.house.add('chicken')
            if not self.state_ok(): self.undo_state()
        elif 'farmer' in self.house and 'chicken' in self.house:
            self.house.remove('farmer')
            self.house.remove('chicken')
            self.shop.add('farmer')
            self.shop.add('chicken')
            if not self.state_ok(): self.undo_state()
{% endhighlight %}

We have two invariants to ensure that the state of the system is consistent:

{% highlight python %}
@invariant()
    def fox_not_with_chicken(self):
        return not self.is_fox_alone_with_chicken()

    @invariant()
    def chicken_not_with_corn(self):
        return not self.is_chicken_alone_with_corn()
{% endhighlight %}

The trick is to create an invariant to check if everyone is on the house side:

{% highlight python %}
@invariant()
    def not_solved(self):
        note("::: shop: {s}, house: {h}".format(s=self.shop, h=self.house))
        assert len(self.house) != 4
{% endhighlight %}

Running using `pytest` finds the solution, including the steps used:

    $ pytest farmerchickenfox.py

    (snipped)

    _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ 

    self = FarmerChickenFox({})

        @invariant()
        def not_solved(self):
            note("::: shop: {s}, house: {h}".format(s=self.shop, h=self.house))
    >       assert len(self.house) != 4
    E       AssertionError: assert 4 != 4
    E        +  where 4 = len({'chicken', 'corn', 'farmer', 'fox'})
    E        +    where {'chicken', 'corn', 'farmer', 'fox'} = FarmerChickenFox({}).house

    farmerchickenfox.py:117: AssertionError
    -------------------------------------------------------------------------------------------- Hypothesis --------------------------------------------------------------------------------------------
    Falsifying example: run_state_machine(factory=FarmerChickenFox, data=data(...))
    state = FarmerChickenFox()
    ::: shop: {'fox', 'chicken', 'farmer', 'corn'}, house: set()
    state.take_chicken()
    ::: shop: {'fox', 'corn'}, house: {'chicken', 'farmer'}
    state.go_alone()
    ::: shop: {'fox', 'farmer', 'corn'}, house: {'chicken'}
    state.take_corn()
    ::: shop: {'fox'}, house: {'corn', 'chicken', 'farmer'}
    state.take_chicken()
    ::: shop: {'fox', 'chicken', 'farmer'}, house: {'corn'}
    state.take_fox()
    ::: shop: {'chicken'}, house: {'corn', 'fox', 'farmer'}
    state.go_alone()
    ::: shop: {'chicken', 'farmer'}, house: {'corn', 'fox'}
    state.take_chicken()
    ::: shop: set(), house: {'corn', 'fox', 'farmer', 'chicken'}
    state.teardown()

    You can reproduce this example by temporarily adding @reproduce_failure('4.1.0', b'AAEBAQABAgEBAQMBAAEB') as a decorator on your test case
    ===================================================================================== 1 failed in 0.46 seconds =====================================================================================

Full source: [farmerchickenfox.py](https://github.com/carlohamalainen/playground/blob/master/python/farmerchickenfox/farmerchickenfox.py).
