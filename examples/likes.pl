% example from https://www.youtube.com/watch?v=IJ5zp9DQfMU

person(alice).
person(mary).
person(sally).

likes(alice, coke).
likes(alice, fanta).
likes(alice, sprite).
likes(mark, water).
likes(mark, coffee).
likes(sally, pepsi).

?- person(Name), likes(Name, Drink).
