removeNon

removeNonUppercase str = [c | c <- str, c `elem` ['A' .. 'Z']]

-- -- doubleMe x = x + x

-- doubleUs x y = x * 2 + y * 2

-- doubleSmallNumber x =
--   if x > 100
--     then x
--     else x * 2

-- lostNumbers = [1, 2, 3, 4, 5, 6]

{-

: puts at beginning of list
[1,2,3] ++ [5] => [1,2,3,5]

++ puts at end of list
5:[1,2,3] => [5,1,2,3]

Strings are lists of chars.

'A':" SMALL CAT" => "A SMALL CAT"

Index list with !!

"Hello" !! 3 => "l"
[1,2,3] !! 1 => 2

Lists can be compared

[3,2,1] > [2,1,0]
[1,2,3] == [1,2,3]

Basic functions

head [1,2,3] => 1
tail [1,2,3] => [2,3]
last [1,2,3] => 3
init [1,2,3] => 1,2
length : get length of list
null : check if list is empty
reverse : reverses a list
take : extract that many numbers from the beginning of the list
drop : drop that many numbers from the list and return the remains
maximum : get biggest element
minimum : get smallest element
sum : sum of all items in list
product : product of all items in list
elem : test if a list includes a value

3 `elem` [1,2,3] => True
10 `elem` [1,2,3] => False

[1..20] => [1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20]
['a'..'z'] => "abcdefghijklmnopqrstuvwxyz"

The syntax for range is:

[first, second .. last]

cycle : repeat items in a list an infinite amount of times

repeat : repeat an element an infinite amount of times

replicate : repeat an element N times

list comprehensions

[x * 2 | x <- [1..10]] => [2,4,6,8,10,12,14,16,18,20]

we can add N predicates (filters)

[x * 2 | x <- [1..10], x*2 >= 12] => [12, 14, 16, 18, 20]

they don't need to be functions

[x * 2 | x <- [1..10], x /= 13, x /= 15, x /= 19] => [10,11,12,14,16,17,18,20]

We can draw from N lists, which is handy. It will supply all possible combinations of each element of those lists.

[ x*y | x <- [2, 5, 10], y <- [8, 10, 11]]

=> [16,20,22,40,50,55,80,100,110]

odd : check if a number is odd

Since Strings are lists, we can use list comprehensions to create them.

List comprehensions can be nested.
[ [ x | x <- xs, even x ] | xs <- xxs] =>

let xxs = [[1,3,5,2,3,1,2,4,5],[1,2,3,4,5,6,7,8,9],[1,2,4,2,1,6,3,1,3,2,3,6]]

tuples are (1, 2)

tuple functions

fst : returns first element from a pair
snd : returns second element from a pair

zip : takes two lists and puts into a single list by joining matching elements into pairs

compare functions will return an Ordering, which can be LT, GT or EQ

5 `compare` 3 => Ord

show : convert to a string

read : parse a value from a string

succ : successor of an element, e.g. 4 -> 5

pred : predecessor of an element, e.g. 4 -> 3

fromIntegral can be used to convert Int and Integer into other number types such as a Float. Example, this won't error:

fromIntegral (length [1,2,3,4]) + 3.2

Pattern matching on function arguments:

lucky :: (Integral a) => a -> String
lucky 7 = "LUCKY NUMBER SEVEN!"
lucky x = "Sorry, you're out of luck, pal!"

factorial :: (Integral a) => a -> a
factorial 0 = 1
factorial n = n * factorial (n - 1)

the @ character can be used to get the entire pattern again

capital :: String -> String
capital "" = "Empty string, whoops!"
capital all@(x:xs) = "The first letter of " ++ all ++ " is " ++ [x]

Guards

densityTell :: (RealFloat a) => a -> a -> String
densityTell mass volume
    | mass / volume < 1.2 = "Wow! You're going for a ride in the sky!"
    | mass / volume <= 1000.0 = "Have fun swimming, but watch out for sharks!"
    | otherwise   = "If it's sink or swim, you're going to sink."
-}

--
-- List comprehensions can be nested.
densityTell :: (RealFloat a) => a -> a -> String
densityTell mass volume
  | mass / volume < 1.2 = "Wow! You're going for a ride in the sky!"
  | mass / volume <= 1000.0 = "Have fun swimming, but watch out for sharks!"
  | otherwise = "If it's sink or swim, you're going to sink."
