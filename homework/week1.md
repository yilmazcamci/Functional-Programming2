## Exercise 1.4
```
double x = incr (incr 0)
    where incr y = x + y
```

1. Evaluate the result of double 5 by reduction as shown in the lecture: place every rewrite
step on a new line and explicitly state the rewrite rule used.

> ```
> double 5
> = 
>   incr (incr 0)
> = 
>   incr (5 + 0)
> = 
>   incr 5
> = 
>   5 + 5
> =
>   10
> ```


2. State the evaluation order you used (e.g., applicative order, normal order, . . . )

> This is applicative order, because at each step we first evaluate the arguments of a function before we apply the function.

## Exercise 1.5
Consider the following
definitions, which introduce a type of persons and some sample data.
```
type Person = (Name, Age, FavouriteCourse)
type Name = String
type Age = Integer
type FavouriteCourse = String

elena, peter, pol :: Person
elena = ("Elena", 33, "Functional Programming")
peter = ("Peter", 57, "Imperative Programming")
pol = ("Pol", 36, "Object Oriented Programming")

students :: [Person]
students = [elena, peter, pol]
```
Note: `students :: [Person]` means that students is a list of Person’s; we will explore lists in
more detail in coming weeks.


1. Add your own data (e.g. yourself) and/or invent some additional entries.

> ```
> matthijs :: Person
> matthijs = ("Matthijs", 20, "Advanced Mechanics")
> ```


2. The function age defined below extracts the age from a person, e.g. age elena = 33. In
case you wonder what the underscores are for, see Hint 1.
```
age :: Person → Age
age (_, n, _) = n
```
Define the functions:
```
name :: Person → Name
favouriteCourse :: Person → FavouriteCourse
```
that extract name and favourite course, respectively.

> ```
> name :: Person -> Name
> name (n, _, _) = n
>
> favouriteCourse :: Person -> FavouriteCourse
> favouriteCourse (_, _, n) = n
>```


3. Define a function showPerson :: Person → String that returns a string representation of
a person. As in Exercise 1.1, the operator ++ (concatenation) and function show (converting
expressions into strings) will be useful.

> ```
>
>
> ```

4. Define a function twins :: Person → Person → Bool that checks whether two persons
are twins. (For lack of data, we agree that two persons are twins if they are of the same
age.)
5. Define a function increaseAge :: Person → Person which increases the age of a given
person by one e.g.
⋙ increaseAge elena
("Elena",34,"Functional Programming")