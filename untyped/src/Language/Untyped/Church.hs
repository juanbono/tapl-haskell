module Language.Untyped.Church where

import           Language.Untyped.Syntax

id :: NamelessTerm
id = NmAbs "x" (NmVar 0)

-- Church Booleans
fls :: NamelessTerm
fls = NmAbs "t" (NmAbs "f" (NmVar 0))

tru :: NamelessTerm
tru = NmAbs "t" (NmAbs "f" (NmVar 1))

-- Church Boolean Operations
test :: NamelessTerm
test = NmAbs "l" (NmAbs "m" (NmAbs "n" (NmApp (NmApp (NmVar 2) (NmVar 1)) (NmVar 0))))

and :: NamelessTerm
and = NmAbs "b" (NmAbs "c" (NmApp (NmApp (NmVar 1) (NmVar 0)) (NmAbs "t" (NmAbs "f" (NmVar 0)))))

or :: NamelessTerm
or = NmAbs "b" (NmAbs "c" (NmApp (NmApp (NmVar 0) tru) (NmVar 0)))

not :: NamelessTerm
not = NmAbs "b" (NmApp (NmApp (NmVar 0) (NmAbs "t" (NmAbs "f" (NmVar 0)))) (NmAbs "t" (NmAbs "f" (NmVar 1))))

-- Church Pairs
pair :: NamelessTerm
pair = NmAbs "f" (NmAbs "s" (NmAbs "b" (NmApp (NmApp (NmVar 0) (NmVar 2)) (NmVar 1))))

fst :: NamelessTerm
fst = NmAbs "p" (NmApp (NmVar 0) tru)

snd :: NamelessTerm
snd = NmAbs "p" (NmApp (NmVar 0) fls)

-- Church Numerals
c0, c1, c2, c3 :: NamelessTerm
c0 = NmAbs "s" (NmAbs "z" (NmVar 0))
c1 = NmAbs "s" (NmAbs "z" (NmApp (NmVar 1) (NmVar 0)))
c2 = NmAbs "s" (NmAbs "z" (NmApp (NmVar 1) (NmApp (NmVar 1) (NmVar 0))))
c3 = NmAbs "s" (NmAbs "z" (NmApp (NmVar 1) (NmApp (NmVar 1) (NmApp (NmVar 1) (NmVar 0)))))

-- Operations on Church Numerals
scc :: NamelessTerm
scc = NmAbs "n" (NmAbs "s" (NmAbs "z" (NmApp (NmVar 1) (NmApp (NmApp (NmVar 2) (NmVar 1)) (NmVar 0)))))

plus :: NamelessTerm
plus = NmAbs "m" (NmAbs "n" (NmAbs "s" (NmAbs "z" (NmApp (NmApp (NmVar 3) (NmVar 1)) (NmApp (NmApp (NmVar 2) (NmVar 1)) (NmVar 0))))))

times :: NamelessTerm
times = NmAbs "m" (NmAbs "n" (NmAbs "s" (NmAbs "z" (NmApp (NmApp (NmVar 3) (NmApp (NmVar 2) (NmVar 1))) (NmVar 0)))))

exp :: NamelessTerm
exp = NmAbs "m" (NmAbs "n" (NmApp (NmVar 1) (NmVar 0)))

iszro :: NamelessTerm
iszro = NmAbs "m" (NmApp (NmApp (NmVar 0) (NmAbs "x" fls)) tru)

pred :: NamelessTerm
pred = undefined

subs :: NamelessTerm
subs = undefined

-- Church Lists
