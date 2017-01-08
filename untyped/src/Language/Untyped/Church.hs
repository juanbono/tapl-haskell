module Language.Untyped.Church where

import           Language.Untyped.Syntax

-- Church Booleans
fls :: Term
fls = TmAbs "t" (TmAbs "f" (TmVar 0))

tru :: Term
tru = TmAbs "t" (TmAbs "f" (TmVar 1))

-- Church Boolean Operations
test :: Term
test = TmAbs "l" (TmAbs "m" (TmAbs "n" (TmApp (TmApp (TmVar 2) (TmVar 1)) (TmVar 0))))

and :: Term
and = TmAbs "b" (TmAbs "c" (TmApp (TmApp (TmVar 1) (TmVar 0)) (TmAbs "t" (TmAbs "f" (TmVar 0)))))

or :: Term
or = TmAbs "b" (TmAbs "c" (TmApp (TmApp (TmVar 0) tru) (TmVar 0)))

not :: Term
not = TmAbs "b" (TmApp (TmApp (TmVar 0) (TmAbs "t" (TmAbs "f" (TmVar 0)))) (TmAbs "t" (TmAbs "f" (TmVar 1))))

-- Church Pairs
pair :: Term
pair = TmAbs "f" (TmAbs "s" (TmAbs "b" (TmApp (TmApp (TmVar 0) (TmVar 2)) (TmVar 1))))

fst :: Term
fst = TmAbs "p" (TmApp (TmVar 0) tru)

snd :: Term
snd = TmAbs "p" (TmApp (TmVar 0) fls)

-- Church Numerals
c0, c1, c2, c3 :: Term
c0 = TmAbs "s" (TmAbs "z" (TmVar 0))
c1 = TmAbs "s" (TmAbs "z" (TmApp (TmVar 1) (TmVar 0)))
c2 = TmAbs "s" (TmAbs "z" (TmApp (TmVar 1) (TmApp (TmVar 1) (TmVar 0))))
c3 = TmAbs "s" (TmAbs "z" (TmApp (TmVar 1) (TmApp (TmVar 1) (TmApp (TmVar 1) (TmVar 0)))))

-- Operations on Church Numerals
scc :: Term
scc = TmAbs "n" (TmAbs "s" (TmAbs "z" (TmApp (TmVar 1) (TmApp (TmApp (TmVar 2) (TmVar 1)) (TmVar 0)))))

plus :: Term
plus = TmAbs "m" (TmAbs "n" (TmAbs "s" (TmAbs "z" (TmApp (TmApp (TmVar 3) (TmVar 1)) (TmApp (TmApp (TmVar 2) (TmVar 1)) (TmVar 0))))))

times :: Term
times = TmAbs "m" (TmAbs "n" (TmAbs "s" (TmAbs "z" (TmApp (TmApp (TmVar 3) (TmApp (TmVar 2) (TmVar 1))) (TmVar 0)))))

exp :: Term
exp = TmAbs "m" (TmAbs "n" (TmApp (TmVar 1) (TmVar 0)))

iszro :: Term
iszro = TmAbs "m" (TmApp (TmApp (TmVar 0) (TmAbs "x" fls)) tru)

pred :: Term
pred = undefined

subs :: Term
subs = undefined

-- Church Lists
