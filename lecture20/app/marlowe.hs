{-# LANGUAGE OverloadedStrings #-}

import Language.Marlowe.Extended.V1

main :: IO ()
main = printJSON $ example [Role "Alice", Role "Bob"] (Role "Charlie")



example :: [Party] -> Party -> Contract
example parties oracle = 
    When
        (map (\p -> deposit p $ filter (/= p) parties ) parties)
        (TimeParam "payment_deadline")
        Close
    where
        cid :: ChoiceId
        cid = ChoiceId "winner" oracle

        choice :: [(Party, Integer)] -> Contract
        choice [] = Close
        choice ((p, n) : xs) = 
            If
                (ValueEQ (ChoiceValue cid) (Constant n))
                (pay p $ filter (/= p) parties)
                (choice xs)

        pay :: Party -> [Party] -> Contract
        pay _ [] = Close
        pay p (q : qs) = Pay q (Account p) ada (ConstantParam "amount") $ pay p qs
            

        deposit :: Party -> [Party] -> Case
        deposit p ps = 
            Case
                (Deposit p p ada (ConstantParam "amount"))
                (case ps of
                    []  -> 
                        When 
                            [Case
                                (Choice cid [Bound 1 $ toInteger $ length parties])
                                (choice (zip parties [1..]))
                            ]
                            (TimeParam "choice_deadline")
                            Close 
                    qs  -> 
                         When
                            (map (\q -> deposit q $ filter (/= q) qs ) qs)
                            (TimeParam "payment_deadline")
                            Close)

    

{-
alice, bob, charlie :: Party
alice = Role "Alice"
bob = Role "Bob"
charlie = Role "Charlie"

cid :: ChoiceId
cid = ChoiceId "winner" charlie

pay :: Party -> Party -> Contract
pay from to = Pay from (Account to) ada (ConstantParam "amount") Close

deposit :: Party -> Party -> Case
deposit first second = Case
        (Deposit first first ada (ConstantParam "amount"))
        (When
            [Case
                (Deposit second second ada (ConstantParam "amount"))
                (When
                    [Case
                        (Choice cid [Bound 1 2]
                        )
                        (If
                            (ValueEQ (ChoiceValue cid) (Constant 1))
                            (pay bob alice)
                            (pay alice bob)
                        )]
                    (TimeParam "choice_deadline")
                    Close 
                )]
            (TimeParam "payment_deadline")
            Close 
        )

example :: Contract
example = When
    [ deposit alice bob
    , deposit bob alice]
    (TimeParam "payment_deadline")
    Close
-}