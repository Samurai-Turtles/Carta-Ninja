data Card = Card String Int

get_elemento::Card->String
get_elemento (Card element _) = element

get_Value::Card -> Int
get_Value (Card _ value) = value


create_weight::Int->[Int]
create_weight n = replicate 5 (1*n)

-- [FAMNT]
generate_weight_hand::[Card]->[Int]->[Int]
generate_weight_hand [] _ = []
generate_weight_hand (a:as) weight = 
    let elem = get_elemento a
    in case elem of
        "F" -> (weight !! 0) : generate_weight_hand as weight
        "A" -> (weight !! 1) : generate_weight_hand as weight
        "M" -> (weight !! 2) : generate_weight_hand as weight
        "N" -> (weight !! 3) : generate_weight_hand as weight
        "T" -> (weight !! 4) : generate_weight_hand as weight

ia_choice :: [Card] -> [Int] -> Card








