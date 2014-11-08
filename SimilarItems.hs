module SimilarItems (jaccardSimilarity, jaccardBagSimilarity)

where
import qualified Data.Set as S
import qualified Data.List as L
import Data.Ratio

-- example of usage:
-- jaccardSimilarity [1,2,3,4,5] [3,4,5,6,7,8] ==> 3/8

jaccardSimilarity :: Ord a => [a] -> [a] -> Ratio Int
jaccardSimilarity list1 list2 =
  let
    set1 = S.fromList list1
    set2 = S.fromList list2
    intersectionSize = S.size (S.intersection set1 set2)
    unionSize = (S.size (S.union set1 set2))
  in
    intersectionSize % unionSize



-- example of usage:
-- jaccardBagSimilarity ['a','a','a','b'] ['a', 'a', 'b', 'b', 'c'] ==> 1 / 3

jaccardBagSimilarity :: Ord a => [a] -> [a] -> Ratio Int
jaccardBagSimilarity list1 list2 =
  let
    intersectionList = S.toList (S.intersection (S.fromList list1) (S.fromList list2))
    occurrenceInList1 = numberOfOccurrences list1 intersectionList
    occurrenceInList2 = numberOfOccurrences list2 intersectionList
    numberOfMinimumOccurrences = map (\x ->
                                let
                                  numberOfOccurrenceInList1 = snd (L.head (L.filter (\(y,n) -> x == y) occurrenceInList1))
                                  numberOfOccurrenceInList2 = snd (L.head (L.filter (\(y,n) -> x == y) occurrenceInList2))
                                in
                                  min numberOfOccurrenceInList1 numberOfOccurrenceInList2
                            )
                        intersectionList
  in
    L.minimum numberOfMinimumOccurrences % L.sum numberOfMinimumOccurrences

-- number of occurrence for each element of list1 in list2
numberOfOccurrences :: Ord a => [a] -> [a] -> [(a, Int)]
numberOfOccurrences list1 list2 = map (\x ->
                                          (x, L.length (L.filter (\y -> x == y) list1))
                                      )
                                  list2
