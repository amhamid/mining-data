module SimilarItems (jaccardSimilarity,
                    jaccardBagSimilarity,
                    shingle,
                    hashing,
                    hashings)

where
import qualified Data.Set as S
import qualified Data.List as L
import qualified Data.Ratio as R
import qualified Data.Hashable as H
import qualified Data.Text as T


-- example of usage:
-- jaccardSimilarity [1,2,3,4,5] [3,4,5,6,7,8] ==> 3/8

jaccardSimilarity :: Ord a => [a] -> [a] -> R.Ratio Int
jaccardSimilarity list1 list2 =
  let
    set1 = S.fromList list1
    set2 = S.fromList list2
    intersectionSize = S.size (S.intersection set1 set2)
    unionSize = (S.size (S.union set1 set2))
  in
    intersectionSize R.% unionSize



-- example of usage:
-- jaccardBagSimilarity ['a','a','a','b'] ['a', 'a', 'b', 'b', 'c'] ==> 1 / 3

jaccardBagSimilarity :: Ord a => [a] -> [a] -> R.Ratio Int
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
    L.minimum numberOfMinimumOccurrences R.% L.sum numberOfMinimumOccurrences

-- number of occurrence for each element of list1 in list2
numberOfOccurrences :: Ord a => [a] -> [a] -> [(a, Int)]
numberOfOccurrences list1 list2 = map (\x ->
                                          (x, L.length (L.filter (\y -> x == y) list1))
                                      )
                                  list2



-- Shingling is a way to transform a document (string of characters) to set representation

-- example of usage:
-- shingle "ammarhamidbasymeleh" 2
-- fromList ["am","ar","as","ba","db","eh","el","ha","id","le","ma","me","mi","mm","rh","sy","ym"]

shingle :: String -> Int -> S.Set String
shingle document k = shingle' (cleanDocument document) k S.empty

shingle' :: String -> Int -> S.Set String -> S.Set String
shingle' [] _ set = S.union S.empty set
shingle' document k set
                    | L.length document < k = shingle' [] k set
                    | otherwise =
                                  let
                                    kShingle = L.take k document
                                  in
                                    shingle' (L.tail document) k (S.insert kShingle set)


-- make sure that the hash result is 32-bit Int
hashing :: String -> Int
hashing document = mod (H.hash document) (2^32)

hashings :: [String] -> [Int]
hashings documents = map (\x -> hashing x) documents

-- clean document from whitespaces and newline
cleanDocument :: String -> String
cleanDocument document =
  let
    emptySpace = T.pack ""
    space = T.pack " "
    newline = T.pack "\n"
    documentWithoutSpace = T.replace space emptySpace (T.pack document) -- remove whitespace
    documentWithoutSpaceAndNewline = T.replace newline emptySpace documentWithoutSpace -- remove newline
  in
    T.unpack documentWithoutSpaceAndNewline
