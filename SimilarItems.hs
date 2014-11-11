module SimilarItems (jaccardSimilarity,
                    jaccardBagSimilarity,
                    shingle,
                    hashing,
                    hashings,
                    generateCharacteristicMatrix,
                    minhashingSignature,
                    localitySensitiveHashing)

where
import qualified Data.Set as S
import qualified Data.List as L
import qualified Data.Ratio as R
import qualified Data.Hashable as H
import qualified Data.Text as T
import qualified Data.Maybe as M


-- example of usage:
-- $> jaccardSimilarity [1,2,3,4,5] [3,4,5,6,7,8] ==> 3/8

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
-- $> jaccardBagSimilarity ['a','a','a','b'] ['a', 'a', 'b', 'b', 'c'] ==> 1 / 3

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
-- $> shingle "ammarhamidbasymeleh" 2
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



-- generate characteristic matrix (see page 81 from mining data syllabus from Coursera (Stanford))
-- this is useful pre-step to compress large data and still be able to check Jaccard Similarity (using minhashing)
-- Element ￼ S1  S2  S3  S4
--       a ￼ 1   0   0   1
--       b ￼ 0   0   1   0
--       c ￼ 0   1   0   1
--       d ￼ 1   0   1   1
--       e ￼ 0   0   1   0

-- example of usage:
-- $> generateCharacteristicMatrix [['a', 'd'], ['c'], ['b', 'd', 'e'], ['a', 'c', 'd']] ['a', 'b', 'c', 'd', 'e']
-- [(0,[True,False,False,True]),(1,[False,False,True,False]),(2,[False,True,False,True]),(3,[True,False,True,True]),(4,[False,False,True,False])]

                                     -- columns   rows
generateCharacteristicMatrix :: Ord a => [[a]] -> [a] -> [(Int, [Bool])]
generateCharacteristicMatrix lists universalList =
  let
    -- remove duplicate by transforming back and forth to Set
    universalSet = S.toList (S.fromList universalList)
    sets = map (\list -> S.toList (S.fromList list)) lists
  in
    map (\element ->
            let
              M.Just index = L.elemIndex element universalSet
            in
              (index, map (\set -> L.elem element set) sets)
        )
    universalSet



-- compute minhashing signature for each set

-- example of usage:
-- before using minhashingSignature, first generate characteristic matrix! (or doing it inline)
-- $> generateCharacteristicMatrix [['a', 'd'], ['c'], ['b', 'd', 'e'], ['a', 'c', 'd']] ['a', 'b', 'c', 'd', 'e']
-- [(0,[True,False,False,True]),(1,[False,False,True,False]),(2,[False,True,False,True]),(3,[True,False,True,True]),(4,[False,False,True,False])]

-- $> minhashingSignature (generateCharacteristicMatrix [['a', 'd'], ['c'], ['b', 'd', 'e'], ['a', 'c', 'd']] ['a', 'b', 'c', 'd', 'e']) [(\x -> mod (x + 1) 5), (\x -> mod (3*x + 1) 5)]
-- [[1,0],[3,2],[0,0],[1,0]]

-- visualization example:
-- 1. characteristic matrix:
-- Element ￼ S1  S2  S3  S4
--       a ￼ 1   0   0   1
--       b ￼ 0   0   1   0
--       c ￼ 0   1   0   1
--       d ￼ 1   0   1   1
--       e ￼ 0   0   1   0

-- 2. Minhashing result:
--    S1 S2 S3 S4
-- h1  1  3  0  1
-- h2  0  2  0  0

                     -- characteristic mattrix  -- hash functions
minhashingSignature :: [(Int, [Bool])] -> [(Int -> Int)] -> [[Int]]
minhashingSignature characteristicMatrix hashFunctions =
  let
    preprocessedMatrix =
                  map (\(index, row) ->
                        map (\element ->
                                  if element then fromIntegral index
                                  else fromIntegral (-1) -- to identify for False (where 0 can be used for index)
                            )
                        row
                      )
                  characteristicMatrix

    transposedMatrixWithComputation = map (\fun ->
                              map (\list ->
                                    map (\x ->
                                            if(x > (-1)) then fun x
                                            else fromIntegral (2^31 - 1) -- to mimic infinity in Int32
                                        )
                                    list)
                              (L.transpose preprocessedMatrix)
                          )
                      hashFunctions

    minhashingResult = map (\xss ->
                                map (\xs -> L.minimum xs) xss)
                       transposedMatrixWithComputation -- intermediate result from the perspective of hash function
  in
    L.transpose minhashingResult -- minhashing signature for each set


-- create locality-sensitive hashing for a given minhashing signature

-- example of usage:
-- $> localitySensitiveHashing [[1,2,3,4], [9,10,11,12]] 2
-- [[[1,2],[9,10]],[[3,4],[11,12]]]

                       --minhash sig.  -- bands
localitySensitiveHashing :: [[Int]] -> Int -> [[[Int]]]
localitySensitiveHashing [[]] _ = error "minhashing signature should be non-empty list"
localitySensitiveHashing minhashingSignature bands
    | L.length (head minhashingSignature) `mod` bands /= 0 = error "minhash signature row `mod` bands should be 0 ==> b * r = n where is the number of row in minhash signature"
    | otherwise =
      let
        transposedMinhashingSignature = L.transpose minhashingSignature
        rows = (L.length (head minhashingSignature)) `quot` bands
      in
        map L.transpose (localitySensitiveHashing' transposedMinhashingSignature bands rows)


localitySensitiveHashing' :: [[Int]] -> Int -> Int -> [[[Int]]]
localitySensitiveHashing' [] _ _ = []
localitySensitiveHashing' transposedMinhashingSignature bands rows =
    let
      headMinHashSignature = L.take rows transposedMinhashingSignature
      tailMinHashSignature = L.drop rows transposedMinhashingSignature
    in
      L.insert headMinHashSignature (localitySensitiveHashing' tailMinHashSignature bands rows)
