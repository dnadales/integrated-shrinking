import IntegratedShrinking as HH

magicLength :: Word
magicLength = 50

main = HH.checkIntegrated listListsWordGen
                          noMagicLengths
  where
    listListsWordGen :: HH.Integrated [[Word]]
    listListsWordGen = HH.iList (HH.iWord (2 * magicLength)) aGen
      where
        aGen :: HH.Integrated [Word]
        aGen = HH.iList (HH.iWord (2 * magicLength)) intGen
          where
            intGen :: HH.Integrated Word
            intGen = HH.iWord 100


    noMagicLengths :: [[Word]] -> Bool
    noMagicLengths xs
      =  length xs <= fromIntegral magicLength
      || all ((<= magicLength) . fromIntegral . length) xs
