module Blaze.FirSpec where

import Blaze.Prelude

import Blaze.Fir
import Blaze.Types.Fir
import Test.Hspec
import Blaze.Types.Graph.Alga (AlgaGraph)
import qualified Blaze.Types.Graph as G
import Blaze.Types.Graph (Edge)


ifChainEdges1 :: [Edge Char]
ifChainEdges1 = G.fromTupleEdge <$>
  [ ('a', 'b')
  , ('a', 's')
  , ('b', 'c')
  , ('b', 's')
  , ('c', 'd')
  , ('c', 's')
  , ('d', 'f')
  , ('d', 's')
  ]

ifChainEdges2 :: [Edge Char]
ifChainEdges2 = G.fromTupleEdge <$>
  [ ('m', 'n')
  , ('n', 'x')
  , ('n', 'o')
  , ('o', 'x')
  , ('o', 'p')
  ]

comboEdges :: [Edge Char]
comboEdges = ifChainEdges1 <> ifChainEdges2 <>
  (G.fromTupleEdge <$>
   [ ('z', 'a')
   , ('y', 'a')
   , ('w', 'z')
   , ('w', 'y')
   , ('p', 'q')
   , ('f', 'q')
   , ('s', 'q')
   ])

singleIfChainGraph :: AlgaGraph () () Char
singleIfChainGraph = G.fromEdges . fmap (G.LEdge ()) $ ifChainEdges1

comboIfChainGraph :: AlgaGraph () () Char
comboIfChainGraph = G.fromEdges . fmap (G.LEdge ()) $ comboEdges

singleIfChainGraph2 :: AlgaGraph () () Char
singleIfChainGraph2 = G.fromEdges . fmap (G.LEdge ()) $ G.Edge 'z' 'a' : ifChainEdges1

spec :: Spec
spec = describe "Blaze.Fir" $ do

  context "if chains" $ do
    it "should detect an obvious if-chain" $ do
      let answer = [IfChain {_commonEscape = 's', _destination = 'f', _nodes = "abcd"}]
      getIfChains singleIfChainGraph `shouldBe` answer
    it "should detect two if-chains" $ do
      let answer = [ IfChain {_commonEscape = 's', _destination = 'f', _nodes = "abcd"}
                   , IfChain {_commonEscape = 'x', _destination = 'p', _nodes = "no"}
                   ]
      getIfChains comboIfChainGraph `shouldBe` answer

  context "toFirGraph" $ do
    it "should convert an obvious if-chain" $ do
      let chains = getIfChains singleIfChainGraph
          g = toFirGraph chains singleIfChainGraph :: AlgaGraph (FirEdgeLabel ()) () (FirNode Char)
          answer = [ G.fromTupleLEdge
                     ( ChainDestinationEdge
                     , ( FirIfChain (IfChain { _commonEscape = 's'
                                             , _destination = 'f'
                                             ,  _nodes = "abcd"})
                       , FirBasicBlock 'f'))
                   , G.fromTupleLEdge
                     ( ChainEscapeEdge
                     , (FirIfChain (IfChain { _commonEscape = 's'
                                            , _destination = 'f'
                                            , _nodes = "abcd"})
                       , FirBasicBlock 's'))]
      G.edges g `shouldBe` answer

    return ()


