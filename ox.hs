{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, AllowAmbiguousTypes, FunctionalDependencies, ScopedTypeVariables #-}

import Data.Foldable ( maximumBy, msum )
import Control.Arrow ( (***), (&&&), first, second )
import Data.Bits ( (.&.) )

-- General minimax code

class (Show g, Num o, Ord o) => Game g m o | g->m, g->o where -- m is a move, o is outcome of game (o could just be Int)
    start :: g                    -- Initial state of new game
    who :: g -> Bool              -- Who's turn is it: players are called False and True
    options :: g -> [m]           -- List of possible moves
    move :: g -> m -> g           -- Apply a move to the game state to get the subsequent game state
    outcome :: g -> Maybe o       -- Nothing if ongoing, else score relative to player who's turn it is, 0=draw
    ask :: g -> IO (Maybe m)      -- Ask human to choose a move

    suggest :: g -> (Maybe m, o)  -- Choose move using non-pruned minimax with lookahead to end of game
    suggest g = maybe             -- The minimax algorithm
        -- Make pairs of current player's move and eventual result for current player. :: [(Maybe m, o)]
        -- Calcuate each possible future by applying move, recursing, then rewriting: reply move -> considered move, outcome -> -outcome
        -- Choose any move with outcome at least as good as any other :: (Maybe m, o). Could prune search when a forceable win is discovered. 
        ( maximumBy (\(_,o1) (_,o2) -> compare o1 o2) $ map ( Just &&& (negate.snd.suggest.(move g)) ) ( options g ) )
        ( \o -> (Nothing, o) ) -- If game ended, return outcome and no move
        ( outcome g ) -- Nothing means game still underway

    play :: g -> IO () -- Interactive loop - this is just a default impl - it belongs at a more specialised level really
    play g = maybe  -- If game still underway
        (   putStrLn (show g) >>  -- Print board
            -- False is human so ask, True is computer so use IO-wrapped minimax ...
            (if not (who g) then ask g else (return.fst) (suggest g)) >>= -- Maybe m just popped out of IO 
            maybe (return ()) (play . move g) -- If we got Just a move, play it and recurse, else exit
            ) 
        ( \o -> (putStrLn.show) g >>  putStrLn (gloat o (who g)) ) -- It's over so print the result and exit
        (outcome g) 
        where gloat o w | o==0 = "Draw"
                        | w = "You won"
                        | otherwise = "I won"
        
        
-- Noughts and Crosses starts here

type Loc = (Int,Int) -- E.g. north east is (0,2), only needed for printing
type Oxm =  (String,Int,Loc) -- String is for looking up user input. The Int is the important bit
-- That Int is a field of 8 doublebits each representing a win type e.g.:
toprow  = 2^0;    midrow = 2^2;    botrow   = 2^4
leftcol = 2^6;    midcol = 2^8;    rightcol = 2^10
foreslash = 2^12; backslash = 2^14
-- A move (i.e. square) has 1 (not 2 or 3) in each doublebit of a win type that that move can contribute to
-- We can just add those into each user's state when he moves. 
-- A win is when any doublebit equals 3 in the state of whoever just played
allcells :: [Oxm]
allcells = [ 
    ("nw", toprow+leftcol+backslash,(0,0)), ("n", toprow+midcol,(0,1)),                     ("ne", toprow+rightcol+foreslash,(0,2)),
    ("w" , midrow+leftcol,(1,0)),           ("c", midrow+midcol+foreslash+backslash,(1,1)), ("e" , midrow+rightcol,(1,2)),
    ("sw", botrow+leftcol+foreslash,(2,0)), ("s", botrow+midcol,(2,1)),                     ("se", botrow+rightcol+backslash,(2,2))
    ]

data Ox = Ox {            -- Game state
    computeron :: Bool,   -- Who's turn
    taken :: (Int,Int),   -- The totals for each player: (human, computer)
    avail :: [Oxm],       -- The available moves
    board :: [String] }   -- Just for printing

instance Show Ox where show g = (msum $ board g) -- Draws the board
           
instance Game Ox Oxm Int where
    start = Ox False (0,0) allcells [" | | \n","-+-+-\n"," | | \n","-+-+-\n"," | | \n"]
    who = computeron
    options = avail

    move g x@(_,m,s) = Ox -- Apply move to evolve gamestate
        ( not $ computeron g ) -- Other player's turn
        ( (if computeron g then second else first) (+m) (taken g) ) -- Add the move's int into the player's total
        ( filter ((/=x)) (avail g) ) -- Take the move out of the available list
        ( let b = board g in poke (2*fst s) (poke (2*snd s) (if computeron g then 'O' else 'X') (b!!(2*fst s)) ) b ) -- Update the affected printable string

    outcome g | won g          = Just (-1)    -- Outcome is from point of view of the player who's turn it is - a complete line means he lost
              | null (avail g) = Just 0       -- No moves left so draw
              | otherwise      = Nothing      -- No outcome yet
                  where won g  = woni $ ( if computeron g then fst else snd ) $ taken g -- Find the total for the player who just played
                        woni i = ((i.&.3)==3) || ((i>0) && woni (div i 4)) -- Look for complete doublebits

    ask g = getLine >>= \l -> -- Get a line of input
            case filter ( (==l).(\(x,_,_)->x) ) $ avail g of -- Match input against first field in available move tuples
                [] -> putStrLn "Invalid input" >> ask g
                (m:_) -> return (Just m) 

main = play (start :: Ox)

-- A crumb that probably exists somewhere already ...
poke :: Int -> a -> [a] -> [a] -- overwrite a member of a list
poke pos nv l = a++(nv:b) where (a,(_:b)) = splitAt pos l


