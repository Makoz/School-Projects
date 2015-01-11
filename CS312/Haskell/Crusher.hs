import Data.List
import Debug.Trace
-- Group Project By:
-- Chen Hao (John) Hsu, Ellis Ho

-- FLAG EXPLANATION:
-- 0 top half including half
-- 1 = bottom half 

{-- Sample Test data
test1 = "WWW-WW-------BB-BBB" -- n=3 
test2 = "WW---BB"		-- n=2
test3 = "WWWW-BBB--WWWW---BBB---WWW---BBB-WWWW" -- n = 4
test4 = "WWW------B------BBB"
test5 = "WWW-W----BBB--B-BB-" --n=3
sepTest1 = separateGame test1 3 3 0 []
sepTest2 = separateGame test4 3 3 0 []
sepTest3 = separateGame test3 4 4 0 []
sepTest4 = separateGame test5 3 3 0 []
game1	 = ["W-W","-BB-","WBBBW","-BB-", "W-W"]

sepTestA = separateGame "---B---" 2 2 0 []
--}
{--
ls - list of previous states
side - Black/White
depth - how far we search
n - edge size 

crusher :: [String]->Char->Int->Int->[String]
crusher ls side depth n = null
--}

-- crusher :: [String]->Char->Int->Int->[String]
crusher :: [String]->Char->Int->Int->[String]
crusher history side depth n
	= (minmax (separateGame (head history) n n 0 []) (historySeperator history n []) side depth n):history

-- minmax :: [String]->[[String]]->Char->Int->Int->String
minmax :: [String]->[[String]]->Char->Int->Int->String
minmax gameState history side depth n
	| depth == 0		= concat gameState
	| side == 'B'		= miniMaxHelper gameState (generateAllNewStates gameState gameState 0 0 n 0 side (gameState:history) []) (gameState:history) side 'W' depth n (-1200, gameState)
	| otherwise		= miniMaxHelper gameState (generateAllNewStates gameState gameState 0 0 n 0 side (gameState:history) []) (gameState:history) side 'B' depth n (-1200, gameState)

miniMaxHelper :: [String]->[[String]]->[[String]]->Char->Char->Int->Int->(Int ,[String])->String
miniMaxHelper currState possibleMoves history side enemySide depth n tupleacc
	|  (null possibleMoves) = concat (snd tupleacc)
	|	otherwise	= miniMaxHelper currState (tail possibleMoves) history side enemySide depth n (maximum [((maxGen (head possibleMoves) (generateAllNewStates currState currState 0 0 n 0 side ((head possibleMoves):history) []) ((head possibleMoves):history) side enemySide (depth-1) n []), (head possibleMoves)) , tupleacc])

maxGen :: [String]->[[String]]->[[String]]->Char->Char->Int->Int->[Int]->Int
maxGen currState possibleMoves history side enemyside depth n acc
	| (null possibleMoves) && (depth /= 0)	= -1000
	| depth == 0		= (valueOfGameState currState n side enemyside)
	| (null possibleMoves)	= maximum acc
	| otherwise		= maxGen currState (tail possibleMoves) history side enemyside depth n ((minGen (head possibleMoves) (generateAllNewStates (head possibleMoves) (head possibleMoves) 0 0 n 0 side ((head possibleMoves):history) []) ((head possibleMoves):history) side enemyside (depth-1) n []):acc) 
																																																			
minGen :: [String]->[[String]]->[[String]]->Char->Char->Int->Int->[Int]->Int
minGen currState possibleMoves history side enemyside depth n acc
	| (null possibleMoves) && (depth /= 0)	= 1000
	| depth == 0		= valueOfGameState currState n side enemyside
	| (null possibleMoves)	= minimum acc
	| otherwise		= minGen currState (tail possibleMoves) history side enemyside depth n ((maxGen (head possibleMoves) (generateAllNewStates (head possibleMoves) (head possibleMoves) 0 0 n 0 side (currState:history) []) (currState:history) side enemyside (depth-1) n []):acc)
	
{--
*** MOVE GENERATION THOUGHTS *** 
Each of these should check the moveable conditions:
If hit another W, check if jumpable
Otherwise just move appropriately
Edge Cases in the Shift Movements can be checked for by keeping in mind the edge length n
and the largest row length 
-- gs = gamestate
-- side = 'W' / 'B'
-- Might be able to use foldr/foldl to manipulate all at once...
 MoveGeneration 
moveGeneration gamestate index row
When generateAllnewStates is called it should just be generateAllNewGamestates gameState (head gameState) [] head(tail gameState) .... 
-- default switch = 0
--}
-- Orig flag = 0

generateAllNewStates :: [String]->[String]->Int->Int->Int->Int->Char->[[String]]->[[String]]->[[String]]
generateAllNewStates gameState origGS rowNum flag n switch side history acc
	| null gameState			= filterHistory acc history [] -- add a function to prune acc with history later done in generatemoves
  | ((flag == 1) || (switch == 1))		= generateAllNewStates (tail gameState) origGS (rowNum+1) 1 n switch side history (concat[(generateMoves (take rowNum origGS) (origGS !! rowNum) (drop (rowNum+1) origGS) flag (findIndices (==side) (origGS !! rowNum)) side []), acc])
  | ((length (head gameState)) == (2*n -1)) 	= generateAllNewStates (tail gameState) origGS (rowNum+1) 1 n 1 side history (concat[(generateMoves (take rowNum origGS) (origGS !! rowNum) (drop (rowNum+1) origGS) flag (findIndices (==side) (origGS !! rowNum)) side []), acc])
	| otherwise				= generateAllNewStates (tail gameState) origGS (rowNum+1) 0 n switch side history (concat[(generateMoves (take rowNum origGS) (origGS !! rowNum) (drop (rowNum+1) origGS) flag (findIndices (==side) (origGS !! rowNum)) side []), acc]) --(generateMoves --SOEMTHINGELSE):acc

generateMoves :: [String]->String->[String]->Int->[Int]->Char->[[String]]->[[String]]
generateMoves prev currRow rest flag lsIdx side acc
	| null lsIdx		= filter (/= []) acc
	| null prev			= generateMoves prev currRow rest flag (tail lsIdx) side (concat[acc, moveLeft prev currRow rest (head lsIdx) side, moveRight prev currRow rest (head lsIdx) side, moveBottomLeft prev currRow (head rest) (tail rest) (head lsIdx) flag side, moveBottomRight prev currRow (head rest) (tail rest) flag (head lsIdx) side]) -- No TopLeft/TopRight movement
	| null rest			= generateMoves prev currRow rest flag (tail lsIdx) side (concat[acc, moveLeft prev currRow rest (head lsIdx) side, moveRight prev currRow rest (head lsIdx) side, moveTopLeft (reverse(tail(reverse(prev)))) (last prev) currRow rest (head lsIdx) flag side, moveTopRight (reverse(tail(reverse(prev)))) (last prev) currRow rest (head lsIdx) flag side]) -- No BottomLeft/BottomRight  -- add Top right 
	| otherwise			= generateMoves prev currRow rest flag (tail lsIdx) side (concat[acc, moveLeft prev currRow rest (head lsIdx) side, moveRight prev currRow rest (head lsIdx) side, moveTopLeft (reverse(tail(reverse(prev)))) (last prev) currRow rest (head lsIdx) flag side, moveTopRight (reverse(tail(reverse(prev)))) (last prev) currRow rest (head lsIdx) flag side, moveBottomLeft prev currRow (head rest) (tail rest) flag (head lsIdx) side, moveBottomRight prev currRow (head rest) (tail rest) flag (head lsIdx) side])

moveBottomRight :: [String]->String->String->[String]->Int->Int->Char->[[String]]
moveBottomRight prev currRow next rest flag lsIdx side
	| (length rest) < 1		= [moveBottomRight1 prev (reverse currRow) (reverse next) rest ((length currRow)-1-lsIdx) flag side]
	| otherwise			= (moveBottomRight1 prev (reverse currRow) (reverse next) rest ((length currRow)-1-lsIdx) flag side):[moveBottomRight2 prev (reverse currRow) (reverse next) (reverse (head rest)) (tail rest) ((length currRow)-1-lsIdx) flag side]

moveBottomRight1 :: [String]->String->String->[String]->Int->Int->Char->[String]	
moveBottomRight1 prev curr next rest index flag side
	| (index == 0) && (flag == 1)														= []
	| (null rest) && (flag == 0) && ((2*(length(head prev))-1) == (length curr)) && (index >=1) && ((next !! (index-1)) == '-')		= concat[prev, [reverse (replaceAtIndex curr index '-')], [reverse (replaceAtIndex next (index-1) side)], rest] -- middle row   
	| (not (null rest)) && (flag == 0) && ((2*(length(last rest))-1) /= (length curr)) && ((next !! index) == '-')				= concat[prev, [reverse (replaceAtIndex curr index '-')], [reverse (replaceAtIndex next index side)], rest]
	| (not (null rest)) && (flag == 0) && ((2*(length(last rest))-1) == (length curr)) && (index >=1) && ((next !! (index-1)) == '-')	= concat[prev, [reverse (replaceAtIndex curr index '-')], [reverse (replaceAtIndex next (index-1) side)], rest] -- middle row
	| (flag == 1) && (index >= 1) && (next !! (index-1)) == '-'										= concat[prev,[reverse (replaceAtIndex curr index '-')], [reverse (replaceAtIndex next (index-1) side)], rest]
	| otherwise																										= []

moveBottomRight2 :: [String]->String->String->String->[String]->Int->Int->Char->[String]
moveBottomRight2 prev curr next next2 rest index flag side		
	| (flag == 1) && (index >= 2) && ((next !! (index-1)) == side) && ((next2 !! (index-2)) /= side) 									= concat [prev, [reverse (replaceAtIndex curr index '-')], [reverse next], [reverse (replaceAtIndex next2 (index-2) side)], rest]
	| (not (null prev)) && (length curr) == (2*(length (head prev))-2) && (flag == 0) && (index >= 1) && (next !! (index-1)) == side && (next2 !! (index-2)) /= side	= concat[prev,[reverse (replaceAtIndex curr index '-')], [reverse next], [reverse (replaceAtIndex next2 (index-2) side)], rest]
	| (not (null prev)) && (length curr) == (2*(length (head prev))-1) && (index >= 2) && (next !! (index-1)) == side && (next2 !! (index-2)) /= side			= concat[prev, [reverse (replaceAtIndex curr index '-')], [reverse next], [reverse(replaceAtIndex next2 (index-2) side)], rest]
	| (flag == 0) && (length curr) < (length next2) &&(next !! index == side) && (next2 !! index) /= side && (length curr) /= (length next2)				= concat [prev, [reverse (replaceAtIndex curr index '-')], [reverse next], [reverse (replaceAtIndex next2 index side)], rest]
	| (flag == 0) && (length curr) == (length next2) && (next !! index) == side && (next2 !! (index-1)) /= side								= concat[prev,[reverse (replaceAtIndex curr index '-')], [reverse next], [reverse (replaceAtIndex next2 (index-1) side)], rest]	 
	| otherwise		= []

	
moveBottomLeft :: [String]->String->String->[String]->Int->Int->Char->[[String]]
moveBottomLeft prev currRow next rest flag lsIdx side
    | (length rest) < 1			= [moveBottomLeft1 prev currRow next rest lsIdx flag side]
	| otherwise			= (moveBottomLeft1 prev currRow next rest lsIdx flag side):[moveBottomLeft2 prev currRow next (head rest) (tail rest) lsIdx flag side]

moveBottomLeft1 :: [String]->String->String->[String]->Int->Int->Char->[String]
moveBottomLeft1 prev curr next rest index flag side
	| (index == 0) && (flag == 1)														= []
	| (null rest) && (flag == 0) && ((2*(length(head prev))-1) == (length curr)) && (index >=1) && ((next !! (index-1)) == '-')		= concat[prev, [(replaceAtIndex curr index '-')], [(replaceAtIndex next (index-1) side)], rest] -- middle row
	| (not (null rest)) && (flag == 0) && ((2*(length(last rest))-1) /= (length curr)) && ((next !! index) == '-')				= concat[prev, [(replaceAtIndex curr index '-')], [(replaceAtIndex next index side)], rest]
	| (not (null rest)) &&  (flag == 0) && ((2*(length(last rest))-1) == (length curr)) && (index >=1) && ((next !! (index-1)) == '-')	= concat[prev, [(replaceAtIndex curr index '-')], [(replaceAtIndex next (index-1) side)], rest] -- middle row
	| (flag == 1) && (index >= 1) && (next !! (index-1)) == '-'										= concat[prev,[(replaceAtIndex curr index '-')], [replaceAtIndex next (index-1) side], rest]
	| otherwise																										= []

moveBottomLeft2 :: [String]->String->String->String->[String]->Int->Int->Char->[String]
moveBottomLeft2 prev curr next next2 rest index flag side		
	| (flag == 1) && (index >= 2) && ((next !! (index-1)) == side) && ((next2 !! (index-2)) /= side) 									= concat [prev, [(replaceAtIndex curr index '-')], [next], [(replaceAtIndex next2 (index-2) side)], rest]
	| (not (null prev)) && (length curr) == (2*(length (head prev))-2) && (flag == 0) && (index >= 1) && (next !! (index-1)) == side && (next2 !! (index-2)) /= side	= concat[prev,[replaceAtIndex curr index '-'], [next], [replaceAtIndex next2 (index-2) side], rest]
	| (not (null prev)) && (length curr) == (2*(length (head prev))-1) && (index >= 2) && (next !! (index-1)) == side && (next2 !! (index-2)) /= side			= concat[prev, [replaceAtIndex curr index '-'], [next], [replaceAtIndex next2 (index-2) side], rest]
	| (flag == 0) && (length curr) < (length next2) &&(next !! index == side) && (next2 !! index) /= side && (length curr) /= (length next2)				= concat [prev, [(replaceAtIndex curr index '-')], [next], [replaceAtIndex next2 index side], rest]
	| (flag == 0) && (length curr) == (length next2) && (next !! index) == side && (next2 !! (index-1)) /= side								= concat[prev,[(replaceAtIndex curr index '-')], [next], [replaceAtIndex next2 (index-1) side], rest]	 
	| otherwise																				= []
	
moveTopLeft :: [String]->String->String->[String]->Int->Int->Char->[[String]]
moveTopLeft prev rowprev curr rest index flag side
	| length(prev) < 1						= [moveTopLeft1 prev rowprev curr rest index flag side]
	| otherwise							= (moveTopLeft1 prev rowprev curr rest index flag side):[(moveTopLeft2 (reverse(tail(reverse(prev)))) (last prev) rowprev curr rest index flag side)]
	

-- TODO: SHOULDNT NEED TO CHECK IF CURR !! INDEX == SIDE ANYMORE! CHECK LATER IF WE CAN REMOVE

moveTopLeft2 :: [String]->String->String->String->[String]->Int->Int->Char->[String]
moveTopLeft2	prev rowprev2 rowprev curr rest index flag side	
	| (flag == 0) && (index >= 2) && ((curr !! index) == side) && ((rowprev !! (index-1)) == side) && ((rowprev2 !! (index-2)) /= side) 												  = concat [prev, [(replaceAtIndex rowprev2 (index-2) side)], [rowprev], [replaceAtIndex curr index '-'], rest] 
	| (null prev) && (flag == 1) && (index >= 1) && ((curr !! index) == side) && ((rowprev !! index) == side) && ((rowprev2 !! (index-1)) /= side)											  = concat [prev, [(replaceAtIndex rowprev2 (index-1) side)], [rowprev], [replaceAtIndex curr index '-'], rest] -- think flag is necessary reconfirm
	| (not (null prev)) && (flag == 1) && (length(curr) == ((2*(length(head prev))-2))) &&  (index >= 1) && ((curr !! index) == side) && ((rowprev !! index) == side) && ((rowprev2 !! (index-1)) /= side)				  = concat[prev, [(replaceAtIndex rowprev2 (index-1) side)], [rowprev], [replaceAtIndex curr index '-'],rest] -- identifies that we're in the row below mid row 
	| (not (null prev)) && (flag == 1) && (length(curr) <  ((2*(length(head prev))-2))) && (length(curr) /= (2*(length (head prev)-1))) && ((curr !! index) == side) && ((rowprev !! index) == side) && ((rowprev2 !! index) /= side) = concat[prev,[(replaceAtIndex rowprev2 index side)], [rowprev], [replaceAtIndex curr index '-'], rest]
	| otherwise		= []

	

moveTopLeft1 :: [String]->String->String->[String]->Int->Int->Char->[String]
moveTopLeft1 prev rowprev curr rest index flag side
	| (index == 0) && (flag == 0)							= []
	| (flag == 0) && ((curr !! index) == side) && ((rowprev !! (index-1)) == '-')	= concat[prev, [(replaceAtIndex rowprev (index-1) side)], [replaceAtIndex curr index '-'], rest]
	| (flag == 1) &&  ((curr !! index) == side) && (((rowprev !! index)) == '-')	= concat[prev,[(replaceAtIndex rowprev (index) side)], [replaceAtIndex curr index '-'],rest]
	| otherwise									= []


-- moveTopRight1/2 should just be moveTopLeft but on input, you reverse prev, rowprev, and curr and when you reconcat, you re-reverse

moveTopRight :: [String]->String->String->[String]->Int->Int->Char->[[String]]
moveTopRight prev rowprev curr rest index flag side
	| length(prev) <1		= [moveTopRight1 prev (reverse rowprev) (reverse curr) rest ((length curr)-1-index) flag side]
	| otherwise			= (moveTopRight1 prev (reverse rowprev) (reverse curr) rest ((length curr) - 1 - index) flag side):[(moveTopRight2 (reverse(tail(reverse(prev)))) (reverse (last prev)) (reverse rowprev) (reverse curr) rest ((length curr) - 1 - index) flag side)]

moveTopRight1 :: [String]->String->String->[String]->Int->Int->Char->[String]
moveTopRight1 prev rowprev curr rest index flag side
	| (index == 0) && (flag == 0)							= []
	| (flag == 0) && ((curr !! index) == side) && ((rowprev !! (index-1)) == '-')	= concat[prev, [reverse (replaceAtIndex rowprev (index-1) side)], [reverse (replaceAtIndex curr index '-')], rest]
	| (flag == 1) &&  ((curr !! index) == side) && (((rowprev !! index)) == '-')	= concat[prev,[reverse (replaceAtIndex rowprev (index) side)], [reverse (replaceAtIndex curr index '-')],rest]
	| otherwise									= []
	

moveTopRight2 :: [String]->String->String->String->[String]->Int->Int->Char->[String]	
moveTopRight2 prev rowprev2 rowprev curr rest index flag side	
	| (flag == 0) && (index >= 2) && ((curr !! index) == side) && ((rowprev !! (index-1)) == side) && ((rowprev2 !! (index-2)) /= side) 												  = concat [prev, [reverse (replaceAtIndex rowprev2 (index-2) side)], [(reverse rowprev)], [reverse (replaceAtIndex curr index '-')], rest] 
	| (null prev) && (flag == 1) && (index >= 1) && ((curr !! index) == side) && ((rowprev !! index) == side) && ((rowprev2 !! (index-1)) /= side)											  = concat [prev, [reverse (replaceAtIndex rowprev2 (index-1) side)], [reverse rowprev], [reverse (replaceAtIndex curr index '-')], rest] -- think flag is necessary reconfirm
	| (not (null prev)) && (flag == 1) && (length(curr) == ((2*(length(head prev))-2))) &&  (index >= 1) && ((curr !! index) == side) && ((rowprev !! index) == side) && ((rowprev2 !! (index-1)) /= side)				  = concat[prev, [reverse (replaceAtIndex rowprev2 (index-1) side)], [reverse rowprev], [reverse (replaceAtIndex curr index '-')],rest] -- identifies that we're in the row below mid row 
	| (not (null prev)) && (flag == 1) && (length(curr) <  ((2*(length(head prev))-2))) && (length(curr) /= (2*(length (head prev)-1))) && ((curr !! index) == side) && ((rowprev !! index) == side) && ((rowprev2 !! index) /= side) = concat[prev,[reverse (replaceAtIndex rowprev2 index side)], [reverse rowprev], [reverse (replaceAtIndex curr index '-')], rest]
	| otherwise																											  = []	


-- ** move left and right both work
moveLeft :: [String]->String->[String]->Int->Char->[[String]]
moveLeft prev curr rest index side
	= (moveLeft1 prev curr rest index side):[(moveLeft2 prev curr rest index side)]

	
	
moveLeft1 :: [String]->String->[String]->Int->Char->[String]
moveLeft1 prev curr rest index side
	| index == 0							= []
	| ((curr !! index) == side) && (curr !! (index-1) == '-')	= concat [prev, [replaceAtIndex (replaceAtIndex curr (index-1) side) index '-'], rest]
	| otherwise							= []
	
moveLeft2 :: [String]->String->[String]->Int->Char->[String]
moveLeft2 prev curr rest index side
	| index < 2											= []
	| ((curr !! index) == side) && ((curr !! (index-1)) == side) && ((curr !! (index-2)) /= side)	= concat [prev, [replaceAtIndex (replaceAtIndex curr(index-2) side) index '-'], rest]
	| otherwise											= []

moveRight :: [String]->String->[String]->Int->Char->[[String]]
moveRight prev curr rest index side
	= (moveRight1 prev curr rest index side):[(moveRight2 prev curr rest index side)]
	
moveRight1 :: [String]->String->[String]->Int->Char->[String]
moveRight1 prev curr rest index side
	| index > ((length curr) - 2)					= []
	| ((curr !! index) == side) && (curr !! (index+1) == '-')	= concat [prev, [replaceAtIndex (replaceAtIndex curr (index+1) side) index '-'], rest]
	| otherwise							= []
	
moveRight2 :: [String]->String->[String]->Int->Char->[String]
moveRight2 prev curr rest index side
	| index > ((length curr) - 3)									= []
	| ((curr !! index) == side) && ((curr !! (index+1)) == side) && ((curr !! (index+2)) /= side)	= concat [prev, [replaceAtIndex (replaceAtIndex curr(index+2) side) index '-'], rest]
	| otherwise											= []
	
filterHistory :: [[String]]->[[String]]->[[String]]->[[String]]
filterHistory gen history acc
	| null gen			= acc
	| (elem (head gen) history)	= filterHistory (tail gen) history acc
	| otherwise			= filterHistory (tail gen) history ((head gen):acc)
	
-- END MOVE GENERATION 
	
-- replaceAtIndex str ind side
-- takes in a char for side
replaceAtIndex [] n side	= []
replaceAtIndex (x:xs) 0 side	= side:xs
replaceAtIndex (x:xs) y z	= x:replaceAtIndex xs (y-1) z

-- generateAllNewStates gameState origGS rowNum flag switch n side history acc
-- gamestate is broken up ie. ["--","----"]

-- Each additional of your own side = +10
-- Each additional for opponent = -10
-- Each additional set up kill = +5
-- If you set opponent up  with a crush = -7 (heavier because opponent can remove on next)
-- generateAllNewStates gameState origGS rowNum flag switch n side history acc

valueOfGameState :: [String]->Int->Char->Char->Int
valueOfGameState gameState n mySide enemySide	
	| 0 == (generateMoveCount gameState gameState 0 0 0 n enemySide mySide 0) =  1000 -- enemy is unable to generate more states
	| (length (findIndices (==enemySide) (concat gameState))) == 0		  = 1000 -- enemy has no more pieces remaining
	| otherwise								  = valueHelper gameState n mySide enemySide

valueHelper :: [String]->Int->Char->Char->Int
valueHelper gameState n mySide enemySide
	= (diffHelper (concat gameState) mySide enemySide) + crushCounter gameState mySide enemySide n - 2*(generateMoveCount gameState gameState 0 0 0 n enemySide mySide 0) + (generateMoveCount gameState gameState 0 0 0 n mySide enemySide 0) -- number of moves opponent can make!
	
-- passed the gamestate as string
-- assigns a value to the difference in pieces
diffHelper :: String->Char->Char->Int
diffHelper gameState mySide enemySide
	= 10*((length (findIndices (==mySide) gameState)) - (length (findIndices (==enemySide) gameState)))
	
-- number of moves enemy can make if number of single space moves + jumps == 0 enemy lost!	
generateMoveCount :: [String]->[String]->Int->Int->Int->Int->Char->Char->Int->Int
generateMoveCount gameState origGS rowNum flag switch n side enemySide acc
	| null gameState					= acc
	| (flag == 1) || (switch == 1)				= generateMoveCount (tail gameState) origGS (rowNum+1) 1 n switch side enemySide ((generateCrushcounts (take rowNum origGS) (origGS !! rowNum) (drop (rowNum+1) origGS) flag (findIndices (==side) (origGS !! rowNum)) side enemySide 0) + acc)
	| ((length (head gameState)) == (2*n -1)) 		= generateMoveCount (tail gameState) origGS (rowNum+1) 0 n 1 side enemySide ((generateCrushcounts (take rowNum origGS) (origGS !! rowNum) (drop (rowNum+1) origGS) flag (findIndices (==side) (origGS !! rowNum)) side enemySide 0) + acc)
	| otherwise						= generateMoveCount (tail gameState) origGS (rowNum+1) 0 n switch side enemySide ((generateCrushcounts (take rowNum origGS) (origGS !! rowNum) (drop (rowNum+1) origGS) flag (findIndices (==side) (origGS !! rowNum)) side enemySide 0) + acc)

generateCrushcounts :: [String]->String->[String]->Int->[Int]->Char->Char->Int->Int
generateCrushcounts prev currRow rest flag lsIdx side enemySide acc
	| null lsIdx			= acc																																																																																				
	| null prev			= generateCrushcounts prev currRow rest flag (tail lsIdx) side enemySide (acc + (countLeftCrush prev currRow rest (head lsIdx) side enemySide) + (countRightCrush prev currRow rest (head lsIdx) side enemySide)+ (countBottomLeftCrush prev currRow (head rest) (tail rest) (head lsIdx) flag side enemySide) + (countBottomRightCrush prev currRow (head rest) (tail rest) (head lsIdx) flag side enemySide)) -- No TopLeft/TopRight countment
	| null rest			= generateCrushcounts prev currRow rest flag (tail lsIdx) side enemySide (acc + (countLeftCrush prev currRow rest (head lsIdx) side enemySide) + (countRightCrush prev currRow rest (head lsIdx) side enemySide)+ (countTopLeftCrush (reverse(tail(reverse(prev)))) (last prev) currRow rest (head lsIdx) flag side enemySide) + ( countTopRightCrush (reverse(tail(reverse(prev)))) (last prev) currRow rest (head lsIdx) flag side enemySide)) -- No BottomLeft/BottomRight 
	| otherwise			= generateCrushcounts prev currRow rest flag (tail lsIdx) side enemySide (acc + (countLeftCrush prev currRow rest (head lsIdx) side enemySide) + (countRightCrush prev currRow rest (head lsIdx) side enemySide)+ (countTopLeftCrush (reverse(tail(reverse(prev)))) (last prev) currRow rest (head lsIdx) flag side enemySide)+( countTopRightCrush (reverse(tail(reverse(prev)))) (last prev) currRow rest (head lsIdx) flag side enemySide)+ (countBottomLeftCrush prev currRow (head rest) (tail rest) flag (head lsIdx) side enemySide)+ (countBottomRightCrush prev currRow (head rest) (tail rest) flag (head lsIdx) side enemySide)) -- Enable all countments	
	
countBottomRightCrush :: [String]->String->String->[String]->Int->Int->Char->Char->Int
countBottomRightCrush prev currRow next rest flag lsIdx side enemySide
	| (length rest) < 1			= countBottomRight1Crush prev (reverse currRow) (reverse next) rest ((length currRow)-1-lsIdx) flag side 
	| otherwise				= (countBottomRight2Crush prev (reverse currRow) (reverse next) (reverse (head rest)) (tail rest) ((length currRow)-1-lsIdx) flag side enemySide) + countBottomRight1Crush prev (reverse currRow) (reverse next) rest ((length currRow)-1-lsIdx) flag side 

countBottomRight1Crush :: [String]->String->String->[String]->Int->Int->Char->Int
countBottomRight1Crush prev curr next rest index flag side
	| (index == 0) && (flag == 1)														= 0
	| (null rest) && (flag == 0) && ((2*(length(head prev))-1) == (length curr)) && (index >=1) && ((next !! (index-1)) == '-')		= 1
	| (not (null rest)) && (flag == 0) && ((2*(length(last rest))-1) /= (length curr)) && ((next !! index) == '-')				= 1
	| (not (null rest)) && (flag == 0) && ((2*(length(last rest))-1) == (length curr)) && (index >=1) && ((next !! (index-1)) == '-')	= 1
	| (flag == 1) && (index >= 1) && (next !! (index-1)) == '-'										= 1
	| otherwise																= 0																								

countBottomRight2Crush :: [String]->String->String->String->[String]->Int->Int->Char->Char->Int
countBottomRight2Crush prev curr next next2 rest index flag side enemySide		
	| (flag == 1) && (index >= 2) && ((next !! (index-1)) == side) && ((next2 !! (index-2)) /= side) 									=1
	| (not (null prev)) && (length curr) == (2*(length (head prev))-2) && (flag == 0) && (index >= 1) && (next !! (index-1)) == side && (next2 !! (index-2)) /= side	= 1
	| (not (null prev)) && (length curr) == (2*(length (head prev))-1) && (index >= 2) && (next !! (index-1)) == side && (next2 !! (index-2)) /= side			= 1
	| (flag == 0) && (length curr) < (length next2) &&(next !! index == side) && (next2 !! index) /= side && (length curr) /= (length next2)				= 1
	| (flag == 0) && (length curr) == (length next2) && (next !! index) == side && (next2 !! (index-1)) /= side								= 1
	| otherwise																				= 0

	
countBottomLeftCrush :: [String]->String->String->[String]->Int->Int->Char->Char->Int
countBottomLeftCrush prev currRow next rest flag lsIdx side enemySide
  	| (length rest) < 1		= countBottomLeft1Crush prev currRow next rest lsIdx flag side
	| otherwise			= (countBottomLeft2Crush prev currRow next (head rest) (tail rest) lsIdx flag side enemySide) + countBottomLeft1Crush prev currRow next rest lsIdx flag side
													
countBottomLeft1Crush :: [String]->String->String->[String]->Int->Int->Char->Int
countBottomLeft1Crush prev curr next rest index flag side
	| (index == 0) && (flag == 1)														= 0
	| (null rest) && (flag == 0) && ((2*(length(head prev))-1) == (length curr)) && (index >=1) && ((next !! (index-1)) == '-')		= 1
	| (not (null rest)) && (flag == 0) && ((2*(length(last rest))-1) /= (length curr)) && ((next !! index) == '-')				= 1
	| (not (null rest)) &&  (flag == 0) && ((2*(length(last rest))-1) == (length curr)) && (index >=1) && ((next !! (index-1)) == '-')	= 1
	| (flag == 1) && (index >= 1) && (next !! (index-1)) == '-'										= 1
	| otherwise																										= 0

countBottomLeft2Crush :: [String]->String->String->String->[String]->Int->Int->Char->Char->Int
countBottomLeft2Crush prev curr next next2 rest index flag side enemyside	
	| (flag == 1) && (index >= 2) && ((next !! (index-1)) == side) && ((next2 !! (index-2)) /= side) 									= 1
	| (not (null prev)) && (length curr) == (2*(length (head prev))-2) && (flag == 0) && (index >= 1) && (next !! (index-1)) == side && (next2 !! (index-2)) /= side	= 1
	| (not (null prev)) && (length curr) == (2*(length (head prev))-1) && (index >= 2) && (next !! (index-1)) == side && (next2 !! (index-2)) /= side			= 1
	| (flag == 0) && (length curr) < (length next2) &&(next !! index == side) && (next2 !! index) /= side && (length curr) /= (length next2)				= 1
	| (flag == 0) && (length curr) == (length next2) && (next !! index) == side && (next2 !! (index-1)) /= side								= 1
	| otherwise																				= 0

countTopLeftCrush :: [String]->String->String->[String]->Int->Int->Char->Char->Int
countTopLeftCrush prev rowprev curr rest index flag side enemySide
	| length(prev) < 1						= countTopLeft1Crush prev rowprev curr rest index flag side
	| otherwise							= (countTopLeft2Crush (reverse(tail(reverse(prev)))) (last prev) rowprev curr rest index flag side enemySide) + countTopLeft1Crush prev rowprev curr rest index flag side
	
countTopLeft1Crush :: [String]->String->String->[String]->Int->Int->Char->Int
countTopLeft1Crush prev rowprev curr rest index flag side
	| (index == 0) && (flag == 0)							= 0
	| (flag == 0) && ((curr !! index) == side) && ((rowprev !! (index-1)) == '-')	= 1
	| (flag == 1) &&  ((curr !! index) == side) && (((rowprev !! index)) == '-')	= 1
	| otherwise									= 0
	

countTopLeft2Crush :: [String]->String->String->String->[String]->Int->Int->Char->Char->Int
countTopLeft2Crush	prev rowprev2 rowprev curr rest index flag side	enemyside
	| (flag == 0) && (index >= 2) && ((curr !! index) == side) && ((rowprev !! (index-1)) == side) && ((rowprev2 !! (index-2)) /= side) 												  = 1
	| (null prev) && (flag == 1) && (index >= 1) && ((curr !! index) == side) && ((rowprev !! index) == side) && ((rowprev2 !! (index-1)) /= side)											  = 1
	| (not (null prev)) && (flag == 1) && (length(curr) == ((2*(length(head prev))-2))) &&  (index >= 1) && ((curr !! index) == side) && ((rowprev !! index) == side) && ((rowprev2 !! (index-1)) /= side)			          = 1
	| (not (null prev)) && (flag == 1) && (length(curr) <  ((2*(length(head prev))-2))) && (length(curr) /= (2*(length (head prev)-1))) && ((curr !! index) == side) && ((rowprev !! index) == side) && ((rowprev2 !! index) /= side) = 1
	| otherwise		= 0

countTopRightCrush :: [String]->String->String->[String]->Int->Int->Char->Char->Int
countTopRightCrush prev rowprev curr rest index flag side enemyside
	| length(prev) <1		= countTopRight1Crush prev (reverse rowprev) (reverse curr) rest ((length curr)-1-index) flag side
	| otherwise			= (countTopRight2Crush (reverse(tail(reverse(prev)))) (reverse (last prev)) (reverse rowprev) (reverse curr) rest ((length curr) - 1 - index) flag side enemyside) + countTopRight1Crush prev (reverse rowprev) (reverse curr) rest ((length curr)-1-index) flag side

countTopRight1Crush :: [String]->String->String->[String]->Int->Int->Char->Int
countTopRight1Crush prev rowprev curr rest index flag side
	| (index == 0) && (flag == 0)							= 0
	| (flag == 0) && ((curr !! index) == side) && ((rowprev !! (index-1)) == '-')	= 1
	| (flag == 1) &&  ((curr !! index) == side) && (((rowprev !! index)) == '-')    = 1
	| otherwise									= 0
	

countTopRight2Crush :: [String]->String->String->String->[String]->Int->Int->Char->Char->Int
countTopRight2Crush prev rowprev2 rowprev curr rest index flag side enemyside
	| (flag == 0) && (index >= 2) && ((curr !! index) == side) && ((rowprev !! (index-1)) == side) && ((rowprev2 !! (index-2)) /= side) 												  = 1
	| (null prev) && (flag == 1) && (index >= 1) && ((curr !! index) == side) && ((rowprev !! index) == side) && ((rowprev2 !! (index-1)) /= side)									                  = 1
	| (not (null prev)) && (flag == 1) && (length(curr) == ((2*(length(head prev))-2))) &&  (index >= 1) && ((curr !! index) == side) && ((rowprev !! index) == side) && ((rowprev2 !! (index-1)) /= side)				  = 1
	| (not (null prev)) && (flag == 1) && (length(curr) <  ((2*(length(head prev))-2))) && (length(curr) /= (2*(length (head prev)-1))) && ((curr !! index) == side) && ((rowprev !! index) == side) && ((rowprev2 !! index) /= side) = 1
	| otherwise																											  = 0	

countLeftCrush :: [String]->String->[String]->Int->Char->Char->Int
countLeftCrush prev curr rest index side enemySide
	= (countLeft2Crush prev curr rest index side enemySide) + countLeft1Crush prev curr rest index side

countLeft1Crush :: [String]->String->[String]->Int->Char->Int
countLeft1Crush prev curr rest index side
	| index == 0							= 0
	| ((curr !! index) == side) && (curr !! (index-1) == '-')	= 1
	| otherwise							= 0
	
countLeft2Crush :: [String]->String->[String]->Int->Char->Char->Int
countLeft2Crush prev curr rest index side enemyside
	| index < 2										 	= 0
	| ((curr !! index) == side) && ((curr !! (index-1)) == side) && ((curr !! (index-2)) /= side)	= 1
	| otherwise											= 0

	

countRightCrush :: [String]->String->[String]->Int->Char->Char->Int
countRightCrush prev curr rest index side enemyside
	= (countRight2Crush prev curr rest index side enemyside) + countRight1Crush prev curr rest index side

countRight1Crush :: [String]->String->[String]->Int->Char->Int
countRight1Crush prev curr rest index side
	| index > ((length curr) - 2)					= 0
	| ((curr !! index) == side) && (curr !! (index+1) == '-')	= 1
	| otherwise							= 0

	
countRight2Crush :: [String]->String->[String]->Int->Char->Char->Int
countRight2Crush prev curr rest index side enemyside
	| index > ((length curr) - 3)									= 0
	| ((curr !! index) == side) && ((curr !! (index+1)) == side) && ((curr !! (index+2)) /= side)	= 1
	| otherwise											= 0
		
	

-- Can crush searches out the number of crushes you can do given this game states
crushCounter :: [String]->Char->Char->Int->Int
crushCounter gameState mySide enemySide n
	= 5*(generateCrushes gameState gameState 0 0 0 n mySide enemySide [] 0) + (-7*(generateCrushes gameState gameState 0 0 0 n enemySide mySide [] 0))


generateCrushes :: [String]->[String]->Int->Int->Int->Int->Char->Char->[[String]]->Int->Int
generateCrushes gameState origGS rowNum flag switch n side enemySide history acc
	| null gameState				= acc
	| (flag == 1) || (switch == 1)			= generateCrushes (tail gameState) origGS (rowNum+1) 1 n switch side enemySide history ((generateCrushMoves (take rowNum origGS) (origGS !! rowNum) (drop (rowNum+1) origGS) flag (findIndices (==side) (origGS !! rowNum)) side enemySide 0) + acc)
	| ((length (head gameState)) == (2*n -1)) 	= generateCrushes (tail gameState) origGS (rowNum+1) 0 n 1 side enemySide history ((generateCrushMoves (take rowNum origGS) (origGS !! rowNum) (drop (rowNum+1) origGS) flag (findIndices (==side) (origGS !! rowNum)) side enemySide 0) + acc)
	| otherwise					= generateCrushes (tail gameState) origGS (rowNum+1) 0 n switch side enemySide history ((generateCrushMoves (take rowNum origGS) (origGS !! rowNum) (drop (rowNum+1) origGS) flag (findIndices (==side) (origGS !! rowNum)) side enemySide 0) + acc)

	
generateCrushMoves :: [String]->String->[String]->Int->[Int]->Char->Char->Int->Int
generateCrushMoves prev currRow rest flag lsIdx side enemySide acc
	| null lsIdx		= acc																																																																																				
	| null prev		= generateCrushMoves prev currRow rest flag (tail lsIdx) side enemySide (acc + (moveLeftCrush prev currRow rest (head lsIdx) side enemySide) + (moveRightCrush prev currRow rest (head lsIdx) side enemySide)+ (moveBottomLeftCrush prev currRow (head rest) (tail rest) (head lsIdx) flag side enemySide) + (moveBottomRightCrush prev currRow (head rest) (tail rest) (head lsIdx) flag side enemySide)) -- No TopLeft/TopRight movement
	| null rest		= generateCrushMoves prev currRow rest flag (tail lsIdx) side enemySide (acc + (moveLeftCrush prev currRow rest (head lsIdx) side enemySide) + (moveRightCrush prev currRow rest (head lsIdx) side enemySide)+ (moveTopLeftCrush (reverse(tail(reverse(prev)))) (last prev) currRow rest (head lsIdx) flag side enemySide)) -- No BottomLeft/BottomRight 
	| otherwise		= generateCrushMoves prev currRow rest flag (tail lsIdx) side enemySide (acc + (moveLeftCrush prev currRow rest (head lsIdx) side enemySide) + (moveRightCrush prev currRow rest (head lsIdx) side enemySide)+ (moveTopLeftCrush (reverse(tail(reverse(prev)))) (last prev) currRow rest (head lsIdx) flag side enemySide)+( moveTopRightCrush (reverse(tail(reverse(prev)))) (last prev) currRow rest (head lsIdx) flag side enemySide)+ (moveBottomLeftCrush prev currRow (head rest) (tail rest) flag (head lsIdx) side enemySide)+ (moveBottomRightCrush prev currRow (head rest) (tail rest) flag (head lsIdx) side enemySide)) -- Enable all movements	
	
	

moveBottomRightCrush :: [String]->String->String->[String]->Int->Int->Char->Char->Int
moveBottomRightCrush prev currRow next rest flag lsIdx side enemySide
	| (length rest) < 1			= 0
	| otherwise				= moveBottomRight2Crush prev (reverse currRow) (reverse next) (reverse (head rest)) (tail rest) ((length currRow)-1-lsIdx) flag side enemySide

																							

moveBottomRight2Crush :: [String]->String->String->String->[String]->Int->Int->Char->Char->Int
moveBottomRight2Crush prev curr next next2 rest index flag side enemySide		
	| (flag == 1) && (index >= 2) && ((next !! (index-1)) == side) && ((next2 !! (index-2)) == enemySide									= 1
	| (not (null prev)) && (length curr) == (2*(length (head prev))-2) && (flag == 0) && (index >= 1) && (next !! (index-1)) == side && (next2 !! (index-2)) == enemySide	= 1
	| (not (null prev)) && (length curr) == (2*(length (head prev))-1) && (index >= 2) && (next !! (index-1)) == side && (next2 !! (index-2)) == enemySide			= 1
	| (flag == 0) && (length curr) < (length next2) &&(next !! index == side) && (next2 !! index) == enemySide && (length curr) /= (length next2)				= 1
	| (flag == 0) && (length curr) == (length next2) && (next !! index) == side && (next2 !! (index-1)) == enemySide							= 1
	| otherwise																				= 0

	
moveBottomLeftCrush :: [String]->String->String->[String]->Int->Int->Char->Char->Int
moveBottomLeftCrush prev currRow next rest flag lsIdx side enemySide
    | (length rest) < 1			= 0
	| otherwise					= moveBottomLeft2Crush prev currRow next (head rest) (tail rest) lsIdx flag side enemySide
													
moveBottomLeft2Crush :: [String]->String->String->String->[String]->Int->Int->Char->Char->Int
moveBottomLeft2Crush prev curr next next2 rest index flag side enemyside	
	| (flag == 1) && (index >= 2) && ((next !! (index-1)) == side) && ((next2 !! (index-2)) == enemyside) = 1
	| (not (null prev)) && (length curr) == (2*(length (head prev))-2) && (flag == 0) && (index >= 1) && (next !! (index-1)) == side && (next2 !! (index-2)) == enemyside	= 1
	| (not (null prev)) && (length curr) == (2*(length (head prev))-1) && (index >= 2) && (next !! (index-1)) == side && (next2 !! (index-2)) == enemyside	= 1
	| (flag == 0) && (length curr) < (length next2) &&(next !! index == side) && (next2 !! index) == enemyside && (length curr) /= (length next2)	= 1
	| (flag == 0) && (length curr) == (length next2) && (next !! index) == side && (next2 !! (index-1)) == enemyside	= 1
	| otherwise																						= 0

moveTopLeftCrush :: [String]->String->String->[String]->Int->Int->Char->Char->Int
moveTopLeftCrush prev rowprev curr rest index flag side enemySide
	| length(prev) < 1						= 0
	| otherwise								= moveTopLeft2Crush (reverse(tail(reverse(prev)))) (last prev) rowprev curr rest index flag side enemySide
	
-- | (flag == 0) && (index >= 2) && ((curr !! index) == side) && ((rowprev !! (index-1)) == side) && ((rowprev2 !! (index-2)) == enemyside) = 1
moveTopLeft2Crush :: [String]->String->String->String->[String]->Int->Int->Char->Char->Int
moveTopLeft2Crush	prev rowprev2 rowprev curr rest index flag side	enemyside
	| (flag == 0) && (index >= 2) && ((curr !! index) == side) && ((rowprev !! (index-1)) == side) && ((rowprev2 !! (index-2)) == enemyside) = 1
	| (null prev) && (flag == 1) && (index >= 1) && ((curr !! index) == side) && ((rowprev !! index) == side) && ((rowprev2 !! (index-1)) == enemyside)	= 1
	| (not (null prev)) && (flag == 1) && (length(curr) == ((2*(length(head prev))-2))) &&  (index >= 1) && ((curr !! index) == side) && ((rowprev !! index) == side) && ((rowprev2 !! (index-1)) == enemyside)	= 1
	| (not (null prev)) && (flag == 1) && (length(curr) <  ((2*(length(head prev))-2))) && (length(curr) /= (2*(length (head prev)-1))) && ((curr !! index) == side) && ((rowprev !! index) == side) && ((rowprev2 !! index) == enemyside) = 1
	| otherwise		= 0

moveTopRightCrush :: [String]->String->String->[String]->Int->Int->Char->Char->Int
moveTopRightCrush prev rowprev curr rest index flag side enemyside
	| length(prev) <1		= 0
	| otherwise				= moveTopRight2Crush (reverse(tail(reverse(prev)))) (reverse (last prev)) (reverse rowprev) (reverse curr) rest ((length curr) - 1 - index) flag side enemyside

moveTopRight2Crush :: [String]->String->String->String->[String]->Int->Int->Char->Char->Int
moveTopRight2Crush prev rowprev2 rowprev curr rest index flag side enemyside
	| (flag == 0) && (index >= 2) && ((curr !! index) == side) && ((rowprev !! (index-1)) == side) && ((rowprev2 !! (index-2)) == enemyside) = 1
	| (null prev) && (flag == 1) && (index >= 1) && ((curr !! index) == side) && ((rowprev !! index) == side) && ((rowprev2 !! (index-1)) == enemyside)	= 1
	| (not (null prev)) && (flag == 1) && (length(curr) == ((2*(length(head prev))-2))) &&  (index >= 1) && ((curr !! index) == side) && ((rowprev !! index) == side) && ((rowprev2 !! (index-1)) == enemyside)	= 1
	| (not (null prev)) && (flag == 1) && (length(curr) <  ((2*(length(head prev))-2))) && (length(curr) /= (2*(length (head prev)-1))) && ((curr !! index) == side) && ((rowprev !! index) == side) && ((rowprev2 !! index) == enemyside) = 1
	| otherwise		= 0	

moveLeftCrush :: [String]->String->[String]->Int->Char->Char->Int
moveLeftCrush prev curr rest index side enemySide
	= moveLeft2Crush prev curr rest index side enemySide

moveLeft2Crush :: [String]->String->[String]->Int->Char->Char->Int
moveLeft2Crush prev curr rest index side enemyside
	| index < 2														= 0
	| ((curr !! index) == side) && ((curr !! (index-1)) == side) && ((curr !! (index-2)) == enemyside)	= 1
	| otherwise														= 0

moveRightCrush :: [String]->String->[String]->Int->Char->Char->Int
moveRightCrush prev curr rest index side enemyside
	= moveRight2Crush prev curr rest index side enemyside
	
moveRight2Crush :: [String]->String->[String]->Int->Char->Char->Int
moveRight2Crush prev curr rest index side enemyside
	| index > ((length curr) - 3)		= 0
	| ((curr !! index) == side) && ((curr !! (index+1)) == side) && ((curr !! (index+2)) == enemyside)	= 1
	| otherwise														= 0
	
{-- Breaks the list into substrings of [String]
Takes in the gamestate as one string, n
returns the gamestate separated into hexgonagle shape
Flag = 0
n == (2*origN-1) Flag = 1 ie. bottom now
--}

separateGame :: String -> Int ->Int->Int->[String]->[String]
separateGame gameState n origN flag acc
	| null gameState     = reverse(acc)
	| n == (2*origN -1)  = separateGame (drop n gameState) (n-1) origN 1 ((take n gameState):acc) 
	| flag == 1	     = separateGame (drop n gameState) (n-1) origN 1 ((take n gameState):acc)
	| otherwise	     = separateGame (drop n gameState) (n+1) origN 0 ((take n gameState):acc)
	
historySeperator :: [String]->Int->[[String]]->[[String]]
historySeperator history n acc
	| null history	= acc
	| otherwis	=  historySeperator (tail history) n ((separateGame (head history) n n 0 []):acc)

	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
