-- Largest Connected Component Algorithm
-- Callum Gill
-- Define types
type Color = Int
type Index = (Int,Int)
type Cells = [(Index,Color)]

-- Main entry point function that takes 2D list of ints and a colour value then returns the length of the largest component
nlcc :: [[Int]] -> Color -> Int
nlcc l v = length (map fst $ getMaxComp (mapBinaryArray l) v)

-- Function to map the 2D binary array to a list of Cells
mapBinaryArray :: [[Int]] -> Cells
mapBinaryArray xs = [((y,x),e) | (t,y) <- zip xs [0..], (e,x) <- zip t [0..]]

-- Function to calculate the largest component by retieving all components and returning the size of the largest
getMaxComp :: Cells -> Color -> Cells
getMaxComp cells color = foldl (\max current -> if length current > length max then current else max) [] $ retrieveAllComps cells color

-- Function to retrieve all components within the binary image
retrieveAllComps :: Cells -> Color -> [Cells]
retrieveAllComps cells color = map (\cell -> removeUnwantedColour (removeDuplicates (retrieveComponent cells cell)) color) cells

-- Function to remove duplicate components from the list of components
removeDuplicates :: Cells -> Cells
removeDuplicates = foldl (\seen x -> if x `elem` seen then seen else seen ++ [x]) []

-- Function to remove all components with the undesired colour value. So that only components with the desirec colour value remains
removeUnwantedColour :: Cells -> Color -> Cells
removeUnwantedColour l c = foldl(\current x -> if snd x /= c then current else current ++ [x]) [] l

-- Function to retrieve a component connected to thee current cell
retrieveComponent :: Cells -> (Index,Color) -> Cells
retrieveComponent _ (_,-1) = []
retrieveComponent cells ((x,y), c) = let current = ((x,y), c)
                                         moveup = retrieveCell cells (x, y + (-1))
                                         movedown = retrieveCell cells (x, y + 1)
                                         moveleft = retrieveCell cells (x + (-1), y)
                                         moveright = retrieveCell cells (x + 1, y)
                                         newCells = deleteCell cells ((x,y), c)
                                         findNeighbors = getRelatedCell c
                                         makeComponent = retrieveComponent newCells
                                    in current : makeComponent (findNeighbors moveup) ++ makeComponent (findNeighbors movedown) ++ makeComponent (findNeighbors moveleft) ++ makeComponent (findNeighbors moveright)

-- Function to get any nearby cells that are related to the current cell. I.e. have the same colour value
getRelatedCell :: Color -> (Index,Color) -> (Index,Color)
getRelatedCell c ((ix,iy),ic) = if isInRange (ix,iy) && ic == c then ((ix,iy),ic) else ((-1,-1),-1)


-- Function to delete a cell from the list of cells returning a new list of cells not containing the desired one to delete
deleteCell :: Cells -> (Index,Color) -> Cells
deleteCell [] _ = []
deleteCell (((ix,iy),ic):xs) ((x,y),c) = if ix /= x || iy /= y then  ((ix,iy),ic) : deleteCell xs ((x,y),c) else deleteCell xs ((x,y),c)

-- Function to retrieve the values of a Cell. Mainly the index and colour value of the cell
retrieveCell :: Cells -> Index -> (Index,Color)
retrieveCell [] _ = ((-1,-1), -1)
retrieveCell (((x,y),c):xs) (ix,iy) = if x == ix && y == iy then ((x,y),c) else retrieveCell xs (ix,iy)


-- Function  to check if a cell at a specific index is in range of the current cell
isInRange :: Index -> Bool
isInRange (x,y) = x >= 0 && x < 6 && y >= 0 && y < 8