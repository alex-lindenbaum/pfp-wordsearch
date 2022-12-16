puzzle = "\
\N Z C G S M Z C M Q A C B O S W R E D V\n\
\X O K P G H N R E X T C V P E Z G W F V\n\
\W C D T N C P I Y F M E R J S Z W L E S\n\
\T R L K Z A S T M Z J V O N W F C Z X K\n\
\E S N N O H R W Y J I S Z A S M Y I S P\n\
\K T M W J R A D F W F M G W L I G O Q Q\n\
\C L D O L W G A M U V K G N I K C U F S\n\
\V X L K U O G H H I E Y Y Z C I S F B H\n\
\G J W B V U O R F H P E Y M N I I T O Q\n\
\G P V S N Y Y M R N H S A C J F J X H Z\n\
\T G J M F B R L E M H N R C S R I J M M\n\
\K V S A J V O N A W J E A F U U D W L B\n\
\P B H R H H Y A D B D W D Y G S W H A A\n\
\I Q I T M K I A Q I Y M W E A T U M J M\n\
\P P X W A T C H B H Y T Y Z D R S U Y U\n\
\V Q J N T L X L F V S D P G P A E Q E K\n\
\S Z N A E C Y N E J K H T W M T S P Z M\n\
\G Q M S E X T L R G Y G U B Y I J I A B\n\
\J C U D P E Z P M D W F Z G I N Z L L Y\n\
\P Y G X F W H P N T X R S I F G C C L T\n\
\Find the words:\n\
\FRUSTRATING INCREDIBLY FUCKING WATCH SMART"



-- TODO: change this to not use dup, take, last
parsePuzzle :: String -> ([[Char]], [String])
parsePuzzle p = (parseLetters $ lines p, parseWords $ lines p)
    where dup f a = f a a
          parseLine = map head . words
          parseLetters = map parseLine . dup (take . (subtract 2) . length)
          parseWords = words . last





























