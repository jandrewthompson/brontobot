{-# LANGUAGE OverloadedStrings #-}
module Network.IRC.Bot.Part.Wisdom where

import Control.Monad            (replicateM)
import Control.Monad.Trans      (liftIO)
import Data.ByteString          (ByteString)
import Data.ByteString.Char8    (pack)
import Data.Monoid              ((<>))
import Network.IRC.Bot.Log      (LogLevel(Debug))
import Network.IRC.Bot.BotMonad (BotMonad(..), maybeZero)
import Network.IRC.Bot.Commands (PrivMsg(..), sendCommand, replyTo)
import Network.IRC.Bot.Parsec   (botPrefix, nat, parsecPart)
import System.Random            (randomRIO)
import Text.Parsec              (ParsecT, (<|>), (<?>), char, skipMany1, space, string, try)

sovietWisdomPart :: (BotMonad m) => m ()
sovietWisdomPart = parsecPart sovietWisdomCommand

sovietWisdomCommand :: (BotMonad m) => ParsecT ByteString () m ()
sovietWisdomCommand =
    do try $ botPrefix >> string "sovietwisdom"
       logM Debug "sovietWisdomPart"
       target <- maybeZero =<< replyTo
       n <- liftIO $ randomRIO (0, length args - 1)
       let wisdomMsg = args!!n 
       sendCommand (PrivMsg Nothing [target] wisdomMsg)
    <|> return ()
    where
        args = [
            "Have Nice Day!",
            "In Soviet Russia, baby aborts YOU!!",
            "In Soviet Russia, car drives YOU!!",
            "In Soviet Russia, monkey spanks YOU!!",
            "In Soviet Russia, toilet flushes YOU!!",
            "In Soviet Russia, road forks YOU!!",
            "In Soviet Russia, fish catches YOU!",
            "In Soviet Russia, radio listens to YOU!!",
            "In Soviet Russia, chair sits on YOU!!",
            "In Soviet Russia, cold catches YOU!!",
            "In Soviet Russia, cow tips YOU!!",
            "In Soviet Russia, sentence finishes YOU!!",
            "In Soviet Russia, bong hits YOU!!",
            "In Soviet Russia, bucket kicks YOU!!",
            "In Soviet Russia, TV watches YOU!!",
            "In Soviet Russia, steak grills YOU!!!",
            "In Soviet Russia, CD burns YOU!!",
            "In Soviet Russia, bar walks into YOU!!",
            "In Soviet Russia, ass kicks YOU!!",
            "In Soviet Russia, Waldo finds YOU!!",
            "In Soviet Russia, n00b pwns j00!!!",
            "In Soviet Russia, bag punches YOU!!",
            "In Soviet Russia, frog dissects YOU!!",
            "In Soviet Russia, kitten huffs YOU!!",
            "In Soviet Russia, mouse traps YOU!!",
            "In Soviet Russia, remote controls YOU!!",
            "In Soviet Russia, movie pirates YOU!!",
            "In Soviet Russia, day seizes YOU!!",
            "In Soviet Russia, picture hangs YOU!!",
            "In Soviet Russia, bank robs YOU!!",
            "In Soviet Russia, deer hunts YOU!!",
            "In Soviet Russia, roulette turns YOU!!",
            "In Soviet Russia, laundry hangs YOU!!",
            "In Soviet Russia, toast burns YOU!!",
            "In Soviet Russia, corn pops YOU!!",
            "In Soviet Russia, nut cracks YOU!!",
            "In Soviet Russia, weight gains YOU!!",
            "In Soviet Russia, Pokemon catches YOU!!",
            "In Soviet Russia, dust bites YOU!!",
            "In Soviet Russia, rocket launches YOU!!",
            "In Soviet Russia, Tree chops YOU!!"
            ]

wisdomPart :: (BotMonad m) => m ()
wisdomPart = parsecPart wisdomCommand

wisdomCommand :: (BotMonad m) => ParsecT ByteString () m ()
wisdomCommand =
    do try $ botPrefix >> string "wisdom"
       logM Debug "wisdomPart"
       target <- maybeZero =<< replyTo
       n <- liftIO $ randomRIO (0, length args - 1)
       let wisdomMsg = args!!n 
       sendCommand (PrivMsg Nothing [target] wisdomMsg)
    <|> return ()
    where
        args = [ "It's sad that a family can be torn apart by something as simple as a pack of wild dogs.","Dad always thought laughter was the best medicine, which I guess is why several of us died of tuberculosis.","Maybe in order to understand mankind, we have to look at the word itself: \"Mankind\". Basically, it's made up of two separate words - \"mank\" and \"ind\". What do these words mean ? It's a mystery, and that's why so is mankind.","I hope if dogs ever take over the world, and they chose a king, they don't just go by size, because I bet there are some Chihuahuas with some good ideas.","It takes a big man to cry, but it takes a bigger man to laugh at that man. ","I guess we were all guilty, in a way. We all shot him, we all skinned him, and we all got a complimentary bumper sticker that said, \"I helped skin Bob.\" ","I bet the main reason the police keep people away from a plane crash is they don't want anybody walking in and lying down in the crash stuff, then, when somebody comes up, act like they just woke up and go, \"What was THAT?!\" ","The face of a child can say it all, especially the mouth part of the face. ","Ambition is like a frog sitting on a Venus Flytrap. The flytrap can bite and bite, but it won't bother the frog because it only has little tiny plant teeth. But some other stuff could happen and it could be like ambition.","I'd rather be rich than stupid.","If you were a poor Indian with no weapons, and a bunch of conquistadors came up to you and asked where the gold was, I don't think it would be a good idea to say, \"I swallowed it. So sue me.\"","If you define cowardice as running away at the first sign of danger, screaming and tripping and begging for mercy, then yes, Mr. Brave man, I guess I'm a coward.","I bet one legend that keeps recurring throughout history, in every culture, is the story of Popeye.","When you go in for a job interview, I think a good thing to ask is if they ever press charges. ","To me, boxing is like a ballet, except there's no music, no choreography, and the dancers hit each other.","What is it that makes a complete stranger dive into an icy river to save a solid gold baby? Maybe we'll never know.","We tend to scoff at the beliefs of the ancients. But we can't scoff at them personally, to their faces, and this is what annoys me.","Probably the earliest flyswatters were nothing more than some sort of striking surface attached to the end of a long stick.", "When you're riding in a time machine way far into the future, don't stick your elbow out the window, or it'll turn into a fossil.","If you were a pirate, you know what would be the one thing that would really make you mad? Treasure chests with no handles. How the hell are you supposed to carry it?!","Better not take a dog on the space shuttle, because if he sticks his head out when you're coming home his face might burn up.","If you're a horse, and someone gets on you, and falls off, and then gets right back on you, I think you should buck him off right away.","If a kid asks where rain comes from, I think a cute thing to tell him is \"God is crying.\" And if he asks why God is crying, another cute thing to tell him is \"Probably because of something you did.\"","The first thing was, I learned to forgive myself. Then, I told myself, \"Go ahead and do whatever you want, it's okay by me.\"","I remember how my Great Uncle Jerry would sit on the porch and whittle all day long. Once he whittled me a toy boat out of a larger toy boat I had. It was almost as good as the first one, except now it had bumpy whittle marks all over it. And no paint, because he had whittled off the paint.","If I ever get real rich, I hope I'm not real mean to poor people, like I am now.","I hope that after I die, people will say of me: \"That guy sure owed me a lot of money.\"","Children need encouragement. So if a kid gets an answer right, tell him it was a lucky guess. That way, he develops a good, lucky feeling.","I can picture in my mind a world without war, a world without hate. And I can picture us attacking that world, because they'd never expect it.","It's easy to sit there and say you'd like to have more money. And I guess that's what I like about it. It's easy. Just sitting there, rocking back and forth, wanting that money.","I wish I would have a real tragic love affair and get so bummed out that I'd just quit my job and become a bum for a few years, because I was thinking about doing that anyway.","The face of a child can say it all, especially the mouth part of the face.","To me, boxing is like a ballet, except there's no music, no choreography, and the dancers hit each other.","Remember, kids in the backseat cause accidents; accidents in the backseat cause kids.","If you're a cowboy and you're dragging a guy behind your horse, I bet it would really make you mad if you looked back and the guy was reading a magazine.","I think people tend to forget that trees are living creatures. They're sort of like dogs. Huge, quiet, motionless dogs, with bark instead of fur.","I think my new thing will be to try to be a real happy guy. I'll just walk around being real happy until some jerk says something stupid to me.","If you lived in the Dark Ages and you were a catapult operator, I bet the most common question people would ask is, 'Can't you make it shoot farther?' 'No, I'm sorry. That's as far as it shoots.' ","Is there anything more beautiful than a beautiful, beautiful flamingo, flying across in front of a beautiful sunset? And he's carrying a beautiful rose in his beak, and also he's carrying a very beautiful painting with his feet. And also, you're drunk.","What is it about a beautiful sunny afternoon, with the birds singing and the wind rustling through the leaves, that makes you want to get drunk?  And after you're real drunk, maybe go down to the public park and stagger around and ask people for money, and then lay down and go to sleep.","Here's a good thing to do if you go to a party and you don't know anybody: First take out the garbage. Then go around and collect any extra garbage that people might have, like a crumpled napkin, and take that out too. Pretty soon people will want to meet the busy garbage guy.","One thing kids like is to be tricked. For instance, I was going to take my little nephew to Disneyland, but instead I drove him to an old burned-out warehouse. 'Oh, no,' I said. 'Disneyland burned down.' He cried and cried, but I think that deep down, he thought it was a pretty good joke. I started to drive over to the real Disneyland, but it was getting pretty late.","Too bad you can't buy a voodoo globe so that you could make the earth spin real fast and freak everybody out.","If you're a young Mafia gangster out on your first date, I bet it's real embarrassing if someone tries to kill you.","You know what's probably a good thing to hang on your porch in the summertime, to keep mosquitos away from you and your guests? Just a big bag full of blood.","I guess the hard thing for a lot of people to accept is why God would allow me to go running through their yards, yelling and spinning around.","Don't ever get your speedometer confused with your clock, like I did once, because the faster you go the later you think you are.","It makes me mad when people say I turned and ran like a scared rabbit. Maybe it was like an angry rabbit, who was going to fight in another fight, away from the first fight.","I wish outer space guys would conquer the Earth and make people their pets, because I'd like to have one of those little beds with my name on it.","I think a good product would be \"Baby Duck Hat\". It's a fake baby duck, which you strap on top of your head. Then you go swimming underwater until you find a mommy duck and her babies, and you join them. Then, all of a sudden, you stand up out of the water and roar like Godzilla. Man, those ducks really take off! Also, Baby Duck Hat is good for parties.","I remember that one fateful day when Coach took me aside. I knew what was coming. \"You don't have to tell me,\" I said. \"I'm off the team, aren't I?\" \"Well,\" said Coach, \"you never were really ON the team. You made that uniform you're wearing out of rags and towels, and your helmet is a toy space helmet. You show up at practice and then either steal the ball and make us chase you to get it back, or you try to tackle people at inappropriate times.\" It was all true what he was saying. And yet, I thought something is brewing inside the head of this Coach. He sees something in me, some kind of raw talent that he can mold. But that's when I felt the handcuffs go on."]
