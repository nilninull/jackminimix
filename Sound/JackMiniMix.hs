------------------------------------------------------------------------------

-- JackMiniMix.hs
-- created: Tue Aug 10 18:05:33 JST 2010

------------------------------------------------------------------------------
--
-- Module      :  Sound.JackMiniMix
-- Copyright   :  (c) Renick Bell 2010
-- License     :  GPL
--
-- Maintainer  :  renick@gmail.com
-- Stability   :  provisional
-- Portability :  Linux only? Does it work on OSX?
--
--
------------------------------------------------------------------------------

-- | This is a module to control JackMiniMix, a GPL mixer for the Jack Audio Connection Kit using OpenSoundControl (OSC).
--
-- JackMiniMix was written by Nicholas J Humfrey. <http://www.aelius.com/njh/jackminimix/>
--
-- OSC was developed at CNMAT by Adrian Freed and Matt Wright. <http://opensoundcontrol.org/>
--
-- The documentation was mostly taken from the source of JackMiniMix by Humfrey.
--
-- This module requires that the JackMiniMix program be installed.
--
-- For general usage, use the camel case functions; they're easier. The functions with underscores in the name need to be called using the withJackMiniMix function; the camelCase ones already include it.

module Sound.JackMiniMix where

import           Control.Applicative
import           Control.Monad
import           Data.Int
import           Data.Maybe
import           Sound.OSC

-- | Bracket JackMiniMix communication.

withJackMiniMix :: Int -> Connection UDP a -> IO a
withJackMiniMix port = withTransport (openUDP "127.0.0.1" port)

channel_count :: Connection UDP [Datum]
channel_count =
    let m = Message "/mixer/get_channel_count" []
    in sendMessage m >> waitDatum "/mixer/channel_count"

-- | Returns the number of stereo input channels that the mixer has.
--
-- The OSC message:
--
-- \/mixer\/get_channel_count        - Get the number of channels
--
-- replies with:
--
-- \/mixer\/channel_count (i)

channelCount :: Int -> IO [Datum]
channelCount port = withJackMiniMix port channel_count

channelCount' :: Int -> IO (Maybe Int32)
channelCount' port = (d_get <=< listToMaybe) <$> withJackMiniMix port channel_count





get_channel_gain :: (RecvOSC m, SendOSC m) => Int32 -> m [Datum]
get_channel_gain channel =
    let m = Message "/mixer/channel/get_gain" [Int32 channel]
    in sendMessage m >> waitDatum "/mixer/channel/gain"

-- | Returns the gain (in decibels) of channel.
--
-- The OSC message:
--
-- \/mixer\/channel\/get_gain (i)     - Get gain of channel i
--
-- replies with:
--
-- \/mixer\/channel\/gain (if)
--
-- channel is the number of the channel (in range 1 to total number of channels).

getChannelGain :: Int -> Int32 -> IO [Datum]
getChannelGain port channel =
    withJackMiniMix port (get_channel_gain channel)

getChannelGain' ::  Int -> Int32 -> IO (Maybe Int32, Maybe Float)
getChannelGain' port channel = do
    [ch, gain] <- withJackMiniMix port (get_channel_gain channel)
    return (d_get ch, d_get gain)


set_channel_gain :: (RecvOSC m, SendOSC m) => Int32 -> Float -> m [Datum]
set_channel_gain channel gain =
    let m = Message "/mixer/channel/set_gain" [Int32 channel,Float gain]
    in sendMessage m >> waitDatum "/mixer/channel/gain"

-- | Sets the gain of channel channel to gain dB.
--
-- The OSC message:
--
-- \/mixer\/channel\/set_gain (if)    - Set the gain of channel i to f dB
--
-- replies with:
--
-- \/mixer\/channel\/gain (if)
--
-- channel is the number of the channel (in range 1 to total number of channels).
--
-- gain is the gain (in decibels) to set the channel to (in range -90 to 90 dB).

setChannelGain :: Int -> Int32 -> Float -> IO [Datum]
setChannelGain port channel gain =
    withJackMiniMix port (set_channel_gain channel gain)

setChannelGain' :: Int -> Int32 -> Float -> IO (Maybe Int32, Maybe Float)
setChannelGain' port channel gain = do
    [ch, newGain] <- withJackMiniMix port (set_channel_gain channel gain)
    return (d_get ch, d_get newGain)


get_channel_label :: (RecvOSC m, SendOSC m) => Int32 -> m [Datum]
get_channel_label channel =
    let m = Message "/mixer/channel/get_label" [Int32 channel]
    in sendMessage m >> waitDatum "/mixer/channel/label"

-- | Returns the label (string) of channel number channel.
--
-- The OSC message:
--
-- \/mixer\/channel\/get_label (i)    - Get the label of channel i
--
-- replies with:
--
-- \/mixer\/channel\/label (is)
--
-- channel is the number of the channel (in range 1 to total number of channels).

getChannelLabel :: Int -> Int32 -> IO [Datum]
getChannelLabel port channel =
    withJackMiniMix port (get_channel_label channel)

getChannelLabel' :: Int -> Int32 -> IO (Maybe Int32, Maybe ASCII)
getChannelLabel' port channel = do
    [ch, label] <- withJackMiniMix port (get_channel_label channel)
    return (d_get ch, d_get label)


set_channel_label :: (RecvOSC m, SendOSC m) => Int32 -> ASCII -> m [Datum]
set_channel_label channel label =
    let m = Message "/mixer/channel/set_label" [Int32 channel, ASCII_String label]
    in sendMessage m >> waitDatum "/mixer/channel/label"

-- | Sets the label (string) of channel number channel to label.
--
-- The OSC message:
--
-- \/mixer\/channel\/set_label (is)   - Set the label of channel i to s
--
-- replies with:
--
-- \/mixer\/channel\/label (is)
--
-- channel is the number of the channel (in range 1 to total number of channels).
--
-- label is the new label for the channel.

setChannelLabel :: Int -> Int32 -> ASCII -> IO [Datum]
setChannelLabel port channel label =
  withJackMiniMix port (set_channel_label channel label)

setChannelLabel' :: Int -> Int32 -> ASCII -> IO (Maybe Int32, Maybe ASCII)
setChannelLabel' port channel label = do
  [ch, newLabel] <- withJackMiniMix port (set_channel_label channel label)
  return (d_get ch, d_get newLabel)



ping :: Connection UDP Message
ping = let m = Message "/ping" []
       in sendMessage m >> waitReply "/pong"

-- | Pings the mixer to see if it is there.
--
-- The OSC message:
--
-- \/ping                           - Check mixer is still there
--
-- replies with:
--
-- \/pong

-- pingMixer :: Int -> IO [Datum]
pingMixer :: Int -> IO Message
pingMixer port = withJackMiniMix port ping
