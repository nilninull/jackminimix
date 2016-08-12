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

import Sound.OpenSoundControl.Transport
import Sound.OpenSoundControl.Transport.UDP
import Sound.OpenSoundControl.OSC



-- | Bracket JackMiniMix communication. 

withJackMiniMix :: Int -> (UDP -> IO a) -> IO a
withJackMiniMix port = withTransport (openUDP "127.0.0.1" port)


channel_count :: (Transport t) => t -> IO OSC
channel_count fd = 
    let m = Message "/mixer/get_channel_count" []
    in send fd m >> wait fd "/mixer/channel_count"

-- | Returns the number of stereo input channels that the mixer has.
--
-- The OSC message:
--
-- \/mixer\/get_channel_count        - Get the number of channels
--
-- replies with:
--
-- \/mixer\/channel_count (i)

channelCount :: Int -- ^ the port number
                -> IO OSC
channelCount port = withJackMiniMix port (\x -> channel_count x)



get_channel_gain :: (Transport t) => t -> Int -> IO OSC
get_channel_gain fd channel = 
    let m = Message "/mixer/channel/get_gain" [Int channel]
    in send fd m >> wait fd "/mixer/channel/gain"

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

getChannelGain :: Int -- ^ the port number
               -> Int -- ^ the channel 
               -> IO OSC
getChannelGain port channel = 
    let getter x = get_channel_gain x channel
    in withJackMiniMix port getter



set_channel_gain :: (Transport t) => t -> Int -> Double -> IO OSC
set_channel_gain fd channel gain = 
    let m = Message "/mixer/channel/set_gain" [Int channel,Float gain]
    in send fd m >> wait fd "/mixer/channel/gain"

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

setChannelGain :: Int -- ^ the port number
               -> Int -- ^ the channel number
               -> Double -- ^ the new gain
               -> IO OSC
setChannelGain port channel gain = 
    let setter x = set_channel_gain x channel gain
    in withJackMiniMix port setter



get_channel_label :: (Transport t) => t -> Int -> IO OSC
get_channel_label fd channel = 
    let m = Message "/mixer/channel/get_label" [Int channel]
    in send fd m >> wait fd "/mixer/channel/label"

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

getChannelLabel :: Int -- ^ the port number
                -> Int -- ^ the channel
                -> IO OSC
getChannelLabel port channel =
    let getter x = get_channel_label x channel
    in withJackMiniMix port getter



set_channel_label :: (Transport t) => t -> Int -> String -> IO OSC
set_channel_label fd channel label = 
    let m = Message "/mixer/channel/set_label" [Int channel,String label]
    in send fd m >> wait fd "/mixer/channel/label"

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

setChannelLabel :: Int -- ^ the port number
                -> Int -- ^ the channel
                -> String -- ^ the new channel label
                -> IO OSC
setChannelLabel port channel label = 
    let setter x = set_channel_label x channel label
    in withJackMiniMix port setter



ping :: (Transport t) => t -> IO OSC
ping fd =
    let m = Message "/ping" []
    in send fd m >> wait fd "/pong"

-- | Pings the mixer to see if it is there.
--
-- The OSC message:
--
-- \/ping                           - Check mixer is still there
--
-- replies with:
--
-- \/pong

pingMixer :: Int -- ^ the port number
          -> IO OSC
pingMixer port = withJackMiniMix port (\x -> ping x)
