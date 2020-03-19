{-
 - This module is a proof-of-concept of network communication in CλaSH.
 - Additionally, it can be used to do basic functionality checking of network
 - communication. Through some trickery, it becomes possible to "ping" the
 - FPGA, verifying that packets can be received and sent.
 -
 - Since IPv4 depends on ARP, and ARP is not implemented, the ARP phase of IP
 - communication needs to be bypassed. On a Linux host where the network
 - interface enp3s0 is connected to the evaluation board with the FPGA, one
 - can accomplish this as follows:
 -
 - # ip route add 10.0.0.2/32 dev enp3s0
 - # ip neigh add 10.0.0.2 lladdr 52:54:00:eb:9b:d0 dev enp3s0
 -
 - This establishes that peer 10.0.0.2 can be reached on enp3s0 at MAC address
 - 52:54:00:eb:9b:d0, without doing ARP. If the network configured on enp3s0
 - is already the network where 10.0.0.2 resides, the first line can be
 - omitted.
 -
 - Load the design in this module on the FPGA. On the HPS of the SoC, run
 - `fpgaeth`, and bridge the Ethernet connection to the TAP interface. This is
 - described in the top-level README.
 -
 - we can now reach the FPGA from a networked Linux host with:
 -
 - $ ping 10.0.0.2
 -
 - The way this is accomplished is some simple trickery described at the
 - `fakeReply` function below.
 -}
module ICMPEcho where

import Clash.Prelude
import qualified Clash.Explicit.Prelude as CEP
import Data.Maybe

import Avalon.ByteStreamToPacket
import Avalon.Master
import Avalon.PacketReader
import Avalon.PacketToByteStream
import Avalon.PacketWriter
import Toolbox.Misc

{-
 - Qualify a packet as ICMP Echo Request or not.
 -
 - scanEcho writes an incoming packet into blockram, while comparing the data
 - to a template of what a valid ICMP Echo Request (ping) packet should look
 - like. As soon as a byte is read whose value ANDed with `vrfyMask` does not
 - equal `vrfyVal`, the packet is marked as not being a ping packet, and it
 - will be discarded after it has been fully read.
 -
 - If however all the packet bytes matched the template, the whole template
 - was matched (`minIcmpEchoLength`) and the last byte was read, the
 - `sendReply` output signals a `Just pktLen` so `fakeReply` can read the full
 - packet from blockram and reply to it.
 -
 - NOTE: The `pktLen` value in `sendReply` is one less than the length of the
 - packet in bytes.
 -
 -}
scanEcho
    :: HiddenClockResetEnable dom
    => "sIn" :::
           Signal dom
               ( Maybe ( "more" ::: Bool
                       , "data" ::: Unsigned 8
                       )
               )
       -- ^ Stream-in
    -> "ramBusy" ::: Signal dom Bool
       -- ^ RAM busy
    -> ( "ramWrite" :::
             Signal dom
                 ( Maybe ( "ramAddr" ::: Unsigned 11
                         , "ramData" ::: Unsigned 8
                         )
                 )
         -- ^ RAM write
       , "sInReady" ::: Signal dom Bool
         -- ^ Stream-in ready
       , "sendReply" ::: Signal dom (Maybe ("pktLen" ::: Unsigned 11))
         -- Send reply
       )

scanEcho sIn ramBusy = mealyB scanEcho' (0, True) (sIn, ramBusy)

scanEcho'
    :: Mealy ( "s" :::
                 ( "cnt" ::: Unsigned 11
                 , "isPing" ::: Bool
                 )
             )
             ( "i" :::
                 ( "sIn" :::
                     Maybe ( "more" ::: Bool
                           , "data" ::: Unsigned 8
                           )
                , "ramBusy" ::: Bool
                )
             )
             ( "o" :::
                 ( "ramWrite" :::
                     Maybe ( "ramAddr" ::: Unsigned 11
                           , "ramData" ::: Unsigned 8
                           )
                 , "sInReady" ::: Bool
                 , "sendReply" ::: Maybe ("pktLen" ::: Unsigned 11)
                 )
             )

scanEcho' (cnt, isPing) (sIn, ramBusy)
    = ((cnt', isPing'), (ramWrite, sInReady, sendReply))
    where
        (sInMore, sInData) = fromJust sIn

        ramWrite | sIn == Nothing = Nothing
                 | otherwise      = Just (cnt, sInData)

        isPing' | cnt' == 0                                    = True
                |    sIn == Nothing
                  || cnt >= minIcmpEchoLength
                  || sInData .&. vrfyMask!!cnt == vrfyVal!!cnt = isPing
                | otherwise                                    = False

        cnt' |    not sInReady
               || sIn == Nothing = cnt
             | not sInMore       = 0
             | otherwise         = cnt + 1

        sInReady = not (cnt == 0 && ramBusy)

        sendReply | cnt' /= 0                              = Nothing
                  | isPing && cnt >= (minIcmpEchoLength-1) = Just cnt
                  | otherwise                              = Nothing

{-
 - The minimum length of an ICMP Echo Request packet, as well as the size of
 - the template.
 -}
minIcmpEchoLength :: Integral a => a

minIcmpEchoLength
    =   14    -- Ethernet header
      + 20    -- IPv4 header
      + 8     -- Minimum ICMP packet length

{-
 - Template mask for matching ICMP Echo Request packets.
 -
 - When a bit in the mask is set, it indicates a bit that should match
 - `vrfyVal` if the packet is a ping to this node.
 -}
vrfyMask :: Vec 42 (Unsigned 8)

vrfyMask
    = $(listToVecTH
            ([ 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0x00, 0x00
             , 0x00, 0x00, 0x00, 0x00, 0xff, 0xff
             -- ^ Ethernet header
             , 0xff, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00
             , 0x00, 0xff, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00
             , 0xff, 0xff, 0xff, 0xff
             -- ^ IPv4 header
             , 0xff, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00
             -- ^ ICMP header
             ] :: [Unsigned 8]))

{-
 - Template for matching ICMP Echo Request packets.
 -
 - It will match a unicast frame to the MAC 52:54:00:eb:9b:d0, addressed to
 - IPv4 address 10.0.0.2, and ICMP type 8, code 0.
 -}
vrfyVal :: Vec 42 (Unsigned 8)

vrfyVal
    = $(listToVecTH
            ([ 0x52, 0x54, 0x00, 0xeb, 0x9b, 0xd0, 0x00, 0x00
             , 0x00, 0x00, 0x00, 0x00, 0x08, 0x00
             -- ^ Ethernet header
             , 0x45, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00
             , 0x00, 0x01, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00
             , 0x0a, 0x00, 0x00, 0x02
             -- ^ IPv4 header
             , 0x08, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00
             -- ^ ICMP header
             ] :: [Unsigned 8]))

{-
 - Pretend we do IPv4 and reply to ping request.
 -
 - Using some trickery, we can reply to ICMP Echo Request (ping) packets. We
 - simply take the incoming packet, swap sender and recipient (both Ethernet
 - and IP), change the ICMP type from 8 to 0, adjust the checksum for the
 - latter and send out the result as our reply.
 -
 - Since the checksum is unchanged under aligned 16-bit word swaps, only the
 - ICMP type change has an effect on the checksum. That effect can be
 - computed, avoiding needing to compute the whole checksum again.
 -
 - So `fakeReply` simply reads out the blockram in such an order that
 - everything that needs to be swapped is swapped, and patches the value only
 - for the ICMP type and the checksum.
 -
 - NOTE: The `pktLen` value in `sendReply` is one less than the length of the
 - packet in bytes.
 -
 - If we are using the blockram, we assert `ramBusy` so `scanEcho` is blocked
 - from using it and corrupting our packet data.
 -
 - This component's (hidden) enable input should be de-asserted if the
 - recipient of `sOut` cannot accept (usually a signal called `sInReady`).
 -
 - The pipelined nature of the blockram plus the need to process data aligned
 - to 16 bits (to tweak the checksum) means the output data lags three cycles
 - behind the read address output.
 -}
fakeReply
    :: HiddenClockResetEnable dom
    => "sendReply" ::: Signal dom (Maybe ("pktLen" ::: Unsigned 11))
       -- ^ Send reply
    -> "readData" ::: Signal dom (Unsigned 8)
       -- ^ RAM read data
    -> ( "ramBusy" ::: Signal dom Bool
         -- ^ RAM busy
       , "readAddr" ::: Signal dom (Unsigned 11)
         -- ^ RAM read address
       , "sOut" :::
             Signal dom
                 (Maybe ( "more" ::: Bool
                        , "data" ::: Unsigned 8
                        )
                 )
         -- ^ Stream-out
       )

fakeReply sendReply readData
    = mealyB fakeReply' fakeReplyIS
             (sendReply, readData)

fakeReplyIS
  :: ( "cnt" ::: Unsigned 12
     , "total" ::: Unsigned 12
     , "prevB" ::: Unsigned 8
     )
fakeReplyIS = (cntInit, 0, undefined)

fakeReply'
    :: Mealy ( "s" :::
                 ( "cnt" ::: Unsigned 12
                 , "total" ::: Unsigned 12
                 , "prevB" ::: Unsigned 8
                 )
             )
             ( "i" :::
                 ( "sendReply" ::: Maybe ("pktLen" ::: Unsigned 11)
                 , "readData" ::: Unsigned 8
                 )
             )
             ( "o" :::
                 ( "ramBusy" ::: Bool
                 , "readAddr" ::: Unsigned 11
                 , "sOut" :::
                       (Maybe ( "more" ::: Bool
                              , "data" ::: Unsigned 8
                              )
                       )
                 )
             )

fakeReply' (cnt, total, prevB) (sendReply, readData)
    = ((cnt', total', prevB'), (ramBusy, readAddr, sOut))
    where
        ramBusy  = cnt /= cntInit
        readAddr = truncateB cnt
        sOut | sOutValid = Just (more, sData)
             | otherwise = Nothing
        more = cnt < total + 3
        sOutValid = not (cnt >= cntInit && cnt < cntInit + 3)

        -- Latch checksum LSB when sData outputs checksum MSB
        prevB' | cnt == 39 = resize cksum
               | otherwise = readData

        total' | total > 0 && not more = 0
               | otherwise             = maybe total resize sendReply

        cnt' | total == 0                           = cntInit
             | cnt < total + 3                      = cntAdvance
             | otherwise                            = cntInit

        cntAdvance
            = case cnt of
                11 ->  0   -- Ethernet src -> Ethernet dst MAC
                5  -> 12   -- Ethernet dst MAC -> Ethertype
                25 -> 30   -- IPv4 Header sum -> dest IP
                33 -> 26   -- Dest IP -> src IP
                29 -> 34   -- Src IP -> ICMP
                _  -> cnt + 1

        -- Note three cycle delay of sData vs. cnt
        sData
            = case cnt of
                37 -> 0                   -- ICMP type
                39 -> resize $ cksum `shiftR` 8
                _  -> prevB

        cksum = bitCoerce (prevB :> readData :> Nil) ~+~ 0x0800 :: Unsigned 16

{-
 - One's complement addition.
 -}
(~+~) :: KnownNat n
      => Unsigned n
      -> Unsigned n
      -> Unsigned n
a ~+~ b = truncateB summed + resize (bitCoerce carry)
    where
        summed = a `add` b
        carry = msb summed

{-
 - Initial value for `fakeReply` counter.
 -
 - The first byte of the incoming packet to output as the first byte of the
 - outgoing packet. So we start with the Ethernet source MAC from the incoming
 - packet, which becomes the destination MAC.
 -}
cntInit :: Unsigned 12
cntInit = 6

{-
 - Respond to pings, bytestream interface.
 -
 - This combines `scanEcho`, `fakeReply` and a blockram. Bytes coming from the
 - network interface enter on `sIn`, and if a ping packet is detected, a reply
 - is output on `sOut`.
 -
 - Input bytes are transferred when `isJust sIn` and `sInReady` is asserted.
 - Similarly for `sOut`.
 -
 - `fakeReply` and the blockram together form a pipeline. This pipeline needs
 - to stall whenever `sOutReady` is deasserted. This is accomplished by using
 - `asyncRamPow2` with separate enable lines for the read path and the write
 - path.
 -}
responderStream
    :: HiddenClockResetEnable dom
    => "sIn" :::
           Signal dom
               ( Maybe ( "more" ::: Bool
                       , "data" ::: Unsigned 8
                       )
               )
       -- ^ Stream-in
    -> "sOutReady" ::: Signal dom Bool
       -- ^ Stream-out ready
    -> ( "sInReady" ::: Signal dom Bool
         -- ^ Stream-in ready
       , "sOut" :::
           Signal dom
               ( Maybe ( "more" ::: Bool
                       , "data" ::: Unsigned 8
                       )
               )
         -- ^ Stream-out
       )

responderStream = hideClockResetEnable responderStream'

responderStream'
    :: KnownDomain dom
    => Clock dom
    -> Reset dom
    -> Enable dom
    -> "sIn" :::
           Signal dom
               ( Maybe ( "more" ::: Bool
                       , "data" ::: Unsigned 8
                       )
               )
       -- ^ Stream-in
    -> "sOutReady" ::: Signal dom Bool
       -- ^ Stream-out ready
    -> ( "sInReady" ::: Signal dom Bool
         -- ^ Stream-in ready
       , "sOut" :::
           Signal dom
               ( Maybe ( "more" ::: Bool
                       , "data" ::: Unsigned 8
                       )
               )
         -- ^ Stream-out
       )

responderStream' clk rst en sIn sOutReady = (sInReady, sOut)
    where
        (ramWrite, sInReady, sendReply)
            = withClockResetEnable clk rst en scanEcho sIn ramBusy
        (ramBusy, readAddr, sOut)
            = withClock clk $ withReset rst $ withEnable sOutEn
              fakeReply sendReply readData'

        readData = CEP.asyncRamPow2 clk clk en readAddr' ramWrite

        readData' = CEP.register clk rst sOutEn 0 readData
        readAddr' = CEP.register clk rst sOutEn 0 readAddr
        sOutEn = CEP.enable en sOutReady

{-
 - Top-level entity for network communication PoC.
 -
 - See the top-level README in this repository for how to use top entities
 - with this "packet_fifo" name and structure.
 -}
{-# ANN responder
    (Synthesize
        { t_name   = "packet_fifo"
        , t_inputs
            = [ PortName "clk"
              , PortName "rst_n"
              , PortName "fpga_debounced_buttons"
              , avalonMasterExtInputNames "fifo_f2h_in_mm_"
              , avalonMasterExtInputNames "fifo_h2f_out_mm_"
              ]
        , t_output
            = PortProduct ""
                [ PortName "fpga_led_internal"
                , avalonMasterExtOutputNames "fifo_f2h_in_mm_"
                , avalonMasterExtOutputNames "fifo_h2f_out_mm_"
                ]
        }) #-}
responder
    :: Clock System
    -> "rst_n" ::: Signal System Bool
    -- ^ Active-low asynchronous reset
    -> "fpga_debounced_buttons" ::: Signal System (BitVector 3)
    -> "fifo_f2h_in" :::
           ( Signal System (Unsigned 32)
           -- ^ fifo_f2h_in_mm_external_interface_read_data
           , Signal System Bool
           -- ^ fifo_f2h_in_mm_external_interface_acknowledge
           )
    -> "fifo_f2h_out" :::
           ( Signal System (Unsigned 32)
           -- ^ fifo_h2f_out_mm_external_interface_read_data
           , Signal System Bool
           -- ^ fifo_h2f_out_mm_external_interface_acknowledge
           )
    -> ( "fpga_led_internal" ::: Signal System (Unsigned 10)
       , "fifo_f2h_in" :::
             ( Signal System (Unsigned 3)
             -- ^ fifo_f2h_in_mm_external_interface_address
             , Signal System (Unsigned 32)
             -- ^ fifo_f2h_in_mm_external_interface_write_data
             , Signal System Bool
             -- ^ fifo_f2h_in_mm_external_interface_read
             , Signal System Bool
             -- ^ fifo_f2h_in_mm_external_interface_write
             , Signal System (BitVector 4)
             -- ^ fifo_f2h_in_mm_external_interface_byte_enable
             )
       , "fifo_f2h_out" :::
             ( Signal System (Unsigned 6)
             -- ^ fifo_h2f_out_mm_external_interface_address
             , Signal System (Unsigned 32)
             -- ^ fifo_h2f_out_mm_external_interface_write_data
             , Signal System Bool
             -- ^ fifo_h2f_out_mm_external_interface_read
             , Signal System Bool
             -- ^ fifo_h2f_out_mm_external_interface_write
             , Signal System (BitVector 4)
             -- ^ fifo_h2f_out_mm_external_interface_byte_enable
             )
       )
responder clk rst_n
    = withClockResetEnable clk rstS enableGen responder'
    where
        rstS = resetSynchronizer clk (unsafeFromLowPolarity rst_n) enableGen
{-# NOINLINE responder #-}

responder'
    :: SystemClockResetEnable
    => "fpga_debounced_buttons" ::: Signal System (BitVector 3)
    -> "fifo_f2h_in" :::
           ( Signal System (Unsigned 32)
           -- ^ fifo_f2h_in_mm_external_interface_read_data
           , Signal System Bool
           -- ^ fifo_f2h_in_mm_external_interface_acknowledge
           )
    -> "fifo_f2h_out" :::
           ( Signal System (Unsigned 32)
           -- ^ fifo_h2f_out_mm_external_interface_read_data
           , Signal System Bool
           -- ^ fifo_h2f_out_mm_external_interface_acknowledge
           )
    -> ( "fpga_led_internal" ::: Signal System (Unsigned 10)
       , "fifo_f2h_in" :::
             ( Signal System (Unsigned 3)
             -- ^ fifo_f2h_in_mm_external_interface_address
             , Signal System (Unsigned 32)
             -- ^ fifo_f2h_in_mm_external_interface_write_data
             , Signal System Bool
             -- ^ fifo_f2h_in_mm_external_interface_read
             , Signal System Bool
             -- ^ fifo_f2h_in_mm_external_interface_write
             , Signal System (BitVector 4)
             -- ^ fifo_f2h_in_mm_external_interface_byte_enable
             )
       , "fifo_f2h_out" :::
             ( Signal System (Unsigned 6)
             -- ^ fifo_h2f_out_mm_external_interface_address
             , Signal System (Unsigned 32)
             -- ^ fifo_h2f_out_mm_external_interface_write_data
             , Signal System Bool
             -- ^ fifo_h2f_out_mm_external_interface_read
             , Signal System Bool
             -- ^ fifo_h2f_out_mm_external_interface_write
             , Signal System (BitVector 4)
             -- ^ fifo_h2f_out_mm_external_interface_byte_enable
             )
       )
responder' _ f2hIn h2fIn
    = (pure 0, f2hOut, h2fOut)
    where
        (f2hOut, f2hOpReady, f2hRes)
            = avalonMaster
                f2hIn f2hOp f2hResReady

        (h2fOut, h2fOpReady, h2fRes)
            = avalonMaster
                h2fIn h2fOp h2fResReady

        (h2fResReady, h2fPacket, h2fOp)
            = packetReader h2fRes h2fPacketReady h2fOpReady

        (f2hPacketReady, f2hOp) = packetWriter f2hPacket f2hOpReady
        f2hResReady = pure True

        (h2fPacketReady, sIn)
            = packetToByteStream h2fPacket sInReady

        (sInReady, sOut) = responderStream sIn sOutReady

        (sOutReady, f2hPacket)
            = byteStreamToPacket sOut f2hPacketReady
