module ICMPEcho where

import Clash.Prelude
import qualified Clash.Explicit.Prelude as CEP
import Data.Maybe
import Debug.Trace (trace)
import qualified Prelude as P

import Avalon.Master
import Avalon.PacketReader
import Avalon.PacketWriter
import Avalon.PacketToByteStream
import Avalon.ByteStreamToPacket

scanEcho
    :: HiddenClockResetEnable dom
    => Signal dom (Maybe (Bool, Unsigned 8))
       -- ^ Stream-in
    -> Signal dom Bool
       -- ^ RAM busy
    -> ( Signal dom (Maybe (Unsigned 11, Unsigned 8))
         -- ^ RAM write
       , Signal dom Bool
         -- ^ Stream-in ready
       , Signal dom (Maybe (Unsigned 11))
         -- Send reply
       )

scanEcho sIn ramBusy = mealyB scanEcho' (0, True) (sIn, ramBusy)

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

minIcmpEchoLength
    =   14    -- Ethernet header
      + 20    -- IPv4 header
      + 8     -- Minimum ICMP packet length

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

vrfyVal
    = $(listToVecTH
            ([ 0x52, 0x54, 0x00, 0xeb, 0x9b, 0xd0, 0x00, 0x00
             , 0x00, 0x00, 0x00, 0x00, 0x08, 0x00
             -- ^ ethernet header
             , 0x45, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00
             , 0x00, 0x01, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00
             , 0x0a, 0x00, 0x00, 0x02
             -- ^ ipv4 header
             , 0x08, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00
             -- ^ icmp header
             ] :: [Unsigned 8]))

fakeReply
    :: HiddenClockResetEnable dom
    => Signal dom (Maybe (Unsigned 11))
       -- ^ Send reply
    -> Signal dom (Unsigned 8)
       -- ^ RAM read data
    -> ( Signal dom Bool
         -- ^ RAM busy
       , Signal dom (Unsigned 11)
         -- ^ RAM read address
       , Signal dom (Maybe (Bool, Unsigned 8))
         -- ^ Stream-out
       )

fakeReply sendReply readData
    = mealyB fakeReply' fakeReplyIS
             (sendReply, readData)

fakeReplyIS = ( cntInit   :: Unsigned 12
              , 0         :: Unsigned 12
              , undefined :: Unsigned 8
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

        -- Note three cycle delay of readData vs. cnt
        sData
            = case cnt of
                37 -> 0                   -- ICMP type
                39 -> resize $ cksum `shiftR` 8
                _  -> prevB

        cksum = bitCoerce (prevB :> readData :> Nil) ~+~ 0x0800 :: Unsigned 16

(~+~) :: KnownNat n
      => Unsigned n
      -> Unsigned n
      -> Unsigned n
a ~+~ b = truncateB summed + resize (bitCoerce carry)
    where
        summed = a `add` b
        carry = msb summed

cntInit = 6 -- Ethernet source MAC

responderStream
    :: HiddenClockResetEnable dom
    => Signal dom (Maybe (Bool, Unsigned 8))
       -- ^ Stream in
    -> Signal dom Bool
       -- ^ Stream out ready
    -> ( Signal dom Bool
         -- ^ Stream in ready
       , Signal dom (Maybe (Bool, Unsigned 8))
         -- ^ Stream out
       )

responderStream = hideClockResetEnable responderStream'

responderStream'
    :: KnownDomain dom
    => Clock dom
    -> Reset dom
    -> Enable dom
    -> Signal dom (Maybe (Bool, Unsigned 8))
       -- ^ Stream in
    -> Signal dom Bool
       -- ^ Stream out ready
    -> ( Signal dom Bool
         -- ^ Stream in ready
       , Signal dom (Maybe (Bool, Unsigned 8))
         -- ^ Stream out
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
    -> Signal System Bool
    -> Signal System (BitVector 3)
    -- ^ fpga_debounced_buttons
    -> ( Signal System (Unsigned 32)
       -- ^ fifo_f2h_in_mm_external_interface_read_data
       , Signal System Bool
       -- ^ fifo_f2h_in_mm_external_interface_acknowledge
       )
    -> ( Signal System (Unsigned 32)
       -- ^ fifo_h2f_out_mm_external_interface_read_data
       , Signal System Bool
       -- ^ fifo_h2f_out_mm_external_interface_acknowledge
       )
    -> ( Signal System (Unsigned 10)
       -- ^ fpga_led_internal
       , ( Signal System (Unsigned 3)
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
       , ( Signal System (Unsigned 6)
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
    => Signal System (BitVector 3)
    -- ^ fpga_debounced_buttons
    -> ( Signal System (Unsigned 32)
       -- ^ fifo_f2h_in_mm_external_interface_read_data
       , Signal System Bool
       -- ^ fifo_f2h_in_mm_external_interface_acknowledge
       )
    -> ( Signal System (Unsigned 32)
       -- ^ fifo_h2f_out_mm_external_interface_read_data
       , Signal System Bool
       -- ^ fifo_h2f_out_mm_external_interface_acknowledge
       )
    -> ( Signal System (Unsigned 10)
       -- ^ fpga_led_internal
       , ( Signal System (Unsigned 3)
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
       , ( Signal System (Unsigned 6)
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
                f2hIn f2hResReady f2hOp

        (h2fOut, h2fOpReady, h2fRes)
            = avalonMaster
                h2fIn h2fResReady h2fOp

        (h2fResReady, h2fPacket, h2fOp)
            = packetReader h2fPacketReady h2fOpReady h2fRes

        (f2hPacketReady, f2hOp) = packetWriter f2hOpReady f2hPacket
        f2hResReady = pure True

        (h2fPacketReady, sIn)
            = packetToByteStream h2fPacket sInReady

        (sInReady, sOut) = responderStream sIn sOutReady

        (sOutReady, f2hPacket)
            = byteStreamToPacket sOut f2hPacketReady
