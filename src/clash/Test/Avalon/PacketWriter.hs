module Test.Avalon.PacketWriter where

import Clash.Prelude

import qualified Data.Text.IO as TIO
import Avalon.PacketWriter
import Avalon.Master
import Test.Avalon.Master.MockSlave

traceSignal1F = flip const
-- traceSignal1F = traceSignal1

{-# ANN avalonPacketWriter
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
avalonPacketWriter clk rst_n
    = exposeClockResetEnable avalonPacketWriter' clk rstS enableGen
    where
        rstS = resetSynchronizer clk (unsafeFromLowPolarity rst_n) enableGen
{-# NOINLINE avalonPacketWriter #-}

avalonPacketWriter'
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

avalonPacketWriter' _ f2hIn h2fIn
    = ( pure 0
      , ( traceSignal1F "f2hAddr" f2hAddr
        , traceSignal1F "f2hWData" f2hWData
        , traceSignal1F "f2hRead" f2hRead
        , traceSignal1F "f2hWrite" f2hWrite
        , traceSignal1F "f2hBE" f2hBE
        )
      , h2fOut
      )
    where
        (f2hRData, f2hAck) = f2hIn
        (f2hAddr, f2hWData, f2hRead, f2hWrite, f2hBE) = f2hOut

        (f2hOut, f2hOpReady, f2hRes)
            = avalonMaster
                ( (traceSignal1F "f2hRData" f2hRData)
                , (traceSignal1F "f2hAck" f2hAck)
                )
                f2hResReady f2hOp

        (h2fOut, h2fOpReady, h2fRes)
            = avalonMaster
                h2fIn h2fResReady h2fOp

        h2fResReady = pure True
        h2fOp = ( pure False, pure undefined, pure ()
                , pure (undefined :: Unsigned 6), pure undefined)

        (pInReady, f2hOp) = packetWriter f2hOpReady pIn
        f2hResReady = pure True

        pIn = writeDummyCounter pInReady

writeDummyCounter
    :: HiddenClockResetEnable dom
    => Signal dom Bool
    -> ( Signal dom Bool
       , Signal dom (Unsigned 32)
       , Signal dom (Unsigned 32)
       )
writeDummyCounter pInReady
    = mealyB writeDummyCounter' 0 pInReady

writeDummyCounter' cnt pInReady
    = (cnt', (pInValid, cnt, other))
    where
        maxCnt = 256
        pInValid = cnt /= maxCnt
        cnt' | cnt == maxCnt = cnt
             | pInReady      = cnt + 1
             | otherwise     = cnt
        other | lsb cnt == 0 = fifoInfoSOP
              | otherwise    = fifoInfoEOP

mockTopEntity
    :: Signal System Bool

mockTopEntity = f2hAck
    where

        (f2hRData, f2hAck) = f2hIn

        (_, f2hOut, _)
            = avalonPacketWriter
                clockGen (pure True) (pure 0b111) f2hIn
                (pure undefined, pure False)
        f2hIn
            = withClockResetEnable
                clockGen resetGen enableGen
                mockAvalonSlave f2hOut

makeVCD
    = do
        Right vcd <-
            dumpVCD
                (0,2000)
                mockTopEntity
                [ "f2hAck", "f2hWrite", "f2hWData"
                ]
        TIO.writeFile "avpw.vcd" vcd