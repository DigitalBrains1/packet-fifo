module Test.Avalon.Master.Writer where

import Clash.Prelude

import Avalon.Master
import Test.Avalon.Master.MockSlave
import Toolbox.Test

{-# ANN avalonMasterWriter
    (Synthesize
        { t_name   = "avalon_master_test"
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
{-# NOINLINE avalonMasterWriter #-}
avalonMasterWriter clk rst_n
    = exposeClockResetEnable avalonMasterWriter' clk rstS enableGen
    where
        rstS = resetSynchronizer clk (unsafeFromLowPolarity rst_n) enableGen

avalonMasterWriter'
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

avalonMasterWriter' _ f2hIn h2fIn
    = ( pure 0
      , ( traceSignal1 "f2hAddr" f2hAddr
        , traceSignal1 "f2hWData" f2hWData
        , traceSignal1 "f2hRead" f2hRead
        , traceSignal1 "f2hWrite" f2hWrite
        , traceSignal1 "f2hBE" f2hBE
        )
      , h2fOut
      )
    where
        (f2hRData, f2hAck) = f2hIn
        (f2hAddr, f2hWData, f2hRead, f2hWrite, f2hBE) = f2hOut

        (f2hOut, f2hOpReady, f2hRes)
            = avalonMaster
                ( (traceSignal1 "f2hRData" f2hRData)
                , (traceSignal1 "f2hAck" f2hAck)
                )
                f2hOp (pure True)

        (h2fOut, h2fOpReady, h2fRes)
            = avalonMaster
                h2fIn h2fOp h2fResReady

        h2fResReady = pure True
        h2fOp = ( pure False, pure undefined, pure ()
                , pure (undefined :: Unsigned 6), pure undefined)

        f2hOp = writeDummyCounter f2hOpReady

writeDummyCounter
    :: HiddenClockResetEnable dom
    => Signal dom Bool
    -> ( Signal dom Bool
       , Signal dom AvalonCmd
       , Signal dom ()
       , Signal dom (Unsigned 3)
       , Signal dom (Unsigned 32)
       )
writeDummyCounter opReady
    = mealyB writeDummyCounter' 0 opReady

writeDummyCounter' cnt opReady = (cnt', (opValid, AvalonWrite, (), 0, cnt))
    where
        maxCnt = 256
        opValid = cnt /= maxCnt
        cnt' | cnt == maxCnt = cnt
             | opReady       = cnt + 1
             | otherwise     = cnt

mockTopEntity
    :: Signal System Bool

mockTopEntity = f2hAck
    where

        (f2hRData, f2hAck) = f2hIn

        (_, f2hOut, _)
            = avalonMasterWriter
                clockGen (pure True) (pure 0b111) f2hIn
                (pure undefined, pure False)
        f2hIn
            = withClockResetEnable
                clockGen resetGen enableGen
                mockAvalonSlave f2hOut

makeVCD
    = writeVCD' "avmw.vcd"
        mockTopEntity
        [ "f2hAddr"
        , "f2hWData"
        , "f2hRead"
        , "f2hWrite"
        , "f2hBE"
        , "f2hRData"
        , "f2hAck"
        ]
