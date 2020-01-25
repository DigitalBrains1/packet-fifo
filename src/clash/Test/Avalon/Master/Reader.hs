module Test.Avalon.Master.Reader where

import Clash.Prelude

import Avalon.Master

{-# ANN avalonMasterReader
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
{-# NOINLINE avalonMasterReader #-}
avalonMasterReader clk rst_n
    = exposeClockResetEnable avalonMasterReader' clk rstS enableGen
    where
        rstS = resetSynchronizer clk (unsafeFromLowPolarity rst_n) enableGen

avalonMasterReader'
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

avalonMasterReader' _ f2hIn h2fIn
    = ( leds
      , f2hOut
      , h2fOut)
    where
        (f2hOut, f2hOpReady, f2hRes)
            = avalonMaster
                f2hIn f2hOp f2hResReady

        f2hResReady = pure True
        f2hOp = ( pure False, pure undefined, pure ()
                , pure (undefined :: Unsigned 3), pure undefined)

        (h2fOut, h2fOpReady, h2fRes)
            = avalonMaster
                h2fIn h2fOp h2fResReady

        (leds, h2fResReady, h2fOp)
            = ledFillLevel h2fOpReady h2fRes


        (f2hOpValid, f2hOpAddr, f2hOpData)
            = (pure False, pure undefined, pure undefined)
              :: ( Signal System Bool, Signal System (Unsigned 3)
                 , Signal System (Unsigned 32))

ledFillLevel
    :: KnownDomain dom
    => Signal dom Bool
    -> ( Signal dom Bool
       , Signal dom ()
       , Signal dom (Unsigned 32))
    -> ( Signal dom (Unsigned 10)
       , Signal dom Bool
       , ( Signal dom Bool
         , Signal dom AvalonCmd
         , Signal dom ()
         , Signal dom (Unsigned 6)
         , Signal dom (Unsigned 32)
         )
       )

ledFillLevel opReady res = (leds, resReady, op)
    where
        op = (pure True, pure AvalonRead, pure (), pure 0, pure undefined)
        opValid = pure True
        resReady = pure True
        (_, _, resData) = res
        leds = truncateB <$> resData
