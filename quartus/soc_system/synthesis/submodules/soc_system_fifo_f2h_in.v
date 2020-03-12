//Legal Notice: (C)2020 Altera Corporation. All rights reserved.  Your
//use of Altera Corporation's design tools, logic functions and other
//software and tools, and its AMPP partner logic functions, and any
//output files any of the foregoing (including device programming or
//simulation files), and any associated documentation or information are
//expressly subject to the terms and conditions of the Altera Program
//License Subscription Agreement or other applicable license agreement,
//including, without limitation, that your use is for the sole purpose
//of programming logic devices manufactured by Altera and sold by Altera
//or its authorized distributors.  Please refer to the applicable
//agreement for further details.

// synthesis translate_off
`timescale 1ns / 1ps
// synthesis translate_on

// turn off superfluous verilog processor warnings 
// altera message_level Level1 
// altera message_off 10034 10035 10036 10037 10230 10240 10030 

module soc_system_fifo_f2h_in_single_clock_fifo (
                                                  // inputs:
                                                   aclr,
                                                   clock,
                                                   data,
                                                   rdreq,
                                                   wrreq,

                                                  // outputs:
                                                   empty,
                                                   full,
                                                   q
                                                )
;

  output           empty;
  output           full;
  output  [ 31: 0] q;
  input            aclr;
  input            clock;
  input   [ 31: 0] data;
  input            rdreq;
  input            wrreq;


wire             empty;
wire             full;
wire    [ 31: 0] q;
  scfifo single_clock_fifo
    (
      .aclr (aclr),
      .clock (clock),
      .data (data),
      .empty (empty),
      .full (full),
      .q (q),
      .rdreq (rdreq),
      .wrreq (wrreq)
    );

  defparam single_clock_fifo.add_ram_output_register = "OFF",
           single_clock_fifo.intended_device_family = "CYCLONEV",
           single_clock_fifo.lpm_numwords = 8,
           single_clock_fifo.lpm_showahead = "OFF",
           single_clock_fifo.lpm_type = "scfifo",
           single_clock_fifo.lpm_width = 32,
           single_clock_fifo.lpm_widthu = 3,
           single_clock_fifo.overflow_checking = "ON",
           single_clock_fifo.underflow_checking = "ON",
           single_clock_fifo.use_eab = "ON";


endmodule


// synthesis translate_off
`timescale 1ns / 1ps
// synthesis translate_on

// turn off superfluous verilog processor warnings 
// altera message_level Level1 
// altera message_off 10034 10035 10036 10037 10230 10240 10030 

module soc_system_fifo_f2h_in_scfifo_with_controls (
                                                     // inputs:
                                                      clock,
                                                      data,
                                                      rdreq,
                                                      reset_n,
                                                      wrreq,

                                                     // outputs:
                                                      empty,
                                                      full,
                                                      q
                                                   )
;

  output           empty;
  output           full;
  output  [ 31: 0] q;
  input            clock;
  input   [ 31: 0] data;
  input            rdreq;
  input            reset_n;
  input            wrreq;


wire             empty;
wire             full;
wire    [ 31: 0] q;
  //the_scfifo, which is an e_instance
  soc_system_fifo_f2h_in_single_clock_fifo the_scfifo
    (
      .aclr  (~reset_n),
      .clock (clock),
      .data  (data),
      .empty (empty),
      .full  (full),
      .q     (q),
      .rdreq (rdreq),
      .wrreq (wrreq)
    );


endmodule


// synthesis translate_off
`timescale 1ns / 1ps
// synthesis translate_on

// turn off superfluous verilog processor warnings 
// altera message_level Level1 
// altera message_off 10034 10035 10036 10037 10230 10240 10030 

module soc_system_fifo_f2h_in_map_avalonmm_to_avalonst (
                                                         // inputs:
                                                          avalonmm_data,

                                                         // outputs:
                                                          avalonst_data
                                                       )
;

  output  [ 31: 0] avalonst_data;
  input   [ 31: 0] avalonmm_data;


wire    [ 31: 0] avalonst_data;
  assign avalonst_data[31 : 24] = avalonmm_data[7 : 0];
  assign avalonst_data[23 : 16] = avalonmm_data[15 : 8];
  assign avalonst_data[15 : 8] = avalonmm_data[23 : 16];
  assign avalonst_data[7 : 0] = avalonmm_data[31 : 24];

endmodule


// synthesis translate_off
`timescale 1ns / 1ps
// synthesis translate_on

// turn off superfluous verilog processor warnings 
// altera message_level Level1 
// altera message_off 10034 10035 10036 10037 10230 10240 10030 

module soc_system_fifo_f2h_in_single_clock_fifo_for_other_info (
                                                                 // inputs:
                                                                  aclr,
                                                                  clock,
                                                                  data,
                                                                  rdreq,
                                                                  wrreq,

                                                                 // outputs:
                                                                  q
                                                               )
;

  output  [  3: 0] q;
  input            aclr;
  input            clock;
  input   [  3: 0] data;
  input            rdreq;
  input            wrreq;


wire    [  3: 0] q;
  scfifo single_clock_fifo
    (
      .aclr (aclr),
      .clock (clock),
      .data (data),
      .q (q),
      .rdreq (rdreq),
      .wrreq (wrreq)
    );

  defparam single_clock_fifo.add_ram_output_register = "OFF",
           single_clock_fifo.intended_device_family = "CYCLONEV",
           single_clock_fifo.lpm_numwords = 8,
           single_clock_fifo.lpm_showahead = "OFF",
           single_clock_fifo.lpm_type = "scfifo",
           single_clock_fifo.lpm_width = 4,
           single_clock_fifo.lpm_widthu = 3,
           single_clock_fifo.overflow_checking = "ON",
           single_clock_fifo.underflow_checking = "ON",
           single_clock_fifo.use_eab = "ON";


endmodule


// synthesis translate_off
`timescale 1ns / 1ps
// synthesis translate_on

// turn off superfluous verilog processor warnings 
// altera message_level Level1 
// altera message_off 10034 10035 10036 10037 10230 10240 10030 

module soc_system_fifo_f2h_in_map_avalonmm_to_avalonst_other_info (
                                                                    // inputs:
                                                                     auto_clr,
                                                                     avalonmm_other_info,
                                                                     clock,
                                                                     enable,
                                                                     reset_n,

                                                                    // outputs:
                                                                     avalonst_other_info
                                                                  )
;

  output  [  3: 0] avalonst_other_info;
  input            auto_clr;
  input   [ 31: 0] avalonmm_other_info;
  input            clock;
  input            enable;
  input            reset_n;


wire    [  3: 0] avalonst_other_info;
wire    [  1: 0] empty;
reg     [  1: 0] empty_q;
wire             eop;
reg              eop_q;
wire             sop;
reg              sop_q;
  assign empty = avalonmm_other_info[3 : 2];
  assign sop = avalonmm_other_info[0];
  assign eop = avalonmm_other_info[1];
  assign avalonst_other_info = {empty_q,
    eop_q,
    sop_q};

  always @(posedge clock or negedge reset_n)
    begin
      if (reset_n == 0)
          sop_q <= 0;
      else if (enable | auto_clr)
          if (auto_clr)
              sop_q <= 0;
          else 
            sop_q <= sop;
    end


  always @(posedge clock or negedge reset_n)
    begin
      if (reset_n == 0)
          eop_q <= 0;
      else if (enable | auto_clr)
          if (auto_clr)
              eop_q <= 0;
          else 
            eop_q <= eop;
    end


  always @(posedge clock or negedge reset_n)
    begin
      if (reset_n == 0)
          empty_q <= 0;
      else if (enable)
          empty_q <= empty;
    end



endmodule


// synthesis translate_off
`timescale 1ns / 1ps
// synthesis translate_on

// turn off superfluous verilog processor warnings 
// altera message_level Level1 
// altera message_off 10034 10035 10036 10037 10230 10240 10030 

module soc_system_fifo_f2h_in_map_fifo_other_info_to_avalonst (
                                                                // inputs:
                                                                 data_in,

                                                                // outputs:
                                                                 avalonst_source_empty,
                                                                 avalonst_source_endofpacket,
                                                                 avalonst_source_startofpacket
                                                              )
;

  output  [  1: 0] avalonst_source_empty;
  output           avalonst_source_endofpacket;
  output           avalonst_source_startofpacket;
  input   [  3: 0] data_in;


wire    [  1: 0] avalonst_source_empty;
wire             avalonst_source_endofpacket;
wire             avalonst_source_startofpacket;
  assign {avalonst_source_empty,
avalonst_source_endofpacket,
avalonst_source_startofpacket} = data_in;

endmodule


// synthesis translate_off
`timescale 1ns / 1ps
// synthesis translate_on

// turn off superfluous verilog processor warnings 
// altera message_level Level1 
// altera message_off 10034 10035 10036 10037 10230 10240 10030 

module soc_system_fifo_f2h_in (
                                // inputs:
                                 avalonmm_write_slave_address,
                                 avalonmm_write_slave_write,
                                 avalonmm_write_slave_writedata,
                                 reset_n,
                                 wrclock,

                                // outputs:
                                 avalonst_source_data,
                                 avalonst_source_empty,
                                 avalonst_source_endofpacket,
                                 avalonst_source_startofpacket,
                                 avalonst_source_valid
                              )
;

  output  [ 31: 0] avalonst_source_data;
  output  [  1: 0] avalonst_source_empty;
  output           avalonst_source_endofpacket;
  output           avalonst_source_startofpacket;
  output           avalonst_source_valid;
  input            avalonmm_write_slave_address;
  input            avalonmm_write_slave_write;
  input   [ 31: 0] avalonmm_write_slave_writedata;
  input            reset_n;
  input            wrclock;


wire    [ 31: 0] avalonmm_map_data_in;
wire    [ 31: 0] avalonst_map_data_out;
wire    [  3: 0] avalonst_other_info;
wire    [ 31: 0] avalonst_source_data;
wire    [  1: 0] avalonst_source_empty;
wire             avalonst_source_endofpacket;
wire             avalonst_source_startofpacket;
reg              avalonst_source_valid;
wire             clock;
wire    [ 31: 0] data;
wire             empty;
wire             full;
wire    [ 31: 0] q;
wire    [  3: 0] q_i;
wire             rdreq;
wire             rdreq_i;
wire             wrreq;
wire             wrreq_driver;
  //the_scfifo_with_controls, which is an e_instance
  soc_system_fifo_f2h_in_scfifo_with_controls the_scfifo_with_controls
    (
      .clock   (clock),
      .data    (data),
      .empty   (empty),
      .full    (full),
      .q       (q),
      .rdreq   (rdreq),
      .reset_n (reset_n),
      .wrreq   (wrreq)
    );

  //in, which is an e_avalon_slave
  //the_map_avalonmm_to_avalonst, which is an e_instance
  soc_system_fifo_f2h_in_map_avalonmm_to_avalonst the_map_avalonmm_to_avalonst
    (
      .avalonmm_data (avalonmm_map_data_in),
      .avalonst_data (avalonst_map_data_out)
    );

  assign wrreq_driver = (avalonmm_write_slave_address == 0) & avalonmm_write_slave_write;
  assign avalonmm_map_data_in = avalonmm_write_slave_writedata;
  assign wrreq = wrreq_driver;
  assign data = avalonst_map_data_out;
  assign clock = wrclock;
  //the_scfifo_other_info, which is an e_instance
  soc_system_fifo_f2h_in_single_clock_fifo_for_other_info the_scfifo_other_info
    (
      .aclr  (~reset_n),
      .clock (clock),
      .data  (avalonst_other_info),
      .q     (q_i),
      .rdreq (rdreq_i),
      .wrreq (wrreq_driver & ~full)
    );

  //the_map_avalonmm_to_avalonst_other_info, which is an e_instance
  soc_system_fifo_f2h_in_map_avalonmm_to_avalonst_other_info the_map_avalonmm_to_avalonst_other_info
    (
      .auto_clr            (wrreq_driver & !full),
      .avalonmm_other_info (avalonmm_write_slave_writedata),
      .avalonst_other_info (avalonst_other_info),
      .clock               (clock),
      .enable              ((avalonmm_write_slave_address == 1) & avalonmm_write_slave_write),
      .reset_n             (reset_n)
    );

  //the_map_fifo_other_info_to_avalonst, which is an e_instance
  soc_system_fifo_f2h_in_map_fifo_other_info_to_avalonst the_map_fifo_other_info_to_avalonst
    (
      .avalonst_source_empty         (avalonst_source_empty),
      .avalonst_source_endofpacket   (avalonst_source_endofpacket),
      .avalonst_source_startofpacket (avalonst_source_startofpacket),
      .data_in                       (q_i)
    );

  assign avalonst_source_data = q;
  assign rdreq = !empty;
  assign rdreq_i = rdreq;
  always @(posedge clock or negedge reset_n)
    begin
      if (reset_n == 0)
          avalonst_source_valid <= 0;
      else 
        avalonst_source_valid <= !empty;
    end


  //out, which is an e_atlantic_master

endmodule

