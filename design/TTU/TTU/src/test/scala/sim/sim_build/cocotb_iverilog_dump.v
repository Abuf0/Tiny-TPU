module cocotb_iverilog_dump();
initial begin
    $dumpfile("sim_build/ttu_core.fst");
    $dumpvars(0, ttu_core);
end
endmodule
