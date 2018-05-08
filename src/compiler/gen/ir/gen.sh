#!/bin/sh
#
# run the IR generation commands.
#

sml gen-ops.sml <<XXXX
GenOps.main ("", ["high-ir.spec", "high-ir.in", "../../high-ir/high-ir.sml"]);
XXXX

sml gen-ops.sml <<XXXX
GenOps.main ("", ["mid-ir.spec", "mid-ir.in", "../../mid-ir/mid-ir.sml"]);
XXXX

sml gen-ops.sml <<XXXX
GenOps.main ("", ["low-ir.spec", "low-ir.in", "../../low-ir/low-ir.sml"]);
XXXX

sml gen-ops.sml <<XXXX
GenOps.main ("", ["tree-ops.spec", "tree-ops.in", "../../tree-ir/tree-ops.sml"]);
XXXX
