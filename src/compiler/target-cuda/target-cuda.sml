

structure TargetCUDA : sig
              val target : TargetOptions.t -> {
                      info : TreeIR.target_info,
                      generate : CmdLineConstants.t * TreeIR.program -> unit
                      
                  }
          end = struct

structure Spec = TargetSpec
fun generate(tgt : TargetOptions.t) (defs, prog) = let
    val spec = TargetSpec.mk (tgt, prog)
in
    if(#exec spec)
    then CudaGen.exec (spec, defs, prog)
    else CudaGen.library (spec, defs, prog)
end
                                                       
fun info (tgt : TargetOptions.t) = {
    layout = if (#scalar tgt)
            then VectorLayout.scalar
            else VectorLayout.layout [1, 2, 3, 4],
    isInline =
    fn LowOps.RealToInt d => (d = 1) 
    | LowOps.EigenVecs2x2 => false
    | LowOps.EigenVecs3x3 => false
    | LowOps.EigenVals2x2 => false
    | LowOps.EigenVals3x3 => false
    | LowOps.Zero _ => false
    | _ => true
}

fun target tgt = {
    info = info tgt,
    generate = generate tgt
}

end
