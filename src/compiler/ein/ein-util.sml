(* ein-util.sml
 *
 * This code is part of the Diderot Project (http://diderot-language.cs.uchicago.edu)
 *
 * COPYRIGHT (c) 2015 The University of Chicago
 * All rights reserved.
 *)

structure EinUtil : sig

  (* Are Ein functions/expressions the same? *)
    val same : Ein.ein * Ein.ein -> bool
    val sameExp : Ein.ein_exp * Ein.ein_exp -> bool

  (* compute a hash for an Ein function *)
    val hash : Ein.ein -> Word.word

    val iterPP: Ein.ein_exp list -> Ein.ein_exp
    val iterAA: Ein.ein_exp list -> Ein.ein_exp

  end = struct

    structure E = Ein

    fun sameExp (e1, e2) = let
        fun sameIndex ([], []) = true
          | sameIndex ((E.V i)::ix, (E.V j)::jx) = (i = j) andalso sameIndex (ix, jx)
          | sameIndex ((E.C i)::ix, (E.C j)::jx) = (i = j) andalso sameIndex (ix, jx)
          | sameIndex _ = false
        fun sameKx ([], [])=true
          | sameKx ((E.V i,_)::ix, (E.V j,_)::jx) = (i = j) andalso sameKx(ix, jx)
          | sameKx ((E.C i,_)::ix, (E.C j,_)::jx) = (i = j) andalso sameKx(ix, jx)
          | sameKx _ = false
        fun sameSx ([], []) = true
          | sameSx ((i,_,_)::ix, (j,_,_)::jx) = (i = j) andalso sameSx(ix, jx)
          | sameSx _ = false
        fun sameOp1 (E.Neg, E.Neg) = true
          | sameOp1 (E.Exp, E.Exp) = true
          | sameOp1 (E.Sqrt, E.Sqrt) = true
          | sameOp1 (E.Cosine, E.Cosine) = true
          | sameOp1 (E.ArcCosine, E.ArcCosine) = true
          | sameOp1 (E.Sine, E.Sine) = true
          | sameOp1 (E.ArcSine, E.ArcSine) = true
          | sameOp1 (E.Tangent, E.Tangent) = true
          | sameOp1 (E.ArcTangent, E.ArcTangent) = true
          | sameOp1 (E.PowInt n1, E.PowInt n2) = (n1 = n2)
          | sameOp1 (E.Abs, E.Abs) = true
          | sameOp1 (E.Sgn, E.Sgn) = true
          | sameOp1 _ = false
        fun same (e1, e2) = (case (e1, e2)
               of (E.Const c1, E.Const c2) => (c1 = c2)
                | (E.ConstR r1, E.ConstR r2) => Rational.same(r1, r2)
                | (E.Tensor(id1, ix1), E.Tensor(id2, ix2)) =>
                    (id1 = id2) andalso sameIndex(ix1, ix2)
                | (E.Zero(ix1), E.Zero(ix2)) => sameIndex(ix1, ix2)
                | (E.Delta(ix1, jx1), E.Delta(ix2, jx2)) =>
                    (ix1 = ix2) andalso (jx1 = jx2)
                | (E.Epsilon(i1, j1, k1), E.Epsilon(i2, j2, k2)) =>
                    (i1 = i2) andalso (j1 = j2) andalso (k1 = k2)
                | (E.Eps2(i1, j1), E.Eps2(i2, j2)) => (i1 = i2) andalso (j1 = j2)
                | (E.Field(id1, ix1), E.Field(id2, ix2)) =>
                    (id1 = id2) andalso sameIndex(ix1, ix2)
                | (E.Lift e1, E.Lift e2) => same (e1, e2)
                | (E.Conv(fid1, alpha1, tid1, ix1), E.Conv(fid2, alpha2, tid2, ix2)) =>
                    (fid1 = fid2) andalso (tid1 = tid2) andalso
                      sameIndex (alpha1, alpha2) andalso sameIndex (ix1, ix2)
                | (E.Partial ix, E.Partial jx) => sameIndex(ix, jx)
                | (E.Apply(e11, e12), E.Apply(e21, e22)) => same(e11, e21) andalso same(e12, e22)
                | (E.Comp(e11, es1), E.Comp(e21, es2)) =>
                    same(e11, e21) andalso sameSubEin(es1, es2)
                | (E.Probe(e11, e12), E.Probe(e21, e22)) => same(e11, e21) andalso same(e12, e22)
                | (E.Value i, E.Value j) => (i = j)
                | (E.Img(id1, ix1, pos1, s1), E.Img(id2, ix2, pos2, s2)) =>
                    (id1 = id2) andalso sameList(pos1, pos2) andalso sameIndex(ix1, ix2) andalso (s1 = s2)
                | (E.Krn(id1, ix1, dim1), E.Krn(id2, ix2, dim2)) =>
                    (id1 = id2) andalso sameKx(ix1, ix2) andalso (dim1 =  dim2)
                | (E.OField(E.CFExp (es1), e1, ix1), E.OField(E.CFExp (es2), e2, ix2)) =>
                    same(e1, e2) andalso same(ix1, ix2) andalso ListPair.allEq (op =) (es1, es2)
                | (E.Poly(e1, n1,alpha1), E.Poly(e2, n2, alpha2)) =>
                     same(e1, e2) andalso (n1 = n2) andalso sameIndex(alpha1, alpha2)
                | (E.Sum(c1, e1), E.Sum(c2, e2)) => sameSx(c1, c2) andalso same(e1, e2)
                | (E.Op1(op1, e1), E.Op1(op2, e2)) => sameOp1(op1, op2) andalso same(e1, e2)
                | (E.Op2(op1, e11, e12), E.Op2(op2, e21, e22)) =>
                    (op1 = op2) andalso same(e11, e21) andalso same(e12, e22)
                | (E.Op3(op1, e11, e12, e13), E.Op3(op2, e21, e22, e23)) =>
                    (op1 = op2) andalso same(e11, e21) andalso same(e12, e22) andalso same(e13, e23)
                | (E.Opn(op1, es1), E.Opn(op2, es2)) =>
                    (op1 = op2) andalso sameList(es1, es2)
                | _ => false
              (* end case *))
        and sameSubEin([], []) = true
            | sameSubEin ((e1 ,_)::es1, (e2, _)::es2) = same(e1, e2) andalso sameSubEin(es1, es2)
            | sameSubEin _ = false
        and sameList ([], []) = true
          | sameList (e1::es1, e2::es2) = same(e1, e2) andalso sameList(es1, es2)
          | sameList _ = false
        in
          same (e1, e2)
        end

    fun same (E.EIN{params=p1, index=ix1, body=e1}, E.EIN{params=p2, index=ix2, body=e2}) = let
          fun sameParam (E.TEN(i1, shp1), E.TEN(i2, shp2)) =
                (i1 = i2) andalso ListPair.allEq (op =) (shp1, shp2)
            | sameParam (E.FLD i1, E.FLD i2) = (i1 = i2)
            | sameParam (E.KRN, E.KRN) = true
            | sameParam (E.IMG(i1, shp1), E.IMG(i2, shp2)) =
                (i1 = i2) andalso ListPair.allEq (op =) (shp1, shp2)
            | sameParam _ = false
          in
            ListPair.allEq sameParam (p1, p2) andalso
              ListPair.allEq (op =) (ix1, ix2) andalso
                sameExp (e1, e2)
          end

    fun hash (Ein.EIN{body, ...}) = let
        fun hash' body = let
            fun hashInt i = Word.fromInt i
            fun iter [e] = hash' e
              | iter (e1::es) = hash' e1 + iter es
            fun iterS [(e,_)] = hash' e
              | iterS ((e1,_)::es) = hash' e1 + iterS es
            fun hashMu (E.C c) = hashInt c + 0w17
              | hashMu (E.V v) = hashInt v
            fun hashAlpha [] = 0w3
              | hashAlpha (e1::es) = hashMu e1 + hashAlpha es
            fun hashDels [] = 0w5
              | hashDels ((i, j)::es) = hashMu i + hashMu j + hashDels es
            in
              case body
               of E.Const i => hashInt i + 0w3
                | E.ConstR _ => 0w5
                | E.Tensor(_, alpha) => 0w23 + hashAlpha alpha
                | E.Zero(alpha) => 0w107 + hashAlpha alpha
                | E.Delta _ => 0w7
                | E.Epsilon _ => 0w13
                | E.Eps2 _ => 0w17
                | E.Field(_, alpha) => 0w29 + hashAlpha alpha
                | E.Lift e1 => 0w61 + hash' e1
                | E.Conv(_, alpha, _, dx) =>
                    0w37 + hashAlpha alpha + hashAlpha dx + hashInt(length dx)
                | E.Partial alpha => 0w19+hashAlpha alpha
                | E.Apply(e1, e2) => 0w97 + hash' e1 + hash' e2
                | E.Comp(e1, es) => 0w141 + hash' e1 + iterS es
                | E.Probe(e1, e2) => 0w101 + hash' e1 + hash' e2
                | E.Value _ => 0w11
                | E.Img (_, alpha, es, _) => 0w43 + hashAlpha alpha + iter es
                | E.Krn (_, dels, dim) => 0w41 + hashDels dels + hashInt dim
                | E.OField(ofld, e2, alpha) => 0w141 +hash' e2  + hash' alpha
                | E.Poly(e1, n1, alpha2) => 0w143 + hash' e1 + hashInt n1 + hashAlpha alpha2
                | E.Sum(c,e1) => 0w53 + hash' e1
                | E.Op1(e1,e2) => (case e1
                     of E.Cosine => 0w113 + hash' e2
                      | E.ArcCosine => 0w127 + hash' e2
                      | E.Sine => 0w131 + hash' e2
                      | E.ArcSine => 0w137 + hash' e2
                      | E.Tangent => 0w139 + hash' e2
                      | E.ArcTangent => 0w149 + hash' e2
                      | E.Neg => 0w59 + hash' e2
                      | E.Sqrt => 0w67 + hash' e2
                      | E.PowInt _ => 0w107 + hash' e2
                      | E.Exp => 0w151 + hash' e2
                      | E.Abs => 0w157 + hash' e2
                      | E.Sgn => 0w157 + hash' e2
                    (* end case *))
                | E.Op2(E.Sub, e1, e2) => 0w79 + hash' e1 + hash' e2
                | E.Op2(E.Div, e1, e2) => 0w83 + hash' e1 + hash' e2
                | E.Op3(E.Clamp, e1, e2, e3) => 0w163 + hash' e1 + hash' e2 + hash' e3
                | E.Opn(E.Add, es) => 0w71 + iter es
                | E.Opn(E.Prod, es) => 0w103 + iter es
              (* end case *)
            end
        in
          hash' body
        end

    fun iterPP es = let
          fun iterP ([], [r]) = r
        | iterP ([], rest) = E.Opn(E.Prod, rest)
        | iterP (E.Const 0::es, rest) = E.Const(0)
        | iterP (E.Const 1::es, rest) = iterP(es, rest)
        | iterP (E.Delta(E.C c1, E.V v1)::E.Delta(E.C c2, E.V v2)::es, rest) =
          (* variable can't be 0 and 1 '*)
        if (c1 = c2)
          then iterP (es, E.Delta(E.C c1, E.V v1)::E.Delta(E.C c2, E.V v2)::rest)
          else E.Const(0)
        | iterP (E.Opn(E.Prod, ys)::es, rest) = iterP(ys@es, rest)
        | iterP (e1::es, rest)   = iterP(es, e1::rest)
          in
            iterP (es, [])
          end

    fun iterAA es = let
      fun iterA ([], []) = E.Const 0
        | iterA ([], [r]) = r
        | iterA ([], rest) = E.Opn(E.Add, rest)
        | iterA (E.Const 0::es, rest) = iterA(es, rest)
        | iterA (E.Opn(E.Add, ys)::es, rest) = iterA(ys@es, rest)
        | iterA (e1::es, rest) = iterA(es, e1::rest)
      in
            iterA (es, [])
          end

  end
