(* parse-tree.sml
 *
 * Diderot parse-tree representation.
 *
 * This code is part of the Diderot Project (http://diderot-language.cs.uchicago.edu)
 *
 * COPYRIGHT (c) 2015 The University of Chicago
 * All rights reserved.
 *)

structure ParseTree =
  struct

  (* a version specifier as specified in the '#version' tag *)
    type version = int list

    type 'a mark = 'a Error.mark

    type var = Atom.atom
    type field = Atom.atom
    type dim = IntInf.int

  (* binding and certain use occurrences of variables are wrapped with location info *)
    type var_bind = var mark

    datatype program = Program of {
          globals : global_dcl list,            (* global-variable decls (including inputs) *)
          globInit : stmt option,               (* additional global initialize code (optional) *)
          strand : strand_dcl,                  (* strand definition *)
          create : create,                      (* strand creation code *)
          start : stmt option,                  (* global start code (optional) *)
          update : stmt option                  (* global update code (optional) *)
        } mark

    and global_dcl
      = GD_Mark of global_dcl mark
      | GD_Const of ty * var_bind * expr option (* constant declaration; if the expression is not *)
                                                (* given, then it must be defined on the command-line. *)
      | GD_Input of ty * var_bind * string option * expr option
                                                (* input variable declaration with optional *)
                                                (* description and optional default value *)
      | GD_Var of var_dcl                       (* global variable declaration *)
      | GD_Func of ty * var_bind * param list * fun_body (* function declaration *)

    and strand_dcl
      = SD_Mark of strand_dcl mark
      | SD_Strand of {                      (* strand declaration *)
            name : var_bind,                    (* strand name *)
            params : param list,                (* creation parameters *)
            state : state_var_dcl list,         (* state-variable decls *)
            stateInit : stmt option,            (* additional state-variable init code (optional) *)
            methods : method list               (* method definitions *)
          }

    and state_var_dcl
      = SVD_Mark of state_var_dcl mark
      | SVD_VarDcl of bool * var_dcl            (* first argument = true means an output variable *)

    and create
      = CR_Mark of create mark
      | CR_Collection of comprehension
      | CR_Array of expr option * comprehension

    and param
      = P_Mark of param mark
      | P_Param of ty * var_bind

    and ty
      = T_Mark of ty mark
      | T_Bool
      | T_Int
      | T_Real
      | T_String
      | T_Id of Atom.atom       (* named type; i.e., strand *)
      | T_Kernel of dim
      | T_Field of {diff : dim, dim : expr, shape : expr list}
      | T_Tensor of expr list
      | T_Image of {dim : expr, shape : expr list}
      | T_Seq of ty * expr
      | T_DynSeq of ty

    and fun_body                                (* function bodies can be expressions or blocks *)
      = FB_Expr of expr
      | FB_Stmt of stmt

    and var_dcl
      = VD_Mark of var_dcl mark
      | VD_Decl of ty * var_bind * expr option

    and method
      = M_Mark of method mark
      | M_Method of StrandUtil.method_name * stmt

    and stmt
      = S_Mark of stmt mark
      | S_Block of stmt list
      | S_IfThen of expr * stmt
      | S_IfThenElse of expr * stmt * stmt
(* NOTE: eventually, I'd like to get rid of the type, but that requires changes to
 * the way that we do queries so that we know the result type.  Perhaps something
 * like "Strand.sphere(radius)"
 *)
      | S_Foreach of ty * iterator * stmt
      | S_Print of expr list
      | S_New of var * expr list
      | S_Stabilize
      | S_Die
      | S_Continue
      | S_Return of expr
      | S_Decl of var_dcl
      | S_Assign of var_bind * var option * expr (* assignment operators; NONE => "=" *)

    and comprehension
      = COMP_Mark of comprehension mark
      | COMP_Comprehension of expr * iterator list
                                                (* iterators are in slow-to-fast order *)

    and iterator
      = I_Mark of iterator mark
      | I_Iterator of var_bind * expr           (* x 'in' e *)

    and expr
      = E_Mark of expr mark
      | E_Cond of expr * expr * expr            (* e1 'if' e2 'else' e3 *)
      | E_Range of expr * expr                  (* e1 '..' e2 *)
      | E_OrElse of expr * expr                 (* e1 '||' e2 *)
      | E_AndAlso of expr * expr                (* e1 '&&' e2 *)
      | E_BinOp of expr * var * expr            (* e1 <op> e2 *)
      | E_UnaryOp of var * expr                 (* <op> e *)
      | E_Apply of expr * expr list             (* field/function/reduction application *)
      | E_Subscript of expr * expr option list  (* sequence/tensor indexing; NONE for ':' *)
      | E_Select of expr * field                (* e '.' <field> *)
      | E_Real of expr                          (* int to real conversion *)
      | E_LoadSeq of expr                       (* initializer for dynamic sequences *)
      | E_LoadImage of expr                     (* initializer for images *)
      | E_Var of var
      | E_Kernel of var * dim                   (* kernel '#' dim *)
      | E_Lit of Literal.t
      | E_Id of expr                            (* identity matrix *)
      | E_Zero of expr list                     (* zero tensor *)
      | E_NaN of expr list                      (* NaN tensor *)
      | E_Sequence of expr list                 (* sequence construction *)
      | E_SeqComp of comprehension              (* sequence comprehension *)
      | E_Cons of expr list                     (* tensor construction *)

  end
