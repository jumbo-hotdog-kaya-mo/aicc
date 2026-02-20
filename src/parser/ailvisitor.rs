#![allow(nonstandard_style)]
// Generated from ail.g4 by ANTLR 4.8
use antlr_rust::tree::{ParseTreeVisitor,ParseTreeVisitorCompat};
use super::ailparser::*;

/**
 * This interface defines a complete generic visitor for a parse tree produced
 * by {@link ailParser}.
 */
pub trait ailVisitor<'input>: ParseTreeVisitor<'input,ailParserContextType>{
	/**
	 * Visit a parse tree produced by {@link ailParser#main}.
	 * @param ctx the parse tree
	 */
	fn visit_main(&mut self, ctx: &MainContext<'input>) { self.visit_children(ctx) }

	/**
	 * Visit a parse tree produced by {@link ailParser#when}.
	 * @param ctx the parse tree
	 */
	fn visit_when(&mut self, ctx: &WhenContext<'input>) { self.visit_children(ctx) }

	/**
	 * Visit a parse tree produced by {@link ailParser#func_def}.
	 * @param ctx the parse tree
	 */
	fn visit_func_def(&mut self, ctx: &Func_defContext<'input>) { self.visit_children(ctx) }

	/**
	 * Visit a parse tree produced by {@link ailParser#global_init}.
	 * @param ctx the parse tree
	 */
	fn visit_global_init(&mut self, ctx: &Global_initContext<'input>) { self.visit_children(ctx) }

	/**
	 * Visit a parse tree produced by {@link ailParser#stmt}.
	 * @param ctx the parse tree
	 */
	fn visit_stmt(&mut self, ctx: &StmtContext<'input>) { self.visit_children(ctx) }

	/**
	 * Visit a parse tree produced by {@link ailParser#if_stmt}.
	 * @param ctx the parse tree
	 */
	fn visit_if_stmt(&mut self, ctx: &If_stmtContext<'input>) { self.visit_children(ctx) }

	/**
	 * Visit a parse tree produced by {@link ailParser#while_stmt}.
	 * @param ctx the parse tree
	 */
	fn visit_while_stmt(&mut self, ctx: &While_stmtContext<'input>) { self.visit_children(ctx) }

	/**
	 * Visit a parse tree produced by {@link ailParser#for_stmt}.
	 * @param ctx the parse tree
	 */
	fn visit_for_stmt(&mut self, ctx: &For_stmtContext<'input>) { self.visit_children(ctx) }

	/**
	 * Visit a parse tree produced by {@link ailParser#call_stmt}.
	 * @param ctx the parse tree
	 */
	fn visit_call_stmt(&mut self, ctx: &Call_stmtContext<'input>) { self.visit_children(ctx) }

	/**
	 * Visit a parse tree produced by {@link ailParser#method_stmt}.
	 * @param ctx the parse tree
	 */
	fn visit_method_stmt(&mut self, ctx: &Method_stmtContext<'input>) { self.visit_children(ctx) }

	/**
	 * Visit a parse tree produced by {@link ailParser#assign_stmt}.
	 * @param ctx the parse tree
	 */
	fn visit_assign_stmt(&mut self, ctx: &Assign_stmtContext<'input>) { self.visit_children(ctx) }

	/**
	 * Visit a parse tree produced by {@link ailParser#modify_stmt}.
	 * @param ctx the parse tree
	 */
	fn visit_modify_stmt(&mut self, ctx: &Modify_stmtContext<'input>) { self.visit_children(ctx) }

	/**
	 * Visit a parse tree produced by {@link ailParser#unary_op}.
	 * @param ctx the parse tree
	 */
	fn visit_unary_op(&mut self, ctx: &Unary_opContext<'input>) { self.visit_children(ctx) }

	/**
	 * Visit a parse tree produced by {@link ailParser#expr}.
	 * @param ctx the parse tree
	 */
	fn visit_expr(&mut self, ctx: &ExprContext<'input>) { self.visit_children(ctx) }

	/**
	 * Visit a parse tree produced by {@link ailParser#rvalue}.
	 * @param ctx the parse tree
	 */
	fn visit_rvalue(&mut self, ctx: &RvalueContext<'input>) { self.visit_children(ctx) }

	/**
	 * Visit a parse tree produced by {@link ailParser#lvalue}.
	 * @param ctx the parse tree
	 */
	fn visit_lvalue(&mut self, ctx: &LvalueContext<'input>) { self.visit_children(ctx) }

	/**
	 * Visit a parse tree produced by {@link ailParser#assign_expr}.
	 * @param ctx the parse tree
	 */
	fn visit_assign_expr(&mut self, ctx: &Assign_exprContext<'input>) { self.visit_children(ctx) }

	/**
	 * Visit a parse tree produced by {@link ailParser#call_expr}.
	 * @param ctx the parse tree
	 */
	fn visit_call_expr(&mut self, ctx: &Call_exprContext<'input>) { self.visit_children(ctx) }

	/**
	 * Visit a parse tree produced by {@link ailParser#block_stmt}.
	 * @param ctx the parse tree
	 */
	fn visit_block_stmt(&mut self, ctx: &Block_stmtContext<'input>) { self.visit_children(ctx) }

	/**
	 * Visit a parse tree produced by {@link ailParser#block_expr}.
	 * @param ctx the parse tree
	 */
	fn visit_block_expr(&mut self, ctx: &Block_exprContext<'input>) { self.visit_children(ctx) }

	/**
	 * Visit a parse tree produced by {@link ailParser#block}.
	 * @param ctx the parse tree
	 */
	fn visit_block(&mut self, ctx: &BlockContext<'input>) { self.visit_children(ctx) }

	/**
	 * Visit a parse tree produced by {@link ailParser#assignlist}.
	 * @param ctx the parse tree
	 */
	fn visit_assignlist(&mut self, ctx: &AssignlistContext<'input>) { self.visit_children(ctx) }

	/**
	 * Visit a parse tree produced by {@link ailParser#arglist}.
	 * @param ctx the parse tree
	 */
	fn visit_arglist(&mut self, ctx: &ArglistContext<'input>) { self.visit_children(ctx) }

	/**
	 * Visit a parse tree produced by {@link ailParser#calllist}.
	 * @param ctx the parse tree
	 */
	fn visit_calllist(&mut self, ctx: &CalllistContext<'input>) { self.visit_children(ctx) }

}

pub trait ailVisitorCompat<'input>:ParseTreeVisitorCompat<'input, Node= ailParserContextType>{
	/**
	 * Visit a parse tree produced by {@link ailParser#main}.
	 * @param ctx the parse tree
	 */
		fn visit_main(&mut self, ctx: &MainContext<'input>) -> Self::Return {
			self.visit_children(ctx)
		}

	/**
	 * Visit a parse tree produced by {@link ailParser#when}.
	 * @param ctx the parse tree
	 */
		fn visit_when(&mut self, ctx: &WhenContext<'input>) -> Self::Return {
			self.visit_children(ctx)
		}

	/**
	 * Visit a parse tree produced by {@link ailParser#func_def}.
	 * @param ctx the parse tree
	 */
		fn visit_func_def(&mut self, ctx: &Func_defContext<'input>) -> Self::Return {
			self.visit_children(ctx)
		}

	/**
	 * Visit a parse tree produced by {@link ailParser#global_init}.
	 * @param ctx the parse tree
	 */
		fn visit_global_init(&mut self, ctx: &Global_initContext<'input>) -> Self::Return {
			self.visit_children(ctx)
		}

	/**
	 * Visit a parse tree produced by {@link ailParser#stmt}.
	 * @param ctx the parse tree
	 */
		fn visit_stmt(&mut self, ctx: &StmtContext<'input>) -> Self::Return {
			self.visit_children(ctx)
		}

	/**
	 * Visit a parse tree produced by {@link ailParser#if_stmt}.
	 * @param ctx the parse tree
	 */
		fn visit_if_stmt(&mut self, ctx: &If_stmtContext<'input>) -> Self::Return {
			self.visit_children(ctx)
		}

	/**
	 * Visit a parse tree produced by {@link ailParser#while_stmt}.
	 * @param ctx the parse tree
	 */
		fn visit_while_stmt(&mut self, ctx: &While_stmtContext<'input>) -> Self::Return {
			self.visit_children(ctx)
		}

	/**
	 * Visit a parse tree produced by {@link ailParser#for_stmt}.
	 * @param ctx the parse tree
	 */
		fn visit_for_stmt(&mut self, ctx: &For_stmtContext<'input>) -> Self::Return {
			self.visit_children(ctx)
		}

	/**
	 * Visit a parse tree produced by {@link ailParser#call_stmt}.
	 * @param ctx the parse tree
	 */
		fn visit_call_stmt(&mut self, ctx: &Call_stmtContext<'input>) -> Self::Return {
			self.visit_children(ctx)
		}

	/**
	 * Visit a parse tree produced by {@link ailParser#method_stmt}.
	 * @param ctx the parse tree
	 */
		fn visit_method_stmt(&mut self, ctx: &Method_stmtContext<'input>) -> Self::Return {
			self.visit_children(ctx)
		}

	/**
	 * Visit a parse tree produced by {@link ailParser#assign_stmt}.
	 * @param ctx the parse tree
	 */
		fn visit_assign_stmt(&mut self, ctx: &Assign_stmtContext<'input>) -> Self::Return {
			self.visit_children(ctx)
		}

	/**
	 * Visit a parse tree produced by {@link ailParser#modify_stmt}.
	 * @param ctx the parse tree
	 */
		fn visit_modify_stmt(&mut self, ctx: &Modify_stmtContext<'input>) -> Self::Return {
			self.visit_children(ctx)
		}

	/**
	 * Visit a parse tree produced by {@link ailParser#unary_op}.
	 * @param ctx the parse tree
	 */
		fn visit_unary_op(&mut self, ctx: &Unary_opContext<'input>) -> Self::Return {
			self.visit_children(ctx)
		}

	/**
	 * Visit a parse tree produced by {@link ailParser#expr}.
	 * @param ctx the parse tree
	 */
		fn visit_expr(&mut self, ctx: &ExprContext<'input>) -> Self::Return {
			self.visit_children(ctx)
		}

	/**
	 * Visit a parse tree produced by {@link ailParser#rvalue}.
	 * @param ctx the parse tree
	 */
		fn visit_rvalue(&mut self, ctx: &RvalueContext<'input>) -> Self::Return {
			self.visit_children(ctx)
		}

	/**
	 * Visit a parse tree produced by {@link ailParser#lvalue}.
	 * @param ctx the parse tree
	 */
		fn visit_lvalue(&mut self, ctx: &LvalueContext<'input>) -> Self::Return {
			self.visit_children(ctx)
		}

	/**
	 * Visit a parse tree produced by {@link ailParser#assign_expr}.
	 * @param ctx the parse tree
	 */
		fn visit_assign_expr(&mut self, ctx: &Assign_exprContext<'input>) -> Self::Return {
			self.visit_children(ctx)
		}

	/**
	 * Visit a parse tree produced by {@link ailParser#call_expr}.
	 * @param ctx the parse tree
	 */
		fn visit_call_expr(&mut self, ctx: &Call_exprContext<'input>) -> Self::Return {
			self.visit_children(ctx)
		}

	/**
	 * Visit a parse tree produced by {@link ailParser#block_stmt}.
	 * @param ctx the parse tree
	 */
		fn visit_block_stmt(&mut self, ctx: &Block_stmtContext<'input>) -> Self::Return {
			self.visit_children(ctx)
		}

	/**
	 * Visit a parse tree produced by {@link ailParser#block_expr}.
	 * @param ctx the parse tree
	 */
		fn visit_block_expr(&mut self, ctx: &Block_exprContext<'input>) -> Self::Return {
			self.visit_children(ctx)
		}

	/**
	 * Visit a parse tree produced by {@link ailParser#block}.
	 * @param ctx the parse tree
	 */
		fn visit_block(&mut self, ctx: &BlockContext<'input>) -> Self::Return {
			self.visit_children(ctx)
		}

	/**
	 * Visit a parse tree produced by {@link ailParser#assignlist}.
	 * @param ctx the parse tree
	 */
		fn visit_assignlist(&mut self, ctx: &AssignlistContext<'input>) -> Self::Return {
			self.visit_children(ctx)
		}

	/**
	 * Visit a parse tree produced by {@link ailParser#arglist}.
	 * @param ctx the parse tree
	 */
		fn visit_arglist(&mut self, ctx: &ArglistContext<'input>) -> Self::Return {
			self.visit_children(ctx)
		}

	/**
	 * Visit a parse tree produced by {@link ailParser#calllist}.
	 * @param ctx the parse tree
	 */
		fn visit_calllist(&mut self, ctx: &CalllistContext<'input>) -> Self::Return {
			self.visit_children(ctx)
		}

}

impl<'input,T> ailVisitor<'input> for T
where
	T: ailVisitorCompat<'input>
{
	fn visit_main(&mut self, ctx: &MainContext<'input>){
		let result = <Self as ailVisitorCompat>::visit_main(self, ctx);
        *<Self as ParseTreeVisitorCompat>::temp_result(self) = result;
	}

	fn visit_when(&mut self, ctx: &WhenContext<'input>){
		let result = <Self as ailVisitorCompat>::visit_when(self, ctx);
        *<Self as ParseTreeVisitorCompat>::temp_result(self) = result;
	}

	fn visit_func_def(&mut self, ctx: &Func_defContext<'input>){
		let result = <Self as ailVisitorCompat>::visit_func_def(self, ctx);
        *<Self as ParseTreeVisitorCompat>::temp_result(self) = result;
	}

	fn visit_global_init(&mut self, ctx: &Global_initContext<'input>){
		let result = <Self as ailVisitorCompat>::visit_global_init(self, ctx);
        *<Self as ParseTreeVisitorCompat>::temp_result(self) = result;
	}

	fn visit_stmt(&mut self, ctx: &StmtContext<'input>){
		let result = <Self as ailVisitorCompat>::visit_stmt(self, ctx);
        *<Self as ParseTreeVisitorCompat>::temp_result(self) = result;
	}

	fn visit_if_stmt(&mut self, ctx: &If_stmtContext<'input>){
		let result = <Self as ailVisitorCompat>::visit_if_stmt(self, ctx);
        *<Self as ParseTreeVisitorCompat>::temp_result(self) = result;
	}

	fn visit_while_stmt(&mut self, ctx: &While_stmtContext<'input>){
		let result = <Self as ailVisitorCompat>::visit_while_stmt(self, ctx);
        *<Self as ParseTreeVisitorCompat>::temp_result(self) = result;
	}

	fn visit_for_stmt(&mut self, ctx: &For_stmtContext<'input>){
		let result = <Self as ailVisitorCompat>::visit_for_stmt(self, ctx);
        *<Self as ParseTreeVisitorCompat>::temp_result(self) = result;
	}

	fn visit_call_stmt(&mut self, ctx: &Call_stmtContext<'input>){
		let result = <Self as ailVisitorCompat>::visit_call_stmt(self, ctx);
        *<Self as ParseTreeVisitorCompat>::temp_result(self) = result;
	}

	fn visit_method_stmt(&mut self, ctx: &Method_stmtContext<'input>){
		let result = <Self as ailVisitorCompat>::visit_method_stmt(self, ctx);
        *<Self as ParseTreeVisitorCompat>::temp_result(self) = result;
	}

	fn visit_assign_stmt(&mut self, ctx: &Assign_stmtContext<'input>){
		let result = <Self as ailVisitorCompat>::visit_assign_stmt(self, ctx);
        *<Self as ParseTreeVisitorCompat>::temp_result(self) = result;
	}

	fn visit_modify_stmt(&mut self, ctx: &Modify_stmtContext<'input>){
		let result = <Self as ailVisitorCompat>::visit_modify_stmt(self, ctx);
        *<Self as ParseTreeVisitorCompat>::temp_result(self) = result;
	}

	fn visit_unary_op(&mut self, ctx: &Unary_opContext<'input>){
		let result = <Self as ailVisitorCompat>::visit_unary_op(self, ctx);
        *<Self as ParseTreeVisitorCompat>::temp_result(self) = result;
	}

	fn visit_expr(&mut self, ctx: &ExprContext<'input>){
		let result = <Self as ailVisitorCompat>::visit_expr(self, ctx);
        *<Self as ParseTreeVisitorCompat>::temp_result(self) = result;
	}

	fn visit_rvalue(&mut self, ctx: &RvalueContext<'input>){
		let result = <Self as ailVisitorCompat>::visit_rvalue(self, ctx);
        *<Self as ParseTreeVisitorCompat>::temp_result(self) = result;
	}

	fn visit_lvalue(&mut self, ctx: &LvalueContext<'input>){
		let result = <Self as ailVisitorCompat>::visit_lvalue(self, ctx);
        *<Self as ParseTreeVisitorCompat>::temp_result(self) = result;
	}

	fn visit_assign_expr(&mut self, ctx: &Assign_exprContext<'input>){
		let result = <Self as ailVisitorCompat>::visit_assign_expr(self, ctx);
        *<Self as ParseTreeVisitorCompat>::temp_result(self) = result;
	}

	fn visit_call_expr(&mut self, ctx: &Call_exprContext<'input>){
		let result = <Self as ailVisitorCompat>::visit_call_expr(self, ctx);
        *<Self as ParseTreeVisitorCompat>::temp_result(self) = result;
	}

	fn visit_block_stmt(&mut self, ctx: &Block_stmtContext<'input>){
		let result = <Self as ailVisitorCompat>::visit_block_stmt(self, ctx);
        *<Self as ParseTreeVisitorCompat>::temp_result(self) = result;
	}

	fn visit_block_expr(&mut self, ctx: &Block_exprContext<'input>){
		let result = <Self as ailVisitorCompat>::visit_block_expr(self, ctx);
        *<Self as ParseTreeVisitorCompat>::temp_result(self) = result;
	}

	fn visit_block(&mut self, ctx: &BlockContext<'input>){
		let result = <Self as ailVisitorCompat>::visit_block(self, ctx);
        *<Self as ParseTreeVisitorCompat>::temp_result(self) = result;
	}

	fn visit_assignlist(&mut self, ctx: &AssignlistContext<'input>){
		let result = <Self as ailVisitorCompat>::visit_assignlist(self, ctx);
        *<Self as ParseTreeVisitorCompat>::temp_result(self) = result;
	}

	fn visit_arglist(&mut self, ctx: &ArglistContext<'input>){
		let result = <Self as ailVisitorCompat>::visit_arglist(self, ctx);
        *<Self as ParseTreeVisitorCompat>::temp_result(self) = result;
	}

	fn visit_calllist(&mut self, ctx: &CalllistContext<'input>){
		let result = <Self as ailVisitorCompat>::visit_calllist(self, ctx);
        *<Self as ParseTreeVisitorCompat>::temp_result(self) = result;
	}

}