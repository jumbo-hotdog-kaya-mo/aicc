#![allow(nonstandard_style)]
// Generated from ail.g4 by ANTLR 4.8
use antlr_rust::tree::ParseTreeListener;
use super::ailparser::*;

pub trait ailListener<'input> : ParseTreeListener<'input,ailParserContextType>{
/**
 * Enter a parse tree produced by {@link ailParser#main}.
 * @param ctx the parse tree
 */
fn enter_main(&mut self, _ctx: &MainContext<'input>) { }
/**
 * Exit a parse tree produced by {@link ailParser#main}.
 * @param ctx the parse tree
 */
fn exit_main(&mut self, _ctx: &MainContext<'input>) { }
/**
 * Enter a parse tree produced by {@link ailParser#when}.
 * @param ctx the parse tree
 */
fn enter_when(&mut self, _ctx: &WhenContext<'input>) { }
/**
 * Exit a parse tree produced by {@link ailParser#when}.
 * @param ctx the parse tree
 */
fn exit_when(&mut self, _ctx: &WhenContext<'input>) { }
/**
 * Enter a parse tree produced by {@link ailParser#func_def}.
 * @param ctx the parse tree
 */
fn enter_func_def(&mut self, _ctx: &Func_defContext<'input>) { }
/**
 * Exit a parse tree produced by {@link ailParser#func_def}.
 * @param ctx the parse tree
 */
fn exit_func_def(&mut self, _ctx: &Func_defContext<'input>) { }
/**
 * Enter a parse tree produced by {@link ailParser#global_init}.
 * @param ctx the parse tree
 */
fn enter_global_init(&mut self, _ctx: &Global_initContext<'input>) { }
/**
 * Exit a parse tree produced by {@link ailParser#global_init}.
 * @param ctx the parse tree
 */
fn exit_global_init(&mut self, _ctx: &Global_initContext<'input>) { }
/**
 * Enter a parse tree produced by {@link ailParser#stmt}.
 * @param ctx the parse tree
 */
fn enter_stmt(&mut self, _ctx: &StmtContext<'input>) { }
/**
 * Exit a parse tree produced by {@link ailParser#stmt}.
 * @param ctx the parse tree
 */
fn exit_stmt(&mut self, _ctx: &StmtContext<'input>) { }
/**
 * Enter a parse tree produced by {@link ailParser#if_stmt}.
 * @param ctx the parse tree
 */
fn enter_if_stmt(&mut self, _ctx: &If_stmtContext<'input>) { }
/**
 * Exit a parse tree produced by {@link ailParser#if_stmt}.
 * @param ctx the parse tree
 */
fn exit_if_stmt(&mut self, _ctx: &If_stmtContext<'input>) { }
/**
 * Enter a parse tree produced by {@link ailParser#while_stmt}.
 * @param ctx the parse tree
 */
fn enter_while_stmt(&mut self, _ctx: &While_stmtContext<'input>) { }
/**
 * Exit a parse tree produced by {@link ailParser#while_stmt}.
 * @param ctx the parse tree
 */
fn exit_while_stmt(&mut self, _ctx: &While_stmtContext<'input>) { }
/**
 * Enter a parse tree produced by {@link ailParser#for_stmt}.
 * @param ctx the parse tree
 */
fn enter_for_stmt(&mut self, _ctx: &For_stmtContext<'input>) { }
/**
 * Exit a parse tree produced by {@link ailParser#for_stmt}.
 * @param ctx the parse tree
 */
fn exit_for_stmt(&mut self, _ctx: &For_stmtContext<'input>) { }
/**
 * Enter a parse tree produced by {@link ailParser#call_stmt}.
 * @param ctx the parse tree
 */
fn enter_call_stmt(&mut self, _ctx: &Call_stmtContext<'input>) { }
/**
 * Exit a parse tree produced by {@link ailParser#call_stmt}.
 * @param ctx the parse tree
 */
fn exit_call_stmt(&mut self, _ctx: &Call_stmtContext<'input>) { }
/**
 * Enter a parse tree produced by {@link ailParser#method_stmt}.
 * @param ctx the parse tree
 */
fn enter_method_stmt(&mut self, _ctx: &Method_stmtContext<'input>) { }
/**
 * Exit a parse tree produced by {@link ailParser#method_stmt}.
 * @param ctx the parse tree
 */
fn exit_method_stmt(&mut self, _ctx: &Method_stmtContext<'input>) { }
/**
 * Enter a parse tree produced by {@link ailParser#assign_stmt}.
 * @param ctx the parse tree
 */
fn enter_assign_stmt(&mut self, _ctx: &Assign_stmtContext<'input>) { }
/**
 * Exit a parse tree produced by {@link ailParser#assign_stmt}.
 * @param ctx the parse tree
 */
fn exit_assign_stmt(&mut self, _ctx: &Assign_stmtContext<'input>) { }
/**
 * Enter a parse tree produced by {@link ailParser#modify_stmt}.
 * @param ctx the parse tree
 */
fn enter_modify_stmt(&mut self, _ctx: &Modify_stmtContext<'input>) { }
/**
 * Exit a parse tree produced by {@link ailParser#modify_stmt}.
 * @param ctx the parse tree
 */
fn exit_modify_stmt(&mut self, _ctx: &Modify_stmtContext<'input>) { }
/**
 * Enter a parse tree produced by {@link ailParser#unary_op}.
 * @param ctx the parse tree
 */
fn enter_unary_op(&mut self, _ctx: &Unary_opContext<'input>) { }
/**
 * Exit a parse tree produced by {@link ailParser#unary_op}.
 * @param ctx the parse tree
 */
fn exit_unary_op(&mut self, _ctx: &Unary_opContext<'input>) { }
/**
 * Enter a parse tree produced by {@link ailParser#expr}.
 * @param ctx the parse tree
 */
fn enter_expr(&mut self, _ctx: &ExprContext<'input>) { }
/**
 * Exit a parse tree produced by {@link ailParser#expr}.
 * @param ctx the parse tree
 */
fn exit_expr(&mut self, _ctx: &ExprContext<'input>) { }
/**
 * Enter a parse tree produced by {@link ailParser#rvalue}.
 * @param ctx the parse tree
 */
fn enter_rvalue(&mut self, _ctx: &RvalueContext<'input>) { }
/**
 * Exit a parse tree produced by {@link ailParser#rvalue}.
 * @param ctx the parse tree
 */
fn exit_rvalue(&mut self, _ctx: &RvalueContext<'input>) { }
/**
 * Enter a parse tree produced by {@link ailParser#lvalue}.
 * @param ctx the parse tree
 */
fn enter_lvalue(&mut self, _ctx: &LvalueContext<'input>) { }
/**
 * Exit a parse tree produced by {@link ailParser#lvalue}.
 * @param ctx the parse tree
 */
fn exit_lvalue(&mut self, _ctx: &LvalueContext<'input>) { }
/**
 * Enter a parse tree produced by {@link ailParser#assign_expr}.
 * @param ctx the parse tree
 */
fn enter_assign_expr(&mut self, _ctx: &Assign_exprContext<'input>) { }
/**
 * Exit a parse tree produced by {@link ailParser#assign_expr}.
 * @param ctx the parse tree
 */
fn exit_assign_expr(&mut self, _ctx: &Assign_exprContext<'input>) { }
/**
 * Enter a parse tree produced by {@link ailParser#call_expr}.
 * @param ctx the parse tree
 */
fn enter_call_expr(&mut self, _ctx: &Call_exprContext<'input>) { }
/**
 * Exit a parse tree produced by {@link ailParser#call_expr}.
 * @param ctx the parse tree
 */
fn exit_call_expr(&mut self, _ctx: &Call_exprContext<'input>) { }
/**
 * Enter a parse tree produced by {@link ailParser#block_stmt}.
 * @param ctx the parse tree
 */
fn enter_block_stmt(&mut self, _ctx: &Block_stmtContext<'input>) { }
/**
 * Exit a parse tree produced by {@link ailParser#block_stmt}.
 * @param ctx the parse tree
 */
fn exit_block_stmt(&mut self, _ctx: &Block_stmtContext<'input>) { }
/**
 * Enter a parse tree produced by {@link ailParser#block_expr}.
 * @param ctx the parse tree
 */
fn enter_block_expr(&mut self, _ctx: &Block_exprContext<'input>) { }
/**
 * Exit a parse tree produced by {@link ailParser#block_expr}.
 * @param ctx the parse tree
 */
fn exit_block_expr(&mut self, _ctx: &Block_exprContext<'input>) { }
/**
 * Enter a parse tree produced by {@link ailParser#block}.
 * @param ctx the parse tree
 */
fn enter_block(&mut self, _ctx: &BlockContext<'input>) { }
/**
 * Exit a parse tree produced by {@link ailParser#block}.
 * @param ctx the parse tree
 */
fn exit_block(&mut self, _ctx: &BlockContext<'input>) { }
/**
 * Enter a parse tree produced by {@link ailParser#assignlist}.
 * @param ctx the parse tree
 */
fn enter_assignlist(&mut self, _ctx: &AssignlistContext<'input>) { }
/**
 * Exit a parse tree produced by {@link ailParser#assignlist}.
 * @param ctx the parse tree
 */
fn exit_assignlist(&mut self, _ctx: &AssignlistContext<'input>) { }
/**
 * Enter a parse tree produced by {@link ailParser#arglist}.
 * @param ctx the parse tree
 */
fn enter_arglist(&mut self, _ctx: &ArglistContext<'input>) { }
/**
 * Exit a parse tree produced by {@link ailParser#arglist}.
 * @param ctx the parse tree
 */
fn exit_arglist(&mut self, _ctx: &ArglistContext<'input>) { }
/**
 * Enter a parse tree produced by {@link ailParser#calllist}.
 * @param ctx the parse tree
 */
fn enter_calllist(&mut self, _ctx: &CalllistContext<'input>) { }
/**
 * Exit a parse tree produced by {@link ailParser#calllist}.
 * @param ctx the parse tree
 */
fn exit_calllist(&mut self, _ctx: &CalllistContext<'input>) { }

}

antlr_rust::coerce_from!{ 'input : ailListener<'input> }


