// Generated from ail.g4 by ANTLR 4.8
#![allow(dead_code)]
#![allow(non_snake_case)]
#![allow(non_upper_case_globals)]
#![allow(nonstandard_style)]
#![allow(unused_imports)]
#![allow(unused_mut)]
#![allow(unused_braces)]
use antlr_rust::PredictionContextCache;
use antlr_rust::parser::{Parser, BaseParser, ParserRecog, ParserNodeType};
use antlr_rust::token_stream::TokenStream;
use antlr_rust::TokenSource;
use antlr_rust::parser_atn_simulator::ParserATNSimulator;
use antlr_rust::errors::*;
use antlr_rust::rule_context::{BaseRuleContext, CustomRuleContext, RuleContext};
use antlr_rust::recognizer::{Recognizer,Actions};
use antlr_rust::atn_deserializer::ATNDeserializer;
use antlr_rust::dfa::DFA;
use antlr_rust::atn::{ATN, INVALID_ALT};
use antlr_rust::error_strategy::{ErrorStrategy, DefaultErrorStrategy};
use antlr_rust::parser_rule_context::{BaseParserRuleContext, ParserRuleContext,cast,cast_mut};
use antlr_rust::tree::*;
use antlr_rust::token::{TOKEN_EOF,OwningToken,Token};
use antlr_rust::int_stream::EOF;
use antlr_rust::vocabulary::{Vocabulary,VocabularyImpl};
use antlr_rust::token_factory::{CommonTokenFactory,TokenFactory, TokenAware};
use super::aillistener::*;
use super::ailvisitor::*;

use antlr_rust::lazy_static;
use antlr_rust::{TidAble,TidExt};

use std::marker::PhantomData;
use std::sync::Arc;
use std::rc::Rc;
use std::convert::TryFrom;
use std::cell::RefCell;
use std::ops::{DerefMut, Deref};
use std::borrow::{Borrow,BorrowMut};
use std::any::{Any,TypeId};

		pub const T__0:isize=1; 
		pub const T__1:isize=2; 
		pub const T__2:isize=3; 
		pub const T__3:isize=4; 
		pub const T__4:isize=5; 
		pub const T__5:isize=6; 
		pub const T__6:isize=7; 
		pub const T__7:isize=8; 
		pub const T__8:isize=9; 
		pub const WS:isize=10; 
		pub const LNCOMMENT:isize=11; 
		pub const BLKCOMMENT:isize=12; 
		pub const STRING:isize=13; 
		pub const COLOR:isize=14; 
		pub const NUMBER:isize=15; 
		pub const BOOL:isize=16; 
		pub const BY:isize=17; 
		pub const ELSE:isize=18; 
		pub const FOR:isize=19; 
		pub const FROM:isize=20; 
		pub const FUNC:isize=21; 
		pub const GLOBAL:isize=22; 
		pub const IF:isize=23; 
		pub const IN:isize=24; 
		pub const LET:isize=25; 
		pub const PROC:isize=26; 
		pub const TO:isize=27; 
		pub const WHEN:isize=28; 
		pub const WHILE:isize=29; 
		pub const AND:isize=30; 
		pub const CARET:isize=31; 
		pub const COLON:isize=32; 
		pub const COMMA:isize=33; 
		pub const DOLLAR:isize=34; 
		pub const DOT:isize=35; 
		pub const EQUAL:isize=36; 
		pub const EXCLAMATION:isize=37; 
		pub const HASH:isize=38; 
		pub const LBRACE:isize=39; 
		pub const LBRACKET:isize=40; 
		pub const LPAREN:isize=41; 
		pub const MINUS:isize=42; 
		pub const PERCENT:isize=43; 
		pub const PIPE:isize=44; 
		pub const PLUS:isize=45; 
		pub const RBRACE:isize=46; 
		pub const RBRACKET:isize=47; 
		pub const RPAREN:isize=48; 
		pub const SEMICOLON:isize=49; 
		pub const SLASH:isize=50; 
		pub const STAR:isize=51; 
		pub const TILDE:isize=52; 
		pub const IDENT:isize=53; 
		pub const INVALID:isize=54;
	pub const RULE_main:usize = 0; 
	pub const RULE_when:usize = 1; 
	pub const RULE_func_def:usize = 2; 
	pub const RULE_global_init:usize = 3; 
	pub const RULE_stmt:usize = 4; 
	pub const RULE_if_stmt:usize = 5; 
	pub const RULE_while_stmt:usize = 6; 
	pub const RULE_for_stmt:usize = 7; 
	pub const RULE_call_stmt:usize = 8; 
	pub const RULE_assign_stmt:usize = 9; 
	pub const RULE_modify_stmt:usize = 10; 
	pub const RULE_unary_op:usize = 11; 
	pub const RULE_binary_op:usize = 12; 
	pub const RULE_expr:usize = 13; 
	pub const RULE_rvalue:usize = 14; 
	pub const RULE_lvalue:usize = 15; 
	pub const RULE_assign_expr:usize = 16; 
	pub const RULE_call_expr:usize = 17; 
	pub const RULE_block_stmt:usize = 18; 
	pub const RULE_block_expr:usize = 19; 
	pub const RULE_block:usize = 20; 
	pub const RULE_assignlist:usize = 21; 
	pub const RULE_arglist:usize = 22; 
	pub const RULE_calllist:usize = 23;
	pub const ruleNames: [&'static str; 24] =  [
		"main", "when", "func_def", "global_init", "stmt", "if_stmt", "while_stmt", 
		"for_stmt", "call_stmt", "assign_stmt", "modify_stmt", "unary_op", "binary_op", 
		"expr", "rvalue", "lvalue", "assign_expr", "call_expr", "block_stmt", 
		"block_expr", "block", "assignlist", "arglist", "calllist"
	];


	pub const _LITERAL_NAMES: [Option<&'static str>;53] = [
		None, Some("'=='"), Some("'!='"), Some("'<'"), Some("'>'"), Some("'<='"), 
		Some("'>='"), Some("'&&'"), Some("'||'"), Some("'#('"), None, None, None, 
		None, None, None, None, Some("'by'"), Some("'else'"), Some("'for'"), Some("'from'"), 
		Some("'func'"), Some("'global'"), Some("'if'"), Some("'in'"), Some("'let'"), 
		Some("'proc'"), Some("'to'"), Some("'when'"), Some("'while'"), Some("'&'"), 
		Some("'^'"), Some("':'"), Some("','"), Some("'$'"), Some("'.'"), Some("'='"), 
		Some("'!'"), Some("'#'"), Some("'{'"), Some("'['"), Some("'('"), Some("'-'"), 
		Some("'%'"), Some("'|'"), Some("'+'"), Some("'}'"), Some("']'"), Some("')'"), 
		Some("';'"), Some("'/'"), Some("'*'"), Some("'~'")
	];
	pub const _SYMBOLIC_NAMES: [Option<&'static str>;55]  = [
		None, None, None, None, None, None, None, None, None, None, Some("WS"), 
		Some("LNCOMMENT"), Some("BLKCOMMENT"), Some("STRING"), Some("COLOR"), 
		Some("NUMBER"), Some("BOOL"), Some("BY"), Some("ELSE"), Some("FOR"), Some("FROM"), 
		Some("FUNC"), Some("GLOBAL"), Some("IF"), Some("IN"), Some("LET"), Some("PROC"), 
		Some("TO"), Some("WHEN"), Some("WHILE"), Some("AND"), Some("CARET"), Some("COLON"), 
		Some("COMMA"), Some("DOLLAR"), Some("DOT"), Some("EQUAL"), Some("EXCLAMATION"), 
		Some("HASH"), Some("LBRACE"), Some("LBRACKET"), Some("LPAREN"), Some("MINUS"), 
		Some("PERCENT"), Some("PIPE"), Some("PLUS"), Some("RBRACE"), Some("RBRACKET"), 
		Some("RPAREN"), Some("SEMICOLON"), Some("SLASH"), Some("STAR"), Some("TILDE"), 
		Some("IDENT"), Some("INVALID")
	];
	lazy_static!{
	    static ref _shared_context_cache: Arc<PredictionContextCache> = Arc::new(PredictionContextCache::new());
		static ref VOCABULARY: Box<dyn Vocabulary> = Box::new(VocabularyImpl::new(_LITERAL_NAMES.iter(), _SYMBOLIC_NAMES.iter(), None));
	}


type BaseParserType<'input, I> =
	BaseParser<'input,ailParserExt<'input>, I, ailParserContextType , dyn ailListener<'input> + 'input >;

type TokenType<'input> = <LocalTokenFactory<'input> as TokenFactory<'input>>::Tok;
pub type LocalTokenFactory<'input> = CommonTokenFactory;

pub type ailTreeWalker<'input,'a> =
	ParseTreeWalker<'input, 'a, ailParserContextType , dyn ailListener<'input> + 'a>;

/// Parser for ail grammar
pub struct ailParser<'input,I,H>
where
    I: TokenStream<'input, TF = LocalTokenFactory<'input> > + TidAble<'input>,
    H: ErrorStrategy<'input,BaseParserType<'input,I>>
{
	base:BaseParserType<'input,I>,
	interpreter:Arc<ParserATNSimulator>,
	_shared_context_cache: Box<PredictionContextCache>,
    pub err_handler: H,
}

impl<'input, I, H> ailParser<'input, I, H>
where
    I: TokenStream<'input, TF = LocalTokenFactory<'input> > + TidAble<'input>,
    H: ErrorStrategy<'input,BaseParserType<'input,I>>
{
	pub fn get_serialized_atn() -> &'static str { _serializedATN }

    pub fn set_error_strategy(&mut self, strategy: H) {
        self.err_handler = strategy
    }

    pub fn with_strategy(input: I, strategy: H) -> Self {
		antlr_rust::recognizer::check_version("0","3");
		let interpreter = Arc::new(ParserATNSimulator::new(
			_ATN.clone(),
			_decision_to_DFA.clone(),
			_shared_context_cache.clone(),
		));
		Self {
			base: BaseParser::new_base_parser(
				input,
				Arc::clone(&interpreter),
				ailParserExt{
					_pd: Default::default(),
				}
			),
			interpreter,
            _shared_context_cache: Box::new(PredictionContextCache::new()),
            err_handler: strategy,
        }
    }

}

type DynStrategy<'input,I> = Box<dyn ErrorStrategy<'input,BaseParserType<'input,I>> + 'input>;

impl<'input, I> ailParser<'input, I, DynStrategy<'input,I>>
where
    I: TokenStream<'input, TF = LocalTokenFactory<'input> > + TidAble<'input>,
{
    pub fn with_dyn_strategy(input: I) -> Self{
    	Self::with_strategy(input,Box::new(DefaultErrorStrategy::new()))
    }
}

impl<'input, I> ailParser<'input, I, DefaultErrorStrategy<'input,ailParserContextType>>
where
    I: TokenStream<'input, TF = LocalTokenFactory<'input> > + TidAble<'input>,
{
    pub fn new(input: I) -> Self{
    	Self::with_strategy(input,DefaultErrorStrategy::new())
    }
}

/// Trait for monomorphized trait object that corresponds to the nodes of parse tree generated for ailParser
pub trait ailParserContext<'input>:
	for<'x> Listenable<dyn ailListener<'input> + 'x > + 
	for<'x> Visitable<dyn ailVisitor<'input> + 'x > + 
	ParserRuleContext<'input, TF=LocalTokenFactory<'input>, Ctx=ailParserContextType>
{}

antlr_rust::coerce_from!{ 'input : ailParserContext<'input> }

impl<'input, 'x, T> VisitableDyn<T> for dyn ailParserContext<'input> + 'input
where
    T: ailVisitor<'input> + 'x,
{
    fn accept_dyn(&self, visitor: &mut T) {
        self.accept(visitor as &mut (dyn ailVisitor<'input> + 'x))
    }
}

impl<'input> ailParserContext<'input> for TerminalNode<'input,ailParserContextType> {}
impl<'input> ailParserContext<'input> for ErrorNode<'input,ailParserContextType> {}

antlr_rust::tid! { impl<'input> TidAble<'input> for dyn ailParserContext<'input> + 'input }

antlr_rust::tid! { impl<'input> TidAble<'input> for dyn ailListener<'input> + 'input }

pub struct ailParserContextType;
antlr_rust::tid!{ailParserContextType}

impl<'input> ParserNodeType<'input> for ailParserContextType{
	type TF = LocalTokenFactory<'input>;
	type Type = dyn ailParserContext<'input> + 'input;
}

impl<'input, I, H> Deref for ailParser<'input, I, H>
where
    I: TokenStream<'input, TF = LocalTokenFactory<'input> > + TidAble<'input>,
    H: ErrorStrategy<'input,BaseParserType<'input,I>>
{
    type Target = BaseParserType<'input,I>;

    fn deref(&self) -> &Self::Target {
        &self.base
    }
}

impl<'input, I, H> DerefMut for ailParser<'input, I, H>
where
    I: TokenStream<'input, TF = LocalTokenFactory<'input> > + TidAble<'input>,
    H: ErrorStrategy<'input,BaseParserType<'input,I>>
{
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.base
    }
}

pub struct ailParserExt<'input>{
	_pd: PhantomData<&'input str>,
}

impl<'input> ailParserExt<'input>{
}
antlr_rust::tid! { ailParserExt<'a> }

impl<'input> TokenAware<'input> for ailParserExt<'input>{
	type TF = LocalTokenFactory<'input>;
}

impl<'input,I: TokenStream<'input, TF = LocalTokenFactory<'input> > + TidAble<'input>> ParserRecog<'input, BaseParserType<'input,I>> for ailParserExt<'input>{}

impl<'input,I: TokenStream<'input, TF = LocalTokenFactory<'input> > + TidAble<'input>> Actions<'input, BaseParserType<'input,I>> for ailParserExt<'input>{
	fn get_grammar_file_name(&self) -> & str{ "ail.g4"}

   	fn get_rule_names(&self) -> &[& str] {&ruleNames}

   	fn get_vocabulary(&self) -> &dyn Vocabulary { &**VOCABULARY }
	fn sempred(_localctx: Option<&(dyn ailParserContext<'input> + 'input)>, rule_index: isize, pred_index: isize,
			   recog:&mut BaseParserType<'input,I>
	)->bool{
		match rule_index {
					13 => ailParser::<'input,I,_>::expr_sempred(_localctx.and_then(|x|x.downcast_ref()), pred_index, recog),
			_ => true
		}
	}
}

impl<'input, I> ailParser<'input, I, DefaultErrorStrategy<'input,ailParserContextType>>
where
    I: TokenStream<'input, TF = LocalTokenFactory<'input> > + TidAble<'input>,
{
	fn expr_sempred(_localctx: Option<&ExprContext<'input>>, pred_index:isize,
						recog:&mut <Self as Deref>::Target
		) -> bool {
		match pred_index {
				0=>{
					recog.precpred(None, 12)
				}
			_ => true
		}
	}
}
//------------------- main ----------------
pub type MainContextAll<'input> = MainContext<'input>;


pub type MainContext<'input> = BaseParserRuleContext<'input,MainContextExt<'input>>;

#[derive(Clone)]
pub struct MainContextExt<'input>{
ph:PhantomData<&'input str>
}

impl<'input> ailParserContext<'input> for MainContext<'input>{}

impl<'input,'a> Listenable<dyn ailListener<'input> + 'a> for MainContext<'input>{
		fn enter(&self,listener: &mut (dyn ailListener<'input> + 'a)) {
			listener.enter_every_rule(self);
			listener.enter_main(self);
		}
		fn exit(&self,listener: &mut (dyn ailListener<'input> + 'a)) {
			listener.exit_main(self);
			listener.exit_every_rule(self);
		}
}

impl<'input,'a> Visitable<dyn ailVisitor<'input> + 'a> for MainContext<'input>{
	fn accept(&self,visitor: &mut (dyn ailVisitor<'input> + 'a)) {
		visitor.visit_main(self);
	}
}

impl<'input> CustomRuleContext<'input> for MainContextExt<'input>{
	type TF = LocalTokenFactory<'input>;
	type Ctx = ailParserContextType;
	fn get_rule_index(&self) -> usize { RULE_main }
	//fn type_rule_index() -> usize where Self: Sized { RULE_main }
}
antlr_rust::tid!{MainContextExt<'a>}

impl<'input> MainContextExt<'input>{
	fn new(parent: Option<Rc<dyn ailParserContext<'input> + 'input > >, invoking_state: isize) -> Rc<MainContextAll<'input>> {
		Rc::new(
			BaseParserRuleContext::new_parser_ctx(parent, invoking_state,MainContextExt{
				ph:PhantomData
			}),
		)
	}
}

pub trait MainContextAttrs<'input>: ailParserContext<'input> + BorrowMut<MainContextExt<'input>>{

/// Retrieves first TerminalNode corresponding to token EOF
/// Returns `None` if there is no child corresponding to token EOF
fn EOF(&self) -> Option<Rc<TerminalNode<'input,ailParserContextType>>> where Self:Sized{
	self.get_token(EOF, 0)
}
fn when_all(&self) ->  Vec<Rc<WhenContextAll<'input>>> where Self:Sized{
	self.children_of_type()
}
fn when(&self, i: usize) -> Option<Rc<WhenContextAll<'input>>> where Self:Sized{
	self.child_of_type(i)
}
fn func_def_all(&self) ->  Vec<Rc<Func_defContextAll<'input>>> where Self:Sized{
	self.children_of_type()
}
fn func_def(&self, i: usize) -> Option<Rc<Func_defContextAll<'input>>> where Self:Sized{
	self.child_of_type(i)
}
fn global_init_all(&self) ->  Vec<Rc<Global_initContextAll<'input>>> where Self:Sized{
	self.children_of_type()
}
fn global_init(&self, i: usize) -> Option<Rc<Global_initContextAll<'input>>> where Self:Sized{
	self.child_of_type(i)
}

}

impl<'input> MainContextAttrs<'input> for MainContext<'input>{}

impl<'input, I, H> ailParser<'input, I, H>
where
    I: TokenStream<'input, TF = LocalTokenFactory<'input> > + TidAble<'input>,
    H: ErrorStrategy<'input,BaseParserType<'input,I>>
{
	pub fn main(&mut self,)
	-> Result<Rc<MainContextAll<'input>>,ANTLRError> {
		let mut recog = self;
		let _parentctx = recog.ctx.take();
		let mut _localctx = MainContextExt::new(_parentctx.clone(), recog.base.get_state());
        recog.base.enter_rule(_localctx.clone(), 0, RULE_main);
        let mut _localctx: Rc<MainContextAll> = _localctx;
		let mut _la: isize = -1;
		let result: Result<(), ANTLRError> = (|| {

			//recog.base.enter_outer_alt(_localctx.clone(), 1);
			recog.base.enter_outer_alt(None, 1);
			{
			recog.base.set_state(53);
			recog.err_handler.sync(&mut recog.base)?;
			_la = recog.base.input.la(1);
			while (((_la) & !0x3f) == 0 && ((1usize << _la) & ((1usize << FUNC) | (1usize << GLOBAL) | (1usize << PROC) | (1usize << WHEN))) != 0) {
				{
				recog.base.set_state(51);
				recog.err_handler.sync(&mut recog.base)?;
				match recog.base.input.la(1) {
				 WHEN 
					=> {
						{
						/*InvokeRule when*/
						recog.base.set_state(48);
						recog.when()?;

						}
					}

				 FUNC | PROC 
					=> {
						{
						/*InvokeRule func_def*/
						recog.base.set_state(49);
						recog.func_def()?;

						}
					}

				 GLOBAL 
					=> {
						{
						/*InvokeRule global_init*/
						recog.base.set_state(50);
						recog.global_init()?;

						}
					}

					_ => Err(ANTLRError::NoAltError(NoViableAltError::new(&mut recog.base)))?
				}
				}
				recog.base.set_state(55);
				recog.err_handler.sync(&mut recog.base)?;
				_la = recog.base.input.la(1);
			}
			recog.base.set_state(56);
			recog.base.match_token(EOF,&mut recog.err_handler)?;

			}
			Ok(())
		})();
		match result {
		Ok(_)=>{},
        Err(e @ ANTLRError::FallThrough(_)) => return Err(e),
		Err(ref re) => {
				//_localctx.exception = re;
				recog.err_handler.report_error(&mut recog.base, re);
				recog.err_handler.recover(&mut recog.base, re)?;
			}
		}
		recog.base.exit_rule();

		Ok(_localctx)
	}
}
//------------------- when ----------------
pub type WhenContextAll<'input> = WhenContext<'input>;


pub type WhenContext<'input> = BaseParserRuleContext<'input,WhenContextExt<'input>>;

#[derive(Clone)]
pub struct WhenContextExt<'input>{
ph:PhantomData<&'input str>
}

impl<'input> ailParserContext<'input> for WhenContext<'input>{}

impl<'input,'a> Listenable<dyn ailListener<'input> + 'a> for WhenContext<'input>{
		fn enter(&self,listener: &mut (dyn ailListener<'input> + 'a)) {
			listener.enter_every_rule(self);
			listener.enter_when(self);
		}
		fn exit(&self,listener: &mut (dyn ailListener<'input> + 'a)) {
			listener.exit_when(self);
			listener.exit_every_rule(self);
		}
}

impl<'input,'a> Visitable<dyn ailVisitor<'input> + 'a> for WhenContext<'input>{
	fn accept(&self,visitor: &mut (dyn ailVisitor<'input> + 'a)) {
		visitor.visit_when(self);
	}
}

impl<'input> CustomRuleContext<'input> for WhenContextExt<'input>{
	type TF = LocalTokenFactory<'input>;
	type Ctx = ailParserContextType;
	fn get_rule_index(&self) -> usize { RULE_when }
	//fn type_rule_index() -> usize where Self: Sized { RULE_when }
}
antlr_rust::tid!{WhenContextExt<'a>}

impl<'input> WhenContextExt<'input>{
	fn new(parent: Option<Rc<dyn ailParserContext<'input> + 'input > >, invoking_state: isize) -> Rc<WhenContextAll<'input>> {
		Rc::new(
			BaseParserRuleContext::new_parser_ctx(parent, invoking_state,WhenContextExt{
				ph:PhantomData
			}),
		)
	}
}

pub trait WhenContextAttrs<'input>: ailParserContext<'input> + BorrowMut<WhenContextExt<'input>>{

/// Retrieves first TerminalNode corresponding to token WHEN
/// Returns `None` if there is no child corresponding to token WHEN
fn WHEN(&self) -> Option<Rc<TerminalNode<'input,ailParserContextType>>> where Self:Sized{
	self.get_token(WHEN, 0)
}
/// Retrieves first TerminalNode corresponding to token DOLLAR
/// Returns `None` if there is no child corresponding to token DOLLAR
fn DOLLAR(&self) -> Option<Rc<TerminalNode<'input,ailParserContextType>>> where Self:Sized{
	self.get_token(DOLLAR, 0)
}
/// Retrieves all `TerminalNode`s corresponding to token IDENT in current rule
fn IDENT_all(&self) -> Vec<Rc<TerminalNode<'input,ailParserContextType>>>  where Self:Sized{
	self.children_of_type()
}
/// Retrieves 'i's TerminalNode corresponding to token IDENT, starting from 0.
/// Returns `None` if number of children corresponding to token IDENT is less or equal than `i`.
fn IDENT(&self, i: usize) -> Option<Rc<TerminalNode<'input,ailParserContextType>>> where Self:Sized{
	self.get_token(IDENT, i)
}
/// Retrieves first TerminalNode corresponding to token DOT
/// Returns `None` if there is no child corresponding to token DOT
fn DOT(&self) -> Option<Rc<TerminalNode<'input,ailParserContextType>>> where Self:Sized{
	self.get_token(DOT, 0)
}
fn block_stmt(&self) -> Option<Rc<Block_stmtContextAll<'input>>> where Self:Sized{
	self.child_of_type(0)
}
/// Retrieves first TerminalNode corresponding to token COLON
/// Returns `None` if there is no child corresponding to token COLON
fn COLON(&self) -> Option<Rc<TerminalNode<'input,ailParserContextType>>> where Self:Sized{
	self.get_token(COLON, 0)
}

}

impl<'input> WhenContextAttrs<'input> for WhenContext<'input>{}

impl<'input, I, H> ailParser<'input, I, H>
where
    I: TokenStream<'input, TF = LocalTokenFactory<'input> > + TidAble<'input>,
    H: ErrorStrategy<'input,BaseParserType<'input,I>>
{
	pub fn when(&mut self,)
	-> Result<Rc<WhenContextAll<'input>>,ANTLRError> {
		let mut recog = self;
		let _parentctx = recog.ctx.take();
		let mut _localctx = WhenContextExt::new(_parentctx.clone(), recog.base.get_state());
        recog.base.enter_rule(_localctx.clone(), 2, RULE_when);
        let mut _localctx: Rc<WhenContextAll> = _localctx;
		let mut _la: isize = -1;
		let result: Result<(), ANTLRError> = (|| {

			//recog.base.enter_outer_alt(_localctx.clone(), 1);
			recog.base.enter_outer_alt(None, 1);
			{
			recog.base.set_state(58);
			recog.base.match_token(WHEN,&mut recog.err_handler)?;

			recog.base.set_state(59);
			recog.base.match_token(DOLLAR,&mut recog.err_handler)?;

			recog.base.set_state(60);
			recog.base.match_token(IDENT,&mut recog.err_handler)?;

			recog.base.set_state(63);
			recog.err_handler.sync(&mut recog.base)?;
			_la = recog.base.input.la(1);
			if _la==COLON {
				{
				recog.base.set_state(61);
				recog.base.match_token(COLON,&mut recog.err_handler)?;

				recog.base.set_state(62);
				recog.base.match_token(IDENT,&mut recog.err_handler)?;

				}
			}

			recog.base.set_state(65);
			recog.base.match_token(DOT,&mut recog.err_handler)?;

			recog.base.set_state(66);
			recog.base.match_token(IDENT,&mut recog.err_handler)?;

			/*InvokeRule block_stmt*/
			recog.base.set_state(67);
			recog.block_stmt()?;

			}
			Ok(())
		})();
		match result {
		Ok(_)=>{},
        Err(e @ ANTLRError::FallThrough(_)) => return Err(e),
		Err(ref re) => {
				//_localctx.exception = re;
				recog.err_handler.report_error(&mut recog.base, re);
				recog.err_handler.recover(&mut recog.base, re)?;
			}
		}
		recog.base.exit_rule();

		Ok(_localctx)
	}
}
//------------------- func_def ----------------
pub type Func_defContextAll<'input> = Func_defContext<'input>;


pub type Func_defContext<'input> = BaseParserRuleContext<'input,Func_defContextExt<'input>>;

#[derive(Clone)]
pub struct Func_defContextExt<'input>{
ph:PhantomData<&'input str>
}

impl<'input> ailParserContext<'input> for Func_defContext<'input>{}

impl<'input,'a> Listenable<dyn ailListener<'input> + 'a> for Func_defContext<'input>{
		fn enter(&self,listener: &mut (dyn ailListener<'input> + 'a)) {
			listener.enter_every_rule(self);
			listener.enter_func_def(self);
		}
		fn exit(&self,listener: &mut (dyn ailListener<'input> + 'a)) {
			listener.exit_func_def(self);
			listener.exit_every_rule(self);
		}
}

impl<'input,'a> Visitable<dyn ailVisitor<'input> + 'a> for Func_defContext<'input>{
	fn accept(&self,visitor: &mut (dyn ailVisitor<'input> + 'a)) {
		visitor.visit_func_def(self);
	}
}

impl<'input> CustomRuleContext<'input> for Func_defContextExt<'input>{
	type TF = LocalTokenFactory<'input>;
	type Ctx = ailParserContextType;
	fn get_rule_index(&self) -> usize { RULE_func_def }
	//fn type_rule_index() -> usize where Self: Sized { RULE_func_def }
}
antlr_rust::tid!{Func_defContextExt<'a>}

impl<'input> Func_defContextExt<'input>{
	fn new(parent: Option<Rc<dyn ailParserContext<'input> + 'input > >, invoking_state: isize) -> Rc<Func_defContextAll<'input>> {
		Rc::new(
			BaseParserRuleContext::new_parser_ctx(parent, invoking_state,Func_defContextExt{
				ph:PhantomData
			}),
		)
	}
}

pub trait Func_defContextAttrs<'input>: ailParserContext<'input> + BorrowMut<Func_defContextExt<'input>>{

/// Retrieves first TerminalNode corresponding to token FUNC
/// Returns `None` if there is no child corresponding to token FUNC
fn FUNC(&self) -> Option<Rc<TerminalNode<'input,ailParserContextType>>> where Self:Sized{
	self.get_token(FUNC, 0)
}
/// Retrieves first TerminalNode corresponding to token IDENT
/// Returns `None` if there is no child corresponding to token IDENT
fn IDENT(&self) -> Option<Rc<TerminalNode<'input,ailParserContextType>>> where Self:Sized{
	self.get_token(IDENT, 0)
}
fn arglist(&self) -> Option<Rc<ArglistContextAll<'input>>> where Self:Sized{
	self.child_of_type(0)
}
fn block_expr(&self) -> Option<Rc<Block_exprContextAll<'input>>> where Self:Sized{
	self.child_of_type(0)
}
/// Retrieves first TerminalNode corresponding to token PROC
/// Returns `None` if there is no child corresponding to token PROC
fn PROC(&self) -> Option<Rc<TerminalNode<'input,ailParserContextType>>> where Self:Sized{
	self.get_token(PROC, 0)
}
fn block_stmt(&self) -> Option<Rc<Block_stmtContextAll<'input>>> where Self:Sized{
	self.child_of_type(0)
}

}

impl<'input> Func_defContextAttrs<'input> for Func_defContext<'input>{}

impl<'input, I, H> ailParser<'input, I, H>
where
    I: TokenStream<'input, TF = LocalTokenFactory<'input> > + TidAble<'input>,
    H: ErrorStrategy<'input,BaseParserType<'input,I>>
{
	pub fn func_def(&mut self,)
	-> Result<Rc<Func_defContextAll<'input>>,ANTLRError> {
		let mut recog = self;
		let _parentctx = recog.ctx.take();
		let mut _localctx = Func_defContextExt::new(_parentctx.clone(), recog.base.get_state());
        recog.base.enter_rule(_localctx.clone(), 4, RULE_func_def);
        let mut _localctx: Rc<Func_defContextAll> = _localctx;
		let result: Result<(), ANTLRError> = (|| {

			recog.base.set_state(79);
			recog.err_handler.sync(&mut recog.base)?;
			match recog.base.input.la(1) {
			 FUNC 
				=> {
					//recog.base.enter_outer_alt(_localctx.clone(), 1);
					recog.base.enter_outer_alt(None, 1);
					{
					recog.base.set_state(69);
					recog.base.match_token(FUNC,&mut recog.err_handler)?;

					recog.base.set_state(70);
					recog.base.match_token(IDENT,&mut recog.err_handler)?;

					/*InvokeRule arglist*/
					recog.base.set_state(71);
					recog.arglist()?;

					/*InvokeRule block_expr*/
					recog.base.set_state(72);
					recog.block_expr()?;

					}
				}

			 PROC 
				=> {
					//recog.base.enter_outer_alt(_localctx.clone(), 2);
					recog.base.enter_outer_alt(None, 2);
					{
					recog.base.set_state(74);
					recog.base.match_token(PROC,&mut recog.err_handler)?;

					recog.base.set_state(75);
					recog.base.match_token(IDENT,&mut recog.err_handler)?;

					/*InvokeRule arglist*/
					recog.base.set_state(76);
					recog.arglist()?;

					/*InvokeRule block_stmt*/
					recog.base.set_state(77);
					recog.block_stmt()?;

					}
				}

				_ => Err(ANTLRError::NoAltError(NoViableAltError::new(&mut recog.base)))?
			}
			Ok(())
		})();
		match result {
		Ok(_)=>{},
        Err(e @ ANTLRError::FallThrough(_)) => return Err(e),
		Err(ref re) => {
				//_localctx.exception = re;
				recog.err_handler.report_error(&mut recog.base, re);
				recog.err_handler.recover(&mut recog.base, re)?;
			}
		}
		recog.base.exit_rule();

		Ok(_localctx)
	}
}
//------------------- global_init ----------------
pub type Global_initContextAll<'input> = Global_initContext<'input>;


pub type Global_initContext<'input> = BaseParserRuleContext<'input,Global_initContextExt<'input>>;

#[derive(Clone)]
pub struct Global_initContextExt<'input>{
ph:PhantomData<&'input str>
}

impl<'input> ailParserContext<'input> for Global_initContext<'input>{}

impl<'input,'a> Listenable<dyn ailListener<'input> + 'a> for Global_initContext<'input>{
		fn enter(&self,listener: &mut (dyn ailListener<'input> + 'a)) {
			listener.enter_every_rule(self);
			listener.enter_global_init(self);
		}
		fn exit(&self,listener: &mut (dyn ailListener<'input> + 'a)) {
			listener.exit_global_init(self);
			listener.exit_every_rule(self);
		}
}

impl<'input,'a> Visitable<dyn ailVisitor<'input> + 'a> for Global_initContext<'input>{
	fn accept(&self,visitor: &mut (dyn ailVisitor<'input> + 'a)) {
		visitor.visit_global_init(self);
	}
}

impl<'input> CustomRuleContext<'input> for Global_initContextExt<'input>{
	type TF = LocalTokenFactory<'input>;
	type Ctx = ailParserContextType;
	fn get_rule_index(&self) -> usize { RULE_global_init }
	//fn type_rule_index() -> usize where Self: Sized { RULE_global_init }
}
antlr_rust::tid!{Global_initContextExt<'a>}

impl<'input> Global_initContextExt<'input>{
	fn new(parent: Option<Rc<dyn ailParserContext<'input> + 'input > >, invoking_state: isize) -> Rc<Global_initContextAll<'input>> {
		Rc::new(
			BaseParserRuleContext::new_parser_ctx(parent, invoking_state,Global_initContextExt{
				ph:PhantomData
			}),
		)
	}
}

pub trait Global_initContextAttrs<'input>: ailParserContext<'input> + BorrowMut<Global_initContextExt<'input>>{

/// Retrieves first TerminalNode corresponding to token GLOBAL
/// Returns `None` if there is no child corresponding to token GLOBAL
fn GLOBAL(&self) -> Option<Rc<TerminalNode<'input,ailParserContextType>>> where Self:Sized{
	self.get_token(GLOBAL, 0)
}
/// Retrieves first TerminalNode corresponding to token IDENT
/// Returns `None` if there is no child corresponding to token IDENT
fn IDENT(&self) -> Option<Rc<TerminalNode<'input,ailParserContextType>>> where Self:Sized{
	self.get_token(IDENT, 0)
}
/// Retrieves first TerminalNode corresponding to token EQUAL
/// Returns `None` if there is no child corresponding to token EQUAL
fn EQUAL(&self) -> Option<Rc<TerminalNode<'input,ailParserContextType>>> where Self:Sized{
	self.get_token(EQUAL, 0)
}
fn expr(&self) -> Option<Rc<ExprContextAll<'input>>> where Self:Sized{
	self.child_of_type(0)
}
/// Retrieves first TerminalNode corresponding to token SEMICOLON
/// Returns `None` if there is no child corresponding to token SEMICOLON
fn SEMICOLON(&self) -> Option<Rc<TerminalNode<'input,ailParserContextType>>> where Self:Sized{
	self.get_token(SEMICOLON, 0)
}

}

impl<'input> Global_initContextAttrs<'input> for Global_initContext<'input>{}

impl<'input, I, H> ailParser<'input, I, H>
where
    I: TokenStream<'input, TF = LocalTokenFactory<'input> > + TidAble<'input>,
    H: ErrorStrategy<'input,BaseParserType<'input,I>>
{
	pub fn global_init(&mut self,)
	-> Result<Rc<Global_initContextAll<'input>>,ANTLRError> {
		let mut recog = self;
		let _parentctx = recog.ctx.take();
		let mut _localctx = Global_initContextExt::new(_parentctx.clone(), recog.base.get_state());
        recog.base.enter_rule(_localctx.clone(), 6, RULE_global_init);
        let mut _localctx: Rc<Global_initContextAll> = _localctx;
		let result: Result<(), ANTLRError> = (|| {

			//recog.base.enter_outer_alt(_localctx.clone(), 1);
			recog.base.enter_outer_alt(None, 1);
			{
			recog.base.set_state(81);
			recog.base.match_token(GLOBAL,&mut recog.err_handler)?;

			recog.base.set_state(82);
			recog.base.match_token(IDENT,&mut recog.err_handler)?;

			recog.base.set_state(83);
			recog.base.match_token(EQUAL,&mut recog.err_handler)?;

			/*InvokeRule expr*/
			recog.base.set_state(84);
			recog.expr_rec(0)?;

			recog.base.set_state(85);
			recog.base.match_token(SEMICOLON,&mut recog.err_handler)?;

			}
			Ok(())
		})();
		match result {
		Ok(_)=>{},
        Err(e @ ANTLRError::FallThrough(_)) => return Err(e),
		Err(ref re) => {
				//_localctx.exception = re;
				recog.err_handler.report_error(&mut recog.base, re);
				recog.err_handler.recover(&mut recog.base, re)?;
			}
		}
		recog.base.exit_rule();

		Ok(_localctx)
	}
}
//------------------- stmt ----------------
pub type StmtContextAll<'input> = StmtContext<'input>;


pub type StmtContext<'input> = BaseParserRuleContext<'input,StmtContextExt<'input>>;

#[derive(Clone)]
pub struct StmtContextExt<'input>{
ph:PhantomData<&'input str>
}

impl<'input> ailParserContext<'input> for StmtContext<'input>{}

impl<'input,'a> Listenable<dyn ailListener<'input> + 'a> for StmtContext<'input>{
		fn enter(&self,listener: &mut (dyn ailListener<'input> + 'a)) {
			listener.enter_every_rule(self);
			listener.enter_stmt(self);
		}
		fn exit(&self,listener: &mut (dyn ailListener<'input> + 'a)) {
			listener.exit_stmt(self);
			listener.exit_every_rule(self);
		}
}

impl<'input,'a> Visitable<dyn ailVisitor<'input> + 'a> for StmtContext<'input>{
	fn accept(&self,visitor: &mut (dyn ailVisitor<'input> + 'a)) {
		visitor.visit_stmt(self);
	}
}

impl<'input> CustomRuleContext<'input> for StmtContextExt<'input>{
	type TF = LocalTokenFactory<'input>;
	type Ctx = ailParserContextType;
	fn get_rule_index(&self) -> usize { RULE_stmt }
	//fn type_rule_index() -> usize where Self: Sized { RULE_stmt }
}
antlr_rust::tid!{StmtContextExt<'a>}

impl<'input> StmtContextExt<'input>{
	fn new(parent: Option<Rc<dyn ailParserContext<'input> + 'input > >, invoking_state: isize) -> Rc<StmtContextAll<'input>> {
		Rc::new(
			BaseParserRuleContext::new_parser_ctx(parent, invoking_state,StmtContextExt{
				ph:PhantomData
			}),
		)
	}
}

pub trait StmtContextAttrs<'input>: ailParserContext<'input> + BorrowMut<StmtContextExt<'input>>{

fn if_stmt(&self) -> Option<Rc<If_stmtContextAll<'input>>> where Self:Sized{
	self.child_of_type(0)
}
fn while_stmt(&self) -> Option<Rc<While_stmtContextAll<'input>>> where Self:Sized{
	self.child_of_type(0)
}
fn for_stmt(&self) -> Option<Rc<For_stmtContextAll<'input>>> where Self:Sized{
	self.child_of_type(0)
}
fn call_stmt(&self) -> Option<Rc<Call_stmtContextAll<'input>>> where Self:Sized{
	self.child_of_type(0)
}
fn assign_stmt(&self) -> Option<Rc<Assign_stmtContextAll<'input>>> where Self:Sized{
	self.child_of_type(0)
}
fn modify_stmt(&self) -> Option<Rc<Modify_stmtContextAll<'input>>> where Self:Sized{
	self.child_of_type(0)
}

}

impl<'input> StmtContextAttrs<'input> for StmtContext<'input>{}

impl<'input, I, H> ailParser<'input, I, H>
where
    I: TokenStream<'input, TF = LocalTokenFactory<'input> > + TidAble<'input>,
    H: ErrorStrategy<'input,BaseParserType<'input,I>>
{
	pub fn stmt(&mut self,)
	-> Result<Rc<StmtContextAll<'input>>,ANTLRError> {
		let mut recog = self;
		let _parentctx = recog.ctx.take();
		let mut _localctx = StmtContextExt::new(_parentctx.clone(), recog.base.get_state());
        recog.base.enter_rule(_localctx.clone(), 8, RULE_stmt);
        let mut _localctx: Rc<StmtContextAll> = _localctx;
		let result: Result<(), ANTLRError> = (|| {

			recog.base.set_state(93);
			recog.err_handler.sync(&mut recog.base)?;
			match  recog.interpreter.adaptive_predict(4,&mut recog.base)? {
				1 =>{
					//recog.base.enter_outer_alt(_localctx.clone(), 1);
					recog.base.enter_outer_alt(None, 1);
					{
					/*InvokeRule if_stmt*/
					recog.base.set_state(87);
					recog.if_stmt()?;

					}
				}
			,
				2 =>{
					//recog.base.enter_outer_alt(_localctx.clone(), 2);
					recog.base.enter_outer_alt(None, 2);
					{
					/*InvokeRule while_stmt*/
					recog.base.set_state(88);
					recog.while_stmt()?;

					}
				}
			,
				3 =>{
					//recog.base.enter_outer_alt(_localctx.clone(), 3);
					recog.base.enter_outer_alt(None, 3);
					{
					/*InvokeRule for_stmt*/
					recog.base.set_state(89);
					recog.for_stmt()?;

					}
				}
			,
				4 =>{
					//recog.base.enter_outer_alt(_localctx.clone(), 4);
					recog.base.enter_outer_alt(None, 4);
					{
					/*InvokeRule call_stmt*/
					recog.base.set_state(90);
					recog.call_stmt()?;

					}
				}
			,
				5 =>{
					//recog.base.enter_outer_alt(_localctx.clone(), 5);
					recog.base.enter_outer_alt(None, 5);
					{
					/*InvokeRule assign_stmt*/
					recog.base.set_state(91);
					recog.assign_stmt()?;

					}
				}
			,
				6 =>{
					//recog.base.enter_outer_alt(_localctx.clone(), 6);
					recog.base.enter_outer_alt(None, 6);
					{
					/*InvokeRule modify_stmt*/
					recog.base.set_state(92);
					recog.modify_stmt()?;

					}
				}

				_ => {}
			}
			Ok(())
		})();
		match result {
		Ok(_)=>{},
        Err(e @ ANTLRError::FallThrough(_)) => return Err(e),
		Err(ref re) => {
				//_localctx.exception = re;
				recog.err_handler.report_error(&mut recog.base, re);
				recog.err_handler.recover(&mut recog.base, re)?;
			}
		}
		recog.base.exit_rule();

		Ok(_localctx)
	}
}
//------------------- if_stmt ----------------
pub type If_stmtContextAll<'input> = If_stmtContext<'input>;


pub type If_stmtContext<'input> = BaseParserRuleContext<'input,If_stmtContextExt<'input>>;

#[derive(Clone)]
pub struct If_stmtContextExt<'input>{
ph:PhantomData<&'input str>
}

impl<'input> ailParserContext<'input> for If_stmtContext<'input>{}

impl<'input,'a> Listenable<dyn ailListener<'input> + 'a> for If_stmtContext<'input>{
		fn enter(&self,listener: &mut (dyn ailListener<'input> + 'a)) {
			listener.enter_every_rule(self);
			listener.enter_if_stmt(self);
		}
		fn exit(&self,listener: &mut (dyn ailListener<'input> + 'a)) {
			listener.exit_if_stmt(self);
			listener.exit_every_rule(self);
		}
}

impl<'input,'a> Visitable<dyn ailVisitor<'input> + 'a> for If_stmtContext<'input>{
	fn accept(&self,visitor: &mut (dyn ailVisitor<'input> + 'a)) {
		visitor.visit_if_stmt(self);
	}
}

impl<'input> CustomRuleContext<'input> for If_stmtContextExt<'input>{
	type TF = LocalTokenFactory<'input>;
	type Ctx = ailParserContextType;
	fn get_rule_index(&self) -> usize { RULE_if_stmt }
	//fn type_rule_index() -> usize where Self: Sized { RULE_if_stmt }
}
antlr_rust::tid!{If_stmtContextExt<'a>}

impl<'input> If_stmtContextExt<'input>{
	fn new(parent: Option<Rc<dyn ailParserContext<'input> + 'input > >, invoking_state: isize) -> Rc<If_stmtContextAll<'input>> {
		Rc::new(
			BaseParserRuleContext::new_parser_ctx(parent, invoking_state,If_stmtContextExt{
				ph:PhantomData
			}),
		)
	}
}

pub trait If_stmtContextAttrs<'input>: ailParserContext<'input> + BorrowMut<If_stmtContextExt<'input>>{

/// Retrieves all `TerminalNode`s corresponding to token IF in current rule
fn IF_all(&self) -> Vec<Rc<TerminalNode<'input,ailParserContextType>>>  where Self:Sized{
	self.children_of_type()
}
/// Retrieves 'i's TerminalNode corresponding to token IF, starting from 0.
/// Returns `None` if number of children corresponding to token IF is less or equal than `i`.
fn IF(&self, i: usize) -> Option<Rc<TerminalNode<'input,ailParserContextType>>> where Self:Sized{
	self.get_token(IF, i)
}
fn expr_all(&self) ->  Vec<Rc<ExprContextAll<'input>>> where Self:Sized{
	self.children_of_type()
}
fn expr(&self, i: usize) -> Option<Rc<ExprContextAll<'input>>> where Self:Sized{
	self.child_of_type(i)
}
fn block_stmt_all(&self) ->  Vec<Rc<Block_stmtContextAll<'input>>> where Self:Sized{
	self.children_of_type()
}
fn block_stmt(&self, i: usize) -> Option<Rc<Block_stmtContextAll<'input>>> where Self:Sized{
	self.child_of_type(i)
}
/// Retrieves all `TerminalNode`s corresponding to token ELSE in current rule
fn ELSE_all(&self) -> Vec<Rc<TerminalNode<'input,ailParserContextType>>>  where Self:Sized{
	self.children_of_type()
}
/// Retrieves 'i's TerminalNode corresponding to token ELSE, starting from 0.
/// Returns `None` if number of children corresponding to token ELSE is less or equal than `i`.
fn ELSE(&self, i: usize) -> Option<Rc<TerminalNode<'input,ailParserContextType>>> where Self:Sized{
	self.get_token(ELSE, i)
}

}

impl<'input> If_stmtContextAttrs<'input> for If_stmtContext<'input>{}

impl<'input, I, H> ailParser<'input, I, H>
where
    I: TokenStream<'input, TF = LocalTokenFactory<'input> > + TidAble<'input>,
    H: ErrorStrategy<'input,BaseParserType<'input,I>>
{
	pub fn if_stmt(&mut self,)
	-> Result<Rc<If_stmtContextAll<'input>>,ANTLRError> {
		let mut recog = self;
		let _parentctx = recog.ctx.take();
		let mut _localctx = If_stmtContextExt::new(_parentctx.clone(), recog.base.get_state());
        recog.base.enter_rule(_localctx.clone(), 10, RULE_if_stmt);
        let mut _localctx: Rc<If_stmtContextAll> = _localctx;
		let mut _la: isize = -1;
		let result: Result<(), ANTLRError> = (|| {

			let mut _alt: isize;
			//recog.base.enter_outer_alt(_localctx.clone(), 1);
			recog.base.enter_outer_alt(None, 1);
			{
			recog.base.set_state(95);
			recog.base.match_token(IF,&mut recog.err_handler)?;

			/*InvokeRule expr*/
			recog.base.set_state(96);
			recog.expr_rec(0)?;

			/*InvokeRule block_stmt*/
			recog.base.set_state(97);
			recog.block_stmt()?;

			recog.base.set_state(105);
			recog.err_handler.sync(&mut recog.base)?;
			_alt = recog.interpreter.adaptive_predict(5,&mut recog.base)?;
			while { _alt!=2 && _alt!=INVALID_ALT } {
				if _alt==1 {
					{
					{
					recog.base.set_state(98);
					recog.base.match_token(ELSE,&mut recog.err_handler)?;

					recog.base.set_state(99);
					recog.base.match_token(IF,&mut recog.err_handler)?;

					/*InvokeRule expr*/
					recog.base.set_state(100);
					recog.expr_rec(0)?;

					/*InvokeRule block_stmt*/
					recog.base.set_state(101);
					recog.block_stmt()?;

					}
					} 
				}
				recog.base.set_state(107);
				recog.err_handler.sync(&mut recog.base)?;
				_alt = recog.interpreter.adaptive_predict(5,&mut recog.base)?;
			}
			recog.base.set_state(110);
			recog.err_handler.sync(&mut recog.base)?;
			_la = recog.base.input.la(1);
			if _la==ELSE {
				{
				recog.base.set_state(108);
				recog.base.match_token(ELSE,&mut recog.err_handler)?;

				/*InvokeRule block_stmt*/
				recog.base.set_state(109);
				recog.block_stmt()?;

				}
			}

			}
			Ok(())
		})();
		match result {
		Ok(_)=>{},
        Err(e @ ANTLRError::FallThrough(_)) => return Err(e),
		Err(ref re) => {
				//_localctx.exception = re;
				recog.err_handler.report_error(&mut recog.base, re);
				recog.err_handler.recover(&mut recog.base, re)?;
			}
		}
		recog.base.exit_rule();

		Ok(_localctx)
	}
}
//------------------- while_stmt ----------------
pub type While_stmtContextAll<'input> = While_stmtContext<'input>;


pub type While_stmtContext<'input> = BaseParserRuleContext<'input,While_stmtContextExt<'input>>;

#[derive(Clone)]
pub struct While_stmtContextExt<'input>{
ph:PhantomData<&'input str>
}

impl<'input> ailParserContext<'input> for While_stmtContext<'input>{}

impl<'input,'a> Listenable<dyn ailListener<'input> + 'a> for While_stmtContext<'input>{
		fn enter(&self,listener: &mut (dyn ailListener<'input> + 'a)) {
			listener.enter_every_rule(self);
			listener.enter_while_stmt(self);
		}
		fn exit(&self,listener: &mut (dyn ailListener<'input> + 'a)) {
			listener.exit_while_stmt(self);
			listener.exit_every_rule(self);
		}
}

impl<'input,'a> Visitable<dyn ailVisitor<'input> + 'a> for While_stmtContext<'input>{
	fn accept(&self,visitor: &mut (dyn ailVisitor<'input> + 'a)) {
		visitor.visit_while_stmt(self);
	}
}

impl<'input> CustomRuleContext<'input> for While_stmtContextExt<'input>{
	type TF = LocalTokenFactory<'input>;
	type Ctx = ailParserContextType;
	fn get_rule_index(&self) -> usize { RULE_while_stmt }
	//fn type_rule_index() -> usize where Self: Sized { RULE_while_stmt }
}
antlr_rust::tid!{While_stmtContextExt<'a>}

impl<'input> While_stmtContextExt<'input>{
	fn new(parent: Option<Rc<dyn ailParserContext<'input> + 'input > >, invoking_state: isize) -> Rc<While_stmtContextAll<'input>> {
		Rc::new(
			BaseParserRuleContext::new_parser_ctx(parent, invoking_state,While_stmtContextExt{
				ph:PhantomData
			}),
		)
	}
}

pub trait While_stmtContextAttrs<'input>: ailParserContext<'input> + BorrowMut<While_stmtContextExt<'input>>{

/// Retrieves first TerminalNode corresponding to token WHILE
/// Returns `None` if there is no child corresponding to token WHILE
fn WHILE(&self) -> Option<Rc<TerminalNode<'input,ailParserContextType>>> where Self:Sized{
	self.get_token(WHILE, 0)
}
fn expr(&self) -> Option<Rc<ExprContextAll<'input>>> where Self:Sized{
	self.child_of_type(0)
}
fn block_stmt(&self) -> Option<Rc<Block_stmtContextAll<'input>>> where Self:Sized{
	self.child_of_type(0)
}

}

impl<'input> While_stmtContextAttrs<'input> for While_stmtContext<'input>{}

impl<'input, I, H> ailParser<'input, I, H>
where
    I: TokenStream<'input, TF = LocalTokenFactory<'input> > + TidAble<'input>,
    H: ErrorStrategy<'input,BaseParserType<'input,I>>
{
	pub fn while_stmt(&mut self,)
	-> Result<Rc<While_stmtContextAll<'input>>,ANTLRError> {
		let mut recog = self;
		let _parentctx = recog.ctx.take();
		let mut _localctx = While_stmtContextExt::new(_parentctx.clone(), recog.base.get_state());
        recog.base.enter_rule(_localctx.clone(), 12, RULE_while_stmt);
        let mut _localctx: Rc<While_stmtContextAll> = _localctx;
		let result: Result<(), ANTLRError> = (|| {

			//recog.base.enter_outer_alt(_localctx.clone(), 1);
			recog.base.enter_outer_alt(None, 1);
			{
			recog.base.set_state(112);
			recog.base.match_token(WHILE,&mut recog.err_handler)?;

			/*InvokeRule expr*/
			recog.base.set_state(113);
			recog.expr_rec(0)?;

			/*InvokeRule block_stmt*/
			recog.base.set_state(114);
			recog.block_stmt()?;

			}
			Ok(())
		})();
		match result {
		Ok(_)=>{},
        Err(e @ ANTLRError::FallThrough(_)) => return Err(e),
		Err(ref re) => {
				//_localctx.exception = re;
				recog.err_handler.report_error(&mut recog.base, re);
				recog.err_handler.recover(&mut recog.base, re)?;
			}
		}
		recog.base.exit_rule();

		Ok(_localctx)
	}
}
//------------------- for_stmt ----------------
pub type For_stmtContextAll<'input> = For_stmtContext<'input>;


pub type For_stmtContext<'input> = BaseParserRuleContext<'input,For_stmtContextExt<'input>>;

#[derive(Clone)]
pub struct For_stmtContextExt<'input>{
ph:PhantomData<&'input str>
}

impl<'input> ailParserContext<'input> for For_stmtContext<'input>{}

impl<'input,'a> Listenable<dyn ailListener<'input> + 'a> for For_stmtContext<'input>{
		fn enter(&self,listener: &mut (dyn ailListener<'input> + 'a)) {
			listener.enter_every_rule(self);
			listener.enter_for_stmt(self);
		}
		fn exit(&self,listener: &mut (dyn ailListener<'input> + 'a)) {
			listener.exit_for_stmt(self);
			listener.exit_every_rule(self);
		}
}

impl<'input,'a> Visitable<dyn ailVisitor<'input> + 'a> for For_stmtContext<'input>{
	fn accept(&self,visitor: &mut (dyn ailVisitor<'input> + 'a)) {
		visitor.visit_for_stmt(self);
	}
}

impl<'input> CustomRuleContext<'input> for For_stmtContextExt<'input>{
	type TF = LocalTokenFactory<'input>;
	type Ctx = ailParserContextType;
	fn get_rule_index(&self) -> usize { RULE_for_stmt }
	//fn type_rule_index() -> usize where Self: Sized { RULE_for_stmt }
}
antlr_rust::tid!{For_stmtContextExt<'a>}

impl<'input> For_stmtContextExt<'input>{
	fn new(parent: Option<Rc<dyn ailParserContext<'input> + 'input > >, invoking_state: isize) -> Rc<For_stmtContextAll<'input>> {
		Rc::new(
			BaseParserRuleContext::new_parser_ctx(parent, invoking_state,For_stmtContextExt{
				ph:PhantomData
			}),
		)
	}
}

pub trait For_stmtContextAttrs<'input>: ailParserContext<'input> + BorrowMut<For_stmtContextExt<'input>>{

/// Retrieves first TerminalNode corresponding to token FOR
/// Returns `None` if there is no child corresponding to token FOR
fn FOR(&self) -> Option<Rc<TerminalNode<'input,ailParserContextType>>> where Self:Sized{
	self.get_token(FOR, 0)
}
fn block_stmt(&self) -> Option<Rc<Block_stmtContextAll<'input>>> where Self:Sized{
	self.child_of_type(0)
}
/// Retrieves all `TerminalNode`s corresponding to token IDENT in current rule
fn IDENT_all(&self) -> Vec<Rc<TerminalNode<'input,ailParserContextType>>>  where Self:Sized{
	self.children_of_type()
}
/// Retrieves 'i's TerminalNode corresponding to token IDENT, starting from 0.
/// Returns `None` if number of children corresponding to token IDENT is less or equal than `i`.
fn IDENT(&self, i: usize) -> Option<Rc<TerminalNode<'input,ailParserContextType>>> where Self:Sized{
	self.get_token(IDENT, i)
}
/// Retrieves first TerminalNode corresponding to token FROM
/// Returns `None` if there is no child corresponding to token FROM
fn FROM(&self) -> Option<Rc<TerminalNode<'input,ailParserContextType>>> where Self:Sized{
	self.get_token(FROM, 0)
}
fn expr_all(&self) ->  Vec<Rc<ExprContextAll<'input>>> where Self:Sized{
	self.children_of_type()
}
fn expr(&self, i: usize) -> Option<Rc<ExprContextAll<'input>>> where Self:Sized{
	self.child_of_type(i)
}
/// Retrieves first TerminalNode corresponding to token TO
/// Returns `None` if there is no child corresponding to token TO
fn TO(&self) -> Option<Rc<TerminalNode<'input,ailParserContextType>>> where Self:Sized{
	self.get_token(TO, 0)
}
/// Retrieves first TerminalNode corresponding to token BY
/// Returns `None` if there is no child corresponding to token BY
fn BY(&self) -> Option<Rc<TerminalNode<'input,ailParserContextType>>> where Self:Sized{
	self.get_token(BY, 0)
}
/// Retrieves first TerminalNode corresponding to token IN
/// Returns `None` if there is no child corresponding to token IN
fn IN(&self) -> Option<Rc<TerminalNode<'input,ailParserContextType>>> where Self:Sized{
	self.get_token(IN, 0)
}
/// Retrieves first TerminalNode corresponding to token COMMA
/// Returns `None` if there is no child corresponding to token COMMA
fn COMMA(&self) -> Option<Rc<TerminalNode<'input,ailParserContextType>>> where Self:Sized{
	self.get_token(COMMA, 0)
}

}

impl<'input> For_stmtContextAttrs<'input> for For_stmtContext<'input>{}

impl<'input, I, H> ailParser<'input, I, H>
where
    I: TokenStream<'input, TF = LocalTokenFactory<'input> > + TidAble<'input>,
    H: ErrorStrategy<'input,BaseParserType<'input,I>>
{
	pub fn for_stmt(&mut self,)
	-> Result<Rc<For_stmtContextAll<'input>>,ANTLRError> {
		let mut recog = self;
		let _parentctx = recog.ctx.take();
		let mut _localctx = For_stmtContextExt::new(_parentctx.clone(), recog.base.get_state());
        recog.base.enter_rule(_localctx.clone(), 14, RULE_for_stmt);
        let mut _localctx: Rc<For_stmtContextAll> = _localctx;
		let result: Result<(), ANTLRError> = (|| {

			//recog.base.enter_outer_alt(_localctx.clone(), 1);
			recog.base.enter_outer_alt(None, 1);
			{
			recog.base.set_state(116);
			recog.base.match_token(FOR,&mut recog.err_handler)?;

			recog.base.set_state(133);
			recog.err_handler.sync(&mut recog.base)?;
			match  recog.interpreter.adaptive_predict(7,&mut recog.base)? {
				1 =>{
					{
					recog.base.set_state(117);
					recog.base.match_token(IDENT,&mut recog.err_handler)?;

					recog.base.set_state(118);
					recog.base.match_token(FROM,&mut recog.err_handler)?;

					/*InvokeRule expr*/
					recog.base.set_state(119);
					recog.expr_rec(0)?;

					recog.base.set_state(120);
					recog.base.match_token(TO,&mut recog.err_handler)?;

					/*InvokeRule expr*/
					recog.base.set_state(121);
					recog.expr_rec(0)?;

					recog.base.set_state(122);
					recog.base.match_token(BY,&mut recog.err_handler)?;

					/*InvokeRule expr*/
					recog.base.set_state(123);
					recog.expr_rec(0)?;

					}
				}
			,
				2 =>{
					{
					recog.base.set_state(125);
					recog.base.match_token(IDENT,&mut recog.err_handler)?;

					recog.base.set_state(126);
					recog.base.match_token(IN,&mut recog.err_handler)?;

					/*InvokeRule expr*/
					recog.base.set_state(127);
					recog.expr_rec(0)?;

					}
				}
			,
				3 =>{
					{
					recog.base.set_state(128);
					recog.base.match_token(IDENT,&mut recog.err_handler)?;

					recog.base.set_state(129);
					recog.base.match_token(COMMA,&mut recog.err_handler)?;

					recog.base.set_state(130);
					recog.base.match_token(IDENT,&mut recog.err_handler)?;

					recog.base.set_state(131);
					recog.base.match_token(IN,&mut recog.err_handler)?;

					/*InvokeRule expr*/
					recog.base.set_state(132);
					recog.expr_rec(0)?;

					}
				}

				_ => {}
			}
			/*InvokeRule block_stmt*/
			recog.base.set_state(135);
			recog.block_stmt()?;

			}
			Ok(())
		})();
		match result {
		Ok(_)=>{},
        Err(e @ ANTLRError::FallThrough(_)) => return Err(e),
		Err(ref re) => {
				//_localctx.exception = re;
				recog.err_handler.report_error(&mut recog.base, re);
				recog.err_handler.recover(&mut recog.base, re)?;
			}
		}
		recog.base.exit_rule();

		Ok(_localctx)
	}
}
//------------------- call_stmt ----------------
pub type Call_stmtContextAll<'input> = Call_stmtContext<'input>;


pub type Call_stmtContext<'input> = BaseParserRuleContext<'input,Call_stmtContextExt<'input>>;

#[derive(Clone)]
pub struct Call_stmtContextExt<'input>{
ph:PhantomData<&'input str>
}

impl<'input> ailParserContext<'input> for Call_stmtContext<'input>{}

impl<'input,'a> Listenable<dyn ailListener<'input> + 'a> for Call_stmtContext<'input>{
		fn enter(&self,listener: &mut (dyn ailListener<'input> + 'a)) {
			listener.enter_every_rule(self);
			listener.enter_call_stmt(self);
		}
		fn exit(&self,listener: &mut (dyn ailListener<'input> + 'a)) {
			listener.exit_call_stmt(self);
			listener.exit_every_rule(self);
		}
}

impl<'input,'a> Visitable<dyn ailVisitor<'input> + 'a> for Call_stmtContext<'input>{
	fn accept(&self,visitor: &mut (dyn ailVisitor<'input> + 'a)) {
		visitor.visit_call_stmt(self);
	}
}

impl<'input> CustomRuleContext<'input> for Call_stmtContextExt<'input>{
	type TF = LocalTokenFactory<'input>;
	type Ctx = ailParserContextType;
	fn get_rule_index(&self) -> usize { RULE_call_stmt }
	//fn type_rule_index() -> usize where Self: Sized { RULE_call_stmt }
}
antlr_rust::tid!{Call_stmtContextExt<'a>}

impl<'input> Call_stmtContextExt<'input>{
	fn new(parent: Option<Rc<dyn ailParserContext<'input> + 'input > >, invoking_state: isize) -> Rc<Call_stmtContextAll<'input>> {
		Rc::new(
			BaseParserRuleContext::new_parser_ctx(parent, invoking_state,Call_stmtContextExt{
				ph:PhantomData
			}),
		)
	}
}

pub trait Call_stmtContextAttrs<'input>: ailParserContext<'input> + BorrowMut<Call_stmtContextExt<'input>>{

/// Retrieves first TerminalNode corresponding to token IDENT
/// Returns `None` if there is no child corresponding to token IDENT
fn IDENT(&self) -> Option<Rc<TerminalNode<'input,ailParserContextType>>> where Self:Sized{
	self.get_token(IDENT, 0)
}
fn calllist(&self) -> Option<Rc<CalllistContextAll<'input>>> where Self:Sized{
	self.child_of_type(0)
}
/// Retrieves first TerminalNode corresponding to token SEMICOLON
/// Returns `None` if there is no child corresponding to token SEMICOLON
fn SEMICOLON(&self) -> Option<Rc<TerminalNode<'input,ailParserContextType>>> where Self:Sized{
	self.get_token(SEMICOLON, 0)
}

}

impl<'input> Call_stmtContextAttrs<'input> for Call_stmtContext<'input>{}

impl<'input, I, H> ailParser<'input, I, H>
where
    I: TokenStream<'input, TF = LocalTokenFactory<'input> > + TidAble<'input>,
    H: ErrorStrategy<'input,BaseParserType<'input,I>>
{
	pub fn call_stmt(&mut self,)
	-> Result<Rc<Call_stmtContextAll<'input>>,ANTLRError> {
		let mut recog = self;
		let _parentctx = recog.ctx.take();
		let mut _localctx = Call_stmtContextExt::new(_parentctx.clone(), recog.base.get_state());
        recog.base.enter_rule(_localctx.clone(), 16, RULE_call_stmt);
        let mut _localctx: Rc<Call_stmtContextAll> = _localctx;
		let result: Result<(), ANTLRError> = (|| {

			//recog.base.enter_outer_alt(_localctx.clone(), 1);
			recog.base.enter_outer_alt(None, 1);
			{
			recog.base.set_state(137);
			recog.base.match_token(IDENT,&mut recog.err_handler)?;

			/*InvokeRule calllist*/
			recog.base.set_state(138);
			recog.calllist()?;

			recog.base.set_state(139);
			recog.base.match_token(SEMICOLON,&mut recog.err_handler)?;

			}
			Ok(())
		})();
		match result {
		Ok(_)=>{},
        Err(e @ ANTLRError::FallThrough(_)) => return Err(e),
		Err(ref re) => {
				//_localctx.exception = re;
				recog.err_handler.report_error(&mut recog.base, re);
				recog.err_handler.recover(&mut recog.base, re)?;
			}
		}
		recog.base.exit_rule();

		Ok(_localctx)
	}
}
//------------------- assign_stmt ----------------
pub type Assign_stmtContextAll<'input> = Assign_stmtContext<'input>;


pub type Assign_stmtContext<'input> = BaseParserRuleContext<'input,Assign_stmtContextExt<'input>>;

#[derive(Clone)]
pub struct Assign_stmtContextExt<'input>{
ph:PhantomData<&'input str>
}

impl<'input> ailParserContext<'input> for Assign_stmtContext<'input>{}

impl<'input,'a> Listenable<dyn ailListener<'input> + 'a> for Assign_stmtContext<'input>{
		fn enter(&self,listener: &mut (dyn ailListener<'input> + 'a)) {
			listener.enter_every_rule(self);
			listener.enter_assign_stmt(self);
		}
		fn exit(&self,listener: &mut (dyn ailListener<'input> + 'a)) {
			listener.exit_assign_stmt(self);
			listener.exit_every_rule(self);
		}
}

impl<'input,'a> Visitable<dyn ailVisitor<'input> + 'a> for Assign_stmtContext<'input>{
	fn accept(&self,visitor: &mut (dyn ailVisitor<'input> + 'a)) {
		visitor.visit_assign_stmt(self);
	}
}

impl<'input> CustomRuleContext<'input> for Assign_stmtContextExt<'input>{
	type TF = LocalTokenFactory<'input>;
	type Ctx = ailParserContextType;
	fn get_rule_index(&self) -> usize { RULE_assign_stmt }
	//fn type_rule_index() -> usize where Self: Sized { RULE_assign_stmt }
}
antlr_rust::tid!{Assign_stmtContextExt<'a>}

impl<'input> Assign_stmtContextExt<'input>{
	fn new(parent: Option<Rc<dyn ailParserContext<'input> + 'input > >, invoking_state: isize) -> Rc<Assign_stmtContextAll<'input>> {
		Rc::new(
			BaseParserRuleContext::new_parser_ctx(parent, invoking_state,Assign_stmtContextExt{
				ph:PhantomData
			}),
		)
	}
}

pub trait Assign_stmtContextAttrs<'input>: ailParserContext<'input> + BorrowMut<Assign_stmtContextExt<'input>>{

fn assignlist(&self) -> Option<Rc<AssignlistContextAll<'input>>> where Self:Sized{
	self.child_of_type(0)
}
fn block_stmt(&self) -> Option<Rc<Block_stmtContextAll<'input>>> where Self:Sized{
	self.child_of_type(0)
}
/// Retrieves first TerminalNode corresponding to token SEMICOLON
/// Returns `None` if there is no child corresponding to token SEMICOLON
fn SEMICOLON(&self) -> Option<Rc<TerminalNode<'input,ailParserContextType>>> where Self:Sized{
	self.get_token(SEMICOLON, 0)
}

}

impl<'input> Assign_stmtContextAttrs<'input> for Assign_stmtContext<'input>{}

impl<'input, I, H> ailParser<'input, I, H>
where
    I: TokenStream<'input, TF = LocalTokenFactory<'input> > + TidAble<'input>,
    H: ErrorStrategy<'input,BaseParserType<'input,I>>
{
	pub fn assign_stmt(&mut self,)
	-> Result<Rc<Assign_stmtContextAll<'input>>,ANTLRError> {
		let mut recog = self;
		let _parentctx = recog.ctx.take();
		let mut _localctx = Assign_stmtContextExt::new(_parentctx.clone(), recog.base.get_state());
        recog.base.enter_rule(_localctx.clone(), 18, RULE_assign_stmt);
        let mut _localctx: Rc<Assign_stmtContextAll> = _localctx;
		let result: Result<(), ANTLRError> = (|| {

			//recog.base.enter_outer_alt(_localctx.clone(), 1);
			recog.base.enter_outer_alt(None, 1);
			{
			/*InvokeRule assignlist*/
			recog.base.set_state(141);
			recog.assignlist()?;

			/*InvokeRule block_stmt*/
			recog.base.set_state(142);
			recog.block_stmt()?;

			recog.base.set_state(143);
			recog.base.match_token(SEMICOLON,&mut recog.err_handler)?;

			}
			Ok(())
		})();
		match result {
		Ok(_)=>{},
        Err(e @ ANTLRError::FallThrough(_)) => return Err(e),
		Err(ref re) => {
				//_localctx.exception = re;
				recog.err_handler.report_error(&mut recog.base, re);
				recog.err_handler.recover(&mut recog.base, re)?;
			}
		}
		recog.base.exit_rule();

		Ok(_localctx)
	}
}
//------------------- modify_stmt ----------------
pub type Modify_stmtContextAll<'input> = Modify_stmtContext<'input>;


pub type Modify_stmtContext<'input> = BaseParserRuleContext<'input,Modify_stmtContextExt<'input>>;

#[derive(Clone)]
pub struct Modify_stmtContextExt<'input>{
ph:PhantomData<&'input str>
}

impl<'input> ailParserContext<'input> for Modify_stmtContext<'input>{}

impl<'input,'a> Listenable<dyn ailListener<'input> + 'a> for Modify_stmtContext<'input>{
		fn enter(&self,listener: &mut (dyn ailListener<'input> + 'a)) {
			listener.enter_every_rule(self);
			listener.enter_modify_stmt(self);
		}
		fn exit(&self,listener: &mut (dyn ailListener<'input> + 'a)) {
			listener.exit_modify_stmt(self);
			listener.exit_every_rule(self);
		}
}

impl<'input,'a> Visitable<dyn ailVisitor<'input> + 'a> for Modify_stmtContext<'input>{
	fn accept(&self,visitor: &mut (dyn ailVisitor<'input> + 'a)) {
		visitor.visit_modify_stmt(self);
	}
}

impl<'input> CustomRuleContext<'input> for Modify_stmtContextExt<'input>{
	type TF = LocalTokenFactory<'input>;
	type Ctx = ailParserContextType;
	fn get_rule_index(&self) -> usize { RULE_modify_stmt }
	//fn type_rule_index() -> usize where Self: Sized { RULE_modify_stmt }
}
antlr_rust::tid!{Modify_stmtContextExt<'a>}

impl<'input> Modify_stmtContextExt<'input>{
	fn new(parent: Option<Rc<dyn ailParserContext<'input> + 'input > >, invoking_state: isize) -> Rc<Modify_stmtContextAll<'input>> {
		Rc::new(
			BaseParserRuleContext::new_parser_ctx(parent, invoking_state,Modify_stmtContextExt{
				ph:PhantomData
			}),
		)
	}
}

pub trait Modify_stmtContextAttrs<'input>: ailParserContext<'input> + BorrowMut<Modify_stmtContextExt<'input>>{

fn lvalue(&self) -> Option<Rc<LvalueContextAll<'input>>> where Self:Sized{
	self.child_of_type(0)
}
/// Retrieves first TerminalNode corresponding to token EQUAL
/// Returns `None` if there is no child corresponding to token EQUAL
fn EQUAL(&self) -> Option<Rc<TerminalNode<'input,ailParserContextType>>> where Self:Sized{
	self.get_token(EQUAL, 0)
}
fn expr(&self) -> Option<Rc<ExprContextAll<'input>>> where Self:Sized{
	self.child_of_type(0)
}
/// Retrieves first TerminalNode corresponding to token SEMICOLON
/// Returns `None` if there is no child corresponding to token SEMICOLON
fn SEMICOLON(&self) -> Option<Rc<TerminalNode<'input,ailParserContextType>>> where Self:Sized{
	self.get_token(SEMICOLON, 0)
}

}

impl<'input> Modify_stmtContextAttrs<'input> for Modify_stmtContext<'input>{}

impl<'input, I, H> ailParser<'input, I, H>
where
    I: TokenStream<'input, TF = LocalTokenFactory<'input> > + TidAble<'input>,
    H: ErrorStrategy<'input,BaseParserType<'input,I>>
{
	pub fn modify_stmt(&mut self,)
	-> Result<Rc<Modify_stmtContextAll<'input>>,ANTLRError> {
		let mut recog = self;
		let _parentctx = recog.ctx.take();
		let mut _localctx = Modify_stmtContextExt::new(_parentctx.clone(), recog.base.get_state());
        recog.base.enter_rule(_localctx.clone(), 20, RULE_modify_stmt);
        let mut _localctx: Rc<Modify_stmtContextAll> = _localctx;
		let result: Result<(), ANTLRError> = (|| {

			//recog.base.enter_outer_alt(_localctx.clone(), 1);
			recog.base.enter_outer_alt(None, 1);
			{
			/*InvokeRule lvalue*/
			recog.base.set_state(145);
			recog.lvalue()?;

			recog.base.set_state(146);
			recog.base.match_token(EQUAL,&mut recog.err_handler)?;

			/*InvokeRule expr*/
			recog.base.set_state(147);
			recog.expr_rec(0)?;

			recog.base.set_state(148);
			recog.base.match_token(SEMICOLON,&mut recog.err_handler)?;

			}
			Ok(())
		})();
		match result {
		Ok(_)=>{},
        Err(e @ ANTLRError::FallThrough(_)) => return Err(e),
		Err(ref re) => {
				//_localctx.exception = re;
				recog.err_handler.report_error(&mut recog.base, re);
				recog.err_handler.recover(&mut recog.base, re)?;
			}
		}
		recog.base.exit_rule();

		Ok(_localctx)
	}
}
//------------------- unary_op ----------------
pub type Unary_opContextAll<'input> = Unary_opContext<'input>;


pub type Unary_opContext<'input> = BaseParserRuleContext<'input,Unary_opContextExt<'input>>;

#[derive(Clone)]
pub struct Unary_opContextExt<'input>{
ph:PhantomData<&'input str>
}

impl<'input> ailParserContext<'input> for Unary_opContext<'input>{}

impl<'input,'a> Listenable<dyn ailListener<'input> + 'a> for Unary_opContext<'input>{
		fn enter(&self,listener: &mut (dyn ailListener<'input> + 'a)) {
			listener.enter_every_rule(self);
			listener.enter_unary_op(self);
		}
		fn exit(&self,listener: &mut (dyn ailListener<'input> + 'a)) {
			listener.exit_unary_op(self);
			listener.exit_every_rule(self);
		}
}

impl<'input,'a> Visitable<dyn ailVisitor<'input> + 'a> for Unary_opContext<'input>{
	fn accept(&self,visitor: &mut (dyn ailVisitor<'input> + 'a)) {
		visitor.visit_unary_op(self);
	}
}

impl<'input> CustomRuleContext<'input> for Unary_opContextExt<'input>{
	type TF = LocalTokenFactory<'input>;
	type Ctx = ailParserContextType;
	fn get_rule_index(&self) -> usize { RULE_unary_op }
	//fn type_rule_index() -> usize where Self: Sized { RULE_unary_op }
}
antlr_rust::tid!{Unary_opContextExt<'a>}

impl<'input> Unary_opContextExt<'input>{
	fn new(parent: Option<Rc<dyn ailParserContext<'input> + 'input > >, invoking_state: isize) -> Rc<Unary_opContextAll<'input>> {
		Rc::new(
			BaseParserRuleContext::new_parser_ctx(parent, invoking_state,Unary_opContextExt{
				ph:PhantomData
			}),
		)
	}
}

pub trait Unary_opContextAttrs<'input>: ailParserContext<'input> + BorrowMut<Unary_opContextExt<'input>>{

/// Retrieves first TerminalNode corresponding to token PLUS
/// Returns `None` if there is no child corresponding to token PLUS
fn PLUS(&self) -> Option<Rc<TerminalNode<'input,ailParserContextType>>> where Self:Sized{
	self.get_token(PLUS, 0)
}
/// Retrieves first TerminalNode corresponding to token MINUS
/// Returns `None` if there is no child corresponding to token MINUS
fn MINUS(&self) -> Option<Rc<TerminalNode<'input,ailParserContextType>>> where Self:Sized{
	self.get_token(MINUS, 0)
}
/// Retrieves first TerminalNode corresponding to token EXCLAMATION
/// Returns `None` if there is no child corresponding to token EXCLAMATION
fn EXCLAMATION(&self) -> Option<Rc<TerminalNode<'input,ailParserContextType>>> where Self:Sized{
	self.get_token(EXCLAMATION, 0)
}

}

impl<'input> Unary_opContextAttrs<'input> for Unary_opContext<'input>{}

impl<'input, I, H> ailParser<'input, I, H>
where
    I: TokenStream<'input, TF = LocalTokenFactory<'input> > + TidAble<'input>,
    H: ErrorStrategy<'input,BaseParserType<'input,I>>
{
	pub fn unary_op(&mut self,)
	-> Result<Rc<Unary_opContextAll<'input>>,ANTLRError> {
		let mut recog = self;
		let _parentctx = recog.ctx.take();
		let mut _localctx = Unary_opContextExt::new(_parentctx.clone(), recog.base.get_state());
        recog.base.enter_rule(_localctx.clone(), 22, RULE_unary_op);
        let mut _localctx: Rc<Unary_opContextAll> = _localctx;
		let mut _la: isize = -1;
		let result: Result<(), ANTLRError> = (|| {

			//recog.base.enter_outer_alt(_localctx.clone(), 1);
			recog.base.enter_outer_alt(None, 1);
			{
			recog.base.set_state(150);
			_la = recog.base.input.la(1);
			if { !(((((_la - 37)) & !0x3f) == 0 && ((1usize << (_la - 37)) & ((1usize << (EXCLAMATION - 37)) | (1usize << (MINUS - 37)) | (1usize << (PLUS - 37)))) != 0)) } {
				recog.err_handler.recover_inline(&mut recog.base)?;

			}
			else {
				if  recog.base.input.la(1)==TOKEN_EOF { recog.base.matched_eof = true };
				recog.err_handler.report_match(&mut recog.base);
				recog.base.consume(&mut recog.err_handler);
			}
			}
			Ok(())
		})();
		match result {
		Ok(_)=>{},
        Err(e @ ANTLRError::FallThrough(_)) => return Err(e),
		Err(ref re) => {
				//_localctx.exception = re;
				recog.err_handler.report_error(&mut recog.base, re);
				recog.err_handler.recover(&mut recog.base, re)?;
			}
		}
		recog.base.exit_rule();

		Ok(_localctx)
	}
}
//------------------- binary_op ----------------
pub type Binary_opContextAll<'input> = Binary_opContext<'input>;


pub type Binary_opContext<'input> = BaseParserRuleContext<'input,Binary_opContextExt<'input>>;

#[derive(Clone)]
pub struct Binary_opContextExt<'input>{
ph:PhantomData<&'input str>
}

impl<'input> ailParserContext<'input> for Binary_opContext<'input>{}

impl<'input,'a> Listenable<dyn ailListener<'input> + 'a> for Binary_opContext<'input>{
		fn enter(&self,listener: &mut (dyn ailListener<'input> + 'a)) {
			listener.enter_every_rule(self);
			listener.enter_binary_op(self);
		}
		fn exit(&self,listener: &mut (dyn ailListener<'input> + 'a)) {
			listener.exit_binary_op(self);
			listener.exit_every_rule(self);
		}
}

impl<'input,'a> Visitable<dyn ailVisitor<'input> + 'a> for Binary_opContext<'input>{
	fn accept(&self,visitor: &mut (dyn ailVisitor<'input> + 'a)) {
		visitor.visit_binary_op(self);
	}
}

impl<'input> CustomRuleContext<'input> for Binary_opContextExt<'input>{
	type TF = LocalTokenFactory<'input>;
	type Ctx = ailParserContextType;
	fn get_rule_index(&self) -> usize { RULE_binary_op }
	//fn type_rule_index() -> usize where Self: Sized { RULE_binary_op }
}
antlr_rust::tid!{Binary_opContextExt<'a>}

impl<'input> Binary_opContextExt<'input>{
	fn new(parent: Option<Rc<dyn ailParserContext<'input> + 'input > >, invoking_state: isize) -> Rc<Binary_opContextAll<'input>> {
		Rc::new(
			BaseParserRuleContext::new_parser_ctx(parent, invoking_state,Binary_opContextExt{
				ph:PhantomData
			}),
		)
	}
}

pub trait Binary_opContextAttrs<'input>: ailParserContext<'input> + BorrowMut<Binary_opContextExt<'input>>{

/// Retrieves first TerminalNode corresponding to token STAR
/// Returns `None` if there is no child corresponding to token STAR
fn STAR(&self) -> Option<Rc<TerminalNode<'input,ailParserContextType>>> where Self:Sized{
	self.get_token(STAR, 0)
}
/// Retrieves first TerminalNode corresponding to token SLASH
/// Returns `None` if there is no child corresponding to token SLASH
fn SLASH(&self) -> Option<Rc<TerminalNode<'input,ailParserContextType>>> where Self:Sized{
	self.get_token(SLASH, 0)
}
/// Retrieves first TerminalNode corresponding to token PERCENT
/// Returns `None` if there is no child corresponding to token PERCENT
fn PERCENT(&self) -> Option<Rc<TerminalNode<'input,ailParserContextType>>> where Self:Sized{
	self.get_token(PERCENT, 0)
}
/// Retrieves first TerminalNode corresponding to token PLUS
/// Returns `None` if there is no child corresponding to token PLUS
fn PLUS(&self) -> Option<Rc<TerminalNode<'input,ailParserContextType>>> where Self:Sized{
	self.get_token(PLUS, 0)
}
/// Retrieves first TerminalNode corresponding to token MINUS
/// Returns `None` if there is no child corresponding to token MINUS
fn MINUS(&self) -> Option<Rc<TerminalNode<'input,ailParserContextType>>> where Self:Sized{
	self.get_token(MINUS, 0)
}
/// Retrieves first TerminalNode corresponding to token AND
/// Returns `None` if there is no child corresponding to token AND
fn AND(&self) -> Option<Rc<TerminalNode<'input,ailParserContextType>>> where Self:Sized{
	self.get_token(AND, 0)
}
/// Retrieves first TerminalNode corresponding to token CARET
/// Returns `None` if there is no child corresponding to token CARET
fn CARET(&self) -> Option<Rc<TerminalNode<'input,ailParserContextType>>> where Self:Sized{
	self.get_token(CARET, 0)
}
/// Retrieves first TerminalNode corresponding to token PIPE
/// Returns `None` if there is no child corresponding to token PIPE
fn PIPE(&self) -> Option<Rc<TerminalNode<'input,ailParserContextType>>> where Self:Sized{
	self.get_token(PIPE, 0)
}

}

impl<'input> Binary_opContextAttrs<'input> for Binary_opContext<'input>{}

impl<'input, I, H> ailParser<'input, I, H>
where
    I: TokenStream<'input, TF = LocalTokenFactory<'input> > + TidAble<'input>,
    H: ErrorStrategy<'input,BaseParserType<'input,I>>
{
	pub fn binary_op(&mut self,)
	-> Result<Rc<Binary_opContextAll<'input>>,ANTLRError> {
		let mut recog = self;
		let _parentctx = recog.ctx.take();
		let mut _localctx = Binary_opContextExt::new(_parentctx.clone(), recog.base.get_state());
        recog.base.enter_rule(_localctx.clone(), 24, RULE_binary_op);
        let mut _localctx: Rc<Binary_opContextAll> = _localctx;
		let mut _la: isize = -1;
		let result: Result<(), ANTLRError> = (|| {

			//recog.base.enter_outer_alt(_localctx.clone(), 1);
			recog.base.enter_outer_alt(None, 1);
			{
			recog.base.set_state(152);
			_la = recog.base.input.la(1);
			if { !((((_la) & !0x3f) == 0 && ((1usize << _la) & ((1usize << T__0) | (1usize << T__1) | (1usize << T__2) | (1usize << T__3) | (1usize << T__4) | (1usize << T__5) | (1usize << T__6) | (1usize << T__7) | (1usize << AND) | (1usize << CARET))) != 0) || ((((_la - 42)) & !0x3f) == 0 && ((1usize << (_la - 42)) & ((1usize << (MINUS - 42)) | (1usize << (PERCENT - 42)) | (1usize << (PIPE - 42)) | (1usize << (PLUS - 42)) | (1usize << (SLASH - 42)) | (1usize << (STAR - 42)))) != 0)) } {
				recog.err_handler.recover_inline(&mut recog.base)?;

			}
			else {
				if  recog.base.input.la(1)==TOKEN_EOF { recog.base.matched_eof = true };
				recog.err_handler.report_match(&mut recog.base);
				recog.base.consume(&mut recog.err_handler);
			}
			}
			Ok(())
		})();
		match result {
		Ok(_)=>{},
        Err(e @ ANTLRError::FallThrough(_)) => return Err(e),
		Err(ref re) => {
				//_localctx.exception = re;
				recog.err_handler.report_error(&mut recog.base, re);
				recog.err_handler.recover(&mut recog.base, re)?;
			}
		}
		recog.base.exit_rule();

		Ok(_localctx)
	}
}
//------------------- expr ----------------
pub type ExprContextAll<'input> = ExprContext<'input>;


pub type ExprContext<'input> = BaseParserRuleContext<'input,ExprContextExt<'input>>;

#[derive(Clone)]
pub struct ExprContextExt<'input>{
ph:PhantomData<&'input str>
}

impl<'input> ailParserContext<'input> for ExprContext<'input>{}

impl<'input,'a> Listenable<dyn ailListener<'input> + 'a> for ExprContext<'input>{
		fn enter(&self,listener: &mut (dyn ailListener<'input> + 'a)) {
			listener.enter_every_rule(self);
			listener.enter_expr(self);
		}
		fn exit(&self,listener: &mut (dyn ailListener<'input> + 'a)) {
			listener.exit_expr(self);
			listener.exit_every_rule(self);
		}
}

impl<'input,'a> Visitable<dyn ailVisitor<'input> + 'a> for ExprContext<'input>{
	fn accept(&self,visitor: &mut (dyn ailVisitor<'input> + 'a)) {
		visitor.visit_expr(self);
	}
}

impl<'input> CustomRuleContext<'input> for ExprContextExt<'input>{
	type TF = LocalTokenFactory<'input>;
	type Ctx = ailParserContextType;
	fn get_rule_index(&self) -> usize { RULE_expr }
	//fn type_rule_index() -> usize where Self: Sized { RULE_expr }
}
antlr_rust::tid!{ExprContextExt<'a>}

impl<'input> ExprContextExt<'input>{
	fn new(parent: Option<Rc<dyn ailParserContext<'input> + 'input > >, invoking_state: isize) -> Rc<ExprContextAll<'input>> {
		Rc::new(
			BaseParserRuleContext::new_parser_ctx(parent, invoking_state,ExprContextExt{
				ph:PhantomData
			}),
		)
	}
}

pub trait ExprContextAttrs<'input>: ailParserContext<'input> + BorrowMut<ExprContextExt<'input>>{

/// Retrieves first TerminalNode corresponding to token LPAREN
/// Returns `None` if there is no child corresponding to token LPAREN
fn LPAREN(&self) -> Option<Rc<TerminalNode<'input,ailParserContextType>>> where Self:Sized{
	self.get_token(LPAREN, 0)
}
fn expr_all(&self) ->  Vec<Rc<ExprContextAll<'input>>> where Self:Sized{
	self.children_of_type()
}
fn expr(&self, i: usize) -> Option<Rc<ExprContextAll<'input>>> where Self:Sized{
	self.child_of_type(i)
}
/// Retrieves first TerminalNode corresponding to token RPAREN
/// Returns `None` if there is no child corresponding to token RPAREN
fn RPAREN(&self) -> Option<Rc<TerminalNode<'input,ailParserContextType>>> where Self:Sized{
	self.get_token(RPAREN, 0)
}
fn unary_op(&self) -> Option<Rc<Unary_opContextAll<'input>>> where Self:Sized{
	self.child_of_type(0)
}
fn block_expr(&self) -> Option<Rc<Block_exprContextAll<'input>>> where Self:Sized{
	self.child_of_type(0)
}
fn call_expr(&self) -> Option<Rc<Call_exprContextAll<'input>>> where Self:Sized{
	self.child_of_type(0)
}
fn assign_expr(&self) -> Option<Rc<Assign_exprContextAll<'input>>> where Self:Sized{
	self.child_of_type(0)
}
/// Retrieves all `TerminalNode`s corresponding to token COMMA in current rule
fn COMMA_all(&self) -> Vec<Rc<TerminalNode<'input,ailParserContextType>>>  where Self:Sized{
	self.children_of_type()
}
/// Retrieves 'i's TerminalNode corresponding to token COMMA, starting from 0.
/// Returns `None` if number of children corresponding to token COMMA is less or equal than `i`.
fn COMMA(&self, i: usize) -> Option<Rc<TerminalNode<'input,ailParserContextType>>> where Self:Sized{
	self.get_token(COMMA, i)
}
/// Retrieves first TerminalNode corresponding to token LBRACKET
/// Returns `None` if there is no child corresponding to token LBRACKET
fn LBRACKET(&self) -> Option<Rc<TerminalNode<'input,ailParserContextType>>> where Self:Sized{
	self.get_token(LBRACKET, 0)
}
/// Retrieves first TerminalNode corresponding to token RBRACKET
/// Returns `None` if there is no child corresponding to token RBRACKET
fn RBRACKET(&self) -> Option<Rc<TerminalNode<'input,ailParserContextType>>> where Self:Sized{
	self.get_token(RBRACKET, 0)
}
/// Retrieves first TerminalNode corresponding to token LBRACE
/// Returns `None` if there is no child corresponding to token LBRACE
fn LBRACE(&self) -> Option<Rc<TerminalNode<'input,ailParserContextType>>> where Self:Sized{
	self.get_token(LBRACE, 0)
}
/// Retrieves first TerminalNode corresponding to token RBRACE
/// Returns `None` if there is no child corresponding to token RBRACE
fn RBRACE(&self) -> Option<Rc<TerminalNode<'input,ailParserContextType>>> where Self:Sized{
	self.get_token(RBRACE, 0)
}
/// Retrieves all `TerminalNode`s corresponding to token COLON in current rule
fn COLON_all(&self) -> Vec<Rc<TerminalNode<'input,ailParserContextType>>>  where Self:Sized{
	self.children_of_type()
}
/// Retrieves 'i's TerminalNode corresponding to token COLON, starting from 0.
/// Returns `None` if number of children corresponding to token COLON is less or equal than `i`.
fn COLON(&self, i: usize) -> Option<Rc<TerminalNode<'input,ailParserContextType>>> where Self:Sized{
	self.get_token(COLON, i)
}
fn rvalue(&self) -> Option<Rc<RvalueContextAll<'input>>> where Self:Sized{
	self.child_of_type(0)
}
/// Retrieves first TerminalNode corresponding to token COLOR
/// Returns `None` if there is no child corresponding to token COLOR
fn COLOR(&self) -> Option<Rc<TerminalNode<'input,ailParserContextType>>> where Self:Sized{
	self.get_token(COLOR, 0)
}
/// Retrieves first TerminalNode corresponding to token STRING
/// Returns `None` if there is no child corresponding to token STRING
fn STRING(&self) -> Option<Rc<TerminalNode<'input,ailParserContextType>>> where Self:Sized{
	self.get_token(STRING, 0)
}
/// Retrieves first TerminalNode corresponding to token NUMBER
/// Returns `None` if there is no child corresponding to token NUMBER
fn NUMBER(&self) -> Option<Rc<TerminalNode<'input,ailParserContextType>>> where Self:Sized{
	self.get_token(NUMBER, 0)
}
/// Retrieves first TerminalNode corresponding to token MINUS
/// Returns `None` if there is no child corresponding to token MINUS
fn MINUS(&self) -> Option<Rc<TerminalNode<'input,ailParserContextType>>> where Self:Sized{
	self.get_token(MINUS, 0)
}
/// Retrieves first TerminalNode corresponding to token BOOL
/// Returns `None` if there is no child corresponding to token BOOL
fn BOOL(&self) -> Option<Rc<TerminalNode<'input,ailParserContextType>>> where Self:Sized{
	self.get_token(BOOL, 0)
}
fn binary_op(&self) -> Option<Rc<Binary_opContextAll<'input>>> where Self:Sized{
	self.child_of_type(0)
}

}

impl<'input> ExprContextAttrs<'input> for ExprContext<'input>{}

impl<'input, I, H> ailParser<'input, I, H>
where
    I: TokenStream<'input, TF = LocalTokenFactory<'input> > + TidAble<'input>,
    H: ErrorStrategy<'input,BaseParserType<'input,I>>
{
	pub fn  expr(&mut self,)
	-> Result<Rc<ExprContextAll<'input>>,ANTLRError> {
		self.expr_rec(0)
	}

	fn expr_rec(&mut self, _p: isize)
	-> Result<Rc<ExprContextAll<'input>>,ANTLRError> {
		let recog = self;
		let _parentctx = recog.ctx.take();
		let _parentState = recog.base.get_state();
		let mut _localctx = ExprContextExt::new(_parentctx.clone(), recog.base.get_state());
		recog.base.enter_recursion_rule(_localctx.clone(), 26, RULE_expr, _p);
	    let mut _localctx: Rc<ExprContextAll> = _localctx;
        let mut _prevctx = _localctx.clone();
		let _startState = 26;
		let mut _la: isize = -1;
		let result: Result<(), ANTLRError> = (|| {
			let mut _alt: isize;
			//recog.base.enter_outer_alt(_localctx.clone(), 1);
			recog.base.enter_outer_alt(None, 1);
			{
			recog.base.set_state(223);
			recog.err_handler.sync(&mut recog.base)?;
			match  recog.interpreter.adaptive_predict(17,&mut recog.base)? {
				1 =>{
					{
					recog.base.set_state(155);
					recog.base.match_token(LPAREN,&mut recog.err_handler)?;

					/*InvokeRule expr*/
					recog.base.set_state(156);
					recog.expr_rec(0)?;

					recog.base.set_state(157);
					recog.base.match_token(RPAREN,&mut recog.err_handler)?;

					}
				}
			,
				2 =>{
					{
					/*InvokeRule unary_op*/
					recog.base.set_state(159);
					recog.unary_op()?;

					/*InvokeRule expr*/
					recog.base.set_state(160);
					recog.expr_rec(13)?;

					}
				}
			,
				3 =>{
					{
					/*InvokeRule block_expr*/
					recog.base.set_state(162);
					recog.block_expr()?;

					}
				}
			,
				4 =>{
					{
					/*InvokeRule call_expr*/
					recog.base.set_state(163);
					recog.call_expr()?;

					}
				}
			,
				5 =>{
					{
					/*InvokeRule assign_expr*/
					recog.base.set_state(164);
					recog.assign_expr()?;

					}
				}
			,
				6 =>{
					{
					recog.base.set_state(165);
					recog.base.match_token(T__8,&mut recog.err_handler)?;

					/*InvokeRule expr*/
					recog.base.set_state(166);
					recog.expr_rec(0)?;

					recog.base.set_state(167);
					recog.base.match_token(COMMA,&mut recog.err_handler)?;

					/*InvokeRule expr*/
					recog.base.set_state(168);
					recog.expr_rec(0)?;

					recog.base.set_state(169);
					recog.base.match_token(COMMA,&mut recog.err_handler)?;

					/*InvokeRule expr*/
					recog.base.set_state(170);
					recog.expr_rec(0)?;

					recog.base.set_state(175);
					recog.err_handler.sync(&mut recog.base)?;
					_la = recog.base.input.la(1);
					if _la==COMMA {
						{
						recog.base.set_state(171);
						recog.base.match_token(COMMA,&mut recog.err_handler)?;

						recog.base.set_state(173);
						recog.err_handler.sync(&mut recog.base)?;
						_la = recog.base.input.la(1);
						if (((_la) & !0x3f) == 0 && ((1usize << _la) & ((1usize << T__8) | (1usize << STRING) | (1usize << COLOR) | (1usize << NUMBER) | (1usize << BOOL) | (1usize << GLOBAL) | (1usize << LET))) != 0) || ((((_la - 34)) & !0x3f) == 0 && ((1usize << (_la - 34)) & ((1usize << (DOLLAR - 34)) | (1usize << (EXCLAMATION - 34)) | (1usize << (LBRACE - 34)) | (1usize << (LBRACKET - 34)) | (1usize << (LPAREN - 34)) | (1usize << (MINUS - 34)) | (1usize << (PLUS - 34)) | (1usize << (IDENT - 34)))) != 0) {
							{
							/*InvokeRule expr*/
							recog.base.set_state(172);
							recog.expr_rec(0)?;

							}
						}

						}
					}

					recog.base.set_state(177);
					recog.base.match_token(RPAREN,&mut recog.err_handler)?;

					}
				}
			,
				7 =>{
					{
					recog.base.set_state(179);
					recog.base.match_token(LBRACKET,&mut recog.err_handler)?;

					recog.base.set_state(185);
					recog.err_handler.sync(&mut recog.base)?;
					_alt = recog.interpreter.adaptive_predict(10,&mut recog.base)?;
					while { _alt!=2 && _alt!=INVALID_ALT } {
						if _alt==1 {
							{
							{
							/*InvokeRule expr*/
							recog.base.set_state(180);
							recog.expr_rec(0)?;

							recog.base.set_state(181);
							recog.base.match_token(COMMA,&mut recog.err_handler)?;

							}
							} 
						}
						recog.base.set_state(187);
						recog.err_handler.sync(&mut recog.base)?;
						_alt = recog.interpreter.adaptive_predict(10,&mut recog.base)?;
					}
					recog.base.set_state(192);
					recog.err_handler.sync(&mut recog.base)?;
					_la = recog.base.input.la(1);
					if (((_la) & !0x3f) == 0 && ((1usize << _la) & ((1usize << T__8) | (1usize << STRING) | (1usize << COLOR) | (1usize << NUMBER) | (1usize << BOOL) | (1usize << GLOBAL) | (1usize << LET))) != 0) || ((((_la - 34)) & !0x3f) == 0 && ((1usize << (_la - 34)) & ((1usize << (DOLLAR - 34)) | (1usize << (EXCLAMATION - 34)) | (1usize << (LBRACE - 34)) | (1usize << (LBRACKET - 34)) | (1usize << (LPAREN - 34)) | (1usize << (MINUS - 34)) | (1usize << (PLUS - 34)) | (1usize << (IDENT - 34)))) != 0) {
						{
						/*InvokeRule expr*/
						recog.base.set_state(188);
						recog.expr_rec(0)?;

						recog.base.set_state(190);
						recog.err_handler.sync(&mut recog.base)?;
						_la = recog.base.input.la(1);
						if _la==COMMA {
							{
							recog.base.set_state(189);
							recog.base.match_token(COMMA,&mut recog.err_handler)?;

							}
						}

						}
					}

					recog.base.set_state(194);
					recog.base.match_token(RBRACKET,&mut recog.err_handler)?;

					}
				}
			,
				8 =>{
					{
					recog.base.set_state(195);
					recog.base.match_token(LBRACE,&mut recog.err_handler)?;

					recog.base.set_state(203);
					recog.err_handler.sync(&mut recog.base)?;
					_alt = recog.interpreter.adaptive_predict(13,&mut recog.base)?;
					while { _alt!=2 && _alt!=INVALID_ALT } {
						if _alt==1 {
							{
							{
							/*InvokeRule expr*/
							recog.base.set_state(196);
							recog.expr_rec(0)?;

							recog.base.set_state(197);
							recog.base.match_token(COLON,&mut recog.err_handler)?;

							/*InvokeRule expr*/
							recog.base.set_state(198);
							recog.expr_rec(0)?;

							recog.base.set_state(199);
							recog.base.match_token(COMMA,&mut recog.err_handler)?;

							}
							} 
						}
						recog.base.set_state(205);
						recog.err_handler.sync(&mut recog.base)?;
						_alt = recog.interpreter.adaptive_predict(13,&mut recog.base)?;
					}
					recog.base.set_state(212);
					recog.err_handler.sync(&mut recog.base)?;
					_la = recog.base.input.la(1);
					if (((_la) & !0x3f) == 0 && ((1usize << _la) & ((1usize << T__8) | (1usize << STRING) | (1usize << COLOR) | (1usize << NUMBER) | (1usize << BOOL) | (1usize << GLOBAL) | (1usize << LET))) != 0) || ((((_la - 34)) & !0x3f) == 0 && ((1usize << (_la - 34)) & ((1usize << (DOLLAR - 34)) | (1usize << (EXCLAMATION - 34)) | (1usize << (LBRACE - 34)) | (1usize << (LBRACKET - 34)) | (1usize << (LPAREN - 34)) | (1usize << (MINUS - 34)) | (1usize << (PLUS - 34)) | (1usize << (IDENT - 34)))) != 0) {
						{
						/*InvokeRule expr*/
						recog.base.set_state(206);
						recog.expr_rec(0)?;

						recog.base.set_state(207);
						recog.base.match_token(COLON,&mut recog.err_handler)?;

						/*InvokeRule expr*/
						recog.base.set_state(208);
						recog.expr_rec(0)?;

						recog.base.set_state(210);
						recog.err_handler.sync(&mut recog.base)?;
						_la = recog.base.input.la(1);
						if _la==COMMA {
							{
							recog.base.set_state(209);
							recog.base.match_token(COMMA,&mut recog.err_handler)?;

							}
						}

						}
					}

					recog.base.set_state(214);
					recog.base.match_token(RBRACE,&mut recog.err_handler)?;

					}
				}
			,
				9 =>{
					{
					/*InvokeRule rvalue*/
					recog.base.set_state(215);
					recog.rvalue()?;

					}
				}
			,
				10 =>{
					{
					recog.base.set_state(216);
					recog.base.match_token(COLOR,&mut recog.err_handler)?;

					}
				}
			,
				11 =>{
					{
					recog.base.set_state(217);
					recog.base.match_token(STRING,&mut recog.err_handler)?;

					}
				}
			,
				12 =>{
					{
					recog.base.set_state(219);
					recog.err_handler.sync(&mut recog.base)?;
					_la = recog.base.input.la(1);
					if _la==MINUS {
						{
						recog.base.set_state(218);
						recog.base.match_token(MINUS,&mut recog.err_handler)?;

						}
					}

					recog.base.set_state(221);
					recog.base.match_token(NUMBER,&mut recog.err_handler)?;

					}
				}
			,
				13 =>{
					{
					recog.base.set_state(222);
					recog.base.match_token(BOOL,&mut recog.err_handler)?;

					}
				}

				_ => {}
			}

			let tmp = recog.input.lt(-1).cloned();
			recog.ctx.as_ref().unwrap().set_stop(tmp);
			recog.base.set_state(231);
			recog.err_handler.sync(&mut recog.base)?;
			_alt = recog.interpreter.adaptive_predict(18,&mut recog.base)?;
			while { _alt!=2 && _alt!=INVALID_ALT } {
				if _alt==1 {
					recog.trigger_exit_rule_event();
					_prevctx = _localctx.clone();
					{
					{
					/*recRuleAltStartAction*/
					let mut tmp = ExprContextExt::new(_parentctx.clone(), _parentState);
					recog.push_new_recursion_context(tmp.clone(), _startState, RULE_expr);
					_localctx = tmp;
					recog.base.set_state(225);
					if !({recog.precpred(None, 12)}) {
						Err(FailedPredicateError::new(&mut recog.base, Some("recog.precpred(None, 12)".to_owned()), None))?;
					}
					/*InvokeRule binary_op*/
					recog.base.set_state(226);
					recog.binary_op()?;

					/*InvokeRule expr*/
					recog.base.set_state(227);
					recog.expr_rec(13)?;

					}
					} 
				}
				recog.base.set_state(233);
				recog.err_handler.sync(&mut recog.base)?;
				_alt = recog.interpreter.adaptive_predict(18,&mut recog.base)?;
			}
			}
			Ok(())
		})();
		match result {
		Ok(_) => {},
        Err(e @ ANTLRError::FallThrough(_)) => return Err(e),
		Err(ref re)=>{
			//_localctx.exception = re;
			recog.err_handler.report_error(&mut recog.base, re);
	        recog.err_handler.recover(&mut recog.base, re)?;}
		}
		recog.base.unroll_recursion_context(_parentctx);

		Ok(_localctx)
	}
}
//------------------- rvalue ----------------
pub type RvalueContextAll<'input> = RvalueContext<'input>;


pub type RvalueContext<'input> = BaseParserRuleContext<'input,RvalueContextExt<'input>>;

#[derive(Clone)]
pub struct RvalueContextExt<'input>{
ph:PhantomData<&'input str>
}

impl<'input> ailParserContext<'input> for RvalueContext<'input>{}

impl<'input,'a> Listenable<dyn ailListener<'input> + 'a> for RvalueContext<'input>{
		fn enter(&self,listener: &mut (dyn ailListener<'input> + 'a)) {
			listener.enter_every_rule(self);
			listener.enter_rvalue(self);
		}
		fn exit(&self,listener: &mut (dyn ailListener<'input> + 'a)) {
			listener.exit_rvalue(self);
			listener.exit_every_rule(self);
		}
}

impl<'input,'a> Visitable<dyn ailVisitor<'input> + 'a> for RvalueContext<'input>{
	fn accept(&self,visitor: &mut (dyn ailVisitor<'input> + 'a)) {
		visitor.visit_rvalue(self);
	}
}

impl<'input> CustomRuleContext<'input> for RvalueContextExt<'input>{
	type TF = LocalTokenFactory<'input>;
	type Ctx = ailParserContextType;
	fn get_rule_index(&self) -> usize { RULE_rvalue }
	//fn type_rule_index() -> usize where Self: Sized { RULE_rvalue }
}
antlr_rust::tid!{RvalueContextExt<'a>}

impl<'input> RvalueContextExt<'input>{
	fn new(parent: Option<Rc<dyn ailParserContext<'input> + 'input > >, invoking_state: isize) -> Rc<RvalueContextAll<'input>> {
		Rc::new(
			BaseParserRuleContext::new_parser_ctx(parent, invoking_state,RvalueContextExt{
				ph:PhantomData
			}),
		)
	}
}

pub trait RvalueContextAttrs<'input>: ailParserContext<'input> + BorrowMut<RvalueContextExt<'input>>{

/// Retrieves first TerminalNode corresponding to token DOLLAR
/// Returns `None` if there is no child corresponding to token DOLLAR
fn DOLLAR(&self) -> Option<Rc<TerminalNode<'input,ailParserContextType>>> where Self:Sized{
	self.get_token(DOLLAR, 0)
}
/// Retrieves all `TerminalNode`s corresponding to token IDENT in current rule
fn IDENT_all(&self) -> Vec<Rc<TerminalNode<'input,ailParserContextType>>>  where Self:Sized{
	self.children_of_type()
}
/// Retrieves 'i's TerminalNode corresponding to token IDENT, starting from 0.
/// Returns `None` if number of children corresponding to token IDENT is less or equal than `i`.
fn IDENT(&self, i: usize) -> Option<Rc<TerminalNode<'input,ailParserContextType>>> where Self:Sized{
	self.get_token(IDENT, i)
}
/// Retrieves first TerminalNode corresponding to token COLON
/// Returns `None` if there is no child corresponding to token COLON
fn COLON(&self) -> Option<Rc<TerminalNode<'input,ailParserContextType>>> where Self:Sized{
	self.get_token(COLON, 0)
}
fn lvalue(&self) -> Option<Rc<LvalueContextAll<'input>>> where Self:Sized{
	self.child_of_type(0)
}

}

impl<'input> RvalueContextAttrs<'input> for RvalueContext<'input>{}

impl<'input, I, H> ailParser<'input, I, H>
where
    I: TokenStream<'input, TF = LocalTokenFactory<'input> > + TidAble<'input>,
    H: ErrorStrategy<'input,BaseParserType<'input,I>>
{
	pub fn rvalue(&mut self,)
	-> Result<Rc<RvalueContextAll<'input>>,ANTLRError> {
		let mut recog = self;
		let _parentctx = recog.ctx.take();
		let mut _localctx = RvalueContextExt::new(_parentctx.clone(), recog.base.get_state());
        recog.base.enter_rule(_localctx.clone(), 28, RULE_rvalue);
        let mut _localctx: Rc<RvalueContextAll> = _localctx;
		let result: Result<(), ANTLRError> = (|| {

			recog.base.set_state(241);
			recog.err_handler.sync(&mut recog.base)?;
			match  recog.interpreter.adaptive_predict(19,&mut recog.base)? {
				1 =>{
					//recog.base.enter_outer_alt(_localctx.clone(), 1);
					recog.base.enter_outer_alt(None, 1);
					{
					recog.base.set_state(234);
					recog.base.match_token(DOLLAR,&mut recog.err_handler)?;

					recog.base.set_state(235);
					recog.base.match_token(IDENT,&mut recog.err_handler)?;

					recog.base.set_state(236);
					recog.base.match_token(COLON,&mut recog.err_handler)?;

					recog.base.set_state(237);
					recog.base.match_token(IDENT,&mut recog.err_handler)?;

					}
				}
			,
				2 =>{
					//recog.base.enter_outer_alt(_localctx.clone(), 2);
					recog.base.enter_outer_alt(None, 2);
					{
					recog.base.set_state(238);
					recog.base.match_token(DOLLAR,&mut recog.err_handler)?;

					recog.base.set_state(239);
					recog.base.match_token(IDENT,&mut recog.err_handler)?;

					}
				}
			,
				3 =>{
					//recog.base.enter_outer_alt(_localctx.clone(), 3);
					recog.base.enter_outer_alt(None, 3);
					{
					/*InvokeRule lvalue*/
					recog.base.set_state(240);
					recog.lvalue()?;

					}
				}

				_ => {}
			}
			Ok(())
		})();
		match result {
		Ok(_)=>{},
        Err(e @ ANTLRError::FallThrough(_)) => return Err(e),
		Err(ref re) => {
				//_localctx.exception = re;
				recog.err_handler.report_error(&mut recog.base, re);
				recog.err_handler.recover(&mut recog.base, re)?;
			}
		}
		recog.base.exit_rule();

		Ok(_localctx)
	}
}
//------------------- lvalue ----------------
pub type LvalueContextAll<'input> = LvalueContext<'input>;


pub type LvalueContext<'input> = BaseParserRuleContext<'input,LvalueContextExt<'input>>;

#[derive(Clone)]
pub struct LvalueContextExt<'input>{
ph:PhantomData<&'input str>
}

impl<'input> ailParserContext<'input> for LvalueContext<'input>{}

impl<'input,'a> Listenable<dyn ailListener<'input> + 'a> for LvalueContext<'input>{
		fn enter(&self,listener: &mut (dyn ailListener<'input> + 'a)) {
			listener.enter_every_rule(self);
			listener.enter_lvalue(self);
		}
		fn exit(&self,listener: &mut (dyn ailListener<'input> + 'a)) {
			listener.exit_lvalue(self);
			listener.exit_every_rule(self);
		}
}

impl<'input,'a> Visitable<dyn ailVisitor<'input> + 'a> for LvalueContext<'input>{
	fn accept(&self,visitor: &mut (dyn ailVisitor<'input> + 'a)) {
		visitor.visit_lvalue(self);
	}
}

impl<'input> CustomRuleContext<'input> for LvalueContextExt<'input>{
	type TF = LocalTokenFactory<'input>;
	type Ctx = ailParserContextType;
	fn get_rule_index(&self) -> usize { RULE_lvalue }
	//fn type_rule_index() -> usize where Self: Sized { RULE_lvalue }
}
antlr_rust::tid!{LvalueContextExt<'a>}

impl<'input> LvalueContextExt<'input>{
	fn new(parent: Option<Rc<dyn ailParserContext<'input> + 'input > >, invoking_state: isize) -> Rc<LvalueContextAll<'input>> {
		Rc::new(
			BaseParserRuleContext::new_parser_ctx(parent, invoking_state,LvalueContextExt{
				ph:PhantomData
			}),
		)
	}
}

pub trait LvalueContextAttrs<'input>: ailParserContext<'input> + BorrowMut<LvalueContextExt<'input>>{

/// Retrieves all `TerminalNode`s corresponding to token IDENT in current rule
fn IDENT_all(&self) -> Vec<Rc<TerminalNode<'input,ailParserContextType>>>  where Self:Sized{
	self.children_of_type()
}
/// Retrieves 'i's TerminalNode corresponding to token IDENT, starting from 0.
/// Returns `None` if number of children corresponding to token IDENT is less or equal than `i`.
fn IDENT(&self, i: usize) -> Option<Rc<TerminalNode<'input,ailParserContextType>>> where Self:Sized{
	self.get_token(IDENT, i)
}
/// Retrieves first TerminalNode corresponding to token GLOBAL
/// Returns `None` if there is no child corresponding to token GLOBAL
fn GLOBAL(&self) -> Option<Rc<TerminalNode<'input,ailParserContextType>>> where Self:Sized{
	self.get_token(GLOBAL, 0)
}
/// Retrieves first TerminalNode corresponding to token DOLLAR
/// Returns `None` if there is no child corresponding to token DOLLAR
fn DOLLAR(&self) -> Option<Rc<TerminalNode<'input,ailParserContextType>>> where Self:Sized{
	self.get_token(DOLLAR, 0)
}
/// Retrieves first TerminalNode corresponding to token COLON
/// Returns `None` if there is no child corresponding to token COLON
fn COLON(&self) -> Option<Rc<TerminalNode<'input,ailParserContextType>>> where Self:Sized{
	self.get_token(COLON, 0)
}
/// Retrieves first TerminalNode corresponding to token DOT
/// Returns `None` if there is no child corresponding to token DOT
fn DOT(&self) -> Option<Rc<TerminalNode<'input,ailParserContextType>>> where Self:Sized{
	self.get_token(DOT, 0)
}

}

impl<'input> LvalueContextAttrs<'input> for LvalueContext<'input>{}

impl<'input, I, H> ailParser<'input, I, H>
where
    I: TokenStream<'input, TF = LocalTokenFactory<'input> > + TidAble<'input>,
    H: ErrorStrategy<'input,BaseParserType<'input,I>>
{
	pub fn lvalue(&mut self,)
	-> Result<Rc<LvalueContextAll<'input>>,ANTLRError> {
		let mut recog = self;
		let _parentctx = recog.ctx.take();
		let mut _localctx = LvalueContextExt::new(_parentctx.clone(), recog.base.get_state());
        recog.base.enter_rule(_localctx.clone(), 30, RULE_lvalue);
        let mut _localctx: Rc<LvalueContextAll> = _localctx;
		let result: Result<(), ANTLRError> = (|| {

			recog.base.set_state(252);
			recog.err_handler.sync(&mut recog.base)?;
			match recog.base.input.la(1) {
			 IDENT 
				=> {
					//recog.base.enter_outer_alt(_localctx.clone(), 1);
					recog.base.enter_outer_alt(None, 1);
					{
					recog.base.set_state(243);
					recog.base.match_token(IDENT,&mut recog.err_handler)?;

					}
				}

			 GLOBAL 
				=> {
					//recog.base.enter_outer_alt(_localctx.clone(), 2);
					recog.base.enter_outer_alt(None, 2);
					{
					recog.base.set_state(244);
					recog.base.match_token(GLOBAL,&mut recog.err_handler)?;

					recog.base.set_state(245);
					recog.base.match_token(IDENT,&mut recog.err_handler)?;

					}
				}

			 DOLLAR 
				=> {
					//recog.base.enter_outer_alt(_localctx.clone(), 3);
					recog.base.enter_outer_alt(None, 3);
					{
					recog.base.set_state(246);
					recog.base.match_token(DOLLAR,&mut recog.err_handler)?;

					recog.base.set_state(247);
					recog.base.match_token(IDENT,&mut recog.err_handler)?;

					recog.base.set_state(248);
					recog.base.match_token(COLON,&mut recog.err_handler)?;

					recog.base.set_state(249);
					recog.base.match_token(IDENT,&mut recog.err_handler)?;

					recog.base.set_state(250);
					recog.base.match_token(DOT,&mut recog.err_handler)?;

					recog.base.set_state(251);
					recog.base.match_token(IDENT,&mut recog.err_handler)?;

					}
				}

				_ => Err(ANTLRError::NoAltError(NoViableAltError::new(&mut recog.base)))?
			}
			Ok(())
		})();
		match result {
		Ok(_)=>{},
        Err(e @ ANTLRError::FallThrough(_)) => return Err(e),
		Err(ref re) => {
				//_localctx.exception = re;
				recog.err_handler.report_error(&mut recog.base, re);
				recog.err_handler.recover(&mut recog.base, re)?;
			}
		}
		recog.base.exit_rule();

		Ok(_localctx)
	}
}
//------------------- assign_expr ----------------
pub type Assign_exprContextAll<'input> = Assign_exprContext<'input>;


pub type Assign_exprContext<'input> = BaseParserRuleContext<'input,Assign_exprContextExt<'input>>;

#[derive(Clone)]
pub struct Assign_exprContextExt<'input>{
ph:PhantomData<&'input str>
}

impl<'input> ailParserContext<'input> for Assign_exprContext<'input>{}

impl<'input,'a> Listenable<dyn ailListener<'input> + 'a> for Assign_exprContext<'input>{
		fn enter(&self,listener: &mut (dyn ailListener<'input> + 'a)) {
			listener.enter_every_rule(self);
			listener.enter_assign_expr(self);
		}
		fn exit(&self,listener: &mut (dyn ailListener<'input> + 'a)) {
			listener.exit_assign_expr(self);
			listener.exit_every_rule(self);
		}
}

impl<'input,'a> Visitable<dyn ailVisitor<'input> + 'a> for Assign_exprContext<'input>{
	fn accept(&self,visitor: &mut (dyn ailVisitor<'input> + 'a)) {
		visitor.visit_assign_expr(self);
	}
}

impl<'input> CustomRuleContext<'input> for Assign_exprContextExt<'input>{
	type TF = LocalTokenFactory<'input>;
	type Ctx = ailParserContextType;
	fn get_rule_index(&self) -> usize { RULE_assign_expr }
	//fn type_rule_index() -> usize where Self: Sized { RULE_assign_expr }
}
antlr_rust::tid!{Assign_exprContextExt<'a>}

impl<'input> Assign_exprContextExt<'input>{
	fn new(parent: Option<Rc<dyn ailParserContext<'input> + 'input > >, invoking_state: isize) -> Rc<Assign_exprContextAll<'input>> {
		Rc::new(
			BaseParserRuleContext::new_parser_ctx(parent, invoking_state,Assign_exprContextExt{
				ph:PhantomData
			}),
		)
	}
}

pub trait Assign_exprContextAttrs<'input>: ailParserContext<'input> + BorrowMut<Assign_exprContextExt<'input>>{

fn assignlist(&self) -> Option<Rc<AssignlistContextAll<'input>>> where Self:Sized{
	self.child_of_type(0)
}
fn block_expr(&self) -> Option<Rc<Block_exprContextAll<'input>>> where Self:Sized{
	self.child_of_type(0)
}

}

impl<'input> Assign_exprContextAttrs<'input> for Assign_exprContext<'input>{}

impl<'input, I, H> ailParser<'input, I, H>
where
    I: TokenStream<'input, TF = LocalTokenFactory<'input> > + TidAble<'input>,
    H: ErrorStrategy<'input,BaseParserType<'input,I>>
{
	pub fn assign_expr(&mut self,)
	-> Result<Rc<Assign_exprContextAll<'input>>,ANTLRError> {
		let mut recog = self;
		let _parentctx = recog.ctx.take();
		let mut _localctx = Assign_exprContextExt::new(_parentctx.clone(), recog.base.get_state());
        recog.base.enter_rule(_localctx.clone(), 32, RULE_assign_expr);
        let mut _localctx: Rc<Assign_exprContextAll> = _localctx;
		let result: Result<(), ANTLRError> = (|| {

			//recog.base.enter_outer_alt(_localctx.clone(), 1);
			recog.base.enter_outer_alt(None, 1);
			{
			/*InvokeRule assignlist*/
			recog.base.set_state(254);
			recog.assignlist()?;

			/*InvokeRule block_expr*/
			recog.base.set_state(255);
			recog.block_expr()?;

			}
			Ok(())
		})();
		match result {
		Ok(_)=>{},
        Err(e @ ANTLRError::FallThrough(_)) => return Err(e),
		Err(ref re) => {
				//_localctx.exception = re;
				recog.err_handler.report_error(&mut recog.base, re);
				recog.err_handler.recover(&mut recog.base, re)?;
			}
		}
		recog.base.exit_rule();

		Ok(_localctx)
	}
}
//------------------- call_expr ----------------
pub type Call_exprContextAll<'input> = Call_exprContext<'input>;


pub type Call_exprContext<'input> = BaseParserRuleContext<'input,Call_exprContextExt<'input>>;

#[derive(Clone)]
pub struct Call_exprContextExt<'input>{
ph:PhantomData<&'input str>
}

impl<'input> ailParserContext<'input> for Call_exprContext<'input>{}

impl<'input,'a> Listenable<dyn ailListener<'input> + 'a> for Call_exprContext<'input>{
		fn enter(&self,listener: &mut (dyn ailListener<'input> + 'a)) {
			listener.enter_every_rule(self);
			listener.enter_call_expr(self);
		}
		fn exit(&self,listener: &mut (dyn ailListener<'input> + 'a)) {
			listener.exit_call_expr(self);
			listener.exit_every_rule(self);
		}
}

impl<'input,'a> Visitable<dyn ailVisitor<'input> + 'a> for Call_exprContext<'input>{
	fn accept(&self,visitor: &mut (dyn ailVisitor<'input> + 'a)) {
		visitor.visit_call_expr(self);
	}
}

impl<'input> CustomRuleContext<'input> for Call_exprContextExt<'input>{
	type TF = LocalTokenFactory<'input>;
	type Ctx = ailParserContextType;
	fn get_rule_index(&self) -> usize { RULE_call_expr }
	//fn type_rule_index() -> usize where Self: Sized { RULE_call_expr }
}
antlr_rust::tid!{Call_exprContextExt<'a>}

impl<'input> Call_exprContextExt<'input>{
	fn new(parent: Option<Rc<dyn ailParserContext<'input> + 'input > >, invoking_state: isize) -> Rc<Call_exprContextAll<'input>> {
		Rc::new(
			BaseParserRuleContext::new_parser_ctx(parent, invoking_state,Call_exprContextExt{
				ph:PhantomData
			}),
		)
	}
}

pub trait Call_exprContextAttrs<'input>: ailParserContext<'input> + BorrowMut<Call_exprContextExt<'input>>{

/// Retrieves first TerminalNode corresponding to token IDENT
/// Returns `None` if there is no child corresponding to token IDENT
fn IDENT(&self) -> Option<Rc<TerminalNode<'input,ailParserContextType>>> where Self:Sized{
	self.get_token(IDENT, 0)
}
fn calllist(&self) -> Option<Rc<CalllistContextAll<'input>>> where Self:Sized{
	self.child_of_type(0)
}

}

impl<'input> Call_exprContextAttrs<'input> for Call_exprContext<'input>{}

impl<'input, I, H> ailParser<'input, I, H>
where
    I: TokenStream<'input, TF = LocalTokenFactory<'input> > + TidAble<'input>,
    H: ErrorStrategy<'input,BaseParserType<'input,I>>
{
	pub fn call_expr(&mut self,)
	-> Result<Rc<Call_exprContextAll<'input>>,ANTLRError> {
		let mut recog = self;
		let _parentctx = recog.ctx.take();
		let mut _localctx = Call_exprContextExt::new(_parentctx.clone(), recog.base.get_state());
        recog.base.enter_rule(_localctx.clone(), 34, RULE_call_expr);
        let mut _localctx: Rc<Call_exprContextAll> = _localctx;
		let result: Result<(), ANTLRError> = (|| {

			//recog.base.enter_outer_alt(_localctx.clone(), 1);
			recog.base.enter_outer_alt(None, 1);
			{
			recog.base.set_state(257);
			recog.base.match_token(IDENT,&mut recog.err_handler)?;

			/*InvokeRule calllist*/
			recog.base.set_state(258);
			recog.calllist()?;

			}
			Ok(())
		})();
		match result {
		Ok(_)=>{},
        Err(e @ ANTLRError::FallThrough(_)) => return Err(e),
		Err(ref re) => {
				//_localctx.exception = re;
				recog.err_handler.report_error(&mut recog.base, re);
				recog.err_handler.recover(&mut recog.base, re)?;
			}
		}
		recog.base.exit_rule();

		Ok(_localctx)
	}
}
//------------------- block_stmt ----------------
pub type Block_stmtContextAll<'input> = Block_stmtContext<'input>;


pub type Block_stmtContext<'input> = BaseParserRuleContext<'input,Block_stmtContextExt<'input>>;

#[derive(Clone)]
pub struct Block_stmtContextExt<'input>{
ph:PhantomData<&'input str>
}

impl<'input> ailParserContext<'input> for Block_stmtContext<'input>{}

impl<'input,'a> Listenable<dyn ailListener<'input> + 'a> for Block_stmtContext<'input>{
		fn enter(&self,listener: &mut (dyn ailListener<'input> + 'a)) {
			listener.enter_every_rule(self);
			listener.enter_block_stmt(self);
		}
		fn exit(&self,listener: &mut (dyn ailListener<'input> + 'a)) {
			listener.exit_block_stmt(self);
			listener.exit_every_rule(self);
		}
}

impl<'input,'a> Visitable<dyn ailVisitor<'input> + 'a> for Block_stmtContext<'input>{
	fn accept(&self,visitor: &mut (dyn ailVisitor<'input> + 'a)) {
		visitor.visit_block_stmt(self);
	}
}

impl<'input> CustomRuleContext<'input> for Block_stmtContextExt<'input>{
	type TF = LocalTokenFactory<'input>;
	type Ctx = ailParserContextType;
	fn get_rule_index(&self) -> usize { RULE_block_stmt }
	//fn type_rule_index() -> usize where Self: Sized { RULE_block_stmt }
}
antlr_rust::tid!{Block_stmtContextExt<'a>}

impl<'input> Block_stmtContextExt<'input>{
	fn new(parent: Option<Rc<dyn ailParserContext<'input> + 'input > >, invoking_state: isize) -> Rc<Block_stmtContextAll<'input>> {
		Rc::new(
			BaseParserRuleContext::new_parser_ctx(parent, invoking_state,Block_stmtContextExt{
				ph:PhantomData
			}),
		)
	}
}

pub trait Block_stmtContextAttrs<'input>: ailParserContext<'input> + BorrowMut<Block_stmtContextExt<'input>>{

fn block(&self) -> Option<Rc<BlockContextAll<'input>>> where Self:Sized{
	self.child_of_type(0)
}

}

impl<'input> Block_stmtContextAttrs<'input> for Block_stmtContext<'input>{}

impl<'input, I, H> ailParser<'input, I, H>
where
    I: TokenStream<'input, TF = LocalTokenFactory<'input> > + TidAble<'input>,
    H: ErrorStrategy<'input,BaseParserType<'input,I>>
{
	pub fn block_stmt(&mut self,)
	-> Result<Rc<Block_stmtContextAll<'input>>,ANTLRError> {
		let mut recog = self;
		let _parentctx = recog.ctx.take();
		let mut _localctx = Block_stmtContextExt::new(_parentctx.clone(), recog.base.get_state());
        recog.base.enter_rule(_localctx.clone(), 36, RULE_block_stmt);
        let mut _localctx: Rc<Block_stmtContextAll> = _localctx;
		let result: Result<(), ANTLRError> = (|| {

			//recog.base.enter_outer_alt(_localctx.clone(), 1);
			recog.base.enter_outer_alt(None, 1);
			{
			/*InvokeRule block*/
			recog.base.set_state(260);
			recog.block()?;

			}
			Ok(())
		})();
		match result {
		Ok(_)=>{},
        Err(e @ ANTLRError::FallThrough(_)) => return Err(e),
		Err(ref re) => {
				//_localctx.exception = re;
				recog.err_handler.report_error(&mut recog.base, re);
				recog.err_handler.recover(&mut recog.base, re)?;
			}
		}
		recog.base.exit_rule();

		Ok(_localctx)
	}
}
//------------------- block_expr ----------------
pub type Block_exprContextAll<'input> = Block_exprContext<'input>;


pub type Block_exprContext<'input> = BaseParserRuleContext<'input,Block_exprContextExt<'input>>;

#[derive(Clone)]
pub struct Block_exprContextExt<'input>{
ph:PhantomData<&'input str>
}

impl<'input> ailParserContext<'input> for Block_exprContext<'input>{}

impl<'input,'a> Listenable<dyn ailListener<'input> + 'a> for Block_exprContext<'input>{
		fn enter(&self,listener: &mut (dyn ailListener<'input> + 'a)) {
			listener.enter_every_rule(self);
			listener.enter_block_expr(self);
		}
		fn exit(&self,listener: &mut (dyn ailListener<'input> + 'a)) {
			listener.exit_block_expr(self);
			listener.exit_every_rule(self);
		}
}

impl<'input,'a> Visitable<dyn ailVisitor<'input> + 'a> for Block_exprContext<'input>{
	fn accept(&self,visitor: &mut (dyn ailVisitor<'input> + 'a)) {
		visitor.visit_block_expr(self);
	}
}

impl<'input> CustomRuleContext<'input> for Block_exprContextExt<'input>{
	type TF = LocalTokenFactory<'input>;
	type Ctx = ailParserContextType;
	fn get_rule_index(&self) -> usize { RULE_block_expr }
	//fn type_rule_index() -> usize where Self: Sized { RULE_block_expr }
}
antlr_rust::tid!{Block_exprContextExt<'a>}

impl<'input> Block_exprContextExt<'input>{
	fn new(parent: Option<Rc<dyn ailParserContext<'input> + 'input > >, invoking_state: isize) -> Rc<Block_exprContextAll<'input>> {
		Rc::new(
			BaseParserRuleContext::new_parser_ctx(parent, invoking_state,Block_exprContextExt{
				ph:PhantomData
			}),
		)
	}
}

pub trait Block_exprContextAttrs<'input>: ailParserContext<'input> + BorrowMut<Block_exprContextExt<'input>>{

fn block(&self) -> Option<Rc<BlockContextAll<'input>>> where Self:Sized{
	self.child_of_type(0)
}

}

impl<'input> Block_exprContextAttrs<'input> for Block_exprContext<'input>{}

impl<'input, I, H> ailParser<'input, I, H>
where
    I: TokenStream<'input, TF = LocalTokenFactory<'input> > + TidAble<'input>,
    H: ErrorStrategy<'input,BaseParserType<'input,I>>
{
	pub fn block_expr(&mut self,)
	-> Result<Rc<Block_exprContextAll<'input>>,ANTLRError> {
		let mut recog = self;
		let _parentctx = recog.ctx.take();
		let mut _localctx = Block_exprContextExt::new(_parentctx.clone(), recog.base.get_state());
        recog.base.enter_rule(_localctx.clone(), 38, RULE_block_expr);
        let mut _localctx: Rc<Block_exprContextAll> = _localctx;
		let result: Result<(), ANTLRError> = (|| {

			//recog.base.enter_outer_alt(_localctx.clone(), 1);
			recog.base.enter_outer_alt(None, 1);
			{
			/*InvokeRule block*/
			recog.base.set_state(262);
			recog.block()?;

			}
			Ok(())
		})();
		match result {
		Ok(_)=>{},
        Err(e @ ANTLRError::FallThrough(_)) => return Err(e),
		Err(ref re) => {
				//_localctx.exception = re;
				recog.err_handler.report_error(&mut recog.base, re);
				recog.err_handler.recover(&mut recog.base, re)?;
			}
		}
		recog.base.exit_rule();

		Ok(_localctx)
	}
}
//------------------- block ----------------
pub type BlockContextAll<'input> = BlockContext<'input>;


pub type BlockContext<'input> = BaseParserRuleContext<'input,BlockContextExt<'input>>;

#[derive(Clone)]
pub struct BlockContextExt<'input>{
ph:PhantomData<&'input str>
}

impl<'input> ailParserContext<'input> for BlockContext<'input>{}

impl<'input,'a> Listenable<dyn ailListener<'input> + 'a> for BlockContext<'input>{
		fn enter(&self,listener: &mut (dyn ailListener<'input> + 'a)) {
			listener.enter_every_rule(self);
			listener.enter_block(self);
		}
		fn exit(&self,listener: &mut (dyn ailListener<'input> + 'a)) {
			listener.exit_block(self);
			listener.exit_every_rule(self);
		}
}

impl<'input,'a> Visitable<dyn ailVisitor<'input> + 'a> for BlockContext<'input>{
	fn accept(&self,visitor: &mut (dyn ailVisitor<'input> + 'a)) {
		visitor.visit_block(self);
	}
}

impl<'input> CustomRuleContext<'input> for BlockContextExt<'input>{
	type TF = LocalTokenFactory<'input>;
	type Ctx = ailParserContextType;
	fn get_rule_index(&self) -> usize { RULE_block }
	//fn type_rule_index() -> usize where Self: Sized { RULE_block }
}
antlr_rust::tid!{BlockContextExt<'a>}

impl<'input> BlockContextExt<'input>{
	fn new(parent: Option<Rc<dyn ailParserContext<'input> + 'input > >, invoking_state: isize) -> Rc<BlockContextAll<'input>> {
		Rc::new(
			BaseParserRuleContext::new_parser_ctx(parent, invoking_state,BlockContextExt{
				ph:PhantomData
			}),
		)
	}
}

pub trait BlockContextAttrs<'input>: ailParserContext<'input> + BorrowMut<BlockContextExt<'input>>{

/// Retrieves first TerminalNode corresponding to token LBRACE
/// Returns `None` if there is no child corresponding to token LBRACE
fn LBRACE(&self) -> Option<Rc<TerminalNode<'input,ailParserContextType>>> where Self:Sized{
	self.get_token(LBRACE, 0)
}
/// Retrieves first TerminalNode corresponding to token RBRACE
/// Returns `None` if there is no child corresponding to token RBRACE
fn RBRACE(&self) -> Option<Rc<TerminalNode<'input,ailParserContextType>>> where Self:Sized{
	self.get_token(RBRACE, 0)
}
fn stmt_all(&self) ->  Vec<Rc<StmtContextAll<'input>>> where Self:Sized{
	self.children_of_type()
}
fn stmt(&self, i: usize) -> Option<Rc<StmtContextAll<'input>>> where Self:Sized{
	self.child_of_type(i)
}
fn expr(&self) -> Option<Rc<ExprContextAll<'input>>> where Self:Sized{
	self.child_of_type(0)
}

}

impl<'input> BlockContextAttrs<'input> for BlockContext<'input>{}

impl<'input, I, H> ailParser<'input, I, H>
where
    I: TokenStream<'input, TF = LocalTokenFactory<'input> > + TidAble<'input>,
    H: ErrorStrategy<'input,BaseParserType<'input,I>>
{
	pub fn block(&mut self,)
	-> Result<Rc<BlockContextAll<'input>>,ANTLRError> {
		let mut recog = self;
		let _parentctx = recog.ctx.take();
		let mut _localctx = BlockContextExt::new(_parentctx.clone(), recog.base.get_state());
        recog.base.enter_rule(_localctx.clone(), 40, RULE_block);
        let mut _localctx: Rc<BlockContextAll> = _localctx;
		let mut _la: isize = -1;
		let result: Result<(), ANTLRError> = (|| {

			let mut _alt: isize;
			//recog.base.enter_outer_alt(_localctx.clone(), 1);
			recog.base.enter_outer_alt(None, 1);
			{
			recog.base.set_state(264);
			recog.base.match_token(LBRACE,&mut recog.err_handler)?;

			recog.base.set_state(268);
			recog.err_handler.sync(&mut recog.base)?;
			_alt = recog.interpreter.adaptive_predict(21,&mut recog.base)?;
			while { _alt!=2 && _alt!=INVALID_ALT } {
				if _alt==1 {
					{
					{
					/*InvokeRule stmt*/
					recog.base.set_state(265);
					recog.stmt()?;

					}
					} 
				}
				recog.base.set_state(270);
				recog.err_handler.sync(&mut recog.base)?;
				_alt = recog.interpreter.adaptive_predict(21,&mut recog.base)?;
			}
			recog.base.set_state(272);
			recog.err_handler.sync(&mut recog.base)?;
			_la = recog.base.input.la(1);
			if (((_la) & !0x3f) == 0 && ((1usize << _la) & ((1usize << T__8) | (1usize << STRING) | (1usize << COLOR) | (1usize << NUMBER) | (1usize << BOOL) | (1usize << GLOBAL) | (1usize << LET))) != 0) || ((((_la - 34)) & !0x3f) == 0 && ((1usize << (_la - 34)) & ((1usize << (DOLLAR - 34)) | (1usize << (EXCLAMATION - 34)) | (1usize << (LBRACE - 34)) | (1usize << (LBRACKET - 34)) | (1usize << (LPAREN - 34)) | (1usize << (MINUS - 34)) | (1usize << (PLUS - 34)) | (1usize << (IDENT - 34)))) != 0) {
				{
				/*InvokeRule expr*/
				recog.base.set_state(271);
				recog.expr_rec(0)?;

				}
			}

			recog.base.set_state(274);
			recog.base.match_token(RBRACE,&mut recog.err_handler)?;

			}
			Ok(())
		})();
		match result {
		Ok(_)=>{},
        Err(e @ ANTLRError::FallThrough(_)) => return Err(e),
		Err(ref re) => {
				//_localctx.exception = re;
				recog.err_handler.report_error(&mut recog.base, re);
				recog.err_handler.recover(&mut recog.base, re)?;
			}
		}
		recog.base.exit_rule();

		Ok(_localctx)
	}
}
//------------------- assignlist ----------------
pub type AssignlistContextAll<'input> = AssignlistContext<'input>;


pub type AssignlistContext<'input> = BaseParserRuleContext<'input,AssignlistContextExt<'input>>;

#[derive(Clone)]
pub struct AssignlistContextExt<'input>{
ph:PhantomData<&'input str>
}

impl<'input> ailParserContext<'input> for AssignlistContext<'input>{}

impl<'input,'a> Listenable<dyn ailListener<'input> + 'a> for AssignlistContext<'input>{
		fn enter(&self,listener: &mut (dyn ailListener<'input> + 'a)) {
			listener.enter_every_rule(self);
			listener.enter_assignlist(self);
		}
		fn exit(&self,listener: &mut (dyn ailListener<'input> + 'a)) {
			listener.exit_assignlist(self);
			listener.exit_every_rule(self);
		}
}

impl<'input,'a> Visitable<dyn ailVisitor<'input> + 'a> for AssignlistContext<'input>{
	fn accept(&self,visitor: &mut (dyn ailVisitor<'input> + 'a)) {
		visitor.visit_assignlist(self);
	}
}

impl<'input> CustomRuleContext<'input> for AssignlistContextExt<'input>{
	type TF = LocalTokenFactory<'input>;
	type Ctx = ailParserContextType;
	fn get_rule_index(&self) -> usize { RULE_assignlist }
	//fn type_rule_index() -> usize where Self: Sized { RULE_assignlist }
}
antlr_rust::tid!{AssignlistContextExt<'a>}

impl<'input> AssignlistContextExt<'input>{
	fn new(parent: Option<Rc<dyn ailParserContext<'input> + 'input > >, invoking_state: isize) -> Rc<AssignlistContextAll<'input>> {
		Rc::new(
			BaseParserRuleContext::new_parser_ctx(parent, invoking_state,AssignlistContextExt{
				ph:PhantomData
			}),
		)
	}
}

pub trait AssignlistContextAttrs<'input>: ailParserContext<'input> + BorrowMut<AssignlistContextExt<'input>>{

/// Retrieves first TerminalNode corresponding to token LET
/// Returns `None` if there is no child corresponding to token LET
fn LET(&self) -> Option<Rc<TerminalNode<'input,ailParserContextType>>> where Self:Sized{
	self.get_token(LET, 0)
}
/// Retrieves first TerminalNode corresponding to token IN
/// Returns `None` if there is no child corresponding to token IN
fn IN(&self) -> Option<Rc<TerminalNode<'input,ailParserContextType>>> where Self:Sized{
	self.get_token(IN, 0)
}
/// Retrieves all `TerminalNode`s corresponding to token IDENT in current rule
fn IDENT_all(&self) -> Vec<Rc<TerminalNode<'input,ailParserContextType>>>  where Self:Sized{
	self.children_of_type()
}
/// Retrieves 'i's TerminalNode corresponding to token IDENT, starting from 0.
/// Returns `None` if number of children corresponding to token IDENT is less or equal than `i`.
fn IDENT(&self, i: usize) -> Option<Rc<TerminalNode<'input,ailParserContextType>>> where Self:Sized{
	self.get_token(IDENT, i)
}
/// Retrieves all `TerminalNode`s corresponding to token EQUAL in current rule
fn EQUAL_all(&self) -> Vec<Rc<TerminalNode<'input,ailParserContextType>>>  where Self:Sized{
	self.children_of_type()
}
/// Retrieves 'i's TerminalNode corresponding to token EQUAL, starting from 0.
/// Returns `None` if number of children corresponding to token EQUAL is less or equal than `i`.
fn EQUAL(&self, i: usize) -> Option<Rc<TerminalNode<'input,ailParserContextType>>> where Self:Sized{
	self.get_token(EQUAL, i)
}
fn expr_all(&self) ->  Vec<Rc<ExprContextAll<'input>>> where Self:Sized{
	self.children_of_type()
}
fn expr(&self, i: usize) -> Option<Rc<ExprContextAll<'input>>> where Self:Sized{
	self.child_of_type(i)
}
/// Retrieves all `TerminalNode`s corresponding to token COMMA in current rule
fn COMMA_all(&self) -> Vec<Rc<TerminalNode<'input,ailParserContextType>>>  where Self:Sized{
	self.children_of_type()
}
/// Retrieves 'i's TerminalNode corresponding to token COMMA, starting from 0.
/// Returns `None` if number of children corresponding to token COMMA is less or equal than `i`.
fn COMMA(&self, i: usize) -> Option<Rc<TerminalNode<'input,ailParserContextType>>> where Self:Sized{
	self.get_token(COMMA, i)
}

}

impl<'input> AssignlistContextAttrs<'input> for AssignlistContext<'input>{}

impl<'input, I, H> ailParser<'input, I, H>
where
    I: TokenStream<'input, TF = LocalTokenFactory<'input> > + TidAble<'input>,
    H: ErrorStrategy<'input,BaseParserType<'input,I>>
{
	pub fn assignlist(&mut self,)
	-> Result<Rc<AssignlistContextAll<'input>>,ANTLRError> {
		let mut recog = self;
		let _parentctx = recog.ctx.take();
		let mut _localctx = AssignlistContextExt::new(_parentctx.clone(), recog.base.get_state());
        recog.base.enter_rule(_localctx.clone(), 42, RULE_assignlist);
        let mut _localctx: Rc<AssignlistContextAll> = _localctx;
		let mut _la: isize = -1;
		let result: Result<(), ANTLRError> = (|| {

			let mut _alt: isize;
			//recog.base.enter_outer_alt(_localctx.clone(), 1);
			recog.base.enter_outer_alt(None, 1);
			{
			recog.base.set_state(276);
			recog.base.match_token(LET,&mut recog.err_handler)?;

			recog.base.set_state(284);
			recog.err_handler.sync(&mut recog.base)?;
			_alt = recog.interpreter.adaptive_predict(23,&mut recog.base)?;
			while { _alt!=2 && _alt!=INVALID_ALT } {
				if _alt==1 {
					{
					{
					recog.base.set_state(277);
					recog.base.match_token(IDENT,&mut recog.err_handler)?;

					recog.base.set_state(278);
					recog.base.match_token(EQUAL,&mut recog.err_handler)?;

					/*InvokeRule expr*/
					recog.base.set_state(279);
					recog.expr_rec(0)?;

					recog.base.set_state(280);
					recog.base.match_token(COMMA,&mut recog.err_handler)?;

					}
					} 
				}
				recog.base.set_state(286);
				recog.err_handler.sync(&mut recog.base)?;
				_alt = recog.interpreter.adaptive_predict(23,&mut recog.base)?;
			}
			recog.base.set_state(293);
			recog.err_handler.sync(&mut recog.base)?;
			_la = recog.base.input.la(1);
			if _la==IDENT {
				{
				recog.base.set_state(287);
				recog.base.match_token(IDENT,&mut recog.err_handler)?;

				recog.base.set_state(288);
				recog.base.match_token(EQUAL,&mut recog.err_handler)?;

				/*InvokeRule expr*/
				recog.base.set_state(289);
				recog.expr_rec(0)?;

				recog.base.set_state(291);
				recog.err_handler.sync(&mut recog.base)?;
				_la = recog.base.input.la(1);
				if _la==COMMA {
					{
					recog.base.set_state(290);
					recog.base.match_token(COMMA,&mut recog.err_handler)?;

					}
				}

				}
			}

			recog.base.set_state(295);
			recog.base.match_token(IN,&mut recog.err_handler)?;

			}
			Ok(())
		})();
		match result {
		Ok(_)=>{},
        Err(e @ ANTLRError::FallThrough(_)) => return Err(e),
		Err(ref re) => {
				//_localctx.exception = re;
				recog.err_handler.report_error(&mut recog.base, re);
				recog.err_handler.recover(&mut recog.base, re)?;
			}
		}
		recog.base.exit_rule();

		Ok(_localctx)
	}
}
//------------------- arglist ----------------
pub type ArglistContextAll<'input> = ArglistContext<'input>;


pub type ArglistContext<'input> = BaseParserRuleContext<'input,ArglistContextExt<'input>>;

#[derive(Clone)]
pub struct ArglistContextExt<'input>{
ph:PhantomData<&'input str>
}

impl<'input> ailParserContext<'input> for ArglistContext<'input>{}

impl<'input,'a> Listenable<dyn ailListener<'input> + 'a> for ArglistContext<'input>{
		fn enter(&self,listener: &mut (dyn ailListener<'input> + 'a)) {
			listener.enter_every_rule(self);
			listener.enter_arglist(self);
		}
		fn exit(&self,listener: &mut (dyn ailListener<'input> + 'a)) {
			listener.exit_arglist(self);
			listener.exit_every_rule(self);
		}
}

impl<'input,'a> Visitable<dyn ailVisitor<'input> + 'a> for ArglistContext<'input>{
	fn accept(&self,visitor: &mut (dyn ailVisitor<'input> + 'a)) {
		visitor.visit_arglist(self);
	}
}

impl<'input> CustomRuleContext<'input> for ArglistContextExt<'input>{
	type TF = LocalTokenFactory<'input>;
	type Ctx = ailParserContextType;
	fn get_rule_index(&self) -> usize { RULE_arglist }
	//fn type_rule_index() -> usize where Self: Sized { RULE_arglist }
}
antlr_rust::tid!{ArglistContextExt<'a>}

impl<'input> ArglistContextExt<'input>{
	fn new(parent: Option<Rc<dyn ailParserContext<'input> + 'input > >, invoking_state: isize) -> Rc<ArglistContextAll<'input>> {
		Rc::new(
			BaseParserRuleContext::new_parser_ctx(parent, invoking_state,ArglistContextExt{
				ph:PhantomData
			}),
		)
	}
}

pub trait ArglistContextAttrs<'input>: ailParserContext<'input> + BorrowMut<ArglistContextExt<'input>>{

/// Retrieves first TerminalNode corresponding to token LPAREN
/// Returns `None` if there is no child corresponding to token LPAREN
fn LPAREN(&self) -> Option<Rc<TerminalNode<'input,ailParserContextType>>> where Self:Sized{
	self.get_token(LPAREN, 0)
}
/// Retrieves first TerminalNode corresponding to token RPAREN
/// Returns `None` if there is no child corresponding to token RPAREN
fn RPAREN(&self) -> Option<Rc<TerminalNode<'input,ailParserContextType>>> where Self:Sized{
	self.get_token(RPAREN, 0)
}
/// Retrieves all `TerminalNode`s corresponding to token IDENT in current rule
fn IDENT_all(&self) -> Vec<Rc<TerminalNode<'input,ailParserContextType>>>  where Self:Sized{
	self.children_of_type()
}
/// Retrieves 'i's TerminalNode corresponding to token IDENT, starting from 0.
/// Returns `None` if number of children corresponding to token IDENT is less or equal than `i`.
fn IDENT(&self, i: usize) -> Option<Rc<TerminalNode<'input,ailParserContextType>>> where Self:Sized{
	self.get_token(IDENT, i)
}
/// Retrieves all `TerminalNode`s corresponding to token COMMA in current rule
fn COMMA_all(&self) -> Vec<Rc<TerminalNode<'input,ailParserContextType>>>  where Self:Sized{
	self.children_of_type()
}
/// Retrieves 'i's TerminalNode corresponding to token COMMA, starting from 0.
/// Returns `None` if number of children corresponding to token COMMA is less or equal than `i`.
fn COMMA(&self, i: usize) -> Option<Rc<TerminalNode<'input,ailParserContextType>>> where Self:Sized{
	self.get_token(COMMA, i)
}

}

impl<'input> ArglistContextAttrs<'input> for ArglistContext<'input>{}

impl<'input, I, H> ailParser<'input, I, H>
where
    I: TokenStream<'input, TF = LocalTokenFactory<'input> > + TidAble<'input>,
    H: ErrorStrategy<'input,BaseParserType<'input,I>>
{
	pub fn arglist(&mut self,)
	-> Result<Rc<ArglistContextAll<'input>>,ANTLRError> {
		let mut recog = self;
		let _parentctx = recog.ctx.take();
		let mut _localctx = ArglistContextExt::new(_parentctx.clone(), recog.base.get_state());
        recog.base.enter_rule(_localctx.clone(), 44, RULE_arglist);
        let mut _localctx: Rc<ArglistContextAll> = _localctx;
		let mut _la: isize = -1;
		let result: Result<(), ANTLRError> = (|| {

			let mut _alt: isize;
			//recog.base.enter_outer_alt(_localctx.clone(), 1);
			recog.base.enter_outer_alt(None, 1);
			{
			recog.base.set_state(297);
			recog.base.match_token(LPAREN,&mut recog.err_handler)?;

			recog.base.set_state(302);
			recog.err_handler.sync(&mut recog.base)?;
			_alt = recog.interpreter.adaptive_predict(26,&mut recog.base)?;
			while { _alt!=2 && _alt!=INVALID_ALT } {
				if _alt==1 {
					{
					{
					recog.base.set_state(298);
					recog.base.match_token(IDENT,&mut recog.err_handler)?;

					recog.base.set_state(299);
					recog.base.match_token(COMMA,&mut recog.err_handler)?;

					}
					} 
				}
				recog.base.set_state(304);
				recog.err_handler.sync(&mut recog.base)?;
				_alt = recog.interpreter.adaptive_predict(26,&mut recog.base)?;
			}
			recog.base.set_state(309);
			recog.err_handler.sync(&mut recog.base)?;
			_la = recog.base.input.la(1);
			if _la==IDENT {
				{
				recog.base.set_state(305);
				recog.base.match_token(IDENT,&mut recog.err_handler)?;

				recog.base.set_state(307);
				recog.err_handler.sync(&mut recog.base)?;
				_la = recog.base.input.la(1);
				if _la==COMMA {
					{
					recog.base.set_state(306);
					recog.base.match_token(COMMA,&mut recog.err_handler)?;

					}
				}

				}
			}

			recog.base.set_state(311);
			recog.base.match_token(RPAREN,&mut recog.err_handler)?;

			}
			Ok(())
		})();
		match result {
		Ok(_)=>{},
        Err(e @ ANTLRError::FallThrough(_)) => return Err(e),
		Err(ref re) => {
				//_localctx.exception = re;
				recog.err_handler.report_error(&mut recog.base, re);
				recog.err_handler.recover(&mut recog.base, re)?;
			}
		}
		recog.base.exit_rule();

		Ok(_localctx)
	}
}
//------------------- calllist ----------------
pub type CalllistContextAll<'input> = CalllistContext<'input>;


pub type CalllistContext<'input> = BaseParserRuleContext<'input,CalllistContextExt<'input>>;

#[derive(Clone)]
pub struct CalllistContextExt<'input>{
ph:PhantomData<&'input str>
}

impl<'input> ailParserContext<'input> for CalllistContext<'input>{}

impl<'input,'a> Listenable<dyn ailListener<'input> + 'a> for CalllistContext<'input>{
		fn enter(&self,listener: &mut (dyn ailListener<'input> + 'a)) {
			listener.enter_every_rule(self);
			listener.enter_calllist(self);
		}
		fn exit(&self,listener: &mut (dyn ailListener<'input> + 'a)) {
			listener.exit_calllist(self);
			listener.exit_every_rule(self);
		}
}

impl<'input,'a> Visitable<dyn ailVisitor<'input> + 'a> for CalllistContext<'input>{
	fn accept(&self,visitor: &mut (dyn ailVisitor<'input> + 'a)) {
		visitor.visit_calllist(self);
	}
}

impl<'input> CustomRuleContext<'input> for CalllistContextExt<'input>{
	type TF = LocalTokenFactory<'input>;
	type Ctx = ailParserContextType;
	fn get_rule_index(&self) -> usize { RULE_calllist }
	//fn type_rule_index() -> usize where Self: Sized { RULE_calllist }
}
antlr_rust::tid!{CalllistContextExt<'a>}

impl<'input> CalllistContextExt<'input>{
	fn new(parent: Option<Rc<dyn ailParserContext<'input> + 'input > >, invoking_state: isize) -> Rc<CalllistContextAll<'input>> {
		Rc::new(
			BaseParserRuleContext::new_parser_ctx(parent, invoking_state,CalllistContextExt{
				ph:PhantomData
			}),
		)
	}
}

pub trait CalllistContextAttrs<'input>: ailParserContext<'input> + BorrowMut<CalllistContextExt<'input>>{

/// Retrieves first TerminalNode corresponding to token LPAREN
/// Returns `None` if there is no child corresponding to token LPAREN
fn LPAREN(&self) -> Option<Rc<TerminalNode<'input,ailParserContextType>>> where Self:Sized{
	self.get_token(LPAREN, 0)
}
/// Retrieves first TerminalNode corresponding to token RPAREN
/// Returns `None` if there is no child corresponding to token RPAREN
fn RPAREN(&self) -> Option<Rc<TerminalNode<'input,ailParserContextType>>> where Self:Sized{
	self.get_token(RPAREN, 0)
}
fn expr_all(&self) ->  Vec<Rc<ExprContextAll<'input>>> where Self:Sized{
	self.children_of_type()
}
fn expr(&self, i: usize) -> Option<Rc<ExprContextAll<'input>>> where Self:Sized{
	self.child_of_type(i)
}
/// Retrieves all `TerminalNode`s corresponding to token COMMA in current rule
fn COMMA_all(&self) -> Vec<Rc<TerminalNode<'input,ailParserContextType>>>  where Self:Sized{
	self.children_of_type()
}
/// Retrieves 'i's TerminalNode corresponding to token COMMA, starting from 0.
/// Returns `None` if number of children corresponding to token COMMA is less or equal than `i`.
fn COMMA(&self, i: usize) -> Option<Rc<TerminalNode<'input,ailParserContextType>>> where Self:Sized{
	self.get_token(COMMA, i)
}

}

impl<'input> CalllistContextAttrs<'input> for CalllistContext<'input>{}

impl<'input, I, H> ailParser<'input, I, H>
where
    I: TokenStream<'input, TF = LocalTokenFactory<'input> > + TidAble<'input>,
    H: ErrorStrategy<'input,BaseParserType<'input,I>>
{
	pub fn calllist(&mut self,)
	-> Result<Rc<CalllistContextAll<'input>>,ANTLRError> {
		let mut recog = self;
		let _parentctx = recog.ctx.take();
		let mut _localctx = CalllistContextExt::new(_parentctx.clone(), recog.base.get_state());
        recog.base.enter_rule(_localctx.clone(), 46, RULE_calllist);
        let mut _localctx: Rc<CalllistContextAll> = _localctx;
		let mut _la: isize = -1;
		let result: Result<(), ANTLRError> = (|| {

			let mut _alt: isize;
			//recog.base.enter_outer_alt(_localctx.clone(), 1);
			recog.base.enter_outer_alt(None, 1);
			{
			recog.base.set_state(313);
			recog.base.match_token(LPAREN,&mut recog.err_handler)?;

			recog.base.set_state(319);
			recog.err_handler.sync(&mut recog.base)?;
			_alt = recog.interpreter.adaptive_predict(29,&mut recog.base)?;
			while { _alt!=2 && _alt!=INVALID_ALT } {
				if _alt==1 {
					{
					{
					/*InvokeRule expr*/
					recog.base.set_state(314);
					recog.expr_rec(0)?;

					recog.base.set_state(315);
					recog.base.match_token(COMMA,&mut recog.err_handler)?;

					}
					} 
				}
				recog.base.set_state(321);
				recog.err_handler.sync(&mut recog.base)?;
				_alt = recog.interpreter.adaptive_predict(29,&mut recog.base)?;
			}
			recog.base.set_state(326);
			recog.err_handler.sync(&mut recog.base)?;
			_la = recog.base.input.la(1);
			if (((_la) & !0x3f) == 0 && ((1usize << _la) & ((1usize << T__8) | (1usize << STRING) | (1usize << COLOR) | (1usize << NUMBER) | (1usize << BOOL) | (1usize << GLOBAL) | (1usize << LET))) != 0) || ((((_la - 34)) & !0x3f) == 0 && ((1usize << (_la - 34)) & ((1usize << (DOLLAR - 34)) | (1usize << (EXCLAMATION - 34)) | (1usize << (LBRACE - 34)) | (1usize << (LBRACKET - 34)) | (1usize << (LPAREN - 34)) | (1usize << (MINUS - 34)) | (1usize << (PLUS - 34)) | (1usize << (IDENT - 34)))) != 0) {
				{
				/*InvokeRule expr*/
				recog.base.set_state(322);
				recog.expr_rec(0)?;

				recog.base.set_state(324);
				recog.err_handler.sync(&mut recog.base)?;
				_la = recog.base.input.la(1);
				if _la==COMMA {
					{
					recog.base.set_state(323);
					recog.base.match_token(COMMA,&mut recog.err_handler)?;

					}
				}

				}
			}

			recog.base.set_state(328);
			recog.base.match_token(RPAREN,&mut recog.err_handler)?;

			}
			Ok(())
		})();
		match result {
		Ok(_)=>{},
        Err(e @ ANTLRError::FallThrough(_)) => return Err(e),
		Err(ref re) => {
				//_localctx.exception = re;
				recog.err_handler.report_error(&mut recog.base, re);
				recog.err_handler.recover(&mut recog.base, re)?;
			}
		}
		recog.base.exit_rule();

		Ok(_localctx)
	}
}

lazy_static! {
    static ref _ATN: Arc<ATN> =
        Arc::new(ATNDeserializer::new(None).deserialize(_serializedATN.chars()));
    static ref _decision_to_DFA: Arc<Vec<antlr_rust::RwLock<DFA>>> = {
        let mut dfa = Vec::new();
        let size = _ATN.decision_to_state.len();
        for i in 0..size {
            dfa.push(DFA::new(
                _ATN.clone(),
                _ATN.get_decision_state(i),
                i as isize,
            ).into())
        }
        Arc::new(dfa)
    };
}



const _serializedATN:&'static str =
	"\x03\u{608b}\u{a72a}\u{8133}\u{b9ed}\u{417c}\u{3be7}\u{7786}\u{5964}\x03\
	\x38\u{14d}\x04\x02\x09\x02\x04\x03\x09\x03\x04\x04\x09\x04\x04\x05\x09\
	\x05\x04\x06\x09\x06\x04\x07\x09\x07\x04\x08\x09\x08\x04\x09\x09\x09\x04\
	\x0a\x09\x0a\x04\x0b\x09\x0b\x04\x0c\x09\x0c\x04\x0d\x09\x0d\x04\x0e\x09\
	\x0e\x04\x0f\x09\x0f\x04\x10\x09\x10\x04\x11\x09\x11\x04\x12\x09\x12\x04\
	\x13\x09\x13\x04\x14\x09\x14\x04\x15\x09\x15\x04\x16\x09\x16\x04\x17\x09\
	\x17\x04\x18\x09\x18\x04\x19\x09\x19\x03\x02\x03\x02\x03\x02\x07\x02\x36\
	\x0a\x02\x0c\x02\x0e\x02\x39\x0b\x02\x03\x02\x03\x02\x03\x03\x03\x03\x03\
	\x03\x03\x03\x03\x03\x05\x03\x42\x0a\x03\x03\x03\x03\x03\x03\x03\x03\x03\
	\x03\x04\x03\x04\x03\x04\x03\x04\x03\x04\x03\x04\x03\x04\x03\x04\x03\x04\
	\x03\x04\x05\x04\x52\x0a\x04\x03\x05\x03\x05\x03\x05\x03\x05\x03\x05\x03\
	\x05\x03\x06\x03\x06\x03\x06\x03\x06\x03\x06\x03\x06\x05\x06\x60\x0a\x06\
	\x03\x07\x03\x07\x03\x07\x03\x07\x03\x07\x03\x07\x03\x07\x03\x07\x07\x07\
	\x6a\x0a\x07\x0c\x07\x0e\x07\x6d\x0b\x07\x03\x07\x03\x07\x05\x07\x71\x0a\
	\x07\x03\x08\x03\x08\x03\x08\x03\x08\x03\x09\x03\x09\x03\x09\x03\x09\x03\
	\x09\x03\x09\x03\x09\x03\x09\x03\x09\x03\x09\x03\x09\x03\x09\x03\x09\x03\
	\x09\x03\x09\x03\x09\x03\x09\x05\x09\u{88}\x0a\x09\x03\x09\x03\x09\x03\x0a\
	\x03\x0a\x03\x0a\x03\x0a\x03\x0b\x03\x0b\x03\x0b\x03\x0b\x03\x0c\x03\x0c\
	\x03\x0c\x03\x0c\x03\x0c\x03\x0d\x03\x0d\x03\x0e\x03\x0e\x03\x0f\x03\x0f\
	\x03\x0f\x03\x0f\x03\x0f\x03\x0f\x03\x0f\x03\x0f\x03\x0f\x03\x0f\x03\x0f\
	\x03\x0f\x03\x0f\x03\x0f\x03\x0f\x03\x0f\x03\x0f\x03\x0f\x03\x0f\x05\x0f\
	\u{b0}\x0a\x0f\x05\x0f\u{b2}\x0a\x0f\x03\x0f\x03\x0f\x03\x0f\x03\x0f\x03\
	\x0f\x03\x0f\x07\x0f\u{ba}\x0a\x0f\x0c\x0f\x0e\x0f\u{bd}\x0b\x0f\x03\x0f\
	\x03\x0f\x05\x0f\u{c1}\x0a\x0f\x05\x0f\u{c3}\x0a\x0f\x03\x0f\x03\x0f\x03\
	\x0f\x03\x0f\x03\x0f\x03\x0f\x03\x0f\x07\x0f\u{cc}\x0a\x0f\x0c\x0f\x0e\x0f\
	\u{cf}\x0b\x0f\x03\x0f\x03\x0f\x03\x0f\x03\x0f\x05\x0f\u{d5}\x0a\x0f\x05\
	\x0f\u{d7}\x0a\x0f\x03\x0f\x03\x0f\x03\x0f\x03\x0f\x03\x0f\x05\x0f\u{de}\
	\x0a\x0f\x03\x0f\x03\x0f\x05\x0f\u{e2}\x0a\x0f\x03\x0f\x03\x0f\x03\x0f\x03\
	\x0f\x07\x0f\u{e8}\x0a\x0f\x0c\x0f\x0e\x0f\u{eb}\x0b\x0f\x03\x10\x03\x10\
	\x03\x10\x03\x10\x03\x10\x03\x10\x03\x10\x05\x10\u{f4}\x0a\x10\x03\x11\x03\
	\x11\x03\x11\x03\x11\x03\x11\x03\x11\x03\x11\x03\x11\x03\x11\x05\x11\u{ff}\
	\x0a\x11\x03\x12\x03\x12\x03\x12\x03\x13\x03\x13\x03\x13\x03\x14\x03\x14\
	\x03\x15\x03\x15\x03\x16\x03\x16\x07\x16\u{10d}\x0a\x16\x0c\x16\x0e\x16\
	\u{110}\x0b\x16\x03\x16\x05\x16\u{113}\x0a\x16\x03\x16\x03\x16\x03\x17\x03\
	\x17\x03\x17\x03\x17\x03\x17\x03\x17\x07\x17\u{11d}\x0a\x17\x0c\x17\x0e\
	\x17\u{120}\x0b\x17\x03\x17\x03\x17\x03\x17\x03\x17\x05\x17\u{126}\x0a\x17\
	\x05\x17\u{128}\x0a\x17\x03\x17\x03\x17\x03\x18\x03\x18\x03\x18\x07\x18\
	\u{12f}\x0a\x18\x0c\x18\x0e\x18\u{132}\x0b\x18\x03\x18\x03\x18\x05\x18\u{136}\
	\x0a\x18\x05\x18\u{138}\x0a\x18\x03\x18\x03\x18\x03\x19\x03\x19\x03\x19\
	\x03\x19\x07\x19\u{140}\x0a\x19\x0c\x19\x0e\x19\u{143}\x0b\x19\x03\x19\x03\
	\x19\x05\x19\u{147}\x0a\x19\x05\x19\u{149}\x0a\x19\x03\x19\x03\x19\x03\x19\
	\x02\x03\x1c\x1a\x02\x04\x06\x08\x0a\x0c\x0e\x10\x12\x14\x16\x18\x1a\x1c\
	\x1e\x20\x22\x24\x26\x28\x2a\x2c\x2e\x30\x02\x04\x05\x02\x27\x27\x2c\x2c\
	\x2f\x2f\x06\x02\x03\x0a\x20\x21\x2c\x2f\x34\x35\x02\u{167}\x02\x37\x03\
	\x02\x02\x02\x04\x3c\x03\x02\x02\x02\x06\x51\x03\x02\x02\x02\x08\x53\x03\
	\x02\x02\x02\x0a\x5f\x03\x02\x02\x02\x0c\x61\x03\x02\x02\x02\x0e\x72\x03\
	\x02\x02\x02\x10\x76\x03\x02\x02\x02\x12\u{8b}\x03\x02\x02\x02\x14\u{8f}\
	\x03\x02\x02\x02\x16\u{93}\x03\x02\x02\x02\x18\u{98}\x03\x02\x02\x02\x1a\
	\u{9a}\x03\x02\x02\x02\x1c\u{e1}\x03\x02\x02\x02\x1e\u{f3}\x03\x02\x02\x02\
	\x20\u{fe}\x03\x02\x02\x02\x22\u{100}\x03\x02\x02\x02\x24\u{103}\x03\x02\
	\x02\x02\x26\u{106}\x03\x02\x02\x02\x28\u{108}\x03\x02\x02\x02\x2a\u{10a}\
	\x03\x02\x02\x02\x2c\u{116}\x03\x02\x02\x02\x2e\u{12b}\x03\x02\x02\x02\x30\
	\u{13b}\x03\x02\x02\x02\x32\x36\x05\x04\x03\x02\x33\x36\x05\x06\x04\x02\
	\x34\x36\x05\x08\x05\x02\x35\x32\x03\x02\x02\x02\x35\x33\x03\x02\x02\x02\
	\x35\x34\x03\x02\x02\x02\x36\x39\x03\x02\x02\x02\x37\x35\x03\x02\x02\x02\
	\x37\x38\x03\x02\x02\x02\x38\x3a\x03\x02\x02\x02\x39\x37\x03\x02\x02\x02\
	\x3a\x3b\x07\x02\x02\x03\x3b\x03\x03\x02\x02\x02\x3c\x3d\x07\x1e\x02\x02\
	\x3d\x3e\x07\x24\x02\x02\x3e\x41\x07\x37\x02\x02\x3f\x40\x07\x22\x02\x02\
	\x40\x42\x07\x37\x02\x02\x41\x3f\x03\x02\x02\x02\x41\x42\x03\x02\x02\x02\
	\x42\x43\x03\x02\x02\x02\x43\x44\x07\x25\x02\x02\x44\x45\x07\x37\x02\x02\
	\x45\x46\x05\x26\x14\x02\x46\x05\x03\x02\x02\x02\x47\x48\x07\x17\x02\x02\
	\x48\x49\x07\x37\x02\x02\x49\x4a\x05\x2e\x18\x02\x4a\x4b\x05\x28\x15\x02\
	\x4b\x52\x03\x02\x02\x02\x4c\x4d\x07\x1c\x02\x02\x4d\x4e\x07\x37\x02\x02\
	\x4e\x4f\x05\x2e\x18\x02\x4f\x50\x05\x26\x14\x02\x50\x52\x03\x02\x02\x02\
	\x51\x47\x03\x02\x02\x02\x51\x4c\x03\x02\x02\x02\x52\x07\x03\x02\x02\x02\
	\x53\x54\x07\x18\x02\x02\x54\x55\x07\x37\x02\x02\x55\x56\x07\x26\x02\x02\
	\x56\x57\x05\x1c\x0f\x02\x57\x58\x07\x33\x02\x02\x58\x09\x03\x02\x02\x02\
	\x59\x60\x05\x0c\x07\x02\x5a\x60\x05\x0e\x08\x02\x5b\x60\x05\x10\x09\x02\
	\x5c\x60\x05\x12\x0a\x02\x5d\x60\x05\x14\x0b\x02\x5e\x60\x05\x16\x0c\x02\
	\x5f\x59\x03\x02\x02\x02\x5f\x5a\x03\x02\x02\x02\x5f\x5b\x03\x02\x02\x02\
	\x5f\x5c\x03\x02\x02\x02\x5f\x5d\x03\x02\x02\x02\x5f\x5e\x03\x02\x02\x02\
	\x60\x0b\x03\x02\x02\x02\x61\x62\x07\x19\x02\x02\x62\x63\x05\x1c\x0f\x02\
	\x63\x6b\x05\x26\x14\x02\x64\x65\x07\x14\x02\x02\x65\x66\x07\x19\x02\x02\
	\x66\x67\x05\x1c\x0f\x02\x67\x68\x05\x26\x14\x02\x68\x6a\x03\x02\x02\x02\
	\x69\x64\x03\x02\x02\x02\x6a\x6d\x03\x02\x02\x02\x6b\x69\x03\x02\x02\x02\
	\x6b\x6c\x03\x02\x02\x02\x6c\x70\x03\x02\x02\x02\x6d\x6b\x03\x02\x02\x02\
	\x6e\x6f\x07\x14\x02\x02\x6f\x71\x05\x26\x14\x02\x70\x6e\x03\x02\x02\x02\
	\x70\x71\x03\x02\x02\x02\x71\x0d\x03\x02\x02\x02\x72\x73\x07\x1f\x02\x02\
	\x73\x74\x05\x1c\x0f\x02\x74\x75\x05\x26\x14\x02\x75\x0f\x03\x02\x02\x02\
	\x76\u{87}\x07\x15\x02\x02\x77\x78\x07\x37\x02\x02\x78\x79\x07\x16\x02\x02\
	\x79\x7a\x05\x1c\x0f\x02\x7a\x7b\x07\x1d\x02\x02\x7b\x7c\x05\x1c\x0f\x02\
	\x7c\x7d\x07\x13\x02\x02\x7d\x7e\x05\x1c\x0f\x02\x7e\u{88}\x03\x02\x02\x02\
	\x7f\u{80}\x07\x37\x02\x02\u{80}\u{81}\x07\x1a\x02\x02\u{81}\u{88}\x05\x1c\
	\x0f\x02\u{82}\u{83}\x07\x37\x02\x02\u{83}\u{84}\x07\x23\x02\x02\u{84}\u{85}\
	\x07\x37\x02\x02\u{85}\u{86}\x07\x1a\x02\x02\u{86}\u{88}\x05\x1c\x0f\x02\
	\u{87}\x77\x03\x02\x02\x02\u{87}\x7f\x03\x02\x02\x02\u{87}\u{82}\x03\x02\
	\x02\x02\u{88}\u{89}\x03\x02\x02\x02\u{89}\u{8a}\x05\x26\x14\x02\u{8a}\x11\
	\x03\x02\x02\x02\u{8b}\u{8c}\x07\x37\x02\x02\u{8c}\u{8d}\x05\x30\x19\x02\
	\u{8d}\u{8e}\x07\x33\x02\x02\u{8e}\x13\x03\x02\x02\x02\u{8f}\u{90}\x05\x2c\
	\x17\x02\u{90}\u{91}\x05\x26\x14\x02\u{91}\u{92}\x07\x33\x02\x02\u{92}\x15\
	\x03\x02\x02\x02\u{93}\u{94}\x05\x20\x11\x02\u{94}\u{95}\x07\x26\x02\x02\
	\u{95}\u{96}\x05\x1c\x0f\x02\u{96}\u{97}\x07\x33\x02\x02\u{97}\x17\x03\x02\
	\x02\x02\u{98}\u{99}\x09\x02\x02\x02\u{99}\x19\x03\x02\x02\x02\u{9a}\u{9b}\
	\x09\x03\x02\x02\u{9b}\x1b\x03\x02\x02\x02\u{9c}\u{9d}\x08\x0f\x01\x02\u{9d}\
	\u{9e}\x07\x2b\x02\x02\u{9e}\u{9f}\x05\x1c\x0f\x02\u{9f}\u{a0}\x07\x32\x02\
	\x02\u{a0}\u{e2}\x03\x02\x02\x02\u{a1}\u{a2}\x05\x18\x0d\x02\u{a2}\u{a3}\
	\x05\x1c\x0f\x0f\u{a3}\u{e2}\x03\x02\x02\x02\u{a4}\u{e2}\x05\x28\x15\x02\
	\u{a5}\u{e2}\x05\x24\x13\x02\u{a6}\u{e2}\x05\x22\x12\x02\u{a7}\u{a8}\x07\
	\x0b\x02\x02\u{a8}\u{a9}\x05\x1c\x0f\x02\u{a9}\u{aa}\x07\x23\x02\x02\u{aa}\
	\u{ab}\x05\x1c\x0f\x02\u{ab}\u{ac}\x07\x23\x02\x02\u{ac}\u{b1}\x05\x1c\x0f\
	\x02\u{ad}\u{af}\x07\x23\x02\x02\u{ae}\u{b0}\x05\x1c\x0f\x02\u{af}\u{ae}\
	\x03\x02\x02\x02\u{af}\u{b0}\x03\x02\x02\x02\u{b0}\u{b2}\x03\x02\x02\x02\
	\u{b1}\u{ad}\x03\x02\x02\x02\u{b1}\u{b2}\x03\x02\x02\x02\u{b2}\u{b3}\x03\
	\x02\x02\x02\u{b3}\u{b4}\x07\x32\x02\x02\u{b4}\u{e2}\x03\x02\x02\x02\u{b5}\
	\u{bb}\x07\x2a\x02\x02\u{b6}\u{b7}\x05\x1c\x0f\x02\u{b7}\u{b8}\x07\x23\x02\
	\x02\u{b8}\u{ba}\x03\x02\x02\x02\u{b9}\u{b6}\x03\x02\x02\x02\u{ba}\u{bd}\
	\x03\x02\x02\x02\u{bb}\u{b9}\x03\x02\x02\x02\u{bb}\u{bc}\x03\x02\x02\x02\
	\u{bc}\u{c2}\x03\x02\x02\x02\u{bd}\u{bb}\x03\x02\x02\x02\u{be}\u{c0}\x05\
	\x1c\x0f\x02\u{bf}\u{c1}\x07\x23\x02\x02\u{c0}\u{bf}\x03\x02\x02\x02\u{c0}\
	\u{c1}\x03\x02\x02\x02\u{c1}\u{c3}\x03\x02\x02\x02\u{c2}\u{be}\x03\x02\x02\
	\x02\u{c2}\u{c3}\x03\x02\x02\x02\u{c3}\u{c4}\x03\x02\x02\x02\u{c4}\u{e2}\
	\x07\x31\x02\x02\u{c5}\u{cd}\x07\x29\x02\x02\u{c6}\u{c7}\x05\x1c\x0f\x02\
	\u{c7}\u{c8}\x07\x22\x02\x02\u{c8}\u{c9}\x05\x1c\x0f\x02\u{c9}\u{ca}\x07\
	\x23\x02\x02\u{ca}\u{cc}\x03\x02\x02\x02\u{cb}\u{c6}\x03\x02\x02\x02\u{cc}\
	\u{cf}\x03\x02\x02\x02\u{cd}\u{cb}\x03\x02\x02\x02\u{cd}\u{ce}\x03\x02\x02\
	\x02\u{ce}\u{d6}\x03\x02\x02\x02\u{cf}\u{cd}\x03\x02\x02\x02\u{d0}\u{d1}\
	\x05\x1c\x0f\x02\u{d1}\u{d2}\x07\x22\x02\x02\u{d2}\u{d4}\x05\x1c\x0f\x02\
	\u{d3}\u{d5}\x07\x23\x02\x02\u{d4}\u{d3}\x03\x02\x02\x02\u{d4}\u{d5}\x03\
	\x02\x02\x02\u{d5}\u{d7}\x03\x02\x02\x02\u{d6}\u{d0}\x03\x02\x02\x02\u{d6}\
	\u{d7}\x03\x02\x02\x02\u{d7}\u{d8}\x03\x02\x02\x02\u{d8}\u{e2}\x07\x30\x02\
	\x02\u{d9}\u{e2}\x05\x1e\x10\x02\u{da}\u{e2}\x07\x10\x02\x02\u{db}\u{e2}\
	\x07\x0f\x02\x02\u{dc}\u{de}\x07\x2c\x02\x02\u{dd}\u{dc}\x03\x02\x02\x02\
	\u{dd}\u{de}\x03\x02\x02\x02\u{de}\u{df}\x03\x02\x02\x02\u{df}\u{e2}\x07\
	\x11\x02\x02\u{e0}\u{e2}\x07\x12\x02\x02\u{e1}\u{9c}\x03\x02\x02\x02\u{e1}\
	\u{a1}\x03\x02\x02\x02\u{e1}\u{a4}\x03\x02\x02\x02\u{e1}\u{a5}\x03\x02\x02\
	\x02\u{e1}\u{a6}\x03\x02\x02\x02\u{e1}\u{a7}\x03\x02\x02\x02\u{e1}\u{b5}\
	\x03\x02\x02\x02\u{e1}\u{c5}\x03\x02\x02\x02\u{e1}\u{d9}\x03\x02\x02\x02\
	\u{e1}\u{da}\x03\x02\x02\x02\u{e1}\u{db}\x03\x02\x02\x02\u{e1}\u{dd}\x03\
	\x02\x02\x02\u{e1}\u{e0}\x03\x02\x02\x02\u{e2}\u{e9}\x03\x02\x02\x02\u{e3}\
	\u{e4}\x0c\x0e\x02\x02\u{e4}\u{e5}\x05\x1a\x0e\x02\u{e5}\u{e6}\x05\x1c\x0f\
	\x0f\u{e6}\u{e8}\x03\x02\x02\x02\u{e7}\u{e3}\x03\x02\x02\x02\u{e8}\u{eb}\
	\x03\x02\x02\x02\u{e9}\u{e7}\x03\x02\x02\x02\u{e9}\u{ea}\x03\x02\x02\x02\
	\u{ea}\x1d\x03\x02\x02\x02\u{eb}\u{e9}\x03\x02\x02\x02\u{ec}\u{ed}\x07\x24\
	\x02\x02\u{ed}\u{ee}\x07\x37\x02\x02\u{ee}\u{ef}\x07\x22\x02\x02\u{ef}\u{f4}\
	\x07\x37\x02\x02\u{f0}\u{f1}\x07\x24\x02\x02\u{f1}\u{f4}\x07\x37\x02\x02\
	\u{f2}\u{f4}\x05\x20\x11\x02\u{f3}\u{ec}\x03\x02\x02\x02\u{f3}\u{f0}\x03\
	\x02\x02\x02\u{f3}\u{f2}\x03\x02\x02\x02\u{f4}\x1f\x03\x02\x02\x02\u{f5}\
	\u{ff}\x07\x37\x02\x02\u{f6}\u{f7}\x07\x18\x02\x02\u{f7}\u{ff}\x07\x37\x02\
	\x02\u{f8}\u{f9}\x07\x24\x02\x02\u{f9}\u{fa}\x07\x37\x02\x02\u{fa}\u{fb}\
	\x07\x22\x02\x02\u{fb}\u{fc}\x07\x37\x02\x02\u{fc}\u{fd}\x07\x25\x02\x02\
	\u{fd}\u{ff}\x07\x37\x02\x02\u{fe}\u{f5}\x03\x02\x02\x02\u{fe}\u{f6}\x03\
	\x02\x02\x02\u{fe}\u{f8}\x03\x02\x02\x02\u{ff}\x21\x03\x02\x02\x02\u{100}\
	\u{101}\x05\x2c\x17\x02\u{101}\u{102}\x05\x28\x15\x02\u{102}\x23\x03\x02\
	\x02\x02\u{103}\u{104}\x07\x37\x02\x02\u{104}\u{105}\x05\x30\x19\x02\u{105}\
	\x25\x03\x02\x02\x02\u{106}\u{107}\x05\x2a\x16\x02\u{107}\x27\x03\x02\x02\
	\x02\u{108}\u{109}\x05\x2a\x16\x02\u{109}\x29\x03\x02\x02\x02\u{10a}\u{10e}\
	\x07\x29\x02\x02\u{10b}\u{10d}\x05\x0a\x06\x02\u{10c}\u{10b}\x03\x02\x02\
	\x02\u{10d}\u{110}\x03\x02\x02\x02\u{10e}\u{10c}\x03\x02\x02\x02\u{10e}\
	\u{10f}\x03\x02\x02\x02\u{10f}\u{112}\x03\x02\x02\x02\u{110}\u{10e}\x03\
	\x02\x02\x02\u{111}\u{113}\x05\x1c\x0f\x02\u{112}\u{111}\x03\x02\x02\x02\
	\u{112}\u{113}\x03\x02\x02\x02\u{113}\u{114}\x03\x02\x02\x02\u{114}\u{115}\
	\x07\x30\x02\x02\u{115}\x2b\x03\x02\x02\x02\u{116}\u{11e}\x07\x1b\x02\x02\
	\u{117}\u{118}\x07\x37\x02\x02\u{118}\u{119}\x07\x26\x02\x02\u{119}\u{11a}\
	\x05\x1c\x0f\x02\u{11a}\u{11b}\x07\x23\x02\x02\u{11b}\u{11d}\x03\x02\x02\
	\x02\u{11c}\u{117}\x03\x02\x02\x02\u{11d}\u{120}\x03\x02\x02\x02\u{11e}\
	\u{11c}\x03\x02\x02\x02\u{11e}\u{11f}\x03\x02\x02\x02\u{11f}\u{127}\x03\
	\x02\x02\x02\u{120}\u{11e}\x03\x02\x02\x02\u{121}\u{122}\x07\x37\x02\x02\
	\u{122}\u{123}\x07\x26\x02\x02\u{123}\u{125}\x05\x1c\x0f\x02\u{124}\u{126}\
	\x07\x23\x02\x02\u{125}\u{124}\x03\x02\x02\x02\u{125}\u{126}\x03\x02\x02\
	\x02\u{126}\u{128}\x03\x02\x02\x02\u{127}\u{121}\x03\x02\x02\x02\u{127}\
	\u{128}\x03\x02\x02\x02\u{128}\u{129}\x03\x02\x02\x02\u{129}\u{12a}\x07\
	\x1a\x02\x02\u{12a}\x2d\x03\x02\x02\x02\u{12b}\u{130}\x07\x2b\x02\x02\u{12c}\
	\u{12d}\x07\x37\x02\x02\u{12d}\u{12f}\x07\x23\x02\x02\u{12e}\u{12c}\x03\
	\x02\x02\x02\u{12f}\u{132}\x03\x02\x02\x02\u{130}\u{12e}\x03\x02\x02\x02\
	\u{130}\u{131}\x03\x02\x02\x02\u{131}\u{137}\x03\x02\x02\x02\u{132}\u{130}\
	\x03\x02\x02\x02\u{133}\u{135}\x07\x37\x02\x02\u{134}\u{136}\x07\x23\x02\
	\x02\u{135}\u{134}\x03\x02\x02\x02\u{135}\u{136}\x03\x02\x02\x02\u{136}\
	\u{138}\x03\x02\x02\x02\u{137}\u{133}\x03\x02\x02\x02\u{137}\u{138}\x03\
	\x02\x02\x02\u{138}\u{139}\x03\x02\x02\x02\u{139}\u{13a}\x07\x32\x02\x02\
	\u{13a}\x2f\x03\x02\x02\x02\u{13b}\u{141}\x07\x2b\x02\x02\u{13c}\u{13d}\
	\x05\x1c\x0f\x02\u{13d}\u{13e}\x07\x23\x02\x02\u{13e}\u{140}\x03\x02\x02\
	\x02\u{13f}\u{13c}\x03\x02\x02\x02\u{140}\u{143}\x03\x02\x02\x02\u{141}\
	\u{13f}\x03\x02\x02\x02\u{141}\u{142}\x03\x02\x02\x02\u{142}\u{148}\x03\
	\x02\x02\x02\u{143}\u{141}\x03\x02\x02\x02\u{144}\u{146}\x05\x1c\x0f\x02\
	\u{145}\u{147}\x07\x23\x02\x02\u{146}\u{145}\x03\x02\x02\x02\u{146}\u{147}\
	\x03\x02\x02\x02\u{147}\u{149}\x03\x02\x02\x02\u{148}\u{144}\x03\x02\x02\
	\x02\u{148}\u{149}\x03\x02\x02\x02\u{149}\u{14a}\x03\x02\x02\x02\u{14a}\
	\u{14b}\x07\x32\x02\x02\u{14b}\x31\x03\x02\x02\x02\x22\x35\x37\x41\x51\x5f\
	\x6b\x70\u{87}\u{af}\u{b1}\u{bb}\u{c0}\u{c2}\u{cd}\u{d4}\u{d6}\u{dd}\u{e1}\
	\u{e9}\u{f3}\u{fe}\u{10e}\u{112}\u{11e}\u{125}\u{127}\u{130}\u{135}\u{137}\
	\u{141}\u{146}\u{148}";

