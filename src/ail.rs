// WARNING: expr IS NOT A <value>!!!

use crate::parser::*;

use std::{
    borrow::Cow,
    convert::Into,
    io::{self, Read},
    rc::Rc
};

use antlr_rust::{
    InputStream, Parser,
    common_token_stream::CommonTokenStream,
    error_listener::ErrorListener,
    errors::ANTLRError,
    parser_rule_context::ParserRuleContext,
    token::Token,
    token_factory::TokenFactory,
    tree::{ParseTree, ParseTreeVisitorCompat},
    vocabulary::Vocabulary,
};

use thiserror::Error as ThisError;
use xmltree::{AttributeMap, Element, XMLNode};

#[macro_export]
macro_rules! xml {
    () => {};
    ($name:ident $(($($attr_name:expr => $attr_value:expr),+ $(,)?))? $([$($ty:ident $body:expr),+ $(,)?])?) => {{
        #[allow(unused_mut)]
        let mut element = Element::new(stringify!($name));
        $($(element.attributes.insert($attr_name.into(), $attr_value.into());)*)?
        $($(element.children.push(xml!($ty @ $body));)*)?
        element
    }};
    (el @ $body:expr) => {XMLNode::Element($body)};
    (tx @ $body:expr) => {XMLNode::Text($body.into())};
}

#[derive(Debug, ThisError)]
pub enum Error {
    #[error("I/O error: {0}")]
    Io(#[from] io::Error),

    #[error("Syntax error: {0}")]
    Syntax(#[from] ANTLRError),
}

#[derive(Clone)]
struct AilErrorListener(String);

#[derive(Debug)]
struct AilVocabulary<'a>(&'a dyn Vocabulary);

struct Visitor(String);

impl Visitor {
    fn try_builtin_expr(&mut self, name: &str, args: &[Rc<ExprContext>]) -> Option<Element> {
        let mut math_abs_sin = |is_trig: bool, op: &str| -> Element {
            if is_trig {
                block("math_sin", [
                    field("OP", op),
                    value("NUM", self.visit_expr(&args[0]).unwrap())
                ])
            } else {
                block("math_abs", [
                    field("OP", op),
                    value("NUM", self.visit_expr(&args[0]).unwrap())
                ])
            }
        };

        match name {
            "sqrt" => Some(math_abs_sin(false, "ROOT")),
            "abs" => Some(math_abs_sin(false, "ABS")),
            "log" => Some(math_abs_sin(false, "LN")),
            "exp" => Some(math_abs_sin(false, "EXP")),
            "round" => Some(math_abs_sin(false, "ROUND")),
            "ceil" => Some(math_abs_sin(false, "CEILING")),
            "floor" => Some(math_abs_sin(false, "FLOOR")),
            "sin" => Some(math_abs_sin(true, "SIN")),
            "cos" => Some(math_abs_sin(true, "COS")),
            "tan" => Some(math_abs_sin(true, "TAN")),
            "arcsin" => Some(math_abs_sin(true, "ASIN")),
            "arccos" => Some(math_abs_sin(true, "ACOS")),
            "arctan" => Some(math_abs_sin(true, "ATAN")),
            "atan2" => Some(block("math_atan2", [
                value("Y", self.visit_expr(&args[0]).unwrap()),
                value("X", self.visit_expr(&args[1]).unwrap()),
            ])),
            "contains" => Some(block("procedures_callreturn", [
                field("PROCNAME", "ail$rt$contains"),
                value("ARG0", self.visit_expr(&args[0]).unwrap()),
                value("ARG1", self.visit_expr(&args[1]).unwrap())
            ])),
            "len" => Some(block("procedures_callreturn", [
                field("PROCNAME", "ail$rt$len"),
                value("ARG0", self.visit_expr(&args[0]).unwrap())
            ])),
            "lexcmp" => Some(block("text_compare", [
                field("OP", match args[1].get_text().as_str() {
                    "lt" => "LT",
                    "gt" => "GT",
                    "eq" => "EQUAL",
                    "ne" => "NEQ",
                    other => {
                        let tok = args[1].start();
                        eprintln!("{}:{}:{}: error: expected any of {{lt, gt, eq, ne}}, got {}",
                            self.0,
                            tok.get_line(),
                            tok.get_column() + 1,
                            other
                        );
                        return None;
                    }
                })
            ])),
            "trim" => Some(block("text_trim", [
                value("TEXT", self.visit_expr(&args[0]).unwrap())
            ])),
            "to_upper" => Some(block("text_changeCase", [
                field("OP", "UPCASE"),
                value("TEXT", self.visit_expr(&args[0]).unwrap())
            ])),
            "to_lower" => Some(block("text_changeCase", [
                field("OP", "DOWNCASE"),
                value("TEXT", self.visit_expr(&args[0]).unwrap())
            ])),
            "find" => Some(block("procedures_callreturn", [
                field("PROCNAME", "ail$rt$find"),
                value("ARG0", self.visit_expr(&args[0]).unwrap()),
                value("ARG1", self.visit_expr(&args[1]).unwrap())
            ])),
            "contains_all" => Some(block("text_contains", [
                field("OP", "CONTAINS_ALL"),
                value("TEXT", self.visit_expr(&args[0]).unwrap()),
                value("PIECE", self.visit_expr(&args[1]).unwrap())
            ])),
            "split" => Some(block("procedures_callreturn", [
                field("PROCNAME", "ail$rt$split"),
                value("ARG0", self.visit_expr(&args[0]).unwrap()),
                value("ARG1", self.visit_expr(&args[1]).unwrap())
            ])),
            "split_once" => Some(block("procedures_callreturn", [
                field("PROCNAME", "ail$rt$split_once"),
                value("ARG0", self.visit_expr(&args[0]).unwrap()),
                value("ARG1", self.visit_expr(&args[1]).unwrap())
            ])),
            "segment" => Some(block("text_segment", [
                value("TEXT", self.visit_expr(&args[0]).unwrap()),
                value("START", self.visit_expr(&args[1]).unwrap()),
                value("LENGTH", self.visit_expr(&args[2]).unwrap())
            ])),
            "replace" => Some(block("text_replace_all", [
                value("TEXT", self.visit_expr(&args[0]).unwrap()),
                value("SEGMENT", self.visit_expr(&args[1]).unwrap()),
                value("REPLACEMENT", self.visit_expr(&args[2]).unwrap())
            ])),
            "reverse" => Some(block("procedures_callreturn", [
                field("PROCNAME", "ail$rt$reverse"),
                value("ARG0", self.visit_expr(&args[0]).unwrap()),
            ])),
            "replace_multi" => Some(block("text_replace_mappings", [
                field("OP", match args[2].get_text().as_str() {
                    "ordered" => "DICTIONARY_ORDER",
                    "max_munch" => "LONGEST_STRING_FIRST",
                    other => {
                        let tok = args[1].start();
                        eprintln!("{}:{}:{}: error: expected any of {{max_munch, ordered}}, got {}",
                            self.0,
                            tok.get_line(),
                            tok.get_column() + 1,
                            other
                        );
                        return None;
                    }
                }),
                value("MAPPINGS", self.visit_expr(&args[1]).unwrap()),
                value("TEXT", self.visit_expr(&args[0]).unwrap())
            ])),
            "copy" => Some(block("procedures_callreturn", [
                field("PROCNAME", "ail$rt$copy"),
                value("ARG0", self.visit_expr(&args[0]).unwrap()),
            ])),
            "lookup_pairs" => Some(block("lists_lookup_in_pairs", [
                value("KEY", self.visit_expr(&args[1]).unwrap()),
                value("LIST", self.visit_expr(&args[0]).unwrap()),
                value("NOTFOUND", self.visit_expr(&args[2]).unwrap())
            ])),
            "join" => Some(block("lists_join_with_separator", [
                value("SEPARATOR", self.visit_expr(&args[1]).unwrap()),
                value("LIST", self.visit_expr(&args[0]).unwrap())
            ])),
            "map" => Some(block("lists_map", [
                field("VAR", args[1].get_text().as_str()),
                value("LIST", self.visit_expr(&args[0]).unwrap()),
                value("TO", self.visit_expr(&args[2]).unwrap())
            ])),
            "filter" => Some(block("lists_filter", [
                field("VAR", args[1].get_text().as_str()),
                value("LIST", self.visit_expr(&args[0]).unwrap()),
                value("TEST", self.visit_expr(&args[2]).unwrap())
            ])),
            "reduce" => Some(block("lists_reduce", [
                field("VAR1", args[3].get_text().as_str()),
                field("VAR2", args[2].get_text().as_str()),
                value("LIST", self.visit_expr(&args[0]).unwrap()),
                value("INITANSWER", self.visit_expr(&args[1]).unwrap()),
                value("COMBINE", self.visit_expr(&args[4]).unwrap())
            ])),
            "sort" => Some(match args.len() {
                1 => block("lists_sort", [
                    value("LIST", self.visit_expr(&args[0]).unwrap()),
                ]),
                3 => block("lists_sort_key", [
                    field("VAR", args[1].get_text().as_str()),
                    value("LIST", self.visit_expr(&args[0]).unwrap()),
                    value("KEY", self.visit_expr(&args[2]).unwrap())
                ]),
                4 => block("lists_sort_comparator", [
                    field("VAR1", args[1].get_text().as_str()),
                    field("VAR2", args[2].get_text().as_str()),
                    value("LIST", self.visit_expr(&args[0]).unwrap()),
                    value("COMPARE", self.visit_expr(&args[3]).unwrap())
                ]),
                _ => {
                    let tok = args[1].start();
                    eprintln!("{}:{}:{}: error: incorrect argument count for `sort'",
                        self.0,
                        tok.get_line(),
                        tok.get_column() + 1,
                    );
                    return None;
                }
            }),
            "min" => Some(block("lists_minimum_value", [
                field("VAR1", args[1].get_text().as_str()),
                field("VAR2", args[2].get_text().as_str()),
                value("LIST", self.visit_expr(&args[0]).unwrap()),
                value("COMPARE", self.visit_expr(&args[3]).unwrap())
            ])),
            "max" => Some(block("lists_maximum_value", [
                field("VAR1", args[1].get_text().as_str()),
                field("VAR2", args[2].get_text().as_str()),
                value("LIST", self.visit_expr(&args[0]).unwrap()),
                value("COMPARE", self.visit_expr(&args[3]).unwrap())
            ])),
            "slice" => Some(block("lists_slice", [
                value("LIST", self.visit_expr(&args[0]).unwrap()),
                value("INDEX1", self.visit_expr(&args[1]).unwrap()),
                value("INDEX2", self.visit_expr(&args[2]).unwrap())
            ])),
            "path_get" => Some(block("dictionaries_recursive_lookup", [
                value("KEYS", self.visit_expr(&args[1]).unwrap()),
                value("DICT", self.visit_expr(&args[0]).unwrap()),
                value("NOTFOUND", self.visit_expr(&args[2]).unwrap())
            ])),
            "keys" => Some(block("dictionaries_getters", [
                field("OP", "KEYS"),
                value("DICT", self.visit_expr(&args[0]).unwrap()),
            ])),
            "values" => Some(block("dictionaries_getters", [
                field("OP", "VALUES"),
                value("DICT", self.visit_expr(&args[0]).unwrap()),
            ])),
            "to_pairs" => Some(block("dictionaries_dict_to_alist", [
                value("DICT", self.visit_expr(&args[0]).unwrap()),
            ])),
            "to_dict" => Some(block("dictionaries_alist_to_dict", [
                value("PAIRS", self.visit_expr(&args[0]).unwrap()),
            ])),
            "path_all" => Some(block("dictionaries_walk_all", [])),
            "path_walk" => Some(block("dictionaries_walk_tree", [
                value("PATH", self.visit_expr(&args[1]).unwrap()),
                value("DICT", self.visit_expr(&args[0]).unwrap()),
            ])),
            _ => None
        }
    }

    fn try_builtin_stmt(&mut self, name: &str, args: &[Rc<ExprContext>]) -> Option<Element> {
        match name {
            "insert" => Some(block("lists_insert_item", [
                value("LIST", self.visit_expr(&args[0]).unwrap()),
                value("INDEX", self.visit_expr(&args[1]).unwrap()),
                value("ITEM", self.visit_expr(&args[2]).unwrap()),
            ])),
            "remove" => Some(block("procedures_callnoreturn", [
                field("PROCNAME", "ail$rt$remove"),
                value("ARG0", self.visit_expr(&args[0]).unwrap()),
                value("ARG1", self.visit_expr(&args[1]).unwrap()),
            ])),
            "append" => Some(block("procedures_callnoreturn", [
                field("PROCNAME", "ail$rt$append"),
                value("ARG0", self.visit_expr(&args[0]).unwrap()),
                value("ARG1", self.visit_expr(&args[1]).unwrap()),
            ])),
            "path_set" => Some(block("dictionaries_recursive_set", [
                value("KEYS", self.visit_expr(&args[1]).unwrap()),
                value("DICT", self.visit_expr(&args[0]).unwrap()),
                value("VALUE", self.visit_expr(&args[2]).unwrap())
            ])),
            _ => None
        }
    }

    fn visit_lvalue<'a>(&mut self, ctx: &LvalueContext<'a>, expr: Option<&ExprContext<'a>>) -> <Self as ParseTreeVisitorCompat<'a>>::Return {
        Some(if ctx.DOLLAR().is_some() {
            let mut block = block("component_set_get", [
                mutation(AttributeMap::from([
                    ("component_type".into(), ctx.IDENT(0).unwrap().get_text()),
                    ("set_or_get".into(), if expr.is_some() { "set" } else { "get" }.into()),
                    ("property_name".into(), ctx.IDENT_all().last().unwrap().get_text()),
                    ("is_generic".into(), (ctx.IDENT_all().len() == 2).to_string()),
                    ("instance_name".into(), ctx.IDENT(1).unwrap().get_text()),
                ]), []),
                field("COMPONENT_SELECTOR", &ctx.IDENT(1).unwrap().get_text()),
                field("PROP", &ctx.IDENT_all().last().unwrap().get_text())
            ]);

            if let Some(expr) = expr {
                block.children.push(XMLNode::Element(value("VALUE", self.visit_expr(expr).unwrap())));
            }

            block
        } else {
            let name = format!("{}{}",
                if ctx.GLOBAL().is_some() { "global " } else { "" }, ctx.IDENT(0).unwrap().get_text()
            );

            match (ctx.expr(), expr) {
                (Some(key), Some(expr)) => block("procedures_callnoreturn", [
                    field("PROCNAME", "ail$rt$setprop"),
                    value("ARG0", block("lexical_variable_get", [
                        field("VAR", &name)
                    ])),
                    value("ARG1", self.visit_expr(&key).unwrap()),
                    value("ARG2", self.visit_expr(expr).unwrap()),
                ]),
                (Some(key), None) => {
                    let tok = &ctx.LBRACKET().unwrap().symbol;
                    block("procedures_callreturn", [
                        field("PROCNAME", "ail$rt$getprop"),
                        value("ARG0", block("lexical_variable_get", [
                            field("VAR", &name)
                        ])),
                        value("ARG1", self.visit_expr(&key).unwrap()),
                        value("ARG2", block("text", [
                            field("TEXT", &format!("{}:{}:{}: no such key {} in {}", self.0, tok.get_line(), tok.get_column() + 1, key.get_text(), name))
                        ]))
                    ])
                },
                (None, Some(expr)) => block("lexical_variable_set", [
                    field("VAR", &name),
                    value("VALUE", self.visit_expr(expr).unwrap())
                ]),
                (None, None) => block("lexical_variable_get", [
                    field("VAR", &name),
                ])
            }
        })
    }

}

impl Vocabulary for AilVocabulary<'_> {
    fn get_max_token_type(&self) -> isize {
        self.0.get_max_token_type()
    }
    fn get_literal_name(&self, token_type: isize) -> Option<&str> {
        self.0.get_literal_name(token_type)
    }
    fn get_symbolic_name(&self, token_type: isize) -> Option<&str> {
        self.0.get_symbolic_name(token_type)
    }
    fn get_display_name(&self, token_type: isize) -> Cow<'_, str> {
        match token_type {
            tokens::IDENT => Cow::Borrowed("identifier"),
            tokens::STRING => Cow::Borrowed("string"),
            tokens::COLOR => Cow::Borrowed("color"),
            tokens::NUMBER => Cow::Borrowed("number"),
            tokens::BOOL => Cow::Borrowed("boolean"),
            other => self.0.get_display_name(other),
        }
    }
}

impl<'a, T: Parser<'a>> ErrorListener<'a, T> for AilErrorListener {
    fn syntax_error(
        &self,
        recognizer: &T,
        offending_symbol: Option<&<T::TF as TokenFactory<'a>>::Inner>,
        line: isize,
        column: isize,
        message: &str,
        error: Option<&ANTLRError>,
    ) {
        let vocab = AilVocabulary(recognizer.get_vocabulary());
        let offending_symbol = offending_symbol.unwrap().to_owned();
        let offending_symbol_text = offending_symbol.get_text();
        eprintln!(
            "{}:{}:{}: {}",
            self.0, line, column + 1,
            match error {
                Some(ANTLRError::NoAltError(nva)) => format!(
                    "expected any of {}, got '{}'",
                    nva.base
                        .get_expected_tokens(recognizer)
                        .to_token_string(&vocab),
                    offending_symbol_text
                ),
                Some(ANTLRError::InputMismatchError(ime)) => format!(
                    "expected {}, got '{}'",
                    ime.base
                        .get_expected_tokens(recognizer)
                        .to_token_string(&vocab),
                    offending_symbol_text
                ),
                _ => message.into(),
            }
        );
    }
}

impl<'a> ParseTreeVisitorCompat<'a> for Visitor {
    type Node = AilParserContextType;
    type Return = Option<Element>;

    fn temp_result(&mut self) -> &mut Self::Return {
        Box::leak(Box::new(None))
    }
}

impl<'a> AilVisitorCompat<'a> for Visitor {
    fn visit_main(&mut self, ctx: &MainContext<'a>) -> Self::Return {
        let mut root_element = xml!(
            xml ("xmlns" => "https://developers.google.com/blockly/xml") [
                el xml!(yacodeblocks (
                    "xmlns" => "http://www.w3.org/1999/xhtml",
                    "ya-version" => "233",
                    "langauge-version" => "37"
                ))
            ]
        );

        root_element.children.extend(ctx.global_init_all().into_iter().map(|gi| XMLNode::Element(self.visit_global_init(&gi).unwrap())));
        root_element.children.extend(ctx.when_all().into_iter().map(|when| XMLNode::Element(self.visit_when(&when).unwrap())));
        ctx.func_def_all().into_iter().try_for_each(|func| {
            root_element.children.push(XMLNode::Element(self.visit_func_def(&func)?));
            Some(())
        })?;

        Some(root_element)
    }

    fn visit_when(&mut self, ctx: &WhenContext<'a>) -> Self::Return {
        let block = block("component_event", [
                mutation(AttributeMap::from([
                    ("component_type".into(), ctx.IDENT(0).unwrap().get_text()),
                    ("is_generic".into(), (ctx.IDENT_all().len() == 2).to_string()),
                    ("instance_name".into(), ctx.IDENT(1).unwrap().get_text()),
                    ("event_name".into(), ctx.IDENT_all().last().unwrap().get_text())
                ]), []),
                field("COMPONENT_SELECTOR", &ctx.IDENT(0).unwrap().get_text()),
                statement("DO", self.visit_block_stmt(&ctx.block_stmt().unwrap()))
        ]);

        Some(block)
    }

    fn visit_func_def(&mut self, ctx: &FuncDefContext<'a>) -> Self::Return {
        let is_expr = ctx.block_expr().is_some();
        let arglist = ctx.arglist().unwrap()
            .IDENT_all().into_iter()
            .filter(|ident| ident.symbol.get_token_type() == tokens::IDENT)
            .map(|ident| ident.get_text());
        let block = block(if is_expr {"procedures_defreturn"} else {"procedures_defnoreturn"}, [
                mutation(AttributeMap::new(), arglist.clone().map(|ident| xml!(arg ("name" => ident)))),
                field("NAME", &ctx.IDENT().unwrap().get_text())
            ].into_iter()
            .chain(arglist.enumerate().map(|(i, arg)| field(&format!("VAR{i}"), &arg)))
            .chain([
                if is_expr {
                    value("RETURN", self.visit_block_expr(&ctx.block_expr().unwrap())?)
                } else {
                    statement("STACK", ctx.block_stmt().and_then(|stmt| self.visit_block_stmt(&stmt)))
                }
            ].into_iter())
        );

        Some(block)
    }

    fn visit_global_init(&mut self, ctx: &GlobalInitContext<'a>) -> Self::Return {
        Some(block("global_declaration", [
            field("NAME", &ctx.IDENT().unwrap().get_text()),
            value("VALUE", self.visit_expr(&ctx.expr().unwrap()).unwrap())
        ]))
    }

    fn visit_stmt(&mut self, ctx: &StmtContext<'a>) -> Self::Return {
        if let Some(if_stmt) = ctx.if_stmt() {
            self.visit_if_stmt(&if_stmt)
        } else if let Some(while_stmt) = ctx.while_stmt() {
            self.visit_while_stmt(&while_stmt)
        } else if let Some(for_stmt) = ctx.for_stmt() {
            self.visit_for_stmt(&for_stmt)
        } else if let Some(call_stmt) = ctx.call_stmt() {
            self.visit_call_stmt(&call_stmt)
        } else if let Some(assign_stmt) = ctx.assign_stmt() {
            self.visit_assign_stmt(&assign_stmt)
        } else if let Some(modify_stmt) = ctx.modify_stmt() {
            self.visit_modify_stmt(&modify_stmt)
        } else {
            None
        }
    }

    fn visit_if_stmt(&mut self, ctx: &IfStmtContext<'a>) -> Self::Return {
        let mut block = block("controls_if", []);

        let has_else = ctx.block_stmt_all().len() > ctx.expr_all().len();

        let cond_stmts = ctx.expr_all().into_iter()
            .zip(ctx.block_stmt_all())
            .enumerate()
            .flat_map(|(index, (cond, block))| {
                let value = value(&format!("IF{index}"), self.visit_expr(&cond).unwrap());

                let statement = statement(&format!("DO{index}"), self.visit_block_stmt(&block));

                [value, statement]
            })
            .collect::<Vec<Element>>();

        block.children.push(XMLNode::Element(mutation(AttributeMap::from([
            ("elseif".into(), (ctx.expr_all().len() - 1).to_string()),
            ("else".into(), if has_else { "1" } else { "0" }.into())
        ]), [])));
        block.children.extend(cond_stmts.into_iter().map(XMLNode::Element));

        if has_else {
            let else_block = statement("ELSE", self.visit_block_stmt(&ctx.block_stmt_all().last().unwrap()));

            block.children.push(XMLNode::Element(else_block));
        }

        Some(block)
    }

    fn visit_while_stmt(&mut self, ctx: &WhileStmtContext<'a>) -> Self::Return {
        Some(block("controls_while", [
            value("TEST", self.visit_expr(&ctx.expr().unwrap()).unwrap()),
            statement("DO", self.visit_block_stmt(&ctx.block_stmt().unwrap()))
        ]))
    }

    fn visit_for_stmt(&mut self, ctx: &ForStmtContext<'a>) -> Self::Return {
        let stmt = self.visit_block_stmt(&ctx.block_stmt().unwrap());
        Some(if ctx.expr(1).is_some() {
            block("controls_forRange", [
                field ("VAR", &ctx.IDENT(0).unwrap().get_text()),
                value ("START", self.visit_expr(&ctx.expr(0).unwrap()).unwrap()),
                value ("END", self.visit_expr(&ctx.expr(1).unwrap()).unwrap()),
                value ("STEP", self.visit_expr(&ctx.expr(2).unwrap()).unwrap()),
                statement("DO", stmt)
            ])
        } else if ctx.COMMA().is_some() {
            block("controls_for_each_dict", [
                field("KEY", &ctx.IDENT(0).unwrap().get_text()),
                field("VALUE", &ctx.IDENT(1).unwrap().get_text()),
                value("DICT", self.visit_expr(&ctx.expr(0).unwrap()).unwrap()),
                statement("DO", stmt)
            ])
        } else {
            block("controls_forEach", [
                field("VAR", &ctx.IDENT(0).unwrap().get_text()),
                value("LIST", self.visit_expr(&ctx.expr(0).unwrap()).unwrap()),
                statement("DO", stmt)
            ])
        })
    }

    fn visit_call_expr(&mut self, ctx: &CallExprContext<'a>) -> Self::Return {
        let name = ctx.IDENT().unwrap().get_text();
        let args = ctx.calllist().unwrap().expr_all();

        Some(if let Some(block) = self.try_builtin_expr(&name, &args[..]) {
            block
        } else {
            let mut block = block("procedures_callreturn", [
                field("PROCNAME", &name)
            ]);

            block.children.extend(args.into_iter().enumerate().map(
                |(i, expr)| XMLNode::Element(value(&format!("ARG{i}"), self.visit_expr(&expr).unwrap()))
            ));

            block
        })
    }

    fn visit_call_stmt(&mut self, ctx: &CallStmtContext<'a>) -> Self::Return {
        let name = ctx.IDENT().unwrap().get_text();
        let args = ctx.calllist().unwrap().expr_all();

        Some(if let Some(block) = self.try_builtin_stmt(&name, &args[..]) {
            block
        } else {
            let mut block = block("procedures_callnoreturn", [
                field("PROCNAME", &name)
            ]);

            block.children.extend(args.into_iter().enumerate().map(
                |(i, expr)| XMLNode::Element(value(&format!("ARG{i}"), self.visit_expr(&expr).unwrap()))
            ));

            block
        })
    }

    fn visit_assign_expr(&mut self, ctx: &AssignExprContext<'a>) -> Self::Return {
        let vars = {
            let assignlist = ctx.assignlist().unwrap();
            assignlist.IDENT_all().into_iter().filter(|ident| ident.symbol.get_token_type() == tokens::IDENT).map(|ident| ident.get_text()).zip(assignlist.expr_all().into_iter())
        };
        let expr = self.visit_block_expr(&ctx.block_expr().unwrap())?;
        let block = block("local_declaration_expression", [
            mutation(AttributeMap::new(), vars.clone().map(|(name, _)| xml!(localname ("name" => name))))
        ].into_iter().chain(
            vars.clone().enumerate().map(|(i, (name, _))| field(&format!("VAR{i}"), &name))
        ).chain(
            vars.enumerate().map(|(i, (_, val))| value(&format!("DECL{i}"), self.visit_expr(&val).unwrap()))
        ).chain(
            [value("RETURN", expr)]
        ));

        Some(block)
    }

    fn visit_assign_stmt(&mut self, ctx: &AssignStmtContext<'a>) -> Self::Return {
        let vars = {
            let assignlist = ctx.assignlist().unwrap();
            assignlist.IDENT_all().into_iter().filter(|ident| ident.symbol.get_token_type() == tokens::IDENT).map(|ident| ident.get_text()).zip(assignlist.expr_all().into_iter())
        };
        let stmt = self.visit_block_stmt(&ctx.block_stmt().unwrap());
        let block = block("local_declaration_statement", [
            mutation(AttributeMap::new(), vars.clone().map(|(name, _)| xml!(localname ("name" => name))))
        ].into_iter().chain(
            vars.clone().enumerate().map(|(i, (name, _))| field(&format!("VAR{i}"), &name))
        ).chain(
            vars.enumerate().map(|(i, (_, val))| value(&format!("DECL{i}"), self.visit_expr(&val).unwrap()))
        ).chain(
            [statement("STACK", stmt)]
        ));

        Some(block)
    }

    fn visit_modify_stmt(&mut self, ctx: &ModifyStmtContext<'a>) -> Self::Return {
        self.visit_lvalue(&ctx.lvalue().unwrap(), Some(&ctx.expr().unwrap()))
    }

    fn visit_expr(&mut self, ctx: &ExprContext<'a>) -> Self::Return {
        if ctx.LPAREN().is_some() {
            self.visit_expr(&ctx.expr(0).unwrap())
        } else if let Some(op) = ctx.unary_op() {
            match op.get_text().as_str() {
                // NOTE: App Inventor uses duck typing, so unary + is redundant
                "+" => self.visit_expr(&ctx.expr(0).unwrap()),
                "-" => Some(block("math_neg", [
                    field("OP", "NEG"),
                    value("NUM", self.visit_expr(&ctx.expr(0).unwrap()).unwrap())
                ])),
                "!" => Some(block("logic_negate", [
                    value("BOOL", self.visit_expr(&ctx.expr(0).unwrap()).unwrap())
                ])),
                _ => panic!("ICE: unexpected parsing op {}", op.get_text())
            }
        } else if let Some(op) = ctx.binary_op() {
            Some(match op.get_text().as_str() {
                "*" => block("math_multiply", [
                    mutation(AttributeMap::from([
                        ("items".into(), "2".into())
                    ]), []),
                    value("NUM0", self.visit_expr(&ctx.expr(0).unwrap()).unwrap()),
                    value("NUM1", self.visit_expr(&ctx.expr(1).unwrap()).unwrap()),
                ]),
                "/" => block("math_division", [
                    value("A", self.visit_expr(&ctx.expr(0).unwrap()).unwrap()),
                    value("B", self.visit_expr(&ctx.expr(1).unwrap()).unwrap()),
                ]),
                "%" => block("math_divide", [
                    field("OP", "MODULO"),
                    value("DIVIDEND", self.visit_expr(&ctx.expr(0).unwrap()).unwrap()),
                    value("DIVISOR", self.visit_expr(&ctx.expr(1).unwrap()).unwrap()),
                ]),
                "+" => block("math_add", [
                    mutation(AttributeMap::from([
                        ("items".into(), "2".into())
                    ]), []),
                    value("NUM0", self.visit_expr(&ctx.expr(0).unwrap()).unwrap()),
                    value("NUM1", self.visit_expr(&ctx.expr(1).unwrap()).unwrap()),
                ]),
                "-" => block("math_subtract", [
                    value("A", self.visit_expr(&ctx.expr(0).unwrap()).unwrap()),
                    value("B", self.visit_expr(&ctx.expr(1).unwrap()).unwrap()),
                ]),
                "&" => block("math_bitwise", [
                    mutation(AttributeMap::from([
                        ("items".into(), "2".into())
                    ]), []),
                    field("OP", "BITAND"),
                    value("NUM0", self.visit_expr(&ctx.expr(0).unwrap()).unwrap()),
                    value("NUM1", self.visit_expr(&ctx.expr(1).unwrap()).unwrap()),
                ]),
                "^" => block("math_bitwise", [
                    mutation(AttributeMap::from([
                        ("items".into(), "2".into())
                    ]), []),
                    field("OP", "BITXOR"),
                    value("NUM0", self.visit_expr(&ctx.expr(0).unwrap()).unwrap()),
                    value("NUM1", self.visit_expr(&ctx.expr(1).unwrap()).unwrap()),
                ]),
                "|" => block("math_bitwise", [
                    mutation(AttributeMap::from([
                        ("items".into(), "2".into())
                    ]), []),
                    field("OP", "BITOR"),
                    value("NUM0", self.visit_expr(&ctx.expr(0).unwrap()).unwrap()),
                    value("NUM1", self.visit_expr(&ctx.expr(1).unwrap()).unwrap()),
                ]),
                "==" => block("math_compare", [
                    field("OP", "EQ"),
                    value("A", self.visit_expr(&ctx.expr(0).unwrap()).unwrap()),
                    value("B", self.visit_expr(&ctx.expr(1).unwrap()).unwrap()),
                ]),
                "!=" => block("math_compare", [
                    field("OP", "NEQ"),
                    value("A", self.visit_expr(&ctx.expr(0).unwrap()).unwrap()),
                    value("B", self.visit_expr(&ctx.expr(1).unwrap()).unwrap()),
                ]),
                "<" => block("math_compare", [
                    field("OP", "LT"),
                    value("A", self.visit_expr(&ctx.expr(0).unwrap()).unwrap()),
                    value("B", self.visit_expr(&ctx.expr(1).unwrap()).unwrap()),
                ]),
                ">" => block("math_compare", [
                    field("OP", "GT"),
                    value("A", self.visit_expr(&ctx.expr(0).unwrap()).unwrap()),
                    value("B", self.visit_expr(&ctx.expr(1).unwrap()).unwrap()),
                ]),
                "<=" => block("math_compare", [
                    field("OP", "LTE"),
                    value("A", self.visit_expr(&ctx.expr(0).unwrap()).unwrap()),
                    value("B", self.visit_expr(&ctx.expr(1).unwrap()).unwrap()),
                ]),
                ">=" => block("math_compare", [
                    field("OP", "GTE"),
                    value("A", self.visit_expr(&ctx.expr(0).unwrap()).unwrap()),
                    value("B", self.visit_expr(&ctx.expr(1).unwrap()).unwrap()),
                ]),
                "&&" => block("logic_operation", [
                    mutation(AttributeMap::from([
                        ("items".into(), "2".into())
                    ]), []),
                    field("OP", "AND"),
                    value("A", self.visit_expr(&ctx.expr(0).unwrap()).unwrap()),
                    value("B", self.visit_expr(&ctx.expr(1).unwrap()).unwrap()),
                ]),
                "||" => block("math_multiply", [
                    mutation(AttributeMap::from([
                        ("items".into(), "2".into())
                    ]), []),
                    field("OP", "OR"),
                    value("A", self.visit_expr(&ctx.expr(0).unwrap()).unwrap()),
                    value("B", self.visit_expr(&ctx.expr(1).unwrap()).unwrap()),
                ]),
                _ => panic!("ICE: unexpected parsing op {}", op.get_text())
            })
        } else if let Some(expr) = ctx.block_expr() {
            self.visit_block_expr(&expr)
        } else if let Some(expr) = ctx.call_expr() {
            self.visit_call_expr(&expr)
        } else if let Some(expr) = ctx.assign_expr() {
            self.visit_assign_expr(&expr)
        } else if ctx.RPAREN().is_some() {
            Some(block("color_make_color", [value("COLORLIST", {
                let list_len = ctx.expr_all().len();
                block("lists_create_with", 
                    [mutation(AttributeMap::from([("items".into(), list_len.to_string())]), [])].into_iter().chain(
                        ctx.expr_all().into_iter().enumerate().map(|(i, expr)| value(&format!("ADD{i}"), self.visit_expr(&expr).unwrap()))
                    )
                )
            })]))
        } else if ctx.LBRACKET().is_some() {
            let list_len = ctx.expr_all().len();
            Some(block("lists_create_with", 
                [mutation(AttributeMap::from([("items".into(), list_len.to_string())]), [])].into_iter().chain(
                    ctx.expr_all().into_iter().enumerate().map(|(i, expr)| value(&format!("ADD{i}"), self.visit_expr(&expr).unwrap()))
                )
            ))
        } else if ctx.LBRACE().is_some() {
            let dict_len = ctx.expr_all().len();
            Some(block("dictionaries_create_with", 
                [mutation(AttributeMap::from([("items".into(), (dict_len / 2).to_string())]), [])].into_iter().chain(
                    ctx.expr_all().as_chunks::<2>().0.into_iter().enumerate().map(|(i, kv)| value(&format!("ADD{i}"), block("pair", [
                        value("KEY", self.visit_expr(&kv[0]).unwrap()),
                        value("VALUE", self.visit_expr(&kv[1]).unwrap())
                    ])))
                )
            ))
        } else if let Some(rvalue) = ctx.rvalue() {
            self.visit_rvalue(&rvalue)
        } else if let Some(color) = ctx.COLOR() {
            Some(block("color_white", [field("COLOR", &color.get_text())]))
        } else if let Some(string) = ctx.STRING() {
            let text = string.get_text();
            Some(block("text", [field("TEXT", &text[1..text.len()-1].replace("\\\"", "\"").replace("\\'", "'"))]))
        } else if let Some(number) = ctx.NUMBER() {
            let s = &number.get_text();
            let num = if let Some(hex) = s.strip_prefix("0x").or_else(|| s.strip_prefix("0X")) {
                i64::from_str_radix(hex, 16)
            } else if let Some(bin) = s.strip_prefix("0b").or_else(|| s.strip_prefix("0B")) {
                i64::from_str_radix(bin, 2)
            } else if s.starts_with("0") {
                i64::from_str_radix(s, 8)
            } else {
                i64::from_str_radix(s, 10)
            }.expect("ICE: unhandled numeric syntax error from antlr");

            Some(block("math_number", [
                field("NUM", 
                    &if ctx.MINUS().is_some() { -num } else { num }.to_string()
                )
            ]))
        } else if let Some(boolean) = ctx.BOOL() {
            Some(block("logic_false", [field("BOOL", &boolean.get_text().to_uppercase())]))
        } else {
            panic!("ICE: unhandled expr syntax error from antlr")
        }
    }

    fn visit_rvalue(&mut self, ctx: &RvalueContext<'a>) -> Self::Return {
        match (ctx.DOLLAR().is_some(), ctx.COLON().is_some()) {
            (true, true) => Some(block("component_component_block", [
                mutation(AttributeMap::from([
                    ("component_type".into(), ctx.IDENT(0).unwrap().get_text()),
                    ("instance_name".into(), ctx.IDENT(1).unwrap().get_text())
                ]), []),
                field("COMPONENT_SELECTOR", &ctx.IDENT(1).unwrap().get_text())
            ])),
            (true, false) => Some(block("component_all_component_block", [
                mutation(AttributeMap::from([
                    ("component_type".into(), ctx.IDENT(0).unwrap().get_text()),
                ]), []),
                field("COMPONENT_TYPE_SELECTOR", &ctx.IDENT(0).unwrap().get_text())
            ])),
            (false, _) => self.visit_lvalue(&ctx.lvalue().unwrap(), None)
        }
    }

    fn visit_block_expr(&mut self, ctx: &BlockExprContext<'a>) -> Self::Return {
        let ctx = ctx.block().unwrap();
        if let Some(expr) = ctx.expr() {
            Some(if ctx.stmt_all().len() == 0 {
                /*block("controls_do_then_return", [
                    value("VALUE", self.visit_expr(&expr)?)
                ])*/
                self.visit_expr(&expr)?
            } else {
                block("controls_do_then_return", [
                    statement("STM", {
                        let mut block = self.visit_stmt(ctx.stmt_all().last().unwrap()).unwrap();
                        for stmt in ctx.stmt_all().into_iter().rev().skip(1) {
                            let mut new_block = self.visit_stmt(&stmt).unwrap();
                            new_block.children.push(XMLNode::Element(block));
                            block = new_block;
                        }

                        Some(block)
                    }),
                    value("VALUE", self.visit_expr(&expr).unwrap())
                ])
            })
        } else {
            let tok = &ctx.LBRACE().unwrap().symbol;
            eprintln!("{}:{}:{} error: block does not return a value", self.0, tok.get_line(), tok.get_column() + 1);
            None
        }
    }

    fn visit_block_stmt(&mut self, ctx: &BlockStmtContext<'a>) -> Self::Return {
        let ctx = ctx.block().unwrap();
        let mut block = self.visit_stmt(ctx.stmt_all().last()?)?;
        for stmt in ctx.stmt_all().into_iter().rev().skip(1) {
             let mut new_block = self.visit_stmt(&stmt)?;
             new_block.children.push(XMLNode::Element(xml!(next [el block])));
             block = new_block;
        }

        Some(block)
    }
}

pub fn parse(name: &str, file: &mut impl Read) -> Result<Element, Error> {
    let mut data = vec![];
    file.read_to_end(&mut data)?;

    let error_listener = AilErrorListener(name.into());

    let lexer = AilLexer::new(InputStream::new(&data[..]));

    let mut parser = AilParser::new(CommonTokenStream::new(lexer));
    parser.remove_error_listeners();
    parser.add_error_listener(Box::new(error_listener));

    let tree = parser.main().unwrap();
    Ok(Visitor(name.into()).visit_main(&tree).unwrap())
}

fn block(typ: &str, children: impl IntoIterator<Item = Element>) -> Element {
    let mut blok = xml!(block ("type" => typ));
    blok.children = children.into_iter().map(|el| XMLNode::Element(el.clone())).collect();
    blok
}

fn field(name: &str, value: &str) -> Element {
    xml!(field ("name" => name) [tx value])
}

fn mutation(attrs: AttributeMap<String, String>, children: impl IntoIterator<Item = Element>) -> Element {
    let mut mutn = Element::new("mutation");
    mutn.attributes = attrs;
    mutn.children = children.into_iter().map(|el| XMLNode::Element(el.clone())).collect();
    mutn
}

fn statement(name: &str, val: Option<Element>) -> Element {
    if let Some(val) = val {
        xml!(statement ("name" => name) [el val])
    } else {
        xml!(statement ("name" => name))
    }
}

fn value(name: &str, val: Element) -> Element {
    xml!(value ("name" => name) [el val])
}
